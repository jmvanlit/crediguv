##### Set-up -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(haven) # for read.csv()
library(texreg)
library(broom) # for tidy()
library(tibble) # for add_row()
library(stringr) # for str_detect()
library(ggtext) # for element_markdown()
library(ggdist) # for stat_halfeye()
library(gghalves) # for geom_half_point()
library(mediation) # for mediate()
library(lavaan)
library(purrr) # for map_df()
library(tidyr) # for pivot_wider()
library(kableExtra)
library(forcats) # for fct_reorder()
library(writexl) # for write_xlsx()

##### Data import --------------------------------------------------------------
crediguv <- read.csv("data/crediguv.csv") |> 
  as_tibble() |> 
  
  # convert to factor
  mutate(
    party = as.factor(party),
    consistency = as.factor(consistency),
    group = as.factor(group))

### Set reference categories
crediguv$party <- relevel(crediguv$party, ref = "opp")
crediguv$consistency <- relevel(crediguv$consistency, ref = "con")
crediguv$group <- relevel(crediguv$group, ref = "Consistent Opposition")

##### Simple Model: H1 ---------------------------------------------------------
# DV = dv_main

m1 <- lm(data = crediguv,
         dv_main ~ party + consistency)

# for exact p-values in the text
summary(m1)

##### Interaction Model: H2, H3 ------------------------------------------------
m2 <- lm(data = crediguv,
         dv_main ~ party + consistency + (party * consistency))

# for exact p-values in the text
summary(m2)

##### Table Main Models --------------------------------------------------------
main_models <- list(m1, m2)

# Table 2
texreg(main_models,
        caption = "When are elite democratic defenders credible?",
        caption.above = TRUE,
        label = "tab:mainmodels",
        custom.coef.names = c("Intercept", "Government", "Inconsistency", "Government * Inconsistency"))

##### Figure 2 -----------------------------------------------------------------

# data preparation
m1.plot <- m1 |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(model = "m1")

m2.plot <- m2 |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)") |> 
  mutate(model = "m2")

m12.plot <- bind_rows(m1.plot, m2.plot) |> 
  mutate(order = factor(term, levels = c("partygov", "consistencyinc", "partygov:consistencyinc"))) |> 
  mutate(term = case_when(
    term == "partygov" ~ "**Government-allied**  \ndemocratic defender",
    term == "consistencyinc" ~ "**Inconsistent**  \ncommitment to democracy",
    term == "partygov:consistencyinc" ~ "**Inconsistent * Government**  \ndemocratic defence")) |> 
  mutate(term = fct_reorder(term, desc(order)))

# plot parameters
fig2.colours <- c("m1" = "#73a2ac",
                  "m2" = "#0b5d96")

fig2.legend <- c("m1" = "**Hypothesis 1**  \nSimple model",
                 "m2" = "**Hypotheses 2 and 3**  \nInteraction model")

# plot...
fig2 <- 
ggplot(data = m12.plot,
       aes(x = estimate,
           y = term)) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points
  geom_point(aes(colour = model,
                 shape = model,
                 fill = model),
             position = position_dodge(width = 0.5),
             size = 3) +
  
  # confidence intervals
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = model,
                     linetype = model),
                 position = position_dodge(width = .5),
                 height = 0,
                 linewidth = 0.6) +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = fig1.colours,
                      labels = fig1.legend) +
  scale_fill_manual(values = fig1.colours,
                    labels = fig1.legend) +
  scale_shape_manual(values = c(22, 23),
                    labels = fig1.legend) +
  scale_linetype_manual(values = c("solid", "solid"),
                        labels = fig1.legend) +
  
  facet_wrap(~ model, 
             ncol = 2,
             labeller = as_labeller(c("m1" = "Model 1  \n**No interactions**  \n*H1*",
                                      "m2" = "Model 2  \n**Interaction**  \n*H2* and *H3*"))) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  scale_x_continuous(limits = c(-2, 1)) +
  theme(axis.text.y = ggtext::element_markdown(),
        # legend.text = ggtext::element_markdown(),
        # legend.title = element_blank(),
        # legend.position = "bottom",
        # legend.key.width = unit(1.5, "cm"),
        # legend.justification = c(1,0),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown())

# ... and save! 
ggsave(filename  = "figures/fig2.png",
       plot = fig2,
       width = 22,
       height = 10,
       dpi = 300,
       units = "cm")

##### Groups as Treatments -----------------------------------------------------
m4 <- lm(data = crediguv,
         dv_main ~ group)

m4.intercept <- coef(m4)[1]

m.interaction <- m4 |> 
  tidy(conf.int = TRUE) |> 
    mutate(
      group = case_when(term == "(Intercept)" ~ "Consistent Opposition",
                      term == "groupConsistent Government" ~ "Consistent Government",
                      term == "groupInconsistent Government" ~ "Inconsistent Government",
                      term == "groupInconsistent Opposition" ~ "Inconsistent Opposition"),
      fitted_mean = case_when(
        term != "(Intercept)" ~ m4.intercept + estimate,
        TRUE ~ estimate),
      fitted_low = fitted_mean - 1.96 * std.error,
      fitted_high = fitted_mean + 1.96 * std.error,
      consistency = ifelse(str_detect(term, "groupInconsistent"), "Inconsistent", "Consistent"),
      party = ifelse(str_detect(term,  "Government"), "Government", "Opposition"),
      # decide if we want these labels somewhere
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""),
      label = paste(round(fitted_mean, 2), stars, sep = ""))

##### Figure 4 -----------------------------------------------------------------
# plot parameters
raincloud.colours <- c("Consistent Opposition" = "#73a2ac",
                       "Consistent Government" = "#4c8ca5",
                       "Inconsistent Government" = "#26759f",
                       "Inconsistent Opposition" = "#0b5d96")

raincloud.legend <- c("Consistent Opposition" = "1",
                      "Consistent Government" = "2",
                      "Inconsistent Government" = "3",
                      "Inconsistent Opposition" = "4")

# plot ...
fig4 <-
ggplot(data = crediguv,
       aes(x = group,
           y = dv_main)) +
  
  geom_boxplot(width = .15,
               outlier.color = NA,
               alpha = 0.8,
               colour = "lightgrey") +
  
  # observations
  gghalves::geom_half_point(alpha = 0.2,
                            side = "l",
                            range_scale = 0.5,
                            aes(colour = group),
                            shape = 4) +
  
  # distribution
  ggdist::stat_halfeye(adjust = 3, 
                       width = .3,
                       justification = -0.4,
                       .width = 0,
                       point_colour = NA,
                       alpha = 0.3,
                       aes(fill = group)) +
  
  # fitted mean errorbars
  geom_errorbar(data = m.interaction,
                aes(y = fitted_mean,
                    ymin = fitted_low,
                    ymax = fitted_high,
                    colour = group),
                width = 0,
                linewidth = 0.6) +

  # fitted mean
  geom_point(data = m.interaction,
             aes(y = fitted_mean,
                 colour = group),
             size = 2,
             shape = 21,
             fill = "white") +
  
  # theme
  theme_minimal() +
  labs(x = "",
       y = "Credibility") +
  scale_fill_manual(values = raincloud.colours,
                    labels = raincloud.legend) +
  scale_colour_manual(values = raincloud.colours,
                      labels = raincloud.legend) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_cartesian(xlim = c(1.2, 3.9), clip = "off") +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7))
  
# ... and save
ggsave(filename  = "figures/fig4.png",
       plot = fig4,
       width = 18,
       height = 10,
       dpi = 300,
       units = "cm")

# Table for the Online Appendix
m.interaction.table <- m.interaction |> 
  dplyr::select(group, fitted_mean, fitted_low, fitted_high) |> 
  mutate(fitted_mean = round(fitted_mean, 2),
         fitted_low = round(fitted_low, 2),
         fitted_high = round(fitted_high, 2))

# Table 
kable(m.interaction.table,
      escape = TRUE,
      booktabs = TRUE, 
      format = "latex",
      caption = "Fitted means for each treatmentgroup",
      label = "fitted_means")

##### Figure 5 -----------------------------------------------------------------
# plot parameters
fig5.colours <- c("Government" = "#73a2ac",
                  "Opposition" = "#0b5d96")

fig5.legend <- c("Government" = "**Government-affiliated**  \ndemocratic defence",
                 "Opposition" = "**Opposition**  \ndemocratic defence")

# plot ...
fig5 <- 
ggplot(data = m.interaction,
       aes(x = consistency,
           y = fitted_mean)) +
  
  # fitted errors
  geom_errorbar(aes(ymin = fitted_low,
                    ymax = fitted_high,
                    colour = party),
                width = 0,
                position = position_dodge(width = 0.02)) +
  
  # fitted means
  geom_point(aes(colour = party,
                 shape = party),
             size = 3,
             position = position_dodge(width = 0.02)) +
  
  # line to show cross-over interaction
  geom_line(aes(group = party,
                colour = party)) +
  
  # theme
  theme_minimal() +
  labs(x = "",
       y = "Credibility") +
  scale_colour_manual(values = fig5.colours,
                      labels = fig5.legend) +
  scale_shape_manual(values = c(15, 19),
                    labels = fig5.legend) +
  theme(legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_cartesian(xlim = c(1.4, 1.6), clip = "off") +
  scale_y_continuous(limits = c(3, 5.5), breaks = seq(2, 6))

# ... and save
ggsave(filename  = "figures/fig5.png",
       plot = fig5,
       width = 14,
       height = 14,
       dpi = 300,
       units = "cm")

##### Mediation ----------------------------------------------------------------

crediguv_mediate <-  crediguv |> 
  mutate(
    party_num = case_when(
      party == "gov" ~ 1,
      party == "opp" ~ 0),
    consistency_nam = case_when(
      consistency == "inc" ~ 1,
      consistency == "con" ~ 0),
    congov = ifelse(group == "Consistent Government", 1, 0),
    conopp = ifelse(group == "Consistent Opposition", 1, 0),
    incgov = ifelse(group == "Inconsistent Government", 1, 0),
    incopp = ifelse(group == "Inconsistent Opposition", 1, 0))

### mediation from party to mandate and info ----
# mm for mediation model
mm.party <- '
# outcome model 
dv_main ~ b1*party + b2*consistency + m1*mv_mand + m2*mv_info

# mediator models
mv_mand ~ a1*party
mv_info ~ a2*party

# model correlation between mediators
mv_mand ~~ mv_info 
'

fit_mm.party <- sem(mm.party, 
                    data = crediguv_mediate)

summary(fit_mm.party, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

### mediation from treatment group to ploy ----
mm.consistency <- '
# outcome model
dv_main ~ b1*congov + b2*incopp + b3*incgov + m1*mv_ploy

# mediator models
mv_ploy ~ a1*congov + a2*incopp + a3*incgov
'

fit_mm.consistency <- sem(mm.consistency, data = crediguv_mediate)

summary(fit_mm.consistency, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

##### Next Steps Effects -------------------------------------------------------

# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ group"))
  model <- lm(lm_formula, data = crediguv)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

### some overall parameters

# define all dvs
nxt.dvs <- c("dv_nxt_scaled", "dv_nxt_1", "dv_nxt_2", "dv_nxt_3", "dv_nxt_4", "dv_nxt_5", 
             "dv_nxt_6", "dv_nxt_7", "dv_nxt_8") 
            # do not include dv_nxt_9 as it means 'nothing'

### simple model
sm.nxt <- map_df(nxt.dvs, run_multiple_lm) |> 
  mutate(dv = factor(dv, levels = c("dv_nxt_scaled", "dv_nxt_7", "dv_nxt_6", "dv_nxt_1", "dv_nxt_3",
                                    "dv_nxt_4", "dv_nxt_2", "dv_nxt_5", "dv_nxt_8"))) |> 
  group_by(dv) |> 
  mutate(
    fitted_mean = case_when(
      term == "(Intercept)" ~ estimate,
      term!= "(Intercept)" ~ estimate + first(estimate[term == "(Intercept)"])),
    fitted_low = fitted_mean - 1.96 * std.error,
    fitted_high = fitted_mean + 1.96 * std.error,
    term = case_when(
      term == "(Intercept)" ~ "Consistent Opposition",
      term == "groupConsistent Government" ~ "Consistent Government",
      term == "groupInconsistent Government" ~ "Inconsistent Government",
      term == "groupInconsistent Opposition" ~ "Inconsistent Opposition"),
    name = case_when(
      dv == "dv_nxt_scaled" ~ "**Total** call for actions",
      dv == "dv_nxt_1" ~ "File a **motion** of  \nno confidence",
      dv == "dv_nxt_2" ~ "Organize a **protest**  \nagainst the government",
      dv == "dv_nxt_3" ~ "Organize a  \n**party meeting**",
      dv == "dv_nxt_4" ~ "Create a **petition**",
      dv == "dv_nxt_5" ~ "Go to **court** against  \nthe government",
      dv == "dv_nxt_6" ~ "Go to the **media**",
      dv == "dv_nxt_7" ~ "Ask for a **debate**",
      dv == "dv_nxt_8" ~ "Other actions",
      dv == "dv_nxt_9" ~ "Nothing")) |> 
  mutate(name = fct_reorder(name, desc(dv)))

### open answers
other <- crediguv |> 
  filter(dv_nxt_8 != 0) |> 
  dplyr::select(group, dv_nxt_8_TEXT)

write_xlsx(other, 
           path = "data/othernxtsteps.xlsx",
           col_names = TRUE)

##### Figure 7 -----------------------------------------------------------------
# plot parameters
nxt.colours.7 <- c("Consistent Opposition" = "#0b5d96",
                   "Consistent Government" = "#73a2ac",
                   "Inconsistent Government" = "#73a2ac",
                   "Inconsistent Opposition" = "#0b5d96")

nxt.shape.7 <- c("Consistent Opposition" = 23,
                   "Consistent Government" = 23,
                   "Inconsistent Government" = 22,
                   "Inconsistent Opposition" = 22)

nxt.linetype.7 <- c("Consistent Opposition" = "solid",
                 "Consistent Government" = "longdash",
                 "Inconsistent Government" = "longdash",
                 "Inconsistent Opposition" = "solid")

# plot ...
fig7 <- 
ggplot(data = sm.nxt,
         aes(x = fitted_mean,
             y = name)) +
  
  # points, errorbars
  geom_point(aes(colour = term,
                 shape = term,
                 fill = term),
             size = 3,
             position = position_dodge(width = .5)) +
  
  geom_errorbarh(aes(xmin = fitted_low,
                     xmax = fitted_high,
                     colour = term,
                     linetype = term),
                 position = position_dodge(width = .5),
                 height = 0,
                 linewidth = 0.6) +
  
  # theme
  theme_bw() +
  scale_colour_manual(values = nxt.colours.7) +
  scale_fill_manual(values = nxt.colours.7) +
  scale_shape_manual(values = nxt.shape.7) +
  scale_linetype_manual(values = nxt.linetype.7) +
  
  # labels and legends
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(0.5,0)) +
    guides(color = guide_legend(nrow = 2))

# ... and save!
ggsave(filename  = "figures/fig7.png",
       plot = fig7,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

# Table for in the appendix
nxt.table <- sm.nxt |>
  ungroup() |> 
  mutate(estimate = paste0(round(fitted_mean, 2)," [", round(fitted_low, 2), ";", round(fitted_high, 2), "]"),
         name = str_replace_all(name, "\\*\\*", "")) |> 
  dplyr::select(term, name, estimate) |> 
  pivot_wider(names_from = term,
              values_from = estimate)

# Table 
kable(nxt.table, 
      booktabs = TRUE, 
      format = "latex",
      caption = "What do citizens expect parliamentarians to do next?",
      label = "nxt",
      escape = TRUE)

# /./ END OF CODE /./ #