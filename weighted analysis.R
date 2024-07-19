##### Define new functions
# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ group"))
  model <- lm(lm_formula, data = crediguv, weight = weight)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

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
library(writexl) 

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
         dv_main ~ party + consistency,
         weight = weight)

##### Interaction Model: H2, H3 ------------------------------------------------
m2 <- lm(data = crediguv,
         dv_main ~ party + consistency + (party * consistency),
         weight = weight)

##### Table Main Models --------------------------------------------------------
main_models <- list(m1, m2)

texreg(main_models,
        caption = "When are elite democratic defenders credible? (weighted)",
        caption.above = TRUE,
        label = "tab:weightedmodels",
        custom.coef.names = c("Intercept", "Coalition", "Inconsistency", "Coalition * Inconsistency"))

##### Figure 1 -----------------------------------------------------------------

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
  mutate(order = factor(term, levels = c("partycoa", "consistencyinc", "partycoa:consistencyinc"))) |> 
  mutate(term = case_when(
    term == "partycoa" ~ "**Coalition**  \ndemocratic defender",
    term == "consistencyinc" ~ "**Inconsistent**  \ncommitment to democracy",
    term == "partycoa:consistencyinc" ~ "**Inconsistent * Government**  \ndemocratic defence")) |> 
  mutate(term = fct_reorder(term, desc(order)))

# plot parameters
fig1.colours <- c("m1" = "#73a2ac",
                  "m2" = "#0b5d96")

fig1.legend <- c("m1" = "**Hypothesis 1**  \nSimple model",
                 "m2" = "**Hypotheses 2 and 3**  \nInteraction model")

# plot...
#fig1.weighted <- 
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
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = fig1.legend) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0))

# ... and save! 
ggsave(filename  = "figures/fig1.weighted.png",
       plot = fig1.weighted,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

##### Groups as Treatments -----------------------------------------------------
m4 <- lm(data = crediguv,
         dv_main ~ group,
         weight = weight)

summary(m4)

m4.intercept <- coef(m4)[1]

m.interaction <- m4 |> 
  tidy(conf.int = TRUE) |> 
    mutate(
      group = case_when(term == "(Intercept)" ~ "Consistent Opposition",
                      term == "groupConsistent Coalition" ~ "Consistent Coalition",
                      term == "groupInconsistent Coalition" ~ "Inconsistent Coalition",
                      term == "groupInconsistent Opposition" ~ "Inconsistent Opposition"),
      fitted_mean = case_when(
        term != "(Intercept)" ~ m4.intercept + estimate,
        TRUE ~ estimate),
      fitted_low = fitted_mean - 1.96 * std.error,
      fitted_high = fitted_mean + 1.96 * std.error,
      consistency = ifelse(str_detect(term, "groupInconsistent"), "Inconsistent", "Consistent"),
      party = ifelse(str_detect(term,  "Coalition"), "Coalition", "Opposition"),
      # decide if we want these labels somewhere
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""),
      label = paste(round(fitted_mean, 2), stars, sep = ""))

##### Figure 2 -----------------------------------------------------------------
# plot parameters
raincloud.colours <- c("Consistent Opposition" = "#868686",
                       "Consistent Coalition" = "#d9af6b",
                       "Inconsistent Coalition" = "#975348",
                       "Inconsistent Opposition" = "#573c4c")

raincloud.colours.2 <- c("Consistent Opposition" = "#9ab89a",
                       "Consistent Coalition" = "#799779",
                       "Inconsistent Coalition" = "#475c46",
                       "Inconsistent Opposition" = "#141e15")

raincloud.colours.3 <- c("Consistent Opposition" = "#c2528b",
                       "Consistent Coalition" = "#f380b9",
                       "Inconsistent Coalition" = "#5f50a9",
                       "Inconsistent Opposition" = "#9652c2")

raincloud.colours.4 <- c("Consistent Opposition" = "#ff8d70",
                       "Consistent Coalition" = "#fcaa82",
                       "Inconsistent Coalition" = "#73a2ac",
                       "Inconsistent Opposition" = "#0b5d96")

raincloud.legend <- c("Consistent Opposition" = "1",
                      "Consistent Coalition" = "2",
                      "Inconsistent Coalition" = "3",
                      "Inconsistent Opposition" = "4")

# plot ...
#fig2.weighted <-
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
                            aes(colour = group)) +
  
  # distribution
  ggdist::stat_halfeye(adjust = 3, 
                       width = .3,
                       justification = -0.4,
                       .width = 0,
                       point_colour = NA,
                       alpha = 0.8,
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
  scale_fill_manual(values = raincloud.colours.2,
                    labels = raincloud.legend) +
  scale_colour_manual(values = raincloud.colours.2,
                      labels = raincloud.legend) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(xlim = c(1.2, 3.9), clip = "off") +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7))
  
# ... and save
ggsave(filename  = "figures/fig2_weighted.png",
       plot = fig2.weighted,
       width = 18,
       height = 10,
       dpi = 300,
       units = "cm")

##### Figure 3 -----------------------------------------------------------------
# plot parameters
fig3.colours <- c("Coalition" = "#73a2ac",
                  "Opposition" = "#0b5d96")

fig3.legend <- c("Coalition" = "**Coalition**  \ndemocratic defence",
                 "Opposition" = "**Opposition**  \ndemocratic defence")

# plot ...
#fig3.weighted <- 
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
  scale_colour_manual(values = fig3.colours,
                      labels = fig3.legend) +
  scale_shape_manual(values = c(15, 19),
                    labels = fig3.legend) +
  theme(legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  coord_cartesian(xlim = c(1.4, 1.6), clip = "off") +
  scale_y_continuous(limits = c(3, 5.5), breaks = seq(2, 6))

# ... and save
ggsave(filename  = "figures/fig3_weighted.png",
       plot = fig3.weighted,
       width = 14,
       height = 14,
       dpi = 300,
       units = "cm")

##### Mediation ----------------------------------------------------------------
### Mandate
cm.mand.xy <- lm(dv_main ~ party + consistency,
                 data = crediguv,
                 weight = weight)

cm.mand.xm <- lm(mv_mand ~ party + consistency,
                 data = crediguv,
                 weight = weight)

cm.mand.xmy <- lm(dv_main ~ party + consistency + mv_mand,
                  data = crediguv,
                  weight = weight)

texreg(list(cm.mand.xy, cm.mand.xm, cm.mand.xmy),
       custom.header = list("Dependent Variable:" = 1:3),
       custom.model.names = c("Credibility", "Mandate", "Credibility"),
       label = "tab:mand",
       caption = "Mandate Mediation Models",
       caption.above = TRUE)

cm.mand.cor <- mediation::mediate(cm.mand.xm, cm.mand.xmy,
                                  treat = "party",
                                  treat.value = "gov",
                                  control.value = "opp",
                                  mediator = "mv_mand")

summary(cm.mand.cor)

### Information
cm.info.xy <- lm(dv_main ~ party + consistency,
                 data = crediguv,
                 weight = weight)

cm.info.xm <- lm(mv_info ~ party + consistency,
                 data = crediguv,
                 weight = weight)

cm.info.xmy <- lm(dv_main ~ party + consistency + mv_info,
                  data = crediguv,
                  weight = weight)

texreg(list(cm.info.xy, cm.info.xm, cm.info.xmy),
       custom.header = list("Dependent Variable:" = 1:3),
       custom.model.names = c("Credibility", "Information", "Credibility"),
       label = "tab:info",
       caption = "Information Mediation Models",
       caption.above = TRUE)

cm.info.cor <- mediation::mediate(cm.info.xm, cm.info.xmy,
                                  treat = "party",
                                  treat.value = "gov",
                                  control.value = "opp",
                                  mediator = "mv_info")

summary(cm.info.cor)

model <- lm(dv_main ~ party + consistency, data = crediguv)
mediator_model <- lm(cbind(mv_info, mv_mand) ~ party + consistency, data = crediguv)

summary(mediator_model)

# Conduct mediation analysis
mediation_results <- mediate(model, mediator_model, sims = 1000)

# View mediation results
summary(mediation_results)

### Ploy
# figure out how to do interaction here
# just do it on treatment groups
cm.ploy.xy <- lm(dv_main ~ group,
                 data = crediguv,
                 weight = weight)

cm.ploy.xm <- lm(mv_ploy ~ group,
                 data = crediguv,
                 weight = weight)

cm.ploy.xmy <- lm(dv_main ~ group + mv_ploy,
                  data = crediguv,
                  weight = weight)

texreg(list(cm.ploy.xy, cm.ploy.xm, cm.ploy.xmy),
       custom.header = list("Dependent Variable:" = 1:3),
       custom.model.names = c("Credibility", "Ploy", "Credibility"),
       label = "tab:ploy",
       caption = "Ploy Mediation Models",
       caption.above = TRUE)

cm.ploy.cor.opp <- mediation::mediate(cm.ploy.xm, cm.ploy.xmy,
                                  treat = "group",
                                  treat.value = "Consistent Opposition",
                                  control.value = "Inconsistent Opposition",
                                  mediator = "mv_ploy")

cm.ploy.cor.coa <- mediation::mediate(cm.ploy.xm, cm.ploy.xmy,
                                      treat = "group",
                                      treat.value = "Inconsistent Coalition",
                                      control.value = "Consistent Coalition",
                                      mediator = "mv_ploy")


summary(cm.ploy.cor.opp)
summary(cm.ploy.cor.coa)

### Ploy

##### lavaan -------------------------------------------------------------------
model <- '
# outcome model 
dv_main ~ b1*party + b2*consistency + b3*mv_mand + b4*mv_info

# mediator models
mv_mand ~ a1*party 
mv_info ~ a2*party

# indirect effects (IDE)
mv_mandIDE  := a1*b3
mv_infoIDE  := a2*b4
sumIDE := (a1*b3) + (a2*b4)

contrast := mv_mandIDE - mv_infoIDE

# total effect
total := b1 + b2 + (a1*b3) + (a2*b4)
mv_mand ~~ mv_info # model correlation between mediators
'

fit <- sem(model, data = crediguv)

summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")

boot.fit

modificationindices(fit, standardized = TRUE, power = TRUE, delta = 0.1, alpha = 0.05,
                    high.power = 0.75, sort. = TRUE)

##### Next Steps Effects -------------------------------------------------------
### some overall parameters

# define all dvs
nxt.dvs <- c("dv_nxt_scaled", "dv_nxt_1", "dv_nxt_2", "dv_nxt_3", "dv_nxt_4", "dv_nxt_5", 
             "dv_nxt_6", "dv_nxt_7", "dv_nxt_8", "dv_nxt_9")

### simple model
sm.nxt <- map_df(nxt.dvs, run_multiple_lm) |> 
  mutate(dv = factor(dv, levels = c("dv_nxt_scaled", "dv_nxt_7", "dv_nxt_6", "dv_nxt_1", "dv_nxt_3",
                                    "dv_nxt_4", "dv_nxt_5", "dv_nxt_2", "dv_nxt_8", "dv_nxt_9"))) |> 
  group_by(dv) |> 
  mutate(
    fitted_mean = case_when(
      term == "(Intercept)" ~ estimate,
      term!= "(Intercept)" ~ estimate + first(estimate[term == "(Intercept)"])),
    fitted_low = fitted_mean - 1.96 * std.error,
    fitted_high = fitted_mean + 1.96 * std.error,
    term = case_when(
      term == "(Intercept)" ~ "Consistent Opposition",
      term == "groupConsistent Coalition" ~ "Consistent Coalition",
      term == "groupInconsistent Coalition" ~ "Inconsistent Coalition",
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

sm.nxt

# update table to show fitted means, not coefficients
#### Table for Nxt-dv ----
nxt.table <- sm.nxt |> 
  mutate(sig = case_when(p.value < 0.05 ~ "*",
                         p.value < 0.01 ~ "**",
                         p.value < 0.001 ~ "***",
                         TRUE ~ ""),
         stat = str_c(round(estimate, 2), " (", round(std.error,2 ), ")", sig)) |> 
  dplyr::select(term, dv, stat) |> 
  pivot_wider(names_from = dv, values_from = stat) |> 
  t()

# Table 
kable(nxt.table, 
      booktabs = TRUE, 
      format = "latex",
      caption = "What do citizens expect parliamentarians to do next?",
      label = "nxt",
      escape = TRUE)

##### Figure 4 -----------------------------------------------------------------
# plot parameters
nxt.colours.2 <- c("Consistent Opposition" = "#0b5d96",
                   "Consistent Coalition" = "#73a2ac",
                   "Inconsistent Coalition" = "#73a2ac",
                   "Inconsistent Opposition" = "#0b5d96")

nxt.shape.2 <- c("Consistent Opposition" = 23,
                   "Consistent Coalition" = 23,
                   "Inconsistent Coalition" = 22,
                   "Inconsistent Opposition" = 22)

nxt.linetype.2 <- c("Consistent Opposition" = "solid",
                 "Consistent Coalition" = "longdash",
                 "Inconsistent Coalition" = "longdash",
                 "Inconsistent Opposition" = "solid")

# plot ...
fig4.weighted <- 
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
  scale_colour_manual(values = nxt.colours.2) +
  scale_fill_manual(values = nxt.colours.2) +
  scale_shape_manual(values = nxt.shape.2) +
  scale_linetype_manual(values = nxt.linetype.2) +
  
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
ggsave(filename  = "figures/fig4_weighted.png",
       plot = fig4.weighted,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")



# /./ END OF CODE /./ #