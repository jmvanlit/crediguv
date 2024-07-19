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
library(gghalves)
library(lavaan) # do we want to use this for sem()

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

##### Attention ----------------------------------------------------------------
attman <- crediguv |>
  dplyr::select(ResponseId, treatment_group, consistency, party, dv_main, party, consistency, attention, mc_part, mc_cons, manipulation)

##### Manipulation and Attention Checks ----------------------------------------
att <- attman |> 
  group_by(attention) |> 
  summarise(count = n()) |> 
  mutate(attention = case_when(
    attention == 0 ~ "Failed",
    attention == 1 ~ "Passed"))

man <- attman |> 
  group_by(manipulation) |> 
  summarise(count = n()) |> 
  mutate(manipulation = case_when(
    manipulation == 2 ~ "Both correct",
    manipulation == 1 ~ "1 correct",
    manipulation == 0 ~ "0 correct"))

man.part <- attman |> 
  group_by(mc_part) |> 
  summarise(count = n()) |> 
  mutate(mc_part = case_when(
    mc_part == 0 ~ "Incorrect",
    TRUE ~ "Correct")) |> 
  rename("Manipulation Check" = mc_part,
         "Party affiliation"= count)

man.cons <- attman |> 
  group_by(mc_cons) |> 
  summarise(count = n()) |> 
  mutate(mc_cons = case_when(
    mc_cons == 0 ~ "Incorrect",
    TRUE ~ "Correct")) |> 
  rename("Manipulation Check" = mc_cons,
         "Consistency"= count)

man.spec <- man.part |> 
  left_join(man.cons)

kable(att,
      col.names = c("Attention check", "Count"),
      caption = "Attention Check",
      label = "attention",
      format = "latex",
      booktabs = TRUE)

kable(man,
      col.names = c("Manipulation check", "Count"),
      caption = "Manipulation Check",
      label = "manipulation",
      format = "latex",
      booktabs = TRUE)

kable(man.spec,
      caption = "Manipulation Checks per Treatment",
      label = "manipulation2",
      format = "latex",
      booktabs = TRUE)

### Controlling for attention and manipulation ----
sm.att  <- lm(data = attman,
              
              dv_main ~
                
                # attention
                as.factor(attention) +
                
                # treatments
                party + consistency)

im.att  <- lm(data = attman,
              
              dv_main ~
                
                # attention
                as.factor(attention) +
                
                # treatments
                party + consistency + party * consistency)

sm.man  <- lm(data = attman,
              
              dv_main ~
                
                # attention
                as.factor(manipulation) +
                
                # treatments
                party + consistency)

im.man  <- lm(data = attman,
              
              dv_main ~
                
                # attention
                as.factor(manipulation) +
                
                # treatments
                party + consistency + party * consistency)

summary(im.man)

attman.cntrl <- list(sm.att, im.att, sm.man, im.man)

texreg(attman.cntrl,
       caption = "Controlling for attention and manipulation",
       label = "tab:attman",
       caption.above = TRUE,
       custom.model.names = c("Attention (no interaction)", "Attention (with interaction)", 
                              "Manipulation (no interaction)", "Manipulation (with interaction)"))

##### Heterogeneous Treatment Effects ------------------------------------------

### Party effects --------------------------------------------------------------
poppa <- readRDS("data/poppa_integrated.rds") |> 
  filter(country == "Netherlands") |> 
  filter(wave == "Wave 2 - 2023") |> 
  dplyr::select(party_short, party_name_original, lroverall) |> 
  mutate(
    
    # create left-right categories
    LR = case_when(
      lroverall < 2.5 ~ "far-left",
      lroverall >= 2.5 & lroverall < 5 ~ "center-left",
      lroverall == 5 ~ "center", # does not exist, but added for completeness
      lroverall > 5 & lroverall <= 7.5 ~ "center-right",
      lroverall > 7.5 ~ "far-right"),
    
    # change names so they match with crediguv
    party_short = case_when(
      party_short == "50+" ~ "50PLUS",
      TRUE ~ party_short))

parties <- crediguv |> 
  left_join(poppa, by = c("prev_vote" = "party_short"))

far_left <- parties |> 
  filter(LR == "far-left")

center_left <- parties |> 
  filter(LR == "center-left")

center_right <- parties |> 
  filter(LR == "center-right")

far_right <- parties |> 
  filter(LR == "far-right")

fl.sm <- lm(dv_main ~ party + consistency, data = far_left)
cl.sm <- lm(dv_main ~ party + consistency, data = center_left)
cr.sm <- lm(dv_main ~ party + consistency, data = center_right)
fr.sm <- lm(dv_main ~ party + consistency, data = far_right)

fl.im <- lm(dv_main ~ party + consistency + party*consistency, data = far_left)
cl.im <- lm(dv_main ~ party + consistency + party*consistency, data = center_left)
cr.im <- lm(dv_main ~ party + consistency + party*consistency, data = center_right)
fr.im <- lm(dv_main ~ party + consistency + party*consistency, data = far_right)

summary(cr.im)

models_eval_parties <- list(fl.sm, fl.im, cl.sm, cl.im, cr.sm, cr.im, fr.sm, fr.im)

texreg(models_eval_parties,
       caption = "Does credibility differ between different party-affiliations?",
       caption.above = TRUE,
       label = "tab:parties",
       custom.header = list("Far-left" = 1:2, "Center-left" = 3:4, "Center-right" = 5:6, "Far-right" = 7:8),
       custom.model.names = c("H1", "H2 and H3", "H1", "H2 and H3", "H1", "H2 and H3", "H1", "H2 and H3"))

# data preparation
fl.plot <- fl.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Far-left")
  
cl.plot <- cl.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Center-left")

cr.plot <- cr.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Center-right")

fr.plot <- fr.im |>
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Far-right")

# number of observations
fl.n <- nobs(fl.im)
cl.n <- nobs(cl.im)
cr.n <- nobs(cr.im)
fr.n <- nobs(fr.im)

parties.plot <- bind_rows(fl.plot, cl.plot, cr.plot, fr.plot) |> 
  mutate(
    order = case_when(
      term == "partygov" ~ 1,
      term == "consistencyinc" ~ 2,
      term == "partygov:consistencyinc" ~ 3),
    term = case_when(
      term == "partygov" ~ "**Government-affiliated**  \ndemocratic defence",
      term == "consistencyinc" ~ "**Inconsistent**  \ndemocratic defence",
      term == "partygov:consistencyinc" ~ "Government * Inconstency"),
    type2 = case_when(
      type == "Far-left" ~ paste0(type, "  \n*n = ", fl.n, "*"),
      type == "Center-left" ~ paste0(type, "  \n*n = ", cl.n, "*"),
      type == "Center-right" ~ paste0(type, "  \n*n = ", cr.n, "*"),
      type == "Far-right" ~ paste0(type, "  \n*n = ", fr.n, "*")))

# plot parameters
party.colours <- c("Far-left" = "#73a2ac",
                   "Center-left" = "#4c8ca5",
                   "Center-right" = "#26759f",
                   "Far-right" =  "#0b5d96")

# set order
parties.plot$type2 <- factor(parties.plot$type2, 
                             levels = c(paste0("Far-left", "  \n*n = ", fl.n, "*"), 
                                        paste0("Center-left", "  \n*n = ", cl.n, "*"),
                                        paste0("Center-right", "  \n*n = ", cr.n, "*"),
                                        paste0("Far-right", "  \n*n = ", fr.n, "*")))

appendix_fig1 <- 
ggplot(data = parties.plot,
         aes(x = estimate,
             y = reorder(term, desc(order)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = type,
                 shape = type),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = type),
                 height = 0,
                 linewidth = 0.6) +
  
  # faceting
  facet_grid(cols = vars(type2),
             scales = "free",
             space = "free_y") +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = party.colours) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown()) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL)

# ... and save!
ggsave(filename  = "figures/appendix_fig1.png",
       plot = appendix_fig1,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

##### LiRe Self-evaluation effects ---------------------------------------------
left <- crediguv |> 
  filter(leri <= 3)

center <- crediguv |> 
  filter(leri >= 4 & leri <= 6)

right <- crediguv |> 
  filter(leri >= 7)

# split samples
left.sm <- lm(dv_main ~ party + consistency, data = left)
center.sm <- lm(dv_main ~ party + consistency, data = center)
right.sm <- lm(dv_main ~ party + consistency, data = right)

left.im <- lm(dv_main ~ party + consistency + party * consistency, data = left)
center.im <- lm(dv_main ~ party + consistency + party * consistency, data = center)
right.im <- lm(dv_main ~ party + consistency + party * consistency, data = right)

summary(left.sm)
summary(right.im)


models_leri <- list(left.sm, left.im, center.sm, center.im, right.sm, right.im)

texreg(models_leri,
       caption = "Does credibility differ if we split samples according to left-right self-placement?",
       caption.above = TRUE,
       label = "tab:leri",
       custom.header = list("Left" = 1:2, "Center" = 3:4, "Right" = 5:6),
       custom.model.names = c("H1", "H2 and H3", "H1", "H2 and H3", "H1", "H2 and H3"))

# data preparation
left.plot <- left.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Left")

center.plot <- center.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Center")

right.plot <- right.im |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)")) |> 
  mutate(type = "Right")

# number of observations
left.n <- nobs(left.im)
center.n <- nobs(center.im)
right.n <- nobs(right.im)

leri.plot <- bind_rows(left.plot, center.plot, right.plot) |> 
  mutate(
    order = case_when(
      term == "partygov" ~ 1,
      term == "consistencyinc" ~ 2,
      term == "partygov:consistencyinc" ~ 3),
    term = case_when(
      term == "partygov" ~ "**Government-affiliated**  \ndemocratic defence",
      term == "consistencyinc" ~ "**Inconsistent**  \ndemocratic defence",
      term == "partygov:consistencyinc" ~ "Government * Inconstency"),
    type2 = case_when(
      type == "Left" ~ paste0(type, "  \n*n = ", left.n, "*"),
      type == "Center" ~ paste0(type, "  \n*n = ", center.n, "*"),
      type == "Right" ~ paste0(type, "  \n*n = ", right.n, "*")))

# plot parameters
leri.colours <- c("Left" = "#FF5154",
                  "Center" = "#84DCCF",
                  "Right" = "#2A1E5C")

# set order
leri.plot$type2 <- factor(leri.plot$type2, 
                          levels = c(paste0("Left", "  \n*n = ", left.n, "*"), 
                                     paste0("Center", "  \n*n = ", center.n, "*"),
                                     paste0("Right", "  \n*n = ", right.n, "*")))

# plot figure 8 ...
appendix_fig2 <- 
ggplot(data = leri.plot,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = type,
                 shape = type),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = type),
                 height = 0,
                 linewidth = 0.6) +
  
  # faceting
  facet_grid(cols = vars(type2),
             scales = "free",
             space = "free_y") +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = leri.colours) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown()) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL)

# ... and save!
ggsave(filename  = "figures/appendix_fig2.png",
       plot = appendix_fig2,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

##### Weighted Sample ----------------------------------------------------------

weight <- crediguv |> 
  dplyr::select(ResponseId, treatment_group, weight, etn, prev_vote, pop_comp, 
                pop_will, pop_impo, pop_regu, emp_school, know_minfin, dem_eval_alt)

m1.weight <- lm(dv_main ~ party + consistency +
                
                # unbalanced in the weighted data
                as.factor(etn) + prev_vote + pop_comp + pop_will + pop_impo + pop_regu +
                emp_school + know_minfin + dem_eval_alt,
                
                data = crediguv,
                weights = weight)

m2.weight <- lm(dv_main ~ party + consistency + party * consistency +
                
                # unbalanced in the weighted data
                as.factor(etn) + prev_vote + pop_comp + pop_will + pop_impo + pop_regu +
                emp_school + know_minfin + dem_eval_alt,

                data = crediguv,
                weights = weight)

weighted_models <- list(m1.weight, m2.weight)

texreg(weighted_models,
       caption = "Weighted models",
       caption.above = TRUE,
       label = "tab:weight",
       custom.model.names = c("Model 1 (no interactions)", "Model 2 (with interactions)"))

##### Groups as Treatments 
m4.weight <- lm(data = crediguv,
                dv_main ~ group + 

                  # unbalanced in the weighted data
                  as.factor(etn) + prev_vote + pop_comp + pop_will + pop_impo + pop_regu +
                  emp_school + know_minfin + dem_eval_alt,, 
                
                weights = weight)

m4.intercept.weight <- coef(m4.weight)[1]

m4.interaction.weight <- m4.weight |> 
  tidy(conf.int = TRUE) |> 
  filter(term %in% c("(Intercept)", "groupConsistent Government", 
                "groupInconsistent Government", "groupInconsistent Opposition")) |> 
  mutate(
    group = case_when(term == "(Intercept)" ~ "Consistent Opposition",
                      term == "groupConsistent Government" ~ "Consistent Government",
                      term == "groupInconsistent Government" ~ "Inconsistent Government",
                      term == "groupInconsistent Opposition" ~ "Inconsistent Opposition"),
    fitted_mean = case_when(
      term != "(Intercept)" ~ m4.intercept.weight + estimate,
      TRUE ~ estimate),
    fitted_low = fitted_mean - 1.96 * std.error,
    fitted_high = fitted_mean + 1.96 * std.error,
    consistency = ifelse(str_detect(term, "groupInconsistent"), "Inconsistent", "Consistent"),
    party = ifelse(str_detect(term,  "Government"), "Government", "Opposition"))

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
# plot parameters
fig5.colours <- c("Government" = "#73a2ac",
                  "Opposition" = "#0b5d96")

fig5.legend <- c("Government" = "**Government-affiliated**  \ndemocratic defence",
                 "Opposition" = "**Opposition**  \ndemocratic defence")

appendix_fig3 <-
ggplot(data = m4.interaction.weight,
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
  scale_y_continuous(limits = c(2.5, 6), breaks = seq(2, 6))

# ... and save
ggsave(filename  = "figures/appendix_fig3.png",
       plot = appendix_fig3,
       width = 14,
       height = 14,
       dpi = 300,
       units = "cm")

##### Alternative Mediation Models ---------------------------------------------
### mediation from consistency to mandate and info
# just to check
# mm for mediation model
mm.con1 <- '
# outcome model 
dv_main ~ b1*party + b2*consistency + m1*mv_mand + m2*mv_info

# mediator models
mv_mand ~ a1*consistency
mv_info ~ a2*consistency


# model correlation between mediators
mv_mand ~~ mv_info 
'

fit_mm.con1<- sem(mm.con1, data = crediguv_mediate)

summary(fit_mm.con1, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

### full with every mediator together?
# sure, but no different results than the seperate mediation models, and because it is a mess of arrows, do not use
mm.full <- '
# outcome model
dv_main ~ b1*congov + b2*incopp + b3*incgov + m1*mv_mand + m2*mv_info + m3*mv_ploy

# mediator models
mv_mand ~ a1*congov + a2*incopp + a3*incgov
mv_info ~ a4*congov + a6*incopp + a6*incgov
mv_ploy ~ a7*congov + a8*incopp + a9*incgov

# correlation between mediators
mv_info ~~ mv_mand
mv_info ~~ mv_ploy
mv_mand ~~ mv_ploy
'

fit_mm.full <- sem(mm.full, data = crediguv_mediate)

summary(fit_mm.full, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)

#### Modifications checks for the mediation models -----------------------------

# Party
modificationindices(fit_mm.party, 
                    standardized = TRUE, 
                    power = TRUE, 
                    delta = 0.1, 
                    alpha = 0.05, 
                    high.power = 0.75, 
                    sort. = TRUE)


# Consistency
modificationindices(fit_mm.consistency, 
                    standardized = TRUE, 
                    power = TRUE, 
                    delta = 0.1, 
                    alpha = 0.05, 
                    high.power = 0.75, 
                    sort. = TRUE)

# /./ End of Code /./ #