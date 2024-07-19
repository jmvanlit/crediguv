##### Set-up -------------------------------------------------------------------
library(tidyverse)
library(haven) # for read.csv()
library(Hmisc) # for describe()
library(kableExtra)

##### Data import --------------------------------------------------------------
crediguv <- read.csv("data/crediguv.csv") |> 
  as_tibble() |> 
  
  # convert to factor
  mutate(
    party = as.factor(party),
    consistency = as.factor(consistency),
    group = as.factor(group))

colnames(crediguv)

### Function to get the summary statistics for intervals -----------------------
get_summary_stats <- function(df, vars) {
  df |> 
    select(all_of(vars)) |> 
    summarise(across(everything(), list(
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      N = ~sum(!is.na(.)),
      Missing = ~sum(is.na(.))
    ), .names = "{col}_{fn}"))
} 

### Treatment groups -----------------------------------------------------------
groups <- crediguv |> 
  filter(!is.na(dv_main)) |> 
  group_by(treatment_group) |> 
  summarise(N = n()) |> 
  mutate(Treatment = case_when(
    treatment_group == 1 ~ "Inconsistent Government",
    treatment_group == 2 ~ "Consistent Government",
    treatment_group == 3 ~ "Inconsistent Opposition",
    treatment_group == 4 ~ "Consistent Opposition")) |> 
  dplyr::select(Treatment, N)

# get total n
total_row <- groups |> 
    summarise(Treatment = "Total", N = sum(N))

# Combine the original dataset with the total row
groups <- bind_rows(groups, total_row)

### Dependent variables --------------------------------------------------------
dvs <- c("dv_main", "dv_nxt", "mv_info", "mv_mand", "mv_ploy")

summary_dvs <- get_summary_stats(crediguv, dvs) |> 
  rename_with(~ str_remove(., "^dv_")) |> 
  rename_with(~ str_remove(., "^mv_")) |> 
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "_") |> 
  mutate(value = round(value, 2)) |> 
  pivot_wider(names_from = "stat",
              values_from = "value")

kable(summary_dvs, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Descriptives: dependent variables",
      label = "desc-dvs",
      escape = TRUE)

### Interval covariates --------------------------------------------------------
ivs <- c("pol_int", "leri", 
         "pol_trust_gov", "pol_trust_med", "pol_trust_pol", "pol_trust_part", "pol_trust_parl", "pol_trust_crt",
         "dem_eval_elec", "dem_eval_media", "dem_eval_minor", "dem_eval_courts", "dem_eval_check", "dem_eval_opinion", "dem_eval_alt", "dem_eval_prot",
         "dem_sup", "dem_satis",
         "pop_will", "pop_impo", "pop_cont", "pop_regu", "pop_talk", "pop_comp", "pop_comp",
         "tech_man", "tech_cpx", "tech_evi", "tech_exp")

summary_ivs <- get_summary_stats(crediguv, ivs) |>  
  t() |>
  as.data.frame() |> 
  rownames_to_column() |> 
  rename("Variable" = "rowname",
         "value" = "V1") |> 
  mutate(stat = str_extract(Variable, "[^_]+$"),
         Variable = str_remove(Variable, "_[^_]+$")) |> 
  mutate(value = round(value, 2)) |> 
  pivot_wider(values_from = "value",
              names_from = "stat")

kable(summary_ivs, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Descriptives: independent variables",
      label = "desc-ivs",
      escape = TRUE)

### Categorical covariates -----------------------------------------------------
# Sex
crediguv |> 
  group_by(sex) |> 
  rename("Sex"= "sex") |> 
  summarise(Count = n()) |> 
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"),
         Percentage = paste0(round(Count / sum(Count), 4) * 100, "%")) |> 
  kable(booktabs = TRUE, 
        format = "latex",
        caption = "Descriptives: sex",
        label = "desc-sex",
        escape = TRUE)

# Education
crediguv |> 
  group_by(edu_3_rec) |> 
  rename("Education"= "edu_3_rec") |> 
  summarise(Count = n()) |> 
  mutate(
    Education = case_when(
      Education == 1 ~ "Low",
      Education == 2 ~ "Middle",
      Education == 3 ~ "High",
      is.na(Education) ~ "Missing"),
    Percentage = paste0(round(Count / sum(Count), 4) * 100, "%")) |>
  kable(booktabs = TRUE, 
        format = "latex",
        caption = "Descriptives: education",
        label = "desc-edu",
        escape = TRUE)

# Previous vote
crediguv |> 
  group_by(prev_vote) |> 
  rename("Previous vote"= "prev_vote") |> 
  summarise(Count = n()) |> 
  mutate(
    `Previous vote` = case_when(
      is.na(`Previous vote`) ~ "Missing",
      `Previous vote` == "NOTA" ~ "None of the above", 
      TRUE ~ `Previous vote`),
    Percentage = paste0(round(Count / sum(Count), 4) * 100, "%")) |>
  kable(booktabs = TRUE, 
        format = "latex",
        caption = "Descriptives: vote at 2021 parliamentary election",
        label = "desc-vote",
        escape = TRUE)

# Province
crediguv |> 
  group_by(geo) |> 
  rename("Province"= "geo") |> 
  summarise(Count = n()) |> 
  mutate(
    Province = case_when(
      Province == 1 ~ "Drenthe",
      Province == 2 ~ "Flevoland",
      Province == 3 ~ "Friesland",
      Province == 4 ~ "Gelderland",
      Province == 5 ~ "Groningen",
      Province == 6 ~ "Limburg",
      Province == 7 ~ "Noord-Brabant",
      Province == 8 ~ "Noord-Holland",
      Province == 9 ~ "Overijssel",
      Province == 10 ~ "Utrecht",
      Province == 11 ~ "Zeeland",
      Province == 12 ~ "Zuid-Holland",
      is.na(Province) ~ "Missing"),
    Percentage = paste0(round(Count / sum(Count), 4) * 100, "%")) |>
  kable(booktabs = TRUE, 
        format = "latex",
        caption = "Descriptives: provinces",
        label = "desc-geo",
        escape = TRUE)

# Income
crediguv |> 
  group_by(income) |> 
  rename("Income"= "income") |> 
  summarise(Count = n()) |> 
  mutate(
    Income = case_when(
      Income == 1 ~ "Less than € 15285",
      Income == 2 ~ "€ 15285 to € 1998",
      Income == 3 ~ "€ 19987 to € 23929",
      Income == 4 ~ "€ 23929 to € 28584",
      Income == 5 ~ " € 28584 to € 34518",
      Income == 6 ~ "€ 34518 to € 42261",
      Income == 7 ~ "€ 42261 to € 51218",
      Income == 8 ~ "€ 51218 to € 61402",
      Income == 9 ~ "€ 61402 to € 76968",
      Income == 10 ~ "More than € 76968",
      is.na(Income) ~ "Missing"),
    Percentage = paste0(round(Count / sum(Count), 4) * 100, "%")) |>
  kable(booktabs = TRUE, 
        format = "latex",
        caption = "Descriptives: income",
        label = "desc-inc",
        escape = TRUE)

# /./ End of Code /./ #