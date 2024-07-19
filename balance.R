### Libraries ------------------------------------------------------------------
library(dplyr) # for pipelines
library(haven) # to read .sav
library(cobalt) # for the balance test
library(tibble) # to convert rownames to a variable
library(knitr)
library(kableExtra)

##### Data import --------------------------------------------------------------
crediguv <- read.csv("data/crediguv.csv")

##### Balance checks -----------------------------------------------------------
covs <-  crediguv |>
  rename(treat = treatment_group) |> 
  dplyr::select(
    # outcome
    treat,
    
    # potential confounders
    pol_int, lire, 
    pol_trust_gov, pol_trust_med, pol_trust_pol, pol_trust_part, pol_trust_parl, pol_trust_crt,
    know_unemp, know_minfin, know_un,
    dem_eval_elec, dem_eval_media, dem_eval_minor, dem_eval_courts, dem_eval_check, dem_eval_opinion, dem_eval_alt, dem_eval_prot,
    dem_sup, dem_satis,
    pop_will, pop_impo, pop_cont, pop_regu, pop_talk, pop_comp, pop_comp,
    tech_man, tech_cpx, tech_evi, tech_exp, 
    sex, edu, etn, geo, prev_vote, income, 
    emp_school, emp_student, emp_self, emp_full, emp_part, emp_diff, emp_parent, emp_pension, emp_unemp, emp_diff2,
    
    # weights
    weight)

covs_unweighted <- covs

balance_unweighted <- bal.tab(data = covs_unweighted,
                              thresholds = c(m = 0.05)[[1]],
                              x = treat ~ pol_int + lire +
                              pol_trust_gov + pol_trust_med + pol_trust_pol + pol_trust_part + pol_trust_parl + pol_trust_crt +
                              know_unemp + know_minfin + know_un +
                              dem_eval_elec + dem_eval_media + dem_eval_minor + dem_eval_courts + dem_eval_check + dem_eval_opinion + dem_eval_alt + dem_eval_prot +
                              dem_sup + dem_satis +
                              pop_will + pop_impo + pop_cont + pop_regu + pop_talk + pop_comp + pop_comp +
                              tech_man + tech_cpx + tech_evi + tech_exp + 
                              sex + edu + etn + geo + prev_vote + income +
                              emp_school + emp_student + emp_self + emp_full + emp_part + emp_diff + emp_parent + emp_pension + emp_unemp + emp_diff2)

balance_unweighted <- balance_unweighted$Balance |> 
  rownames_to_column(var = "variable") |> 
  dplyr::select(variable, Corr.Un, R.Threshold.Un) |> 
  rename(correlation = Corr.Un,
         balance = R.Threshold.Un) |> 
  mutate(correlation = round(correlation, 3)) |> 
  arrange(desc(abs(correlation))) |> 
  top_n(n = 20,
        wt = abs(correlation))

# Table 
kable(balance_unweighted, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Top-20 most unbalanced covariates",
      label = "bal_unw",
      escape = TRUE)

covs_weighted <- covs |> 
  filter(!is.na(weight))

balance_weighted <- bal.tab(data = covs_weighted,
                            weights = "weight",
                            thresholds = c(m = 0.05)[[1]],
                            x = treat ~ pol_int + lire +
                              pol_trust_gov + pol_trust_med + pol_trust_pol + pol_trust_part + pol_trust_parl + pol_trust_crt +
                              know_unemp + know_minfin + know_un +
                              dem_eval_elec + dem_eval_media + dem_eval_minor + dem_eval_courts + dem_eval_check + dem_eval_opinion + dem_eval_alt + dem_eval_prot +
                              dem_sup + dem_satis +
                              pop_will + pop_impo + pop_cont + pop_regu + pop_talk + pop_comp + pop_comp +
                              tech_man + tech_cpx + tech_evi + tech_exp + 
                              sex + edu + etn + geo + prev_vote + income +
                              emp_school + emp_student + emp_self + emp_full + emp_part + emp_diff + emp_parent + emp_pension + emp_unemp + emp_diff2)

balance_weighted <- balance_weighted$Balance |> 
  rownames_to_column(var = "variable") |> 
  dplyr::select(variable, Corr.Adj, R.Threshold) |> 
  rename(correlation = Corr.Adj,
         balance = R.Threshold) |> 
  mutate(correlation = round(correlation, 3)) |> 
  arrange(desc(abs(correlation))) |> 
  top_n(n = 20,
        wt = abs(correlation))

# Table 
kable(balance_weighted, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Top-20 most unbalanced covariates for the weighted data",
      label = "bal_w",
      escape = TRUE)
