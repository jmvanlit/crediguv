##### Set-up -------------------------------------------------------------------
library(haven)
library(dplyr)

###### Data import and cleaning ------------------------------------------------
crediguv <- read_sav("data/NLOZ23-10_Veni+final_231114.sav") |> 
  dplyr::select(
    # treatments
    treatment_group,
    
    # outcomes
    dv_main,
    dv_nxt_1, dv_nxt_2, dv_nxt_3, dv_nxt_4, dv_nxt_5, dv_nxt_6, dv_nxt_7, dv_nxt_8, dv_nxt_9, dv_nxt_8_TEXT,
    mv_info, mv_mand, mv_ploy,
    
    # manipulation
    mc_part, mc_cons,
    
    # attention
    attention_post, attention_id, attention_back, attention_int, attention_cntrl,
    
    # demographics
    byear, sex_rec, edu_clean, edu_3_rec, etn_3_rec, geo_prov_rec, urb, vote.recall_rec, Q38.income.category,
    starts_with("Q12_"),
    
    # weight
    weight,
    
    # potential confounders
    pol_int, LR, 
    pol_trust_gov, pol_trust_med, pol_trust_pol, pol_trust_part, pol_trust_parl, pol_trust_crt,
    know_unemp, know_minfin, know_un,
    dem_eval_elec, dem_eval_media, dem_eval_minor, dem_eval_courts, dem_eval_check, dem_eval_opinion, dem_eval_alt, dem_eval_prot,
    dem_sup, dem_satis,
    pop_will, pop_impo, pop_cont, pop_regu, pop_talk, pop_comp, pop_comp,
    tech_man, tech_cpx, tech_evi, tech_exp, 
    
    # meta info
    ResponseId, To_weight
  ) |> 
  
  # rename for clarity
  rename(leri = LR, 
         sex = sex_rec,
         edu = edu_clean,
         etn = etn_3_rec,
         geo = geo_prov_rec,
         prev_vote = vote.recall_rec,
         income = Q38.income.category,
         emp_school = Q12_1,
         emp_student = Q12_2,
         emp_self = Q12_3,
         emp_full = Q12_4,
         emp_part = Q12_5,
         emp_diff = Q12_6,
         emp_parent = Q12_7,
         emp_pension = Q12_8,
         emp_unemp = Q12_9,
         emp_diff2 = Q12_10,
         emp_opn = Q12_10_TEXT) |> 
  
  mutate(
    # party names
    prev_vote = case_when(
      prev_vote == 1 ~ "VVD",
      prev_vote == 2 ~ "D66",
      prev_vote == 3 ~ "PVV",
      prev_vote == 4 ~ "CDA",
      prev_vote == 5 ~ "SP",
      prev_vote == 6 ~ "PvdA",
      prev_vote == 7 ~ "GL",
      prev_vote == 8 ~ "FvD",
      prev_vote == 9 ~ "PvdD",
      prev_vote == 10 ~ "CU",
      prev_vote == 11 ~ "JA21",
      prev_vote == 12 ~ "SGP",
      prev_vote == 13 ~ "Volt",
      prev_vote == 14 ~ "DENK",
      prev_vote == 15 ~ "50PLUS",
      prev_vote == 16 ~ "BBB",
      prev_vote == 17 ~ "BIJ1",
      prev_vote == 66 ~ "NOTA", # none of the above
      prev_vote == 77 ~ "NOTA",
      prev_vote == 88 ~ "NOTA",
      prev_vote == 98 ~ "NOTA"),
    
    emp_school = ifelse(is.na(emp_school), 0, 1),
    
    # make dummies for treatments
    party = case_when(
      treatment_group == 1 | treatment_group == 2 ~ "gov",
      treatment_group == 3 | treatment_group == 4 ~ "opp"),
    consistency = case_when(
      treatment_group == 1 | treatment_group == 3 ~ "inc",
      treatment_group == 2 | treatment_group == 4 ~ "con"),
    group = case_when(
      treatment_group == 1 ~ "Inconsistent Government",
      treatment_group == 2 ~ "Consistent Government",
      treatment_group == 3 ~ "Inconsistent Opposition",
      treatment_group == 4 ~ "Consistent Opposition"),
    
    # clean up NAs
    across(pol_int:tech_exp, ~ na_if(., 99)),
    
    # manipulation
    mc_part = case_when(
      party == "gov" & mc_part == 1 ~ 1,
      party == "opp" & mc_part == 2 ~ 1,
      TRUE ~ 0),
    mc_cons = case_when(
      consistency == "inc" & mc_cons == 1 ~ 1,
      consistency == "con" & mc_cons == 2 ~ 1,
      TRUE ~ 0),
    manipulation = mc_part + mc_cons,
    
    # attention
    attention = case_when(
      attention_cntrl == 5 ~ 1,
      TRUE ~ 0)) |> 
 
  # create next actions battery
  mutate_at(vars(dv_nxt_1:dv_nxt_9), ~ifelse(is.na(.), 0, .)) |> 
  mutate(dv_nxt = dv_nxt_1 + dv_nxt_2 + dv_nxt_3 + dv_nxt_4 +
           dv_nxt_5 + dv_nxt_6 + dv_nxt_7 + dv_nxt_8, # do not include dv_nxt_9 as it's meaning is 'nothing'
         dv_nxt_scaled = dv_nxt / 8) |> 
  
  # remove NAs listwise
  filter(!is.na(party) | !is.na(consistency) | !is.na(dv_main))

##### Export -------------------------------------------------------------------
write.csv(crediguv,
          file = "data/crediguv.csv",
          row.names = FALSE)

# /./ END OF CODE /./ #


