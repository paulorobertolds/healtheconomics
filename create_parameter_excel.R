# create_parameter_excel.R
library(writexl)

# Define the parameters data frame based on Fonseca et al. (2013) Appendix A
params_df <- data.frame(
  Parameter = c(
    # --- Simulation Settings ---
    "age_init", "cycle_length", "discount_rate_annual", "sexual_debut_age",

    # --- Vaccination ---
    "vaccine_coverage", "vaccine_cost_3doses", "booster_cost", "vaccine_efficacy_lsil_red",

    # --- Screening ---
    # Strategy 1: 3x lifetime (approx ages 18-28 (2nd dec), 38-48 (4th), 58-68 (6th)?
    # Paper says: "randomly within the second, fourth, and sixth decades of life"
    # 2nd decade: 11-20. 4th: 31-40. 6th: 51-60.
    # Start age: 12.
    "screen_prob_2nd_decade", "screen_prob_4th_decade", "screen_prob_6th_decade",

    # Costs
    "c_pap", "c_visit", "c_colpo_biopsy",
    "c_cryo_lsil", "c_cryo_hsil", # Paper uses same cost for cryo? Table says "Cryosurgery: 26.8"
    "c_conization", "c_hysterectomy",

    # Treat Cancer
    "c_treat_loc", "c_treat_reg", "c_treat_meta",

    # Test Performance
    "sens_pap_lsil", "sens_pap_hsil", "spec_pap",

    # --- Transitions: Healthy -> LSIL (Annual probability) ---
    # Dependent on years since sexual debut.
    "p_lsil_yr1", "p_lsil_yr2", "p_lsil_yr3", "p_lsil_yr4",
    "p_lsil_yr5_25", "p_lsil_yr26_50", "p_lsil_yr51_plus",

    # --- Transitions: LSIL Outcomes ---
    "p_lsil_to_norm_young", # < 30
    "p_lsil_to_norm_old", # > 30
    "p_lsil_to_hsil",
    "p_lsil_to_cancer",

    # --- Transitions: HSIL Outcomes ---
    "p_hsil_to_norm", # Regression
    "p_hsil_to_cancer",

    # --- Cancer Progression (at diagnosis distribution) ---
    # This is for prevalence? Or incidence?
    # "Given the progression to cervical invasive cancer... probabilities of its detection... were derived from local study"
    # "The tumor stages were simplified as localized... regional... metastatic"
    # This usually implies when cancer "happens", it distributes into these stages?
    # Or does it progress HSIL -> Loc -> Reg -> Meta?
    # Fig 1 shows HSIL -> Cancer (Early, Regional, Meta). Arrows go to all 3.
    # So from HSIL, you can go to any of them?
    # "HSIL... progress to localized, regional, or metastatic invasive cancer"
    # So we need probabilities for HSIL -> Loc, HSIL -> Reg, HSIL -> Meta.
    # Prob(HSIL -> Cancer) is 0.0078.
    # Conditional on cancer, split is:
    "prop_cancer_loc", "prop_cancer_reg", "prop_cancer_meta",

    # --- Survival (Annual prob of death) ---
    # Paper gives 5-year survival. We convert to annual prob.
    "surv_5yr_loc", "surv_5yr_reg", "surv_5yr_meta",

    # --- Utilities ---
    "u_norm", "u_pap", "u_colpo", "u_coniz",
    "u_cancer_loc", "u_cancer_reg", "u_cancer_meta"
  ),
  Value = c(
    # Sim Settings
    12, 1, 0.05, 13,

    # Vaccination
    0.90, 150, 50, 0.50, # 50% reduction in LSIL prob (Appendix A: "Reduction... 40-70", Base 50)

    # Screening Probabilities (Annual prob to achieve 1 screen in 10 years approx = 0.1)
    0.1, 0.1, 0.1,

    # Costs (USD)
    8, 5.5, 26.8,
    26.8, 26.8, # Cryo
    498, 1236,
    3702, 8420, 2625,

    # Test Performance
    0.70, 0.80, 0.90,

    # Healthy -> LSIL
    0.285, 0.117, 0.114, 0.075,
    0.070, 0.053, 0.010,

    # LSIL Outcomes
    0.193, 0.113, 0.110, 0.00075,

    # HSIL Outcomes
    0.175, 0.0078,

    # Cancer Props
    0.315, 0.488, 0.197,

    # Survival (5yr)
    0.920, 0.557, 0.165,

    # Utilities
    1.0, 0.99, 0.95, 0.95,
    0.76, 0.67, 0.48
  ),
  Description = c(
    "Cohort starting age", "Cycle length (years)", "Discount rate", "Age of sexual debut",
    "Vaccine coverage", "Vaccine cost (3 doses)", "Booster cost", "Vaccine efficacy (LSIL risk reduction)",
    "Screen prob ages 11-20", "Screen prob ages 31-40", "Screen prob ages 51-60",
    "Cost Pap", "Cost Visit", "Cost Colpo", "Cost Cryo LSIL", "Cost Cryo HSIL", "Cost Conization", "Cost Hysterectomy",
    "Cost Treat Local", "Cost Treat Regional", "Cost Treat Meta",
    "Sens Pap LSIL", "Sens Pap HSIL", "Spec Pap",
    "Prob LSIL Yr 1", "Prob LSIL Yr 2", "Prob LSIL Yr 3", "Prob LSIL Yr 4", "Prob LSIL Yr 5-25", "Prob LSIL Yr 26-50", "Prob LSIL Yr 51+",
    "Prob LSIL to Norm (<30)", "Prob LSIL to Norm (>30)", "Prob LSIL to HSIL", "Prob LSIL to Cancer",
    "Prob HSIL to Norm", "Prob HSIL to Cancer",
    "Prop Cancer Local", "Prop Cancer Regional", "Prop Cancer Metastatic",
    "Survival 5yr Local", "Survival 5yr Regional", "Survival 5yr Metastatic",
    "Utility Normal", "Utility Pap", "Utility Colpo", "Utility Coniz", "Utility Local", "Utility Regional", "Utility Meta"
  )
)

write_xlsx(params_df, "parameters.xlsx")
print("Created parameters.xlsx")
