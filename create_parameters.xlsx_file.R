# ------------------------------------------------------------------------------
# SCRIPT 1: GENERATE PARAMETERS.XLSX
# ------------------------------------------------------------------------------
if (!require("writexl")) install.packages("writexl")
if (!require("dplyr")) install.packages("dplyr")
library(writexl)
library(dplyr)

# 1. SCALAR PARAMETERS
# Source: Fonseca 2013, Appendix A
params_scalar <- tibble(
  ParameterName = c(
    "discount_rate",        # 5%
    "vaccine_efficacy",     # 50% reduction in LSIL (Base Case)
    "vaccine_coverage",     # 90% coverage
    "vaccine_cost_total",   # $150 (3 doses)
    "p_lsil_to_hsil",       # 0.110
    "p_lsil_to_cancer",     # 0.00075
    "p_hsil_to_cancer",     # 0.0078
    "p_caloc_to_careg",     # 0.019
    "p_careg_to_camet",     # 0.063
    "p_die_caloc",          # 0.0165 (Cancer death - Local)
    "p_die_careg",          # 0.1101 (Cancer death - Regional)
    "p_die_camet",          # 0.3050 (Cancer death - Metastatic)
    "sens_pap_lsil",        # 0.70
    "sens_pap_hsil",        # 0.80
    "eff_cryo_lsil",        # 0.85
    "eff_cryo_hsil",        # 0.75
    "cost_pap",             # $8.00
    "cost_colpo",           # $26.80
    "cost_biopsy",          # $26.80
    "cost_treat_lsil",      # $100.00
    "cost_treat_hsil",      # $498.00
    "start_age"             # 12
  ),
  BaseValue = c(
    0.05, 0.50, 0.90, 150,
    0.110, 0.00075, 0.0078, 
    0.019, 0.063, 
    0.0165, 0.1101, 0.305,
    0.70, 0.80,
    0.85, 0.75,
    8, 26.8, 26.8,
    100, 498,
    12
  )
)

# 2. AGE-DEPENDENT PARAMETERS (Non-Homogeneous Logic)
params_age_dependent <- tibble(
  ParameterName = c(
    rep("p_healthy_to_lsil", 7), # Incidence logic
    rep("p_lsil_regress", 2),    # Regression <30 vs >30
    rep("p_hsil_regress", 2)
  ),
  AgeThreshold = c(
    13, 14, 15, 16, 37, 62, 100, 
    30, 100,                     
    30, 100                      
  ),
  BaseValue = c(
    0.285, 0.117, 0.114, 0.075, 0.070, 0.053, 0.010, 
    0.193, 0.113, 
    0.125, 0.035  
  )
)

# 3. SCREENING SCHEDULES
screening_schedules <- tibble(
  Strategy = c(rep("Screen_3", 3), rep("Screen_10", 10)),
  Age = c(
    25, 35, 45,                                      # 3 Tests (Approx. 2nd, 4th, 6th decades)
    25, 30, 35, 40, 43, 46, 49, 52, 55, 65           # 10 Tests (Fonseca Schedule)
  )
)

# 4. STATE VALUES
state_values <- tibble(
  StateName = c("Healthy", "LSIL", "HSIL", "Cancer_Early", "Cancer_Regional", "Cancer_Metastatic", "Death"),
  AnnualCost = c(0, 0, 0, 370.20, 842.00, 262.50, 0),
  TxCost_Init = c(0, 0, 0, 3702, 8420, 2625, 0),
  QALY_Weight = c(1.0, 1.0, 1.0, 0.76, 0.67, 0.48, 0.00)
)

# Write to Excel
write_xlsx(list(
  params_scalar = params_scalar,
  params_age_dependent = params_age_dependent,
  screening_schedules = screening_schedules,
  state_values = state_values
), "parameters.xlsx")

message("File 'parameters.xlsx' created successfully.")