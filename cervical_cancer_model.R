# # Main script for the Cervical Cancer Cost-Effectiveness Model
# 
# # --- 1. SETUP AND DATA LOADING ---
# 
# # Load required libraries
# library(heemod)
# library(readxl)
# library(dplyr)
# library(ggplot2)
# 
# # Source the custom function to get WHO data
# source("get_who_mortality.R")
# 
# # Define the path to the parameters file
# params_file <- "parameters.xlsx"
# 
# # Load data from each sheet into data frames
# df_probs <- read_excel(params_file, sheet = "transition_probabilities")
# df_costs <- read_excel(params_file, sheet = "costs")
# df_utils <- read_excel(params_file, sheet = "utilities")
# df_strat <- read_excel(params_file, sheet = "strategy_params")
# 
# # Convert data frames to named lists for easier access
# params_probs <- setNames(as.list(df_probs$BaseValue), df_probs$ParameterName)
# params_costs <- setNames(as.list(df_costs$AnnualCost), df_costs$StateName)
# params_utils <- setNames(as.list(df_utils$QALY_Weight), df_utils$StateName)
# params_strat <- setNames(as.list(df_strat$BaseValue), df_strat$ParameterName)
# #convert year to numeric if needed
# params_strat$mortality_year <- as.numeric(params_strat$mortality_year)
# #convert start_age to numeric if needed
# params_strat$start_age <- as.numeric(params_strat$start_age)
# #convert vaccine_cost to numeric if needed
# params_strat$vaccine_cost <- as.numeric(params_strat$vaccine_cost)
# #convert vaccine_efficacy to numeric if needed
# params_strat$vaccine_efficacy <- as.numeric(params_strat$vaccine_efficacy)
# #convert discount_rate to numeric if needed
# params_strat$discount_rate <- as.numeric(params_strat$discount_rate)
# 
# 
# # --- 2. DEFINE MODEL PARAMETERS ---
# 
# # Fetch the life table from a local file for stability
# mortality_table <- get_who_mortality_local(
#   file_path = params_strat$mortality_file,
#   target_year = params_strat$mortality_year,
#   target_sex = "Female" # Model is for a female cohort
# )
# 
# # Create a "vectorized" lookup function for mortality rate
# # This function is now designed to accept a vector of ages
# get_mortality_rate <- function(current_age_vector) {
#   # Use sapply to apply the lookup logic to EACH element of the age vector
#   sapply(current_age_vector, function(age) {
#     
#     # Find the single row where the age fits the bracket
#     rate_row <- mortality_table[
#       age >= mortality_table$age_lower & age <= mortality_table$age_upper, 
#     ]
#     
#     # Return the mortality rate (select first row if multiple matches, though unlikely)
#     return(rate_row$mortality_rate[4])
#   })
# }
# 
# # Define all parameters for the heemod model
# model_params <- define_parameters(
#   p_healthy_lsil = params_probs$p_healthy_to_lsil,
#   p_lsil_hsil = params_probs$p_lsil_to_hsil,
#   p_lsil_healthy = params_probs$p_lsil_to_healthy,
#   p_hsil_lsil = params_probs$p_hsil_to_lsil,
#   p_hsil_cancer_early = params_probs$p_hsil_to_cancer_early,
#   p_early_regional = params_probs$p_cancer_early_to_regional,
#   p_regional_meta = params_probs$p_cancer_regional_to_metastatic,
#   p_death_early = params_probs$p_death_cancer_early,
#   p_death_regional = params_probs$p_death_cancer_regional,
#   p_death_meta = params_probs$p_death_cancer_metastatic,
#   vaccine_eff = params_strat$vaccine_efficacy,
#   cost_vaccine = params_strat$vaccine_cost,
#   dr = params_strat$discount_rate,
#   current_age = params_strat$start_age + model_time - 1,
#   p_death_all = get_mortality_rate(current_age)
# )
# 
# 
# # --- 3. DEFINE TRANSITION MATRICES ---
# 
# state_names <- c("Healthy", "LSIL", "HSIL", "Cancer_Early", "Cancer_Regional", "Cancer_Metastatic", "Death")
# 
# # Define the transition matrix for the "No Vaccination" strategy
# transition_matrix_no_vacc <- define_transition(
#   state_names = state_names,
#   C, p_healthy_lsil, 0, 0, 0, 0, p_death_all,
#   p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
#   0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
#   0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
#   0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
#   0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
#   0, 0, 0, 0, 0, 0, 1
# )
# 
# # Define the transition matrix for the "Vaccination" strategy
# transition_matrix_vacc <- define_transition(
#   state_names = state_names,
#   C, p_healthy_lsil * (1 - vaccine_eff), 0, 0, 0, 0, p_death_all,
#   p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
#   0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
#   0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
#   0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
#   0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
#   0, 0, 0, 0, 0, 0, 1
# )
# 
# 
# # --- 4. DEFINE STATE VALUES ---
# 
# state_healthy <- define_state(
#   cost_health = discount(params_costs$Healthy, r = dr),
#   cost_vaccine = dispatch_strategy(
#     No_Vaccination = 0,
#     Vaccination = ifelse(model_time == 1, cost_vaccine, 0)
#   ),
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$Healthy, r = dr)
# )
# 
# state_lsil <- define_state(
#   cost_health = discount(params_costs$LSIL, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$LSIL, r = dr)
# )
# 
# state_hsil <- define_state(
#   cost_health = discount(params_costs$HSIL, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$HSIL, r = dr)
# )
# 
# state_cancer_early <- define_state(
#   cost_health = discount(params_costs$Cancer_Early, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$Cancer_Early, r = dr)
# )
# 
# state_cancer_regional <- define_state(
#   cost_health = discount(params_costs$Cancer_Regional, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$Cancer_Regional, r = dr)
# )
# 
# state_cancer_metastatic <- define_state(
#   cost_health = discount(params_costs$Cancer_Metastatic, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$Cancer_Metastatic, r = dr)
# )
# 
# state_death <- define_state(
#   cost_health = discount(params_costs$Death, r = dr),
#   cost_vaccine = 0,
#   cost_total = cost_health + cost_vaccine,
#   qaly = discount(params_utils$Death, r = dr)
# )
# 
# 
# # --- 5. DEFINE AND RUN STRATEGIES ---
# 
# strategy_no_vacc <- define_strategy(
#   transition = transition_matrix_no_vacc,
#   Healthy = state_healthy, LSIL = state_lsil, HSIL = state_hsil,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# 
# strategy_vacc <- define_strategy(
#   transition = transition_matrix_vacc,
#   Healthy = state_healthy, LSIL = state_lsil, HSIL = state_hsil,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# 
# model_run <- run_model(
#   parameters = model_params,
#   No_Vaccination = strategy_no_vacc,
#   Vaccination = strategy_vacc,
#   cycles = 100 - params_strat$start_age,
#   cost = cost_total,
#   effect = qaly
# )
# 
# 
# # --- 6. ANALYZE AND VISUALIZE RESULTS ---
# 
# # Print summary table
# print(summary(model_run))
# 
# # Plot Cost-Effectiveness Plane
# plot(model_run, type = "ce") +
#   labs(title = "Cost-Effectiveness Plane: HPV Vaccination")
# 
# # Plot Cohort Trace
# plot(model_run, type = "counts", panel = "by_strategy") +
#   labs(title = "Cohort Trace: Proportion in Each Health State Over Time")

# Main script for the Cervical Cancer Cost-Effectiveness Model

# --- 1. SETUP AND DATA LOADING ---

# Load required libraries
library(heemod)
library(readxl)
library(dplyr)
library(ggplot2)
rm(list = ls())
# Source the custom function to get WHO data
source("get_who_mortality.R")

# Define the path to the parameters file
params_file <- "parameters.xlsx"

# Load data from each sheet into data frames
df_probs <- read_excel(params_file, sheet = "transition_probabilities")
df_costs <- read_excel(params_file, sheet = "costs")
df_utils <- read_excel(params_file, sheet = "utilities")
df_strat <- read_excel(params_file, sheet = "strategy_params")

# Convert data frames to named lists for easier access
params_probs <- setNames(as.list(df_probs$BaseValue), df_probs$ParameterName)
params_costs <- setNames(as.list(df_costs$AnnualCost), df_costs$StateName)
params_utils <- setNames(as.list(df_utils$QALY_Weight), df_utils$StateName)
params_strat <- setNames(as.list(df_strat$BaseValue), df_strat$ParameterName)

# Convert types as needed
params_strat$mortality_year <- as.numeric(params_strat$mortality_year)
params_strat$start_age <- as.numeric(params_strat$start_age)
params_strat$vaccine_cost <- as.numeric(params_strat$vaccine_cost)
params_strat$vaccine_efficacy <- as.numeric(params_strat$vaccine_efficacy)
params_strat$discount_rate <- as.numeric(params_strat$discount_rate)

# --- 2. DEFINE MODEL PARAMETERS ---

# Fetch the life table from a local file for stability
mortality_table <- get_who_mortality_local(
  file_path = params_strat$mortality_file,
  target_year = params_strat$mortality_year,
  target_sex = "Female" # Model is for a female cohort
)

# Create a "vectorized" lookup function for mortality rate
get_mortality_rate <- function(current_age_vector) {
  sapply(current_age_vector, function(age) {
    # Find the row where the age fits the bracket
    rate_row <- mortality_table[
      age >= mortality_table$age_lower & age <= mortality_table$age_upper, 
    ]
    
    # Return the mortality rate (use first match)
    if (nrow(rate_row) > 0) {
      return(rate_row$mortality_rate[1])
    } else {
      # If no match found, return a high mortality rate for safety
      warning(paste("No mortality rate found for age", age, "- using default 0.5"))
      return(0.5)
    }
  })
}

# Define all parameters for the heemod model
model_params <- define_parameters(
  p_healthy_lsil = params_probs$p_healthy_to_lsil,
  p_lsil_hsil = params_probs$p_lsil_to_hsil,
  p_lsil_healthy = params_probs$p_lsil_to_healthy,
  p_hsil_lsil = params_probs$p_hsil_to_lsil,
  p_hsil_cancer_early = params_probs$p_hsil_to_cancer_early,
  p_early_regional = params_probs$p_cancer_early_to_regional,
  p_regional_meta = params_probs$p_cancer_regional_to_metastatic,
  p_death_early = params_probs$p_death_cancer_early,
  p_death_regional = params_probs$p_death_cancer_regional,
  p_death_meta = params_probs$p_death_cancer_metastatic,
  vaccine_eff = params_strat$vaccine_efficacy,
  cost_vaccine = params_strat$vaccine_cost,
  dr = params_strat$discount_rate,
  current_age = params_strat$start_age + model_time - 1,
  p_death_all = get_mortality_rate(current_age)
)

# --- 3. DEFINE TRANSITION MATRICES ---

state_names <- c("Healthy", "LSIL", "HSIL", "Cancer_Early", "Cancer_Regional", "Cancer_Metastatic", "Death")

# FIXED: Use proper matrix format for define_transition
# Define the transition matrix for the "No Vaccination" strategy
transition_matrix_no_vacc <- define_transition(
  C, p_healthy_lsil, 0, 0, 0, 0, p_death_all,
  p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
  0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
  0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
  0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
  0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
  0, 0, 0, 0, 0, 0, 1,
  state_names = state_names
)

# Define the transition matrix for the "Vaccination" strategy
transition_matrix_vacc <- define_transition(
  C, p_healthy_lsil * (1 - vaccine_eff), 0, 0, 0, 0, p_death_all,
  p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
  0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
  0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
  0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
  0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
  0, 0, 0, 0, 0, 0, 1,
  state_names = state_names
)

# --- 4. DEFINE STATE VALUES ---

state_healthy <- define_state(
  cost_health = discount(params_costs$Healthy, r = dr),
  cost_vaccine = dispatch_strategy(
    No_Vaccination = 0,
    Vaccination = ifelse(model_time == 1, cost_vaccine, 0)
  ),
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$Healthy, r = dr)
)

state_lsil <- define_state(
  cost_health = discount(params_costs$LSIL, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$LSIL, r = dr)
)

state_hsil <- define_state(
  cost_health = discount(params_costs$HSIL, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$HSIL, r = dr)
)

state_cancer_early <- define_state(
  cost_health = discount(params_costs$Cancer_Early, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$Cancer_Early, r = dr)
)

state_cancer_regional <- define_state(
  cost_health = discount(params_costs$Cancer_Regional, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$Cancer_Regional, r = dr)
)

state_cancer_metastatic <- define_state(
  cost_health = discount(params_costs$Cancer_Metastatic, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$Cancer_Metastatic, r = dr)
)

state_death <- define_state(
  cost_health = discount(params_costs$Death, r = dr),
  cost_vaccine = 0,
  cost_total = cost_health + cost_vaccine,
  qaly = discount(params_utils$Death, r = dr)
)

# --- 5. DEFINE AND RUN STRATEGIES ---

strategy_no_vacc <- define_strategy(
  transition = transition_matrix_no_vacc,
  Healthy = state_healthy, 
  LSIL = state_lsil, 
  HSIL = state_hsil,
  Cancer_Early = state_cancer_early, 
  Cancer_Regional = state_cancer_regional,
  Cancer_Metastatic = state_cancer_metastatic, 
  Death = state_death
)

strategy_vacc <- define_strategy(
  transition = transition_matrix_vacc,
  Healthy = state_healthy, 
  LSIL = state_lsil, 
  HSIL = state_hsil,
  Cancer_Early = state_cancer_early, 
  Cancer_Regional = state_cancer_regional,
  Cancer_Metastatic = state_cancer_metastatic, 
  Death = state_death
)

# Run the model with error handling
tryCatch({
  model_run <- run_model(
    parameters = model_params,
    No_Vaccination = strategy_no_vacc,
    Vaccination = strategy_vacc,
    cycles = 100 - params_strat$start_age,
    cost = cost_total,
    effect = qaly,
    method = "end"
  )
  
  # Print results
  print(model_run)
  summary(model_run)
  
}, error = function(e) {
  message("Error running model: ", e$message)
  message("Let's debug the transition matrices...")
  
  # Debug: Check if transition matrices sum to 1
  test_params <- eval_parameters(model_params, 1)
  
  message("Testing No Vaccination transition matrix:")
  test_trans_no_vacc <- eval_transition(transition_matrix_no_vacc, test_params, 1)
  print(test_trans_no_vacc)
  message("Row sums: ", paste(round(rowSums(test_trans_no_vacc), 3), collapse = ", "))
  
  message("Testing Vaccination transition matrix:")
  test_trans_vacc <- eval_transition(transition_matrix_vacc, test_params, 1)
  print(test_trans_vacc)
  message("Row sums: ", paste(round(rowSums(test_trans_vacc), 3), collapse = ", "))
})


