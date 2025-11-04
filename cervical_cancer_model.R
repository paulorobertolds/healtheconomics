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
#convert year to numeric if needed
params_strat$mortality_year <- as.numeric(params_strat$mortality_year)
#convert start_age to numeric if needed
params_strat$start_age <- as.numeric(params_strat$start_age)
#convert vaccine_cost to numeric if needed
params_strat$vaccine_cost <- as.numeric(params_strat$vaccine_cost)
#convert vaccine_efficacy to numeric if needed
params_strat$vaccine_efficacy <- as.numeric(params_strat$vaccine_efficacy)
#convert discount_rate to numeric if needed
params_strat$discount_rate <- as.numeric(params_strat$discount_rate)


# --- 2. DEFINE MODEL PARAMETERS ---

# Fetch the life table from a local file for stability
mortality_table <- get_who_mortality_local(
  file_path = params_strat$mortality_file,
  target_year = params_strat$mortality_year,
  target_sex = "Female" # Model is for a female cohort
)

# Create a lookup function for mortality rate
get_mortality_rate <- function(current_age) {
  # Find the correct age bracket from the table
  # The 'na.omit' handles cases where age might fall outside the table range, though it shouldn't.
  rate <- na.omit(mortality_table$mortality_rate[
    current_age >= mortality_table$age_lower & current_age <= mortality_table$age_upper
  ])
  # Return the first match (there should only be one)
  return(rate[4])
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

# Define the transition matrix for the "No Vaccination" strategy
transition_matrix_no_vacc <- define_transition(
  state_names = state_names,
  
  # From Healthy
  C, p_healthy_lsil, 0, 0, 0, 0, p_death_all,
  
  # From LSIL
  p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
  
  # From HSIL
  0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
  
  # From Cancer_Early
  0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
  
  # From Cancer_Regional
  0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
  
  # From Cancer_Metastatic
  0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
  
  # From Death (absorbing state)
  0, 0, 0, 0, 0, 0, 1
)

# Define the transition matrix for the "Vaccination" strategy
transition_matrix_vacc <- define_transition(
  state_names = state_names,
  
  # From Healthy (Vaccine effect applied here)
  C, p_healthy_lsil * (1 - vaccine_eff), 0, 0, 0, 0, p_death_all,
  
  # From LSIL
  p_lsil_healthy, C, p_lsil_hsil, 0, 0, 0, p_death_all,
  
  # From HSIL
  0, p_hsil_lsil, C, p_hsil_cancer_early, 0, 0, p_death_all,
  
  # From Cancer_Early
  0, 0, 0, C, p_early_regional, 0, combine_probs(p_death_all, p_death_early),
  
  # From Cancer_Regional
  0, 0, 0, 0, C, p_regional_meta, combine_probs(p_death_all, p_death_regional),
  
  # From Cancer_Metastatic
  0, 0, 0, 0, 0, C, combine_probs(p_death_all, p_death_meta),
  
  # From Death (absorbing state)
  0, 0, 0, 0, 0, 0, 1
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
  Healthy = state_healthy, LSIL = state_lsil, HSIL = state_hsil,
  Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
  Cancer_Metastatic = state_cancer_metastatic, Death = state_death
)

strategy_vacc <- define_strategy(
  transition = transition_matrix_vacc,
  Healthy = state_healthy, LSIL = state_lsil, HSIL = state_hsil,
  Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
  Cancer_Metastatic = state_cancer_metastatic, Death = state_death
)

model_run <- run_model(
  parameters = model_params,
  No_Vaccination = strategy_no_vacc,
  Vaccination = strategy_vacc,
  cycles = 100 - params_strat$start_age,
  cost = cost_total,
  effect = qaly
)


# --- 6. ANALYZE AND VISUALIZE RESULTS ---

# Print summary table
print(summary(model_run))

# Plot Cost-Effectiveness Plane
plot(model_run, type = "ce") +
  labs(title = "Cost-Effectiveness Plane: HPV Vaccination")

# Plot Cohort Trace
plot(model_run, type = "counts", panel = "by_strategy") +
  labs(title = "Cohort Trace: Proportion in Each Health State Over Time")