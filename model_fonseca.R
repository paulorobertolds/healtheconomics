# model_fonseca.R
library(heemod)
library(readxl)
library(dplyr)

# Helper to read params
read_fonseca_params <- function(path = "parameters.xlsx") {
  df <- read_excel(path)
  # Convert to named list
  p <- setNames(as.list(df$Value), df$Parameter)
  return(p)
}

# Define the model
define_fonseca_model <- function(
    screening_strategy = "None", # "None", "3x", "10x"
    vaccination = FALSE) {
  # Load parameters from Excel
  raw_params <- read_fonseca_params("parameters.xlsx")

  # Define strategy-specific constants to pass as parameters
  strat_params <- list(
    strategy_name = screening_strategy,
    vaccination_enabled = as.numeric(vaccination)
  )

  # Construct parameter list with quoted expressions
  param_expressions <- list(
    # Time & Age
    age = quote(age_init + model_time),
    discount_rate = quote(discount_rate_annual),
    cycle_length = quote(cycle_length),

    # --- Vaccination Logic ---
    vaccine_eff = quote(if (vaccination_enabled == 1) vaccine_efficacy_lsil_red else 0),

    # --- Screening Logic ---
    prob_screen_base = quote(case_when(
      strategy_name == "None" ~ 0,
      strategy_name == "3x" & age >= 11 & age <= 20 ~ screen_prob_2nd_decade,
      strategy_name == "3x" & age >= 31 & age <= 40 ~ screen_prob_4th_decade,
      strategy_name == "3x" & age >= 51 & age <= 60 ~ screen_prob_6th_decade,
      strategy_name == "10x" & age >= 25 & age <= 64 ~ 0.25,
      TRUE ~ 0
    )),

    # Actual screening probability
    p_screen = quote(prob_screen_base),

    # --- LSIL Acquisition (Time Dependent) ---
    years_since_debut = quote(pmax(1, age - sexual_debut_age)),
    p_lsil_raw = quote(case_when(
      years_since_debut == 1 ~ p_lsil_yr1,
      years_since_debut == 2 ~ p_lsil_yr2,
      years_since_debut == 3 ~ p_lsil_yr3,
      years_since_debut == 4 ~ p_lsil_yr4,
      years_since_debut <= 25 ~ p_lsil_yr5_25,
      years_since_debut <= 50 ~ p_lsil_yr26_50,
      TRUE ~ p_lsil_yr51_plus
    )),
    p_lsil_acq = quote(p_lsil_raw * (1 - vaccine_eff)),

    # --- Transitions (LSIL/HSIL) ---
    p_lsil_to_norm = quote(ifelse(age < 30, p_lsil_to_norm_young, p_lsil_to_norm_old)),

    # Detection & Treatment Logic
    p_hsil_treat_cure = quote(p_screen * sens_pap_hsil * 0.75),
    p_hsil_to_norm_final = quote(pmin(1, p_hsil_to_norm + p_hsil_treat_cure)),

    # --- Cancer Progression ---
    p_hsil_to_loc = quote(p_hsil_to_cancer * prop_cancer_loc),
    p_hsil_to_reg = quote(p_hsil_to_cancer * prop_cancer_reg),
    p_hsil_to_meta = quote(p_hsil_to_cancer * prop_cancer_meta),

    # --- Mortality ---
    rate_die_loc = quote(-log(surv_5yr_loc) / 5),
    p_die_ca_loc = quote(1 - exp(-rate_die_loc)),
    rate_die_reg = quote(-log(surv_5yr_reg) / 5),
    p_die_ca_reg = quote(1 - exp(-rate_die_reg)),
    rate_die_meta = quote(-log(surv_5yr_meta) / 5),
    p_die_ca_meta = quote(1 - exp(-rate_die_meta)),

    # Background mortality
    p_die_other = quote(0.005),

    # --- Costs Calculation ---
    c_screen_total = quote(p_screen * (c_pap + c_visit)),
    prob_det_lsil = quote(p_screen * sens_pap_lsil),
    c_treat_lsil_total = quote(prob_det_lsil * c_colpo_biopsy),
    prob_det_hsil = quote(p_screen * sens_pap_hsil),
    c_treat_hsil_total = quote(prob_det_hsil * (c_colpo_biopsy + c_cryo_hsil)),
    c_treat_loc_init = quote(c_treat_loc),
    c_treat_loc_annual = quote(c_treat_loc * 0.10),
    c_treat_reg_init = quote(c_treat_reg),
    c_treat_reg_annual = quote(c_treat_reg * 0.10),
    c_treat_meta_init = quote(c_treat_meta),
    c_treat_meta_annual = quote(c_treat_meta * 0.10),

    # --- Utilities ---
    u_screen_decrement = quote(ifelse(p_screen > 0, (1 - u_pap), 0)),
    u_current_base = quote(u_norm),
    u_current = quote(u_current_base - u_screen_decrement),

    # Vaccine Cost (Applied at start)
    # Applied only at model_time = 1 (Age 12->13 cycle? or Age 12 start).
    # Since transition happens AFTER costs, first cost is cycle 1.
    c_vaccine_applied = quote(ifelse(model_time == 1 & vaccination_enabled == 1, vaccine_cost_3doses, 0))
  )

  # Combine everything
  all_params <- c(raw_params, strat_params, param_expressions)

  def_params <- do.call(define_parameters, all_params)

  # Transition Matrix Definition
  mat_trans <- define_transition(
    state_names = c(
      "Normal", "LSIL", "HSIL",
      "Ca_Loc", "Ca_Reg", "Ca_Meta",
      "Death_Ca", "Death_Other"
    ),

    # 1. Normal
    C, p_lsil_acq, 0, 0, 0, 0, 0, p_die_other,

    # 2. LSIL
    p_lsil_to_norm, C, p_lsil_to_hsil, p_lsil_to_cancer, 0, 0, 0, p_die_other,

    # 3. HSIL
    p_hsil_to_norm_final, 0, C, p_hsil_to_loc, p_hsil_to_reg, p_hsil_to_meta, 0, p_die_other,

    # 4. Ca_Loc
    C, 0, 0, 0, 0, 0, p_die_ca_loc, p_die_other,

    # 5. Ca_Reg
    0, 0, 0, 0, C, 0, p_die_ca_reg, p_die_other,

    # 6. Ca_Meta
    0, 0, 0, 0, 0, C, p_die_ca_meta, p_die_other,

    # 7. Death_Ca
    0, 0, 0, 0, 0, 0, 1, 0,

    # 8. Death_Other
    0, 0, 0, 0, 0, 0, 0, 1
  )

  # Define States with Discounting
  state_normal <- define_state(
    cost = discount(c_screen_total + c_vaccine_applied, r = discount_rate),
    utility = discount(u_current, r = discount_rate)
  )

  state_lsil <- define_state(
    cost = discount(c_screen_total + c_treat_lsil_total, r = discount_rate),
    utility = discount(u_current, r = discount_rate)
  )

  state_hsil <- define_state(
    cost = discount(c_screen_total + c_treat_hsil_total, r = discount_rate),
    utility = discount(u_current, r = discount_rate)
  )

  state_ca_loc <- define_state(
    cost = discount(ifelse(state_time == 1, c_treat_loc_init, c_treat_loc_annual), r = discount_rate),
    utility = discount(u_cancer_loc, r = discount_rate)
  )

  state_ca_reg <- define_state(
    cost = discount(ifelse(state_time == 1, c_treat_reg_init, c_treat_reg_annual), r = discount_rate),
    utility = discount(u_cancer_reg, r = discount_rate)
  )

  state_ca_meta <- define_state(
    cost = discount(ifelse(state_time == 1, c_treat_meta_init, c_treat_meta_annual), r = discount_rate),
    utility = discount(u_cancer_meta, r = discount_rate)
  )

  state_death_ca <- define_state(
    cost = 0, utility = 0
  )

  state_death_oth <- define_state(
    cost = 0, utility = 0
  )


  # Define Strategy
  strat <- define_strategy(
    transition = mat_trans,
    Normal = state_normal,
    LSIL = state_lsil,
    HSIL = state_hsil,
    Ca_Loc = state_ca_loc,
    Ca_Reg = state_ca_reg,
    Ca_Meta = state_ca_meta,
    Death_Ca = state_death_ca,
    Death_Other = state_death_oth
  )

  return(list(strat = strat, params = def_params))
}
