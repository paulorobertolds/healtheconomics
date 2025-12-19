# parameters.R
# Parameters from Goldie 2004, Table 1

define_goldie_parameters <- function(excel_path = "parameters.xlsx") {
  library(readxl)

  # Read parameters from Excel
  params_df <- read_excel(excel_path)

  # Helper to get value by parameter name
  get_param <- function(name) {
    val <- params_df$Value[params_df$Parameter == name]
    if (length(val) == 0) stop(paste("Parameter not found:", name))
    return(as.numeric(val))
  }

  # Extract values
  age_init_val <- get_param("age_init")
  cycle_length_val <- get_param("cycle_length")
  discount_rate_annual <- get_param("discount_rate_annual")

  vaccine_efficacy_default <- get_param("vaccine_efficacy_default")
  screen_interval_default <- get_param("screen_interval_default")
  screen_start_age_default <- get_param("screen_start_age_default")

  sens_pap_val <- get_param("sens_pap")
  sens_pap_high_grade_mult <- get_param("sens_pap_high_grade_mult")
  sens_screen_cancer_cap <- get_param("sens_screen_cancer_cap")
  spec_pap <- get_param("spec_pap")

  p_norm_to_pers_young <- get_param("p_norm_to_pers_young")
  p_norm_to_pers_old <- get_param("p_norm_to_pers_old")

  p_norm_to_trans_young <- get_param("p_norm_to_trans_young")
  p_norm_to_trans_old <- get_param("p_norm_to_trans_old")

  p_hpv_to_norm_val <- get_param("p_hpv_to_norm")

  p_hpv_to_cin1_young <- get_param("p_hpv_to_cin1_young")
  p_hpv_to_cin1_old <- get_param("p_hpv_to_cin1_old")

  p_hpv_to_cin23_young <- get_param("p_hpv_to_cin23_young")
  p_hpv_to_cin23_old <- get_param("p_hpv_to_cin23_old")

  p_cin1_to_cin23_young <- get_param("p_cin1_to_cin23_young")
  p_cin1_to_cin23_old <- get_param("p_cin1_to_cin23_old")

  p_cin23_to_cancer_young <- get_param("p_cin23_to_cancer_young")
  p_cin23_to_cancer_mid <- get_param("p_cin23_to_cancer_mid")
  p_cin23_to_cancer_old <- get_param("p_cin23_to_cancer_old")

  p_cin1_to_hpv_val <- get_param("p_cin1_to_hpv")
  p_cin1_to_norm_val <- get_param("p_cin1_to_norm")
  p_cin23_to_hpv_val <- get_param("p_cin23_to_hpv")
  p_cin23_to_norm_val <- get_param("p_cin23_to_norm")

  p_stg1_to_stg2_val <- get_param("p_stg1_to_stg2")
  p_stg2_to_stg3_val <- get_param("p_stg2_to_stg3")
  p_stg3_to_stg4_val <- get_param("p_stg3_to_stg4")

  p_det_stg1_val <- get_param("p_det_stg1")
  p_det_stg2_val <- get_param("p_det_stg2")
  p_det_stg3_val <- get_param("p_det_stg3")
  p_det_stg4_val <- get_param("p_det_stg4")

  surv_5yr_stg1 <- get_param("surv_5yr_stg1")
  surv_5yr_stg2 <- get_param("surv_5yr_stg2")
  surv_5yr_stg3 <- get_param("surv_5yr_stg3")
  surv_5yr_stg4 <- get_param("surv_5yr_stg4")

  p_die_other_val <- get_param("p_die_other")

  c_vaccine_val <- get_param("c_vaccine")
  c_screen_conv_val <- get_param("c_screen_conv")
  c_screen_liq_val <- get_param("c_screen_liq")
  c_hpv_test_val <- get_param("c_hpv_test")
  c_office_val <- get_param("c_office")
  c_colpo_biopsy_val <- get_param("c_colpo_biopsy")
  c_treat_cin1_val <- get_param("c_treat_cin1")
  c_treat_cin23_val <- get_param("c_treat_cin23")
  c_treat_stg1_val <- get_param("c_treat_stg1")
  c_treat_stg2_val <- get_param("c_treat_stg2")
  c_treat_stg3_val <- get_param("c_treat_stg3")
  c_treat_stg4_val <- get_param("c_treat_stg4")
  c_terminal_val <- get_param("c_terminal")

  u_norm_val <- get_param("u_norm")
  u_cancer_det_stg1_val <- get_param("u_cancer_det_stg1")
  u_cancer_det_stg2_val <- get_param("u_cancer_det_stg2")
  u_cancer_det_stg3_val <- get_param("u_cancer_det_stg3")
  u_cancer_det_stg4_val <- get_param("u_cancer_det_stg4")
  u_cancer_trt_stg1_val <- get_param("u_cancer_trt_stg1")
  u_cancer_trt_stg2_val <- get_param("u_cancer_trt_stg2")
  u_cancer_trt_stg3_val <- get_param("u_cancer_trt_stg3")
  u_cancer_trt_stg4_val <- get_param("u_cancer_trt_stg4")

  define_parameters(
    # --- Simulation Settings ---
    age_init = !!age_init_val,
    cycle_length = !!cycle_length_val,
    model_time_years = model_time * cycle_length,
    age = age_init + model_time_years,

    # Discount rate per cycle
    discount_rate = (1 + !!discount_rate_annual)^cycle_length - 1,

    # --- Strategy Parameters (Overwritten by define_strategy) ---
    vaccine_efficacy = !!vaccine_efficacy_default,
    screen_interval = !!screen_interval_default,
    screen_start_age = !!screen_start_age_default,
    sens_pap = !!sens_pap_val,

    # --- Derived Parameters ---
    p_vaccine_multiplier = 1 - vaccine_efficacy,

    # Screening probability per cycle
    is_screening_age = ifelse(screen_interval > 0 & age >= screen_start_age,
      ifelse((age - screen_start_age) %% screen_interval < 0.01, 1, 0),
      0
    ),

    # Probability of detection by screening
    p_screen_detect_cin1 = ifelse(is_screening_age == 1, sens_pap, 0),
    p_screen_detect_cin23 = ifelse(is_screening_age == 1, sens_pap * !!sens_pap_high_grade_mult, 0),
    p_screen_detect_cancer = ifelse(is_screening_age == 1, !!sens_screen_cancer_cap, 0),

    # False positive probability
    p_false_pos = ifelse(is_screening_age == 1, 1 - !!spec_pap, 0),

    # --- Probabilities: HPV Incidence & Clearance ---
    p_norm_to_pers = ifelse(age < 35, !!p_norm_to_pers_young, !!p_norm_to_pers_old) * p_vaccine_multiplier,
    p_norm_to_trans = ifelse(age < 35, !!p_norm_to_trans_young, !!p_norm_to_trans_old),
    p_hpv_to_norm = !!p_hpv_to_norm_val,

    # --- Probabilities: CIN Natural History ---
    p_hpv_to_cin1 = ifelse(age < 35, !!p_hpv_to_cin1_young, !!p_hpv_to_cin1_old),
    p_hpv_to_cin23 = ifelse(age < 35, !!p_hpv_to_cin23_young, !!p_hpv_to_cin23_old),
    p_cin1_to_cin23 = ifelse(age < 35, !!p_cin1_to_cin23_young, !!p_cin1_to_cin23_old),
    p_cin23_to_cancer = ifelse(age < 35, !!p_cin23_to_cancer_young,
      ifelse(age < 65, !!p_cin23_to_cancer_mid, !!p_cin23_to_cancer_old)
    ),

    # Regression
    p_cin1_to_hpv = !!p_cin1_to_hpv_val,
    p_cin1_to_norm = !!p_cin1_to_norm_val,
    p_cin23_to_hpv = !!p_cin23_to_hpv_val,
    p_cin23_to_norm = !!p_cin23_to_norm_val,

    # --- Probabilities: Cancer Progression ---
    p_stg1_to_stg2 = !!p_stg1_to_stg2_val,
    p_stg2_to_stg3 = !!p_stg2_to_stg3_val,
    p_stg3_to_stg4 = !!p_stg3_to_stg4_val,

    # --- Probabilities: Symptom Detection ---
    p_det_stg1 = !!p_det_stg1_val,
    p_det_stg2 = !!p_det_stg2_val,
    p_det_stg3 = !!p_det_stg3_val,
    p_det_stg4 = !!p_det_stg4_val,

    # --- Probabilities: Mortality ---
    # 6-month prob = 1 - S(5)^(0.1)
    p_die_stg1 = 1 - (!!surv_5yr_stg1)^(0.1),
    p_die_stg2 = 1 - (!!surv_5yr_stg2)^(0.1),
    p_die_stg3 = 1 - (!!surv_5yr_stg3)^(0.1),
    p_die_stg4 = 1 - (!!surv_5yr_stg4)^(0.1),

    # Background Mortality
    p_die_other = !!p_die_other_val,

    # --- Costs ---
    c_vaccine = !!c_vaccine_val,
    c_screen_conv = !!c_screen_conv_val,
    c_screen_liq = !!c_screen_liq_val,
    c_hpv_test = !!c_hpv_test_val,
    c_office = !!c_office_val,
    c_colpo_biopsy = !!c_colpo_biopsy_val,
    c_treat_cin1 = !!c_treat_cin1_val,
    c_treat_cin23 = !!c_treat_cin23_val,
    c_treat_stg1 = !!c_treat_stg1_val,
    c_treat_stg2 = !!c_treat_stg2_val,
    c_treat_stg3 = !!c_treat_stg3_val,
    c_treat_stg4 = !!c_treat_stg4_val,
    c_terminal = !!c_terminal_val,

    # Cost of screening visit
    c_screen_visit = ifelse(is_screening_age == 1, c_screen_conv + c_office, 0),

    # --- Utilities ---
    u_norm = !!u_norm_val,
    u_cancer_det_stg1 = !!u_cancer_det_stg1_val,
    u_cancer_det_stg2 = !!u_cancer_det_stg2_val,
    u_cancer_det_stg3 = !!u_cancer_det_stg3_val,
    u_cancer_det_stg4 = !!u_cancer_det_stg4_val,
    u_cancer_trt_stg1 = !!u_cancer_trt_stg1_val,
    u_cancer_trt_stg2 = !!u_cancer_trt_stg2_val,
    u_cancer_trt_stg3 = !!u_cancer_trt_stg3_val,
    u_cancer_trt_stg4 = !!u_cancer_trt_stg4_val
  )
}
