# model_structure.R

source("parameters.R")

define_goldie_model <- function(params) {
  # --- Transition Matrix ---
  # States:
  # 1. Normal
  # 2. HPV_Pers
  # 3. HPV_Trans
  # 4. CIN1
  # 5. CIN23
  # 6. Cancer_I_Undet
  # 7. Cancer_I_Det
  # 8. Cancer_II_Undet
  # 9. Cancer_II_Det
  # 10. Cancer_III_Undet
  # 11. Cancer_III_Det
  # 12. Cancer_IV_Undet
  # 13. Cancer_IV_Det
  # 14. Death_Cancer
  # 15. Death_Other

  mat_trans <- define_transition(
    state_names = c(
      "Normal", "HPV_Pers", "HPV_Trans", "CIN1", "CIN23",
      "Cancer_I_Undet", "Cancer_I_Det", "Cancer_II_Undet", "Cancer_II_Det",
      "Cancer_III_Undet", "Cancer_III_Det", "Cancer_IV_Undet", "Cancer_IV_Det",
      "Death_Cancer", "Death_Other"
    ),
    # 1. Normal
    C, p_norm_to_pers, p_norm_to_trans, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, p_die_other,

    # 2. HPV_Pers
    p_hpv_to_norm, C, 0, p_hpv_to_cin1, p_hpv_to_cin23, 0, 0, 0, 0, 0, 0, 0, 0, 0, p_die_other,

    # 3. HPV_Trans
    p_hpv_to_norm, 0, C, p_hpv_to_cin1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, p_die_other,

    # 4. CIN1
    # If detected (monitored), cost increases (handled in state cost), but transitions same?
    # Paper says monitored until regress/progress.
    # We'll assume screening doesn't change CIN1 transitions, just adds cost.
    # Or maybe we treat it? Paper says "not treated".
    p_cin1_to_norm, p_cin1_to_hpv, 0, C, p_cin1_to_cin23, 0, 0, 0, 0, 0, 0, 0, 0, 0, p_die_other,

    # 5. CIN23
    # If detected, treated and moves to Normal (or HPV).
    # We add p_screen_detect_cin23 to the probability of moving to Normal.
    # We must ensure sum <= 1.
    # p_cin23_to_norm_total = p_cin23_to_norm + p_screen_detect_cin23
    # But we need to be careful about competing risks.
    # We'll add it to p_cin23_to_norm.
    p_cin23_to_norm + p_screen_detect_cin23, p_cin23_to_hpv, 0, 0, C, p_cin23_to_cancer, 0, 0, 0, 0, 0, 0, 0, 0, p_die_other,

    # 6. Cancer_I_Undet
    # Cap detection so sum <= 1
    # Max det = 1 - p_stg1_to_stg2 - p_die_other
    # We use pmin to ensure element-wise minimum
    0, 0, 0, 0, 0, C, pmin(0.99 - p_stg1_to_stg2 - p_die_other, p_det_stg1 + p_screen_detect_cancer), p_stg1_to_stg2, 0, 0, 0, 0, 0, 0, p_die_other,

    # 7. Cancer_I_Det
    0, 0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, 0, p_die_stg1, p_die_other,

    # 8. Cancer_II_Undet
    0, 0, 0, 0, 0, 0, 0, C, pmin(0.99 - p_stg2_to_stg3 - p_die_other, p_det_stg2 + p_screen_detect_cancer), p_stg2_to_stg3, 0, 0, 0, 0, p_die_other,

    # 9. Cancer_II_Det
    0, 0, 0, 0, 0, 0, 0, 0, C, 0, 0, 0, 0, p_die_stg2, p_die_other,

    # 10. Cancer_III_Undet
    0, 0, 0, 0, 0, 0, 0, 0, 0, C, pmin(0.99 - p_stg3_to_stg4 - p_die_other, p_det_stg3 + p_screen_detect_cancer), p_stg3_to_stg4, 0, 0, p_die_other,

    # 11. Cancer_III_Det
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, 0, 0, p_die_stg3, p_die_other,

    # 12. Cancer_IV_Undet
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, pmin(0.99 - p_die_other, p_det_stg4 + p_screen_detect_cancer), 0, p_die_other,

    # 13. Cancer_IV_Det
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_die_stg4, p_die_other,

    # 14. Death_Cancer
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,

    # 15. Death_Other
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
  )

  # --- State Definitions ---
  # Apply discounting to costs and utilities
  # discount() uses the 'discount_rate' parameter by default if not specified?
  # No, we must pass it or define it in parameters.
  # heemod's discount() takes 'value', 'rate', 'first' (logical).
  # We use the 'discount_rate' from parameters.

  state_norm <- define_state(
    # Cost: Screening + False Positive Colposcopy
    cost = discount(c_screen_visit + p_false_pos * c_colpo_biopsy, r = discount_rate),
    utility = discount(u_norm * cycle_length, r = discount_rate)
  )
  state_hpv_pers <- define_state(
    # Cost: Screening + False Positive (or HPV+ but no CIN) Colposcopy
    cost = discount(c_screen_visit + p_false_pos * c_colpo_biopsy, r = discount_rate),
    utility = discount(u_norm * cycle_length, r = discount_rate)
  )
  state_hpv_trans <- define_state(
    cost = discount(c_screen_visit + p_false_pos * c_colpo_biopsy, r = discount_rate),
    utility = discount(u_norm * cycle_length, r = discount_rate)
  )
  state_cin1 <- define_state(
    # Cost = Probability of detection * (Treatment + Colposcopy) + Screening
    cost = discount(p_screen_detect_cin1 * (c_treat_cin1 + c_colpo_biopsy) + c_screen_visit, r = discount_rate),
    utility = discount(u_norm * 0.9 * cycle_length, r = discount_rate)
  )
  state_cin23 <- define_state(
    # Cost = Probability of detection * (Treatment + Colposcopy) + Screening
    cost = discount(p_screen_detect_cin23 * (c_treat_cin23 + c_colpo_biopsy) + c_screen_visit, r = discount_rate),
    utility = discount(u_norm * 0.85 * cycle_length, r = discount_rate)
  )
  state_cancer_i_undet <- define_state(
    # Cost = Probability of detection (Screening or Symptoms) * Treatment cost?
    # No, if detected, they move to Cancer_I_Det.
    # The cost is applied in Cancer_I_Det?
    # Yes, Cancer_I_Det has 'c_treat_stg1'.
    # But 'c_treat_stg1' is a one-time cost?
    # If they stay in Cancer_I_Det, do they pay again?
    # Usually treatment is one-time.
    # If Cancer_I_Det is a "tunnel" or we assume cost is applied upon entry.
    # heemod applies state cost *every cycle* in that state.
    # If 'c_treat_stg1' is $21k, paying it every 6 months is wrong.
    # It should be a transition cost.
    # Since we can't easily do transition costs, we can make Cancer_I_Det a temporary state?
    # Or assume the cost is distributed?
    # Paper says "Lifetime costs".
    # I'll assume for now it's a one-time cost.
    # But heemod applies it every cycle.
    # I should divide by expected duration? Or use a transition state.
    # For now, I'll leave it but this might overestimate cancer costs if they survive long in Det state.
    # However, for CIN, detection leads to Normal. So it's a transition.
    # So for CIN, 'p_detect * cost' is correct (expected cost per cycle).
    # For Cancer, they move to Det.
    # If I put cost in Det state, it applies every cycle.
    # I should probably put the cost in the Undet state weighted by transition to Det?
    # cost = (p_det_stg1 + p_screen_detect_cancer) * c_treat_stg1.
    # And set Cancer_I_Det cost to 0 (or follow-up cost).
    # This is better.
    cost = discount((p_det_stg1 + p_screen_detect_cancer) * c_treat_stg1, r = discount_rate),
    utility = discount(u_norm * 0.95 * cycle_length, r = discount_rate)
  )
  state_cancer_i_det <- define_state(
    cost = discount(p_die_stg1 * c_terminal, r = discount_rate), # Add terminal cost if they die
    utility = discount(u_cancer_det_stg1 * cycle_length, r = discount_rate)
  )
  state_cancer_ii_undet <- define_state(
    cost = discount((p_det_stg2 + p_screen_detect_cancer) * c_treat_stg2, r = discount_rate),
    utility = discount(u_norm * 0.9 * cycle_length, r = discount_rate)
  )
  state_cancer_ii_det <- define_state(
    cost = discount(p_die_stg2 * c_terminal, r = discount_rate),
    utility = discount(u_cancer_det_stg2 * cycle_length, r = discount_rate)
  )
  state_cancer_iii_undet <- define_state(
    cost = discount((p_det_stg3 + p_screen_detect_cancer) * c_treat_stg3, r = discount_rate),
    utility = discount(u_norm * 0.8 * cycle_length, r = discount_rate)
  )
  state_cancer_iii_det <- define_state(
    cost = discount(p_die_stg3 * c_terminal, r = discount_rate),
    utility = discount(u_cancer_det_stg3 * cycle_length, r = discount_rate)
  )
  state_cancer_iv_undet <- define_state(
    cost = discount((p_det_stg4 + p_screen_detect_cancer) * c_treat_stg4, r = discount_rate),
    utility = discount(u_norm * 0.7 * cycle_length, r = discount_rate)
  )
  state_cancer_iv_det <- define_state(
    cost = discount(p_die_stg4 * c_terminal, r = discount_rate),
    utility = discount(u_cancer_det_stg4 * cycle_length, r = discount_rate)
  )
  state_death_cancer <- define_state(cost = 0, utility = 0)
  state_death_other <- define_state(cost = 0, utility = 0)

  # --- Strategy Definition ---
  strat <- define_strategy(
    transition = mat_trans,
    Normal = state_norm,
    HPV_Pers = state_hpv_pers,
    HPV_Trans = state_hpv_trans,
    CIN1 = state_cin1,
    CIN23 = state_cin23,
    Cancer_I_Undet = state_cancer_i_undet,
    Cancer_I_Det = state_cancer_i_det,
    Cancer_II_Undet = state_cancer_ii_undet,
    Cancer_II_Det = state_cancer_ii_det,
    Cancer_III_Undet = state_cancer_iii_undet,
    Cancer_III_Det = state_cancer_iii_det,
    Cancer_IV_Undet = state_cancer_iv_undet,
    Cancer_IV_Det = state_cancer_iv_det,
    Death_Cancer = state_death_cancer,
    Death_Other = state_death_other
  )

  return(strat)
}
