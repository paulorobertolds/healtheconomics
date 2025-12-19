# run_analysis.R

library(heemod)
source("parameters.R")
source("model_structure.R")

# 1. Define Base Parameters
params_base <- define_goldie_parameters()

# 2. Define Parameter Sets for Strategies
# Strategy 1: No Intervention (Base case: screen_interval=0, vaccine_efficacy=0)
params_no_int <- params_base

# Strategy 2: Screening Only (Triennial, start 25)
params_screen <- modify(
  params_base,
  screen_interval = 3,
  screen_start_age = 25,
  vaccine_efficacy = 0
)

# Strategy 3: Screening + Vaccination (90% efficacy)
params_screen_vac <- modify(
  params_base,
  screen_interval = 3,
  screen_start_age = 25,
  vaccine_efficacy = 0.9
)

# 3. Define Model Structure (Same for all, logic is in parameters)
strat <- define_goldie_model(params_base)

# 4. Run Models
# Run for 140 cycles (70 years)
cycles <- 140

res_no_int <- run_model(
  No_Intervention = strat,
  parameters = params_no_int,
  cycles = cycles,
  cost = cost,
  effect = utility,
  method = "life-table"
)

res_screen <- run_model(
  Screening_Only = strat,
  parameters = params_screen,
  cycles = cycles,
  cost = cost,
  effect = utility,
  method = "life-table"
)

res_screen_vac <- run_model(
  Screening_Vaccine = strat,
  parameters = params_screen_vac,
  cycles = cycles,
  cost = cost,
  effect = utility,
  method = "life-table"
)

# 5. Combine and Print Results
# We can manually combine summaries
cat("\n--- No Intervention ---\n")
print(summary(res_no_int))

cat("\n--- Screening Only ---\n")
print(summary(res_screen))

cat("\n--- Screening + Vaccine ---\n")
print(summary(res_screen_vac))

# Calculate ICERs manually if needed
# Cost and Effect from summaries
c_no_int <- summary(res_no_int)$res$cost
e_no_int <- summary(res_no_int)$res$utility

c_screen <- summary(res_screen)$res$cost
e_screen <- summary(res_screen)$res$utility

c_screen_vac <- summary(res_screen_vac)$res$cost
e_screen_vac <- summary(res_screen_vac)$res$utility

cat("\n--- Comparison ---\n")
cat("Strategy | Cost | QALYs | Incr Cost | Incr QALY | ICER\n")
cat(sprintf("No Int | %.0f | %.2f | - | - | -\n", c_no_int, e_no_int))
cat(sprintf(
  "Screen | %.0f | %.2f | %.0f | %.2f | %.0f\n",
  c_screen, e_screen, c_screen - c_no_int, e_screen - e_no_int, (c_screen - c_no_int) / (e_screen - e_no_int)
))
cat(sprintf(
  "Scr+Vac| %.0f | %.2f | %.0f | %.2f | %.0f\n",
  c_screen_vac, e_screen_vac, c_screen_vac - c_screen, e_screen_vac - e_screen, (c_screen_vac - c_screen) / (e_screen_vac - e_screen)
))
