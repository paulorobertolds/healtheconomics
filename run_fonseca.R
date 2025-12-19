# run_fonseca.R
source("model_fonseca.R")

# Define Strategies
# 1. No Screening
mod_none <- define_fonseca_model(screening_strategy = "None", vaccination = FALSE)

# 2. No Screening + Vaccination
mod_none_vax <- define_fonseca_model(screening_strategy = "None", vaccination = TRUE)

# 3. 3x Screening
mod_3x <- define_fonseca_model(screening_strategy = "3x", vaccination = FALSE)

# 4. 3x Screening + Vaccination
mod_3x_vax <- define_fonseca_model(screening_strategy = "3x", vaccination = TRUE)

# 5. 10x Screening
mod_10x <- define_fonseca_model(screening_strategy = "10x", vaccination = FALSE)

# 6. 10x Screening + Vaccination
mod_10x_vax <- define_fonseca_model(screening_strategy = "10x", vaccination = TRUE)


# Explicitly define initial state (100% Normal)
init_vec <- c(1, 0, 0, 0, 0, 0, 0, 0)

# Function to run model wrapper
run_strat <- function(mod, name) {
  print(paste("Running", name, "..."))
  run_model(
    mod$strat,
    parameters = mod$params,
    init = init_vec,
    cycles = 70,
    cost = cost,
    effect = utility,
    method = "life-table"
  )
}

# Run Analysis
res_none <- run_strat(mod_none, "No Screening")
res_none_vax <- run_strat(mod_none_vax, "No Screening + Vax")
res_3x <- run_strat(mod_3x, "Screening 3x")
res_3x_vax <- run_strat(mod_3x_vax, "Screening 3x + Vax")
res_10x <- run_strat(mod_10x, "Screening 10x")
res_10x_vax <- run_strat(mod_10x_vax, "Screening 10x + Vax")

# Combine Results into a list or table logic
results_list <- list(
  "No Screening" = res_none,
  "No Screening + Vax" = res_none_vax,
  "Screening 3x" = res_3x,
  "Screening 3x + Vax" = res_3x_vax,
  "Screening 10x" = res_10x,
  "Screening 10x + Vax" = res_10x_vax
)

# Print Summaries
for (name in names(results_list)) {
  print(paste("---", name, "---"))
  print(summary(results_list[[name]]))
}

# Helper to extract cost/qaly
get_res_values <- function(res) {
  s <- summary(res)
  c(cost = s$res_values$cost[1], qaly = s$res_values$utility[1])
}

# Comparison Table
df_res <- data.frame(
  Strategy = names(results_list),
  Cost = sapply(results_list, function(x) get_res_values(x)["cost"]),
  QALY = sapply(results_list, function(x) get_res_values(x)["qaly"])
)

# Calculate ICERs (Vaccination vs No Vaccination within same screening strategy)
# 1. None + Vax vs None
icer_none <- (df_res$Cost[2] - df_res$Cost[1]) / (df_res$QALY[2] - df_res$QALY[1])

# 2. 3x + Vax vs 3x
icer_3x <- (df_res$Cost[4] - df_res$Cost[3]) / (df_res$QALY[4] - df_res$QALY[3])

# 3. 10x + Vax vs 10x
icer_10x <- (df_res$Cost[6] - df_res$Cost[5]) / (df_res$QALY[6] - df_res$QALY[5])

print("--- ICER Summary (Vaccination Effect) ---")
print(paste("ICER (No Screening):", round(icer_none, 2)))
print(paste("ICER (3x Screening):", round(icer_3x, 2)))
print(paste("ICER (10x Screening):", round(icer_10x, 2)))

print("--- Full Result Table ---")
print(df_res)
