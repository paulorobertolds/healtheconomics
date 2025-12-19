# check_calibration.R

library(heemod)
source("parameters.R")
source("model_structure.R")

# Define Base Parameters
params_base <- define_goldie_parameters()
strat <- define_goldie_model(params_base)

# Run No Intervention
res <- run_model(
  No_Intervention = strat,
  parameters = params_base,
  cycles = 140,
  cost = cost,
  effect = utility,
  method = "life-table"
)

# Extract counts
counts <- get_counts(res)
# counts is a list of data frames? No, it returns a data frame with 'markov_cycle', 'state_names', 'count'.
# Let's check the structure.

# Calculate cumulative cancer incidence
# Sum of people entering Cancer states?
# Easier: Count people in Cancer states + Death_Cancer at the end?
# But people die of other causes too.
# We need to track "Ever Cancer".
# Since we don't have a tracker state, we can sum the flow into Cancer_I_Undet.
# Flow into Cancer_I_Undet comes from CIN23.
# Flow = Count(CIN23) * p_cin23_to_cancer.

# We can extract transition probabilities?
# Or just look at the counts in Cancer states.
# Total Cancer Cases = Sum(New Cases per cycle).
# New Cases = Count(CIN23) * p_cin23_to_cancer.
# We need p_cin23_to_cancer from parameters.
# It varies by age.

# Alternative: Use heemod's trace to estimate.
# We can just sum the counts in Cancer states? No, that's prevalence.
# We need incidence.

# Let's print the final counts first.
print(tail(counts, 1))

# Calculate roughly:
# Total deaths by cancer
final_death_cancer <- subset(counts, model_time == 140 & state_names == "Death_Cancer")$count
print(paste("Cancer Deaths (per 1000):", final_death_cancer))

# This is a lower bound for incidence (since some survive or die of other).
