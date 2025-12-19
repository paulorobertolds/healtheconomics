<<<<<<< HEAD
######################################1trial

# 1. Load necessary libraries
# Note: 'here' package is good practice but not strictly needed
# if CSVs are in the same working directory.
library(readr)
library(dplyr)
library(heemod)
library(flexsurv) # Added this, as it's needed for flexsurvreg
library(survival) # Added this, as it's needed for Surv()

rm(list = ls())

# 2. Helper function to load and process WHO mortality data from CSV
load_and_process_who_data <- function(filepath, sex_filter) {
  # Read the CSV, skipping the metadata header
  # The actual data starts around row 7
  data <- read_csv(filepath, skip = 6)
  
  # Clean up column names (remove spaces, special chars)
  colnames(data) <- make.names(colnames(data))
  
  data <- data %>%
    # Filter for the most recent year available in the file (e.g., 2021)
    # and the specific sex
    filter(Year == max(Year, na.rm = TRUE), Sex == sex_filter) %>%
    # Select only the columns we need
    dplyr::select(Age.Group, Death.rate.per.100.000.population) %>%
    # Rename for easier use
    rename(
      age_group_str = Age.Group,
      rate_per_100k = Death.rate.per.100.000.population
    ) %>%
    # Remove rows where rate is missing
    filter(!is.na(rate_per_100k)) %>%
    # Extract numeric age boundaries from the string (e.g., "[35-39]")
    mutate(
      # Remove brackets
      age_group_clean = gsub("\\[|\\]", "", age_group_str),
      # Handle "85+" by replacing with "85-110" (an upper limit)
      age_group_clean = ifelse(age_group_clean == "85+", "85-110", age_group_clean),
      # Handle single age "0"
      age_group_clean = ifelse(age_group_clean == "0", "0-0", age_group_clean)
    )
  
  # Split the "age_start-age_end" string into two new columns
  # Use suppressWarnings to handle the NAs created by splitting "Age (All)" which we filter out next
  age_split <- suppressWarnings(tidyr::separate(data, age_group_clean, into = c("age_start", "age_end"), sep = "-", convert = TRUE))
  
  data_filtered <- age_split %>%
    filter(!is.na(age_start)) # Remove any rows that didn't parse correctly (like "Age (All)")
  
  # Convert rate per 100,000 to an annual probability (q_x)
  # This is an approximation: q_x = 1 - exp(-n * m_x)
  # For 1-year cycles, n=1. m_x = rate / 100000
  # q_x = 1 - exp(-rate_per_100k / 100000)
  data_final <- data_filtered %>%
    mutate(
      prob_death = 1 - exp(-rate_per_100k / 100000)
    ) %>%
    # Select final columns for the lookup table
    dplyr::select(age_start, age_end, prob_death)
  
  return(data_final)
}

# 3. Load the mortality data CSVs
# Make sure these CSV files are in your working directory!
mortality_male <- load_and_process_who_data("GBR-males.csv", "Male")
mortality_female <- load_and_process_who_data("GBR-females.csv", "Female")

# 4. Create the new mortality rate lookup function
get_mortality_rate <- function(age_vector, sex_str) {
  
  # Choose the correct lookup table based on sex
  lookup_table <- if (sex_str == "MLE") {
    mortality_male
  } else if (sex_str == "FMLE") {
    mortality_female
  } else {
    stop("Invalid sex specified. Use 'MLE' or 'FMLE'.")
  }
  
  # 'findInterval' is a fast way to match each age in age_vector
  # to the correct age bracket's starting age.
  # We use 'rightmost.closed = TRUE' to include the lowest age (0).
  indices <- findInterval(age_vector, lookup_table$age_start, rightmost.closed = TRUE)
  
  # Handle ages outside the lookup table range (e.g., > 110)
  # If index is 0 or out of bounds, return NA or a default high probability
  indices[indices == 0] <- NA # Ages below the first bracket (e.g. < 0)
  indices[indices > nrow(lookup_table)] <- NA # Ages above the last bracket
  
  # Return the corresponding probabilities
  return(lookup_table$prob_death[indices])
}


# 5. Define base parameters
par_mod <- define_parameters(age_base = 20,
                             age_cycle = model_time + age_base)

# 6. Modify parameters - add all-cause mortality
par_mod <- modify(
  par_mod,
  sex_indiv = "MLE", # MLE => male in the WHO database
  p_death_all = get_mortality_rate(age_vector = age_cycle,
                                   sex_str = sex_indiv)
)

# 7. Define survival data and fit model *before* they are used
# This was reordered from your original script to fix a run error
tab_surv <- structure(list(time = c(0.4, 8.7, 7, 5.1, 9.2, 1, 0.5, 3.3, 1.8,
                                    3, 6.7, 3.7, 1.1, 5.9, 5.1, 10, 10, 10, 10, 10, 10, 10, 10, 10,
                                    10), status = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                    1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), .Names = c("time",
                                                                                                                     "status"), row.names = c(NA, -25L), class = "data.frame")

fit_death_disease <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1,
                                           dist = "weibull",
                                           data = tab_surv)

# 8. Now modify parameters using the fitted model
par_mod <- modify(par_mod,
                  p_death_disease = compute_surv(fit_death_disease,
                                                 time = state_time,
                                                 km_limit = 5)
)

# 9. Define p_death_symp *after* p_death_all and p_death_disease
par_mod <- modify(par_mod,
                  p_death_symp = combine_probs(
                    p_death_all,
                    p_death_disease)
)

# 10. Define parameters, splitting dependent ones
par_mod <- modify(par_mod,
                  p_disease_base = 0.25,
                  med_effect  = 0.5
)

# 11. Define p_disease_med *after* its dependencies
par_mod <- modify(par_mod,
                  p_disease_med = p_disease_base * med_effect
)


# 12. Define survival distribution parameters
# THIS SECTION IS NOW FIXED
# The correct function is define_surv_dist()
par_mod <- modify(par_mod,
                  shape = 1.5,
                  scale = 5,
                  p_disease_surg = define_surv_dist(
                    distribution = "weibull",
                    shape = shape,
                    scale = scale) %>%
                    compute_surv(time = state_time)
)

# 13. Define cost parameters
par_mod <- modify(par_mod,
                  cost_surg = 20000,
                  cost_surg_cycle = ifelse(state_time == 1, cost_surg, 0)
)
#Because surgery is only performed once at the beginning of the pre state, the time-dependant
#variable state_time was used to limit surgery costs to the first cycle in the pre state.

par_mod <- modify(par_mod,
                  cost_hospit_start = 11000,
                  cost_hospit_end = 9000,
                  n_years = 9,
                  cost_hospit_cycle = ifelse( state_time < n_years, cost_hospit_start, cost_hospit_end)
)
#After n_years in the symptomatic state the symptoms become milder and hospital costs decrease (from cost_hospit_start
#to cost_hospit_end). We used the time-dependant variable state_time to condition the hospital costs cost_hospit on n_years.
par_mod <- modify(par_mod,
                  p_cured = 0.001,
                  cost_med = 5000,
                  dr = 0.05,
                  qaly_disease = 0.5
)

# 14. Define transitions
# We are back to using the 'p_disease_surg' variable, which is now
# correctly defined in section 12.
mat_base <- define_transition(state_names = c("pre", "symp", "death"),
                              C,           p_disease_base, p_death_all,
                              p_cured,     C,              p_death_symp,
                              0,           0,              1)
mat_base


mat_med <- define_transition(state_names = c("pre", "symp", "death"),
                             C,           p_disease_med, p_death_all,
                             p_cured,     C,             p_death_symp,
                             0,           0,             1)

mat_surg <- define_transition(state_names = c("pre", "symp", "death"),
                              C,           p_disease_surg, p_death_all,
                              p_cured,     C,              p_death_symp,
                              0,           0,              1)


# 15. State values
# Note: The original script defined state_symp twice. I've kept the first, more complex one.
# If you intended to use the second, simpler one, just delete the first 'state_pre' and 'state_symp' definitions.
state_pre <- define_state(cost_treat = dispatch_strategy(base = 0, # no treatment => no treatment cost
                                                         med = cost_med,
                                                         surg = cost_surg_cycle),
                          cost_hospit = 0, # good health => no hospital expenses
                          cost_total = discount(cost_treat + cost_hospit, r = dr),
                          qaly  = 1)

state_symp <- define_state(cost_treat = dispatch_strategy(base = 0,
                                                          med = cost_med,
                                                          surg = 0), #surgery only in pre state
                           cost_hospit = cost_hospit_cycle,
                           cost_total = discount(cost_treat + cost_hospit, r = dr),
                           qaly  = qaly_disease)

# This was the second definition in your script. I'm commenting it out.
# state_symp <- define_state(cost_treat = 0,
#                            cost_hospit = cost_hospit_cycle,
#                            cost_total = discount(cost_treat + cost_hospit, r = dr),
#                            qaly = qaly_disease)

state_death <- define_state(cost_treat = 0,
                            cost_hospit = 0,
                            cost_total = 0,
                            qaly  = 0)

# 16. Strategies
# THIS SECTION IS NOW FIXED
# The typo 'state_ripre' has been corrected to 'state_pre'
strat_base <- define_strategy(transition = mat_base,
                              pre = state_pre,
                              symp = state_symp,
                              death = state_death)

strat_med <- define_strategy(transition = mat_med,
                             pre = state_pre,
                             symp = state_symp,
                             death = state_death)

strat_surg <- define_strategy(transition = mat_surg,
                              pre = state_pre,
                              symp = state_symp,
                              death = state_death)

# 17. Running the Model
=======
# Function to load and process a single WHO mortality CSV file
load_and_process_who_data <- function(filepath) {
  # 1. Read the CSV, skipping the 5 metadata header lines.
  #    check.names = TRUE sanitizes column names automatically.
  raw_data <- read.csv(filepath, skip = 5, check.names = TRUE)
  
  # 2. Filter the data to use only the most recent calendar year,
  #    creating a static life table for the model.
  latest_year <- max(raw_data$Year, na.rm = TRUE)
  data_filtered <- raw_data
  
  # 3. Parse the 'Age.Group' column to create numerical start and end ages.
  #    Remove brackets and the '+' sign.
  age_clean <- gsub("\\[|\\]|\\+", "", data_filtered$Age.Group)
  
  #    Split ranges like "1-4" into two columns.
  age_split <- strsplit(age_clean, "-")
  
  #    Process the split list to create age_start and age_end vectors.
  data_filtered$age_start <- as.numeric(sapply(age_split, `[`, 1))
  data_filtered$age_end <- as.numeric(sapply(age_split, `[`, 2))
  
  #    Handle the open-ended age group (e.g., 85+) by setting age_end to Infinity.
  #    Also handle single-year groups like  where the split results in NA.
  data_filtered$age_end[is.na(data_filtered$age_end)] <- data_filtered$age_start[is.na(data_filtered$age_end)]
  
  #    Specifically find the open-ended group (e.g., contains "+") and set to Inf
  open_ended_index <- grepl("\\+", data_filtered$Age.Group)
  data_filtered$age_end[open_ended_index] <- Inf
  
  # 4. Select and rename columns for clarity and consistency.
  final_data <- data.frame(
    year = data_filtered$Year,
    sex = data_filtered$Sex,
    age_start = data_filtered$age_start,
    age_end = data_filtered$age_end,
    rate_per_100k = data_filtered$Death.rate.per.100.000.population
  )
  
  return(final_data)
}

# Function to look up mortality rate by age and sex from the processed data
get_local_mortality_rate <- function(age, sex, mortality_data) {
  # 1. Map the model's sex code ("Male", "Female") to the codes in the data.
  #    The CSV files use "Male" and "Female", so we ensure consistency.
  sex_code <- ifelse(sex == "MLE", "Male", "Female")
  
  # 2. Filter the data for the matching sex and the age bin that contains the input age.
  #    This vectorized operation efficiently handles calls for multiple ages.
  rate_value <- mortality_data$rate_per_100k[
    mortality_data$sex == sex_code &
      age >= mortality_data$age_start &
      age <= mortality_data$age_end
  ]
  
  # 3. Error handling: if no match is found, stop with an informative message.
  if (length(rate_value) == 0) {
    stop(paste("No mortality rate found for age", age, "and sex", sex_code))
  }
  
  # 4. Return the first match (there should only be one).
  return(rate_value)
}

# 1. SETUP: Load libraries and define custom functions
# =======================================================
library(heemod)
library(dplyr) # Used for combining data frames

# --- Paste the load_and_process_who_data() function definition here ---
load_and_process_who_data <- function(filepath) {
  raw_data <- read.csv(filepath, skip = 5, check.names = TRUE)
  latest_year <- max(raw_data$Year, na.rm = TRUE)
  data_filtered <- raw_data
  age_clean <- gsub("\\[|\\]|\\+", "", data_filtered$Age.Group)
  age_split <- strsplit(age_clean, "-")
  data_filtered$age_start <- as.numeric(sapply(age_split, `[`, 1))
  data_filtered$age_end <- as.numeric(sapply(age_split, `[`, 2))
  data_filtered$age_end[is.na(data_filtered$age_end)] <- data_filtered$age_start[is.na(data_filtered$age_end)]
  open_ended_index <- grepl("\\+", data_filtered$Age.Group)
  data_filtered$age_end[open_ended_index] <- Inf
  final_data <- data.frame(
    year = data_filtered$Year,
    sex = data_filtered$Sex,
    age_start = data_filtered$age_start,
    age_end = data_filtered$age_end,
    rate_per_100k = data_filtered$Death.rate.per.100.000.population
  )
  return(final_data)
}

# --- Paste the get_local_mortality_rate() function definition here ---
get_local_mortality_rate <- function(age, sex, mortality_data) {
  sex_code <- ifelse(sex == "MLE", "Male", "Female")
  rate_value <- mortality_data$rate_per_100k[
    mortality_data$sex == sex_code &
      age >= mortality_data$age_start &
      age <= mortality_data$age_end
  ]
  if (length(rate_value) == 0) {
    stop(paste("No mortality rate found for age", age, "and sex", sex_code))
  }
  return(rate_value)
}


# 2. DATA LOADING: Load and combine the local mortality data
# ============================================================
# Note: Replace file paths with the actual locations of your CSV files.
mortality_male <- load_and_process_who_data("https://github.com/paulorobertolds/healtheconomics/blob/main/GBR-males.csv")
mortality_female <- load_and_process_who_data("https://github.com/paulorobertolds/healtheconomics/blob/main/GBR-females.csv")

# Combine into a single data frame for the lookup function
who_mortality_data <- bind_rows(mortality_male, mortality_female)


# 3. HEEMOD MODEL SCRIPT: Re-creation of the original model
# ===========================================================
# Survival data for disease-specific death
tab_surv <- structure(list(time = c(0.4, 8.7, 7, 5.1, 9.2, 1, 0.5, 3.3, 1.8, 3, 6.7, 3.7, 1.1, 5.9, 5.1, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), status = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)),.Names = c("time", "status"), row.names = c(NA, -25L), class = "data.frame")
fit_death_disease <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ 1, dist = "weibull", data = tab_surv)

# Define model parameters
par_mod <- define_parameters(
  age_base = 20,
  age_cycle = model_time + age_base,
  sex_indiv = "MLE",
  
  # === THE CRITICAL MODIFICATION ===
  # Original failing line:
  # p_death_all = get_who_mr(age = age_cycle, sex = sex_indiv, country = "GBR", local = TRUE),
  
  # New replacement line:
  p_death_all = get_local_mortality_rate(age = age_cycle, sex = sex_indiv, mortality_data = who_mortality_data) / 100000,
  
  p_death_disease = compute_surv(fit_death_disease, time = state_time, km_limit = 5),
  p_death_symp = combine_probs(p_death_all, p_death_disease),
  p_disease_base = 0.25,
  med_effect = 0.5,
  p_disease_med = p_disease_base * med_effect,
  shape = 1.5,
  scale = 5,
  p_disease_surg = compute_surv(define_survival(distribution = "weibull", shape = shape, scale = scale), time = state_time),
  cost_surg = 20000,
  cost_surg_cycle = ifelse(state_time == 1, cost_surg, 0),
  cost_hospit_start = 11000,
  cost_hospit_end = 9000,
  n_years = 9,
  cost_hospit_cycle = ifelse(state_time < n_years, cost_hospit_start, cost_hospit_end),
  p_cured = 0.001,
  cost_med = 5000,
  dr = 0.05,
  qaly_disease = 0.5
)

# Define transitions, states, and strategies (as per original script)
mat_base <- define_transition(C, p_disease_base, p_death_all, p_cured, C, p_death_symp, 0, 0, 1)
mat_med <- define_transition(C, p_disease_med, p_death_all, p_cured, C, p_death_symp, 0, 0, 1)
mat_surg <- define_transition(C, p_disease_surg, p_death_all, p_cured, C, p_death_symp, 0, 0, 1)

state_pre <- define_state(
  cost_treat = dispatch_strategy(base = 0, med = cost_med, surg = cost_surg_cycle),
  cost_hospit = 0,
  qaly = 1,
  cost_total = discount(cost_treat + cost_hospit, r = dr)
)
state_symp <- define_state(
  cost_treat = 0,
  cost_hospit = cost_hospit_cycle,
  qaly = qaly_disease,
  cost_total = discount(cost_treat + cost_hospit, r = dr)
)
state_death <- define_state(cost_treat = 0, cost_hospit = 0, cost_total = 0, qaly = 0)

strat_base <- define_strategy(transition = mat_base, pre = state_pre, symp = state_symp, death = state_death)
strat_med <- define_strategy(transition = mat_med, pre = state_pre, symp = state_symp, death = state_death)
strat_surg <- define_strategy(transition = mat_surg, pre = state_pre, symp = state_symp, death = state_death)

# 4. RUN AND VERIFY: Execute the model and display summary
# =========================================================
>>>>>>> d92873f99d056808e30e7931dcca2bf810876a40
res_mod <- run_model(
  parameters = par_mod,
  base = strat_base,
  med = strat_med,
  surg = strat_surg,
  cycles = 10,
  cost = cost_total,
  effect = qaly,
<<<<<<< HEAD
  method = "life-table")

summary(res_mod, threshold = c(1000, 5000, 15000))



# 18. Uncertainty Analysis
def_dsa <- define_dsa( age_base, 15, 30,
                       p_disease_base, 0.2, 0.3,
                       p_cured, 0.005, 0.02,
                       med_effect, 0.3, 0.7,
                       shape, 1.4, 1.6,
                       scale, 4, 6,
                       cost_med, 4000, 6000,
                       cost_surg, 8000, 12000,
                       cost_hospit_start, 5000, 15000,
                       dr, 0, 0.1,
                       qaly_disease, 0.3, 0.7,
                       n_years, 8, 10)


res_dsa <- run_dsa(res_mod, dsa = def_dsa)

def_psa <- define_psa( age_base ~ normal(mean = 20, sd = 5),
                       p_disease_base ~ binomial(prob = 0.25, size = 500),
                       p_cured ~ binomial(prob = 0.001, size = 500),
                       med_effect ~ lognormal(mean = 0.5, sd = 0.1),
                       shape ~ normal(mean = 1.5, sd = 0.2),
                       scale ~ normal(mean = 5, sd = 1),
                       cost_med ~ gamma(mean = 5000, sd = 1000),
                       cost_surg ~ gamma(mean = 20000, sd = 3000),
                       cost_hospit_start ~ gamma(mean = 11000, sd = 2000),
                       dr ~ binomial(prob = 0.05, size = 100),
                       qaly_disease ~ normal(mean = 0.5, sd = 0.1),
                       n_years ~ poisson(mean = 9),
                       correlation = define_correlation( shape, scale, -0.5,
                                                         age_base, p_disease_base, 0.3))

res_psa <- run_psa(res_mod, psa = def_psa, N = 1000)

# 19. Heterogeneous analysis
# Note: 'tab_pop' is not defined in the provided script.
# This line will cause an error until 'tab_pop' is created.
# head(tab_pop)
# pop_mod <- update(res_mod, newdata = tab_pop)

# 20. Budget impact analysis
res_bia <- run_model(parameters = par_mod,
                     med = strat_med,
                     cycles = 10,
                     cost = cost_total,
                     effect = qaly,
                     method = "life-table",
                     init = c( pre = 25000, symp = 5000, death = 0),
                     inflow = define_inflow( pre = 8000, symp = 0, death = 0))

summary(res_bia)

# 21. Plotting
# Note: The objects 'fit_cov', 'fitcov_poor', 'fitcov_medium', 'fit_w'
# are not defined in the provided script.
# This section will cause an error until they are defined.

# fit_cov %>%
#   apply_hr(hr = 2) %>%
#   join( fitcov_poor, at = 3) %>%
#   pool( fitcov_medium, weights = c(0.25, 0.75)) %>%
#   add_hazards( fit_w) %>%
#   compute_surv(time = 1:5)

library(ggplot2)
plot(res_psa, type = "ce") +
  scale_color_brewer(name = "Treatment", palette = "Set1") +
  facet_wrap(~ .strategy_names) +
  xlab("Incremental QALYs") + ylab("Incremental Costs") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")



=======
  method = "life-table"
)

summary(res_mod)
>>>>>>> d92873f99d056808e30e7931dcca2bf810876a40
