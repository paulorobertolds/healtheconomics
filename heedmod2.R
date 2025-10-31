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
res_mod <- run_model(
  parameters = par_mod,
  base = strat_base,
  med = strat_med,
  surg = strat_surg,
  cycles = 10,
  cost = cost_total,
  effect = qaly,
  method = "life-table"
)

summary(res_mod)
