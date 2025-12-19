# # ------------------------------------------------------------------------------
# # SCRIPT 2: HEEMOD ANALYSIS (FONSECA 2013 REPLICATION)
# # ------------------------------------------------------------------------------
# library(heemod)
# library(readxl)
# library(dplyr)
# library(tidyr)
# 
# # --- 1. DATA IMPORT & PROCESSING ---
# 
# params_file <- "parameters.xlsx"
# 
# # A. Load Excel Parameters
# scalar_vec <- read_excel(params_file, sheet = "params_scalar") %>% tibble::deframe()
# age_logic_df <- read_excel(params_file, sheet = "params_age_dependent")
# state_df <- read_excel(params_file, sheet = "state_values")
# schedules_df <- read_excel(params_file, sheet = "screening_schedules")
# 
# # B. Process Mortality Data (FIXED FUNCTION)
# mortality_raw <- read.csv("BRA-females.csv", skip = 5, check.names = FALSE, row.names = NULL)
# 
# get_mortality_vector <- function(df) {
#   # Filter for 2019 (pre-pandemic)
#   df_clean <- df %>% 
#     filter(Year == 2019) %>%
#     # Rename the specific column to avoid space/character issues
#     select(AgeGroup = `Age Group`, Rate = `Death rate per 100 000 population`)
#   
#   probs <- numeric(101) # Vector for ages 0 to 100
#   
#   for(i in 1:nrow(df_clean)) {
#     # Extract rate safely
#     r <- df_clean$Rate[i]
#     if(is.na(r)) r <- 0
#     p <- r / 100000
#     
#     # Parse Age Group string (e.g. "[20-24]", "", "[85+]")
#     txt <- df_clean$AgeGroup[i]
#     nums <- as.numeric(unlist(regmatches(txt, gregexpr("\\d+", txt))))
#     
#     if(length(nums) == 0) next
#     
#     if(grepl("\\+", txt)) {
#       idx <- (nums[1]:100) + 1
#     } else if(length(nums) == 1) {
#       idx <- nums[1] + 1
#     } else {
#       idx <- (nums[1]:nums[2]) + 1
#     }
#     
#     # Assign probability to indices (ensuring within bounds)
#     idx <- idx[idx <= 101]
#     probs[idx] <- p
#   }
#   
#   # Fill any gaps with previous value (simple imputation)
#   for(j in 2:101) if(probs[j] == 0) probs[j] <- probs[j-1]
#   return(probs)
# }
# 
# v_mortality <- get_mortality_vector(mortality_raw)
# 
# # --- 2. MODEL SETUP ---
# 
# # Helper: Convert Excel logic table to a vector of size 101
# get_age_curve <- function(param_name) {
#   sub <- age_logic_df %>% filter(ParameterName == param_name) %>% arrange(AgeThreshold)
#   vec <- numeric(101)
#   cur_age <- 0
#   for(i in 1:nrow(sub)) {
#     limit <- sub$AgeThreshold[i]
#     val <- sub$BaseValue[i]
#     idx <- (cur_age:limit) + 1
#     idx <- idx[idx <= 101]
#     vec[idx] <- val
#     cur_age <- limit + 1
#   }
#   return(vec)
# }
# 
# # Pre-calculate curves
# v_incid <- get_age_curve("p_healthy_to_lsil")
# v_reg_lsil <- get_age_curve("p_lsil_regress")
# v_reg_hsil <- get_age_curve("p_hsil_regress")
# 
# # Helper for scalars
# get_p <- function(n) scalar_vec[[n]]
# 
# # Define Parameters
# param_def <- define_parameters(
#   # Time
#   age_start = 12,
#   age = age_start + model_time - 1,
#   
#   # 1. Probabilities from Vectors
#   p_death_bg = v_mortality[pmin(age + 1, 101)],
#   p_incid_nat = v_incid[pmin(age + 1, 101)],
#   p_lsil_reg = v_reg_lsil[pmin(age + 1, 101)],
#   p_hsil_reg = v_reg_hsil[pmin(age + 1, 101)],
#   
#   # 2. Vaccine Effect (50% reduction in incidence if vax_active=1)
#   p_healthy_lsil = p_incid_nat * (1 - (vax_active * get_p("vaccine_efficacy") * get_p("vaccine_coverage"))),
#   
#   # 3. Screening Logic
#   is_screen_year = ifelse(age %in% screen_schedule, 1, 0),
#   
#   p_detect_lsil = is_screen_year * get_p("sens_pap_lsil"),
#   p_detect_hsil = is_screen_year * get_p("sens_pap_hsil"),
#   
#   # 4. Costs
#   c_vax = ifelse(model_time == 1, vax_active * get_p("vaccine_cost_total"), 0),
#   c_screen = is_screen_year * get_p("cost_pap"),
#   # If detected, we incur diagnosis + treatment costs
#   c_treat_lsil = p_detect_lsil * (get_p("cost_colpo") + get_p("cost_treat_lsil")),
#   c_treat_hsil = p_detect_hsil * (get_p("cost_colpo") + get_p("cost_biopsy") + get_p("cost_treat_hsil")),
#   
#   # 5. Cancer Mortality (Competing Risk)
#   # p_die = 1 - (S_bg * S_cancer)
#   p_die_caloc = 1 - ((1-p_death_bg) * (1-get_p("p_die_caloc"))),
#   p_die_careg = 1 - ((1-p_death_bg) * (1-get_p("p_die_careg"))),
#   p_die_camet = 1 - ((1-p_death_bg) * (1-get_p("p_die_camet")))
# )

# ------------------------------------------------------------------------------
# SCRIPT 2: HEEMOD ANALYSIS - DEBUGGED VERSION
# ------------------------------------------------------------------------------
library(heemod)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

# --- 1. ROBUST DATA EXTRACTION ---

params_file <- "parameters.xlsx"
mortality_file <- "BRA-females.csv"

# A. Load Excel Data
scalar_vec <- read_excel(params_file, sheet = "params_scalar") %>% tibble::deframe()
age_logic_df <- read_excel(params_file, sheet = "params_age_dependent")
state_df <- read_excel(params_file, sheet = "state_values")
schedules_df <- read_excel(params_file, sheet = "screening_schedules")

# B. DEBUGGED Mortality Processing
get_mortality_vector <- function(csv_file) {
  # First, let's inspect the file structure
  cat("Reading mortality file:", csv_file, "\n")
  
  # Read with more flexible parameters
  raw <- read.csv(csv_file, skip = 5, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Debug: Print structure to understand the data
  cat("File structure:\n")
  print(head(raw))
  cat("Column names:\n")
  print(names(raw))
  
  # Find the death rate column more flexibly
  rate_col <- NULL
  possible_cols <- c("Death rate", "Death Rate", "Rate", "Mortality", "death")
  
  for (col_pattern in possible_cols) {
    matches <- grep(col_pattern, names(raw), ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      rate_col <- matches[1]
      cat("Found rate column:", rate_col, "\n")
      break
    }
  }
  
  if (is.null(rate_col)) {
    # If no specific rate column found, try to find any numeric column
    numeric_cols <- names(raw)[sapply(raw, is.numeric)]
    if (length(numeric_cols) > 0) {
      rate_col <- numeric_cols[1]
      cat("Using numeric column as rate:", rate_col, "\n")
    } else {
      stop("Could not find Death Rate column in CSV. Available columns: ", paste(names(raw), collapse = ", "))
    }
  }
  
  # Find age group column
  age_col <- NULL
  age_patterns <- c("Age", "Age Group", "AgeGroup", "age")
  for (col_pattern in age_patterns) {
    matches <- grep(col_pattern, names(raw), ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      age_col <- matches[1]
      cat("Found age column:", age_col, "\n")
      break
    }
  }
  
  if (is.null(age_col)) {
    stop("Could not find Age Group column in CSV")
  }
  
  # Filter and clean data
  df <- raw %>% 
    # Try to find year column if exists, otherwise use all data
    { 
      if ("Year" %in% names(.)) filter(., Year == 2019) else .
    } %>%
    select(AgeGroup = all_of(age_col), Rate = all_of(rate_col)) %>%
    # Convert rate to numeric, handling any non-numeric values
    mutate(
      Rate = as.numeric(gsub("[^0-9.]", "", Rate))  # Remove non-numeric characters
    ) %>%
    filter(!is.na(Rate) & !is.na(AgeGroup))  # Remove NA values
  
  cat("Processed data:\n")
  print(df)
  
  probs <- numeric(101) # Ages 0-100
  
  for(i in 1:nrow(df)) {
    # DEBUGGED: Safe numeric conversion
    r_val <- df$Rate[i]
    
    # Check if value is valid
    if (length(r_val) == 0 || is.na(r_val) || !is.numeric(r_val)) {
      cat("Skipping invalid rate at row", i, ":", r_val, "\n")
      next
    }
    
    prob <- r_val / 100000
    
    # Parse Age String safely
    txt <- as.character(df$AgeGroup[i])
    nums <- as.numeric(unlist(regmatches(txt, gregexpr("\\d+", txt))))
    
    if(length(nums) == 0) {
      cat("Could not parse age group:", txt, "\n")
      next
    }
    
    # Determine index range (R is 1-based, so Age 0 is index 1)
    if(grepl("\\+", txt)) {
      idx <- (nums[1]:100) + 1
    } else if(length(nums) == 1) {
      idx <- nums[1] + 1
    } else {
      idx <- (nums[1]:nums[2]) + 1
    }
    
    # Assign probability to valid indices
    idx <- idx[idx <= 101 & idx >= 1]
    if (length(idx) > 0) {
      probs[idx] <- prob
    }
  }
  
  # Fill gaps (imputation) - only if there are zeros after processing
  if (any(probs == 0)) {
    for(k in 2:101) {
      if(probs[k] == 0 && probs[k-1] > 0) {
        probs[k] <- probs[k-1]
      }
    }
  }
  
  cat("Final mortality probabilities (first 20 ages):\n")
  print(probs[1:20])
  
  return(probs)
}

# Try loading mortality data with error handling
tryCatch({
  v_mortality <- get_mortality_vector(mortality_file)
}, error = function(e) {
  cat("Error loading mortality data:", e$message, "\n")
  cat("Using default mortality rates as fallback\n")
  # Fallback: Use generic mortality rates
  v_mortality <- rep(0.001, 101)  # Default low mortality rate
})

# --- 2. MODEL DEFINITION ---

# Helper for age curves
get_age_curve <- function(param_name) {
  sub <- age_logic_df %>% filter(ParameterName == param_name) %>% arrange(AgeThreshold)
  vec <- numeric(101)
  cur <- 0
  for(i in 1:nrow(sub)) {
    lim <- sub$AgeThreshold[i]
    val <- sub$BaseValue[i]
    idx <- (cur:lim) + 1
    idx <- idx[idx <= 101]
    vec[idx] <- val
    cur <- lim + 1
  }
  return(vec)
}

v_incid <- get_age_curve("p_healthy_to_lsil")
v_reg_lsil <- get_age_curve("p_lsil_regress")
v_reg_hsil <- get_age_curve("p_hsil_regress")
get_p <- function(n) scalar_vec[[n]]

# Define Parameters
param_def <- define_parameters(
  # Time
  age_start = 12,
  age = age_start + model_time - 1,
  
  # 1. Age-Dependent Probabilities
  p_death_bg = v_mortality[pmin(age + 1, 101)],
  p_incid_nat = v_incid[pmin(age + 1, 101)],
  p_lsil_reg = v_reg_lsil[pmin(age + 1, 101)],
  p_hsil_reg = v_reg_hsil[pmin(age + 1, 101)],
  
  # 2. Vaccine Effect (Strategy Flag: vax_active)
  p_healthy_lsil = p_incid_nat * (1 - (vax_active * get_p("vaccine_efficacy") * get_p("vaccine_coverage"))),
  
  # 3. Screening Logic (Strategy Schedule)
  is_screen_year = ifelse(age %in% screen_schedule, 1, 0),
  
  p_detect_lsil = is_screen_year * get_p("sens_pap_lsil"),
  p_detect_hsil = is_screen_year * get_p("sens_pap_hsil"),
  
  # 4. Costs
  c_vax = ifelse(model_time == 1, vax_active * get_p("vaccine_cost_total"), 0),
  c_screen = is_screen_year * get_p("cost_pap"),
  # Treatment costs if detected
  c_treat_lsil = p_detect_lsil * (get_p("cost_colpo") + get_p("cost_treat_lsil")),
  c_treat_hsil = p_detect_hsil * (get_p("cost_colpo") + get_p("cost_treat_hsil")),
  
  # 5. Cancer Mortality
  p_die_caloc = 1 - ((1-p_death_bg) * (1-get_p("p_die_caloc"))),
  p_die_careg = 1 - ((1-p_death_bg) * (1-get_p("p_die_careg"))),
  p_die_camet = 1 - ((1-p_death_bg) * (1-get_p("p_die_camet")))
)

# [Rest of the code remains the same...]

# Transition Matrix
trans_mat <- define_transition(
  state_names = c("Healthy", "LSIL", "HSIL", "Cancer_Early", "Cancer_Regional", "Cancer_Metastatic", "Death"),
  
  # Healthy
  C, p_healthy_lsil, 0, 0, 0, 0, p_death_bg,
  
  # LSIL (Regression + Screen Cure vs Progression)
  p_lsil_reg + (p_detect_lsil * get_p("eff_cryo_lsil")), C, get_p("p_lsil_to_hsil"), get_p("p_lsil_to_cancer"), 0, 0, p_death_bg,
  
  # HSIL (Regression + Screen Cure vs Progression)
  p_hsil_reg + (p_detect_hsil * get_p("eff_cryo_hsil")), 0, C, get_p("p_hsil_to_cancer"), 0, 0, p_death_bg,
  
  # Cancer
  0, 0, 0, C, get_p("p_caloc_to_careg"), 0, p_die_caloc,
  0, 0, 0, 0, C, get_p("p_careg_to_camet"), p_die_careg,
  0, 0, 0, 0, 0, C, p_die_camet,
  0, 0, 0, 0, 0, 0, 1
)

# Define States Programmatically
st_list <- list()
for(s in state_df$StateName) {
  vals <- state_df %>% filter(StateName == s)
  
  # Dynamic cost string
  cost_str <- paste(vals$AnnualCost)
  if(s == "Healthy") cost_str <- paste(cost_str, "+ c_vax + c_screen")
  if(s == "LSIL")    cost_str <- paste(cost_str, "+ c_screen + c_treat_lsil")
  if(s == "HSIL")    cost_str <- paste(cost_str, "+ c_screen + c_treat_hsil")
  
  st_list[[s]] <- define_state(
    cost = eval(parse(text = cost_str)),
    cost_init = vals$TxCost_Init,
    utility = vals$QALY_Weight
  )
}

# --- 3. EXECUTE SCENARIOS - DEBUGGED VERSION ---

# Strategy Factory - FIXED VERSION
make_strat <- function(vax, sched) {
  define_strategy(
    transition = trans_mat,
    parameters = param_def,
    Healthy = st_list[["Healthy"]],
    LSIL = st_list[["LSIL"]],
    HSIL = st_list[["HSIL"]],
    Cancer_Early = st_list[["Cancer_Early"]],
    Cancer_Regional = st_list[["Cancer_Regional"]],
    Cancer_Metastatic = st_list[["Cancer_Metastatic"]],
    Death = st_list[["Death"]],
    vax_active = vax,
    screen_schedule = sched
  )
}

# Schedules
sched_0 <- c(999)  # No screening
sched_3 <- schedules_df %>% filter(Strategy == "Screen_3") %>% pull(Age)
sched_10 <- schedules_df %>% filter(Strategy == "Screen_10") %>% pull(Age)

# Settings
cycles_run <- 88 # 12 to 100 (88 years)
pop <- c(1000, 0, 0, 0, 0, 0, 0)  # All start healthy
dr <- 0.05
disc <- c(cost = dr, effect = dr)

# SCENARIO 1: NON-SCREENING
cat("\n--- SCENARIO 1: NON-SCREENING ---\n")
tryCatch({
  res1 <- run_model(
    NoVax = make_strat(0, sched_0),
    Vax   = make_strat(1, sched_0),
    parameters = param_def, 
    cycles = cycles_run, 
    init = pop,
    cost = "cost", 
    effect = "utility",
    method = "life-table", 
    discount = disc
  )
  print(summary(res1))
}, error = function(e) {
  cat("Error in Scenario 1:", e$message, "\n")
})

# SCENARIO 2: 3 SCREENINGS (BASE CASE)
cat("\n--- SCENARIO 2: 3 SCREENINGS (BASE CASE) ---\n")
tryCatch({
  res2 <- run_model(
    NoVax_3S = make_strat(0, sched_3),
    Vax_3S   = make_strat(1, sched_3),
    parameters = param_def, 
    cycles = cycles_run, 
    init = pop,
    cost = "cost", 
    effect = "utility",
    method = "life-table", 
    discount = disc
  )
  print(summary(res2))
}, error = function(e) {
  cat("Error in Scenario 2:", e$message, "\n")
})

# SCENARIO 3: 10 SCREENINGS
cat("\n--- SCENARIO 3: 10 SCREENINGS ---\n")
tryCatch({
  res3 <- run_model(
    NoVax_10S = make_strat(0, sched_10),
    Vax_10S   = make_strat(1, sched_10),
    parameters = param_def, 
    cycles = cycles_run, 
    init = pop,
    cost = "cost", 
    effect = "utility",
    method = "life-table", 
    discount = disc
  )
  print(summary(res3))
}, error = function(e) {
  cat("Error in Scenario 3:", e$message, "\n")
})

# ALTERNATIVE APPROACH IF STILL HAVING ISSUES:

# If the above doesn't work, try this simplified state definition:
define_states_simple <- function() {
  define_state(
    cost = 0,
    cost_init = 0,
    utility = 1.0
  )
}

# Simple state definitions (remove complex cost calculations)
st_list_simple <- list(
  Healthy = define_state(
    cost = 0,
    cost_init = 0,
    utility = 1.0
  ),
  LSIL = define_state(
    cost = 0,
    cost_init = 0,
    utility = 1.0
  ),
  HSIL = define_state(
    cost = 0,
    cost_init = 0,
    utility = 1.0
  ),
  Cancer_Early = define_state(
    cost = 370.20,
    cost_init = 3702,
    utility = 0.76
  ),
  Cancer_Regional = define_state(
    cost = 842.00,
    cost_init = 8420,
    utility = 0.67
  ),
  Cancer_Metastatic = define_state(
    cost = 262.50,
    cost_init = 2625,
    utility = 0.48
  ),
  Death = define_state(
    cost = 0,
    cost_init = 0,
    utility = 0.00
  )
)

# Alternative strategy factory with simple states
make_strat_simple <- function(vax, sched) {
  define_strategy(
    transition = trans_mat,
    parameters = param_def,
    Healthy = st_list_simple[["Healthy"]],
    LSIL = st_list_simple[["LSIL"]],
    HSIL = st_list_simple[["HSIL"]],
    Cancer_Early = st_list_simple[["Cancer_Early"]],
    Cancer_Regional = st_list_simple[["Cancer_Regional"]],
    Cancer_Metastatic = st_list_simple[["Cancer_Metastatic"]],
    Death = st_list_simple[["Death"]],
    vax_active = vax,
    screen_schedule = sched
  )
}

# Test with simple states if complex version fails
cat("\n--- TESTING WITH SIMPLIFIED STATES ---\n")
tryCatch({
  res_test <- run_model(
    Test = make_strat_simple(0, sched_0),
    parameters = param_def, 
    cycles = 10,  # Shorter for testing
    init = pop,
    cost = "cost", 
    effect = "utility",
    method = "life-table", 
    discount = disc
  )
  print(summary(res_test))
}, error = function(e) {
  cat("Error with simplified states:", e$message, "\n")
})

####END####

# 
# ################################################################################
# #
# # A Comprehensive, Parameterized R/heemod Model for HPV Cost-Effectiveness
# #
# # This script builds a non-homogeneous, time-dependent Markov cohort model
# # based on the methodology of Fonseca et al. (2013).
# #
# # All model parameters (probabilities, costs, utilities, and logic) are
# # dynamically extracted from an external "parameters.xlsx" file and
# # "BRA-females.csv" , as requested.
# #
# ################################################################################
# 
# 
# #
# # 1. LOAD LIBRARIES
# # ------------------------------------------------------------------------------
# # install.packages(c("heemod", "readxl", "dplyr", "tidyr", "purrr"))
# 
# library(heemod)      # For Markov modeling 
# library(readxl)      # To read the Excel parameter file
# library(dplyr)       # For data manipulation 
# library(tidyr)       # For data reshaping (nest, fill)
# library(purrr)       # For programmatic list manipulation (map, pmap)
# 
# 
# #
# # 2. DEFINE FILE PATHS
# # ------------------------------------------------------------------------------
# #!!! ATTENTION: Update these paths to the location of your files!!!
# file_path_params <- "parameters.xlsx"
# file_path_mortality <- "BRA-females.csv"
# 
# 
# #
# # 3. DATA INGESTION: MORTALITY (BRA-females.csv)
# # ------------------------------------------------------------------------------
# # This section processes the raw WHO mortality data  into a 1-year
# # probability vector for ages 0-100.
# 
# # 3.1. Helper Function to Expand Age Bands
# expand_age_bands <- function(age_group_str, prob) {
#   # Handle "85+" or "85_over"
#   if (grepl("85", age_group_str)) {
#     ages <- 85:100 # Assume model runs to age 100
#   } else {
#     nums <- as.numeric(unlist(regmatches(age_group_str, gregexpr("\\d+", age_group_str))))
#     if (length(nums) == 1) { # Handle single age, e.g., "" or "Age00"
#       ages <- nums
#     } else {
#       ages <- nums[1]:nums[2] # Handle band, e.g., "[1-4]"
#     }
#   }
#   set_names(rep(prob, length(ages)), ages)
# }
# 
# # 3.2. Load and Process Mortality Data
# mortality_data_raw <- read.csv(
#   file_path_mortality, 
#   skip = 5, # Skip metadata header rows
#   check.names = FALSE
# )
# 
# # 3.3. Clean, Filter, and Transform Data
# mortality_data_clean <- mortality_data_raw %>%
#   # METHODOLOGICAL CHOICE: Filter for 2019.
#   # This is the most recent, pre-pandemic year in the data.
#   # Years 2020-2021 show COVID-19 excess mortality, which would
#   # incorrectly inflate background mortality and devalue the intervention.
#   filter(Year == 2019) %>%
#   select(`Age Group`, `Death rate per 100 000 population`) %>%
#   rename(
#     age_group_str = `Age Group`,
#     rate_per_100k = `Death rate per 100 000 population`
#   ) %>%
#   mutate(p_death = rate_per_100k / 100000) # Convert rate to probability
# 
# # 3.4. Expand Bands to 1-Year Vector (Ages 0-100)
# v_mortality_brazil <- pmap(
#   list(mortality_data_clean$age_group_str, mortality_data_clean$p_death),
#   expand_age_bands
# ) %>%
#   unlist() %>%
#   .[order(as.numeric(names(.)))]
# 
# # 3.5. Ensure vector covers ages 0-100 and fill any gaps
# full_age_vector <- 0:100
# v_mortality_brazil <- set_names(
#   v_mortality_brazil[as.character(full_age_vector)], 
#   full_age_vector
# ) %>%
#   tidyr::fill(everything(),.direction = "down") # Fills NAs with last known value
# 
# # Make this vector globally available for the parameter object
# # (This is a simple way; a more advanced method would pass it as an argument)
# v_mortality_global <- v_mortality_brazil
# 
# 
# #
# # 4. DATA INGESTION: MODEL PARAMETERS (parameters.xlsx)
# # ------------------------------------------------------------------------------
# # This section reads all data from the structured Excel file.
# 
# # 4.1. Import Scalar Parameters (Sheet: 'params_scalar')
# # Converted to a named list (e.g., list(p_lsil_hsil = 0.110,...))
# params_scalar_list <- read_excel(file_path_params, sheet = "params_scalar") %>%
#   tibble::deframe()
# 
# # 4.2. Import Age-Dependent Parameters (Sheet: 'params_age_dependent')
# # Creates a list of data frames (e.g., age_params_list$p_healthy_to_lsil)
# age_params_list <- read_excel(file_path_params, sheet = "params_age_dependent") %>%
#   group_by(ParameterName) %>%
#   # Ensure data is sorted by threshold for correct logic
#   arrange(AgeThreshold,.by_group = TRUE) %>%
#   # Split into a list of data.frames
#   group_split(.keep = FALSE) %>%
#   set_names(map_chr(., ~ first(.$ParameterName)))
# 
# # 4.3. Import and Reshape State Values (Sheets: 'state_costs', 'state_utilities')
# state_costs_df <- read_excel(file_path_params, sheet = "state_costs")
# state_utils_df <- read_excel(file_path_params, sheet = "state_utilities")
# 
# # Join, rename to heemod defaults ('cost', 'effect'), and reshape
# state_values_list <- full_join(state_costs_df, state_utils_df, by = "StateName") %>%
#   rename(
#     cost = AnnualCost,
#     effect = QALY_Weight
#   ) %>%
#   # Convert to the nested list structure: list(State = list(cost=A, effect=B),...)
#   group_by(StateName) %>%
#   nest() %>%
#   mutate(data = map(data, as.list)) %>%
#   tibble::deframe()
# 
# #
# # 5. DEFINE MASTER PARAMETER OBJECT (define_parameters)
# # ------------------------------------------------------------------------------
# # This is the "brain" of the model. All logic is defined here.
# 
# # --- Get the logic data frames from the list we made in 4.2 ---
# logic_p_h_lsil <- age_params_list$p_healthy_to_lsil
# logic_p_lsil_regress <- age_params_list$p_lsil_regress
# 
# param_obj <- define_parameters(
#   
#   # === 1. Core Model Time & Age ===
#   # Cohort starts at age 12 (vaccination age )
#   # model_time starts at 1 , so age in cycle 1 is 12
#   age_start = 12,
#   age = age_start + model_time - 1,
#   
#   # === 2. Scalar Parameters ===
#   # Splat (!!!) the named list of scalar params from Excel (Section 4.1)
#   !!!params_scalar_list,
#   
#   # === 3. Dynamic All-Cause Mortality ===
#   # Looks up the correct mortality probability from our vector (Section 3.5)
#   # based on the cohort's current 'age'.
#   # 'age + 1' is used because R vectors are 1-indexed (age 0 is at index 1).
#   p_death_other = v_mortality_global[pmin(age + 1, length(v_mortality_global))],
#   
#   # === 4. Age-Dependent Transition Probabilities ===
#   # This logic is now fully driven by the 'params_age_dependent' sheet
#   
#   # Probability: Healthy -> LSIL (data from )
#   p_healthy_to_lsil = ifelse(
#     age < logic_p_h_lsil$AgeThreshold[1], 
#     logic_p_h_lsil$BaseValue[1], 
#     logic_p_h_lsil$BaseValue[2]
#   ),
#   
#   # Probability: LSIL -> Healthy (data from )
#   p_lsil_regress = ifelse(
#     age < logic_p_lsil_regress$AgeThreshold[1], 
#     logic_p_lsil_regress$BaseValue[1], 
#     logic_p_lsil_regress$BaseValue[2]
#   ),
#   
#   # === 5. Cancer Survival and Competing Risks ===
#   # Convert 5-year survival (from params_scalar_list) to 1-year death probs
#   p_death_ca_local = 1 - p_ca_local_5yr_surv ^ (1/5),
#   p_death_ca_reg   = 1 - p_ca_reg_5yr_surv ^ (1/5),
#   p_death_ca_met   = 1 - p_ca_met_5yr_surv ^ (1/5),
#   
#   # Define total death probability from cancer states using the
#   # competing risk formula: p_total = 1 - ((1 - p_other) * (1 - p_cancer))
#   p_total_death_local = 1 - ((1 - p_death_other) * (1 - p_death_ca_local)),
#   p_total_death_reg   = 1 - ((1 - p_death_other) * (1 - p_death_ca_reg)),
#   p_total_death_met   = 1 - ((1 - p_death_other) * (1 - p_death_ca_met))
# )
# 
# 
# #
# # 6. DEFINE MODEL COMPONENTS
# # ------------------------------------------------------------------------------
# # Define states and the transition matrix.
# 
# # 6.1. Define States Programmatically
# # Uses the list 'state_values_list' from Section 4.3
# model_states_list <- purrr::pmap(
#   state_values_list,
#   .f = function(cost, effect) {
#     define_state(cost = cost, effect = effect)
#   }
# )
# 
# # Manually define the absorbing Death state
# state_death <- define_state(cost = 0, effect = 0)
# 
# # Combine all states into a single list
# all_states <- c(model_states_list, list(Death = state_death))
# 
# # 6.2. Define the Non-Homogeneous Transition Matrix
# # The matrix is filled with parameter names from 'param_obj'.
# # 'C' = Complement (1 - sum of other probabilities in the row) 
# state_names <- c(
#   "Healthy", 
#   "LSIL", 
#   "HSIL", 
#   "Cancer_Local", 
#   "Cancer_Regional", 
#   "Cancer_Metastatic", 
#   "Death"
# )
# 
# matrix_base <- define_transition(
#   state_names = state_names,
#   
#   # Row 1: From Healthy
#   C, p_healthy_to_lsil, 0, 0, 0, 0, p_death_other,
#   
#   # Row 2: From LSIL
#   p_lsil_regress, C, p_lsil_to_hsil, 0, 0, 0, p_death_other,
#   
#   # Row 3: From HSIL
#   p_hsil_regress, 0, C, p_hsil_to_cancer, 0, 0, p_death_other,
#   
#   # Row 4: From Cancer_Local
#   0, 0, 0, C, p_cancer_local_to_reg, 0, p_total_death_local,
#   
#   # Row 5: From Cancer_Regional
#   0, 0, 0, 0, C, p_cancer_reg_to_met, p_total_death_reg,
#   
#   # Row 6: From Cancer_Metastatic
#   0, 0, 0, 0, 0, C, p_total_death_met,
#   
#   # Row 7: From Death (Absorbing State)
#   0, 0, 0, 0, 0, 0, 1
# )
# 
# 
# #
# # 7. DEFINE STRATEGIES
# # ------------------------------------------------------------------------------
# # We assemble the components into the comparator and intervention strategies.
# 
# # 7.1. Strategy 1: Base Case (No Vaccination)
# # We use do.call() to pass the 'all_states' list as named arguments.
# strategy_novax <- do.call(
#   define_strategy,
#   c(
#     list(transition = matrix_base),
#     all_states
#   )
# )
# 
# # 7.2. Strategy 2: Intervention (Vaccination)
# # We use heemod::modify()  to clone the base strategy and
# # redefine only the parameter(s) affected by the intervention.
# strategy_vax <- modify(
#   strategy_novax,
#   
#   # Override 'p_healthy_to_lsil' from param_obj.
#   # The original (age-dependent) probability is multiplied by
#   # the (1 - vaccine_efficacy), both from the Excel file.
#   p_healthy_to_lsil = p_healthy_to_lsil * (1 - vaccine_efficacy)
# )
# 
# 
# #
# # 8. RUN MODEL AND SHOW RESULTS
# # ------------------------------------------------------------------------------
# 
# # 8.1. Run the Model
# model_run <- run_model(
#   # Strategies to compare
#   no_vax = strategy_novax,
#   vax    = strategy_vax,
#   
#   # The single, master parameter object
#   parameters = param_obj,
#   
#   # Starting cohort of 100,000
#   init = c(Healthy = 100000,
#            LSIL = 0,
#            HSIL = 0,
#            Cancer_Local = 0,
#            Cancer_Regional = 0,
#            Cancer_Metastatic = 0,
#            Death = 0),
#   
#   # Run for 89 cycles (from age 12 to 100)
#   cycles = 89,
#   
#   # Aggregate 'cost' and 'effect' values from states
#   cost = cost,
#   effect = effect,
#   
#   # Set discount rates (example: 3% for both, as per common practice)
#   # The Fonseca paper used 5% 
#   discount = list(cost = 0.05, effect = 0.05) 
# )
# 
# # 8.2. Print the Summary
# cat("--- Model Run Summary ---\n")
# print(summary(model_run))
# 
# cat("\n--- Incremental Cost-Effectiveness (ICER) Analysis ---\n")
# print(summary(model_run, "no_vax"))
# 

#####END#####

# # main R script to run the cervical cancer Markov model
# library(heemod)
# rm(list = ls())
# # --- 1. Load Background Mortality Data ---
# # Load the all-cause mortality table for Brazilian females, 1999
# #
# load_mortality_data(
#   file = "BRA-females.csv",
#   name = "p_mort_all_cause",
#   year_col = "Year",
#   age_col = "Age",
#   prob_col = "Prob",
#   year_val = 1999
# )
# 
# 
# # --- 2. Define All Model Parameters ---
# # Parameters are sourced exclusively from Fonseca et al. (2013)
# # Appendix A  and text.
# 
# params <- define_parameters(
# 
#   # --- Cohort Settings ---
#   start_age = 12, # Age at vaccination
# 
#   # --- Discount Rate (CRITICAL) ---
#   discount_rate = 0.05, # Base case 5%
# 
#   # --- Vaccine Parameters (CRITICAL) ---
#   vaccine_cost = 150,     # Base case cost for 3 doses
#   vaccine_efficacy = 0.50,  # Base case 50% efficacy
#   vaccine_coverage = 0.90,  # Base case 90% coverage
# 
#   # --- Screening Parameters (Base Case = 3 Pap tests) ---
#   # Pap test cost and utility, for simplicity, are annualized
#   # This is complex, but Fonseca's Table 1 total cost for
#   # "Only screening" is $155. This implies screening costs
#   # are low. We will model the incremental cost of the vaccine.
#   cost_pap_test = 8,      # Cost per test
#   util_pap_test = 0.01,   # Utility decrement (1.0 - 0.99)
# 
#   # --- Disease Probabilities (from [1, 1]) ---
#   # Healthy to LSIL (Base)
#   p_healthy_to_lsil_base = 0.07,
# 
#   # Healthy to LSIL (Vaccinated Strategy)
#   # (risk * (1-eff) * coverage) + (risk * (1-coverage))
#   p_healthy_to_lsil_vacc = (0.07 * (1 - 0.50) * 0.90) + (0.07 * (1 - 0.90)),
# 
#   # LSIL
#   p_lsil_to_hsil = 0.11,
# 
#   # LSIL to Healthy (NON-HOMOGENEOUS)
#   # This is the age-dependent (model_time-dependent) transition
#   p_lsil_to_healthy = ifelse(
#     model_time < 18, # 18 cycles = age 30 (since start_age=12)
#     0.193,           # < 30 years old
#     0.113            # > 30 years old
#   ),
# 
#   # HSIL
#   p_hsil_to_lsil = 0.175,
#   p_hsil_to_cancer_early = 0.0078,
# 
#   # Cancer Progression
#   p_cancer_early_to_regional = 0.15,
#   p_cancer_regional_to_metastatic = 0.16,
# 
#   # Cancer-Specific Mortality
#   p_death_cancer_early = 0.0165,
#   p_death_cancer_regional = 0.1101,
#   p_death_cancer_metastatic = 0.305,
# 
#   # --- State Costs (CRITICAL: Initial vs. Annual)  ---
#   cost_cancer_early_init = 3702,
#   cost_cancer_regional_init = 8420,
#   cost_cancer_metastatic_init = 2625,
# 
#   cost_cancer_early_annual = 3702 * 0.10,     # 10% of initial
#   cost_cancer_regional_annual = 8420 * 0.10,
#   cost_cancer_metastatic_annual = 2625 * 0.10,
# 
#   # --- State Utilities  ---
#   util_healthy = 1.0,
#   util_lsil = 1.0,
#   util_hsil = 1.0,
#   util_cancer_early = 0.76,
#   util_cancer_regional = 0.67,
#   util_cancer_metastatic = 0.48,
#   util_death = 0
# )
# 
# 
# # --- 3. Define State Transitions ---
# # Uses parameters defined above
# trans_matrix <- define_transition(
#   state_names = c(
#     "Healthy",
#     "LSIL",
#     "HSIL",
#     "Cancer_Early",
#     "Cancer_Regional",
#     "Cancer_Metastatic",
#     "Death"
#   ),
# 
#   # C,... = complement (1 - sum of other probabilities)
#   # p_mort_all_cause is from the loaded mortality table
# 
#   # Healthy
#   C, p_healthy_to_lsil_base, 0, 0, 0, 0, p_mort_all_cause,
#   # LSIL
#   p_lsil_to_healthy, C, p_lsil_to_hsil, 0, 0, 0, p_mort_all_cause,
#   # HSIL
#   0, p_hsil_to_lsil, C, p_hsil_to_cancer_early, 0, 0, p_mort_all_cause,
#   # Cancer_Early
#   0, 0, 0, C, p_cancer_early_to_regional, 0, p_death_cancer_early + p_mort_all_cause,
#   # Cancer_Regional
#   0, 0, 0, 0, C, p_cancer_regional_to_metastatic, p_death_cancer_regional + p_mort_all_cause,
#   # Cancer_Metastatic
#   0, 0, 0, 0, 0, C, p_death_cancer_metastatic + p_mort_all_cause,
#   # Death
#   0, 0, 0, 0, 0, 0, 1
# )
# 
# # --- 4. Define Health States (Costs and Utilities) ---
# 
# state_healthy <- define_state(
#   cost = 0,
#   utility = util_healthy
# )
# state_lsil <- define_state(
#   cost = 0, # Asymptomatic, costs are procedural
#   utility = util_lsil
# )
# state_hsil <- define_state(
#   cost = 0, # Asymptomatic, costs are procedural
#   utility = util_hsil
# )
# state_cancer_early <- define_state(
#   cost_init = cost_cancer_early_init,
#   cost = cost_cancer_early_annual,
#   utility = util_cancer_early
# )
# state_cancer_regional <- define_state(
#   cost_init = cost_cancer_regional_init,
#   cost = cost_cancer_regional_annual,
#   utility = util_cancer_regional
# )
# state_cancer_metastatic <- define_state(
#   cost_init = cost_cancer_metastatic_init,
#   cost = cost_cancer_metastatic_annual,
#   utility = util_cancer_metastatic
# )
# state_death <- define_state(
#   cost = 0,
#   utility = util_death
# )
# 
# # --- 5. Define the Strategies ---
# 
# # Strategy 1: Screening Only (Base Case)
# # This is the base model. Costs for the 3 Pap smears are
# # implicitly part of the $155 total cost.
# strategy_screen_only <- define_strategy(
#   transition = trans_matrix,
#   Healthy = state_healthy,
#   LSIL = state_lsil,
#   HSIL = state_hsil,
#   Cancer_Early = state_cancer_early,
#   Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic,
#   Death = state_death
# )
# 
# # Strategy 2: Vaccination + Screening
# # This strategy modifies the transition matrix and adds a one-time
# # cost for the vaccine.
# 
# strategy_vaccine <- define_strategy(
#   transition = modify_transition(
#     trans_matrix,
#     # Modify the Healthy -> LSIL probability
#     Healthy, LSIL = p_healthy_to_lsil_vacc
#   ),
# 
#   # Add one-time vaccine cost at cycle 0
#   cost_init = cost_vaccine,
# 
#   # All states are otherwise identical
#   Healthy = state_healthy,
#   LSIL = state_lsil,
#   HSIL = state_hsil,
#   Cancer_Early = state_cancer_early,
#   Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic,
#   Death = state_death
# )
# 
# # --- 6. Run the Model and Extract Results ---
# 
# model_run <- run_model(
#   strategy_screen_only = strategy_screen_only,
#   strategy_vaccine = strategy_vaccine,
# 
#   parameters = params,
#   init = c(Healthy = 1000, 0, 0, 0, 0, 0, 0), # 1000 women in Healthy
#   cycles = 70, # 70-year simulation
# 
#   cost = cost,
#   effect = utility,
# 
#   method = "life-table",
# 
#   # Apply 5% discount rate to both
#   discount = list(
#     cost = discount_rate,
#     effect = discount_rate
#   )
# )
# 
# # --- 7. Generate the Summary (THE SOLUTION) ---
# 
# # This command generates the final cost-effectiveness table
# model_summary <- summary(model_run)
# 
# # Print the results to the console
# print(model_summary)
# 
# # main R script to run the cervical cancer Markov model
# library(heemod)
# rm(list = ls())
# 
# # --- 1. Load Background Mortality Data ---
# # Load the all-cause mortality table for Brazilian females, 1999 
# source("get_who_mortality.R") # Ensure the function is sourced
# 
# load_mortality_data(
#   file = "BRA-females.csv",
#   name = "p_mort_all_cause",
#   year_col = "Year",
#   age_col = "Age",
#   prob_col = "Prob",
#   year_val = 1999
# )
# 
# 
# # --- 2. Define All Model Parameters ---
# # Parameters are sourced exclusively from Fonseca et al. (2013)
# # Appendix A  and text.
# 
# params <- define_parameters(
#   
#   # --- Cohort Settings ---
#   start_age = 12, # Age at vaccination 
#   
#   # --- Discount Rate (CRITICAL) ---
#   discount_rate = 0.05, # Base case 5% 
#   
#   # --- Vaccine Parameters (CRITICAL) ---
#   vaccine_cost = 150,     # Base case cost for 3 doses 
#   vaccine_efficacy = 0.50,  # Base case 50% efficacy 
#   vaccine_coverage = 0.90,  # Base case 90% coverage 
#   
#   # --- Screening Ages ---
#   # 3-test scenario (interpretation of "randomly within the second, fourth, and sixth decades" )
#   screening_ages_3 = c(20, 40, 60), 
#   # 10-test scenario (explicit schedule from paper )
#   screening_ages_10 = c(25, 30, 35, 40, 43, 46, 49, 52, 55, 65),
#   
#   # --- Screening Parameters (from Appendix A) ---
#   cost_pap_test = 8,                # Cost per test 
#   util_pap_test_decrement = 0.01, # (1.0 - 0.99) 
#   
#   # --- Disease Probabilities (from ) ---
#   # Healthy to LSIL (Base)
#   p_healthy_to_lsil_base = 0.07,
#   
#   # Healthy to LSIL (Vaccinated Strategy)
#   # (risk * (1-eff) * coverage) + (risk * (1-coverage))
#   p_healthy_to_lsil_vacc = (0.07 * (1 - 0.50) * 0.90) + (0.07 * (1 - 0.90)),
#   
#   # LSIL
#   p_lsil_to_hsil = 0.11,
#   
#   # LSIL to Healthy (NON-HOMOGENEOUS)
#   # This is the age-dependent (model_time-dependent) transition
#   p_lsil_to_healthy = ifelse(
#     model_time < 18, # 18 cycles = age 30 (since start_age=12)
#     0.193,           # < 30 years old 
#     0.113            # > 30 years old 
#   ),
#   
#   # HSIL
#   p_hsil_to_lsil = 0.175,
#   p_hsil_to_cancer_early = 0.0078,
#   
#   # Cancer Progression
#   p_cancer_early_to_regional = 0.15,
#   p_cancer_regional_to_metastatic = 0.16,
#   
#   # Cancer-Specific Mortality (from 5-year survival rates )
#   p_death_cancer_early = 0.0165,
#   p_death_cancer_regional = 0.1101,
#   p_death_cancer_metastatic = 0.305,
#   
#   # --- State Costs (CRITICAL: Initial vs. Annual)  ---
#   cost_cancer_early_init = 3702,
#   cost_cancer_regional_init = 8420,
#   cost_cancer_metastatic_init = 2625,
#   
#   cost_cancer_early_annual = 3702 * 0.10,     # 10% of initial 
#   cost_cancer_regional_annual = 8420 * 0.10,
#   cost_cancer_metastatic_annual = 2625 * 0.10,
#   
#   # --- State Utilities  ---
#   util_healthy = 1.0,
#   util_lsil = 1.0,
#   util_hsil = 1.0,
#   util_cancer_early = 0.76,
#   util_cancer_regional = 0.67,
#   util_cancer_metastatic = 0.48,
#   util_death = 0,
#   
#   # --- Age & Screening Flags ---
#   current_age = start_age + model_time - 1,
#   is_screening_3 = current_age %in% screening_ages_3,
#   is_screening_10 = current_age %in% screening_ages_10
# )
# 
# 
# # --- 3. Define State Transitions ---
# 
# # Base transition matrix (No Vaccination)
# trans_matrix_no_vacc <- define_transition(
#   state_names = c(
#     "Healthy", "LSIL", "HSIL",
#     "Cancer_Early", "Cancer_Regional", "Cancer_Metastatic", "Death"
#   ),
#   
#   # C = complement, p_mort_all_cause is from loaded mortality table 
#   
#   # Healthy
#   C, p_healthy_to_lsil_base, 0, 0, 0, 0, p_mort_all_cause,
#   # LSIL
#   p_lsil_to_healthy, C, p_lsil_to_hsil, 0, 0, 0, p_mort_all_cause,
#   # HSIL
#   0, p_hsil_to_lsil, C, p_hsil_to_cancer_early, 0, 0, p_mort_all_cause,
#   # Cancer_Early
#   0, 0, 0, C, p_cancer_early_to_regional, 0, p_death_cancer_early + p_mort_all_cause,
#   # Cancer_Regional
#   0, 0, 0, 0, C, p_cancer_regional_to_metastatic, p_death_cancer_regional + p_mort_all_cause,
#   # Cancer_Metastatic
#   0, 0, 0, 0, 0, C, p_death_cancer_metastatic + p_mort_all_cause,
#   # Death
#   0, 0, 0, 0, 0, 0, 1
# )
# 
# # Vaccinated transition matrix
# trans_matrix_vacc <- modify_transition(
#   trans_matrix_no_vacc,
#   Healthy, LSIL = p_healthy_to_lsil_vacc
# )
# 
# # --- 4. Define Common Health States (Costs and Utilities) ---
# # These are shared across all strategies
# state_cancer_early <- define_state(
#   cost_init = cost_cancer_early_init,
#   cost = cost_cancer_early_annual,
#   utility = util_cancer_early
# )
# state_cancer_regional <- define_state(
#   cost_init = cost_cancer_regional_init,
#   cost = cost_cancer_regional_annual,
#   utility = util_cancer_regional
# )
# state_cancer_metastatic <- define_state(
#   cost_init = cost_cancer_metastatic_init,
#   cost = cost_cancer_metastatic_annual,
#   utility = util_cancer_metastatic
# )
# state_death <- define_state(
#   cost = 0,
#   utility = util_death
# )
# 
# # --- 5. Define Scenario-Specific States ---
# # States differ based on screening costs and utility decrements
# 
# # -- Scenario 1 (No Screening) States --
# state_healthy_s1 <- define_state(cost = 0, utility = util_healthy)
# state_lsil_s1 <- define_state(cost = 0, utility = util_lsil)
# state_hsil_s1 <- define_state(cost = 0, utility = util_hsil)
# 
# # -- Scenario 2 (3 Screenings) States --
# state_healthy_s2 <- define_state(
#   cost = ifelse(is_screening_3, cost_pap_test, 0),
#   utility = util_healthy - ifelse(is_screening_3, util_pap_test_decrement, 0)
# )
# state_lsil_s2 <- define_state(
#   cost = ifelse(is_screening_3, cost_pap_test, 0),
#   utility = util_lsil - ifelse(is_screening_3, util_pap_test_decrement, 0)
# )
# state_hsil_s2 <- define_state(
#   cost = ifelse(is_screening_3, cost_pap_test, 0),
#   utility = util_hsil - ifelse(is_screening_3, util_pap_test_decrement, 0)
# )
# 
# # -- Scenario 3 (10 Screenings) States --
# state_healthy_s3 <- define_state(
#   cost = ifelse(is_screening_10, cost_pap_test, 0),
#   utility = util_healthy - ifelse(is_screening_10, util_pap_test_decrement, 0)
# )
# state_lsil_s3 <- define_state(
#   cost = ifelse(is_screening_10, cost_pap_test, 0),
#   utility = util_lsil - ifelse(is_screening_10, util_pap_test_decrement, 0)
# )
# state_hsil_s3 <- define_state(
#   cost = ifelse(is_screening_10, cost_pap_test, 0),
#   utility = util_hsil - ifelse(is_screening_10, util_pap_test_decrement, 0)
# )
# 
# 
# # --- 6. Define the 6 Strategies ---
# 
# # -- Scenario 1: Non-screening --
# strategy_nat_course <- define_strategy(
#   transition = trans_matrix_no_vacc,
#   Healthy = state_healthy_s1, LSIL = state_lsil_s1, HSIL = state_hsil_s1,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# strategy_vacc_only <- define_strategy(
#   transition = trans_matrix_vacc,
#   cost_init = vaccine_cost,
#   Healthy = state_healthy_s1, LSIL = state_lsil_s1, HSIL = state_hsil_s1,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# 
# # -- Scenario 2: 3 Screenings (Base Case) --
# strategy_screen_3 <- define_strategy(
#   transition = trans_matrix_no_vacc,
#   Healthy = state_healthy_s2, LSIL = state_lsil_s2, HSIL = state_hsil_s2,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# strategy_vacc_screen_3 <- define_strategy(
#   transition = trans_matrix_vacc,
#   cost_init = vaccine_cost,
#   Healthy = state_healthy_s2, LSIL = state_lsil_s2, HSIL = state_hsil_s2,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# 
# # -- Scenario 3: 10 Screenings --
# strategy_screen_10 <- define_strategy(
#   transition = trans_matrix_no_vacc,
#   Healthy = state_healthy_s3, LSIL = state_lsil_s3, HSIL = state_hsil_s3,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# strategy_vacc_screen_10 <- define_strategy(
#   transition = trans_matrix_vacc,
#   cost_init = vaccine_cost,
#   Healthy = state_healthy_s3, LSIL = state_lsil_s3, HSIL = state_hsil_s3,
#   Cancer_Early = state_cancer_early, Cancer_Regional = state_cancer_regional,
#   Cancer_Metastatic = state_cancer_metastatic, Death = state_death
# )
# 
# 
# # --- 7. Run Models and Print Summaries for All 3 Scenarios ---
# 
# # Common model settings
# model_cycles <- 70 # 70-year simulation 
# model_init <- c(Healthy = 1000, 0, 0, 0, 0, 0, 0)
# model_discount <- list(cost = discount_rate, effect = discount_rate)
# 
# # --- SCENARIO 1 ---
# cat("--- SCENARIO 1: NON-SCREENING ---\n")
# run_s1 <- run_model(
#   Natural_Course = strategy_nat_course,
#   Vaccination_Only = strategy_vacc_only,
#   parameters = params,
#   init = model_init, cycles = model_cycles,
#   cost = cost, effect = utility, method = "life-table",
#   discount = model_discount
# )
# print(summary(run_s1))
# 
# # --- SCENARIO 2 ---
# cat("\n--- SCENARIO 2: THREE SCREENINGS (BASE CASE) ---\n")
# run_s2 <- run_model(
#   Screening_Only_3 = strategy_screen_3,
#   Vaccination_Screening_3 = strategy_vacc_screen_3,
#   parameters = params,
#   init = model_init, cycles = model_cycles,
#   cost = cost, effect = utility, method = "life-table",
#   discount = model_discount
# )
# print(summary(run_s2))
# 
# # --- SCENARIO 3 ---
# cat("\n--- SCENARIO 3: TEN SCREENINGS ---\n")
# run_s3 <- run_model(
#   Screening_Only_10 = strategy_screen_10,
#   Vaccination_Screening_10 = strategy_vacc_screen_10,
#   parameters = params,
#   init = model_init, cycles = model_cycles,
#   cost = cost, effect = utility, method = "life-table",
#   discount = model_discount
# )
# print(summary(run_s3))


