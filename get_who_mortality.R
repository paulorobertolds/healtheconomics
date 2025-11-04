#' # Load required libraries for data manipulation
#' # install.packages(c("dplyr", "stringr", "readr"))
#' library(dplyr)
#' library(stringr)
#' library(readr)
#' 
#' #' Load and Process WHO Mortality Rates from a Local CSV File
#' #'
#' #' This function reads a standardized mortality data CSV file downloaded from the WHO,
#' #' filters it for a specific year and sex, and processes it into a life table format
#' #' suitable for the Markov model.
#' #'
#' #' @param file_path A character string for the path to the CSV file.
#' #' @param target_year A numeric value for the desired year of data to extract.
#' #' @param target_sex A character string for the sex. Accepted values: "Female", "Male", "All".
#' #'
#' #' @return A tidy data frame (tibble) with the following columns:
#' #'   - age_group: The original WHO age group string (e.g., "[1-4]").
#' #'   - mortality_rate: The annual probability of death for that age group.
#' #'   - age_lower: The lower bound of the age group (numeric).
#' #'   - age_upper: The upper bound of the age group (numeric, Inf for open-ended).
#' 
#' get_who_mortality_local <- function(file_path, target_year, target_sex) {
#'   
#'   message(paste("Loading mortality data from local file:", file_path))
#'   
#'   if (!file.exists(file_path)) {
#'     stop(paste("Mortality data file not found at:", file_path,
#'                "\nPlease ensure the CSV file is in the correct directory."),
#'          call. = FALSE)
#'   }
#'   
#'   # 1. Read and Clean the Raw Data
#'   raw_data <- readr::read_csv(file_path, skip = 5, show_col_types = FALSE) %>%
#'     # Filter for the specific year and sex required
#'    filter(Year == target_year, Sex == target_sex)
#'   
#'   if (nrow(raw_data) == 0) {
#'     stop(paste("No data found in", file_path, "for year", target_year, "and sex", target_sex),
#'          call. = FALSE)
#'   }
#'   
#'   # 2. Process and Tidy the Data
#'   mortality_data <- raw_data %>%
#'     # Select and rename the essential columns
#'     select(
#'       age_group_raw = `Age Group`,
#'       death_rate_per_100k = `Death rate per 100 000 population`
#'     ) %>%
#'     # Convert the rate per 100,000 to an annual probability
#'     mutate(mortality_rate = death_rate_per_100k / 100000) %>%
#'     # Create clean age group labels
#'     mutate(age_group = stringr::str_replace_all(age_group_raw, "\\[|\\]", "")) %>%
#'     # Create numeric age bounds from the age group strings
#'     mutate(
#'       age_numbers = stringr::str_extract_all(age_group, "[0-9]+"),
#'       age_lower = as.numeric(sapply(age_numbers, function(x) x[1])),
#'       age_upper = sapply(age_numbers, function(x) {
#'         if (length(x) > 1) {
#'           # Case like "1-4"
#'           as.numeric(x[2])
#'         } else if (grepl("\\+", x[1])) {
#'           # Case like "85+"
#'           Inf
#'         } else {
#'           # Case like "0"
#'           as.numeric(x[1])
#'         }
#'       })
#'     ) %>%
#'     # Final selection and arrangement of columns
#'     select(age_group, mortality_rate, age_lower, age_upper) %>%
#'     arrange(age_lower)
#'   
#'   message("Successfully loaded and processed local mortality data.")
#'   return(mortality_data)
#' }

# Load required libraries for data manipulation
# install.packages(c("dplyr", "stringr", "readr"))
library(dplyr)
library(stringr)
library(readr)

#' Load and Process WHO Mortality Rates from a Local CSV File
#'
#' This function reads a standardized mortality data CSV file downloaded from the WHO,
#' filters it for a specific year and sex, and processes it into a life table format
#' suitable for the Markov model.
#'
#' @param file_path A character string for the path to the CSV file.
#' @param target_year A numeric value for the desired year of data to extract.
#' @param target_sex A character string for the sex. Accepted values: "Female", "Male", "All".
#'
#' @return A tidy data frame (tibble) with the following columns:
#'   - age_group: The original WHO age group string (e.g., "[1-4]").
#'   - mortality_rate: The annual probability of death for that age group.
#'   - age_lower: The lower bound of the age group (numeric).
#'   - age_upper: The upper bound of the age group (numeric, Inf for open-ended).

get_who_mortality_local <- function(file_path, target_year, target_sex) {
  
  message(paste("Loading mortality data from local file:", file_path))
  
  if (!file.exists(file_path)) {
    stop(paste("Mortality data file not found at:", file_path,
               "\nPlease ensure the CSV file is in the correct directory."),
         call. = FALSE)
  }
  
  # 1. Read and Clean the Raw Data
  raw_data <- readr::read_csv(file_path, skip = 5, show_col_types = FALSE) %>%
    # Filter for the specific year and sex required
    filter(Year == target_year, Sex == target_sex)
  
  if (nrow(raw_data) == 0) {
    stop(paste("No data found in", file_path, "for year", target_year, "and sex", target_sex),
         call. = FALSE)
  }
  
  # 2. Process and Tidy the Data
  mortality_data <- raw_data %>%
    # Select and rename the essential columns
    select(
      age_group_raw = `Age Group`,
      death_rate_per_100k = `Death rate per 100 000 population`
    ) %>%
    # Convert the rate per 100,000 to an annual probability
    mutate(mortality_rate = death_rate_per_100k / 100000) %>%
    # Create clean age group labels
    mutate(age_group = stringr::str_replace_all(age_group_raw, "\\[|\\]", "")) %>%
    # Create numeric age bounds from the age group strings
    mutate(
      # FIXED: Handle the "+" notation for 85+ age group
      age_lower = case_when(
        grepl("\\+", age_group) ~ as.numeric(str_extract(age_group, "\\d+")),
        grepl("_", age_group_raw) ~ as.numeric(str_extract(age_group_raw, "\\d+(?=_)")),
        grepl("-", age_group) ~ as.numeric(str_extract(age_group, "\\d+(?=-)")),
        TRUE ~ as.numeric(str_extract(age_group, "\\d+"))
      ),
      age_upper = case_when(
        grepl("\\+", age_group) ~ Inf,  # 85+ becomes 85 to Inf
        grepl("_", age_group_raw) ~ as.numeric(str_extract(age_group_raw, "(?<=_)\\d+")),
        grepl("-", age_group) ~ as.numeric(str_extract(age_group, "(?<=-)\\d+")),
        TRUE ~ as.numeric(str_extract(age_group, "\\d+"))
      )
    ) %>%
    # Final selection and arrangement of columns
    select(age_group, mortality_rate, age_lower, age_upper) %>%
    arrange(age_lower)
  
  message("Successfully loaded and processed local mortality data.")
  
  # Print summary to verify 85+ age group is handled correctly
  eighty_five_plus <- mortality_data %>% filter(age_upper == Inf)
  if (nrow(eighty_five_plus) > 0) {
    message(paste("85+ age group found: lower =", eighty_five_plus$age_lower, 
                  "upper = Inf, mortality_rate =", round(eighty_five_plus$mortality_rate, 6)))
  }
  
  return(mortality_data)
}

# Test the function with your data
#test_result <- get_who_mortality_local("BRA-females.csv", 2021, "Female")
#print(test_result)