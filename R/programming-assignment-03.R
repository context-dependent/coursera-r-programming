library(tidyverse)

outcome_of_care_raw <- read_csv("data/hospital-data/outcome-of-care-measures.csv")

outcome_rank_table <- 
  
  outcome_of_care_raw %>% 
    
    # Select the id, name, and state of the hospital, along with the 30 day mortality columns
    select(
      `Provider Number`, 
      `Hospital Name`,
      State,
      matches("^Hospital 30-Day Death")
    ) %>% 
  
    # Reshape the data so that each row represents a hospital / outcome pair
    gather(var, val, matches("Hospital 30-Day")) %>% 
    
    # Extract the outcome from the variable names we gathered
    mutate(
      outcome = tolower(str_extract(var, "(?<=from ).+")),
      val = as.numeric(val)
    ) %>% 
  
    # Drop missing values (we don't need them)
    filter(!is.na(val)) %>% 
    select(-var) %>% 
    
    # Convert names to lower snake case
    janitor::clean_names() %>% 
    
    # Rank each mortality rate by state and outcome
    group_by(state, outcome) %>%
    arrange(
      outcome, 
      val,
      hospital_name
    ) %>% 
    mutate(rank = row_number()) %>% 
    ungroup()

check_args <- function(state = state, outcome = outcome, data = data) {
  if(!(state %in% data$state)) {
    stop(glue::glue("\"{state}\" not a real state!"))
  } else if(!(outcome %in% data$outcome)) {
    stop(glue::glue("\"{outcome}\" is not in the data!"))
  }
}

write_rds(outcome_rank_table, "data/tidy/week-04_hospital-rankings.rds")

best <- function(state, outcome, data_path = "data/tidy/week-04_hospital-rankings.rds") {
  
  # read tidy hospital rankings data
  data <- read_rds(data_path)
  
  # check arguments
  check_args(state = state, outcome = outcome, data = data)
  
  # reassign parameters to avoid namespace conflicts in filter
  .state <- state
  .outcome <- outcome
  
  res <- data %>% 
    
    # Keep the hospital in the specified state 
    # with the best ranking for the specified outcome
    filter(
      state == .state, 
      outcome == .outcome,
      rank == 1
    ) %>% 
   
    # extract hospital name
    pull(hospital_name)
  
  res
}

rankhospital <- function(state, outcome, rank, data_path = "data/tidy/week-04_hospital-rankings.rds") {
  
  # read tidy hospital rankings data
  data <- read_rds(data_path)
  
  # check arguments
  check_args(state = state, outcome = outcome, data = data)
  
  # reassign parameters to avoid namespace conflicts in filter
  .state <- state
  .outcome <- outcome
  .rank <- rank
  
  res <- data %>% 
    
    # Keep the hospital name in the specified state, 
    # with the specified rank
    # for the specified outcome
    filter(
      state == .state, 
      outcome == .outcome,
      rank == switch(
        .rank, 
        
        # if rank is "best", take the first ranked hospital
        "best" = 1, 
        
        # if it's "worst", take the last
        "worst" = last(rank), 
        
        # if neither of the above are true, take the numeric value of rank
        .rank
      )
    ) %>% 
    
    pull(hospital_name)
  
  res
  
}

rankall <- function(outcome, rank, data_path = "data/tidy/week-04_hospital-rankings.rds") {
  
  # read tidy hospital rankings data
  data <- read_rds(data_path)
  
  # check arguments
  check_args(state = state, outcome = outcome, data = data)
  
  # reassign parameters to avoid namespace conflicts in filter
  .state <- state
  .outcome <- outcome
  .rank <- rank
  
  res <- data %>% 
    
    # Keep the hospital name in the specified state, 
    # with the specified rank
    # for the specified outcome
    filter(
      state == .state, 
      outcome == .outcome,
      rank == switch(
        .rank, 
        
        # if rank is "best", take the first ranked hospital
        "best" = 1, 
        
        # if it's "worst", take the last
        "worst" = last(rank), 
        
        # if neither of the above are true, take the numeric value of rank
        .rank
      )
    ) %>% 
    
    select(
      hospital_name, state
    )
  
  res
  
}
