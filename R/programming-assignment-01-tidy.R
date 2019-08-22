library(tidyverse)


# Avoid repeating the code used to read in the data by creating a load_specdata function
load_specdata <- function(directory = "data/specdata", id = 1:332) {
  
  # Throw an error if any of the ids fall outside the eligible range
  if(any(id > 332 | id < 1)) {
    stop("Please specify ids between 1 and 332 inclusive")
  }
  
  
  # Create a vector of leading zeroes to concatenate with the numbers
  leading_zeroes <- strrep("0", 3 - nchar(id))
  
  # Assemble file names by pasting together leading zeroes, ids, and .csv
  file_names <- paste(leading_zeroes, id, ".csv", sep = "")
  
  # Assemble file paths by pasting prepending the directory name
  paths <- paste(directory, "/", file_names, sep = "")
  
  # map read_csv over paths
  res <- paths %>% 
    map( ~ data.table::fread(.x)) %>% 
    
    # bind the resulting list of data frames together
    bind_rows() %>% 
    
    # convert all column names to lower snakecase
    janitor::clean_names() %>% 
    
    as_tibble()
  
  res
}

pollutantmean <- function(directory = "data/specdata", pollutant, id = 1:332) {
  
  # Load the specdata for the selected ids
  specdata <- load_specdata(directory = directory, id = id) 
  
  # calculate the mean for the pollutant
  res <- mean(specdata[[pollutant]])
  
  res
  
}

complete <- function(directory = "data/specdata", id = 1:332) {
  
  # Load the specdata for the selected ids
  specdata <- load_specdata(directory = directory, id = id)
  
  # drop incomplete rows from spedata
  res <- specdata %>% 
    drop_na() %>% 
    
    # group by monitor id
    group_by(id) %>% 
  
    # count the number of rows by id
    summarize(nobs = n())
  
  res
  
}

corr <- function(directory = "data/specdata", threshold = 0) {
  
  # Load specdata for all ids
  specdata <- load_specdata(directory = directory)
  
  res <- specdata %>% 
    
    drop_na() %>% 
    
    group_by(id) %>% 
    
    # filter for ids that have at least threshold complete cases
    filter(n() >= threshold) %>% 
    
    # calculate the correlation by id
    summarize(ns_correlation = cor(nitrate, sulfate))
  
  
  res
  
}
