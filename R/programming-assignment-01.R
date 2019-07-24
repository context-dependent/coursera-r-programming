
# BASE R ------------------------------------------------------------------

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
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
  
  # Make a container for the data you'll read in by for loop
  list_data <- list()
  
  # Iterate over a sequence that represents the paths by index
  for(i in seq_along(paths)) {
    
    # Read in the ith id's data and assign it to the ith list item
    list_data[[i]] <- read.csv(paths[i])
    
  }
  
  # Bind together the list of data.frames the old fashioned way
  table_data <- do.call(rbind, list_data)
  
  # Assign the desired output to res
  res <- mean(table_data[[pollutant]], na.rm = TRUE)
  
  # Return res
  res
  
}

complete <- function(directory, id = 1:332) {
  
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
  
  # Make a container for the data you'll read in by for loop
  list_data <- list()
  
  # Iterate over a sequence that represents the paths by index
  for(i in seq_along(paths)) {
    
    # Read in the ith id's data and assign it to the variable x
    x <- read.csv(paths[i])
    
    # Make a tiny data frame for each id with the id and the number of complete cases
    list_data[[i]] <- data.frame(id = id[i], nobs = sum(complete.cases(x)))
    
  }
  
  # Bind together the list of data.frames the old fashioned way
  res <- do.call(rbind, list_data)
  
  # Return res
  res
  
}

corr <- function(directory, threshold = 0) {
  
  # Create a vector of leading zeroes to concatenate with the numbers
  leading_zeroes <- strrep("0", 3 - nchar(id))
  
  # Assemble file names by pasting together leading zeroes, ids, and .csv
  file_names <- paste(leading_zeroes, id, ".csv", sep = "")
  
  # Assemble file paths by pasting prepending the directory name
  paths <- paste(directory, "/", file_names, sep = "")
  
  # Make a container for the data you'll read in by for loop
  list_data <- list()
  
  # Iterate over a sequence that represents the paths by index
  for(i in seq_along(paths)) {
    
    # Read in the ith id's data and assign it to the variable x
    x <- read.csv(paths[i])
    
    # filter for only the complete cases
    x <- x[complete.cases(x), ]
    
    # If the number of complete rows in the file is greater than or 
    # equal to the threshold value,
    if(nrow(x) >= threshold) {
      # Make a tiny data frame for each id with the id and the correlation
      list_data[[i]] <- data.frame(id = x$ID[1], correlation = with(x, cor(nitrate, sulfate)))
    }
    
  }
  
  # Bind together the list of data.frames the old fashioned way
  res <- do.call(rbind, list_data)
  
  # Return res
  res
  
}


# DRY AND TIDY ------------------------------------------------------------

library(tidyverse)


read_pollutant_data <- function(directory, id = 1:332) {
  
  # Throw an error if any of the ids fall outside the eligible range
  if(any(id > 332 | id < 1)) {
    stop("Please specify ids between 1 and 332 inclusive")
  }
  
  # Assemble file names by pasting together leading zeroes, ids, and .csv
  file_names <- str_pad(id, 3, pad = "0", "left") %>% str_c(".csv")
  
  # Assemble file paths by pasting prepending the directory name
  paths <- paste(directory, "/", file_names, sep = "")
  
  # Use map to crisply and declaratively iterate over a collection of objects
  res <- paths %>% 
    map(read_csv, col_types = cols(Date = col_date(), sulfate = col_double(), nitrate = col_double(), ID = col_integer())) %>% 
    bind_rows() %>% 
    rename(id = ID)
  
  res
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  data <- read_pollutant_data(directory, id)
  
  res <- mean(data[[pollutant]], na.rm = TRUE)

  res  
}

complete <- function(directory, id = 1:332) {
  
  data <- read_pollutant_data(directory, id)
  
  res <- data %>% 
    
    # Drop incomplete rows
    drop_na() %>% 
    
    # Group by id column
    group_by(id) %>% 
    
    # Count the number of observations using n()
    summarize(nobs = n())
  
  res
  
}

corr <- function(directory, threshold) {
  
  data <- read_pollutant_data(directory)
  
  res <- data %>% 
    
    # Drop incomplete rows
    drop_na() %>% 
    
    # Group by id column
    group_by(id) %>%
    
    # Keep groups where there at least the threshold number 
    # of complete rows 
    filter(n() >= threshold) %>% 
    
    # Create a summary table with id, correlation
    summarize(correlation = cor(sulfate, nitrate))
  
  res
}

