
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
