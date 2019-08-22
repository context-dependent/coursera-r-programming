library(tidyverse)



read_measurements <- function(directory, relevant_features, activity_labels, n_max = Inf) {
  
  files <- list.files(directory, pattern = "\\.txt", full.names = TRUE)
  
  x <- 
    
    read_delim(
      files[str_detect(files, "X_")], 
      delim = " ", 
      col_names = FALSE, 
      n_max = n_max, 
      trim_ws = TRUE
    ) %>% 
    
    select(
      !!relevant_features$feature_id
    ) %>% 
    
    set_names(
      relevant_features$feature_name
    )
  
  y <- 
    
    read_csv(
      files[str_detect(files, "y_")], 
      n_max = n_max, 
      col_names = "activity_id"
    )
  
  subject <- 
    
    read_csv(
      files[str_detect(files, "subject_")], 
      n_max = n_max, 
      col_names = "subject_id"
    )
  
  res <-
    
    y %>% 
    
      bind_cols(
        subject, 
        x
      ) %>% 
      left_join(
        activity_labels
      ) %>% 
      mutate(
        test_id = row_number()
      ) %>% 
      select(
        subject_id, 
        activity_id, 
        activity_name,
        test_id,
        everything()
      )
  
  res
  
}


relevant_features <- 
  
  read_delim(
    "data/raw/UCI HAR Dataset/features.txt", 
    delim = " ",
    col_names = c("feature_id", "feature_name")
  ) %>% 
  
  filter(
    str_detect(feature_name, "mean\\(\\)|std\\(\\)")
  )

activity_labels <- read_delim(
  "data/raw/UCI HAR Dataset/activity_labels.txt", 
  delim = " ", 
  col_names = c("activity_id", "activity_name")
)


dat <- 
  
  map(
    c("data/raw/UCI HAR Dataset/train", "data/raw/UCI HAR Dataset/test"), 
    read_measurements, 
    relevant_features, 
    activity_labels, 
    n_max = 500
  ) %>% 
  
  bind_rows()


dat_long <- 
  
  dat %>%
  
    gather(
      var_stat, val, 
      -subject_id, -activity_id, -activity_name, -test_id
    ) %>% 
    
    mutate(
      stat = str_extract(var_stat, "mean|std"), 
      var  = str_remove(var_stat, "-mean\\(\\)|-std\\(\\)")
    ) %>% 
  
    select(
      -var_stat
    ) %>% 
  
    spread(
      stat, val
    )

dat_summary <- 
  
  dat_long %>%
    group_by(subject_id, activity_id, activity_name, var) %>%
    summarize(
      avg_mean = mean(mean), 
      avg_std = mean(std)
    )




