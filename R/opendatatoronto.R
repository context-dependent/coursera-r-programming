remotes::install_github("sharlagelfand/opendatatoronto")


library(opendatatoronto)
library(tidyverse)


apartment_data <- 
  
  
  opendatatoronto::list_package_resources("https://open.toronto.ca/dataset/apartment-building-evaluation/") %>% 
  
  opendatatoronto::get_resource() %>% 
  
  janitor::clean_names() %>% 
  
  mutate_all(
    list(~na_if(., "N/A"))
  ) %>% 
  
  mutate_if(
    ~ all(str_detect(na.omit(.), "^\\d+$")),
    list(~ as.numeric(.))
  )


apartment_data
