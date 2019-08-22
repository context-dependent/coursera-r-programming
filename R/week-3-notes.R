library(datasets)
library(tidyverse)
data(mtcars)
data(iris)

mtcars
iris

iris %>% 
  filter(Species == "virginica") %>% 
  summarize(mean = mean(Sepal.Length))

# Question 1, if you want to get specific
iris %>% 
  group_by(Species) %>% 
  summarize(
    mean_sepal_length = mean(Sepal.Length), 
    mean_sepal_width = mean(Sepal.Width)
  )

iris %>% 
  
  group_by(Species) %>% 
  summarize_all(mean)

iris %>% split(iris$Species) %>% map(select, -Species) %>% map(colMeans)

# Question 3, 4, 5

tab_by_cyl <- mtcars %>% 
  group_by(cyl, gear) %>% 
  summarize(
    mean_mpg = mean(mpg), 
    mean_hp = mean(hp)
  )

hp_range <- abs(diff(range(tab_by_cyl$mean_hp)))