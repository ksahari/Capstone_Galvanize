library(tidyverse)
library(tidymodels)



# data --------------------------------------------------------------------

d <- 
  read_csv("data/p_paysim.csv") %>% 
  select(-days, -name_orig, -name_dest) %>% 
  mutate(day = as.character(day),
         hour = as.character(hour))


# splits ------------------------------------------------------------------


splits <- initial_split(d, strata = "is_fraud")

d_training <- training(splits)
d_testing <- testing(splits)


# prepare data ------------------------------------------------------------


rec <- 
  d_training  %>% 
  recipe(is_fraud ~.) %>% 
  step_dummy(type, day, hour) %>% 
  step_YeoJohnson(all_numeric(),-all_outcomes(), -step) %>% 
  prep()

d_train <- 
  rec %>% 
  juice()

d_test <-
  rec %>% 
  bake(d_testing)



# logstic regression ------------------------------------------------------


# svm ---------------------------------------------------------------------



# xgboost -----------------------------------------------------------------


# random forest -----------------------------------------------------------



# nn ----------------------------------------------------------------------

  
