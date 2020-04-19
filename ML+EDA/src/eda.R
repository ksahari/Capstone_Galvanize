library(tidyverse)
library(janitor)
library(tidylog)
library(tidymodels)
library(DataExplorer)
library(plotly)
d <- 
  read_csv("data/paysim.csv") %>%
  clean_names()


#remove suspicous results

dd <- 
  d %>% 
  mutate(orig_diff = oldbalance_org- newbalance_orig,
         dest_diff = newbalance_dest - oldbalance_dest,
         is_equal_orig = abs(round(amount,2)) == abs(round(orig_diff,2)),
         is_equal_dest =  abs(round(amount,2)) == abs(round(dest_diff,2))
  )%>% 
  select(amount, orig_diff, dest_diff, is_equal_orig, is_equal_dest, everything()) %>% 
  filter(is_equal_orig == TRUE,
         is_equal_dest == TRUE,
         amount <= oldbalance_org) %>% 
  select(-orig_diff, -dest_diff, -is_equal_orig, -is_equal_dest, -is_flagged_fraud) %>% 
  mutate(days = step/24, 
         day = as.integer(days)+1,
         hour = as.integer(days%%1 * 24))


dd <- write_csv(dd, "data/p_paysim.csv")



# eda ---------------------------------------------------------------------


dd %>% create_report()





 



##


#fraud count per hour
dd %>% 
  filter(is_fraud==1) %>% 
  select(-starts_with("name"), -starts_with("is")) %>%
  count(hour) %>% 
  mutate(hour = as.integer(hour)) %>%
  ggplot(aes(x = hour, y = n))+
  geom_col()



# fraud count per day

dd %>% 
  filter(is_fraud==1) %>% 
  select(-starts_with("name"), -starts_with("is")) %>%
  count(day) %>% 
  #mutate(hour = as.integer(hour)) %>%
  ggplot(aes(x = day, y = n))+
  geom_line()




## fraud table

dd %>% 
  filter(is_fraud == 1) %>% 
  select(step, amount, type, starts_with("new"), starts_with("old"))



##

#show fraud by type
dd %>% 
  count(type, is_fraud)

dd %>% 
  count(type)


# fraud by account balance ------------------------------------------------


## fraud by accounts balance per day
dd %>% 
  filter(is_fraud==1) %>% 
  select(-starts_with("name"), -starts_with("is")) %>%
  select(day, amount, starts_with("old"), starts_with("new")) %>% 
  group_by(day) %>% 
  nest(.) %>% 
  mutate(col_sum = map(data, function(x){
    
    map_df(x, sum)
  })) %>% 
  unnest(col_sum) %>% 
  select(-data) %>% 
  pivot_longer(-day) %>% 
  mutate(value = sqrt(value)) %>% 
  ggplot(aes(x = day, y = value, col = name)) +
  geom_line()

  

## fraud by accounts balance per hour


dd %>% 
  filter(is_fraud==1) %>% 
  select(-starts_with("name"), -starts_with("is")) %>%
  select(hour, amount, starts_with("old"), starts_with("new")) %>% 
  group_by(hour) %>% 
  nest(.) %>% 
  mutate(col_sum = map(data, function(x){
    
    map_df(x, sum)
  })) %>% 
  unnest(col_sum) %>% 
  select(-data) %>% 
  pivot_longer(-hour) %>% 
  mutate(value = sqrt(value)) %>% 
  ggplot(aes(x = hour, y = value, col = name)) +
  geom_line()




# corr --------------------------------------------------------------------

d_cor <-
  
  
  dd %>%
  group_by(type) %>% 
  nest() %>% 
  mutate(type_cor = map2(data, type, function(x,y){
    x %>% 
      select_if(is.numeric) %>% 
      select(-days,-day,-hour, -is_fraud) %>% 
      cor() %>% 
      corrplot(title = y )
  }))
  filter(type != "CASH_IN") %>% 
  select_if(is.numeric) %>% 
  select(-step, -day,-hour, -is_fraud, -days) %>%
  cor() %>% 
  corrplot()
