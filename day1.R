library(tidyverse)

data <- read_csv('data/day1.csv', col_names = F, show_col_types = F)


sonar <- mutate(data, next_value = lead(X1),
                direction = if_else(next_value > X1,
                                    'increase',
                                    'decreased')) %>% 
  group_by(direction) %>% 
  summarise(n())


#### Part 2 ####

sliding_window <- mutate(data, next_value_1 = lead(X1),
                next_value_2 = lead(next_value_1),
                window_sum =  X1 + next_value_1 + next_value_2,
                window_next = lead(window_sum),
                direction = if_else(window_next > window_sum,
                                    'increase',
                                    'decreased')) %>% 
  group_by(direction) %>% 
  summarise(n())
