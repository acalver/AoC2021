library(tidyverse)

data <- read_csv('data/day2.csv', col_names = F, show_col_types = F)


data %>% 
  separate(X1, c('direction', 'value')) %>% 
  mutate(n = row_number(),value=as.integer(value)) %>% 
  pivot_wider(names_from = 'direction', values_from = 'value', values_fill =0) %>% 
  mutate(forward_c = cumsum(forward),
         down_c = cumsum(down),
         up_c = -cumsum(up),
         result = forward_c * (down_c + up_c)) %>% 
  tail(1)

###### Part 2 ######
data %>% 
  separate(X1, c('direction', 'value')) %>% 
  mutate(n = row_number(),value=as.integer(value)) %>% 
  pivot_wider(names_from = 'direction', values_from = 'value', values_fill =0) %>% 
  mutate(forward_c = cumsum(forward),
         down_c = cumsum(down),
         up_c = -cumsum(up),
         
         aim = down_c + up_c,
         depth = forward * aim,
         depth = cumsum(depth),
         
         result = forward_c * depth) %>% 
  tail(1)




tibble(direction =c('forward', 'down', 'forward', 'up', 'down', 'forward'),
       value =c(5,5,8,3,8,2)
) %>% 
  mutate(n = row_number(),value=as.integer(value)) %>% 
  pivot_wider(names_from = 'direction', values_from = 'value', values_fill =0) %>% 
  mutate(forward_c = cumsum(forward),
         down_c = cumsum(down),
         up_c = -cumsum(up),
         
         aim = down_c + up_c,
         depth = forward * aim,
         depth = cumsum(depth),
         
         result = forward_c * depth) %>% 
  tail(1)
