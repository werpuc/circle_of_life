library(tidyverse)
library(ggplot2)
library(wesanderson)

colores <- wes_palette(name = 'IsleofDogs1', type = 'continuous')

# what is in data

skim(life_data)

life_data %>%
  skim() %>%
  filter(stat == "mean" | stat == 'missing') %>%
  arrange(variable)

# how many values is missing

life_data %>%
  filter(date <= (today() - days(2))) %>%
  skim() %>%
  filter(stat == 'missing') %>%
  select(variable, value, type) %>%
  arrange(value) %>%
  ggplot(aes(x = variable, y = value, col = type)) +
    geom_point() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_manual(values = colores)
    
    
  
## how number of pomodoros changes over time
  
life_data %>%
  filter(!is.na(pomodoros) & is_workday) %>%
  ggplot(aes(x = date, y = pomodoros, color = is_workday)) + 
    geom_point(aes(alpha = pomodoros)) + 
    geom_smooth(aes(alpha = 0.1)) +
    theme_minimal() +
    theme(legend.position="none") +
    scale_color_manual(values = colores) +
    labs(x = 'month', y = 'no. pomodores', title = 'Pomodores change over time')

## and now, the weather
# temp, templow in time
# baro in time
# is_x in month
# hum in time
# hum vs is_rainy
# wind in time
