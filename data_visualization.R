library(tidyverse)
library(ggplot2)
library(wesanderson)

# analysis of all data combined together

colores <- wes_palette(name = 'IsleofDogs1', type = 'continuous')
final_date <- max(life_data[!is.na(life_data$temp), ]$date) # after this date data not completed

## what is in data

skim(life_data)

# how many values are missing
life_data %>%
  filter(date <= final_date) %>%
  skim() %>%
  filter(stat == 'missing') %>%
  select(variable, value, type) %>%
  mutate(value = factor(value)) %>%
  ggplot(aes(x = variable, y = value, col = type)) +
    geom_point() +
    theme_minimal() +
    coord_flip() +
    scale_color_manual(values = colores) +
    labs(y = 'missing', x = 'variable', title = 'How many variabes are missing?')
    
life_data %>%
  filter(date <= final_date) %>%
  skim() %>%
  filter(stat == 'missing', value != 0) %>%
  select(variable, value, type) %>%
  ggplot(aes(x = value, fill = type)) +
    geom_histogram() + 
    xlim(0, NA) +
    scale_fill_manual(values = colores) +
    labs(x = 'missing', t = 'count', title = 'Data missing together')
  
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

mean_pomodoros <- round(mean(life_data$pomodoros, na.rm = TRUE), 2)
max_pomodors <- max(life_data$pomodoros, na.rm = TRUE)
life_data %>%
  filter(pomodoros > 0) %>%
  ggplot(aes(x = pomodoros, fill = is_workday)) +
    geom_histogram(bins = max_pomodors) +
    scale_fill_manual(values = colores)


# pomodoros vs baro
ggplot(life_data, aes(x = pomodoros, y = baro)) +
  geom_point() + 
  scale_color_manual(values = colores)

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = baro)) +
    geom_histogram()

# pomodoros vs temp
ggplot(life_data, aes(x = pomodoros, y = temp)) + 
  geom_point()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = temp)) +
    geom_histogram()

# pomodoros vs templow
ggplot(life_data, aes(x = pomodoros, y = templow)) + 
  geom_point()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = templow)) + 
    geom_histogram()

# pomodoros vs wind
ggplot(life_data, aes(x = pomodoros, y = wind)) + 
  geom_point()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  # summarize(cnt = n())
  ggplot(aes(x = wind)) +
    geom_histogram()

##
# busy_day - over 8 pomodoros
life_data %>%
  mutate(busy_day = pomodoros > 8) %>%
  select(date, pomodoros, busy_day) %>%
  ggplot(aes(pomodoros, fill = busy_day)) +
    geom_histogram(bins = 15) + 
    xlim(0, 16) + 
    scale_fill_manual(values = colores)

life_data %>%
  filter(pomodoros > 8) %>%
  ggplot(aes(x = pomodoros, y = baro)) + 
    geom_point() 

life_data %>%
  filter(pomodoros > 8) %>%
  group_by(month(date, label = TRUE)) %>%
  summarize(cnt = n())

life_data %>%
  filter(pomodoros > 8) %>%
  group_by(week(date)) %>%
  summarize(cnt = n())

life_data %>%
  ggplot(aes(pomodoros)) + 
    geom_histogram() + 
    facet_wrap(~month(date, label = TRUE))

life_data %>%
  filter(!is.na(pomodoros)) %>%
  group_by(month(date, label = TRUE)) %>%
  summarize(cnt = n())
  
