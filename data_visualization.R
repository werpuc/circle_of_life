library(tidyverse)
library(ggplot2)
library(wesanderson)

colores <- wes_palette(name = 'IsleofDogs1', type = 'continuous')
final_date <- max(life_data[!is.na(life_data$temp), ]$date)

# what is in data

skim(life_data)

life_data %>%
  skim() %>%
  filter(stat == "mean" | stat == 'missing') %>%
  arrange(variable)

# how many values is missing

life_data %>%
  filter(date <= final_date) %>%
  skim() %>%
  filter(stat == 'missing') %>%
  select(variable, value, type) %>%
  # arrange(value) %>% # jak to wymusic? sprawdzic w notatkach
  ggplot(aes(x = variable, y = value, col = type)) +
    geom_point() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_manual(values = colores)
    
life_data %>%
  filter(date <= final_date) %>%
  skim() %>%
  filter(stat == 'missing', value != 0) %>%
  select(variable, value, type) %>%
  # arrange(value) %>% # jak to wymusic? sprawdzic w notatkach
  ggplot(aes(x = value, col = type)) +
    geom_histogram() + 
    xlim(0, NA)
  
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

max_pomodors <- max(life_data$pomodoros, na.rm = TRUE)

life_data %>%
  filter(pomodoros > 0) %>%
  ggplot(aes(x = pomodoros, col = is_workday)) +
    geom_histogram(bins = max_pomodors)

mean_pomodoros <- round(mean(life_data$pomodoros, na.rm = TRUE), 2)

# pomodoros vs baro
ggplot(life_data, aes(x = pomodoros, y = baro)) +
  geom_jitter()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = baro)) +
    geom_histogram()


# pomodoros vs temp
ggplot(life_data, aes(x = pomodoros, y = temp)) + 
  geom_jitter()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = temp)) +
    geom_histogram()

# pomodoros vs templow
ggplot(life_data, aes(x = pomodoros, y = templow)) + 
  geom_jitter()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  ggplot(aes(x = templow)) + 
    geom_histogram()

# pomodoros vs wind
ggplot(life_data, aes(x = pomodoros, y = wind)) + 
  geom_jitter()

life_data %>%
  filter(!is.na(pomodoros)) %>%
  # summarize(cnt = n())
  ggplot(aes(x = wind)) +
  geom_histogram()

##
# okreœlmy : powyzej 8 pomodoro to pracowity dzieñ
life_data %>%
  filter(pomodoros > 8) %>%
  select(date, pomodoros) %>%
  ggplot(aes(pomodoros)) +
    geom_histogram(bins = 15)

life_data %>%
  filter(pomodoros > 8) %>%
  ggplot(aes(x = pomodoros, y = baro)) + 
    geom_jitter() 

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
  
## and now, the weather
# temp, templow in time


ggplot(weather_data, aes(x = date, y = temp)) +
  geom_line()

# œrednia temperatura dzienna

weather_data %>%
  filter(ts == parse_time('6:00') | ts == parse_time('12:00')) %>%
  group_by(date) %>%
  mutate(mean_temp = mean(temp),
         mean_templow = mean(templow),
         difference = abs(mean_temp - mean_templow)) %>%
  select(date, mean_temp, mean_templow, difference) %>%
  ggplot(aes(x = date)) +
    geom_line(aes(y = mean_temp), col = colores[1]) + 
    geom_line(aes(y = mean_templow), col = colores[2]) + 
    labs(x = 'month', y = 'mean temperature', title = 'Average daily temperature over time') 

    

# baro in time
weather_data %>%
  ggplot(aes(x = date, y = baro)) + 
  geom_line()

#œrednia temp dzienna
weather_data %>%
  filter(ts == parse_time('6:00') | ts == parse_time('12:00')) %>%
  group_by(date) %>%
  mutate(mean_baro = mean(baro)) %>%
  ggplot(aes(x = date, y = mean_baro)) +
    geom_line()

# is_x in month
# hum in time
# hum vs is_rainy
# wind in time
