library(tidyverse)

#daily temerature

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

ggplot(weather_data, aes(x = date, y = temp)) +
  geom_line()


# baro in time
weather_data %>%
  ggplot(aes(x = date, y = baro)) + 
  geom_line()

# is_x in month
# hum in time
# hum vs is_rainy
# wind in time
