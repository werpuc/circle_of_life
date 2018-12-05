library(tidyverse)

# hours of sleep vs day

avg_sleep_time_day <- round(mean(life_data$actual_sleep_time, na.rm = TRUE), 2)

life_data %>%
  ggplot(aes(x = date, y = actual_sleep_time)) +
    geom_col(aes(fill = is_workday), width = 1) + 
    scale_fill_manual(values = colores) + 
    geom_hline(yintercept =  avg_sleep_time_day, col = colores[3], linetype = 'dashed') + 
    geom_text(aes(as.Date('2018-11-06'), avg_sleep_time_day, label = 'average', vjust = 1)) +
    geom_text(aes(as.Date('2018-11-06'), avg_sleep_time_day, label = paste0(avg_sleep_time_day, ' hours'), vjust = -.5))

# average hours of sleep per weekday

life_data %>%
  group_by(day = wday(date,  label = TRUE)) %>%
  summarize(mean_sleep_time = round(mean(actual_sleep_time, na.rm = TRUE), 1),
            sd = round(sd(actual_sleep_time, na.rm = TRUE), 1)) %>%
  mutate(day = gsub('\\\\.', '', day),
         is_workday = (day!='niedz' & day!='sob')) %>%
  mutate(day = factor(day, day_levels)) %>%
  ggplot(aes(x = day, y = mean_sleep_time, fill = is_workday)) + 
  scale_fill_manual(values = colores) +
  labs(x = 'weekday', y = 'sleep time [hours]', title = 'Mean sleep time per weekday') +
  geom_col(aes(x = day, y = mean_sleep_time + sd, fill = !is_workday), alpha = 0.2) +
  geom_col(aes(x = day, y = mean_sleep_time, fill = is_workday)) +
  geom_label(aes(label = mean_sleep_time), vjust = 1.5) + 
  geom_col(aes(x = day, y = mean_sleep_time - sd, fill = !is_workday), alpha = 0.2) +
  guides(fill = 'none')

# pure app data 
# hard to convert time
life_data %>%
  ggplot(aes(x = date, y = time_to_bed)) +
    geom_point() + 
    scale_y_continuous(trans = "reverse" , limits = c())

# time up

avg_time_up <- round(mean(life_data$time_up, na.rm = TRUE), 2)

life_data %>%
  ggplot(aes(x = date, y = time_up)) + 
    geom_point(aes(col = is_workday)) +
    geom_hline(yintercept = avg_time_up, linetype = 'dashed', color = colores[3]) +
    geom_text(aes(as.Date('2018-11-09'), avg_time_up, label = 'average', vjust = 1)) +
    geom_text(aes(as.Date('2018-11-09'), avg_time_up, label = paste0(hms(avg_time_up)), vjust = -.5)) +
    scale_color_manual(values = colores)

life_data %>%
  ggplot(aes(x = date, y = sleep_over, fill = is_workday)) + 
    geom_col(width = 1) + 
    scale_fill_manual(values = colores) + 
    geom_hline(yintercept = round(mean(life_data$sleep_over, na.rm = TRUE), 2), linetype = 'dashed', color = colores[3]) + 
    labs(x = 'month', y = 'sleep over [hours]', title = 'Sleep over time in time')
