library("tidyverse")
library("skimr")
library('anytime')
library('lubridate')

## reading data and preparing for later analysis
weather_data <- read_csv('weather_data_cumulated.csv')
work_data = read_csv('my_work_data.csv')
sleep_data = read_delim('sleepdata.csv', ';')

sleep_data = sleep_data[,1:4]
colnames(sleep_data) <- c('start', 'end', 'sleep_quality', 'time_in_bed')

sleep_data <- sleep_data %>%
  mutate(planned_time_up = parse_time(round_date(end, unit = 'minute'), format='%Y-%m-%d %H:%M:%S'),
         date = parse_date(end, format='%Y-%m-%d %H:%M:%S'),
         time_to_bed = parse_time(round_date(start, unit = 'minute'), format='%Y-%m-%d %H:%M:%S'),
         in_bed_before_midnight = start < anydate(end),
         sleep_quality = parse_number(sleep_quality)) %>%
  select(date, time_to_bed, in_bed_before_midnight, time_in_bed, sleep_quality, planned_time_up)

life_data <- work_data %>%
  mutate(is_workday = wday(date) != 1 & wday(date) != 7) %>%
  left_join(sleep_data, by = 'date') %>%
  mutate(sleep_over = round(difftime(time_up, planned_time_up, units = 'hour'), 2),
         actual_sleep_time = round(difftime(as.difftime(time_in_bed) + sleep_over, units = 'hour'),2)) %>%
  select(date, is_workday, pomodoros, sleep_over, actual_sleep_time, time_up, time_to_bed, in_bed_before_midnight, time_in_bed, sleep_quality, planned_time_up)

life_data <- weather_data %>%
  filter(ts == parse_time('6:00')) %>%
  right_join(life_data, by = 'date') %>%
  select(-ts)

skim(life_data)
