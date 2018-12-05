library(skimr)
library(tidyverse)
library(wesanderson)
library(DT)

# work_data check

skim_with(integer = list(min = partial(min, na.rm = TRUE), max = partial(max, na.rm = TRUE)), append = TRUE)

work_data %>%
  skim() %>%
  filter(stat %in% c('missing', 'min', 'max')) %>%
  select(variable, stat, formatted) %>%
  spread(stat, formatted) %>%
  select(variable, missing, min, max)


# pomodoros per weekday

colores <- wes_palette(name = 'GrandBudapest1', type = 'continuous')
day_levels <- c('pon', 'wt', 'œr', 'czw', 'pt', 'sob', 'niedz')

work_data %>%
  group_by(day = wday(date,  label = TRUE)) %>%
  summarize(mean_pomodoros = round(mean(pomodoros, na.rm = TRUE), 1),
            sd = round(sd(pomodoros, na.rm = TRUE), 1)) %>%
  mutate(day = gsub('\\\\.', '', day),
        is_workday = (day!='niedz' & day!='sob')) %>%
  mutate(day = factor(day, day_levels)) %>%
  ggplot(aes(x = day, y = mean_pomodoros, fill = is_workday)) + 
    scale_fill_manual(values = colores) +
    labs(x = 'weekday', y = 'pomodoros', title = 'Mean pomodoros per weekday') +
    geom_col(aes(x = day, y = mean_pomodoros + sd, fill = !is_workday), alpha = 0.2) +
    geom_col(aes(x = day, y = mean_pomodoros, fill = is_workday)) +
    geom_label(aes(label = mean_pomodoros), vjust = 1.5) + 
    geom_col(aes(x = day, y = mean_pomodoros - sd, fill = !is_workday), alpha = 0.2) +
    guides(fill = 'none')
    # scale_colour_continuous(name  ="value",
    #                       breaks=c("min", "mean", "max"),
    #                       labels=c("min", "mean", "max"))

# pomodoros per month
work_data %>%
  filter(wday(date)!=1 & wday(date)!=7) %>%
  group_by(month = month(date, label = TRUE)) %>%
  summarize(monthly_pomodoroes = sum(pomodoros, na.rm = TRUE)) %>%
  mutate(hours = monthly_pomodoroes/2) %>%
  ggplot(aes(x = month, y = monthly_pomodoroes)) +
    geom_col(fill = colores[1]) +
    geom_hline(yintercept = 160, linetype="dashed", color = colores[2]) + 
    geom_text(aes(3, 160, label = '80 hours', vjust = -.5, color = colores[2])) + 
    scale_fill_manual(values = colores) +
    theme(legend.position="none")

# pomodoros per week

avg_pomodoros_weekly <-  work_data %>%
  group_by(week_no = week(date)) %>%
  summarize(weekly_pomodores = sum(pomodoros, na.rm = TRUE)) %>%
  summarize(mean(weekly_pomodores)) %>%
  .[[1]] %>%
  round()

work_data %>%
  group_by(week_no = week(date)) %>%
  summarize(weekly_pomodoroes = sum(pomodoros, na.rm = TRUE),
            avg_weekly_pomodores = round(mean(pomodoros, na.rm = TRUE), 2)) %>%
  mutate(hours = weekly_pomodoroes/2) %>%
  ggplot(aes(x = week_no, y = weekly_pomodoroes)) +
  geom_col(fill = colores[1]) +
  geom_hline(yintercept = 40, linetype="dashed", color = colores[2]) + 
  geom_text(aes(45, 40, label = '20 hours', vjust = -.5, color = colores[2])) + 
  scale_fill_manual(values = colores) +
  geom_hline(yintercept = avg_pomodoros_ever, linetype = "dashed", color = colores[3]) + 
  geom_text(aes(45, avg_pomodoros_weekly, label = paste0(avg_pomodoros_weekly/2, ' hours'), vjust = -.2)) + 
  geom_text(aes(45, avg_pomodoros_weekly, label = 'average', vjust = 1.5)) + 
  theme(legend.position="none") + 
  labs(x = 'week', y = 'sum of pomodoros', title = 'Sum of pomodoros per week') 

# pomodors per day

avg_pomodoros_daily = round(mean(work_data[wday(work_data$date)!=1 & wday(work_data$date)!=7, ]$pomodoros, na.rm = TRUE))

work_data %>%
  mutate(is_workday = (wday(date)!=1 & wday(date)!=7)) %>%
  ggplot(aes(x = date, y = pomodoros)) +
    geom_col(aes(fill = is_workday), show.legend = TRUE, width = 1) +
    scale_fill_manual(values = colores) +
    geom_hline(yintercept = avg_pomodoros_daily, linetype = 'dashed', color = colores[3]) + 
    geom_text(aes(as.Date('2018-11-09'), y = avg_pomodoros_daily, label = paste0(avg_pomodoros_daily/2, ' hours'), vjust = -.5)) +
    geom_text(aes(as.Date('2018-11-09'), y = avg_pomodoros_daily, label = 'average', vjust = 1.3)) +
    labs(x = 'month', y = 'pomodoros', title = 'Pomodoros in time')


# time_up vs weekday

work_data %>%
  group_by(day = wday(date,  label = TRUE)) %>%
  summarize(mean_timeup = round(mean(time_up, na.rm = TRUE)/3600, 1),
            sd = round(sd(time_up, na.rm = TRUE)/3600, 1)) %>%
  mutate(day = gsub('\\\\.', '', day),
         is_workday = (day!='niedz' & day!='sob')) %>%
  mutate(day = factor(day, day_levels)) %>%
  ggplot(aes(x = day, y = mean_timeup, fill = is_workday)) + 
  scale_fill_manual(values = colores) +
  labs(x = 'weekday', y = 'time up [hours]', title = 'Mean time up per weekday') +
  geom_col(aes(x = day, y = mean_timeup + sd, fill = !is_workday), alpha = 0.2) +
  geom_col(aes(x = day, y = mean_timeup, fill = is_workday)) +
  geom_label(aes(label = mean_timeup), vjust = 1.5) + 
  geom_col(aes(x = day, y = mean_timeup - sd, fill = !is_workday), alpha = 0.2) +
  guides(fill = 'none')
  

