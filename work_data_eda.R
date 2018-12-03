library(skimr)
library(tidyverse)
library(wesanderson)

# work_data check

skim_with(integer = list(min = partial(min, na.rm = TRUE), max = partial(max, na.rm = TRUE)), append = TRUE)

work_data %>%
  skim() %>%
  filter(stat %in% c('missing', 'min', 'max')) %>%
  select(variable, stat, formatted) %>%
  spread(stat, formatted) %>%
  select(variable, missing, min, max)

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

work_data %>%
  filter(wday(date)!=1 & wday(date)!=7) %>%
  group_by(month = month(date, label = TRUE)) %>%
  summarize(monthly_pomodoroes = sum(pomodoros, na.rm = TRUE)) %>%
  mutate(hours = monthly_pomodoroes/2) %>%
  ggplot(aes(x = month, y = monthly_pomodoroes)) +
    geom_col(fill = colores[1]) +
    geom_hline(yintercept = 160, linetype="dashed", color = colores[2]) + 
    scale_fill_manual(values = colores)

