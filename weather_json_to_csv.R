library(rjson)
library(data.table)
library(tidyverse)
library(anytime)

# manipulate monthly weather data in jsons into functionable data frame 

setwd('C:\\Users\\pucha\\Projects\\circle_of_life')

## function to read json into table

get_data_from_json_into_df <- function(file_name, data){
  
  result <- fromJSON(file = file_name)
  all <- as.data.frame(do.call("rbind", result['detail']))
  end <- 4 * (length(all) %/% 4)
  
  for (i in c(1:end)){
    tmp <- as.data.frame(all[i][[1]]$detail)
    data <- bind_rows(data, tmp)
  } 
  
  return(data)
}

## creating table from all data files

items <- list.files("data_files", pattern='*.json')
data <- data.frame()

for (i in 1:length(items)){
  data <- get_data_from_json_into_df(paste('data_files\\',items[i], sep=''), data)
}

## manipulating data

data <- data %>%
  select(-icon, -ds, -hl, -hls, -hlsh, -wd) %>%
  mutate(ts = parse_time(ts)) %>%
  mutate(is_cloudy = grepl('cloud', desc),
         is_sunny = grepl('sun', desc),
         is_rainy = grepl('rain', desc),
         is_snowy = grepl('snow', desc),
         is_foggy = grepl('fog', desc),
         date = anydate(date/1000)) %>%
  select(-desc)

# # common weather features in desc to analyse logical values
# data %>%
#   select(desc) %>%
#   group_by(desc) %>%
#   count() %>%
#   arrange(desc(n))

## write to file for later use

write_csv(data, 'weather_data_cumulated.csv')



