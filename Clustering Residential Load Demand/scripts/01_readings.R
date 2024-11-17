# Load libraries
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(janitor)


# Read data file.

zip_path <- file.path(getwd(), "raw_data/Solar home half-hour data - 1 July 2010 to 30 June 2011.zip")
csv_file <- "2010-2011 Solar home electricity data.csv"

raw_data <- read_csv(
                  unzip(zip_path, files = csv_file),
                  col_types = cols(Postcode = col_character(),
                                      date = col_character()), na = "NA",
                  skip = 1
            )

# Set seasons
seasons <- c("Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter", "Winter", "Winter", "Spring", "Spring", "Spring", "Summer")


# Transformed data and added a couple of features to facilitate EDA
transformed_data <- raw_data %>% 
  clean_names() %>% 
  select(customer, consumption_category, date, x0_30:x0_00) %>% 
  filter(consumption_category %in% c('GC')) %>% 
  group_by(customer, date) %>% 
  summarise_at(vars(x0_30:x0_00), sum) %>% 
  mutate(date = dmy(date)) %>% 
  mutate(
    day_name = wday(date, label = TRUE, abbr = F),
    weektype = if_else(day_name %in% c('Saturday','Sunday'), 'Weekend', 'Weekday'),
    month_name = month(date, label = T, abbr = F),
    season = seasons[month(date, label = T)]
  ) %>% 
  relocate(
    day_name, month_name, weektype, season,
    .after = date
  ) %>% 
  arrange(customer, date) %>% 
  ungroup()

saveRDS(transformed_data, file = "output/data/readings.rds")
