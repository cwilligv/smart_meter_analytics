# library(factoextra)
# library(purrr)
library(cluster)
library(ggplot2)
# library(mclust)
library(clusterCrit)
library(ClusterR)

excluded_dates <- c(as.Date("2011-01-26"), as.Date("2011-02-01"), as.Date("2011-02-02"), as.Date("2011-02-03"), as.Date("2011-02-05"))

time_intervals <- c("x0_30"="1", "x1_00"="2", "x1_30"="3", "x2_00"="4", "x2_30"="5", "x3_00"="6", "x3_30"="7", "x4_00"="8", "x4_30"="9",
                    "x5_00"="10", "x5_30"="11", "x6_00"="12", "x6_30"="13", "x7_00"="14", "x7_30"="15", "x8_00"="16", "x8_30"="17",
                    "x9_00"="18", "x9_30"="19", "x10_00"="20", "x10_30"="21", "x11_00"="22", "x11_30"="23", "x12_00"="24", "x12_30"="25",
                    "x13_00"="26", "x13_30"="27", "x14_00"="28", "x14_30"="29", "x15_00"="30", "x15_30"="31", "x16_00"="32", "x16_30"="33",
                    "x17_00"="34", "x17_30"="35", "x18_00"="36", "x18_30"="37", "x19_00"="38", "x19_30"="39", "x20_00"="40", "x20_30"="41",
                    "x21_00"="42", "x21_30"="43", "x22_00"="44", "x22_30"="45", "x23_00"="46", "x23_30"="47", "x0_00"="48")


readings <- readRDS("output/data/readings.rds")  %>% 
  filter(!date %in% excluded_dates)

# Time periods definitions
# breakfast: from interval 13 (6:00-6:30am) to 20 (9:30-10am)
# daytime: from interval 21 (10-10:30am) to 32 (3:30-4pm)
# evening: from interval 33 (4-4:30pm) to 45 (10-10:30pm)
# overnight: from interval 46 (10:30-11pm) to 48 (11.30-12am) and 1 (00-1:30am) to 12 (5:30-6am)


df_features <- readings %>%
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(
    time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
    time_intervals = as.numeric(time_intervals[intervals]),
    # equal = if_else(between(time, hms::as_hms("09:00:00"), hms::as_hms("12:00:00")), value, 0),
    overnight = if_else(dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48), value, 0),
    breakfast = if_else(dplyr::between(time_intervals,13,20), value, 0),
    daytime = if_else(dplyr::between(time_intervals,21,32), value, 0),
    evening = if_else(dplyr::between(time_intervals,33,45), value, 0),
    summer_overnight = if_else(season == 'Summer' & (dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48)), value, 0),
    summer_breakfast = if_else(season == 'Summer' & dplyr::between(time_intervals,13,20), value, 0),
    summer_daytime = if_else(season == 'Summer' & dplyr::between(time_intervals,21,32), value, 0),
    summer_evening = if_else(season == 'Summer' & dplyr::between(time_intervals,33,45), value, 0),
    winter_overnight = if_else(season == 'Winter' & (dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48)), value, 0),
    winter_breakfast = if_else(season == 'Winter' & dplyr::between(time_intervals,13,20), value, 0),
    winter_daytime = if_else(season == 'Winter' & dplyr::between(time_intervals,21,32), value, 0),
    winter_evening = if_else(season == 'Winter' & dplyr::between(time_intervals,33,45), value, 0),
    weekends_overnight = if_else(weektype == 'Weekend' & (dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48)), value, 0),
    weekends_breakfast = if_else(weektype == 'Weekend' & dplyr::between(time_intervals,13,20), value, 0),
    weekends_daytime = if_else(weektype == 'Weekend' & dplyr::between(time_intervals,21,32), value, 0),
    weekends_evening = if_else(weektype == 'Weekend' & dplyr::between(time_intervals,33,45), value, 0),
    weekdays_overnight = if_else(weektype == 'Weekday' & (dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48)), value, 0),
    weekdays_breakfast = if_else(weektype == 'Weekday' & dplyr::between(time_intervals,13,20), value, 0),
    weekdays_daytime = if_else(weektype == 'Weekday' & dplyr::between(time_intervals,21,32), value, 0),
    weekdays_evening = if_else(weektype == 'Weekday' & dplyr::between(time_intervals,33,45), value, 0)
  ) %>% 
  group_by(customer) %>% 
  summarise(
    mean_overnight = mean(overnight),
    mean_breakfast = mean(breakfast),
    mean_daytime = mean(daytime),
    mean_evening = mean(evening),
    sd_overnight = sd(overnight),
    sd_breakfast = sd(breakfast),
    sd_daytime = sd(daytime),
    sd_evening = sd(evening),
    mean_year = mean(value),
    mean_summer_overnight = mean(summer_overnight),
    mean_summer_breakfast = mean(summer_breakfast),
    mean_summer_daytime = mean(summer_daytime),
    mean_summer_evening = mean(summer_evening),
    mean_winter_overnight = mean(winter_overnight),
    mean_winter_breakfast = mean(winter_breakfast),
    mean_winter_daytime = mean(winter_daytime),
    mean_winter_evening = mean(winter_evening),
    mean_weekends_overnight = mean(weekends_overnight),
    mean_weekends_breakfast = mean(weekends_breakfast),
    mean_weekends_daytime = mean(weekends_daytime),
    mean_weekends_evening = mean(weekends_evening),
    mean_weekdays_overnight = mean(weekdays_overnight),
    mean_weekdays_breakfast = mean(weekdays_breakfast),
    mean_weekdays_daytime = mean(weekdays_daytime),
    mean_weekdays_evening = mean(weekdays_evening)
  ) %>% 
  mutate(
    rmp_overnight = mean_overnight/mean_year,
    rmp_breakfast = mean_breakfast/mean_year,
    rmp_daytime = mean_daytime/mean_year,
    rmp_evening = mean_evening/mean_year,
    mrsd = (1/4)*(sd_overnight/mean_overnight)+(sd_breakfast/mean_breakfast)+(sd_daytime/mean_daytime)+(sd_evening/mean_evening),
    sscore = (abs(mean_winter_overnight-mean_summer_overnight)/mean_overnight)+(abs(mean_winter_breakfast-mean_summer_breakfast)/mean_breakfast)+
      (abs(mean_winter_daytime-mean_summer_daytime)/mean_daytime)+(abs(mean_winter_evening-mean_summer_evening)/mean_evening),
    wdscore = (abs(mean_weekends_overnight-mean_weekdays_overnight)/mean_overnight)+(abs(mean_weekends_breakfast-mean_weekdays_breakfast)/mean_breakfast)+
      (abs(mean_weekends_daytime-mean_weekdays_daytime)/mean_daytime)+(abs(mean_weekends_evening-mean_weekdays_evening)/mean_evening)
  ) %>% 
  select(
    customer,
    rmp_overnight,
    rmp_breakfast,
    rmp_daytime,
    rmp_evening,
    mrsd,
    sscore,
    wdscore
  )

saveRDS(df_features, file = "output/data/df_features.rds")