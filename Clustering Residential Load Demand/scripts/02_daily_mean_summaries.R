library(dplyr)

readings <- readRDS("output/data/readings.rds")

time_intervals <- c("x0_30"="1", "x1_00"="2", "x1_30"="3", "x2_00"="4", "x2_30"="5", "x3_00"="6", "x3_30"="7", "x4_00"="8", "x4_30"="9",
                    "x5_00"="10", "x5_30"="11", "x6_00"="12", "x6_30"="13", "x7_00"="14", "x7_30"="15", "x8_00"="16", "x8_30"="17",
                    "x9_00"="18", "x9_30"="19", "x10_00"="20", "x10_30"="21", "x11_00"="22", "x11_30"="23", "x12_00"="24", "x12_30"="25",
                    "x13_00"="26", "x13_30"="27", "x14_00"="28", "x14_30"="29", "x15_00"="30", "x15_30"="31", "x16_00"="32", "x16_30"="33",
                    "x17_00"="34", "x17_30"="35", "x18_00"="36", "x18_30"="37", "x19_00"="38", "x19_30"="39", "x20_00"="40", "x20_30"="41",
                    "x21_00"="42", "x21_30"="43", "x22_00"="44", "x22_30"="45", "x23_00"="46", "x23_30"="47", "x0_00"="48")

daily_mean_annual <- readings %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = as.numeric(time_intervals[intervals])) %>% 
  group_by(time2) %>% 
  summarise(value = mean(value)) %>% 
  arrange(time2)

saveRDS(daily_mean_annual, file = "output/data/daily_mean_annual.rds")

daily_mean_monthly <- readings %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = as.numeric(time_intervals[intervals])) %>% 
  group_by(month_name, time2) %>% 
  summarise(value = mean(value)) %>% 
  arrange(time2)

saveRDS(daily_mean_monthly, file = "output/data/daily_mean_monthly.rds")

daily_mean_annual_by_dayname <- readings %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = as.numeric(time_intervals[intervals])) %>% 
  group_by(day_name, time2) %>% 
  summarise(value = mean(value)) %>% 
  arrange(time2)

saveRDS(daily_mean_annual_by_dayname, file = "output/data/daily_mean_annual_by_dayname.rds")

daily_mean_annual_by_weektype <- readings %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = as.numeric(time_intervals[intervals])) %>% 
  group_by(weektype, time2) %>% 
  summarise(value = mean(value)) %>% 
  arrange(time2)

saveRDS(daily_mean_annual_by_weektype, file = "output/data/daily_mean_annual_by_weektype.rds")

daily_sum_annual_by_time_period <- readings %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = as.numeric(time_intervals[intervals]),
         time_intervals = as.numeric(time_intervals[intervals]),
         overnight = if_else(dplyr::between(time_intervals,1,12) | dplyr::between(time_intervals,46,48), value, 0),
         breakfast = if_else(dplyr::between(time_intervals,13,20), value, 0),
         daytime = if_else(dplyr::between(time_intervals,21,32), value, 0),
         evening = if_else(dplyr::between(time_intervals,33,45), value, 0)) %>% 
  group_by(date) %>% 
  summarise(across(overnight:evening, sum)) %>% 
  pivot_longer(cols = overnight:evening, names_to = "time_period") %>%
  arrange(time_period, date)

saveRDS(daily_sum_annual_by_time_period, file = "output/data/daily_sum_annual_by_time_period.rds")