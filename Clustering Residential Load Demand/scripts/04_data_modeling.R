# library(factoextra)
# library(purrr)
library(cluster)
library(ggplot2)
# library(mclust)
library(clusterCrit)
library(ClusterR)

# normalising data

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

excluded_dates <- c(as.Date("2011-01-26"), as.Date("2011-02-01"), as.Date("2011-02-02"), as.Date("2011-02-03"), as.Date("2011-02-05"))

readings <- readRDS("output/data/readings.rds")  %>% 
  filter(!date %in% excluded_dates)

df_features <- readRDS("output/data/df_features.rds")

df_features_scale <- df_features %>% select(-customer) %>% scale() %>% as.data.frame()

gmm_optimal <- Optimal_Clusters_GMM(
  df_features_scale,
  max_clusters = 15,
  criterion = "BIC",
  dist_mode = "eucl_dist",
  seed_mode = "random_spread",
  km_iter = 7, 
  em_iter = 5, 
  var_floor = 1e-10, 
  plot_data = T,
  seed = 123
) 

df_gmm_optimal <- data.frame(
  clusters = 1:15,
  BIC = gmm_optimal
)

saveRDS(df_gmm_optimal, file = "output/data/df_gmm_optimal.rds")

gmm = GMM(df_features_scale,6, dist_mode = "eucl_dist", seed_mode = "random_spread", km_iter = 7, em_iter = 5, var_floor = 1e-10, seed = 123)
gmm_pr <- predict(gmm, df_features_scale)
saveRDS(gmm_pr, file = "output/data/gmm_pr.rds")
sil <- silhouette(gmm_pr, dist(df_features_scale))
# Silhouette plot
# plot(sil, main ="Silhouette plot - GMM")

# intCriteria(as.matrix(df_features_scale),as.integer(gmm_pr),c("Dunn", "Davies_Bouldin", "Silhouette"))

customers_by_cluster <- df_features %>% 
  mutate(cluster = paste0("C", gmm_pr)) %>% 
  group_by(cluster) %>% 
  summarise(cnt_customers = n())

saveRDS(customers_by_cluster, file = "output/data/customers_by_cluster.rds")


# for gmm clusters
clustered_data <- df_features %>% 
  # filter(weekend == 0) %>% 
  mutate(cluster = paste0("C", gmm_pr)) %>% 
  select(customer, cluster) %>%
  right_join(readings, by = c("customer")) %>%
  relocate(cluster)

saveRDS(clustered_data, file = "output/data/clustered_data.rds")

# Profiles

time_intervals <- c("x0_30"="1", "x1_00"="2", "x1_30"="3", "x2_00"="4", "x2_30"="5", "x3_00"="6", "x3_30"="7", "x4_00"="8", "x4_30"="9",
                    "x5_00"="10", "x5_30"="11", "x6_00"="12", "x6_30"="13", "x7_00"="14", "x7_30"="15", "x8_00"="16", "x8_30"="17",
                    "x9_00"="18", "x9_30"="19", "x10_00"="20", "x10_30"="21", "x11_00"="22", "x11_30"="23", "x12_00"="24", "x12_30"="25",
                    "x13_00"="26", "x13_30"="27", "x14_00"="28", "x14_30"="29", "x15_00"="30", "x15_30"="31", "x16_00"="32", "x16_30"="33",
                    "x17_00"="34", "x17_30"="35", "x18_00"="36", "x18_30"="37", "x19_00"="38", "x19_30"="39", "x20_00"="40", "x20_30"="41",
                    "x21_00"="42", "x21_30"="43", "x22_00"="44", "x22_30"="45", "x23_00"="46", "x23_30"="47", "x0_00"="48")

profiles <- clustered_data %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time = hms::as_hms(paste0(gsub('_', ':', sub('.','',intervals)),':00')),
         time2 = time_intervals[intervals]) %>% 
  group_by(cluster, time2) %>% 
  summarise(value = mean(value))

saveRDS(profiles, file = "output/data/profiles.rds")

  ggplot(profiles) +
  geom_line(aes(x=as.numeric(time2), y=value), color = "black", linewidth = 2) +
  facet_wrap(vars(cluster))

cluster_centers <- clustered_data %>% 
  group_by(cluster) %>% 
  summarise(across(x0_30:x0_00, mean)) %>% 
  pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
  mutate(time2 = time_intervals[intervals]) %>% 
  arrange(as.numeric(time2)) %>% 
  select(cluster, time2, value)

saveRDS(cluster_centers, file = "output/data/cluster_centers.rds")

df_features_cluster <- df_features %>% 
  # filter(weekend == 0) %>% 
  mutate(cluster = paste0("C", gmm_pr)) %>% 
  group_by(cluster) %>% 
  summarise(across(rmp_overnight:wdscore, mean), cnt = n())

# cl <- "C5"
# clustered_data %>% 
#   # mutate(customer == 15) %>%
#   filter(cluster == cl) %>%
#   pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
#   mutate(time2 = time_intervals[intervals]) %>% 
#   arrange(as.numeric(time2)) %>%
#   group_by(customer, time2) %>%
#   summarise(value = mean(value)) %>%
#   # summarize(value = entropy::entropy(value, unit = 'log10')) %>% 
#   # filter(value <= 1) %>%
#   ggplot() +
#   geom_line(aes(x=as.numeric(time2), y=value, colour = factor(customer))) + 
#   geom_line(
#     data =(cluster_centers %>% filter(cluster == cl)),
#     aes(x = as.numeric(time2), y = value), color = "black", linewidth = 2
#   ) + 
#   theme(legend.position = "none")
# 
# clustered_data %>% 
#   # mutate(customer == 15) %>%
#   filter(cluster == cl) %>%
#   pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
#   mutate(time2 = time_intervals[intervals]) %>% 
#   arrange(as.numeric(time2)) %>%
#   group_by(weektype, time2) %>%
#   summarise(value = mean(value)) %>%
#   # summarize(value = entropy::entropy(value, unit = 'log10')) %>% 
#   # filter(value <= 1) %>%
#   ggplot() +
#   geom_line(aes(x=as.numeric(time2), y=value, colour = factor(weektype)))
# 
# clustered_data %>% 
#   # mutate(customer == 15) %>%
#   filter(cluster == cl) %>%
#   pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
#   mutate(time2 = time_intervals[intervals]) %>% 
#   arrange(as.numeric(time2)) %>%
#   group_by(season, time2) %>%
#   summarise(value = mean(value)) %>%
#   # summarize(value = entropy::entropy(value, unit = 'log10')) %>% 
#   # filter(value <= 1) %>%
#   ggplot() +
#   geom_line(aes(x=as.numeric(time2), y=value, colour = factor(season)), size = 2)
# 
# clustered_features <- df_features_scale %>% 
#   # filter(weekend == 0) %>% 
#   mutate(cluster = paste0("C", gmm_pr)) %>% 
#   filter(cluster == cl) %>% 
#   pivot_longer(cols = rmp_overnight:rmp_daytime,names_to = "time_period", values_to = "relative_mean")
# 
#   ggplot(clustered_features) + 
#   geom_point(aes(x=relative_mean, y=mrsd, colour = time_period), size=3)
#   cl <- "C6"  
#   clustered_data %>% 
#     # mutate(customer == 15) %>%
#     filter(cluster == cl) %>%
#     pivot_longer(cols = x0_30:x0_00, names_to = "intervals") %>% 
#     mutate(time2 = time_intervals[intervals]) %>% 
#     arrange(as.numeric(time2)) %>%
#     # group_by(time2) %>% 
#     # summarise(sd = sd(value)) %>% 
#     # filter(time2 == 1) %>% 
#     ggplot(mapping = aes(x=factor(time2), y=value, group = factor(time2))) +
#     geom_boxplot(fill="skyblue", notch=FALSE, outlier.colour="red", outlier.shape=8,
#                  outlier.size=4)
