library(sabRmetrics)
library(tidyverse)
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
 start_date = "2024-01-01",
 end_date = "2024-12-31",
 cl = cluster
)
parallel::stopCluster(cluster)

write_csv(data_baseballsavant, "savant_data_2024.csv")

set.seed(27)
data_baseballsavant %>% 
  ggplot(aes(x=attack_angle, y=swing_path_tilt)) + 
  geom_point()


data_baseballsavant %>% 
  ggplot(aes(x=attack_angle, y=attack_direction)) + 
  geom_point()


data_baseballsavant %>% 
  ggplot(aes(x=swing_length, y=bat_speed)) + 
  geom_point()

data_baseballsavant %>% distinct(description)

swing= c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "fould_tip")

swing_data_2024=data_baseballsavant %>% filter(description %in% swing)

stanton_swings= swing_data_2024 %>% filter(batter_name=="Stanton, Giancarlo")

stanton_swings=stanton_swings %>% mutate(ideal_attack_angle = ifelse(attack_angle>=5 & attack_angle<=20, 1, 0)) %>% 
  mutate(ideal_attack_angle=as_factor(ideal_attack_angle))

stanton_swings %>% 
  ggplot(aes(x=attack_angle, y=swing_path_tilt,color=ideal_attack_angle)) + 
  geom_point()


stanton_swings %>% 
  ggplot(aes(x=attack_angle, y=attack_direction, color=ideal_attack_angle)) + 
  geom_point()


stanton_swings %>% 
  ggplot(aes(x=swing_length, y=bat_speed)) + 
  geom_point()


stanton_swings %>% 
  ggplot(aes(y=bat_speed, x=attack_direction, color=ideal_attack_angle)) + 
  geom_point()

stanton_swings %>% 
  ggplot(aes(x=bat_speed, y=swing_length)) + 
  geom_point()

stanton_swings=stanton_swings %>% mutate(ideal_attack_angle = ifelse(attack_angle>=5 & attack_angle<=20, 1, 0)) %>% 
  mutate(ideal_attack_angle=as_factor(ideal_attack_angle))


stanton_swings %>% 
  ggplot(aes(x=bat_speed, y=swing_length, color=ideal_attack_angle)) + 
  geom_point()


stanton_features <- stanton_swings |>
  select(bat_speed, attack_direction, swing_length, swing_path_tilt, attack_angle) |> 
  drop_na() 

std_stanton_features <- stanton_features |> 
  scale(center = TRUE, scale = TRUE)

kmeans_many_features <- std_stanton_features |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 4, nstart = 50) 

library(factoextra)
kmeans_many_features |> 
  # need to pass in data used for clustering
  fviz_cluster(data = std_stanton_features,
               geom = "point",
               ellipse = FALSE) +
  ggthemes::scale_color_colorblind() + 
  theme_light()


cluster_assignments <- kmeans_many_features$cluster
stanton_clustered <- stanton_features |> 
  mutate(cluster = factor(cluster_assignments))

# Filter to rows that were used (i.e., rows with no NAs in the selected features)
used_rows <- stanton_swings |> 
  filter(!is.na(bat_speed) & !is.na(attack_direction) & !is.na(swing_length) & 
           !is.na(swing_path_tilt) & !is.na(attack_angle)) |> 
  mutate(cluster = factor(cluster_assignments))

library(ggplot2)

ggplot(used_rows, aes(x = bat_speed, y = attack_angle, color = cluster)) +
  geom_point(alpha = 0.6) +
  ggthemes::scale_color_colorblind() +
  theme_minimal() +
  labs(title = "Bat Speed vs. Attack Angle by Cluster")

used_rows |> 
  group_by(cluster) |> 
  summarise(across(c(bat_speed, attack_direction, swing_length, swing_path_tilt, attack_angle), mean, na.rm = TRUE))

stanton_clustered <- stanton_swings |>
  filter(!is.na(bat_speed) & !is.na(attack_direction) & !is.na(swing_length) & 
           !is.na(swing_path_tilt) & !is.na(attack_angle)) |>
  mutate(cluster = factor(kmeans_many_features$cluster))


library(dplyr)

stanton_clustered |> 
  group_by(cluster, events) |> 
  summarise(n = n(), .groups = "drop") |> 
  arrange(cluster, desc(n))

library(tidyr)

stanton_clustered |>
  count(cluster, events) |>
  pivot_wider(names_from = events, values_from = n, values_fill = 0)

stanton_clustered |>
  group_by(cluster) |>
  summarise(mean_xwOBA = mean(expected_woba, na.rm = TRUE),
            median_xwOBA = median(expected_woba, na.rm = TRUE),
            max_xwOBA = max(expected_woba, na.rm = TRUE),
            n = n())

stanton_clustered |>
  group_by(cluster, balls, strikes) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(cluster, balls, strikes)


library(ggplot2)

stanton_clustered |>
  group_by(cluster, balls, strikes) |>
  summarise(count = n(), .groups = "drop") |>
  ggplot(aes(x = factor(balls), y = factor(strikes), fill = count)) +
  geom_tile(color = "white") +
  facet_wrap(~ cluster) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Swing Count by Balls-Strikes and Cluster",
       x = "Balls", y = "Strikes", fill = "Count") +
  theme_minimal()


stanton_clustered |>
  filter(!is.na(pre_runner_3b_id), outs < 2) |>
  group_by(cluster) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count))

stanton_clustered |>
  group_by(cluster) |>
  mutate(ideal_attack_angle = as.numeric(as.character(ideal_attack_angle))) %>% 
  summarise(count = n(), ideal_per=mean(ideal_attack_angle, na.rm=TRUE), launch=mean(launch_angle, na.rm=TRUE), .groups = "drop") |>
  arrange(desc(count))

stanton_clustered %>% 
  ggplot(aes(x=attack_angle, y=launch_angle, color=cluster)) +
  geom_point()




library(tidyr)

stanton_clustered |>
  filter(!is.na(bb_type)) |>
  count(cluster, bb_type) |>
  pivot_wider(names_from = bb_type, values_from = n, values_fill = 0)


# function to perform clustering for each value of k
stanton_kmeans <- function(k) {
  
  kmeans_results <- std_stanton_features |>
    kmeans(centers = k, nstart = 30)
  
  kmeans_out <- tibble(
    clusters = k,
    total_wss = kmeans_results$tot.withinss
  )
  return(kmeans_out)
}

# number of clusters to search over
n_clusters_search <- 2:12

# iterate over each k to compute total wss
kmeans_search <- n_clusters_search |> 
  map(stanton_kmeans) |> 
  bind_rows()

kmeans_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 4) +
  scale_x_continuous(breaks = n_clusters_search)

stanton_pca <- prcomp(stanton_features, center = TRUE, scale. = TRUE)
summary(stanton_pca)

stanton_pca |> 
  fviz_pca_biplot(label = "var",
                  alpha.ind = 0.25,
                  alpha.var = 0.75,
                  labelsize = 5,
                  col.var = "darkblue",
                  repel = TRUE)


