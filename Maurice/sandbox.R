library(sabRmetrics)
library(tidyverse)
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
 start_date = "2024-01-01",
 end_date = "2024-12-31",
 cl = cluster
)
parallel::stopCluster(cluster)

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




