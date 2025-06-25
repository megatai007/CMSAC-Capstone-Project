library(tidyverse)

data_2023=read_csv("savant_data_2023.csv")
data_2022=read_csv("savant_data_2022.csv")

breakout_players22=data_2022 %>% 
  filter(`last_name, first_name` %in% c("Hays, Austin", 
                                       "Murphy, Sean",
                                       "Heim, Jonah",
                                       "Rutschman, Adley"))


league_avg <- data_2022 %>%
  summarise(avg_woba = mean(woba, na.rm = TRUE))

breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, woba), y = woba)) +
  geom_col() +
  geom_hline(yintercept = league_avg$avg_woba,
             color = "red", linetype = "dashed", linewidth = 1) +
  coord_flip() +
  labs(x = "Player", y = "wOBA", title = "Breakout Players 2022: wOBA") +
  theme_minimal()


breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, xwoba), y = xwoba)) +
  geom_col() +
  coord_flip() +  # optional: makes bars horizontal
  labs(x = "Player", y = "xwOBA", title = "Breakout Players 2022: xwOBA") +
  theme_minimal()

breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, xwobacon), y = xwobacon)) +
  geom_col() +
  coord_flip() +  # optional: makes bars horizontal
  labs(x = "Player", y = "xwOBACON", title = "Breakout Players 2022: xwOBACON") +
  theme_minimal()

breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, oz_swing_percent), y = oz_swing_percent)) +
  geom_col() +
  coord_flip() +  # optional: makes bars horizontal
  labs(x = "Player", y = "OZ Swing%", title = "Breakout Players 2022: OZ Swing%") +
  theme_minimal()

breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, iz_contact_percent), y = iz_contact_percent)) +
  geom_col() +
  coord_flip() +  # optional: makes bars horizontal
  labs(x = "Player", y = "IZ Contact%", title = "Breakout Players 2022: IZ Contact%") +
  theme_minimal()

breakout_players22 %>%
  ggplot(aes(x = reorder(`last_name, first_name`, solidcontact_percent), y = solidcontact_percent)) +
  geom_col() +
  coord_flip() +  # optional: makes bars horizontal
  labs(x = "Player", y = "Solid Contact", title = "Breakout Players 2022: Solid Contact%") +
  theme_minimal()
