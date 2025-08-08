library(tidyverse)

# Merge everything together by Batter
hitting_ultra <- hitter_results %>%
  select(Batter, PA, Results_Plus) %>%
  left_join(eye_scores %>% select(Batter, Eye_Plus), by = "Batter") %>%
  left_join(contact_scores %>% select(Batter, Contact_Plus), by = "Batter") %>%
  left_join(expected_results %>% select(Batter, xwOBA), by = "Batter") %>%
  left_join(battles_by_hitter %>% select(Batter, Avg_Battle_Score), by = "Batter")

# Normalize xwOBA to Plus Scale
league_xwoba <- mean(expected_results$xwOBA, na.rm = TRUE)

hitting_ultra <- hitting_ultra %>%
  mutate(
    xwOBA_Plus = 100 * xwOBA / league_xwoba
  )

# Normalize Battle Score to Plus Scale
league_battle <- mean(battles_by_hitter$Avg_Battle_Score, na.rm = TRUE)
sd_battle <- sd(battles_by_hitter$Avg_Battle_Score, na.rm = TRUE)

hitting_ultra <- hitting_ultra %>%
  mutate(
    Battle_Plus = 100 + 15 * (Avg_Battle_Score - league_battle) / sd_battle
  )

# Handle missing values by assigning league average (100)
hitting_ultra <- hitting_ultra %>%
  replace_na(list(
    Eye_Plus = 100,
    Contact_Plus = 100,
    xwOBA_Plus = 100,
    Battle_Plus = 100
  ))

# Calculate final Hitting Ultra Score
hitting_ultra <- hitting_ultra %>%
  mutate(
    Hitting_Ultra = 0.35 * Results_Plus +
      0.10 * Eye_Plus +
      0.25 * Contact_Plus +
      0.15 * xwOBA_Plus +
      0.15 * Battle_Plus
  )
