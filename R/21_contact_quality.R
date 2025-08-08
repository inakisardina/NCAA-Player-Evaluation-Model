library(tidyverse)

# Use existing clean_data
contact_data <- clean_data %>%
  filter(!is.na(ExitSpeed), !is.na(Angle)) %>%
  mutate(
    Barrel = ifelse(ExitSpeed >= 98 & Angle >= 26 & Angle <= 30, 1, 0),
    HardHit = ifelse(ExitSpeed >= 95, 1, 0),
    SweetSpot = ifelse(Angle >= 8 & Angle <= 32, 1, 0)
  )

# PART 1: BY PITCH TYPE SUMMARY
hitter_contact_summary <- contact_data %>%
  group_by(Batter, TaggedPitchType) %>%
  summarise(
    avg_EV = mean(ExitSpeed, na.rm = TRUE),
    Barrel_Rate = mean(Barrel, na.rm = TRUE),
    HardHit_Rate = mean(HardHit, na.rm = TRUE),
    SweetSpot_Rate = mean(SweetSpot, na.rm = TRUE),
    Total_BattedBalls = n(),
    .groups = "drop"
  ) %>%
  filter(Total_BattedBalls >= 1)

# League averages by pitch type
league_contact_summary <- contact_data %>%
  group_by(TaggedPitchType) %>%
  summarise(
    league_EV = mean(ExitSpeed, na.rm = TRUE),
    league_Barrel = mean(Barrel, na.rm = TRUE),
    league_HardHit = mean(HardHit, na.rm = TRUE),
    league_SweetSpot = mean(SweetSpot, na.rm = TRUE),
    .groups = "drop"
  )

# Merge league averages and calculate Plus metrics
hitter_contact_summary <- hitter_contact_summary %>%
  left_join(league_contact_summary, by = "TaggedPitchType") %>%
  mutate(
    EV_Plus = 100 * avg_EV / league_EV,
    Barrel_Plus = 100 * Barrel_Rate / league_Barrel,
    HardHit_Plus = 100 * HardHit_Rate / league_HardHit,
    SweetSpot_Plus = 100 * SweetSpot_Rate / league_SweetSpot,
    Contact_Plus = 0.35 * EV_Plus + 0.25 * Barrel_Plus + 0.15 * HardHit_Plus + 0.25 * SweetSpot_Plus
  )

# PART 2: Aggregate to overall Contact+ for Hitting Ultra
contact_scores <- hitter_contact_summary %>%
  group_by(Batter) %>%
  summarise(
    Contact_Plus = weighted.mean(Contact_Plus, Total_BattedBalls, na.rm = TRUE),
    .groups = "drop"
  )

