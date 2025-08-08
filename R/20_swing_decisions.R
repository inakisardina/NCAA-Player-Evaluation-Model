library(tidyverse)

# Use existing clean_data
data <- clean_data

data <- data %>%
  mutate(
    PlateX = PlateLocSide,
    PlateZ = PlateLocHeight,
    Swing_numeric = ifelse(Swing == TRUE, 1, 0)
  )

# Strike zone boundaries
zone_top <- 3.5
zone_bottom <- 1.5
zone_left <- -0.83
zone_right <- 0.83

# Classify pitches as InZone or OutZone
data <- data %>%
  mutate(
    ZoneFlag = ifelse(
      PlateX >= zone_left & PlateX <= zone_right &
        PlateZ >= zone_bottom & PlateZ <= zone_top,
      "InZone", "OutZone"
    )
  )

# Calculate Chase Severity
zone_half_width <- 0.83
zone_half_height <- (zone_top - zone_bottom) / 2
zone_mid_height <- (zone_top + zone_bottom) / 2

data <- data %>%
  mutate(
    delta_x = pmax(0, abs(PlateX) - zone_half_width),
    delta_z = pmax(0, abs(PlateZ - zone_mid_height) - zone_half_height),
    ChaseSeverity = sqrt(delta_x^2 + delta_z^2),
    WeightedChase = ifelse(ZoneFlag == "OutZone" & Swing_numeric == 1, (ChaseSeverity)^2, 0),
    TakeQuality = ifelse(ZoneFlag == "OutZone" & Swing_numeric == 0, 1 / (1 + ChaseSeverity), 0)
  )

# Middle zone for swing plus
middle_left <- -0.33
middle_right <- 0.33
middle_bottom <- 2.2
middle_top <- 3.0

data <- data %>%
  mutate(
    MiddleFlag = ifelse(
      PlateX >= middle_left & PlateX <= middle_right &
        PlateZ >= middle_bottom & PlateZ <= middle_top,
      1, 0
    )
  )

# Part 1: By Pitch Type Summary
hitter_pitchtype_decision_summary <- data %>%
  group_by(Batter, TaggedPitchType) %>%
  summarise(
    Middle_Swing_Pct = mean(Swing_numeric[MiddleFlag == 1], na.rm = TRUE),
    Total_Middle = sum(MiddleFlag),
    Weighted_Chase_Score = sum(WeightedChase, na.rm = TRUE) / sum(ZoneFlag == "OutZone"),
    Weighted_Take_Score = sum(TakeQuality, na.rm = TRUE) / sum(ZoneFlag == "OutZone"),
    Total_Pitches = n(),
    .groups = "drop"
  ) %>%
  filter(Total_Middle >= 1, Total_Pitches >= 1)

# League averages
league_averages <- data %>%
  group_by(TaggedPitchType) %>%
  summarise(
    League_Middle_Swing = mean(Swing_numeric[MiddleFlag == 1], na.rm = TRUE),
    league_weighted_chase = sum(WeightedChase, na.rm = TRUE) / sum(ZoneFlag == "OutZone"),
    league_weighted_take = sum(TakeQuality, na.rm = TRUE) / sum(ZoneFlag == "OutZone"),
    .groups = "drop"
  )

# Merge league averages and apply stabilization
hitter_pitchtype_decision_summary <- hitter_pitchtype_decision_summary %>%
  left_join(league_averages, by = "TaggedPitchType") %>%
  mutate(
    safe_chase = ifelse(Weighted_Chase_Score < 0.001, 0.001, Weighted_Chase_Score),
    safe_take = ifelse(Weighted_Take_Score < 0.001, 0.001, Weighted_Take_Score),
    Swing_Plus = 100 * Middle_Swing_Pct / League_Middle_Swing,
    Chase_Plus = 100 * league_weighted_chase / safe_chase,
    Take_Plus = 100 * safe_take / league_weighted_take,
    Decision_Plus = 0.5 * Chase_Plus + 0.5 * Take_Plus,
    Eye_Plus = 0.5 * Swing_Plus + 0.5 * Decision_Plus
  )

# Part 2: Aggregate to overall Eye+ for Hitting Ultra
hitter_pitchtype_stable <- hitter_pitchtype_decision_summary %>%
  filter(Total_Pitches >= 10)

# Aggregate to overall Eye+ with weighting
eye_scores <- hitter_pitchtype_stable %>%
  group_by(Batter) %>%
  summarise(
    Eye_Plus_raw = weighted.mean(Eye_Plus, Total_Pitches, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Winsorize final Eye+ scores to cap extreme values
  mutate(Eye_Plus = pmin(Eye_Plus_raw, 250)) %>%
  select(Batter, Eye_Plus)
