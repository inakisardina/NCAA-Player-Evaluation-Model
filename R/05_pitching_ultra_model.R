# Step 1: Ensure consistent year type in all sources
pitcher_summary <- pitcher_summary %>%
  mutate(year = as.character(year))

xpitching_plus <- xpitching_plus %>%
  mutate(year = as.character(year))

# Step 2: Total pitch count per pitcher-year
pitch_totals_yearly <- clean_data %>%
  group_by(Pitcher, year) %>%
  summarise(total_pitches = n(), .groups = "drop") %>%
  mutate(year = as.character(year))

# Step 3: Combine Pitching+ and XPitching+ per year
pitching_combined_yearly <- pitcher_summary %>%
  select(Pitcher, year, Pitching_plus, Stuff_plus, Command_plus) %>%
  rename(
    avg_StuffPlus = Stuff_plus,
    avg_CommandPlus = Command_plus
  ) %>%
  left_join(xpitching_plus, by = c("Pitcher", "year")) %>%
  mutate(
    Pitching_Ultra = round((Pitching_plus * 0.5 + XPitching_plus * 0.5), 1)
  ) %>%
  left_join(pitch_totals_yearly, by = c("Pitcher", "year"))

# Step 4: Career-level (Overall) aggregation directly from pitch-level data
pitching_combined_overall <- pitching_combined_yearly %>%
  filter(!is.na(Pitching_Ultra), !is.na(total_pitches)) %>%
  group_by(Pitcher) %>%
  summarise(
    Pitching_plus = round(weighted.mean(Pitching_plus, total_pitches, na.rm = TRUE), 1),
    XPitching_plus = round(weighted.mean(XPitching_plus, total_pitches, na.rm = TRUE), 1),
    avg_StuffPlus = round(weighted.mean(avg_StuffPlus, total_pitches, na.rm = TRUE), 1),
    avg_CommandPlus = round(weighted.mean(avg_CommandPlus, total_pitches, na.rm = TRUE), 1),
    Pitching_Ultra = round(weighted.mean(Pitching_Ultra, total_pitches, na.rm = TRUE), 1),
    total_pitches = sum(total_pitches, na.rm = TRUE),
    year = "Overall",
    .groups = "drop"
  )

# Step 5: Combine yearly + overall
pitching_ultra <- bind_rows(pitching_combined_yearly, pitching_combined_overall) %>%
  mutate(year = factor(year, levels = c("2024", "2025", "Overall"))) %>%
  arrange(Pitcher, year)

# Step 6: Percentile ranks within year groups
pitching_ultra <- pitching_ultra %>%
  group_by(year) %>%
  mutate(
    Pitching_plus_percentile = round(percent_rank(Pitching_plus) * 100, 1),
    XPitching_plus_percentile = round(percent_rank(XPitching_plus) * 100, 1),
    Pitching_Ultra_percentile = round(percent_rank(Pitching_Ultra) * 100, 1),
    Command_plus_percentile = round(percent_rank(avg_CommandPlus) * 100, 1),
    Stuff_plus_percentile = round(percent_rank(avg_StuffPlus) * 100, 1)
  ) %>%
  ungroup()
