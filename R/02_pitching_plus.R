# 1. Summarize pitcher "stuff" metrics with year
pitcher_stuff <- clean_data %>%
  filter(!is.na(TaggedPitchType), !is.na(HorzBreak), !is.na(InducedVertBreak), !is.na(RelSpeed), !is.na(SpinRate)) %>%
  mutate(year = as.character(year)) %>%
  group_by(Pitcher, TaggedPitchType, PitcherTeam, Hand, year) %>%
  summarise(
    avg_Vel = mean(RelSpeed, na.rm = TRUE),
    avg_HorzBrk = mean(HorzBreak, na.rm = TRUE),
    avg_VertBrk = mean(InducedVertBreak, na.rm = TRUE),
    avg_SpinRate = mean(SpinRate, na.rm = TRUE),
    pitch_count = n(),
    .groups = "drop"
  )

# 2. League averages by year
league_stats <- clean_data %>%
  filter(!is.na(TaggedPitchType), !is.na(HorzBreak), !is.na(InducedVertBreak), !is.na(RelSpeed)) %>%
  mutate(year = as.character(year)) %>%
  group_by(TaggedPitchType, Hand, year) %>%
  summarise(
    league_Vel = mean(RelSpeed, na.rm = TRUE),
    league_HorzBrk = mean(HorzBreak, na.rm = TRUE),
    league_VertBrk = mean(InducedVertBreak, na.rm = TRUE),
    league_SpinRate = mean(SpinRate, na.rm = TRUE),
    sd_HorzBrk = sd(HorzBreak, na.rm = TRUE),
    sd_VertBrk = sd(InducedVertBreak, na.rm = TRUE),
    sd_SpinRate = sd(SpinRate, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Average exit velocity
exit_velocity <- clean_data %>%
  filter(!is.na(ExitSpeed), !is.na(TaggedPitchType)) %>%
  mutate(year = as.character(year)) %>%
  group_by(Pitcher, TaggedPitchType, year) %>%
  summarise(avg_ev = mean(ExitSpeed, na.rm = TRUE), .groups = "drop")

# 3.5 Command+ calculation
command_tagged <- clean_data %>%
  filter(!is.na(PlateLocSide), !is.na(PlateLocHeight), !is.na(TaggedPitchType), !is.na(PitcherThrows)) %>%
  mutate(
    TaggedPitchType = tolower(TaggedPitchType),
    is_LHP = PitcherThrows == "Left",
    year = as.character(year),
    
    is_competitive = PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
      PlateLocHeight >= 1.2 & PlateLocHeight <= 4.0,
    
    location_rating = case_when(
      TaggedPitchType == "four-seam" &
        (PlateLocHeight >= 3.2 | (abs(PlateLocSide) >= 0.7 & abs(PlateLocSide) <= 0.95)) &
        is_competitive ~ "ideal",
      
      TaggedPitchType == "sinker" &
        (PlateLocHeight <= 2.2 | (abs(PlateLocSide) >= 0.7 & abs(PlateLocSide) <= 0.95)) &
        is_competitive ~ "ideal",
      
      TaggedPitchType == "slider" &
        PlateLocHeight <= 2 &
        ((PlateLocSide < 0 & !is_LHP) | (PlateLocSide > 0 & is_LHP)) &
        is_competitive ~ "ideal",
      
      TaggedPitchType == "changeup" &
        PlateLocHeight <= 2 & is_competitive ~ "ideal",
      
      TaggedPitchType == "curveball" &
        PlateLocHeight <= 2 & is_competitive ~ "ideal",
      
      TaggedPitchType == "cutter" &
        PlateLocHeight >= 2.5 &
        abs(PlateLocSide) >= 0.6 & abs(PlateLocSide) <= 1.1 &
        is_competitive ~ "ideal",
      
      PlateLocSide < -1.6 | PlateLocSide > 1.6 |
        PlateLocHeight < 1.0 | PlateLocHeight > 4.2 ~ "bad",
      
      abs(PlateLocSide) <= 0.3 & PlateLocHeight >= 2.2 & PlateLocHeight <= 3.2 ~ "bad",
      
      TRUE ~ "non-ideal"
    ),
    
    is_strike = ifelse(PitchCall %in% c("StrikeCalled", "StrikeC'alled", "StrikeSwinging", "FoulBall"), 1, 0)
  )

pitcher_command <- command_tagged %>%
  group_by(Pitcher, TaggedPitchType, year) %>%
  summarise(
    pct_ideal = mean(location_rating == "ideal", na.rm = TRUE),
    pct_bad = mean(location_rating == "bad", na.rm = TRUE),
    pct_strikes = mean(is_strike == 1, na.rm = TRUE),
    .groups = "drop"
  )

league_command <- pitcher_command %>%
  group_by(TaggedPitchType, year) %>%
  summarise(
    league_ideal = mean(pct_ideal, na.rm = TRUE),
    league_strike = mean(pct_strikes, na.rm = TRUE),
    league_bad = mean(pct_bad, na.rm = TRUE),
    .groups = "drop"
  )

penalty_factor <- 0.3

pitcher_command <- pitcher_command %>%
  left_join(league_command, by = c("TaggedPitchType", "year")) %>%
  mutate(
    loc_plus = pct_ideal / league_ideal,
    strike_plus = pct_strikes / league_strike,
    penalty = pmin(pct_bad / league_bad, 2),
    Command_plus = round(((loc_plus * 0.5 + strike_plus * 0.3) * (1 - penalty_factor * penalty)) * 100, 1)
  )

# 4. Stuff+ calculation
# Helper: normalize HB by side of the pitcher
normalize_hb <- function(hand, hb, side = c("arm", "glove")) {
  side <- match.arg(side)
  if (side == "arm") {
    # Arm-side positive for both hands
    ifelse(hand == "RHP", hb, -hb)
  } else {
    # Glove-side positive for both hands
    ifelse(hand == "RHP", -hb, hb)
  }
}

evaluate_slider_stuff <- function(Velo_z, HB_gs_z, IVB_z, Spin_z, HB_gs_raw, VertBrk_raw) {
  is_sweeper <- (HB_gs_raw > 10) & (VertBrk_raw < 10)
  sweeper_val <- (0.89 * HB_gs_z + 0.05 * Velo_z - 0.05 * IVB_z + 0.01 * Spin_z) * 100 + 100
  gyro_val    <- (0.30 * Velo_z - 0.30 * IVB_z + 0.30 * Spin_z + 0.10 * HB_gs_z) * 100 + 100
  ifelse(is_sweeper, sweeper_val, gyro_val)
}

pitching_plus <- pitcher_stuff %>%
  left_join(league_stats, by = c("TaggedPitchType", "Hand", "year")) %>%
  mutate(
    HB_as        = normalize_hb(Hand, avg_HorzBrk, side = "arm"),
    HB_gs        = normalize_hb(Hand, avg_HorzBrk, side = "glove"),
    league_HB_as = normalize_hb(Hand, league_HorzBrk, side = "arm"),
    league_HB_gs = normalize_hb(Hand, league_HorzBrk, side = "glove"),
    Velo_z = (avg_Vel - league_Vel) / league_Vel,
    HB_as_z = (HB_as - league_HB_as) / sd_HorzBrk,
    HB_gs_z = (HB_gs - league_HB_gs) / sd_HorzBrk,
    IVB_z   = (avg_VertBrk - league_VertBrk) / sd_VertBrk,
    Spin_z  = (avg_SpinRate - league_SpinRate) / sd_SpinRate
  ) %>%
  rowwise() %>%
  mutate(
    Stuff_plus = case_when(
      TaggedPitchType == "four-seam" ~ (0.5 * Velo_z + 0.4 * IVB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "sinker"    ~ (0.4 * Velo_z + 0.4 * HB_as_z - 0.3 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "slider"    ~ evaluate_slider_stuff(
        Velo_z, HB_gs_z, IVB_z, Spin_z,
        HB_gs_raw = HB_gs, VertBrk_raw = avg_VertBrk
      ),
      TaggedPitchType == "curveball" ~ (0.3 * Velo_z - 0.6 * IVB_z + 0.1 * Spin_z + 0.0 * HB_gs_z) * 100 + 100,
      TaggedPitchType == "cutter"    ~ (0.4 * Velo_z + 0.3 * IVB_z + 0.2 * HB_gs_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "changeup"  ~ (0.4 * Velo_z + 0.3 * HB_as_z - 0.2 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TRUE ~ NA_real_
    ),
    Stuff_plus = round(Stuff_plus, 1)
  ) %>%
  ungroup() %>%
  left_join(pitcher_command %>% select(Pitcher, TaggedPitchType, year, Command_plus),
            by = c("Pitcher", "TaggedPitchType", "year")) %>%
  left_join(exit_velocity, by = c("Pitcher", "TaggedPitchType", "year"))

# 5. Final metric calculation
pitching_plus <- pitching_plus %>%
  mutate(
    Contact_plus = round((1 - avg_ev / 85) * 100 + 100, 1),
    Raw_Pitching_plus = round((Stuff_plus * 0.4 + Command_plus * 0.3 + Contact_plus * 0.3), 1)
  )

league_avg_pitching <- mean(pitching_plus$Raw_Pitching_plus, na.rm = TRUE)

pitching_plus <- pitching_plus %>%
  mutate(
    weight = pmin(pitch_count / 100, 1),
    PerPitch_Pitching_plus = round((Raw_Pitching_plus * weight + league_avg_pitching * (1 - weight)), 1)
  )

# 6. Pitcher-year summary
pitcher_summary <- pitching_plus %>%
  filter(!is.na(Pitcher), !is.na(year), !is.na(PerPitch_Pitching_plus), !is.na(pitch_count)) %>%
  group_by(Pitcher, year) %>%
  summarise(
    Pitching_plus = round(weighted.mean(PerPitch_Pitching_plus, pitch_count, na.rm = TRUE), 1),
    Stuff_plus = round(weighted.mean(Stuff_plus, pitch_count, na.rm = TRUE), 1),
    Command_plus = round(weighted.mean(Command_plus, pitch_count, na.rm = TRUE), 1),
    Contact_plus = round(weighted.mean(Contact_plus, pitch_count, na.rm = TRUE), 1),
    total_pitches = sum(pitch_count, na.rm = TRUE),
    pitch_types_used = n_distinct(TaggedPitchType),
    .groups = "drop"
  )

# 7. Pitcher overall summary
pitcher_overall <- pitching_plus %>%
  filter(!is.na(Pitcher), !is.na(PerPitch_Pitching_plus), !is.na(pitch_count)) %>%
  group_by(Pitcher) %>%
  summarise(
    Pitching_plus = round(weighted.mean(PerPitch_Pitching_plus, pitch_count, na.rm = TRUE), 1),
    Stuff_plus = round(weighted.mean(Stuff_plus, pitch_count, na.rm = TRUE), 1),
    Command_plus = round(weighted.mean(Command_plus, pitch_count, na.rm = TRUE), 1),
    Contact_plus = round(weighted.mean(Contact_plus, pitch_count, na.rm = TRUE), 1),
    total_pitches = sum(pitch_count, na.rm = TRUE),
    pitch_types_used = n_distinct(TaggedPitchType),
    year = "Overall",
    .groups = "drop"
  )

# 8. Final output
pitching_plus_summary <- bind_rows(pitcher_summary, pitcher_overall) %>%
  mutate(year = as.character(year)) %>%
  arrange(Pitcher, factor(year, levels = c("2024", "2025", "Overall")))