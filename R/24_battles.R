# Battles 

library(tidyverse)

# Load clean_data
data <- clean_data

# Step 1: League averages for Stuff+ calculation
league_stats <- data %>%
  filter(!is.na(TaggedPitchType), !is.na(HorzBreak), !is.na(InducedVertBreak), 
         !is.na(RelSpeed), !is.na(SpinRate)) %>%
  group_by(TaggedPitchType, Hand) %>%
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

# Gyro vs Sweeper evaluation function
evaluate_slider_stuff <- function(Velo_z, HB_z, IVB_z, Spin_z, avg_HorzBrk, avg_VertBrk) {
  is_sweeper <- avg_HorzBrk < -10 & avg_VertBrk < 10
  sweeper_val <- (0.89 * HB_z + 0.05 * Velo_z - 0.05 * IVB_z + 0.01 * Spin_z) * 100 + 100
  gyro_val <- (0.3 * Velo_z - 0.3 * IVB_z + 0.3 * Spin_z + 0.1 * HB_z) * 100 + 100
  ifelse(is_sweeper, sweeper_val, gyro_val)
}

# Step 2: Calculate Stuff+ per pitch
pitch_data_stuff <- data %>%
  filter(!is.na(TaggedPitchType), !is.na(HorzBreak), !is.na(InducedVertBreak), 
         !is.na(RelSpeed), !is.na(SpinRate)) %>%
  left_join(league_stats, by = c("TaggedPitchType", "Hand")) %>%
  mutate(
    HB_gs = case_when(
      Hand == "RHP" & TaggedPitchType %in% c("slider", "cutter") ~ -HorzBreak,
      Hand == "LHP" & TaggedPitchType %in% c("slider", "cutter") ~  HorzBreak,
      TRUE ~ HorzBreak
    ),
    league_HB_gs = case_when(
      Hand == "RHP" & TaggedPitchType %in% c("slider", "cutter") ~ -league_HorzBrk,
      Hand == "LHP" & TaggedPitchType %in% c("slider", "cutter") ~  league_HorzBrk,
      TRUE ~ league_HorzBrk
    ),
    Velo_z = (RelSpeed - league_Vel) / league_Vel,
    HB_z = (HB_gs - league_HB_gs) / sd_HorzBrk,
    IVB_z = (InducedVertBreak - league_VertBrk) / sd_VertBrk,
    Spin_z = (SpinRate - league_SpinRate) / sd_SpinRate
  ) %>%
  rowwise() %>%
  mutate(
    Stuff_plus = case_when(
      TaggedPitchType == "four-seam" ~ (0.5 * Velo_z + 0.4 * IVB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "sinker"    ~ (0.4 * Velo_z + 0.4 * HB_z - 0.3 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "slider"    ~ evaluate_slider_stuff(Velo_z, HB_z, IVB_z, Spin_z, HorzBreak, InducedVertBreak),
      TaggedPitchType == "curveball" ~ (0.3 * Velo_z - 0.6 * IVB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "cutter"    ~ (0.4 * Velo_z + 0.3 * IVB_z + 0.2 * HB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "changeup"  ~ (0.4 * Velo_z + 0.3 * HB_z - 0.2 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TRUE ~ NA_real_
    ),
    Stuff_plus = round(Stuff_plus, 1)
  ) %>%
  ungroup()

# Step 3: Battle Score calculation
battle_data <- pitch_data_stuff %>%
  mutate(
    Swing = PitchCall_clean %in% c("Swinging Strike", "Foul", "In play"),
    Miss = PitchCall_clean == "Swinging Strike",
    Contact = PitchCall_clean == "In play",
    
    Contact_Score = case_when(
      !is.na(ExitSpeed) ~ (ExitSpeed - 85) / 15,
      TRUE ~ 0
    ),
    
    Battle_Score = case_when(
      Miss ~ -1,
      Contact ~ Contact_Score,
      TRUE ~ 0
    )
  )

# Step 4: Summarize Battle Score by hitter
battles_by_hitter <- battle_data %>%
  filter(Swing) %>%
  group_by(Batter) %>%
  summarise(
    Swings = n(),
    Total_Battle_Score = sum(Battle_Score, na.rm = TRUE),
    Avg_Battle_Score = mean(Battle_Score, na.rm = TRUE),
    Avg_Stuff_Faced = mean(Stuff_plus, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Battle_Score))