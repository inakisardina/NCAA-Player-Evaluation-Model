library(dplyr)
library(stringr)

OVERALL_YEAR <- 9999L

coerce_year_int <- function(df) {
  if ("year" %in% names(df)) dplyr::mutate(df, year = as.integer(year)) else df
}
safe_bind_rows <- function(a, b) {
  a <- coerce_year_int(a)
  b <- coerce_year_int(b)
  dplyr::bind_rows(a, b)
}

clean_data <- coerce_year_int(clean_data)

# COUNT-LEVEL METRICS BY YEAR AND OVERALL
count_summary_by_year <- clean_data %>%
  mutate(
    Count     = paste0(Balls, "-", Strikes),
    Whiff     = as.integer(Swing & PitchCall_clean == "Swinging Strike"),
    Strike    = as.integer(PitchCall_clean %in% c("Called Strike", "Swinging Strike", "Foul")),
    HardHit   = as.integer(!is.na(ExitSpeed) & ExitSpeed >= 95),
    BallInPlay= as.integer(PitchCall_clean == "In play"),
    K         = as.integer(KorBB == "Strikeout"),
    BB        = as.integer(KorBB == "Walk"),
    HBP       = as.integer(KorBB == "HitByPitch"),
    Hit       = as.integer(PlayResult_clean %in% c("Single", "Double", "Triple", "HomeRun"))
  ) %>%
  group_by(Pitcher, year, Count) %>%
  summarise(
    Pitches     = n(),
    WhiffRate   = sum(Whiff,   na.rm = TRUE) / pmax(sum(Swing,      na.rm = TRUE), 1),
    StrikeRate  = sum(Strike,  na.rm = TRUE) / Pitches,
    HardHitRate = sum(HardHit, na.rm = TRUE) / pmax(sum(BallInPlay, na.rm = TRUE), 1),
    HitRate     = sum(Hit,     na.rm = TRUE) / Pitches,
    BB_Rate     = sum(BB,      na.rm = TRUE) / Pitches,
    K_Rate      = sum(K,       na.rm = TRUE) / Pitches,
    HBP_Rate    = sum(HBP,     na.rm = TRUE) / Pitches,
    .groups = "drop"
  ) %>%
  mutate(
    CountGroup = case_when(
      Count %in% c("2-0", "3-0", "3-1") ~ "Hitter Count",
      Count %in% c("0-0", "1-1", "2-2") ~ "Neutral Count",
      Count %in% c("0-1", "0-2", "1-2") ~ "Pitcher Count",
      TRUE ~ "Other"
    )
  )

# STRIKEOUT AFTER FALLING BEHIND
k_after_behind_by_year <- clean_data %>%
  mutate(PA_ID = paste(GameID, BatterId, Pitcher, Inning, sep = "_")) %>%
  group_by(Pitcher, year, PA_ID) %>%
  summarise(
    EverBehind = any(paste0(Balls, "-", Strikes) %in% c("2-0", "3-0", "3-1")),
    Result_K   = any(KorBB == "Strikeout"),
    .groups = "drop"
  ) %>%
  group_by(Pitcher, year) %>%
  summarise(
    K_After_Behind = sum(Result_K & EverBehind),
    Total_Behind   = sum(EverBehind),
    .groups = "drop"
  ) %>%
  mutate(
    K_Rate_Behind = if_else(Total_Behind > 0, K_After_Behind / Total_Behind, NA_real_)
  )

k_overall <- k_after_behind_by_year %>%
  group_by(Pitcher) %>%
  summarise(
    K_After_Behind = sum(K_After_Behind, na.rm = TRUE),
    Total_Behind   = sum(Total_Behind,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    K_Rate_Behind = if_else(Total_Behind > 0, K_After_Behind / Total_Behind, NA_real_),
    year = OVERALL_YEAR
  )

k_after_behind_by_year <- safe_bind_rows(k_after_behind_by_year, k_overall)

# WALK AFTER GETTING AHEAD
bb_after_ahead_by_year <- clean_data %>%
  mutate(PA_ID = paste(GameID, BatterId, Pitcher, Inning, sep = "_")) %>%
  group_by(Pitcher, year, PA_ID) %>%
  summarise(
    EverAhead = any(paste0(Balls, "-", Strikes) %in% c("0-2", "1-2")),
    Result_BB = any(KorBB == "Walk"),
    .groups = "drop"
  ) %>%
  group_by(Pitcher, year) %>%
  summarise(
    BB_After_Ahead = sum(Result_BB & EverAhead),
    Total_Ahead    = sum(EverAhead),
    .groups = "drop"
  ) %>%
  mutate(
    BB_Rate_Ahead = if_else(Total_Ahead > 0, BB_After_Ahead / Total_Ahead, NA_real_)
  )

bb_overall <- bb_after_ahead_by_year %>%
  group_by(Pitcher) %>%
  summarise(
    BB_After_Ahead = sum(BB_After_Ahead, na.rm = TRUE),
    Total_Ahead    = sum(Total_Ahead,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    BB_Rate_Ahead = if_else(Total_Ahead > 0, BB_After_Ahead / Total_Ahead, NA_real_),
    year = OVERALL_YEAR
  )

bb_after_ahead_by_year <- safe_bind_rows(bb_after_ahead_by_year, bb_overall)


# FIRST PITCH STRIKE %
fps_by_year <- clean_data %>%
  filter(PitchofPA == 1) %>%
  group_by(Pitcher, year) %>%
  summarise(
    FirstPitchStrikes  = sum(PitchCall_clean %in% c("Called Strike", "Swinging Strike", "Foul")),
    Total_FirstPitches = n(),
    .groups = "drop"
  ) %>%
  mutate(
    FPS_percent = FirstPitchStrikes / pmax(Total_FirstPitches, 1)
  )

fps_overall <- fps_by_year %>%
  group_by(Pitcher) %>%
  summarise(
    FirstPitchStrikes  = sum(FirstPitchStrikes,  na.rm = TRUE),
    Total_FirstPitches = sum(Total_FirstPitches, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FPS_percent = FirstPitchStrikes / pmax(Total_FirstPitches, 1),
    year = OVERALL_YEAR
  )

fps_by_year <- safe_bind_rows(fps_by_year, fps_overall)

# SHORT PLATE APPEARANCES
pa_length_by_year <- clean_data %>%
  group_by(GameID, Inning, BatterId, Pitcher, year) %>%
  summarise(PA_Length = n(), .groups = "drop") %>%
  group_by(Pitcher, year) %>%
  summarise(
    Short_PAs = sum(PA_Length <= 4),
    Total_PAs = n(),
    .groups = "drop"
  ) %>%
  mutate(Short_PA_Rate = Short_PAs / pmax(Total_PAs, 1))

pa_length_overall <- pa_length_by_year %>%
  group_by(Pitcher) %>%
  summarise(
    Short_PAs = sum(Short_PAs, na.rm = TRUE),
    Total_PAs = sum(Total_PAs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Short_PA_Rate = Short_PAs / pmax(Total_PAs, 1),
    year = OVERALL_YEAR
  )

pa_length_by_year <- safe_bind_rows(pa_length_by_year, pa_length_overall)

# MOST USED PITCH BY COUNT
most_used_pitch_by_count <- clean_data %>%
  filter(!is.na(TaggedPitchType)) %>%
  mutate(Count = paste0(Balls, "-", Strikes)) %>%
  group_by(Pitcher, year, Count, TaggedPitchType) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Pitcher, year, Count) %>%
  mutate(
    total = sum(n),
    usage_pct = round(100 * n / total, 1)
  ) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  select(Pitcher, year, Count, Most_Used_Pitch = TaggedPitchType, Usage_Percent = usage_pct)

most_used_pitch_overall <- clean_data %>%
  filter(!is.na(TaggedPitchType)) %>%
  mutate(Count = paste0(Balls, "-", Strikes)) %>%
  group_by(Pitcher, Count, TaggedPitchType) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Pitcher, Count) %>%
  mutate(
    total = sum(n),
    usage_pct = round(100 * n / total, 1)
  ) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  mutate(year = OVERALL_YEAR) %>%
  select(Pitcher, year, Count, Most_Used_Pitch = TaggedPitchType, Usage_Percent = usage_pct)

most_used_pitch_by_count <- safe_bind_rows(most_used_pitch_by_count, most_used_pitch_overall)

# COMBINE COUNT BEHAVIOR METRICS
count_behavior_summary_by_year <- fps_by_year %>%
  left_join(k_after_behind_by_year, by = c("Pitcher", "year")) %>%
  left_join(bb_after_ahead_by_year, by = c("Pitcher", "year")) %>%
  left_join(pa_length_by_year,      by = c("Pitcher", "year"))

stopifnot(
  is.integer(k_after_behind_by_year$year),
  is.integer(bb_after_ahead_by_year$year),
  is.integer(fps_by_year$year),
  is.integer(pa_length_by_year$year),
  is.integer(most_used_pitch_by_count$year)
)