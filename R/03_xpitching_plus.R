library(dplyr)
library(stringr)

# Step 1: Prepare pitch-level data
pitch_features <- clean_data %>%
  filter(
    !is.na(RelSpeed), !is.na(HorzBreak), !is.na(InducedVertBreak),
    !is.na(SpinAxis), !is.na(PitchCall),
    !is.na(PlateLocSide), !is.na(PlateLocHeight)
  ) %>%
  select(Pitcher, TaggedPitchType, RelSpeed, HorzBreak, InducedVertBreak, SpinRate,
         SpinAxis, PlateLocSide, PlateLocHeight, PitchCall, ExitSpeed, year) %>%
  mutate(
    pitch_id = row_number(),
    year = as.character(year)
  )

# Step 2: Scale features for clustering
pitch_features_scaled <- pitch_features %>%
  mutate(
    z_Velo = scale(RelSpeed),
    z_HorzBrk = scale(HorzBreak),
    z_VertBrk = scale(InducedVertBreak),
    z_SpinAxis = scale(SpinAxis),
    z_PlateX = scale(PlateLocSide),
    z_PlateZ = scale(PlateLocHeight),
    z_SpinRate = scale(SpinRate)
  )

# Step 3: K-means clustering (global, not year-specific)
set.seed(42)
kmeans_model <- kmeans(
  pitch_features_scaled %>%
    select(z_Velo, z_HorzBrk, z_VertBrk, z_SpinAxis, z_PlateX, z_PlateZ),
  centers = 50,
  nstart = 50
)
pitch_features$bucket <- kmeans_model$cluster

# Step 4: Define binary outcome: bcw (better contact or whiff)
pitch_features <- pitch_features %>%
  mutate(
    PitchCall_clean = str_to_lower(PitchCall),
    is_whiff = PitchCall_clean %in% c("strikeswinging", "strikeswingingblocked"),
    is_called_strike = PitchCall_clean %in% c("strikecalled", "strikec'alled"),
    is_csw = is_whiff | is_called_strike,
    is_weak_contact = PitchCall_clean == "inplay" & !is.na(ExitSpeed) & ExitSpeed < 85,
    bcw = as.integer(is_csw | is_weak_contact)
  )

# Step 5: Expected BCW% per bucket
bucket_performance <- pitch_features %>%
  group_by(bucket) %>%
  summarise(
    bucket_bcw = mean(bcw, na.rm = TRUE),
    pitch_count = n(),
    .groups = "drop"
  )

# Step 6: Actual per-pitcher, per-year
pitcher_performance_year <- pitch_features %>%
  group_by(Pitcher, year) %>%
  summarise(
    actual_bcw = mean(bcw, na.rm = TRUE),
    .groups = "drop"
  )

# Step 7: Expected per-pitcher, per-year
pitcher_expected_year <- pitch_features %>%
  left_join(bucket_performance, by = "bucket") %>%
  group_by(Pitcher, year) %>%
  summarise(
    expected_bcw = mean(bucket_bcw, na.rm = TRUE),
    .groups = "drop"
  )

# Step 8: XPitching+ per year
xpitching_plus_yearly <- pitcher_performance_year %>%
  left_join(pitcher_expected_year, by = c("Pitcher", "year")) %>%
  filter(!is.na(actual_bcw), !is.na(expected_bcw), expected_bcw > 0) %>%
  mutate(
    XPitching_plus = round((actual_bcw / expected_bcw) * 100, 1)
  )

# Step 9: XPitching+ overall (career-level)
xpitching_plus_overall <- pitch_features %>%
  left_join(bucket_performance, by = "bucket") %>%
  group_by(Pitcher) %>%
  summarise(
    actual_bcw = mean(bcw, na.rm = TRUE),
    expected_bcw = mean(bucket_bcw, na.rm = TRUE),
    XPitching_plus = round((actual_bcw / expected_bcw) * 100, 1),
    year = "Overall",
    .groups = "drop"
  ) %>%
  filter(!is.na(actual_bcw), !is.na(expected_bcw), expected_bcw > 0)

# Step 10: Final output
xpitching_plus <- bind_rows(
  mutate(xpitching_plus_yearly, year = as.character(year)),
  xpitching_plus_overall
) %>%
  arrange(Pitcher, factor(year, levels = c("2024", "2025", "Overall")))