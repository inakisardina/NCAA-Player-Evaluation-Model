library(data.table)
library(tidyverse)

# Namespace lock for safety (prevents masking issues)
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
rename <- dplyr::rename
summarise <- dplyr::summarise
arrange <- dplyr::arrange

# Set working directory (optional dynamic setup for rendering safety)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
raw_data <- fread("Data/TrackmanCorRegSeasonUltra.csv")
conference_data <- fread("Data/conferences.csv", data.table = FALSE) %>% as_tibble()

# Function to standardize pitch names
clean_pitch_type <- function(pitch) {
  str_to_lower(pitch) %>%
    str_replace_all("fourseamfastball", "four-seam") %>%
    str_replace_all("twoseamfastball|oneseamfastball", "sinker") %>%
    str_replace_all("fastball", "four-seam") %>%
    str_replace_all("changeup", "changeup") %>%
    str_replace_all("curveball", "curveball") %>%
    str_replace_all("slider", "slider") %>%
    str_replace_all("sweeper", "slider") %>%
    str_replace_all("sinker", "sinker") %>%
    str_replace_all("splitter", "changeup") %>%
    str_replace_all("cutter", "cutter") %>%
    str_replace_all("knuckleball", "knuckleball") %>%
    str_replace_all("other|undefined", NA_character_)
}

# Filter for 2024 season, clean pitch type, and assign hand
clean_data <- raw_data %>%
  mutate(
    TaggedPitchType = clean_pitch_type(AutoPitchType),
    Hand = ifelse(PitcherThrows == "Right", "RHP", "LHP")
  )

# Join conference data
pitcher_conferences <- conference_data %>%
  select(Trackman, Conference) %>%
  rename(PitcherTeam = Trackman, PitcherConference = Conference)

batter_conferences <- conference_data %>%
  select(Trackman, Conference) %>%
  rename(BatterTeam = Trackman, BatterConference = Conference)

clean_data <- clean_data %>%
  left_join(pitcher_conferences, by = "PitcherTeam") %>%
  left_join(batter_conferences, by = "BatterTeam")

# Clean PitchCall and PlayResult columns
clean_data <- clean_data %>%
  mutate(
    PitchCall_clean = case_when(
      str_detect(PitchCall, regex("strike", ignore_case = TRUE)) &
        str_detect(PitchCall, regex("swing", ignore_case = TRUE)) ~ "Swinging Strike",
      PitchCall %in% c("InPlay") ~ "In play",
      str_detect(PitchCall, regex("Foul", ignore_case = TRUE)) ~ "Foul",
      str_detect(PitchCall, regex("Strike", ignore_case = TRUE)) ~ "Called Strike",
      str_detect(PitchCall, regex("Ball", ignore_case = TRUE)) &
        !str_detect(PitchCall, regex("Intent", ignore_case = TRUE)) ~ "Ball",
      str_detect(PitchCall, regex("Intent", ignore_case = TRUE)) ~ "Intent Ball",
      str_detect(PitchCall, regex("HitByPitch|HItByPitch", ignore_case = TRUE)) ~ "HBP",
      TRUE ~ "Other"
    ),
    Swing = PitchCall_clean %in% c("Swinging Strike", "Foul", "In play"),
    Take  = !Swing
  ) %>%
  mutate(
    PlayResult_clean = case_when(
      PlayResult %in% c("Single", "SIngle") ~ "Single",
      PlayResult == "Double" ~ "Double",
      PlayResult %in% c("Triple", "triple") ~ "Triple",
      PlayResult %in% c("HomeRun", "Homerun") ~ "HomeRun",
      PlayResult == "Out" ~ "Out",
      PlayResult == "FieldersChoice" ~ "FieldersChoice",
      PlayResult == "Sacrifice" ~ "Sacrifice",
      TRUE ~ NA_character_
    )
  )