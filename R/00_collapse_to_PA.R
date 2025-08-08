# PA adjustments 

library(tidyverse)

# Load cleaned data
data <- clean_data

# Collapse pitch-level into PA-level dataset
pa_data <- data %>%
  group_by(GameID, Inning, PAofInning, Batter, Pitcher, BatterTeam, PitcherTeam) %>%
  summarise(
    # IDs
    BatterId = first(BatterId),
    PitcherId = first(PitcherId),
    BatterSide = first(BatterSide),
    PitcherThrows = first(PitcherThrows),
    PitcherConference = first(PitcherConference),
    BatterConference = first(BatterConference),
    
    # Safely collapse PlayResult_clean
    PlayResult_clean = ifelse(any(!is.na(PlayResult_clean)), last(na.omit(PlayResult_clean)), NA_character_),
    
    # Safely collapse KorBB and treat 'Undefined' as NA
    KorBB = ifelse(any(KorBB != "Undefined" & !is.na(KorBB)), last(KorBB[KorBB != "Undefined" & !is.na(KorBB)]), NA_character_),
    
    # Keep batted ball metrics for later contact models
    ExitSpeed = last(na.omit(ExitSpeed)),
    Angle = last(na.omit(Angle)),
    
    # Number of pitches in PA (for diagnostics only)
    NumPitches = n()
  ) %>%
  ungroup()