# Collapse pitch-level into PA-level data (pitcher-centric)
pa_data_pitcher <- clean_data %>%
  group_by(GameID, Inning, PAofInning, Batter, Pitcher, BatterTeam, PitcherTeam) %>%
  summarise(
    PitcherId = first(PitcherId),
    Pitcher = first(Pitcher),
    PitcherThrows = first(PitcherThrows),
    year = first(year),
    
    # Keep outcomes
    PlayResult_clean = ifelse(any(!is.na(PlayResult_clean)), last(na.omit(PlayResult_clean)), NA_character_),
    KorBB = ifelse(any(KorBB != "Undefined" & !is.na(KorBB)), last(KorBB[KorBB != "Undefined" & !is.na(KorBB)]), NA_character_),
    
    # Batted ball data
    ExitSpeed = last(na.omit(ExitSpeed)),
    Angle = last(na.omit(Angle)),
    
    # Number of pitches
    NumPitches = n(),
    
    .groups = "drop"
  )