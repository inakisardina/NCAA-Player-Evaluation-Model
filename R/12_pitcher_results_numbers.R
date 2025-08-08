pitcher_results <- pa_data_pitcher %>%
  mutate(
    # Flag outcomes
    Single = as.integer(PlayResult_clean == "Single"),
    Double = as.integer(PlayResult_clean == "Double"),
    Triple = as.integer(PlayResult_clean == "Triple"),
    HR = as.integer(PlayResult_clean == "HomeRun"),
    Hit = Single + Double + Triple + HR,
    BB = as.integer(KorBB %in% c("Walk", "walk")),
    HBP = as.integer(KorBB == "HitByPitch"),
    K = as.integer(KorBB %in% c("Strikeout", "StrikeOut")),
    Outs_minus_k = as.integer(PlayResult_clean == "Out"),
    IP_numeric = (Outs_minus_k + K) / 3,
    PA = 1
  ) %>%
  group_by(Pitcher, year) %>%
  summarise(
    G = n_distinct(GameID),
    PA = sum(PA, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    HBP = sum(HBP, na.rm = TRUE),
    K = sum(K, na.rm = TRUE),
    Hits = sum(Hit, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    Outs_minus_k = sum(Outs_minus_k, na.rm = TRUE),
    Outs = sum((Outs_minus_k + K), na.rm = TRUE),
    IP_numeric = sum(Outs, na.rm = TRUE) / 3,
    IP_display = paste0(floor(sum(Outs, na.rm = TRUE) / 3), ".", sum(Outs, na.rm = TRUE) %% 3),
    WHIP = ifelse(IP_numeric > 0, round((BB + HBP + Hits) / IP_numeric, 2), NA_real_),
    BB_percent = round(100 * BB / PA, 1),
    K_percent = round(100 * K / PA, 1),
    
    FIP = ifelse(IP_numeric > 0, round(((13 * HR + 3 * (BB + HBP) - 2 * K) / IP_numeric) + 3.1, 2), NA_real_),
    
    .groups = "drop"
  )