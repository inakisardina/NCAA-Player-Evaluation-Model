hugus <- pitching_ultra %>% filter(Pitcher == "Hugus, Griffin")
doyle <- count_behavior_summary_by_year %>% filter(Pitcher == "Doyle, Liam")
anderson <- most_used_pitch_by_count %>% filter(Pitcher == "Anderson, Kade")
arnold <- most_used_pitch_by_count %>% filter(Pitcher == "Arnold, Jamie")
with <- most_used_pitch_by_count %>% filter(Pitcher == "Witherspoon, Kyson")
bremner <- most_used_pitch_by_count %>% filter(Pitcher == "Bremner, Tyler")
wood <- most_used_pitch_by_count %>% filter(Pitcher == "Wood, Gage")
forbes <- most_used_pitch_by_count %>%  filter(Pitcher == "Forbes, Patrick")
quick <- most_used_pitch_by_count %>%  filter(Pitcher == "Quick, Riley")
eyanson <- most_used_pitch_by_count %>% filter(Pitcher == "Eyanson, Anthony")


pitcher_name <- "Doyle, Liam"

viz_data <- clean_data %>%
  filter(!is.na(PlateLocSide), !is.na(PlateLocHeight), !is.na(TaggedPitchType), !is.na(PitcherThrows)) %>%
  mutate(
    TaggedPitchType = tolower(TaggedPitchType),
    is_LHP = PitcherThrows == "Left",
    is_competitive = PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
      PlateLocHeight >= 1.2 & PlateLocHeight <= 4.0,
    
    location_rating = case_when(
      # Ideal zones by pitch type
      TaggedPitchType == "four-seam" &
        (PlateLocHeight >= 3.2 | (abs(PlateLocSide) >= 0.7 & abs(PlateLocSide) <= 0.95)) &
        is_competitive ~ "ideal",
      
      TaggedPitchType == "sinker" &
        PlateLocHeight <= 2.2 & is_competitive ~ "ideal",
      
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
      
      # Bad: way outside or heart of the plate
      PlateLocSide < -1.6 | PlateLocSide > 1.6 |
        PlateLocHeight < 1.0 | PlateLocHeight > 4.2 ~ "bad",
      
      abs(PlateLocSide) <= 0.3 & PlateLocHeight >= 2.2 & PlateLocHeight <= 3.2 ~ "bad",
      
      # Non-ideal fallback
      TRUE ~ "non-ideal"
    )
  )

# Plot — Faceted by pitch type (optional: filter for one pitcher)
ggplot(viz_data %>% filter(Pitcher == "Fink, Carter"), 
       aes(x = PlateLocSide, y = PlateLocHeight, fill = location_rating)) +
  geom_point(alpha = 0.7, shape = 21, color = "black", size = 2) +
  geom_rect(aes(xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5), 
            fill = NA, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("ideal" = "darkgreen", "non-ideal" = "gold", "bad" = "red")) +
  facet_wrap(~ TaggedPitchType) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Pitch Command Classification by Type",
    x = "Plate X (ft)", y = "Plate Z (ft)",
    fill = "Location Rating"
  )

"Bradley-Cooney, Packy"
hugus <- pitcher_s %>% filter(Pitcher == "Hugus, Griffin")
doyle <- count_behavior_summary %>% filter(Pitcher == "Doyle, Liam")
fink <- pitching_ultra %>% filter(Pitcher == "Fink, Carter")
Cooney <- clean_data %>% filter(Pitcher == "Bradley-Cooney, Packy")

pitching_plus <- pitcher_stuff %>%
  left_join(league_stats, by = c("TaggedPitchType", "Hand")) %>%
  mutate(
    Velo_z = (avg_Vel - league_Vel) / league_Vel,
    HB_z = -(avg_HorzBrk - league_HorzBrk) / sd_HorzBrk,
    IVB_z = -(avg_VertBrk - league_VertBrk) / sd_VertBrk,
    Spin_z = -(avg_SpinRate - league_SpinRate) / sd_SpinRate,  # you'll need to define this in league_stats
    
    Stuff_plus = case_when(
      TaggedPitchType == "four-seam" ~ round((Velo_z * 0.5 + IVB_z * 0.4 + HB_z * 0.1) * 100 + 100, 1),
      TaggedPitchType == "sinker"    ~ round((Velo_z * 0.4 + HB_z * 0.4 + IVB_z * 0.2) * 100 + 100, 1),
      TaggedPitchType == "cutter"    ~ round((Velo_z * 0.4 + HB_z * 0.4 + IVB_z * 0.2) * 100 + 100, 1),
      TaggedPitchType == "slider"    ~ round((HB_z * 0.5 + IVB_z * 0.4 + Velo_z * 0.1) * 100 + 100, 1),
      TaggedPitchType == "curveball" ~ round((IVB_z * 0.4 + HB_z * 0.4 + Velo_z * 0.2) * 100 + 100, 1),
      TaggedPitchType == "changeup"  ~ round((HB_z * 0.4 + IVB_z * 0.2 + Velo_z * 0.1 + Spin_z * 0.3) * 100 + 100, 1),
      TRUE                           ~ round((Velo_z * 0.4 + HB_z * 0.3 + IVB_z * 0.3) * 100 + 100, 1)
    )
  )


slider_data <- pitching_plus %>%
  filter(TaggedPitchType == "slider") %>%
  mutate(
    Stuff_calc = evaluate_slider_stuff(Velo_z, HB_z, IVB_z, Spin_z, avg_HorzBrk, avg_VertBrk),
    slider_type = ifelse(avg_HorzBrk < -10 & avg_VertBrk < 10, "sweeper", "gyro")
  )
library(ggplot2)

ggplot(slider_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_calc)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray50") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Slider Stuff+ Based on Movement Profile",
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Stuff+"
  ) +
  theme_minimal()

ggplot(slider_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = slider_type)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("sweeper" = "steelblue", "gyro" = "firebrick")) +
  labs(title = "Slider Classification by Movement",
       x = "Horizontal Break (in)", y = "Vertical Break (in)",
       color = "Slider Type") +
  theme_minimal()

top_sliders <- slider_data %>%
  arrange(desc(Stuff_calc)) %>%
  slice_head(n = 10)

ggplot(slider_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_calc)) +
  geom_point(alpha = 0.7) +
  geom_text(data = top_sliders, aes(label = Pitcher), vjust = -1, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = -10, linetype = "dashed") +
  geom_hline(yintercept = 10, linetype = "dashed") +
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal()

elite_sweepers <- pitching_plus %>% filter(TaggedPitchType == "slider") %>% filter(Hand == "LHP")

library(ggforce)

test_pitcher <- "Anderson, Kade"
pitcher_mov <- pitching_plus %>%
  filter(Pitcher == test_pitcher)

ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 12), color = "lightgray", linetype = "dotted") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 24), color = "lightgray", linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = pitcher_mov, 
             aes(x = avg_HorzBrk, y = avg_VertBrk, color = TaggedPitchType), 
             size = 5) +
  
  coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
  labs(
    title = paste("Pitch Movement –", test_pitcher),
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Pitch Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Clean Duplicates
clean_data <- clean_data %>%
  mutate(
    Pitcher = ifelse(
      Pitcher == "Fink, Carter" & PitcherTeam == "NIA_EAG",
      "Fink, Carter (NIA)",
      Pitcher
      )
 )



look <- shortlist %>% filter(Pitcher %in% c("Moffett, Aiden",
                                       "Rodriguez, Ryne",
                                       "Behnke, Andrew",
                                       "Poole, Owen",
                                       "Ficklin, Nolan",
                                       "Garcia, Santiago",
                                       "Giordano, Jack",
                                       "Jackson, Luke",
                                       "Hunley, Austin",
                                       "Purcell, Brodie"))


ggplot(count_summary %>% filter(Pitcher == "Hugus, Griffin"),
       aes(x = Count, y = WhiffRate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Whiff Rate by Count – Hugus, Griffin",
       x = "Count", y = "Whiff Rate") +
  theme_minimal()

ggplot(count_summary, aes(x = CountGroup, y = WhiffRate, fill = CountGroup)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Whiff Rate Comparison: 2-Strike vs. Hitter Counts",
    x = "Count Group",
    y = "Whiff Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(doyle, aes(x = FPS_percent, y = K_Rate_Behind)) +
  geom_point() +
  labs(
    title = "First-Pitch Strike % vs K Rate After Falling Behind",
    x = "First-Pitch Strike %",
    y = "K After Falling Behind"
  ) +
  theme_minimal()