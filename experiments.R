# Test

draft_pitchers <- pitching_ultra %>% filter(Pitcher %in% c("Doyle, Liam", "Anderson, Kade", "Bremner, Tyler"), (year == "Overall"))


draft_pitchers <- pitching_ultra %>%
  filter(Pitcher %in% c("Doyle, Liam", "Anderson, Kade", "Bremner, Tyler", "Burns, Chase", "Arnold, Jamie",
                        "Witherspoon, Kyson", "Wood, Gage", "Smith, Hagen", "Yesavage, Trey", "Hess, Ben")) %>%
  group_by(Pitcher) %>%
  filter(year == year[which.max(Pitching_Ultra)]) %>%
  ungroup()


table_draft_data <- draft_pitchers %>%
  arrange(desc(Pitching_Ultra)) %>%
  select(Pitcher, year, Pitching_Ultra, Pitching_plus, avg_StuffPlus) %>%
  mutate(year = as.character(year))

kbl(
  table_draft_data,
  format = "html",
  align = c("l", "c", "c", "c", "c"),
  col.names = c("Pitcher", "Year", "Pitching Ultra", "Pitching Plus", "Stuff+"),
  caption = "Best Pitching Ultra Season — 2024 & 2025 First Round Pitchers"
) %>%
  kable_classic(full_width = FALSE, html_font = "Arial") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(3, bold = TRUE, color = "white", background = "#f47422")





library(ggrepel)

# ----- Sliders -----

best_slider_data <- pitching_plus %>%
  filter((TaggedPitchType == "slider"), (Hand == "LHP"), (pitch_count > 100), (Stuff_plus >= 50 & Stuff_plus <= 180))

top5_sliders <- best_slider_data %>%
  arrange(desc(Stuff_plus)) %>%
  slice_head(n = 5)   # use slice_max(Stuff_plus, n = 5, with_ties = FALSE) if you want to break ties

ggplot(best_slider_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_plus)) +
  geom_point(size = 3, alpha = 0.8) +
  # highlight the top 5
  geom_point(data = top5_sliders, size = 4.2, color = "orange") +
  geom_text_repel(
    data = top5_sliders,
    aes(label = Pitcher),
    size = 3.3,
    min.segment.length = 5,
    box.padding = 0.25,
    point.padding = 0.25,
    seed = 42
  ) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 10,  linetype = "dashed", color = "gray50") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Slider Stuff+ Based on Movement Profile",
    subtitle = "Top 5 sliders by Stuff+ labeled with pitcher",
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Stuff+"
  ) +
  theme_minimal()



# ------ Changeups -----

best_changeup_data <- pitching_plus %>%
  filter((TaggedPitchType == "changeup"), (Hand == "LHP"), (pitch_count > 100), (Stuff_plus >= 50 & Stuff_plus <= 180))


top5_changeup <- best_changeup_data %>%
  arrange(desc(Stuff_plus)) %>%
  slice_head(n = 5) 

ggplot(best_changeup_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_plus)) +
  geom_point(size = 3, alpha = 0.8) +
  # highlight the top 5
  geom_point(data = top5_changeup, size = 4.2, color = "orange") +
  geom_text_repel(
    data = top5_changeup,
    aes(label = Pitcher),
    size = 3.3,
    min.segment.length = 5,
    box.padding = 0.25,
    point.padding = 0.25,
    seed = 42
  ) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 10,  linetype = "dashed", color = "gray50") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Changeups Stuff+ Based on Movement Profile",
    subtitle = "Top 5 Changeups by Stuff+ labeled with pitcher",
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Stuff+"
  ) +
  theme_minimal()



# ----- Fastballs -----

best_fastballs_data <- pitching_plus %>%
  filter((TaggedPitchType == "four-seam"), (Hand == "RHP"), (pitch_count > 200), (Stuff_plus >= 50 & Stuff_plus <= 180))


top5_fastballs <- best_fastballs_data %>%
  arrange(desc(Stuff_plus)) %>%
  slice_head(n = 5) 

ggplot(best_fastballs_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_plus)) +
  geom_point(size = 3, alpha = 0.8) +
  # highlight the top 5
  geom_point(data = top5_fastballs, size = 4.2, color = "orange") +
  geom_text_repel(
    data = top5_fastballs,
    aes(label = Pitcher),
    size = 3.3,
    min.segment.length = 5,
    box.padding = 0.25,
    point.padding = 0.25,
    seed = 42
  ) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 10,  linetype = "dashed", color = "gray50") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Fastball Stuff+ Based on Movement Profile",
    subtitle = "Top 5 Fastballs by Stuff+ labeled with pitcher",
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Stuff+"
  ) +
  theme_minimal()

# ----- Sinkers -----


best_sinker_data <- pitching_plus %>%
  filter((TaggedPitchType == "sinker"), (Hand == "LHP"), (pitch_count > 100), (Stuff_plus >= 50 & Stuff_plus <= 180))


top5_sinker <- best_sinker_data %>%
  arrange(desc(Stuff_plus)) %>%
  slice_head(n = 5) 

ggplot(best_sinker_data, aes(x = avg_HorzBrk, y = avg_VertBrk, color = Stuff_plus)) +
  geom_point(size = 3, alpha = 0.8) +
  # highlight the top 5
  geom_point(data = top5_sinker, size = 4.2, color = "orange") +
  geom_text_repel(
    data = top5_sinker,
    aes(label = Pitcher),
    size = 3.3,
    min.segment.length = 5,
    box.padding = 0.25,
    point.padding = 0.25,
    seed = 42
  ) +
  geom_vline(xintercept = -10, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 10,  linetype = "dashed", color = "gray50") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(
    title = "Sinkers Stuff+ Based on Movement Profile",
    subtitle = "Top 5 Sinkers by Stuff+ labeled with pitcher",
    x = "Horizontal Break (in)",
    y = "Vertical Break (in)",
    color = "Stuff+"
  ) +
  theme_minimal()









# og stuff +

evaluate_slider_stuff <- function(Velo_z, HB_z, IVB_z, Spin_z, avg_HorzBrk, avg_VertBrk) {
  is_sweeper <- avg_HorzBrk < -10 & avg_VertBrk < 10
  sweeper_val <- (0.89 * HB_z + 0.05 * Velo_z - 0.05 * IVB_z + 0.01 * Spin_z) * 100 + 100
  gyro_val <- (0.3 * Velo_z - 0.3 * IVB_z + 0.3 * Spin_z + 0.1 * HB_z) * 100 + 100
  ifelse(is_sweeper, sweeper_val, gyro_val)
}

pitching_plus <- pitcher_stuff %>%
  left_join(league_stats, by = c("TaggedPitchType", "Hand", "year")) %>%
  mutate(
    HB_gs = case_when(
      Hand == "RHP" & TaggedPitchType %in% c("slider", "cutter") ~ -avg_HorzBrk,
      Hand == "LHP" & TaggedPitchType %in% c("slider", "cutter") ~  avg_HorzBrk,
      TRUE ~ avg_HorzBrk
    ),
    league_HB_gs = case_when(
      Hand == "RHP" & TaggedPitchType %in% c("slider", "cutter") ~ -league_HorzBrk,
      Hand == "LHP" & TaggedPitchType %in% c("slider", "cutter") ~  league_HorzBrk,
      TRUE ~ league_HorzBrk
    ),
    Velo_z = (avg_Vel - league_Vel) / league_Vel,
    HB_z = (HB_gs - league_HB_gs) / sd_HorzBrk,
    IVB_z = (avg_VertBrk - league_VertBrk) / sd_VertBrk,
    Spin_z = (avg_SpinRate - league_SpinRate) / sd_SpinRate
  ) %>%
  rowwise() %>%
  mutate(
    Stuff_plus = case_when(
      TaggedPitchType == "four-seam" ~ (0.5 * Velo_z + 0.4 * IVB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "sinker"    ~ (0.4 * Velo_z + 0.4 * HB_z - 0.3 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "slider"    ~ evaluate_slider_stuff(Velo_z, HB_z, IVB_z, Spin_z, avg_HorzBrk, avg_VertBrk),
      TaggedPitchType == "curveball" ~ (0.3 * Velo_z - 0.6 * IVB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "cutter"    ~ (0.4 * Velo_z + 0.3 * IVB_z + 0.2 * HB_z + 0.1 * Spin_z) * 100 + 100,
      TaggedPitchType == "changeup"  ~ (0.4 * Velo_z + 0.3 * HB_z - 0.2 * IVB_z - 0.1 * Spin_z) * 100 + 100,
      TRUE ~ NA_real_
    ),
    Stuff_plus = round(Stuff_plus, 1)
  ) %>%
  ungroup() %>%
  left_join(pitcher_command %>% select(Pitcher, TaggedPitchType, year, Command_plus),
            by = c("Pitcher", "TaggedPitchType", "year")) %>%
  left_join(exit_velocity, by = c("Pitcher", "TaggedPitchType", "year"))