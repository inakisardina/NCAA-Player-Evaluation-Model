# Output leaderboard (Top 20 Eye+)
hitter_pitchtype_decision_summary %>%
  arrange(desc(Eye_Plus)) %>%
  head(20) %>%
  print()

# Visualization: swing decisions by pitch type
example_hitter <- "Ogden, Jake"  # replace with actual hitter name

plot_data <- data %>% filter(Batter == example_hitter)

ggplot(plot_data, aes(x = PlateX, y = PlateZ, color = Decision)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_rect(aes(xmin = zone_left, xmax = zone_right, ymin = zone_bottom, ymax = zone_top),
            fill = NA, color = "black", linetype = "dashed") +
  scale_color_manual(values = c(
    "Correct Swing" = "green",
    "Passive" = "yellow",
    "Chase" = "red",
    "Correct Take" = "blue"
  )) +
  coord_fixed() +
  facet_wrap(~ TaggedPitchType, ncol = 3) +
  theme_minimal() +
  labs(title = paste("Swing Decisions by Pitch Type for", example_hitter))
chases_data <- data %>%
  filter(ZoneFlag == "OutZone", Swing_numeric == 1)

ogden <- hitting_ultra %>% filter(Batter == "Ogden, Jake")
cuvet <- hitting_ultra %>% filter(Batter == "Cuvet, Daniel")
galvin <- hitting_ultra %>% filter(Batter == "Galvin, Max")
tanner <- hitting_ultra %>%  filter(Batter == "Smith, Tanner")
arquette <- hitting_ultra %>%  filter(Batter == "Arquette, Aiva")
laviolette <- hitting_ultra %>%  filter(Batter == "LaViolette, Jace")
evans <- hitting_ultra %>%  filter(Batter == "Evans, Ty")
houston <- hitting_ultra %>%  filter(Batter == "Houston, Marek")
marsh <- hitting_ultra %>%  filter(Batter == "Marsh, Bobby")



# Plot location vs chase severity
ggplot(chases_data, aes(x = PlateX, y = PlateZ, color = log1p(ChaseSeverity))) +
  geom_point(size = 2, alpha = 0.8) +
  geom_rect(aes(xmin = zone_left, xmax = zone_right, ymin = zone_bottom, ymax = zone_top),
            fill = NA, color = "black", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red") +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Chase Severity by Location (Log Scale)",
    subtitle = "Log1p transform improves separation",
    x = "Plate Side (X)", y = "Plate Height (Z)", color = "Log(Severity+1)"
  )


library(scales)

ggplot(chases_data, aes(x = PlateX, y = PlateZ, color = ChaseSeverity)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_rect(aes(xmin = zone_left, xmax = zone_right, ymin = zone_bottom, ymax = zone_top),
            fill = NA, color = "black", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red", limits = c(0, 5), oob = squish) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Chase Severity by Location (Capped Scale)",
    subtitle = "Values capped at 5 to improve contrast",
    x = "Plate Side (X)", y = "Plate Height (Z)", color = "Severity"
  )


leaderboard <- hitting_ultra %>%
  filter(PA > 80) %>% 
  arrange(desc(Hitting_Ultra)) %>%
  select(
    Batter, 
    PA, 
    Results_Plus, 
    Eye_Plus, 
    Contact_Plus, 
    xwOBA_Plus, 
    Battle_Plus, 
    Hitting_Ultra
  )

# Print top 30
leaderboard %>% print(n = 30)


bb_data <- hitter_results %>%
  select(Batter, PA, BB) %>%
  mutate(BB_rate = BB / PA)

eye_vs_walks <- bb_data %>%
  left_join(eye_scores, by = "Batter")

correlation <- cor(eye_vs_walks$Eye_Plus, eye_vs_walks$BB_rate, use = "complete.obs")
print(correlation)

ggplot(eye_vs_walks, aes(x = Eye_Plus, y = BB_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Eye+ vs BB%",
    x = "Eye+",
    y = "BB Rate"
  ) +
  theme_minimal()



bb_ev_data <- clean_data %>%
  filter(Batter == params$player, 
         !is.na(PlateLocSide), 
         !is.na(PlateLocHeight), 
         !is.na(ExitSpeed)) %>%
  mutate(PlateX = PlateLocSide, PlateZ = PlateLocHeight)

p <- ggplot(bb_ev_data, aes(x = PlateX, y = PlateZ, group = 1)) +
  geom_density_2d_filled(aes(weight = ExitSpeed), contour_var = "ndensity", n = 200, alpha = 0.8) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5),
            fill = NA, color = "black", linetype = "dashed", size = 1) +
  facet_wrap(~ TaggedPitchType, ncol = 3) +
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  ) +
  labs(
    title = paste("Exit Velo Zones –", params$player),
    x = "Horizontal Plate Location (ft)",
    y = "Vertical Plate Location (ft)",
    fill = "Weighted Density"
  )

print(p)