library(ggplot2)
library(ggforce)
library(dplyr)

# === Parameters ===
target_pitcher <- "Onorato, Daniel"
target_year <- "2025"  # Use "2024", "2025", or "Overall"

# === Filter pitch-level data ===
pitcher_data <- clean_data %>%
  filter(Pitcher == target_pitcher, !is.na(TaggedPitchType)) %>%
  filter(if (target_year != "Overall") year == target_year else TRUE)

# === Pitch Usage Bar Chart ===
pitch_usage <- pitcher_data %>%
  group_by(TaggedPitchType) %>%
  summarise(pitch_count = n(), .groups = "drop") %>%
  filter(pitch_count >= 5) %>%
  mutate(pct = round(pitch_count / sum(pitch_count) * 100, 1))

ggplot(pitch_usage, aes(x = reorder(TaggedPitchType, -pct), y = pct, fill = TaggedPitchType)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3) +
  labs(
    title = paste("Pitch Usage –", target_pitcher, ifelse(target_year != "Overall", paste("(", target_year, ")"), "")),
    x = "Pitch Type", y = "Usage (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# === Movement vs League Averages ===
league_movement <- clean_data %>%
  filter(!is.na(TaggedPitchType), !is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
  group_by(TaggedPitchType) %>%
  summarise(
    avg_HorzBrk = mean(HorzBreak, na.rm = TRUE),
    avg_VertBrk = mean(InducedVertBreak, na.rm = TRUE),
    .groups = "drop"
  )

pitcher_mov <- pitcher_data %>%
  filter(!is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
  group_by(TaggedPitchType) %>%
  summarise(
    avg_HorzBrk = mean(HorzBreak, na.rm = TRUE),
    avg_VertBrk = mean(InducedVertBreak, na.rm = TRUE),
    pitch_count = n(),
    .groups = "drop"
  ) %>%
  filter(pitch_count >= 5) %>%
  mutate(usage_pct = pitch_count / sum(pitch_count))

ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 12), color = "lightgray", linetype = "dotted") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 24), color = "lightgray", linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = league_movement, aes(x = avg_HorzBrk, y = avg_VertBrk, color = TaggedPitchType), size = 3, alpha = 0.25) +
  geom_point(data = pitcher_mov, aes(x = avg_HorzBrk, y = avg_VertBrk, color = TaggedPitchType, size = usage_pct * 100)) +
  scale_size_continuous(name = "Usage %") +
  coord_fixed(xlim = c(-25, 25), ylim = c(-25, 25)) +
  labs(
    title = paste("Movement vs League –", target_pitcher, ifelse(target_year != "Overall", paste("(", target_year, ")"), "")),
    x = "Horizontal Break", y = "Vertical Break"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# === Strike Zone Scatter Plot ===
ggplot(pitcher_data %>% filter(TaggedPitchType %in% pitcher_mov$TaggedPitchType),
       aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  geom_point(alpha = 0.5) +
  geom_rect(aes(xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5),
            fill = NA, color = "black", linetype = "dashed", size = 1) +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  labs(
    title = paste("Strike Zone Plot –", target_pitcher, ifelse(target_year != "Overall", paste("(", target_year, ")"), "")),
    x = "Plate X", y = "Plate Z", color = "Pitch Type"
  )

# === Pitch Heatmaps (density) by Pitch Type ===
ggplot(pitcher_data %>% filter(TaggedPitchType %in% pitcher_mov$TaggedPitchType),
       aes(x = PlateLocSide, y = PlateLocHeight)) +
  geom_density_2d_filled(alpha = 0.8) +
  geom_rect(aes(xmin = -0.95, xmax = 0.95, ymin = 1.5, ymax = 3.5),
            fill = NA, color = "black", linetype = "dashed") +
  facet_wrap(~ TaggedPitchType, ncol = 3) +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  labs(
    title = paste("Heatmaps by Pitch –", target_pitcher, ifelse(target_year != "Overall", paste("(", target_year, ")"), "")),
    x = "Plate X", y = "Plate Z", fill = "Density"
  )

# === Stuff+ vs XPitching+ Comparison ===
target_ultra <- pitching_ultra %>%
  filter(Pitcher == target_pitcher, if (target_year != "Overall") year == target_year else TRUE)

ggplot(target_ultra, aes(x = avg_StuffPlus, y = XPitching_plus, label = Pitcher)) +
  geom_point(color = "steelblue", size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 11) +
  labs(
    title = paste("Stuff+ vs XPitching+", ifelse(target_year != "Overall", paste("(", target_year, ")"), "")),
    x = "Stuff+ (What Happened)",
    y = "XPitching+ (What Should Have Happened)"
  ) +
  xlim(80, 140) + ylim(80, 140)