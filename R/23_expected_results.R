library(tidyverse)

# Load PA-level data
data <- pa_data

# Filter to batted ball events
batted_balls <- data %>%
  filter(!is.na(ExitSpeed), !is.na(Angle), !is.na(PlayResult_clean)) %>%
  mutate(
    Single = as.integer(PlayResult_clean == "Single"),
    Double = as.integer(PlayResult_clean == "Double"),
    Triple = as.integer(PlayResult_clean == "Triple"),
    HR = as.integer(PlayResult_clean == "HomeRun"),
    Hit = Single + Double + Triple + HR,
    TB = Single + 2*Double + 3*Triple + 4*HR,
    wOBA_contrib = 0.89*Single + 1.28*Double + 1.63*Triple + 2.10*HR
  )

# Create EV / LA bins
batted_balls <- batted_balls %>%
  mutate(
    EV_bin = round(ExitSpeed),
    LA_bin = round(Angle)
  )

# Build EV-LA grid table
evla_table <- batted_balls %>%
  group_by(EV_bin, LA_bin) %>%
  summarise(
    n = n(),
    wOBA_avg = mean(wOBA_contrib, na.rm = TRUE),
    Hits = sum(Hit),
    TB = sum(TB),
    BBE = n(),
    SLG_avg = TB / BBE  # true slugging % for bin
  ) %>%
  ungroup()

# Merge back into batted_balls
batted_balls <- batted_balls %>%
  left_join(evla_table %>% select(EV_bin, LA_bin, wOBA_avg, SLG_avg), by = c("EV_bin", "LA_bin")) %>%
  rename(xwOBA = wOBA_avg, xSLG = SLG_avg)

# Aggregate expected results by hitter
expected_results <- batted_balls %>%
  group_by(Batter) %>%
  summarise(
    BBE = n(),
    avg_EV = mean(ExitSpeed),
    avg_LA = mean(Angle),
    xwOBA = mean(xwOBA, na.rm = TRUE),
    xSLG = mean(xSLG, na.rm = TRUE)
  )

