portal <- read_csv("Data/Transfer Portal Players - Pitching.csv")

filtered_portal <- portal %>% filter(IP >= 15)

filtered_portal <- filtered_portal %>%
  mutate(Name = str_trim(Name))

pitching_ultra <- pitching_ultra %>%
  mutate(Pitcher = str_trim(Pitcher))

# 3. Match pitchers by name (inner join)
matched_pitchers <- filtered_portal %>%
  inner_join(pitching_ultra, by = c("Name" = "Pitcher"))

# 4. Select and sort by Pitching_Ultra score
top_pitchers_portal <- matched_pitchers %>%
  arrange(desc(Pitching_Ultra)) %>%
  select(Name, School, Conference, Pitching_Ultra, avg_StuffPlus, avg_CommandPlus, XPitching_plus)

# 5. Show Top 15
print(top_pitchers_portal %>% head(15))