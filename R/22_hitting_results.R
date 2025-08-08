# Results

library(tidyverse)

# Load collapsed PA-level data
# Assume pa_data has already been created from 00_collapse_to_PA.R

# Build proper outcome flags with NA protection
pa_results <- pa_data %>%
  mutate(
    Single = as.integer(PlayResult_clean == "Single"),
    Double = as.integer(PlayResult_clean == "Double"),
    Triple = as.integer(PlayResult_clean == "Triple"),
    HR = as.integer(PlayResult_clean == "HomeRun"),
    Sacrifice = as.integer(PlayResult_clean == "Sacrifice"),
    FieldersChoice = as.integer(PlayResult_clean == "FieldersChoice"),
    Out = as.integer(PlayResult_clean == "Out" | PlayResult_clean == "FieldersChoice"),
    Hit = Single + Double + Triple + HR,
    TB = Single + 2*Double + 3*Triple + 4*HR,
    BB = as.integer(KorBB == "Walk"),
    HBP = as.integer(KorBB == "HitByPitch"),
    K = as.integer(KorBB == "Strikeout"),
    PA = 1
  )

# Aggregate hitter results fully protected against NA
hitter_results <- pa_results %>%
  group_by(Batter) %>%
  summarise(
    PA = sum(PA, na.rm=TRUE),
    AB = PA - sum(BB, na.rm=TRUE) - sum(HBP, na.rm=TRUE) - sum(Sacrifice, na.rm=TRUE),
    Hits = sum(Hit, na.rm=TRUE),
    HR = sum(HR, na.rm=TRUE),
    Double = sum(Double, na.rm=TRUE),
    Triple = sum(Triple, na.rm=TRUE),
    BB = sum(BB, na.rm=TRUE),
    HBP = sum(HBP, na.rm=TRUE),
    K = sum(K, na.rm=TRUE),
    TB = sum(TB, na.rm=TRUE),
    AVG = Hits / AB,
    SLG = TB / AB,
    ISO = SLG - AVG,
    OBP = (Hits + BB + HBP) / PA,
    wOBA_numerator = (0.69*BB + 0.72*HBP + 0.89*(Hits - (Double + Triple + HR)) +
                        1.28*Double + 1.63*Triple + 2.10*HR),
    wOBA = wOBA_numerator / PA
  )

# League averages
league <- hitter_results %>% summarise(
  league_wOBA = sum(wOBA * PA, na.rm=TRUE) / sum(PA, na.rm=TRUE),
  league_OBP = sum(OBP * PA, na.rm=TRUE) / sum(PA, na.rm=TRUE),
  league_ISO = sum(ISO * PA, na.rm=TRUE) / sum(PA, na.rm=TRUE)
)

# Create normalized plus metrics
hitter_results <- hitter_results %>%
  mutate(
    wOBA_Plus = 100 * wOBA / league$league_wOBA,
    OBP_Plus = 100 * OBP / league$league_OBP,
    ISO_Plus = 100 * ISO / league$league_ISO,
    Results_Plus = 0.6 * wOBA_Plus + 0.15 * OBP_Plus + 0.25 * ISO_Plus
  )