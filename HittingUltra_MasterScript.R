# Hitting ultra Master Script

# what makes a good hitter? good discipline, hits mistakes, does damage, results, hits the ball hard. 
# Important notes: Splits, tendencies, 

# Working Directory
setwd("/Users/kaks/Desktop/CanesBaseball/PitchingUltra")


# 1. Load and clean data
source("R/01_load_clean_data.R")

# 2. Calculate eye+
source("R/20_swing_decisions.R")

# 3. Contact Quality
source("R/21_contact_quality.R")

# 3.5 PA adjustments
source("R/00_collapse_to_PA.R")

#4. Results 
source("R/22_hitting_results.R")

#5. Expected Results
source("R/23_expected_results.R")

#6. vs Stuff+
source("R/24_battles.R")

#7. Hitting Ultra Model 
source("R/25_hitting_ultra_model.R")

#8. Generate hitting report
source("R/26_generate_hitter_report.R")

#9. Multiple Reports
source("R/27_vector_hitters.R")

cuvet <- hitting_ultra %>% filter(Batter == "Cuvet, Daniel")