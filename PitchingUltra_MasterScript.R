# Pitching Ultra Master Script

# 1. Load and clean data
source("R/01_load_clean_data.R")

# 2. Calculate Pitching+
source("R/02_pitching_plus.R")

# 3. Estimate expected performance
source("R/03_xpitching_plus.R")
#chases

# 4. Classify pitcher types
source("R/04_counts.R")

# 5. Final Pitching Ultra model
source("R/05_pitching_ultra_model.R")

# 6. Generate visualizations
source("R/06_visualizations.R")

# 11 Collapse AB for Pitchers
source("R/11_pitchers_ab_collapse.R")

# 12 Pitcher Results
source("R/12_pitcher_results_numbers.R")

# 7. Generate Leaderboards PDF Reports 
source("R/07_generate_reports.R")

# 8. Generate single report
source("R/08_generate_single_report.R")

# 9 Leader board pitcher reports
source("R/09_generate_leaderboard_reports.R")

# 10 Generate multiple reports from a Vector
source("R/10_vector_reports.R")
