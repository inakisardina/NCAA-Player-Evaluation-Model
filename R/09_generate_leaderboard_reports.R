library(rmarkdown)
library(dplyr)

# Ensure output directory exists
output_dir <- "output/reports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get top pitchers from the leaderboard (Script 07 output)
matched_pitchers <- pitching_ultra %>%
  filter(total_pitches >= 100) %>%
  arrange(desc(Pitching_Ultra)) %>%
  slice_head(n = 15) %>%  # Adjust number of reports here
  pull(Pitcher)

# Loop and render an individual HTML for each pitcher
for (p in matched_pitchers) {
  file_safe_name <- gsub(", ", "_", p)
  
  rmarkdown::render(
    input = "RMD/pitcher_report_template.Rmd",
    output_format = "html_document",  # Specify HTML output
    output_file = paste0("Report_", file_safe_name, ".html"),
    output_dir = output_dir,
    params = list(pitcher = p),
    envir = globalenv()
  )
  
  message("✅ HTML report generated for ", p)
}