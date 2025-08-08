library(rmarkdown)

# Create the output folder if it doesn't exist
if (!dir.exists("output/reports")) {
  dir.create("output/reports", recursive = TRUE)
}

# Render the leaderboard report as HTML
rmarkdown::render(
  input = "RMD/leaderboard_report.Rmd",
  output_format = "html_document",  # Explicitly specify HTML
  output_file = "PitchingUltra_Leaderboard.html",  # .html extension
  output_dir = "output/reports",
  envir = globalenv()
)