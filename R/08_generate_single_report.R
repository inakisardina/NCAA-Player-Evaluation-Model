generate_pitcher_report <- function(pitcher_name) {
  # Ensure output folder exists
  if (!dir.exists("output/reports")) {
    dir.create("output/reports", recursive = TRUE)
  }
  
  # Clean up the file name for safe HTML output
  output_file_name <- paste0("Report_", gsub(", ", "_", pitcher_name), ".html")
  
  # Render the RMarkdown report as HTML with pitcher name as parameter
  rmarkdown::render(
    input = "RMD/pitcher_report_template.Rmd",
    output_format = "html_document",  # Explicitly specify HTML
    output_file = output_file_name,
    output_dir = "output/reports",
    params = list(pitcher = pitcher_name),
    envir = globalenv()
  )
  
  message("✅ HTML report created: output/reports/", output_file_name)
}