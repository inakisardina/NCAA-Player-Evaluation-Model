# Generate hitter report

generate_hitter_report <- function(hitter_name) {
  # Create output directory if it doesn't exist
  if (!dir.exists("output/reports")) {
    dir.create("output/reports", recursive = TRUE)
  }
  
  # Clean up file name for safe HTML output
  output_file_name <- paste0("Report_", gsub(", ", "_", hitter_name), ".html")
  
  # Render the RMarkdown report as HTML with hitter name as parameter
  rmarkdown::render(
    input = "RMD/hitter_report_template.Rmd",
    output_format = "html_document", 
    output_file = output_file_name,
    output_dir = "output/reports",
    params = list(player = hitter_name),
    envir = globalenv()
  )
  
  message("Report generated: output/reports/", output_file_name)
}