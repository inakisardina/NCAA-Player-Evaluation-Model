library(rmarkdown)
library(dplyr)

# Load hitter comments
comments_df <- read.csv("Data/hitter_notes.csv", stringsAsFactors = FALSE)

# Ensure output directory exists
output_dir <- "output/reports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List of hitters
prospect_hitters <- c("Vance, Cooper", 
                      "Gupton, Michael", 
                      "Jimenez, Antonio", 
                      "Brooks, Braxton", 
                      "Hull, Owen", 
                      "Moran, Landon", 
                      "Thach, Tanner", 
                      "Canon, Cider", 
                      "Miller, Sam", 
                      "Torres, Cooper", 
                      "Watson, Rex")

# Loop and render individual reports
for (h in prospect_hitters) {
  file_safe_name <- gsub(", ", "_", h)
  
  # Look up custom comment for the pitcher
  comment <- comments_df %>%
    filter(Player == h) %>%
    pull(Comment)
  
  # Default to blank if no match found
  if (length(comment) == 0) comment <- ""
  
  # Render the report with extra_note param
  rmarkdown::render(
    input = "RMD/hitter_report_template.Rmd",
    output_file = paste0("Report_", file_safe_name, ".html"),
    output_dir = output_dir,
    params = list(player = h, extra_note = comment),
    envir = globalenv()
  )
  
  message("✅ HTML report generated for ", h
)}