library(rmarkdown)
library(dplyr)

# Load pitcher comments
comments_df <- read.csv("Data/notes3.csv", stringsAsFactors = FALSE)

# Ensure output directory exists
output_dir <- "output/reports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List of pitchers
prospect_pitchers <- c("Kling, Corey",
                       "Kalkbrenner, Craig",
                       "Colucci, Joey",
                       "Smith, Kaden",
                       "Heyl, Matthew",
                       "Crowther, Ashton",
                       "Hess, Tate",
                       "Purcell, Brodie",
                       "Cushing, Stone",
                       "Chicoli, Connor",
                       "Smith, Dylan",
                       "Combs, Ryan",
                       "Mendes, Max",
                       "Bilka, Ryan",
                       "Guentz, Braden",
                       "Crane, Wayland",
                       "Volchko, Joey",
                       "Pintar, Luka",
                       "Dean, Ben")

# Loop and render individual reports
for (p in prospect_pitchers) {
  file_safe_name <- gsub(", ", "_", p)
  
  # Look up custom comment for the pitcher
  comment <- comments_df %>%
    filter(Pitcher == p) %>%
    pull(Comment)
  
  # Default to blank if no match found
  if (length(comment) == 0) comment <- ""
  
  # Render the report with extra_note param
  rmarkdown::render(
    input = "RMD/pitcher_report_template.Rmd",
    output_file = paste0("Report_", file_safe_name, ".html"),
    output_dir = output_dir,
    params = list(pitcher = p, extra_note = comment),
    envir = globalenv()
  )
  
  message("✅ HTML report generated for ", p)
}