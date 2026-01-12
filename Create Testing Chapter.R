library(knitr)
library(bookdown)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Enter Rmarkdown file of Chapter(s) You Wish To Test
# More than 1 can be entered at a time (c() format)

chapter_oi <- c("Demographics.Rmd")
LOCALITY <- "Falkirk West"

# Output Directory
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
output_dir <- path(lp_path, "Profiles Output")

# Create Test Chapter
create_testing_chapter(chapter_oi, LOCALITY, output_dir)
