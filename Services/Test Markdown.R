source("Master RMarkdown Document & Render Code/Global Script.R")

return_rows <- function(x) {
  n <- sample(x, 1)
  return(x == n)
}

randomly_selected_localities <- read_in_localities() %>%
  group_by(hb2019name) %>%
  filter(return_rows(hscp_locality)) %>%
  ungroup() %>%
  pull(hscp_locality) %>%
  unique()

for (Loc in randomly_selected_localities) {
  rmarkdown::render(
    "Services/Services-Testing-Markdown.Rmd",
    output_file = paste0("Services-Testing-Markdown-", Loc, ".docx"),
    output_dir = "Services",
    knit_root_dir = rstudioapi::getActiveProject(),
    envir = new.env()
  )
}


# USE TO DELETE TEST DOCUMENTS

list.files("Services", pattern = "*.docx", full.names = TRUE) %>%
  unlink()
