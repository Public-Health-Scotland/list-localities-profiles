
library(officer)
library(tidyverse)

add_lp_cover_page <- function(lp_path,cover_path,cover_title,cover_subtitle,cover_date){

  # Load Cover Page and Enter User Info Into Placeholders 
  
  cover_page <- officer::read_docx(cover_path) %>%
    officer::body_replace_all_text("Publication title", cover_title) %>%
    officer::body_replace_all_text("Subtitle", cover_subtitle) %>%
    officer::body_replace_all_text("DD Month YYYY", cover_date)
  
  # Get .docx file name from user file pathway and remove blank spaces and ampersands
  # (block_pour_docx requires no spaces or ampersands)
  
  file_name_no_spaces <- str_split(lp_path,"/") %>%
    unlist() %>%
    last() %>%
    gsub(" ","",.) %>%
    gsub("&","",.)
  
  # Create new file path to .docx file without spaces or ampersands
  
  lp_path_no_spaces <- str_split(lp_path,"/") %>%
    unlist() %>%
    head(x=.,length(.) - 1) %>%
    c(file_name_no_spaces) %>%
    paste0(collapse="/")
  
  # Rename .docx file from name with spaces and ampersands to name without
  
  file.rename(from=lp_path, to=lp_path_no_spaces)
  
  # Converts .docx file to correct format
  
  xml_elt <- officer::to_wml(
    officer::block_pour_docx(lp_path_no_spaces),
    add_ns = TRUE
  )
  
  # Corrects xml_elt for ampersands potentially in file path
  
  if (grepl("&", lp_path_no_spaces)) {
    output_escape <- gsub("&", "&amp;", lp_path_no_spaces)
    xml_elt <- gsub(lp_path_no_spaces, output_escape, xml_elt)
  }
  
  
  # Add Cover Page to document
  cover_page %>%
    officer::cursor_end() %>%
    officer::body_add_break() %>%
    officer::body_add_xml(str = xml_elt) %>%
    officer::set_doc_properties(title = cover_title) %>%
    print(lp_path_no_spaces)
  
  # Rename to original name 
  
  file.rename(from=lp_path_no_spaces, to=lp_path)
  
}


example_pathway <- "/conf/LIST_analytics/Forth Valley/Ryan/Angus North West - Locality Profile.docx"

cover_page_pathway <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/Master RMarkdown Document & Render Code/phs-mngtinfo-cover.docx"

cover_page_title <- "We Are Testing"
cover_page_subtitle <- "It's A Subtitle"
cover_page_date <- format(Sys.Date(),"%d %m %Y")


add_lp_cover_page(example_pathway,cover_page_pathway,cover_page_title,cover_page_subtitle,cover_page_date)

