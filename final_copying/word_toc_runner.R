
#Script to be run in R Desktop to run word update on each profiles in order to update the TOC 

library(RDCOMClient)

word <- COMCreate("Word.Application")
word[["Visible"]] <- FALSE
word[["DisplayAlerts"]] <- 0
#word$Options()$UpdateLinksAtOpen <- FALSE

files <- list.files("\\\\Isdsf00d03\\LIST_analytics\\West Hub/02 - Scaled Up Work\\RMarkdown\\Locality Profiles\\Profiles Output", "\\.docx$", full.names = TRUE)

for (f in files) {
  tryCatch({
    doc <- word$Documents()$Open(
      FileName = normalizePath(f),
      ConfirmConversions = FALSE,
      ReadOnly = FALSE,
      AddToRecentFiles = FALSE,
      Revert = TRUE
    )
    
    Sys.sleep(1)
    doc$Fields()$Update()
    doc$Save()
    doc$Close()
    
  }, error = function(e) {
    message("Failed on: ", f)
  })
}

word$Quit()