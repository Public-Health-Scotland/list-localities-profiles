# It makes most sense to 'source' this script by clicking 'source' (top-right)
# or Ctrl + Shift + S You can run it line by line if desired

# It interactively asks for the 'Final Profiles' year
# It then shows how many files will be copied
# If overwriting is required it will ask for additional specific approval.

source(file.path("Master RMarkdown Document & Render Code", "Global Script.R"))
library(cli)

# Release details ----
cli_h1("Copy final Locality Profiles")
options(cli.progress_show_after = 0)

year <- year(Sys.Date())
release <- readline(glue("Release [{year}]: "))
if (!nzchar(release)) {
  release <- year
}

final_dir <- path(lp_path, "Final Profiles", paste0(release, " Final Profiles"))
background_dir <- path(
  lp_path,
  "Master RMarkdown Document & Render Code",
  "Output",
  "background data"
)

# Choose the Word profiles folder (e.g. Profiles Output Apr26).
profile_dir <- path(lp_path, "Profiles Output")

if (!dir_exists(profile_dir)) {
  cli_abort("No Profiles Output folder found.")
}

# Find and match files ----
cli_h2("Find completed files")

lookup <- read_in_localities() |>
  mutate(
    hscp_locality = if_else(
      hscp_locality == "Dumbarton/Alexandria",
      "Dumbarton-Alexandria",
      hscp_locality
    )
  ) |>
  distinct(hscp_locality, hscp2019name)

word_files <- tibble(
  path = dir_ls(
    profile_dir,
    type = "file",
    regexp = " - Locality Profile[.]docx$"
  )
) |>
  mutate(
    file_name = path_file(path),
    hscp_locality = str_remove(file_name, " - Locality Profile[.]docx$"),
    type = rep("Word profiles", n())
  ) |>
  filter(!startsWith(file_name, "~$")) |>
  left_join(lookup, by = join_by(hscp_locality))

excel_files <- tibble(
  path = dir_ls(
    background_dir,
    type = "file",
    regexp = " - Locality Profile (SDC highlight|data)[.]xlsx$"
  )
) |>
  mutate(
    file_name = path_file(path),
    hscp2019name = str_remove(
      file_name,
      " - Locality Profile (SDC highlight|data)[.]xlsx$"
    ),
    type = if_else(
      str_ends(file_name, " SDC highlight.xlsx"),
      "SDC workbooks",
      "Background data workbooks"
    )
  ) |>
  filter(!startsWith(file_name, "~$"))

files <- bind_rows(word_files, excel_files) |>
  filter(!is.na(hscp2019name), hscp2019name %in% lookup$hscp2019name) |>
  mutate(
    new_dir = path(final_dir, hscp2019name),
    new_path = path(new_dir, file_name),
    exists = file_exists(new_path)
  )

if (!nrow(files)) {
  cli_abort("No matching files to copy.")
}

# Review and confirm ----
cli_h2("Review copy plan")
cli_text("Destination: {.path {final_dir}}")
cli_text("Word source: {.path {profile_dir}}")
cli_text("Excel source: {.path {background_dir}}")
for (type in c("Word profiles", "SDC workbooks", "Background data workbooks")) {
  count <- sum(files$type == type)
  cli_text("  {type}: {count} files")
}
cli_text("Already in destination: {sum(files$exists)}")

if (sum(files$exists) >= 1) {
  overwrite <- isTRUE(utils::askYesNo(
    "Overwrite existing files?",
    default = FALSE
  ))
} else {
  overwrite <- FALSE
}
to_copy <- if (overwrite) nrow(files) else sum(!files$exists)
replacements <- if (overwrite) sum(files$exists) else 0L
cli_alert_info(
  "{to_copy} files to copy, including {replacements} replacements."
)

# Copy with progress and permission warnings ----
if (to_copy) {
  proceed <- isTRUE(utils::askYesNo("Proceed with copying?", default = FALSE))
} else {
  proceed <- FALSE
  cli_alert_success("Nothing to copy.")
}
if (to_copy && !proceed) {
  cli_alert_info("Cancelled. No files copied.")
}

# Clear the file list empty when cancelled, so no copy operations are attempted.
if (!proceed) {
  files <- files[0, ]
}

totals <- c(copied = 0L, overwritten = 0L, skipped = 0L, failed = 0L)

for (type in intersect(
  c("Word profiles", "SDC workbooks", "Background data workbooks"),
  files$type
)) {
  stage <- filter(files, .data$type == .env$type)
  cli_h2("Copy {type}")
  progress <- cli_progress_bar(
    name = type,
    total = nrow(stage),
    clear = FALSE
  )
  for (i in seq_len(nrow(stage))) {
    src <- stage$path[[i]]
    dst <- stage$new_path[[i]]
    folder <- stage$new_dir[[i]]

    if (file_exists(dst) && !overwrite) {
      totals[["skipped"]] <- totals[["skipped"]] + 1L
      cli_progress_update(id = progress)
      next
    }

    existed <- file_exists(dst)
    copied <- tryCatch(
      {
        if (!dir_exists(folder)) {
          dir_create(folder, mode = "ug=rwx,o=rx")
          group_result <- try(
            file_chown(folder, group_id = "LIST_analytics"),
            silent = TRUE
          )
          if (inherits(group_result, "try-error")) {
            cli_alert_warning(
              "Created {.path {folder}} but could not set its group."
            )
          }
        }
        file_copy(src, dst, overwrite = overwrite)
        TRUE
      },
      error = function(e) {
        name <- path_file(src)
        cli_alert_warning(
          "Could not copy {.file {name}}: {conditionMessage(e)}"
        )
        FALSE
      }
    )

    if (copied) {
      key <- if (existed) "overwritten" else "copied"
      totals[[key]] <- totals[[key]] + 1L
      group_result <- try(
        file_chown(dst, group_id = "LIST_analytics"),
        silent = TRUE
      )
      if (inherits(group_result, "try-error")) {
        cli_alert_warning(
          "Copied {.file {path_file(dst)}} but could not set its group."
        )
      }
    } else {
      totals[["failed"]] <- totals[["failed"]] + 1L
    }
    cli_progress_update(id = progress)
  }
  cli_progress_done(id = progress)
}

if (proceed) {
  cli_h2("Finished")
  cli_text(
    "{totals[['copied']]} new; {totals[['overwritten']]} overwritten."
  )
  cli_text(
    "{totals[['skipped']]} skipped; {totals[['failed']]} failed."
  )
  if (totals[["failed"]]) {
    cli_alert_warning(
      "Some files could not be copied; check the warnings above."
    )
  }
  cli_alert_info("Destination: {.path {final_dir}}")
}
