source("Master RMarkdown Document & Render Code/Global Script.R")

output_dir <- fs::path(lp_path, "Profiles Output")
lookup <- read_in_localities()

hscp_list <- c("Orkney Islands")

unknown_hscps <- setdiff(hscp_list, lookup$hscp2019name)
if (length(unknown_hscps) > 0) {
  stop("Unknown HSCPs: ", toString(unknown_hscps), call. = FALSE)
}

# Keep localities in the selected HSCP order.
localities <- lookup |>
  filter(hscp2019name %in% hscp_list) |>
  arrange(match(hscp2019name, hscp_list)) |>
  pull(hscp_locality)

result <- build_profiles(
  localities = localities,
  output_dir = output_dir,
  progress = function(event) {
    if (event$status %in% c("succeeded", "failed")) {
      message(sprintf(
        "[%d/%d] %s: %s",
        event$position,
        event$total,
        event$locality,
        event$status
      ))
    }
  }
)

print(result$results)
message(sprintf(
  "Profiles complete: %d succeeded, %d failed.",
  result$succeeded,
  result$failed
))
if (result$failed > 0L) {
  warning("Some Locality Profiles could not be generated.", call. = FALSE)
}
