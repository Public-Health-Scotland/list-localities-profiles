lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

source("Master RMarkdown Document & Render Code/Global Script.R")

# Read in the Excel Lookup
custom_lookup <- readxl::read_excel(fs::path(
  lp_path,
  "custom_lookups/custom_south_ayrshire_localities.xlsx"
))

# Make locality lookup
custom_lookup |>
  bind_rows(
    read_in_localities(dz_level = TRUE) |>
      filter(hscp2019name != "South Ayrshire")
  ) |>
  mutate(hscp_locality = sub("&", "and", hscp_locality, fixed = TRUE)) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/custom_south_ayrshire_localities.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )

# Make IZ lookup
custom_lookup |>
  distinct(
    intzone2011,
    intzone2011name,
    hscp_locality
  ) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/custom_south_ayrshire_localities_iz.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )
