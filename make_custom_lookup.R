lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

source("Master RMarkdown Document & Render Code/Global Script.R")

# Read in the Excel Lookup
custom_lookup <- readxl::read_excel(fs::path(
  lp_path,
  "custom_lookups/forres_and_lossiemouth_april_2022.xlsx"
)) |>
  # Clean names
  clean_names() |>
  # Make all rows work
  mutate(
    sub_area = if_else(sub_area == "Not yet required", "Other areas", sub_area)
  )

# Make locality lookup
custom_lookup |>
  # Make variables match the locality lookup
  mutate(
    datazone2011 = datazone,
    hscp_locality = sub_area,
    hscp2019name = "Moray",
    hscp2019 = "S37000019",
    hb2019name = "NHS Grampian",
    hb2019 = "S08000020",
    .keep = "none"
  ) |>
  bind_rows(
    read_in_localities(dz_level = TRUE) |>
      filter(hscp2019name != "Moray")
  ) |>
  mutate(hscp_locality = sub("&", "and", hscp_locality, fixed = TRUE)) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/forres_and_lossiemouth_april_2022.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )

# Make IZ lookup
custom_lookup |>
  distinct(intermediate_zone, iz_name, hscp_locality = sub_area) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/forres_and_lossiemouth_iz_april_2022.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )
