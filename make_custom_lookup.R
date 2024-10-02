lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

source("Master RMarkdown Document & Render Code/Global Script.R")
dz_lookup <- read_in_localities(T) |>
  clean_names() |>
  #filter(hscp2019name == "East Ayrshire") |>
  select(datazone2011,hscp_locality,hb2019name)
  

# Read in the Excel Lookup
custom_lookup <- readxl::read_excel(fs::path(
  lp_path,
  "custom_lookups/Ayrshire_mmw.xlsx"
))|>
  # Clean names
  left_join(dz_lookup) |>
  clean_names()


# Make locality lookup
custom_lookup |>
  mutate(hscp_locality = sub("&", "and", hscp_locality, fixed = TRUE)) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/custom_mmw.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )

# Make IZ lookup
custom_lookup |>
  distinct(
    intzone2011,
    intzone2011name,
    hscp_locality,
    mm_ward_name
  ) |>
  # Write out as a parquet file
  arrow::write_parquet(
    fs::path(
      lp_path,
      "custom_lookups/custom_mmw_iz.parquet"
    ),
    version = "latest",
    compression = "zstd"
  )
