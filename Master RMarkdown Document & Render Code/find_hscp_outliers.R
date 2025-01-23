# Add some metrics to the lookup 
lookup_with_metrics <- lookup |> 
  mutate(
    locality_length = str_width(hscp_locality),
    hscp_length = str_width(hscp2019name)) |> 
  group_by(hscp2019name) |> 
  mutate(
    n_localities = n_distinct(hscp_locality),
    total_length = str_width(paste0(paste(hscp_locality, collapse = ""), hscp2019name))
    ) |> 
  ungroup()

# Use the metrics to take the HSCP with:
# The longest and shortest locality name
# The most and least number of localities
# The longest and shortest when considering all locality names + the HSCP name
# The longest HSCP name
# Note there will likely be overlaps, so the list shouldn't be too long
outlier_hscps <- bind_rows(
  slice_max(lookup_with_metrics, locality_length, with_ties = FALSE),
  slice_max(lookup_with_metrics, n_localities, with_ties = FALSE),
  slice_max(lookup_with_metrics, total_length, with_ties = FALSE),
  slice_max(lookup_with_metrics, hscp_length, with_ties = FALSE),
  slice_min(lookup_with_metrics, locality_length, with_ties = FALSE),
  slice_min(lookup_with_metrics, n_localities, with_ties = FALSE),
  slice_min(lookup_with_metrics, total_length, with_ties = FALSE),
) |> 
  pull(hscp2019name) |> 
  unique()

rm(lookup_with_metrics)
