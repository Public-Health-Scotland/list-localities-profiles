read_in_localities <- function(dz_level = FALSE) {
  data <- arrow::read_parquet(
    fs::path(
      "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/",
      "custom_lookups/custom_mmw.parquet"
    )
  )

  if (!dz_level) {
    data <- data |>
      dplyr::distinct(hscp_locality, hscp2019name, hscp2019, hb2019name, hb2019,mm_ward_name,mm_ward_code)
  }

  return(data)
}

read_in_iz <- function(dz_all = FALSE) {
  iz_lookup <- arrow::read_parquet(
    fs::path(lp_path, "custom_lookups/custom_mmw_iz.parquet")
  )

  if (dz_all) {
    all_iz <- readr::read_csv(
      file = fs::path(
        "/conf/linkage/output/lookups/Unicode/Geography",
        "DataZone2011/Datazone2011lookup.csv"
      ),
      col_types = "c",
      col_select = c(
        "datazone2011",
        "intzone2011" = "IZ2011_Code",
        "intzone2011name" = "IZ2011_Name"
      ),
      lazy = TRUE
    )

    iz_lookup <- iz_lookup |>
      dplyr::full_join(
        all_iz,
        by = dplyr::join_by(intzone2011, intzone2011name)
      ) |>
      dplyr::left_join(
        read_in_localities(dz_level = TRUE),
        by = dplyr::join_by(datazone2011)
      ) |> 
      dplyr::mutate(
        hscp_locality = dplyr::if_else(is.na(hscp_locality.y), hscp_locality.x, hscp_locality.y),
        .keep = "unused"
      ) |> 
      dplyr::mutate(
        intzone2011 = dplyr::if_else(is.na(intzone2011.y), intzone2011.x, intzone2011.y),
        .keep = "unused")|> 
      dplyr::mutate(
        intzone2011name = dplyr::if_else(is.na(intzone2011name.y), intzone2011name.x, intzone2011name.y),
        .keep = "unused")|> 
      dplyr::mutate(
        mm_ward_name = dplyr::if_else(is.na(mm_ward_name.y), mm_ward_name.x, mm_ward_name.y),
        .keep = "unused")
  }
  
  return(iz_lookup)
}


# Aggregate and calculate confidence interval
summarise_iz_to_locality <- function(data, iz_lookup = read_in_iz(dz_all = FALSE)) {
  iz_data <- data |>
    inner_join(iz_lookup, by = c("area_code" = "intzone2011"))

  if (anyNA(iz_data$numerator)) {
    locality_data <- iz_data |>
      group_by(indicator, year, period, area_name = hscp_locality, definition, data_source) |>
      summarise(
        measure = mean(measure),
        area_type = "HSC locality",
        .groups = "drop"
      )
  } else {
    locality_data <- iz_data |>
      mutate(weighted_measure = numerator * measure / 100000) %>%
      group_by(indicator, year, period, area_name = hscp_locality, definition, data_source) |>
      summarise(
        numerator = sum(numerator),
        denominator = sum(numerator / measure * 100000), # Adjust for per 100,000
        measure = sum(weighted_measure) / sum(numerator) * 100000, # Adjust for per 100,000
        se = sqrt(sum(numerator * (measure - lower_confidence_interval)^2 / (denominator - 1)) +
          sum(numerator * (upper_confidence_interval - measure)^2 / (denominator - 1))),
        lower_confidence_interval = measure - 1.96 * se,
        upper_confidence_interval = measure + 1.96 * se,
        area_type = "HSC locality",
        .groups = "drop"
      ) |>
      select(!denominator, !se)
  }

  new_data <- data |>
    filter(area_type %in% c("HSC locality","Scotland","HSC partnership","Health board")) |>
    bind_rows(locality_data)

  return(new_data)
}

clean_scotpho_dat <- function(data) {
  data %>%
    summarise_iz_to_locality() |>
    filter(area_type != "Council area" & area_type != "Intermediate zone") %>%
    mutate(area_name = gsub("&", "and", area_name)) %>%
    mutate(area_name = if_else(area_name == "Renfrewshire West", "West Renfrewshire", area_name)) %>%
    mutate(
      area_type = if_else(area_type == "HSC partnership", "HSCP", area_type),
      area_type = if_else(area_type == "HSC locality", "Locality", area_type)
    )
}

