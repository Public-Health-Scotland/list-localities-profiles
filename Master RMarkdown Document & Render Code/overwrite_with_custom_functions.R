read_in_localities <- function(dz_level = FALSE) {
  data <- arrow::read_parquet(
    fs::path(
      "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/",
      "custom_lookups/forres_and_lossiemouth_april_2022.parquet"
    )
  )

  if (!dz_level) {
    data <- data |>
      dplyr::distinct(hscp_locality, hscp2019name, hscp2019, hb2019name, hb2019)
  }

  return(data)
}

read_in_iz <- function(dz_all = FALSE) {
  iz_lookup <- arrow::read_parquet(
    fs::path(
      lp_path,
      "custom_lookups/forres_and_lossiemouth_iz_april_2022.parquet"
    )
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
        hscp_locality = dplyr::if_else(
          is.na(hscp_locality.y),
          hscp_locality.x,
          hscp_locality.y
        ),
        .keep = "unused"
      )
  }

  return(iz_lookup)
}

# A helper function to calculate Wilson's Score confidence intervals
# and return them at a specified rate (e.g., per 100,000).
wilson_ci_wrapper <- function(numerator, denominator, rate_per = 100000, conf.level = 0.95) {
  # Avoid division by zero and handle cases with no data
  if (denominator == 0) {
    return(list(lower_confidence_interval = NA_real_, upper_confidence_interval = NA_real_))
  }

  z <- qnorm(1 - (1 - conf.level) / 2)
  p_hat <- numerator / denominator

  # Wilson's Score formula for the confidence interval of a proportion
  lower_prop <- (p_hat + z^2 / (2 * denominator) - z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * denominator)) / denominator)) / (1 + z^2 / denominator)
  upper_prop <- (p_hat + z^2 / (2 * denominator) + z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * denominator)) / denominator)) / (1 + z^2 / denominator)

  # Convert the proportions to the desired rate
  lower_rate <- max(0, lower_prop) * rate_per
  upper_rate <- upper_prop * rate_per

  return(list(
    lower_confidence_interval = lower_rate,
    upper_confidence_interval = upper_rate
  ))
}

summarise_iz_to_locality <- function(
  data,
  iz_lookup = read_in_iz(dz_all = FALSE)
) {
  iz_data <- data |>
    dplyr::inner_join(iz_lookup, by = c("area_code" = "intzone2011"))

  if (anyNA(iz_data$numerator)) {
    locality_data <- iz_data |>
      dplyr::group_by(
        indicator,
        year,
        period,
        area_name = hscp_locality
      ) |>
      dplyr::summarise(
        measure = mean(measure),
        area_type = "HSC locality",
        .groups = "drop"
      )
  } else {
    locality_data <- iz_data |>
      dplyr::mutate(
        denominator = dplyr::case_when(
          (numerator == 0 | measure == 0) ~ 0,
          .default = numerator * 100000 / measure
        )
      ) |>
      dplyr::group_by(
        indicator,
        year,
        period,
        area_name = hscp_locality
      ) |>
      dplyr::summarise(
        numerator = sum(numerator),
        denominator = sum(denominator),
        measure = dplyr::case_when(
          (numerator == 0 | denominator == 0) ~ 0,
          .default = (numerator * 100000) / denominator
        ),
        # Use the helper function to calculate the CIs
        ci = list(wilson_ci_wrapper(numerator, denominator)),
        area_type = "HSC locality",
        .groups = "drop"
      ) |>
      tidyr::unnest_wider(ci) |>
      dplyr::select(!denominator)
  }

  new_data <- data |>
    dplyr::filter(
      area_type %in%
        c("HSC locality", "Scotland", "HSC partnership", "Health board")
    ) |>
    dplyr::bind_rows(locality_data)

  return(new_data)
}

clean_scotpho_dat <- function(data) {
  data %>%
    summarise_iz_to_locality() |>
    filter(area_type != "Council area" & area_type != "Intermediate zone") %>%
    mutate(area_name = gsub("&", "and", area_name)) %>%
    mutate(
      area_name = if_else(
        area_name == "Renfrewshire West",
        "West Renfrewshire",
        area_name
      )
    ) %>%
    mutate(
      area_type = if_else(area_type == "HSC partnership", "HSCP", area_type),
      area_type = if_else(area_type == "HSC locality", "Locality", area_type)
    )
}
