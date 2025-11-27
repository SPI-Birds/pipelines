#' Construct standard format for data from [SITE NAME].
#' Adhere to protocol version 2.0.0.
#'
#' This is a pipeline to produce the 2.0.0 standard format data
#' for the [BIRD TYPE] population
#' in [LOCATION], administered by [DATA CUSTODIAN/INSTITUTION].
#'
#' Please provide data management choices in this section.
#' For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#' \strong{Data Description:}
#'
#' [Provide a brief description of the study.]
#'
#' @inheritParams pipeline_params
#'
#' @return Standard format list with 6 data frames and the protocol version.
#'
#' @export

format_XXX <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R") {
  protocol_version <- "2.0.0"
  force(db)
  start_time <- Sys.time()

  # ---- CONFIGURATION ----
  # Define all site-specific mappings in one place
  config <- list(
    # For example: "HOC", "NIOO" etc.
    study_id = "XXX",
    # Can be a vector, e.g., c("HOG", "WAR" etc in the case of NIOO)
    site_id = c("XXX"),

    # Species mapping: primary data code -> scientific name
    # Example showed here
    # TODO: can we integrate this with pop_species_combos.csv?
    species_map = c(
      "GRETI" = "Parus major",
      "BLUTI" = "Cyanistes caeruleus"
    ),

    # Define default filters
    default_species = species_codes$Species,
    default_pops = pop_codes$PopID,

    # Define file paths
    files = list(
      primary = "XXX_PrimaryData.xlsx",
      supplementary1 = NULL,
      supplementary2 = NULL
    ),

    # Define parsing patterns for standard columns
    parse_patterns = list(
      date = "_date$",
      year = "_year$|^year$",
      number = "size$|^mass$|tarsus|wing|age$|count"
    )
  )

  # ---- LOAD DATA ----
  message("Importing primary data...")

  raw_data <- load_data_XXX(db, config)

  # Apply filters
  raw_data <- apply_filters(
    raw_data,
    species_filter = dplyr::coalesce(species, config$default_species),
    pop_filter = dplyr::coalesce(pop, config$default_pops)
  )

  # ---- CREATE TABLES ----
  message("Compiling standard format tables...")

  tables <- create_all_tables_XXX(raw_data, config, protocol_version)

  # ---- FINALIZE ----
  tables <- finalize_tables(tables, protocol_version)

  time_elapsed <- difftime(Sys.time(), start_time, units = "sec")
  message(sprintf("All tables generated in %.2f seconds", time_elapsed))

  # ---- EXPORT ----
  export_tables(tables, output_type, path, "XXX", protocol_version)
}


# ==============================================================================
# DATA LOADING FUNCTIONS
# ==============================================================================

#' Load primary data for XXX
#'
#' @param db Database path
#' @param config Configuration list
#'
#' @return List of data frames

load_data_XXX <- function(db, config) {
  # Load main data file
  primary <- readxl::read_xlsx(
    path = file.path(db, config$files$primary),
    col_types = "text",
    na = c("", "NA")
  ) %>%
    janitor::clean_names()

  # Load supplemental data files, if any
  supplementary1 <- readxl::read_xlsx(
    path = file.path(db, config$files$supplementary1),
    col_types = "text",
    na = c("", "NA")
  ) %>%
    janitor::clean_names()

  # Load supplemental data files, if any
  supplementary2 <- readxl::read_xlsx(
    path = file.path(db, config$files$supplementary2),
    col_types = "text",
    na = c("", "NA")
  ) %>%
    janitor::clean_names()

  # Parse dates and numbers
  primary <- parse_standard_columns_XXX(primary, config)
  supplementary1 <- parse_standard_columns_XXX(supplementary1, config)
  supplementary2 <- parse_standard_columns_XXX(supplementary2, config)

  return(list(
    primary = primary,
    supplementary1 = supplementary1,
    supplementary2 = supplementary2
  ))
}

#' Parse date, years and numeric columns for standard column types
#'
#' @param data Data frame to parse (primary, supplementary1 etc)
#'
#' @return Data frame with parsed date columns

parse_standard_columns_XXX <- function(data, config) {
  patterns <- config$parse_patterns
  data %>%
    dplyr::mutate(
      # Dates
      dplyr::across(
        dplyr::matches(patterns$date),
        ~ lubridate::ymd(.)
      ),
      # Years
      dplyr::across(
        dplyr::matches(patterns$year),
        ~ as.integer(.)
      ),
      # Numbers
      dplyr::across(
        dplyr::matches(patterns$number),
        ~ as.numeric(.)
      )
    )
}

#' Apply species and population filters
#'
#' @param data_list List of data frames
#' @param species_filter Species to keep
#' @param pop_filter Populations to keep
#'
#' @return Filtered list of data frames

apply_filters <- function(data_list, species_filter, pop_filter) {
  # Apply to each data frame in list
  data_list$primary <- data_list$primary %>%
    dplyr::filter(
      if (!is.null(species_filter)) {
        .data$species %in% species_filter
      } else {
        TRUE
      },
      if (!is.null(pop_filter)) {
        .data$population %in% pop_filter
      } else {
        TRUE
      }
    )

  return(data_list)
}

# ==============================================================================
# TABLE CREATION - HIGH LEVEL
# ==============================================================================


#' Create all standard format tables (as variables, not functions)
#'
#' Sections are preserved for clarity

# ===========================================================================
# TABLE CREATION - HIGH LEVEL
# ===========================================================================


#' Build brood data table
build_brood_table_XXX <- function(data, config) {
  data$primary %>%
    dplyr::distinct(.data$brood_id, .keep_all = TRUE) %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      broodID = as.character(.data$brood_id),
      speciesID = config$species_map[.data$species],
      studyID = config$study_id,
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      plotID = as.character(.data$plot),
      locationID = as.character(.data$location),
      femaleID = as.character(.data$female_ring),
      maleID = as.character(.data$male_ring),
      observedClutchSize = as.integer(.data$clutch_size),
      observedBroodSize = as.integer(.data$brood_size),
      observedNumberFledged = as.integer(.data$fledged)
    ) %>%
    add_date_components("lay_date", "Lay") %>%
    add_date_components("hatch_date", "Hatch") %>%
    add_date_components("fledge_date", "Fledge") %>%
    add_quality_columns() %>%
    dplyr::select(
      "row", "broodID", "speciesID", "studyID", "siteID",
      "plotID", "locationID", "femaleID", "maleID",
      dplyr::starts_with("observed"),
      dplyr::starts_with("minimum"),
      dplyr::starts_with("maximum"),
      "treatmentID", "rowWarning", "rowError"
    )
}

#' Build capture data table
build_capture_table_XXX <- function(data, config) {
  data$primary %>%
    dplyr::filter(!is.na(.data$ring_number)) %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      captureID = paste(
        .data$ring_number,
        .data$capture_date,
        sep = "_"
      ),
      individualID = as.character(.data$ring_number),
      captureTagID = as.character(.data$ring_number),
      releaseTagID = as.character(.data$ring_number),
      studyID = config$study_id,
      speciesID = config$species_map[.data$species],
      observedSex = dplyr::case_when(
        .data$sex == "M" ~ "M",
        .data$sex == "F" ~ "F",
        TRUE ~ NA_character_
      ),
      captureYear = lubridate::year(.data$capture_date),
      captureMonth = lubridate::month(.data$capture_date),
      captureDay = lubridate::day(.data$capture_date),
      captureTime = format(.data$capture_time, "%H:%M:%S"),
      captureSiteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      releaseSiteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      capturePlotID = as.character(.data$plot),
      releasePlotID = as.character(.data$plot),
      captureLocationID = as.character(.data$location),
      releaseLocationID = as.character(.data$location),
      recordedBy = as.character(.data$observer),
      capturePhysical = TRUE,
      captureAlive = TRUE,
      releaseAlive = TRUE,
      chickAge = as.integer(.data$chick_age),
      treatmentID = NA_character_
    ) %>%
    add_quality_columns() %>%
    dplyr::select(
      "row", "captureID", "individualID", "captureTagID",
      "releaseTagID", "speciesID", "studyID", "observedSex",
      "captureYear", "captureMonth", "captureDay", "captureTime",
      "recordedBy", "captureSiteID", "releaseSiteID",
      "capturePlotID", "releasePlotID",
      "captureLocationID", "releaseLocationID",
      "capturePhysical", "captureAlive", "releaseAlive",
      "chickAge", "treatmentID", "rowWarning", "rowError"
    )
}

#' Build individual data table
build_individual_table_XXX <- function(Capture_data, Brood_data, config) {
  Capture_data %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(
      speciesID = dplyr::first(.data$speciesID),
      studyID = dplyr::first(.data$studyID),
      tagSiteID = dplyr::first(.data$captureSiteID),
      tagYear = min(.data$captureYear, na.rm = TRUE),
      tagMonth = dplyr::first(.data$captureMonth[.data$captureYear == tagYear]),
      tagDay = dplyr::first(.data$captureDay[.data$captureYear == tagYear]),
      min_chick_age = min(.data$chickAge, na.rm = TRUE),
      first_location = dplyr::first(
        .data$captureLocationID[.data$captureYear == tagYear]
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tagStage = dplyr::case_when(
        .data$min_chick_age < 18 ~ "chick",
        TRUE ~ "adult"
      )
    ) %>%
    dplyr::left_join(
      Brood_data %>%
        dplyr::select("broodID", "locationID", "observedHatchYear"),
      by = c(
        "first_location" = "locationID",
        "tagYear" = "observedHatchYear"
      )
    ) %>%
    dplyr::mutate(
      broodIDLaid = dplyr::if_else(
        .data$tagStage == "chick",
        .data$broodID,
        NA_character_
      ),
      broodIDFledged = .data$broodIDLaid,
      row = dplyr::row_number(),
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      geneticSex = NA_character_
    ) %>%
    add_quality_columns() %>%
    dplyr::select(
      "row", "individualID", "speciesID", "studyID", "siteID",
      "broodIDLaid", "broodIDFledged",
      "tagYear", "tagMonth", "tagDay", "tagStage", "tagSiteID",
      "geneticSex", "rowWarning", "rowError"
    )
}

#' Build location data table
build_location_table_XXX <- function(data, config) {
  data$primary %>%
    dplyr::distinct(.data$location, .keep_all = TRUE) %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      locationID = as.character(.data$location),
      locationType = "nest",
      locationDetails = paste("Nestbox", .data$location),
      studyID = config$study_id,
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      decimalLatitude = as.numeric(.data$latitude),
      decimalLongitude = as.numeric(.data$longitude),
      elevation = NA_real_,
      startYear = min(.data$year, na.rm = TRUE),
      endYear = NA_integer_
    ) %>%
    add_quality_columns() %>%
    dplyr::select(
      "row", "locationID", "locationType", "locationDetails",
      "studyID", "siteID", "decimalLatitude", "decimalLongitude",
      "elevation", "startYear", "endYear",
      "rowWarning", "rowError"
    )
}

#' Build measurement data table (empty)
build_measurement_table_XXX <- function(Capture_data, Brood_data, config) {
  tibble::tibble()
}

#' Build experiment data table (empty)
build_experiment_table_XXX <- function(data, config) {
  tibble::tibble()
}

tables <- list(
  Brood_data = build_brood_table_XXX(raw_data, config),
  Capture_data = build_capture_table_XXX(raw_data, config),
  Individual_data = build_individual_table_XXX(
    build_capture_table_XXX(raw_data, config),
    build_brood_table_XXX(raw_data, config),
    config
  ),
  Location_data = build_location_table_XXX(raw_data, config),
  Measurement_data = build_measurement_table_XXX(
    build_capture_table_XXX(raw_data, config),
    build_brood_table_XXX(raw_data, config),
    config
  ),
  Experiment_data = build_experiment_table_XXX(raw_data, config),
  protocol_version = protocol_version
)


# ==============================================================================
# TABLE BUILDERS - BROOD
# ==============================================================================

#' Build brood data table
#'
#' @param data Primary data, provided as a list
#' @param config Configuration list
#'
#' @return Brood data frame

build_brood_table_XXX <- function(data, config) {
  data$primary %>%
    dplyr::distinct(.data$brood_id, .keep_all = TRUE) %>%
    add_brood_identifiers(config) %>%
    add_brood_parents() %>%
    add_brood_dates() %>%
    add_brood_sizes() %>%
    add_quality_columns() %>%
    select_brood_columns()
}

add_brood_identifiers <- function(data, config) {
  data$primary %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      broodID = as.character(.data$brood_id),
      speciesID = config$species_map[.data$species],
      studyID = config$study_id,
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      plotID = as.character(.data$plot),
      locationID = as.character(.data$location)
    )
}

add_brood_parents <- function(data) {
  data$primary %>%
    dplyr::mutate(
      femaleID = as.character(.data$female_ring),
      maleID = as.character(.data$male_ring)
    )
}

add_brood_dates <- function(data) {
  data$primary %>%
    add_date_components("lay_date", "Lay") %>%
    add_date_components("hatch_date", "Hatch") %>%
    add_date_components("fledge_date", "Fledge")
}

add_brood_sizes <- function(data) {
  data$primary %>%
    dplyr::mutate(
      observedClutchSize = as.integer(.data$clutch_size),
      observedBroodSize = as.integer(.data$brood_size),
      observedNumberFledged = as.integer(.data$fledged)
    )
}

select_brood_columns <- function(data) {
  data$primary %>%
    dplyr::select(
      "row", "broodID", "speciesID", "studyID", "siteID",
      "plotID", "locationID", "femaleID", "maleID",
      dplyr::starts_with("observed"),
      dplyr::starts_with("minimum"),
      dplyr::starts_with("maximum"),
      "treatmentID", "rowWarning", "rowError"
    )
}


# ==============================================================================
# TABLE BUILDERS - CAPTURE
# ==============================================================================

#' Build capture data table
#'
#' @param data Primary data
#' @param config Configuration list
#'
#' @return Capture data frame

build_capture_table_XXX <- function(data, config) {
  data$primary %>%
    dplyr::filter(!is.na(.data$ring_number)) %>%
    add_capture_identifiers(config) %>%
    add_capture_taxonomy(config) %>%
    add_capture_datetime() %>%
    add_capture_location(config) %>%
    add_capture_status() %>%
    add_capture_metadata() %>%
    add_quality_columns() %>%
    select_capture_columns()
}

add_capture_identifiers <- function(data, config) {
  data$primary %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      captureID = paste(
        .data$ring_number,
        .data$capture_date,
        sep = "_"
      ),
      individualID = as.character(.data$ring_number),
      captureTagID = as.character(.data$ring_number),
      releaseTagID = as.character(.data$ring_number),
      studyID = config$study_id
    )
}

add_capture_taxonomy <- function(data, config) {
  data$primary %>%
    dplyr::mutate(
      speciesID = config$species_map[.data$species],
      observedSex = dplyr::case_when(
        .data$sex == "M" ~ "M",
        .data$sex == "F" ~ "F",
        TRUE ~ NA_character_
      )
    )
}

add_capture_datetime <- function(data) {
  data$primary %>%
    dplyr::mutate(
      captureYear = lubridate::year(.data$capture_date),
      captureMonth = lubridate::month(.data$capture_date),
      captureDay = lubridate::day(.data$capture_date),
      captureTime = format(.data$capture_time, "%H:%M:%S")
    )
}

add_capture_location <- function(data, config) {
  data$primary %>%
    dplyr::mutate(
      captureSiteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      releaseSiteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      capturePlotID = as.character(.data$plot),
      releasePlotID = as.character(.data$plot),
      captureLocationID = as.character(.data$location),
      releaseLocationID = as.character(.data$location),
      recordedBy = as.character(.data$observer)
    )
}

add_capture_status <- function(data) {
  data$primary %>%
    dplyr::mutate(
      capturePhysical = TRUE,
      captureAlive = TRUE,
      releaseAlive = TRUE
    )
}

add_capture_metadata <- function(data) {
  data$primary %>%
    dplyr::mutate(
      chickAge = as.integer(.data$chick_age),
      treatmentID = NA_character_
    )
}

select_capture_columns <- function(data) {
  data$primary %>%
    dplyr::select(
      "row", "captureID", "individualID", "captureTagID",
      "releaseTagID", "speciesID", "studyID", "observedSex",
      "captureYear", "captureMonth", "captureDay", "captureTime",
      "recordedBy", "captureSiteID", "releaseSiteID",
      "capturePlotID", "releasePlotID",
      "captureLocationID", "releaseLocationID",
      "capturePhysical", "captureAlive", "releaseAlive",
      "chickAge", "treatmentID", "rowWarning", "rowError"
    )
}


# ==============================================================================
# TABLE BUILDERS - INDIVIDUAL
# ==============================================================================

#' Build individual data table
#'
#' @param capture_data Capture data frame
#' @param brood_data Brood data frame
#' @param config Configuration list
#'
#' @return Individual data frame

build_individual_table_XXX <- function(capture_data, brood_data, config) {
  capture_data %>%
    summarize_first_captures() %>%
    link_to_broods(brood_data) %>%
    add_individual_identifiers(config) %>%
    add_quality_columns() %>%
    select_individual_columns()
}

summarize_first_captures <- function(capture_data) {
  capture_data %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(
      speciesID = dplyr::first(.data$speciesID),
      studyID = dplyr::first(.data$studyID),
      tagSiteID = dplyr::first(.data$captureSiteID),
      tagYear = min(.data$captureYear, na.rm = TRUE),
      tagMonth = dplyr::first(.data$captureMonth[
        .data$captureYear == tagYear
      ]),
      tagDay = dplyr::first(.data$captureDay[
        .data$captureYear == tagYear
      ]),
      min_chick_age = min(.data$chickAge, na.rm = TRUE),
      first_location = dplyr::first(.data$captureLocationID[
        .data$captureYear == tagYear
      ]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tagStage = dplyr::case_when(
        .data$min_chick_age < 18 ~ "chick",
        TRUE ~ "adult"
      )
    )
}

link_to_broods <- function(individual_data, brood_data) {
  individual_data %>%
    dplyr::left_join(
      brood_data %>%
        dplyr::select("broodID", "locationID", "observedHatchYear"),
      by = c(
        "first_location" = "locationID",
        "tagYear" = "observedHatchYear"
      )
    ) %>%
    dplyr::mutate(
      broodIDLaid = dplyr::if_else(
        .data$tagStage == "chick",
        .data$broodID,
        NA_character_
      ),
      broodIDFledged = .data$broodIDLaid
    )
}

add_individual_identifiers <- function(data, config) {
  data %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      },
      geneticSex = NA_character_
    )
}

select_individual_columns <- function(data) {
  data %>%
    dplyr::select(
      "row", "individualID", "speciesID", "studyID", "siteID",
      "broodIDLaid", "broodIDFledged",
      "tagYear", "tagMonth", "tagDay", "tagStage", "tagSiteID",
      "geneticSex", "rowWarning", "rowError"
    )
}


# ==============================================================================
# TABLE BUILDERS - LOCATION
# ==============================================================================

#' Build location data table
#'
#' @param data Primary data
#' @param config Configuration list
#'
#' @return Location data frame

build_location_table_XXX <- function(data, config) {
  data %>%
    extract_unique_locations() %>%
    add_location_identifiers(config) %>%
    add_location_coordinates() %>%
    add_location_years() %>%
    add_quality_columns() %>%
    select_location_columns()
}

extract_unique_locations <- function(data) {
  data %>%
    dplyr::distinct(.data$location, .keep_all = TRUE)
}

add_location_identifiers <- function(data, config) {
  data %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      locationID = as.character(.data$location),
      locationType = "nest",
      locationDetails = paste("Nestbox", .data$location),
      studyID = config$study_id,
      siteID = if (length(config$site_id) > 1) {
        paste(
          config$site_id,
          collapse = ","
        )
      } else {
        config$site_id
      }
    )
}

add_location_coordinates <- function(data) {
  data %>%
    dplyr::mutate(
      decimalLatitude = as.numeric(.data$latitude),
      decimalLongitude = as.numeric(.data$longitude),
      elevation = NA_real_
    )
}

add_location_years <- function(data) {
  data %>%
    dplyr::mutate(
      startYear = min(.data$year, na.rm = TRUE),
      endYear = NA_integer_
    )
}

select_location_columns <- function(data) {
  data %>%
    dplyr::select(
      "row", "locationID", "locationType", "locationDetails",
      "studyID", "siteID", "decimalLatitude", "decimalLongitude",
      "elevation", "startYear", "endYear",
      "rowWarning", "rowError"
    )
}


# ==============================================================================
# TABLE BUILDERS - MEASUREMENT
# ==============================================================================

#' Build measurement data table
#'
#' @param capture_data Capture data frame
#' @param brood_data Brood data frame (for brood-level measurements)
#' @param config Configuration list
#'
#' @return Measurement data frame

build_measurement_table_XXX <- function(capture_data, brood_data, config) {
  # For now, return empty table with correct structure
  tibble::tibble()
}


# ==============================================================================
# TABLE BUILDERS - EXPERIMENT
# ==============================================================================

#' Build experiment data table
#'
#' @param data Primary data
#' @param config Configuration list
#'
#' @return Experiment data frame

build_experiment_table_XXX <- function(data, config) {
  # For now, return empty table with correct structure
  tibble::tibble()
}


# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Add date components (year, month, day) for a given date column
#'
#' @param data Data frame
#' @param date_col Name of date column
#' @param prefix Prefix for output columns (e.g., "Lay", "Hatch")
#'
#' @return Data frame with added date component columns

add_date_components <- function(data, date_col, prefix) {
  year_col <- paste0("observed", prefix, "Year")
  month_col <- paste0("observed", prefix, "Month")
  day_col <- paste0("observed", prefix, "Day")

  data %>%
    dplyr::mutate(
      !!year_col := lubridate::year(.data[[date_col]]),
      !!month_col := lubridate::month(.data[[date_col]]),
      !!day_col := lubridate::day(.data[[date_col]])
    )
}


#' Add quality check columns (rowWarning, rowError)
#'
#' @param data Data frame
#'
#' @return Data frame with quality columns

add_quality_columns <- function(data) {
  data %>%
    dplyr::mutate(
      rowWarning = NA,
      rowError = NA
    )
}


#' Finalize all tables to match protocol template
#'
#' @param tables List of data frames
#' @param protocol_version Protocol version string
#'
#' @return List of finalized data frames

finalize_tables <- function(tables, protocol_version) {
  template <- data_templates[[paste0("v", protocol_version)]]

  for (table_name in names(template)) {
    if (table_name %in% names(tables)) {
      tables[[table_name]] <- conform_to_template(
        tables[[table_name]],
        template[[table_name]]
      )
    }
  }

  return(tables)
}


#' Conform data frame to template structure
#'
#' @param data Data frame to conform
#' @param template Template data frame
#'
#' @return Conformed data frame

conform_to_template <- function(data, template) {
  # Add missing columns
  missing_cols <- setdiff(names(template), names(data))
  for (col in missing_cols) {
    data[[col]] <- template[[col]][1]
  }

  # Select and order columns to match template
  data %>%
    dplyr::select(names(template))
}


#' Export tables to files or return as R objects
#'
#' @param tables List of data frames
#' @param output_type "R" or "csv"
#' @param path Output path
#' @param site_code Site code for file naming
#' @param protocol_version Protocol version string

export_tables <- function(tables, output_type, path, site_code,
                          protocol_version) {
  if (output_type == "csv") {
    message("Saving .csv files...")

    for (table_name in names(tables)) {
      if (table_name != "protocol_version") {
        utils::write.csv(
          x = tables[[table_name]],
          file = file.path(path, paste0(table_name, "_", site_code, ".csv")),
          row.names = FALSE
        )
      }
    }

    utils::write.table(
      x = protocol_version,
      file = file.path(path, paste0("protocol_version_", site_code, ".txt")),
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )

    invisible(NULL)
  } else if (output_type == "R") {
    message("Returning R objects...")
    return(tables)
  }
}
