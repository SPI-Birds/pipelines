#' Construct standard format for data from [SITE NAME]
#'
#' A pipeline to produce the standard format for the [BIRD TYPE] population
#' in [LOCATION], administered by [DATA CUSTODIAN/INSTITUTION].
#'
#' This section provides details on data management choices that are unique
#' to this data. For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#' \strong{Data Description:}
#'
#' [Provide a brief description of the study system, species monitored, and
#' years of data collection.]
#'
#' @inheritParams pipeline_params
#'
#' @return Standard format list with 6 data frames and protocol version.
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
    study_id = "XXX",
    site_id = "XXX",
    
    # Species mapping: primary data code -> scientific name
    species_map = c(
      "GRETI" = "Parus major",
      "BLUTI" = "Cyanistes caeruleus"
    ),
    
    # Default filters
    default_species = species_codes$Species,
    default_pops = "XXX",
    
    # File paths
    files = list(
      primary = "XXX_PrimaryData.xlsx",
      supplementary = NULL  # Add if needed
    ),
    
    # Column mappings: standard name -> primary data column name
    columns = list(
      brood = c(
        brood_id = "brood_id",
        species = "species",
        plot = "plot",
        location = "location",
        female_ring = "female_ring",
        male_ring = "male_ring",
        lay_date = "lay_date",
        clutch_size = "clutch_size",
        hatch_date = "hatch_date",
        brood_size = "brood_size",
        fledge_date = "fledge_date",
        fledged = "fledged"
      ),
      capture = c(
        ring_number = "ring_number",
        species = "species",
        sex = "sex",
        capture_date = "capture_date",
        capture_time = "capture_time",
        observer = "observer",
        plot = "plot",
        location = "location",
        mass = "mass",
        tarsus = "tarsus",
        wing = "wing",
        chick_age = "chick_age"
      )
    )
  )

  # ---- LOAD DATA ----
  message("Importing primary data...")
  
  raw_data <- load_primary_data_XXX(db, config)
  
  # Apply filters
  raw_data <- apply_filters(
    raw_data,
    species_filter = species %||% config$default_species,
    pop_filter = pop %||% config$default_pops
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

load_primary_data_XXX <- function(db, config) {
  
  # Load main data file
  primary <- readxl::read_xlsx(
    path = file.path(db, config$files$primary),
    col_types = "text",
    na = c("", "NA")
  ) %>%
    janitor::clean_names()
  
  # Parse dates and numbers once
  primary <- parse_standard_columns(primary)
  
  return(list(primary = primary))
}


#' Parse standard column types
#'
#' @param data Data frame to parse
#'
#' @return Data frame with parsed columns

parse_standard_columns <- function(data) {
  
  data %>%
    dplyr::mutate(
      # Dates
      dplyr::across(
        dplyr::matches("_date$"),
        ~lubridate::ymd(.)
      ),
      # Years
      dplyr::across(
        dplyr::matches("_year$|^year$"),
        ~as.integer(.)
      ),
      # Numbers
      dplyr::across(
        dplyr::matches("size$|^mass$|tarsus|wing|age$|count"),
        ~as.numeric(.)
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
      if (!is.null(species_filter)) .data$species %in% species_filter 
      else TRUE,
      if (!is.null(pop_filter)) .data$population %in% pop_filter 
      else TRUE
    )
  
  return(data_list)
}


# ==============================================================================
# TABLE CREATION - HIGH LEVEL
# ==============================================================================

#' Create all standard format tables
#'
#' @param raw_data List of raw data frames
#' @param config Configuration list
#' @param protocol_version Protocol version string
#'
#' @return List of standard format tables

create_all_tables_XXX <- function(raw_data, config, protocol_version) {
  
  # Create tables in dependency order
  Brood_data <- build_brood_table(raw_data$primary, config)
  
  Capture_data <- build_capture_table(raw_data$primary, config)
  
  Individual_data <- build_individual_table(Capture_data, Brood_data, config)
  
  Location_data <- build_location_table(raw_data$primary, config)
  
  Measurement_data <- build_measurement_table(Capture_data, Brood_data, config)
  
  Experiment_data <- build_experiment_table(raw_data$primary, config)
  
  return(list(
    Brood_data = Brood_data,
    Capture_data = Capture_data,
    Individual_data = Individual_data,
    Location_data = Location_data,
    Measurement_data = Measurement_data,
    Experiment_data = Experiment_data,
    protocol_version = protocol_version
  ))
}


# ==============================================================================
# TABLE BUILDERS - BROOD
# ==============================================================================

#' Build brood data table
#'
#' @param data Primary data
#' @param config Configuration list
#'
#' @return Brood data frame

build_brood_table <- function(data, config) {
  
  cols <- config$columns$brood
  
  data %>%
    dplyr::distinct(.data[[cols["brood_id"]]], .keep_all = TRUE) %>%
    add_brood_identifiers(config) %>%
    add_brood_parents(cols) %>%
    add_brood_dates(cols) %>%
    add_brood_sizes(cols) %>%
    add_quality_columns() %>%
    select_brood_columns()
}

add_brood_identifiers <- function(data, config) {
  data %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      broodID = as.character(.data[[config$columns$brood["brood_id"]]]),
      speciesID = config$species_map[.data[[config$columns$brood["species"]]]],
      studyID = config$study_id,
      siteID = config$site_id,
      plotID = as.character(.data[[config$columns$brood["plot"]]]),
      locationID = as.character(.data[[config$columns$brood["location"]]])
    )
}

add_brood_parents <- function(data, cols) {
  data %>%
    dplyr::mutate(
      femaleID = as.character(.data[[cols["female_ring"]]]),
      maleID = as.character(.data[[cols["male_ring"]]])
    )
}

add_brood_dates <- function(data, cols) {
  data %>%
    add_date_components(cols["lay_date"], "Lay") %>%
    add_date_components(cols["hatch_date"], "Hatch") %>%
    add_date_components(cols["fledge_date"], "Fledge")
}

add_brood_sizes <- function(data, cols) {
  data %>%
    dplyr::mutate(
      observedClutchSize = as.integer(.data[[cols["clutch_size"]]]),
      observedBroodSize = as.integer(.data[[cols["brood_size"]]]),
      observedNumberFledged = as.integer(.data[[cols["fledged"]]])
    )
}

select_brood_columns <- function(data) {
  data %>%
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

build_capture_table <- function(data, config) {
  
  cols <- config$columns$capture
  
  data %>%
    dplyr::filter(!is.na(.data[[cols["ring_number"]]])) %>%
    add_capture_identifiers(config, cols) %>%
    add_capture_taxonomy(config, cols) %>%
    add_capture_datetime(cols) %>%
    add_capture_location(config, cols) %>%
    add_capture_status() %>%
    add_capture_metadata(cols) %>%
    add_quality_columns() %>%
    select_capture_columns()
}

add_capture_identifiers <- function(data, config, cols) {
  data %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      captureID = paste(
        .data[[cols["ring_number"]]], 
        .data[[cols["capture_date"]]], 
        sep = "_"
      ),
      individualID = as.character(.data[[cols["ring_number"]]]),
      captureTagID = as.character(.data[[cols["ring_number"]]]),
      releaseTagID = as.character(.data[[cols["ring_number"]]]),
      studyID = config$study_id
    )
}

add_capture_taxonomy <- function(data, config, cols) {
  data %>%
    dplyr::mutate(
      speciesID = config$species_map[.data[[cols["species"]]]],
      observedSex = dplyr::case_when(
        .data[[cols["sex"]]] == "M" ~ "M",
        .data[[cols["sex"]]] == "F" ~ "F",
        TRUE ~ NA_character_
      )
    )
}

add_capture_datetime <- function(data, cols) {
  data %>%
    dplyr::mutate(
      captureYear = lubridate::year(.data[[cols["capture_date"]]]),
      captureMonth = lubridate::month(.data[[cols["capture_date"]]]),
      captureDay = lubridate::day(.data[[cols["capture_date"]]]),
      captureTime = format(.data[[cols["capture_time"]]], "%H:%M:%S")
    )
}

add_capture_location <- function(data, config, cols) {
  data %>%
    dplyr::mutate(
      captureSiteID = config$site_id,
      releaseSiteID = config$site_id,
      capturePlotID = as.character(.data[[cols["plot"]]]),
      releasePlotID = as.character(.data[[cols["plot"]]]),
      captureLocationID = as.character(.data[[cols["location"]]]),
      releaseLocationID = as.character(.data[[cols["location"]]]),
      recordedBy = as.character(.data[[cols["observer"]]])
    )
}

add_capture_status <- function(data) {
  data %>%
    dplyr::mutate(
      capturePhysical = TRUE,
      captureAlive = TRUE,
      releaseAlive = TRUE
    )
}

add_capture_metadata <- function(data, cols) {
  data %>%
    dplyr::mutate(
      chickAge = as.integer(.data[[cols["chick_age"]]]),
      treatmentID = NA_character_
    )
}

select_capture_columns <- function(data) {
  data %>%
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

build_individual_table <- function(capture_data, brood_data, config) {
  
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
      by = c("first_location" = "locationID", 
             "tagYear" = "observedHatchYear")
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
      siteID = config$site_id,
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

build_location_table <- function(data, config) {
  
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
      siteID = config$site_id
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

build_measurement_table <- function(capture_data, brood_data, config) {
  
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

build_experiment_table <- function(data, config) {
  
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


# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
