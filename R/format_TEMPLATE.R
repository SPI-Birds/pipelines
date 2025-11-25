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
#' \strong{individualID:} [Describe how individual IDs are formatted and any
#' data-specific rules.]
#'
#' \strong{broodID:} [Describe how brood IDs are created and formatted.]
#'
#' \strong{locationID:} [Describe how location IDs are structured.]
#'
#' \strong{captureID:} [Describe how capture IDs are generated.]
#'
#' \strong{Assumptions:}
#'
#' [List any important assumptions made during data processing, such as:]
#' \itemize{
#'   \item All individuals are assumed to be captured and released alive
#'   unless otherwise specified.
#'   \item Missing values in [VARIABLE] are assumed to indicate [REASON].
#'   \item [Add other assumptions as needed.]
#' }
#'
#' @inheritParams pipeline_params
#'
#' @return When \code{output_type = "R"}, a list of 6 data frames
#'   corresponding to the standard data tables (Individual_data, Brood_data,
#'   Capture_data, Location_data, Measurement_data, Experiment_data) and one
#'   character vector indicating the protocol version (2.0.0). When
#'   \code{output_type = "csv"}, six .csv files and one text file with
#'   protocol version.
#'
#' @export

format_XXX <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R") {

  # Set protocol version
  protocol_version <- "2.0.0"

  # Force directory selection if using choose_directory()
  force(db)

  # Record start time for processing time calculation
  start_time <- Sys.time()

  # Store original options and restore on exit
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)

  # ---- SETUP FILTERS ----

  # Determine species filter
  if (is.null(species)) {
    species_filter <- species_codes$Species
  } else {
    species_filter <- species
  }

  # Determine population filter
  if (is.null(pop)) {
    pop_filter <- "XXX"  # Replace with default population code(s)
  } else {
    pop_filter <- pop
  }

  # ---- LOAD PRIMARY DATA ----

  message("Importing primary data...")

  # TODO: Load your primary data file(s) here
  # Example for Excel file:
  # primary_data <- readxl::read_xlsx(
  #   path = paste0(db, "/XXX_PrimaryData.xlsx"),
  #   col_types = "text",  # Read as text to avoid coercion
  #   na = c("", "NA")
  # ) %>%
  #   janitor::clean_names()

  # Example for CSV file:
  # primary_data <- utils::read.csv(
  #   file = paste0(db, "/XXX_PrimaryData.csv"),
  #   stringsAsFactors = FALSE,
  #   na.strings = c("", "NA")
  # ) %>%
  #   janitor::clean_names()

  # ---- CLEAN AND PREPARE DATA ----

  # TODO: Perform initial data cleaning and type conversions
  # Example:
  # primary_data <- primary_data %>%
  #   dplyr::mutate(
  #     # Convert date columns
  #     date_column = lubridate::dmy(.data$date_column),
  #     # Convert numeric columns
  #     numeric_column = as.numeric(.data$numeric_column),
  #     # Convert integer columns
  #     year = as.integer(.data$year),
  #     # Convert character columns
  #     id_column = as.character(.data$id_column)
  #   )

  # Apply species and population filters if specified
  # primary_data <- primary_data %>%
  #   dplyr::filter(
  #     .data$species %in% species_filter,
  #     .data$population %in% pop_filter
  #   )

  # ---- CREATE STANDARD FORMAT TABLES ----

  message("Compiling brood data...")
  Brood_data <- create_brood_XXX(
    data = primary_data,
    protocol_version = protocol_version
  )

  message("Compiling capture data...")
  Capture_data <- create_capture_XXX(
    data = primary_data,
    protocol_version = protocol_version
  )

  message("Compiling individual data...")
  Individual_data <- create_individual_XXX(
    Capture_data = Capture_data,
    Brood_data = Brood_data,
    protocol_version = protocol_version
  )

  message("Compiling location data...")
  Location_data <- create_location_XXX(
    data = primary_data,
    protocol_version = protocol_version
  )

  message("Compiling measurement data...")
  Measurement_data <- create_measurement_XXX(
    Capture_data = Capture_data,
    Brood_data = Brood_data,
    protocol_version = protocol_version
  )

  message("Compiling experiment data...")
  Experiment_data <- create_experiment_XXX(
    data = primary_data,
    protocol_version = protocol_version
  )

  # ---- FINALIZE AND VALIDATE ----

  # Ensure all tables conform to standard format
  Brood_data <- Brood_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Brood_data[
        1,
        !(names(data_templates[[paste0("v", protocol_version)]]$Brood_data)
          %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Brood_data)
    )

  Capture_data <- Capture_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Capture_data[
        1,
        !(names(data_templates[[paste0("v", protocol_version)]]$Capture_data)
          %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Capture_data)
    )

  Individual_data <- Individual_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Individual_data[
        1,
        !(names(data_templates[[paste0("v", protocol_version)]]$Individual_data)
          %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Individual_data)
    )

  Location_data <- Location_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Location_data[
        1,
        !(names(data_templates[[paste0("v", protocol_version)]]$Location_data)
          %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Location_data)
    )

  Measurement_data <- Measurement_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Measurement_data[
        1,
        !(names(
          data_templates[[paste0("v", protocol_version)]]$Measurement_data
        ) %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Measurement_data)
    )

  Experiment_data <- Experiment_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Experiment_data[
        1,
        !(names(
          data_templates[[paste0("v", protocol_version)]]$Experiment_data
        ) %in% names(.))
      ]
    ) %>%
    dplyr::select(
      names(data_templates[[paste0("v", protocol_version)]]$Experiment_data)
    )

  # Calculate processing time
  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # ---- EXPORT DATA ----

  if (output_type == "csv") {

    message("Saving .csv files...")

    utils::write.csv(
      x = Brood_data,
      file = paste0(path, "/Brood_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      x = Capture_data,
      file = paste0(path, "/Capture_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      x = Individual_data,
      file = paste0(path, "/Individual_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      x = Location_data,
      file = paste0(path, "/Location_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      x = Measurement_data,
      file = paste0(path, "/Measurement_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      x = Experiment_data,
      file = paste0(path, "/Experiment_data_XXX.csv"),
      row.names = FALSE
    )

    utils::write.table(
      x = protocol_version,
      file = paste0(path, "/protocol_version_XXX.txt"),
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )

    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

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

}


#' Create brood data table for [SITE NAME]
#'
#' Create brood data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param data Data frame. Primary data from [SITE NAME].
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with brood data in standard format.

create_brood_XXX <- function(data, protocol_version) {

  # TODO: Create brood data table
  # Key columns: row, broodID, speciesID, studyID, siteID
  #
  # Example structure:
  #
  # # Step 1: Get unique broods
  # Brood_data <- data %>%
  #   dplyr::distinct(.data$brood_id, .keep_all = TRUE)
  #
  # # Step 2: Add core identifiers
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     broodID = as.character(.data$brood_id),
  #     studyID = "XXX",
  #     siteID = "XXX",
  #     plotID = as.character(.data$plot),
  #     locationID = as.character(.data$location)
  #   )
  #
  # # Step 3: Convert species codes to scientific names
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     speciesID = dplyr::case_when(
  #       .data$species == "GRETI" ~ "Parus major",
  #       .data$species == "BLUTI" ~ "Cyanistes caeruleus",
  #       TRUE ~ NA_character_
  #     )
  #   )
  #
  # # Step 4: Add parent IDs
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     femaleID = as.character(.data$female_ring),
  #     maleID = as.character(.data$male_ring)
  #   )
  #
  # # Step 5: Add laying date components
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedLayYear = lubridate::year(.data$lay_date),
  #     observedLayMonth = lubridate::month(.data$lay_date),
  #     observedLayDay = lubridate::day(.data$lay_date)
  #   )
  #
  # # Step 6: Add clutch size
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedClutchSize = as.integer(.data$clutch_size)
  #   )
  #
  # # Step 7: Add hatch date components
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedHatchYear = lubridate::year(.data$hatch_date),
  #     observedHatchMonth = lubridate::month(.data$hatch_date),
  #     observedHatchDay = lubridate::day(.data$hatch_date)
  #   )
  #
  # # Step 8: Add brood size
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedBroodSize = as.integer(.data$brood_size)
  #   )
  #
  # # Step 9: Add fledge date components
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedFledgeYear = lubridate::year(.data$fledge_date),
  #     observedFledgeMonth = lubridate::month(.data$fledge_date),
  #     observedFledgeDay = lubridate::day(.data$fledge_date)
  #   )
  #
  # # Step 10: Add number fledged
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     observedNumberFledged = as.integer(.data$fledged)
  #   )
  #
  # # Step 11: Initialize quality check columns
  # Brood_data <- Brood_data %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )
  #
  # # Step 12: Select and order columns for output
  # Brood_data <- Brood_data %>%
  #   dplyr::select(
  #     "row", "broodID", "speciesID", "studyID", "siteID",
  #     "plotID", "locationID", "femaleID", "maleID",
  #     "observedClutchType",
  #     "observedLayYear", "observedLayMonth", "observedLayDay",
  #     "minimumLayYear", "minimumLayMonth", "minimumLayDay",
  #     "maximumLayYear", "maximumLayMonth", "maximumLayDay",
  #     "observedClutchSize", "minimumClutchSize", "maximumClutchSize",
  #     "observedHatchYear", "observedHatchMonth", "observedHatchDay",
  #     "minimumHatchYear", "minimumHatchMonth", "minimumHatchDay",
  #     "maximumHatchYear", "maximumHatchMonth", "maximumHatchDay",
  #     "observedBroodSize", "minimumBroodSize", "maximumBroodSize",
  #     "observedFledgeYear", "observedFledgeMonth", "observedFledgeDay",
  #     "minimumFledgeYear", "minimumFledgeMonth", "minimumFledgeDay",
  #     "maximumFledgeYear", "maximumFledgeMonth", "maximumFledgeDay",
  #     "observedNumberFledged", "minimumNumberFledged",
  #     "maximumNumberFledged", "treatmentID",
  #     "rowWarning", "rowError"
  #   )

  Brood_data <- tibble::tibble()

  return(Brood_data)

}


#' Create capture data table for [SITE NAME]
#'
#' Create capture data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param data Data frame. Primary data from [SITE NAME].
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with capture data in standard format.

create_capture_XXX <- function(data, protocol_version) {

  # TODO: Create capture data table
  # Key columns: row, captureID, individualID, releaseTagID, studyID,
  #              captureYear, captureSiteID, capturePhysical,
  #              captureAlive, releaseAlive
  #
  # Example structure:
  #
  # # Step 1: Filter to records with valid individual IDs
  # Capture_data <- data %>%
  #   dplyr::filter(!is.na(.data$ring_number))
  #
  # # Step 2: Create unique capture and individual identifiers
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     captureID = paste(.data$ring_number, .data$capture_date,
  #                       sep = "_"),
  #     individualID = as.character(.data$ring_number),
  #     captureTagID = as.character(.data$ring_number),
  #     releaseTagID = as.character(.data$ring_number)
  #   )
  #
  # # Step 3: Add species and study identifiers
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     speciesID = dplyr::case_when(
  #       .data$species == "GRETI" ~ "Parus major",
  #       .data$species == "BLUTI" ~ "Cyanistes caeruleus",
  #       TRUE ~ NA_character_
  #     ),
  #     studyID = "XXX"
  #   )
  #
  # # Step 4: Add observed sex
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     observedSex = dplyr::case_when(
  #       .data$sex == "M" ~ "M",
  #       .data$sex == "F" ~ "F",
  #       TRUE ~ NA_character_
  #     )
  #   )
  #
  # # Step 5: Extract capture date components
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     captureYear = lubridate::year(.data$capture_date),
  #     captureMonth = lubridate::month(.data$capture_date),
  #     captureDay = lubridate::day(.data$capture_date),
  #     captureTime = format(.data$capture_time, "%H:%M:%S")
  #   )
  #
  # # Step 6: Add observer and location information
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     recordedBy = as.character(.data$observer),
  #     captureSiteID = "XXX",
  #     releaseSiteID = "XXX",
  #     capturePlotID = as.character(.data$plot),
  #     releasePlotID = as.character(.data$plot),
  #     captureLocationID = as.character(.data$location),
  #     releaseLocationID = as.character(.data$location)
  #   )
  #
  # # Step 7: Add capture status flags
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     capturePhysical = TRUE,
  #     captureAlive = TRUE,
  #     releaseAlive = TRUE
  #   )
  #
  # # Step 8: Add age and treatment information
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     chickAge = as.integer(.data$chick_age),
  #     treatmentID = NA_character_
  #   )
  #
  # # Step 9: Initialize quality check columns
  # Capture_data <- Capture_data %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )
  #
  # # Step 10: Select and order columns for output
  # Capture_data <- Capture_data %>%
  #   dplyr::select(
  #     "row", "captureID", "individualID", "captureTagID",
  #     "releaseTagID", "speciesID", "studyID", "observedSex",
  #     "captureYear", "captureMonth", "captureDay", "captureTime",
  #     "recordedBy", "captureSiteID", "releaseSiteID",
  #     "capturePlotID", "releasePlotID",
  #     "captureLocationID", "releaseLocationID",
  #     "capturePhysical", "captureAlive", "releaseAlive",
  #     "chickAge", "treatmentID", "rowWarning", "rowError"
  #   )

  Capture_data <- tibble::tibble()

  return(Capture_data)

}


#' Create individual data table for [SITE NAME]
#'
#' Create individual data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param Capture_data Data frame. Capture data generated by
#'   \code{\link{create_capture_XXX}}.
#' @param Brood_data Data frame. Brood data generated by
#'   \code{\link{create_brood_XXX}}.
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with individual data in standard format.

create_individual_XXX <- function(Capture_data,
                                  Brood_data,
                                  protocol_version) {

  # TODO: Create individual data table
  # Key columns: row, individualID, speciesID, studyID, siteID
  #
  # Example structure:
  #
  # # Step 1: Summarize capture data by individual (first capture info)
  # Individual_data <- Capture_data %>%
  #   dplyr::group_by(.data$individualID) %>%
  #   dplyr::summarise(
  #     speciesID = dplyr::first(.data$speciesID),
  #     studyID = dplyr::first(.data$studyID),
  #     siteID = dplyr::first(.data$captureSiteID),
  #     tagYear = min(.data$captureYear, na.rm = TRUE),
  #     tagMonth = dplyr::first(.data$captureMonth[
  #       .data$captureYear == tagYear
  #     ]),
  #     tagDay = dplyr::first(.data$captureDay[
  #       .data$captureYear == tagYear
  #     ]),
  #     tagStage = dplyr::case_when(
  #       min(.data$chickAge, na.rm = TRUE) < 18 ~ "chick",
  #       TRUE ~ "adult"
  #     ),
  #     tagSiteID = dplyr::first(.data$captureSiteID[
  #       .data$captureYear == tagYear
  #     ])
  #   ) %>%
  #   dplyr::ungroup()
  #
  # # Step 2: Link individuals to their brood of origin (for chicks)
  # Individual_data <- Individual_data %>%
  #   dplyr::left_join(
  #     Brood_data %>%
  #       dplyr::select("broodID", "locationID", "observedHatchYear"),
  #     by = c("locationID", "tagYear" = "observedHatchYear")
  #   )
  #
  # # Step 3: Add brood identifiers (only for chicks)
  # Individual_data <- Individual_data %>%
  #   dplyr::mutate(
  #     broodIDLaid = dplyr::if_else(
  #       .data$tagStage == "chick",
  #       .data$broodID,
  #       NA_character_
  #     ),
  #     broodIDFledged = dplyr::if_else(
  #       .data$tagStage == "chick",
  #       .data$broodID,
  #       NA_character_
  #     )
  #   )
  #
  # # Step 4: Add row numbers and genetic sex
  # Individual_data <- Individual_data %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     geneticSex = NA_character_
  #   )
  #
  # # Step 5: Initialize quality check columns
  # Individual_data <- Individual_data %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )
  #
  # # Step 6: Select and order columns for output
  # Individual_data <- Individual_data %>%
  #   dplyr::select(
  #     "row", "individualID", "speciesID", "studyID", "siteID",
  #     "broodIDLaid", "broodIDFledged",
  #     "tagYear", "tagMonth", "tagDay", "tagStage", "tagSiteID",
  #     "geneticSex", "rowWarning", "rowError"
  #   )

  Individual_data <- tibble::tibble()

  return(Individual_data)

}


#' Create location data table for [SITE NAME]
#'
#' Create location data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param data Data frame. Primary data from [SITE NAME].
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with location data in standard format.

create_location_XXX <- function(data, protocol_version) {

  # TODO: Create location data table
  # Key columns: row, locationID, locationType, studyID, siteID
  #
  # Example structure:
  #
  # # Step 1: Extract unique locations
  # Location_data <- data %>%
  #   dplyr::select("location", "latitude", "longitude", "year") %>%
  #   dplyr::distinct(.data$location, .keep_all = TRUE)
  #
  # # Step 2: Add core identifiers
  # Location_data <- Location_data %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     locationID = as.character(.data$location),
  #     locationType = "nest",
  #     locationDetails = paste("Nestbox", .data$location),
  #     studyID = "XXX",
  #     siteID = "XXX"
  #   )
  #
  # # Step 3: Add geographic coordinates
  # Location_data <- Location_data %>%
  #   dplyr::mutate(
  #     decimalLatitude = as.numeric(.data$latitude),
  #     decimalLongitude = as.numeric(.data$longitude),
  #     elevation = NA_real_
  #   )
  #
  # # Step 4: Determine active years for each location
  # Location_data <- Location_data %>%
  #   dplyr::mutate(
  #     startYear = min(.data$year, na.rm = TRUE),
  #     endYear = NA_integer_
  #   )
  #
  # # Step 5: Initialize quality check columns
  # Location_data <- Location_data %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )
  #
  # # Step 6: Select and order columns for output
  # Location_data <- Location_data %>%
  #   dplyr::select(
  #     "row", "locationID", "locationType", "locationDetails",
  #     "studyID", "siteID", "decimalLatitude", "decimalLongitude",
  #     "elevation", "startYear", "endYear",
  #     "rowWarning", "rowError"
  #   )

  Location_data <- tibble::tibble()

  return(Location_data)

}


#' Create measurement data table for [SITE NAME]
#'
#' Create measurement data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param Capture_data Data frame. Capture data generated by
#'   \code{\link{create_capture_XXX}}.
#' @param Brood_data Data frame. Brood data generated by
#'   \code{\link{create_brood_XXX}}.
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with measurement data in standard format.

create_measurement_XXX <- function(Capture_data,
                                   Brood_data,
                                   protocol_version) {

  # TODO: Create measurement data table
  # Key columns: row, measurementID, recordID, studyID, siteID,
  #              measurementSubject, measurementType, measurementValue,
  #              measurementUnit, measurementDeterminedYear
  #
  # This table stores morphological measurements (mass, tarsus, wing length)
  # from captures, as well as clutch-level and brood-level measurements.
  #
  # Example structure for capture measurements:
  #
  # # Step 1: Select relevant capture data with measurements
  # Capture_measurements <- Capture_data %>%
  #   dplyr::select(
  #     "captureID", "studyID", "captureSiteID",
  #     "captureYear", "captureMonth", "captureDay", "captureTime",
  #     "recordedBy", "mass", "tarsus", "wing"
  #   )
  #
  # # Step 2: Reshape from wide to long format (one row per measurement)
  # Capture_measurements <- Capture_measurements %>%
  #   tidyr::pivot_longer(
  #     cols = c("mass", "tarsus", "wing"),
  #     names_to = "measurementType",
  #     values_to = "measurementValue"
  #   ) %>%
  #   dplyr::filter(!is.na(.data$measurementValue))
  #
  # # Step 3: Create measurement identifiers
  # Capture_measurements <- Capture_measurements %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     measurementID = paste(.data$captureID, .data$measurementType,
  #                           sep = "_"),
  #     recordID = .data$captureID,
  #     siteID = .data$captureSiteID,
  #     measurementSubject = "capture"
  #   )
  #
  # # Step 4: Standardize measurement type names
  # Capture_measurements <- Capture_measurements %>%
  #   dplyr::mutate(
  #     measurementType = dplyr::case_when(
  #       .data$measurementType == "mass" ~ "body mass",
  #       .data$measurementType == "tarsus" ~ "tarsus length",
  #       .data$measurementType == "wing" ~ "wing length",
  #       TRUE ~ .data$measurementType
  #     )
  #   )
  #
  # # Step 5: Add measurement units based on type
  # Capture_measurements <- Capture_measurements %>%
  #   dplyr::mutate(
  #     measurementUnit = dplyr::case_when(
  #       .data$measurementType == "body mass" ~ "g",
  #       .data$measurementType == "tarsus length" ~ "mm",
  #       .data$measurementType == "wing length" ~ "mm",
  #       TRUE ~ NA_character_
  #     ),
  #     measurementAccuracy = NA_real_,
  #     measurementMethod = NA_character_
  #   )
  #
  # # Step 6: Add measurement date/time information
  # Capture_measurements <- Capture_measurements %>%
  #   dplyr::mutate(
  #     measurementDeterminedYear = .data$captureYear,
  #     measurementDeterminedMonth = .data$captureMonth,
  #     measurementDeterminedDay = .data$captureDay,
  #     measurementDeterminedTime = .data$captureTime
  #   )
  #
  # # Step 7: Initialize quality check columns
  # Capture_measurements <- Capture_measurements %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )

  Measurement_data <- tibble::tibble()

  return(Measurement_data)

}


#' Create experiment data table for [SITE NAME]
#'
#' Create experiment data table in standard format (v2.0.0) for data from
#' [SITE NAME].
#'
#' @param data Data frame. Primary data from [SITE NAME].
#' @param protocol_version Character string. The version of the standard
#'   protocol on which this pipeline is based.
#'
#' @return A data frame with experiment data in standard format.

create_experiment_XXX <- function(data, protocol_version) {

  # TODO: Create experiment data table
  # Key columns: row, treatmentID, experimentID, studyID, siteID,
  #              experimentType, treatmentDetails, treatmentStartYear,
  #              treatmentEndYear
  #
  # This table stores information about experimental manipulations.
  # If there are no experiments, return an empty tibble with the correct
  # structure.
  #
  # Example structure:
  #
  # # Step 1: Filter to records with experiment information
  # Experiment_data <- data %>%
  #   dplyr::filter(!is.na(.data$experiment_id)) %>%
  #   dplyr::distinct(.data$treatment_id, .keep_all = TRUE)
  #
  # # Step 2: Add core identifiers
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     row = dplyr::row_number(),
  #     treatmentID = as.character(.data$treatment_id),
  #     experimentID = as.character(.data$experiment_id),
  #     studyID = "XXX",
  #     siteID = "XXX"
  #   )
  #
  # # Step 3: Add experiment classification
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     experimentType = as.character(.data$experiment_type),
  #     treatmentDetails = as.character(.data$treatment_description),
  #     treatmentStage = NA_character_
  #   )
  #
  # # Step 4: Extract treatment start date components
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     treatmentStartYear = lubridate::year(.data$treatment_start_date),
  #     treatmentStartMonth = lubridate::month(.data$treatment_start_date),
  #     treatmentStartDay = lubridate::day(.data$treatment_start_date),
  #     treatmentStartTime = NA_character_
  #   )
  #
  # # Step 5: Extract treatment end date components
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     treatmentEndYear = lubridate::year(.data$treatment_end_date),
  #     treatmentEndMonth = lubridate::month(.data$treatment_end_date),
  #     treatmentEndDay = lubridate::day(.data$treatment_end_date),
  #     treatmentEndTime = NA_character_
  #   )
  #
  # # Step 6: Add observer and reference information
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     recordedBy = as.character(.data$observer),
  #     reference = NA_character_
  #   )
  #
  # # Step 7: Initialize quality check columns
  # Experiment_data <- Experiment_data %>%
  #   dplyr::mutate(
  #     rowWarning = NA,
  #     rowError = NA
  #   )
  #
  # # Step 8: Select and order columns for output
  # Experiment_data <- Experiment_data %>%
  #   dplyr::select(
  #     "row", "treatmentID", "experimentID", "studyID", "siteID",
  #     "experimentType", "treatmentDetails",
  #     "treatmentStartYear", "treatmentStartMonth", "treatmentStartDay",
  #     "treatmentStartTime",
  #     "treatmentEndYear", "treatmentEndMonth", "treatmentEndDay",
  #     "treatmentEndTime", "treatmentStage",
  #     "recordedBy", "reference", "rowWarning", "rowError"
  #   )

  Experiment_data <- tibble::tibble()

  return(Experiment_data)

}
