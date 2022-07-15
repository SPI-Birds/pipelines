#' Construct standard format for data from Santo Stefano Quisquina, Italy.
#'
#' A pipeline to produce the standard format for the great and blue tit population
#' in Santo Stefano Quisquina, Sicly, Italy, administered by Camillo Cusimano
#' and Daniela Campobello.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#' \strong{broodID}: Unique broodID is constructed using:
#' year_locationID_layDate, where layDate is in March days.
#'
#' \strong{locationID, startYear}: Some nest boxes were replaced over the course of
#' the study. However, this was not done explicitly (IDs remain the same).
#' Therefore, we use nest box number as locationID, and all nest boxes are listed
#' as functional for the full study period. Check with data owner.
#'
#' \strong{speciesID}: In the individual data, there are some cases where an
#' individualID is associated with >1 species. These are considered conflicted species.
#'
#' \strong{captureDate}: No exact capture date is currently given. For adults we
#' use the start of incubation (laying date + clutch size) as a proxy for capture date.
#' Chicks were only ever captured on the nest, we used laying date + clutch size + 15 days
#' incubation + 12 days. This is because chicks were ringed at 12 days old at
#' the latest.
#'
#' \strong{Individual_data}: There are cases where chicks from different nests are
#' given the same ring number. Unsure if this is the rings being reused or a
#' typo. Currently, I leave it as is and assume this is a typo that needs to be
#' fixed in the primary data.
#'
#' \strong{habitatID}: Assume that habitat type is 1.4: Forest - Temperate. Check with data owner.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format.
#' @export
#'

format_SSQ <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       output_type = "R"){

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) {

    optional_variables <- names(unlist(unname(utility_variables)))

  }

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  message("Importing primary data...")

  # Read in data with readxl
  all_data <- readxl::read_excel(path = paste0(db, "/SSQ_PrimaryData.xlsx")) %>%
    # Clean all names with janitor to snake_case
    janitor::clean_names() %>%
    # Remove the column 'row'. This is just the row number, we have this already.
    dplyr::select(-.data$row) %>%
    janitor::remove_empty(which = "rows") %>%
    # Create IDs
    dplyr::mutate(year = as.integer(.data$year),
                  femaleID = .data$f_id,
                  maleID = .data$m_id,
                  # Pad nest ID so they are all the same length
                  locationID = stringr::str_pad(.data$nest_id,
                                                width = 3,
                                                pad = "0"),
                  plotID = dplyr::case_when(is.na(.data$habitat_of_ringing) ~ NA_character_,
                                            TRUE ~ paste0("SSQ_", .data$habitat_of_ringing)),
                  speciesID = dplyr::case_when(.data$species == "Parus major" ~ species_codes[species_codes$speciesCode == 10001, ]$speciesID,
                                               .data$species == "Cyanistes caeruleus" ~ species_codes[species_codes$speciesCode == 10002, ]$speciesID),
                  siteID = "SSQ",
                  # broodID: year_locationID_layDate
                  broodID = paste(.data$year,
                                  .data$locationID,
                                  stringr::str_pad(.data$ld, width = 3, pad = "0"),
                                  sep = "_")) %>%
    # Filter species
    dplyr::filter(.data$speciesID %in% {{species}})


  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_SSQ(all_data, optional_variables)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_SSQ(all_data, optional_variables)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_SSQ(Capture_data, optional_variables)


  # LOCATION DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_SSQ(all_data)


  # MEASUREMENT DATA

  message("Compiling measurement information...")

  # NB: There is no measurement information so we create an empty data table
  Measurement_data <- data_templates$v1.2$Measurement_data[0,]


  # EXPERIMENT DATA

  message("Compiling experiment information...")

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v1.2$Experiment_data[0,]


  # WRANGLE DATA FOR EXPORT

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Capture_data[1, !(names(data_templates$v1.2$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                         ignore.case = FALSE))

  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Individual_data[1, !(names(data_templates$v1.2$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                           ignore.case = FALSE))

  Location_data <- Location_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Location_data[1, !(names(data_templates$v1.2$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Location_data))


  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_SSQ.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_SSQ.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_SSQ.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_SSQ.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_SSQ.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_SSQ.csv"), row.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data,
                Experiment_data = Experiment_data))

  }

}

#' Create brood data table for Santo Stefano Quisquina, Italy.
#'
#' Create brood data table in standard format for data from Santo Stefano
#' Quisquina, Italy.
#'
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_SSQ <- function(data,
                             optional_variables = NULL){

  Brood_data <- data %>%
    # Convert dates from March days (1 = March 1st) to calendar dates
    dplyr::mutate(observedLayDate = as.Date(paste(.data$year, "03-01", sep = "-", format = "%Y-%m-%d")) + .data$ld - 1,
                  observedLayYear = as.integer(lubridate::year(.data$observedLayDate)),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedClutchSize = as.integer(.data$cs),
                  observedHatchDate = as.Date(paste(.data$year, "03-01", sep = "-", format = "%Y-%m-%d")) + .data$hd - 1,
                  observedHatchYear = as.integer(lubridate::year(.data$observedHatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedBroodSize = as.integer(.data$hs),
                  observedNumberFledged = as.integer(.data$fs),
                  observedClutchType = dplyr::case_when(.data$class == 1 ~ "first",
                                                        .data$class == 3 ~ "second",
                                                        .data$class == 2 ~ "replacement")) %>%
    # Arrange data
    dplyr::arrange(.data$observedLayYear, .data$femaleID, .data$observedLayDate) %>%
    # Remove brood with unknown year
    dplyr::filter(!is.na(.data$observedLayYear))

  # Add optional variables
  output <- Brood_data %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = .,
                                                              season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = .,
                                                                        na.rm = FALSE,
                                                                        protocol_version = "1.2") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = .,
                                                                      season = .data$breedingSeason) else .}

  return(output)

}

#' Create capture data table for Santo Stefano Quisquina, Italy.
#'
#' Create capture data table in standard format for data from Santo Stefano
#' Quisquina, Italy.
#'
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_SSQ <- function(data,
                               optional_variables = NULL){

  Adult_captures <- data %>%
    # Pivot information on females and males into rows
    tidyr::pivot_longer(cols = c(.data$femaleID, .data$maleID),
                        values_to = "individualID",
                        names_to = "sex") %>%
    # If individualID differs from expected format, set to NA
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(.data$individualID,
                                                                      "^[:alnum:]{7}$") ~ .data$individualID,
                                                  TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(is.na(.data$individualID) ~ NA_character_,
                                                  TRUE ~ paste0("SSQ_", .data$individualID))) %>%
    # Remove unknown individuals
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::mutate(observedSex = dplyr::case_when(.data$sex == "femaleID" ~ "F",
                                                 .data$sex == "maleID" ~ "M"),
                  age = dplyr::case_when(.data$observedSex == "F" ~ as.integer(.data$f_age),
                                         .data$observedSex == "M" ~ as.integer(.data$m_age)),
                  age = dplyr::case_when(.data$age == 1 ~ "subadult",
                                         .data$age == 2 ~ "adult"),
                  # Treat capture date as the start of incubation (i.e., laying date + clutch size)
                  captureDate = as.Date(paste(.data$year, "03-01",
                                              sep = "-", format = "%Y-%m-%d")) + .data$ld + .data$cs - 1,
                  captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)))

  #Also extract chick capture information
  Chick_captures <- data %>%
    # Pivot information on chicks into rows
    tidyr::pivot_longer(cols = c(.data$chick1_id:.data$chick13_id),
                        names_to = "chickNumber",
                        values_to = "individualID") %>%
    # If individualID differs from expected format, set to NA
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(.data$individualID,
                                                                      "^[:alnum:]{7}$") ~ .data$individualID,
                                                  TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(is.na(.data$individualID) ~ NA_character_,
                                                  TRUE ~ paste0("SSQ_", .data$individualID))) %>%
    # Remove unknown individuals
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::mutate(age = "chick",
                  # For chicks, we currently don't have the version of the individual level capture data.
                  # For now, we use lay date + clutch size + 15 (incubation days in SSQ) + 12.
                  # Chicks were captured and weighed at 12 days old at the latest
                  captureDate = as.Date(paste(.data$year, "03-01",
                                              sep = "-", format = "%Y-%m-%d")) + .data$ld + .data$cs + 27 - 1,
                  captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)))

  # Combine adult and chick data
  Capture_data <- dplyr::bind_rows(Adult_captures, Chick_captures) %>%
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    dplyr::mutate(captureSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releaseSiteID = .data$captureSiteID,
                  releasePlotID = .data$capturePlotID,
                  chickAge = NA_integer_) %>%
    dplyr::group_by(.data$individualID) %>%
    # First captures are assumed to be ringing events, and thus captureRingNumber = NA
    dplyr::mutate(captureRingNumber = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       TRUE ~ stringr::str_sub(.data$individualID, 5,
                                                                               nchar(.data$individualID))),
                  # All releases are assumed to be alive (also see releaseAlive), so no NAs in releaseRingNumber
                  releaseRingNumber = stringr::str_sub(.data$individualID, 5, nchar(.data$individualID)),
                  # Create captureID
                  captureID = paste(.data$individualID, 1:n(), sep = "_")) %>%
    dplyr::ungroup()


  # Add optional variables
  output <- Capture_data %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            Year = .data$year,
                                                                                            protocol_version = "1.2") %>% dplyr::select(dplyr::contains(c(names(Capture_data), optional_variables))) else .}

  return(output)

}

#' Create individual data table for Santo Stefano Quisquina, Italy.
#'
#' Create individual data table in standard format for data from Santo Stefano
#' Quisquina, Italy.
#'
#' @param Capture_data Data frame. Generate by \code{\link{create_capture_SSQ}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_individual_SSQ <- function(Capture_data,
                                  optional_variables = NULL){

  # Create a list of all individuals
  Individual_data <- Capture_data %>%
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(speciesID = dplyr::case_when(length(unique(.data$speciesID)) == 2 ~ "CCCCCC",
                                                  TRUE ~ dplyr::first(.data$speciesID)),
                     ringDate = dplyr::first(.data$captureDate),
                     ringYear = as.integer(lubridate::year(.data$ringDate)),
                     ringMonth = as.integer(lubridate::month(.data$ringDate)),
                     ringDay = as.integer(lubridate::day(.data$ringDate)),
                     ringStage = dplyr::case_when(is.na(dplyr::first(.data$age)) ~ "adult",
                                                  TRUE ~ dplyr::first(.data$age)),
                     ringSiteID = dplyr::first(.data$siteID),
                     firstBrood = dplyr::first(.data$broodID),
                     .groups = "drop") %>%
    # Determine broodIDLaid and broodIDFledged for individuals ringed as chicks
    dplyr::mutate(broodIDLaid = dplyr::case_when(.data$ringStage == "chick" ~ .data$firstBrood,
                                                 TRUE ~ NA_character_),
                  # We have no information on cross-fostering, so we assume the brood laid and ringed are the same
                  broodIDFledged = .data$broodIDLaid,
                  siteID = "SSQ")

    # Add optional variables
    output <- Individual_data %>%
      {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                            capture_data = Capture_data) else .}

    return(output)

}

#' Create location data table for Santo Stefano Quisquina, Italy.
#'
#' Create location data table in standard format for data from Santo Stefano
#' Quisquina, Italy.
#'
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#'
#' @return A data frame.
#'

create_location_SSQ <- function(data){

  Location_data <- data %>%
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(startYear = min(data$year),
                     # Some nest boxes were replaced over the course of study,
                     # such that a single locationID has varying coordinates
                     # TODO: Check with data owner
                     # For now, we use the first recorded coordinates for
                     # each nest box
                     decimalLatitude = dplyr::first(.data$y_coord),
                     decimalLongitude = dplyr::first(.data$x_coord),
                     .groups = "drop") %>%
    dplyr::mutate(locationType = "nest",
                  siteID = "SSQ",
                  # TODO: Check habitat type with data owner
                  habitatID = "1.4")

  return(Location_data)

}

#----------------------#
#TODO: Check multiple different latitude & longitude records for single nest boxes. See e.g. nest_id: 11
#TODO: Verify habitatID
