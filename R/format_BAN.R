#'Construct standard format for data from Bandon Valley, Ireland.
#'
#'A pipeline to produce the standard format for the nest box population in Bandon
#'Valley, Ireland, administered by John Quinn.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{speciesID}: There are records where species is uncertain (e.g. listed as
#''GRETI?'). This uncertainty is ignored. We assume that the suggested species
#'is correct. We include blue tit, great tit, and coal tit. No other species are
#'recorded.
#'
#'\strong{observedClutchType}: Clutch type is only listed as 'first' and
#''second'. There is no distinction between 'second' and 'replacement'. We
#'categorise all these as 'second'. Possible distinction between 'second' and
#''replacement' can be made with \strong{calculatedClutchType}. There are a
#'small number of cases where nest attempt number is uncertain (e.g.
#'`2(MAYBE)`). This uncertainty is ignored.
#'
#'\strong{Lay date, hatch date, number fledged}: There are some cases where
#'values are given with uncertainty (e.g. 97+, 95?). We don't know how much
#'uncertainty is involved here, it is currently ignored, but we need to
#'discuss this with data custodian.
#'
#'\strong{Clutch size}: Cases where clutch size is uncertain (e.g. nests were
#'predated before completion) are treated as NA because clutch size is unknown.
#'Cases where clutch size is of the form "4 or more" or "at least 5", observedClutchSize
#'is set to the observed value (i.e., 4 or 5, respectively) and maximumClutchSize
#'is set to Inf because the final clutch size has not been observed.
#'
#'\strong{plotID}: The institutionID (i.e., "BAN") is added to the plot names to ensure
#'unique plotIDs across pipelines.
#'
#'\strong{locationID}: Box numbers are not unique, they are repeated between
#'plots. To generate locationID, we therefore use plotID_nestboxNumber
#'
#'\strong{broodID}: Unique BroodID is currently made with
#'year_plotName_nestboxNumber_layDate, where plotName is plotID without the institutionID,
#'and layDate is in March days (i.e., 1 = March 1st)
#'
#'\strong{broodIDLaid, broodIDFledged}: Currently, we have no information about the
#'brood where each individual was laid. Therefore, these are currently left as NA.
#'
#'\strong{captureAlive, releaseAlive}: Assume all individuals were alive when captured and released.
#'
#'\strong{capturePhysical}: Assume all individuals were physically captured.
#'
#'\strong{captureTagID}: First captures of all individuals are assumed to be ringing events, and thus captureTagID is set to NA.
#'
#'\strong{ringStage}: Assume all individuals were ringed as adults.
#'
#'\strong{startYear}: Assume all boxes were placed in the first year of the study.
#'
#'\strong{habitatID}: Currently unknown, check with data custodian.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'
#'@export
#'

format_BAN <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       study = NULL,
                       optional_variables = NULL,
                       output_type = 'R'){

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  message("Importing primary data...")

  # Warnings arise when we coerce records like 'UNKNOWN' into numeric (making NA by coercion)
  # We want this behaviour, so we hide the warnings.
  all_data <- suppressWarnings(readxl::read_excel(paste0(db, "/BAN_PrimaryData.xlsx")) %>%
                                 # Convert all cols to snake_case
                                 janitor::clean_names() %>%
                                 dplyr::mutate(dplyr::across(.cols = everything(),
                                                             .fns = ~{

                                                               replace(x = .x,
                                                                       list = .x %in% c("NA", "na", "unknown"),
                                                                       values = NA)

                                                             })) %>%
                                 # Convert column names to match standard format
                                 dplyr::mutate(studyID = "BAN-1",
                                               siteID = "BAN",
                                               plotID = paste0("BAN_", .data$site),
                                               # Create a unique locationID using plot and box number
                                               locationID = paste(.data$site,
                                                                  stringr::str_pad(string = .data$box_number,
                                                                                   width = 3,
                                                                                   pad = "0"), sep = "_"),
                                               # Ignore uncertainty in species (e.g. GRETI?)
                                               # TODO: Need to check with data custodian
                                               speciesID = dplyr::case_when(grepl(pattern = "GRETI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10001, ]$speciesID,
                                                                            grepl(pattern = "BLUTI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10002, ]$speciesID,
                                                                            grepl(pattern = "COATI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10005, ]$speciesID),
                                               # Create a unique broodID from year, locationID, lay date (in March days)
                                               broodID = paste(.data$year,
                                                               .data$locationID,
                                                               .data$first_egg_lay_date,
                                                               sep = "_"),
                                               # If maleID & femaleID differ from expected format, set to NA
                                               # Ensure that individuals are unique: add institutionID as prefix
                                               maleID = .data$male_id,
                                               femaleID = .data$female_id,
                                               dplyr::across(.cols = c(.data$maleID, .data$femaleID),
                                                             .fns = ~{

                                                               dplyr::case_when(stringr::str_detect(.x, "^[:alpha:]{1,3}[:digit:]{4,6}$") ~ paste0("BAN_", .x),
                                                                                TRUE ~ NA_character_)

                                                             }),
                                               # Dates are recorded in March days, i.e., 1 = March 1st
                                               # Create marchDate as baseline for other dates
                                               # We need to do March 1st - 1 + date to get corresponding calendar date
                                               # (can't use end of Feb + date because of leap years)
                                               marchDate = as.Date(paste0(.data$year, '-03-01'), format = "%Y-%m-%d"),
                                               maleCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_male_trapping_date),
                                               femaleCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_female_trapping_date),
                                               chickCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_pullus_ringing_date)) %>%
                                 # Filter only the species of interest
                                 dplyr::filter(.data$speciesID %in% {{species}}))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_BAN(all_data,
                                 optional_variables = optional_variables)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_BAN(all_data,
                                     optional_variables = optional_variables)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_BAN(Capture_data,
                                           optional_variables = optional_variables)


  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_BAN(all_data)


  # MEASUREMENT DATA

  message("Compiling measurement information...")

  # NB: There is no measurement information so we create an empty data table
  Measurement_data <- data_templates$v2.0$Measurement_data[0,]


  # EXPERIMENT DATA

  message("Compiling experiment information...")

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v2.0$Experiment_data[0,]


  # WRANGLE DATA FOR EXPORT

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[1, !(names(data_templates$v2.0$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Brood_data), any_of(names(utility_variables$Brood_data)))

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[1, !(names(data_templates$v2.0$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Capture_data), any_of(names(utility_variables$Capture_data)))

  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[1, !(names(data_templates$v2.0$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Individual_data), any_of(names(utility_variables$Individual_data)))

  Location_data <- Location_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Location_data[1, !(names(data_templates$v2.0$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Location_data))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == 'csv'){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_BAN.csv"), row.names = F)

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

#' Create brood data table for Bandon Valley, Ireland.
#'
#' Create brood data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param data Data frame. Primary data from Bandon Valley.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_BAN <- function(data,
                             optional_variables = NULL) {

  Brood_data <- data %>%
    # Ignore uncertainty in clutch type (e.g., 2(MAYBE))
    dplyr::mutate(observedClutchType = dplyr::case_when(grepl(pattern = 1, x = .data$nest_attempt) ~ "first",
                                                        grepl(pattern = 2, x = .data$nest_attempt) ~ "second"),
                  # Ignore uncertainty in lay date (e.g., 97? or 97+)
                  # TODO: Need to check with data custodian
                  observedLayDate = dplyr::case_when(is.na(.data$first_egg_lay_date) ~ as.Date(NA),
                                                     stringr::str_detect(.data$first_egg_lay_date, "^\\?") ~ as.Date(NA),
                                                     TRUE ~ .data$marchDate - 1 + as.numeric(stringr::str_remove(.data$first_egg_lay_date, "\\?|\\+"))),
                  observedLayYear = as.integer(lubridate::year(.data$observedLayDate)),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedClutchSize = dplyr::case_when(stringr::str_detect(.data$final_clutch_size, "[:digit:]+") ~ as.integer(stringr::str_extract(.data$final_clutch_size, "[:digit:]+")),
                                                        TRUE ~ NA_integer_),
                  # Clutch sizes of the form "4 or more" or "at least 5" have Inf as maximum clutch size
                  maximumClutchSize = dplyr::case_when(stringr::str_detect(.data$final_clutch_size, "more|least") & !is.na(.data$final_clutch_size) ~ Inf,
                                                       TRUE ~ NA_real_),
                  observedHatchDate = dplyr::case_when(is.na(.data$actual_hatch_date) ~ as.Date(NA),
                                                       TRUE ~ .data$marchDate - 1 + as.numeric(stringr::str_remove(.data$actual_hatch_date, "nA"))),
                  observedHatchYear = as.integer(lubridate::year(.data$observedHatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  # Ignore uncertainty in number fledged (e.g., 0?)
                  observedNumberFledged = as.integer(gsub(pattern = "\\?",
                                                          replacement = "",
                                                          x = .data$number_fledged)))

  # Add optional variables
  output <- Brood_data %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = .,
                                                              season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE,
                                                                        protocol_version = "2.0") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = .,
                                                                      season = .data$breedingSeason) else .}

  return(output)

}

#' Create capture data table for Bandon Valley, Ireland.
#'
#' Create capture data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param data Data frame. Primary data from Bandon Valley.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_BAN <- function(data,
                               optional_variables = NULL) {

  Capture_data <- data %>%
    dplyr::select("studyID", "speciesID", "year", "locationID", "plotID",
                  "femaleCaptureDate", "maleCaptureDate",
                  "femaleID", "maleID") %>%
    tidyr::pivot_longer(cols = c("femaleID", "maleID"),
                        names_to = "sex",
                        values_to = "individualID") %>%
    dplyr::arrange(.data$sex) %>%
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::mutate(observedSex = dplyr::case_when(grepl(pattern = "female", .data$sex) ~ "F",
                                                 grepl(pattern = "male", .data$sex) ~ "M"),
                  captureDate = dplyr::case_when(.data$observedSex == "F" ~ .data$femaleCaptureDate,
                                                 .data$observedSex == "M" ~ .data$maleCaptureDate),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ as.integer(.data$year),
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  capturePhysical = TRUE,
                  captureSiteID = "BAN",
                  releaseSiteID = "BAN",
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  captureLocationID = .data$locationID,
                  releaseLocationID = .data$locationID,
                  chickAge = NA_integer_,
                  age = NA_character_) %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    # First captures are assumed to be ringing events, and thus captureTagID = NA.
    dplyr::mutate(captureTagID = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       TRUE ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  # All releases are assumed to be alive (also see releaseAlive), so no NAs in releaseTagID
                  releaseTagID = stringr::str_sub(.data$individualID, 5, nchar(.data$individualID)),
                  # Create captureID
                  captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("captureID", tidyselect::everything())

  # Add optional variables
  output <- Capture_data %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            protocol_version = "2.0") %>%
        dplyr::select(dplyr::contains(c(names(Capture_data), optional_variables))) else .}

  return(output)

}

#' Create individual data table for Bandon Valley, Ireland.
#'
#' Create individual data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_BAN}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_individual_BAN <- function(Capture_data,
                                  optional_variables = NULL) {

  Individual_data <- Capture_data %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(speciesID = unique(stats::na.omit(.data$speciesID)),
                     tagDate = dplyr::first(.data$captureDate),
                     tagYear = as.integer(dplyr::first(.data$year)),
                     tagMonth = as.integer(lubridate::month(.data$tagDate)),
                     tagDay = as.integer(lubridate::day(.data$tagDate)),
                     tagSiteID = dplyr::first(.data$captureSiteID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(studyID = "BAN-1",
                  siteID = "BAN",
                  broodIDLaid = NA_character_,
                  broodIDFledged = NA_character_,
                  tagStage = "adult")

  # Add optional variables
  output <- Individual_data %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = ., capture_data = Capture_data) else .}

  return(output)

}

#' Create location data table for Bandon Valley, Ireland.
#'
#' Create location data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.
#'

create_location_BAN <- function(data) {

  Location_data <- tibble::tibble(locationID = unique(data$locationID),
                                  locationType = "nest",
                                  studyID = "BAN-1",
                                  siteID = "BAN",
                                  startYear = as.integer(min(data$year)),
                                  # TODO: Check habitat type with data custodian
                                  habitatID = NA_character_)

  return(Location_data)

}
