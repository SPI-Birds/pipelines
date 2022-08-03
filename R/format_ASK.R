#'Construct standard format for data from Askainen, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population in Askainen, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to this data. For a general description of the standard protocol please see \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#'@inheritParams pipeline_params
#'
#'\strong{Species:} We only include records for species with at least 50 broods throughout the study period: great tit, pied flycatcher, blue tit, coal tit, Eurasian wryneck, common redstart. The few records for common starling, European crested tit, and imprecise species (e.g., PARXXX) are excluded.
#'
#'\strong{Minimum & maximum lay & hatch dates}: Accuracy of lay and hatch date are given as categories: 1 = 0-1, 2 = 1-2, 3 = 2-3, 4 = inaccurate. Where error is a range, the more conservative error is used (i.e., 0-1 is recorded as 1). Cases listed as 'inaccurate' have an error of at least a week. Dates in these cases are highly inaccurate and shouldn't be considered for any phenology analysis.
#'
#'\strong{captureDate}:  No exact capture date is given. For adults we use the start of incubation (laying date + clutch size) as a proxy for capture date, or the laying date if clutch size is unknown.
#'
#'\strong{captureAlive, releaseAlive}: All individuals are assumed to be captured and released alive.
#'
#'\strong{captureRingNumber}: First captures of all individuals are assumed to be ringing events, and thus captureRingNumber is set to NA.
#'
#'\strong{ringStage}: Individuals that are not caught as a chick on the nest are assumed to be ringed as "subadult". This way, calculation of their minimum age is the most conservative.
#'
#'\strong{startYear, endYear}: Assume all boxes were placed in the first year of study and stopped being used in the last year of study.
#'
#'\strong{habitatID}: Assume that habitat type is 1.1: Forest - Boreal. Check with data owner.
#'
#'\strong{Measurement data, Experiment data}: No measurements were taken of individuals or locations, nor any experiments conducted, resulting in empty Measurement data and Experiment data tables.
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export
#'

format_ASK <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

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

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_ASK(db = db,
                                 species_filter = species,
                                 optional_variables = optional_variables)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_ASK(db = db,
                                     brood_data = Brood_data,
                                     species_filter = species,
                                     optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_ASK(capture_data = Capture_data,
                                           species_filter = species,
                                           optional_variables = optional_variables)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_ASK(db = db)


  # MEASUREMENT DATA

  # NB: There is no measurement information so we create an empty data table
  Measurement_data <- data_templates$v1.2$Measurement_data[0,]


  # EXPERIMENT DATA

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v1.2$Experiment_data[0,]


  # WRANGLE DATA FOR EXPORT

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Capture_data[1, !(names(data_templates$v1.2$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Individual_data[1, !(names(data_templates$v1.2$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

  Location_data <- Location_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Location_data[1, !(names(data_templates$v1.2$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Location_data))


  # TIME

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  # OUTPUT

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_DLO.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_DLO.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_DLO.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_DLO.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_DLO.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_DLO.csv"), row.names = FALSE)

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


#' Create brood data table for Askainen, Finland.
#'
#' Create brood data table in standard format for data from Askainen, Finland.
#'
#' @param db Location of primary data from Askainen.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_ASK <- function(db,
                             species_filter,
                             optional_variables) {

  message("Extracting brood data from paradox database...")

  # Pesat.DB contains the basic breeding parameters for each nest.
  broods <- extract_paradox_db(path = db, file_name = "ASK_PrimaryData_Pesat.DB") %>%
    # Rename columns to English (based on description provided by data owner)
    # -- Anro: Nest number in same box (1 = first nest in box, 2 = second nest in same box)
    # -- Ltar: unknown variables
    dplyr::rename(nestID = .data$Diario,
                  year = .data$Vuos,
                  locationID = .data$Nuro,
                  speciesID = .data$Laji,
                  nestAttemptBox = .data$Anro,
                  observedClutchType = .data$Pesa,
                  femaleID = .data$Naaras,
                  maleID = .data$Koiras,
                  observedLayDay = .data$Mpv,
                  observedLayMonth = .data$Mkk,
                  errorLayDay = .data$Mtar,
                  observedHatchDay = .data$Kpv,
                  observedHatchMonth = .data$Kkk,
                  errorHatchDay = .data$Ktar,
                  incubation = .data$Halku,
                  observedClutchSize = .data$Mulu,
                  observedBroodSize = .data$Kuor,
                  observedNumberFledged = .data$Lent,
                  reasonFailed = .data$Tsyy,
                  comments = .data$Lisatieto) %>%
    # Create IDs
    # Unique brood IDs: year_locationID_nestAttemptBox
    dplyr::mutate(broodID = paste(.data$year, .data$locationID, .data$nestAttemptBox, sep = "_"),
                  # Set species codes
                  # Note, rare species/codes are ignored and set to NA
                  # e.g., PARCRI, STUVUL, PARSPP, PARXXX
                  speciesID = dplyr::case_when(.data$speciesID == "FICHYP" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                               .data$speciesID == "PARMAJ" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                               .data$speciesID == "PARCAE" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                               .data$speciesID == "PARATE" ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                               .data$speciesID == "PHOPHO" ~ species_codes$speciesID[species_codes$speciesCode == "10010"],
                                               .data$speciesID == "JYNTOR" ~ species_codes$speciesID[species_codes$speciesCode == "10011"],
                                               TRUE ~ NA_character_),
                  # If femaleID & maleID differ from expected format, set to NA
                  # Ensure that individuals are unique: add institutionID as prefix to femaleID & maleID
                  dplyr::across(.cols = c(.data$femaleID, .data$maleID),
                                .fns = ~ {

                                  dplyr::case_when(stringr::str_detect(string = .x,
                                                                       pattern = "^[:upper:]{1}[:digit:]{6}$") ~ paste0("ASK_", .x),
                                                                       TRUE ~ NA_character_)

                                }),
                  siteID = "ASK") %>%
    # Errors in lay date and hatch date are interpreted as symmetrical, so that
    # an error of, e.g., 2 results in a minimum lay date 2 days earlier than observed,
    # and a maximum lay date of 2 days later than observed.
    # Errors marked as NA, result in no known minimum/maximum lay & hatch dates.
    # Errors in category 4 (inaccurate) are interpreted as 1 week, resulting in a
    # minimum lay date 7 days earlier than observed, and a maximum lay date of
    # 7 days later than observed.
    dplyr::mutate(dplyr::across(.cols = c(.data$errorLayDay, .data$errorHatchDay),
                                .fns = ~ {

                                  dplyr::case_when(.x == 4 ~ 7L,
                                                   TRUE ~ .x)

                                }),
                  # Set 0s in lay day & lay month to NA
                  dplyr::across(.cols = c(.data$observedLayDay, .data$observedLayMonth),
                                .fns = ~ {

                                  dplyr::case_when(.x == 0 ~ NA_integer_,
                                                   TRUE ~ .x)

                                }),
                  observedLayYear = .data$year,
                  observedLayDate = lubridate::make_date(year = .data$observedLayYear,
                                                         month = .data$observedLayMonth,
                                                         day = .data$observedLayDay),
                  minimumLayDate = dplyr::case_when(is.na(.data$errorLayDay) ~ as.Date(NA),
                                                   TRUE ~ .data$observedLayDate - .data$errorLayDay),
                  maximumLayDate = dplyr::case_when(is.na(.data$errorLayDay) ~ as.Date(NA),
                                                    TRUE ~ .data$observedLayDate + .data$errorLayDay),
                  minimumLayDay = as.integer(lubridate::day(.data$minimumLayDate)),
                  minimumLayMonth = as.integer(lubridate::month(.data$minimumLayDate)),
                  minimumLayYear = as.integer(lubridate::year(.data$minimumLayDate)),
                  maximumLayDay = as.integer(lubridate::day(.data$maximumLayDate)),
                  maximumLayMonth = as.integer(lubridate::month(.data$maximumLayDate)),
                  maximumLayYear = as.integer(lubridate::year(.data$maximumLayDate)),
                  observedHatchYear = .data$year,
                  observedHatchDate  = lubridate::make_date(year = .data$observedHatchYear,
                                                            month = .data$observedHatchMonth,
                                                            day = .data$observedHatchDay),
                  minimumHatchDate = dplyr::case_when(is.na(.data$errorHatchDay) ~ as.Date(NA),
                                                      TRUE ~ .data$observedHatchDate - .data$errorHatchDay),
                  maximumHatchDate = dplyr::case_when(is.na(.data$errorHatchDay) ~ as.Date(NA),
                                                      TRUE ~ .data$observedHatchDate + .data$errorHatchDay),
                  minimumHatchDay = as.integer(lubridate::day(.data$minimumHatchDate)),
                  minimumHatchMonth = as.integer(lubridate::month(.data$minimumHatchDate)),
                  minimumHatchYear = as.integer(lubridate::year(.data$minimumHatchDate)),
                  maximumHatchDay = as.integer(lubridate::day(.data$maximumHatchDate)),
                  maximumHatchMonth = as.integer(lubridate::month(.data$maximumHatchDate)),
                  maximumHatchYear = as.integer(lubridate::year(.data$maximumHatchDate)),
                  # Convert clutch type observed to our codes
                  # TODO: What does clutch type "0" mean?
                  observedClutchType = dplyr::case_when(.data$observedClutchType == 1 ~ "first",
                                                        .data$observedClutchType == 2 ~ "replacement",
                                                        .data$observedClutchType == 5 ~ "second",
                                                        TRUE ~ NA_character_))

    output <- broods %>%
      # Filter species and remove unknown species
      dplyr::filter(speciesID %in% {{species_filter}} & !is.na(speciesID)) %>%
      # Add optional variables
      {if("breedingSeason" %in% optional_variables) calc_season(data = .,
                                                                season = .data$year) else .} %>%
      {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = .,
                                                                          na.rm = FALSE,
                                                                          protocol_version = "1.2") else .} %>%
      {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = .,
                                                                        season = .data$breedingSeason) else .}

    return(output)

}



#' Create capture data table for Askainen, Finland.
#'
#' Create capture data table in standard format for data from Askainen, Finland.
#'
#' @param db Location of primary data from Askainen.
#' @param brood_data Data frame. Output from \code{\link{create_brood_ASK}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_ASK <- function(db,
                               brood_data,
                               species_filter,
                               optional_variables) {

  message("Extracting nest visit data from paradox database...")

  # Nest visits: contain info on nest stage & timing of ringing
  nest_visits <- extract_paradox_db(path = db, file_name = "ASK_PrimaryData_Visitit.DB") %>%
    # Rename columns to English (based on description provided by data owner)
    dplyr::select(nestID = .data$Diario,
                  year = .data$Vuos,
                  month = .data$Kk,
                  day = .data$Pv,
                  time = .data$Klo,
                  breedingPhase = .data$Tila) %>%
    dplyr::mutate(recordedBy = "LH") # See "Havno" column in Visitit.db

  # Brood data: contain info on broodID & timing of laying/hatching
  broods <- brood_data %>%
    dplyr::mutate(observedLayDate = lubridate::make_date(year = .data$observedLayYear,
                                                         month = .data$observedLayMonth,
                                                         day = .data$observedLayDay)) %>%
    dplyr::select(.data$nestID,
                  .data$broodID,
                  .data$speciesID,
                  .data$locationID,
                  .data$observedLayDate,
                  .data$observedHatchDate)

  # Chicks
  chick_visits <- nest_visits %>%
    # Retrieve chick ringing info per nest
    # breedingPhase P5 indicates a nest visit during which nestlings were ringed
    dplyr::filter(breedingPhase == "P5")

  message("Extracting chick ring numbers from paradox database...")

  chicks <- extract_paradox_db(path = db, file_name = "ASK_PrimaryData_Pulreng.DB") %>%
    # Rename columns to English (based on description provided by data owner)
    dplyr::select(nestID = .data$Diario,
                  ringSeries = .data$Rs,
                  firstRingNumber = .data$Mista,
                  lastRingNumber = .data$Mihin) %>%
    # Join in brood information using nestID
    dplyr::left_join(broods, by = "nestID") %>%
    # Join in chick ringing information using nestID
    dplyr::left_join(chick_visits, by = "nestID") %>%
    # Determine series of chick rings per nestID
    dplyr::mutate(individualID = purrr::map2(.x = .data$firstRingNumber,
                                             .y = .data$lastRingNumber,
                                             .f = ~ {

                                               # If first & last ring number are not NA,
                                               # determine ring series
                                               if(!is.na(.y)) {

                                                 ring_string <- paste0(.x, ":", .y)


                                               } else {

                                                 # If only first ring number is present,
                                                 # that ring number is the ring series
                                                 if(!is.na(.x)) {

                                                   ring_string <- .x

                                                   # If both first & last ring number are NA,
                                                   # set ring series to NA
                                                 } else {

                                                   ring_string <- NA_character_

                                                 }

                                               }

                                               # Parse text
                                               ring_series <- eval(parse(text = ring_string))

                                               # Pad zero if chick IDs started with 0
                                               output <-  stringr::str_pad(as.character(ring_series),
                                                                           width = nchar(.x),
                                                                           side = "left",
                                                                           pad = 0)

                                               # Set chick IDs to NA if the number of IDs is unlikely large
                                               # TODO: Check with data owner.
                                               # Three cases seem to have typos. NestIDs:
                                               # - 180348
                                               # - 181006
                                               # - 180985
                                               if(length(output) > 14) {

                                                 output <- NA_character_

                                               }

                                               return(output)

                                             })) %>%
    tidyr::unnest(cols = .data$individualID) %>%
    # Add ring series letter, if present
    dplyr::mutate(individualID = dplyr::case_when(!is.na(.data$ringSeries) ~ paste0(.data$ringSeries,
                                                                                    .data$individualID),
                                                  TRUE ~ .data$individualID),
                  # If individualID differs from expected format, set to NA
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(stringr::str_detect(string = .data$individualID,
                                                                      pattern = "^[:upper:]{1}[:digit:]{6}$") ~ paste0("ASK_",
                                                                                                                       .data$individualID),
                                                  TRUE ~ NA_character_)) %>%
    # Remove unknown individualIDs
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::rename(captureDay = .data$day,
                  captureMonth = .data$month,
                  captureYear = .data$year) %>%
    dplyr::mutate(captureTime = dplyr::na_if(x = paste0(stringr::str_pad(string = .data$time,
                                                                         width = 2,
                                                                         pad = "0",
                                                                         side = "left"),
                                                        ":00"),
                                             y = "NA:00"),
                  observedSex = NA_character_,
                  # Calculate chick age
                  age = "chick",
                  chickAge = as.integer(lubridate::make_date(year = .data$captureYear,
                                                             month = .data$captureMonth,
                                                             day = .data$captureDay) - .data$observedHatchDate)) %>%
    dplyr::select(.data$nestID,
                  .data$broodID,
                  .data$speciesID,
                  .data$individualID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID,
                  .data$age,
                  .data$chickAge)


  # Parents
  # Captures of parents only available through brood data
  parents <- brood_data %>%
    # Treat capture date of parents as the start of incubation (i.e., laying date + clutch size)
    # or at the start of laying when clutch size is unknown
    # TODO: Verify with data owner
    dplyr::mutate(observedLayDate = lubridate::make_date(year = .data$observedLayYear,
                                                         month = .data$observedLayMonth,
                                                         day = .data$observedLayDay),
                  captureDate = dplyr::case_when(!is.na(.data$observedClutchSize) ~ .data$observedLayDate + .data$observedClutchSize,
                                                 TRUE ~ .data$observedLayDate),
                  captureYear = .data$year,
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = NA_character_) %>%
    # Pivot information on females and males into rows
    tidyr::pivot_longer(cols = c(.data$femaleID, .data$maleID),
                        names_to = "sex",
                        values_to = "individualID") %>%
    # Remove unknown individualIDs
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::mutate(observedSex = dplyr::case_when(.data$sex == "femaleID" ~ "F",
                                                 .data$sex == "maleID" ~ "M"),
                  # TODO: parents assumed to be ringed as "subadult" - check with data owner
                  age = "subadult",
                  chickAge = NA_integer_,
                  recordedBy = "LH") %>%
    dplyr::select(.data$nestID,
                  .data$broodID,
                  .data$speciesID,
                  .data$individualID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID,
                  .data$age,
                  .data$chickAge)


  # Combine capture tables
  captures <- dplyr::bind_rows(parents, chicks) %>%
    dplyr::mutate(captureSiteID = "ASK",
                  releaseSiteID = .data$captureSiteID,
                  # TODO: Individuals are assumed to be captured alive, without replacing rings - verify
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  capturePhysical = TRUE) %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay) %>%
    dplyr::group_by(.data$individualID) %>%

    dplyr::mutate(captureRingNumber = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       TRUE ~ .data$individualID),
                  # All releases are assumed to be alive (also see releaseAlive), so no NAs in releaseRingNumber
                  releaseRingNumber = .data$individualID,
                  # Create captureID
                  captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    # Filter species
    dplyr::filter(speciesID %in% {{species_filter}})

  # Add optional variables
  output <- captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            Year = .data$captureYear,
                                                                                            protocol_version = "1.2") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}


  return(output)

}


#' Create individual data table for Askainen, Finland.
#'
#' Create individual data table in standard format for data from Askainen, Finland.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_ASK}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_individual_ASK <- function(capture_data,
                                  species_filter,
                                  optional_variables) {

  # Create a list of individuals from capture data
  individuals <- capture_data %>%
    dplyr::mutate(captureDate = lubridate::make_date(year = .data$captureYear,
                                                     month = .data$captureMonth,
                                                     day = .data$captureDay)) %>%
    # Arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    # For every individual...
    dplyr::group_by(.data$individualID) %>%
    # ... determine first stage, brood, ring date of each individual
    dplyr::summarise(ringStage = dplyr::first(.data$age),
                     ringDate = dplyr::first(.data$captureDate),
                     firstYear = dplyr::first(.data$year),
                     firstBrood = dplyr::first(.data$broodID),
                     speciesID = dplyr::first(.data$speciesID)) %>%
    # If capture date is NA, use year column from primary data for ringYear instead
    dplyr::mutate(ringYear = dplyr::case_when(is.na(.data$ringDate) ~ as.integer(.data$firstYear),
                                              TRUE ~ as.integer(lubridate::year(.data$ringDate))),
                  ringMonth = as.integer(lubridate::month(.data$ringDate)),
                  ringDay = as.integer(lubridate::day(.data$ringDate)),
                  # Only assign brood ID if individual was caught as a chick
                  broodIDLaid = dplyr::case_when(.data$ringStage != "chick" ~ NA_character_,
                                                 TRUE ~ .data$firstBrood),
                  # There is no information on cross-fostering, so we assume that the brood laid and fledged are the same
                  broodIDFledged =.data$broodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ringSiteID = "ASK",
                  siteID = "ASK") %>%
    dplyr::filter(speciesID %in% {{species_filter}})

  # Add optional variables
  output <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = capture_data) else .}

  return(output)

}

#' Create location data table for Askainen, Finland.
#'
#' Create location data table in standard format for data from Askainen, Finland.
#'
#' @param db Location of primary data from Askainen.
#'
#' @return A data frame.
#'

create_location_ASK <- function(db) {

  message("Extracting location data from paradox database...")

  locations <- extract_paradox_db(path = db, file_name = "ASK_PrimaryData_Pontot.DB") %>%
    # Rename columns to English (based on description provided by data owner)
    dplyr::select(locationID = .data$Nuro,
                  latitude = .data$Leve,
                  longitude = .data$Pitu,
                  geoPrecision = .data$KT)

  # Convert location data to sf
  coordinates <- locations %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 # Assign coordinate reference system according to data owner
                 crs = 2393) %>%
    # Transform conference reference system to longitudes/latitudes
    sf::st_transform(crs = 4326)

  # Create list of locations
  output <- locations %>%
    # Add converted coordinates, set to NA if precision is very low (i.e., 3)
    dplyr::mutate(decimalLongitude = dplyr::case_when(.data$geoPrecision == 3, NA_real_,
                                                      sf::st_coordinates(coordinates)[,1]),
                  decimalLatitude = dplyr::case_when(.data$geoPrecision == 3, NA_real_,
                                                     sf::st_coordinates(coordinates)[,2]),
                  locationType = "nest",
                  # TODO: habitat is set to 1.4 Forest - Boreal; check with data owner
                  habitatID = "1.1",
                  # Assumed that all boxes were placed at the start of the study, and removed at the end of the study
                  # TODO: Check with data owner
                  startYear = 1941L,
                  endYear = 1994L,
                  siteID = "ASK")

  return(output)

}

#----------------------#
# TODO: Check clutch type ("Pesa") codes; what does 0 mean?
# TODO: Check chick ring series. Three nests must contain typos, because the resulting number of chicks is > 100.
# TODO: Check parent age
# TODO: Check whether individuals were only caught/released alive & physically
# TODO: Verify habitatID
# TODO: Verify start & end year of boxes
