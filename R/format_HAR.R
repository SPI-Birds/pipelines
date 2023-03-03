#'Construct standard format for data from Harjavalta, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{Species}: Data from Harjavalta contains information on 23 different species. We only include records for species with at least 50 broods throughout the study period: pied flycatcher, great tit, blue tit, coal tit, European crested tit, and common redstart. The few records for other species (e.g., Eurasian wryneck, common starling, house sparrow) are excluded.
#'
#'\strong{Stage}: Age/stage is listed as: PP (nestling), PM (fledgling), FL(unknown),
#'1, +1, 2, +2. PP/PM are 'chicks', where PP are chicks in the nest and PM chicks left the nest but caught by hand (i.e. still not able to fly), for which exact ages can be calculated. 1 (fully grown in first year) and +1 (fully grown, after first-year) are considered 'subadults', and 2 (fully grown and known to be born last year) and +2 (fully grown, after second-year) 'adults'. For either, exact age cannot be calculated. FL is unknown.
#'
#'\strong{Minimum & maximum lay & hatch dates}: Accuracy of laying and hatch date
#'are given as categories: 1 = 0-1, 2 = 1-2, 3 = 2-3; 4 = inaccurate. Where error is a range,
#'the more conservative error is used (i.e., 0-1 is recorded as 1).
#'Cases listed as 'inaccurate' have an error of at least a week. Dates in these cases are highly inaccurate
#'and shouldn't be considered for any phenology analysis.
#'
#'\strong{Capture data:} Linking nestling and adult capture
#'data can be difficult. There are eight different scenarios we need to
#'consider. Each of these is described below, with our solution: \itemize{ \item
#'#1. Individual captured as adult (age is NA or not PM/PP) (e.g., 00-829590).
#'
#'In this case, we assume that there is only information in Capture_data, and we
#'do not search for any information in Nestling_data.
#'
#'\item #2. Record of chick (age is PM/PP) with no information in the last ring
#'number column AND no corresponding record in Nestling_data (i.e., no record
#'where broodID and last 2 digits of the ring number were the same) (e.g.,
#'HL-025681).
#'
#'In this case, we assume that the information in Capture_data is the correct
#'capture information.
#'
#'\item #3.	Record of chick (age is PM/PP) with no information in the last ring
#'number column, with corresponding record in nestling_data, BUT where the
#'capture dates in Nestling_data and Capture_data do not match (e.g., HA-30262).
#'
#'In this case, we assume that the individual was captured multiple times and
#'the records in Nestling_data and Capture_data are both true capture records.
#'All information from both tables is kept.
#'
#'\item #4.	Record of chick (age is PM/PP) with no information in the last ring
#'number column, corresponding record in Nestling_data that was taken at exactly
#'the same capture date (e.g., HA-30103).
#'
#'Where there are measurements (e.g., mass, tarsus) in both Nestling_data and
#'Capture_data, we give precedence to the data from Nestling_data. We only use
#'data from Capture_data when there is no data in Nestling_data.
#'
#'\item #5.	Record of chick (age is PM/PP) with information in the last ring
#'number column which matches data in Nestling_data (e.g., HL-26103 - 26107)
#'
#'We assume that there is a continuous ring series used (e.g. 26103, 26104,
#'26105, 26106, 26107) and link all corresponding records from Nestling_data
#'where the BroodID and last 2 digits of the ring number match.
#'
#'\item #6.	Record of chick (Age is PM/PP) with information in the last ring
#'number column but no matches to data in Nestling_data (e.g. HL-364911 -
#'364916).
#'
#'We assume the individuals were captured and ringed, but there was no data
#'collected on them (e.g., mass), only individualID, captureDate, captureTime.
#'
#'\item #7. Record in Nestling_data that has no ring number information (e.g., A,
#'B, C, D) (e.g., Brood 2011_0003_1)
#'
#'We assume these were chicks captured before they were old enough to be ringed.
#'We keep these as a legitimate capture record as they can tell us something
#'about chick growth.
#'
#'\item #8.	Record in Nestling_data that has ring number information, but there
#'is no corresponding record of chicks with those rings being captured from that
#'brood in Capture_data (e.g. Brood 1985_2081_1 with individuals 71, 72, 73,
#'74).
#'
#'We cannot determine the identity of the captured individual without a matching
#'record in Capture_data because we only know the last 2 digits of the ring
#'number. Currently, we include them as captures with individualID = NA.}
#'
#'\strong{mass}: Mass of birds is measured in mg. This is converted
#'to grams to match other populations.
#'
#'\strong{tarsus}: Tarsus length is measured for both left and right leg.
#'If both are measured, both are reported in Measurement_data. Tarsus measurement
#'in adults can be either left or right leg.
#'
#'\strong{observedSex}: Bird classified as 'likely male' or 'likely female' are simply
#'given code 'M' and 'F' respectively (i.e., this uncertainty is ignored).
#'
#'\strong{broodID}: Unique broodID values are generated using year_locationPlotID_nestAttemptNumber, where locationPlotID is a concatenation of plotID (without the institutionID prefix) and locationID, and nestAttemptNumber, the nesting attempt in a nest box (locationID) per season.
#'
#'\strong{Experiment start and end}: Start of experimental manipulations are set to the first laying date of the affected broods, so that `experimentStartYear = min(observedLayYear)`, `experimentStartMonth = min(observedLayMonth)`, and `experimentStartDay = min(observedLayDay)`. Experiments are assumed to start and end within the same year, so that experimentEndYear = experimentStartYear. experimentEndMonth and experimentEndDay are, however, set to NA. Discuss with data custodian.
#'
#'\strong{recordedBy}: Persons who visited the nests/broods that were used in experiments are assumed to have conducted the experiment as well.
#'
#'\strong{locationID}: In the primary data, single nest boxes may change coordinates throughout the study period. When they do, new locationIDs are assigned. Note that locationIDs (i.e., nest box numbers) may be non-unique across plots (plotIDs). Unique identifiers for nest boxes can be retrieved using both plotID and locationID.
#'
#'@inheritParams pipeline_params
#'
#'@param return_errors Logical (TRUE/FALSE). If true, return all records of
#'nestling with no corresponding ringing data.
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_HAR <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R",
                       return_errors = FALSE){

  # Force user to select directory
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

  message("Compiling brood data....")

  Brood_data <- create_brood_HAR(db = db,
                                 species_filter = species,
                                 optional_variables = optional_variables)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_HAR(db = db,
                                     brood_data = Brood_data,
                                     species_filter = species,
                                     optional_variables = optional_variables,
                                     return_errors = return_errors)

  if(return_errors){

    return(Capture_data)

  }

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_HAR(capture_data = Capture_data,
                                           optional_variables = optional_variables)

  # MEASUREMENT DATA

  message("Compiling measurement data...")

  Measurement_data <- create_measurement_HAR(capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_HAR(db = db)

  # EXPERIMENT DATA

  message("Compiling experiment data...")

  Experiment_data <- create_experiment_HAR(db = db,
                                           brood_data = Brood_data)

  # WRANGLE DATA FOR EXPORT

  # - Brood data
  Brood_data <- Brood_data %>%
    # Add treatmentID from Experiment data
    # First create experimentCode variable used to join by
    dplyr::mutate(experimentCode = paste(.data$year, .data$experimentID, sep = "_")) %>%
    dplyr::left_join(Experiment_data %>% dplyr::select(.data$treatmentID, .data$experimentCode),
                     by = "experimentCode") %>%
    # Add locationID from Location data
    # Store old locationID as locationCode
    dplyr::rename(locationCode = "locationID") %>%
    dplyr::left_join(Location_data %>% dplyr::select("locationPlotID", "locationID", "year"),
                     by = c("locationPlotID", "observedLayYear" = "year")) %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[1, !(names(data_templates$v2.0$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Brood_data), any_of(names(utility_variables$Brood_data)))

  # - Capture data
  Capture_data <- Capture_data %>%
    # Add locationID from Location data
    # Store old captureLocationID as captureLocationCode & old releaseLocationID as releaseLocationCode
    dplyr::rename(captureLocationCode = "captureLocationID",
                  releaseLocationCode = "releaseLocationID") %>%
    # TODO: Some locationIDs in Capture_data are missing from Location data. Check with data custodian.
    dplyr::left_join(Location_data %>% dplyr::select("locationPlotID", "captureLocationID" = "locationID", "year"),
                     by = c("locationPlotID", "captureYear" = "year")) %>%
    dplyr::mutate(releaseLocationID = .data$captureLocationID) %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[1, !(names(data_templates$v2.0$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Capture_data), any_of(names(utility_variables$Capture_data)))

  # - Individual data
  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[1, !(names(data_templates$v2.0$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Individual_data), any_of(names(utility_variables$Individual_data)))

  # - Measurement data
  Measurement_data <- Measurement_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Measurement_data[1, !(names(data_templates$v2.0$Measurement_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Measurement_data))

  # - Location data
  # create_location_HAR() returns annual location records, so that these can be easily added to
  # Brood data and Capture data
  # In the final output, however, each locationID should occur only once in the Location_data table
  Location_data <- Location_data %>%
    dplyr::group_by(.data$plotID, .data$locationID) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::summarise(decimalLatitude = as.numeric(dplyr::first(latitude)),
                     decimalLongitude = as.numeric(dplyr::first(longitude)),
                     startYear = min(.data$year),
                     endYear = max(.data$year),
                     studyID = dplyr::first(.data$studyID),
                     siteID = dplyr::first(.data$siteID),
                     habitatID = dplyr::first(.data$habitatID),
                     locationType = dplyr::first(.data$locationType),
                     .groups = "drop") %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Location_data[1, !(names(data_templates$v2.0$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Location_data))

  # - Experiment data
  Experiment_data <- Experiment_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Experiment_data[1, !(names(data_templates$v2.0$Experiment_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Experiment_data))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_HAR.csv"), row.names = F)

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

#' Create brood data table for Harjavalta, Finland.
#'
#' Create brood data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#'  @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_brood_HAR <- function(db,
                             species_filter,
                             optional_variables){

  message("Extracting brood data from paradox database")

  # Extract table "Pesat.db" which contains brood data
  # Rename columns to English (based on description provided by data custodian)
  # Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  # see what is being removed.
  broods <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Pesat.DB") %>%
    dplyr::rename(year = "Vuos",
                  locationPlotID = "Nuro",
                  nestAttemptNumber = "Anro",
                  speciesID = "Laji",
                  observedClutchType = "Pesa",
                  femaleID = "Naaras",
                  maleID = "Koiras",
                  observedLayDay = "Mpv",
                  observedLayMonth = "Mkk",
                  errorLayDay = "Mtar",
                  observedHatchDay = "Kpv",
                  observedHatchMonth = "Kkk",
                  errorHatchDay = "Ktar",
                  incubation = "Halku",
                  observedClutchSize = "Mulu",
                  observedBroodSize = "Kuor",
                  observedNumberFledged = "Lent",
                  reasonFailed = "Tsyy",
                  nestlingInjuries = "Jalat",
                  malePresent = "Koir",
                  experimentID = "Koe",
                  expData1 = "Olent",
                  expData2 = "Vlent",
                  deadParent = "Delfh",
                  eggShells = "Mkuor",
                  tempCode1 = "Tark",
                  tempCode2 = "Tark2") %>%
    # Remove unwanted columns
    dplyr::select(-"reasonFailed":-"malePresent",
                  -"expData1":-"tempCode2") %>%
    # Create IDs
    # locationPlotID (Nuro) is a concatenation of plot ID (first two symbols)
    # and nestbox number (last two symbols); split, and make both unique
    # TODO: Check with data custodian, it seems that the first two are plot, and the last two are nestbox,
    # whereas the meta data file states the other way around
    dplyr::mutate(plotID = paste("HAR",
                                 stringr::str_sub(string = .data$locationPlotID,
                                                  start = 1,
                                                  end = 2),
                                 sep = "_"),
                  locationID = paste(.data$plotID,
                                     stringr::str_sub(string = .data$locationPlotID,
                                                      start = 3,
                                                      end = 4),
                                     sep = "_"),
                  # Create unique BroodID with year_locationPlotID_nestAttemptNumber
                  broodID = paste(.data$year,
                                  .data$locationPlotID,
                                  .data$nestAttemptNumber,
                                  sep = "_"),
                  # Set species codes
                  # Note, rare species/codes are ignored and set to NA
                  # e.g., JYNTOR, CERFAM, MOTALB, PARMON
                  speciesID = dplyr::case_when(.data$speciesID == "FICHYP" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                               .data$speciesID == "PARCAE" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                               .data$speciesID == "PARMAJ" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                               .data$speciesID == "PARATE" ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                               .data$speciesID == "PHOPHO" ~ species_codes$speciesID[species_codes$speciesCode == "10010"],
                                               .data$speciesID == "PARCRI" ~ species_codes$speciesID[species_codes$speciesCode == "10012"],
                                               TRUE ~ NA_character_),
                  # If femaleID & maleID differ from expected format, set to NA
                  # Ensure that individuals are unique: add institutionID as prefix to femaleID & maleID
                  # Remove punctuation
                  dplyr::across(.cols = c(.data$femaleID, .data$maleID),
                                .fns = ~ {

                                  dplyr::case_when(stringr::str_detect(string = .x,
                                                                       pattern = "^[:alpha:]{1,2}[:digit:]{5,6}$") ~ paste0("HAR_", stringr::str_remove_all(.x, "[:punct:]")),
                                                   TRUE ~ NA_character_)

                                }),
                  studyID = "HAR-1",
                  siteID = "HAR") %>%
    # Adjust clutch type observed to meet our wording
    dplyr::mutate(observedClutchType = dplyr::case_when(.data$observedClutchType == 1 ~ "first",
                                                        .data$observedClutchType %in% c(2, 3, 6) ~ "replacement",
                                                        .data$observedClutchType == 5 ~ "second"),
                  # Errors in lay date and hatch date are interpreted as symmetrical, so that
                  # an error of, e.g., 2 results in a minimum lay date 2 days earlier than observed,
                  # and a maximum lay date of 2 days later than observed.
                  # Errors marked as NA, result in no known minimum/maximum lay & hatch dates.
                  # Errors in category 4 (inaccurate) are interpreted as 1 week, resulting in a
                  # minimum lay date 7 days earlier than observed, and a maximum lay date of
                  # 7 days later than observed.
                  dplyr::across(.cols = c(.data$errorLayDay, .data$errorHatchDay),
                                .fns = ~ {

                                  dplyr::case_when(.x == 4 ~ 7L,
                                                   TRUE ~ .x)

                                }),
                  # Create calendar date for laying date and hatch date
                  observedLayYear = .data$year,
                  observedLayDate = lubridate::make_date(year = .data$observedLayYear,
                                                         month = .data$observedLayMonth,
                                                         day = .data$observedLayDay),
                  observedHatchYear = .data$year,
                  observedHatchDate  = lubridate::make_date(year = .data$observedHatchYear,
                                                            month = .data$observedHatchMonth,
                                                            day = .data$observedHatchDay),
                  # Subtract error in lay/hatch date from observed to create minimum lay/hatch date
                  minimumLayDate = dplyr::case_when(is.na(.data$errorLayDay) ~ as.Date(NA),
                                                   TRUE ~ .data$observedLayDate - .data$errorLayDay),
                  minimumHatchDate = dplyr::case_when(is.na(.data$errorHatchDay) ~ as.Date(NA),
                                                     TRUE ~ .data$observedHatchDate - .data$errorHatchDay),
                  # Add error in lay/hatch date from observed to create maximum lay/hatch date
                  maximumLayDate = dplyr::case_when(is.na(.data$errorLayDay) ~ as.Date(NA),
                                                   TRUE ~ .data$observedLayDate + .data$errorLayDay),
                  maximumHatchDate = dplyr::case_when(is.na(.data$errorHatchDay) ~ as.Date(NA),
                                                   TRUE ~ .data$observedHatchDate + .data$errorHatchDay),
                  # Split minimum/maximum date columns in year, month, day
                  minimumLayYear = as.integer(lubridate::year(.data$minimumLayDate)),
                  minimumLayMonth = as.integer(lubridate::month(.data$minimumLayDate)),
                  minimumLayDay = as.integer(lubridate::day(.data$minimumLayDate)),
                  maximumLayYear = as.integer(lubridate::year(.data$maximumLayDate)),
                  maximumLayMonth = as.integer(lubridate::month(.data$maximumLayDate)),
                  maximumLayDay = as.integer(lubridate::day(.data$maximumLayDate)),
                  minimumHatchYear = as.integer(lubridate::year(.data$minimumHatchDate)),
                  minimumHatchMonth = as.integer(lubridate::month(.data$minimumHatchDate)),
                  minimumHatchDay = as.integer(lubridate::day(.data$minimumHatchDate)),
                  maximumHatchYear = as.integer(lubridate::year(.data$maximumHatchDate)),
                  maximumHatchMonth = as.integer(lubridate::month(.data$maximumHatchDate)),
                  maximumHatchDay = as.integer(lubridate::day(.data$maximumHatchDate)))

  output <- broods %>%
    # Filter species and remove unknown species
    dplyr::filter(!is.na(.data$speciesID) & .data$speciesID %in% {{species_filter}}) %>%
    # Add optional variables
    {if("breedingSeason" %in% optional_variables) calc_season(data = .,
                                                              season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = .,
                                                                        na.rm = FALSE,
                                                                        protocol_version = "2.0") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = .,
                                                                      season = .data$breedingSeason) else .}

  return(output)

}

#' Create nestling data capture table for Harjavalta, Finland.
#'
#' Create nestling data capture table for data from Harjavalta, Finland. This is
#' used inside \code{\link{create_capture_HAR}}.
#'
#' @param db Location of primary data from Harjavalta, Finland.
#' @param brood_data Data frame. Output of \code{\link{create_brood_HAR}}.
#'
#' @return A data frame.

create_nestling_HAR <- function(db,
                                brood_data){

  message("Extracting nestling ringing data from paradox database")

  # Extract table "Pullit.db" which contains nestling data
  nestling_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Pullit.DB") %>%
    # Rename columns to English (based on description provided by data custodian)
    dplyr::rename(captureYear = "Vuos",
                  locationPlotID = "Nuro",
                  nestAttemptNumber = "Anro",
                  captureMonth = "Kk",
                  captureDay = "Pv",
                  captureTime = "Klo",
                  nestlingNumber = "Poik",
                  last2DigitsRingNumber = "Reng",
                  dead = "Dead",
                  totalWingLength = "Siipi",
                  mass = "Paino",
                  breastMuscle = "Lihas",
                  leftLegAbnormalities = "Vjalka",
                  rightLegAbnormalities = "Ojalka",
                  leftP3 = "Vkas",
                  rightP3 = "Okas",
                  leftRectrix = "Vpys",
                  rightRectrix = "Opys",
                  leftTarsusLength = "Vnil",
                  rightTarsusLength = "Onil",
                  leftTarsusWidth = "Vpak",
                  rightTarsusWidth = "Opak",
                  greatTitBreastYellow = "Vari",
                  luteinSupplementation = "Lkoe",
                  bloodSample = "Wb",
                  columnLengthBlood = "Tot",
                  lengthBlood = "Pun",
                  breastFeatherLutein = "FetLut",
                  nailClipping = "Varpaat",
                  geneticSex = "Sp",
                  headLength = "Head",
                  faecalSample1 = "Feces1",
                  faecalSample2 = "Feces2",
                  tempCode = "Tark") %>%
    # Create unique broodID (year_locationPlotID_nestAttemptNumber)
    dplyr::mutate(broodID = paste(.data$captureYear,
                                  .data$locationPlotID,
                                  .data$nestAttemptNumber,
                                  sep = "_"),
                  captureDate = lubridate::make_date(year = .data$captureYear,
                                                     month = .data$captureMonth,
                                                     day = .data$captureDay),
                  captureTime = dplyr::na_if(x = paste0(.data$captureTime, ":00"),
                                             y = "NA:00"),
                  leftTarsusLength = dplyr::na_if(.data$leftTarsusLength, 0),
                  totalWingLength = dplyr::na_if(.data$totalWingLength, 0),
                  headLength = dplyr::na_if(.data$headLength, 0)) %>%
    # Join hatch date data from brood data table
    dplyr::left_join(brood_data %>%  dplyr::select("broodID", "observedHatchDate"),
                     by = "broodID") %>%
    # Determine age at capture
    dplyr::mutate(chickAge = as.integer(.data$captureDate - .data$observedHatchDate),
                  # Nestling tarsus measures seem to have different units depending on which leg was measured.
                  # Left in mm, right in 10*mm.
                  # TODO: check with data custodian
                  rightTarsusLength = .data$rightTarsusLength / 10,
                  leftP3 = .data$leftP3 / 10,
                  rightP3 = .data$rightP3 / 10,
                  leftRectrix = .data$leftRectrix / 10,
                  rightRectrix = .data$rightRectrix / 10)

  return(nestling_data)

}

#' Create capture table for Harjavalta, Finland.
#'
#' Create full capture data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param brood_data Data frame. Output of \code{\link{create_brood_HAR}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#' @param return_errors Logical. Return those records with errors in the ring sequence.
#'
#' @return A data frame.

create_capture_HAR <- function(db,
                               brood_data,
                               species_filter,
                               optional_variables,
                               return_errors){

  # Extract nestling data
  nestling_data <- create_nestling_HAR(db = db,
                                       brood_data = brood_data)

  message("Extracting capture data from paradox database")

  # Extract table "Rengas.db" which contains ringing data
  # N.B. lastRingNumber_Brood = the end of the ringing series when ringing chicks
  # e.g. a record with ringNumber = 662470 and lastRingNumber_Brood = 662473 had three ringed chicks:
  # 662470, 662471, 662472, 662473
  # The number of nestlings ringed is stored in nestlingNumber (Poik).
  capture_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Rengas.DB") %>%
    # Rename columns to English (based on description provided by data custodian)
    dplyr::rename(ringSeries = "Sarja",
                  ringNumber = "Mista",
                  captureType = "Tunnus",
                  captureYear = "Vuos",
                  captureMonth = "Kk",
                  captureDay = "Pv",
                  captureTime = "Klo",
                  locationPlotID = "Nuro",
                  nestAttemptNumber = "Anro",
                  recordedBy = "Havno",
                  lastRingNumber = "Mihin",
                  speciesID = "Laji",
                  observedSex = "Suku",
                  sexMethod = "Sp",
                  age = "Ika",
                  ageMethod = "Ip",
                  ringType = "Rtapa",
                  condition = "Kunto",
                  birdStatus = "Tila",
                  captureMethod = "Ptapa",
                  nestlingNumber = "Poik",
                  totalWingLength = "Siipi",
                  mass = "Paino",
                  moult = "Sulsat",
                  fatScore = "Rasik",
                  extraInfo = "Lisa",
                  plumage = "Vari",
                  tailFeather = "Psulka",
                  columnLengthBlood = "Tot",
                  lengthBlood = "Pun",
                  tarsusLength = "Tarsus",
                  breastMuscle = "Lihas",
                  headLength = "Head",
                  ticks = "Ticks",
                  tempCode = "Tark")

  capture_data <- capture_data %>%
    # Create unique broodID (year_locationPlotID_nestAttemptNumber)
    dplyr::mutate(broodID = paste(.data$captureYear,
                                  .data$locationPlotID,
                                  .data$nestAttemptNumber,
                                  sep = "_"),
                  captureDate = lubridate::make_date(year = .data$captureYear,
                                                     month = .data$captureMonth,
                                                     day = .data$captureDay),
                  captureTime = dplyr::na_if(x = paste0(.data$captureTime, ":00"),
                                             y = "NA:00")) %>%
    # Set species codes
    # Note, rare species/codes are ignored and set to NA
    # e.g., JYNTOR, CERFAM, MOTALB, PARMON
    dplyr::mutate(speciesID = dplyr::case_when(.data$speciesID == "FICHYP" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                               .data$speciesID == "PARCAE" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                               .data$speciesID == "PARMAJ" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                               .data$speciesID == "PARATE" ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                               .data$speciesID == "PHOPHO" ~ species_codes$speciesID[species_codes$speciesCode == "10010"],
                                               .data$speciesID == "PARCRI" ~ species_codes$speciesID[species_codes$speciesCode == "10012"],
                                               TRUE ~ NA_character_)) %>%
    dplyr::mutate(observedSex = dplyr::case_when(.data$observedSex %in% c("N", "O") ~ "F",
                                                 .data$observedSex %in% c("K", "L") ~ "M"),
                  mass = dplyr::na_if(.data$mass, 0),
                  tarsusLength = dplyr::na_if(.data$tarsusLength, 0),
                  headLength = dplyr::na_if(.data$headLength, 0),
                  totalWingLength = dplyr::na_if(.data$totalWingLength, 0)) %>%
    # Filter species and remove unknown species
    dplyr::filter(!is.na(.data$speciesID) & .data$speciesID %in% {{species_filter}})

  # We have eight scenarios we need to deal with in the capture data.
  # See an explanation in the help documentation:

  ####

  # 1. Individual adult captures
  adult_capture <- capture_data %>%
    dplyr::filter(!.data$age %in% c("PP", "PM") | is.na(.data$age)) %>%
    dplyr::mutate(captureStage = "adult") %>%
    dplyr::mutate(individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber))

  ####

  message("Determining chick ring numbers...")

  # Isolate only chick captures
  chick_data <- capture_data %>%
    dplyr::filter(.data$age %in% c("PP", "PM"))

  # 2-4. Individual chick captures
  indv_chick_capture <- chick_data %>%
    dplyr::filter(is.na(.data$lastRingNumber))

  # 2. No information in nestling data
  # In this case, it doesn't matter whether individual measurements are NA, they are still legit capture records
  indv_chick_capture_only <- indv_chick_capture %>%
    # Filter only those where the broodID_ringNumber combo is not found in Nestling data
    dplyr::filter(!paste(broodID, stringr::str_sub(ringNumber, start = -2), sep = "_") %in% paste(nestling_data$broodID, nestling_data$last2DigitsRingNumber, sep = "_")) %>%
    # Join in brood_data (with observedHatchDate) so we can determine chickAge
    dplyr::left_join(brood_data %>% dplyr::select("broodID", "observedHatchDate"),
                     by = "broodID") %>%
    dplyr::mutate(captureStage = "chick",
                  last2DigitsRingNumber = NA,
                  chickAge = as.integer(.data$captureDate - .data$observedHatchDate),
                  individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber))
  ####

  # 3-4. Individual chick captures with that also have records in Nestling data
  # These can be:
  # - 3. where the records were at different dates. In this case, both records are used, or
  # - 4. where the records are on the same date. In this case, nestling record is used unless data are missing

  # First, identify those cases where the individual chick record is in both tables
  non_matching_records <- indv_chick_capture %>%
    dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(.data$ringNumber, start = -2),
                  individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) %>%
    dplyr::semi_join(nestling_data %>%
                       dplyr::select("broodID", "last2DigitsRingNumber", captureDateNestling = "captureDate"),
                     by = c("broodID", "last2DigitsRingNumber")) %>%
    dplyr::pull(.data$individualID)

  # Next, go through capture data records and return all the cases where these
  # chicks had a record in capture data that didn't match anything in nestling data
  unique_individual_captures <- indv_chick_capture %>%
    dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(.data$ringNumber, start = -2),
                  individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) %>%
    dplyr::filter(.data$individualID %in% {{non_matching_records}}) %>%
    dplyr::anti_join(nestling_data %>%
                       dplyr::select("broodID", "last2DigitsRingNumber", "captureDate"),
                     by = c("broodID", "last2DigitsRingNumber", "captureDate")) %>%
    dplyr::left_join(brood_data %>%
                       dplyr::select("broodID", "observedHatchDate"),
                     by = "broodID") %>%
    dplyr::mutate(chickAge = as.integer(.data$captureDate - .data$observedHatchDate),
                  captureStage = "chick")

  # Go through the nestling data records and do the same thing
  unique_individual_nestlings <- nestling_data %>%
    dplyr::left_join(indv_chick_capture %>%
                       dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(ringNumber, start = -2)) %>%
                       dplyr::select("broodID", "last2DigitsRingNumber", "ringSeries",
                                     "ringNumber", "speciesID"),
                     by = c("broodID", "last2DigitsRingNumber")) %>%
    dplyr::mutate(individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) %>%
    dplyr::filter(.data$individualID %in% {{non_matching_records}}) %>%
    dplyr::anti_join(indv_chick_capture %>%
                       dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(.data$ringNumber, start = -2)) %>%
                       dplyr::select("broodID", "last2DigitsRingNumber", "captureDate"),
                     by = c("broodID", "last2DigitsRingNumber", "captureDate")) %>%
    dplyr::mutate(age = "PP",
                  captureStage = "chick",
                  captureType = "5")

  indv_chick_multirecord <- dplyr::bind_rows(unique_individual_captures, unique_individual_nestlings)

  # Go through and find the exact matches
  indv_chick_record_conflict <- indv_chick_capture %>%
    # Join nestling data and filter those cases where the same date is present
    dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(.data$ringNumber, start = -2),
                  individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) %>%
    dplyr::left_join(nestling_data %>%
                       dplyr::select("broodID", "last2DigitsRingNumber", captureDateNestling = "captureDate",
                                     captureTimeNestling = "captureTime", massNestling = "mass",
                                     totalWingLengthNestling = "totalWingLength",
                                     headLengthNestling = "headLength",
                                     "leftTarsusLength", "rightTarsusLength",
                                     "leftP3", "rightP3",
                                     "leftRectrix", "rightRectrix",
                                     "observedHatchDate", "chickAge"),
                     by = c("broodID", "last2DigitsRingNumber")) %>%
    # Filter those cases that are individual captures (i.e., no last ring number)
    # Find cases where there was mass and/or wing length in the nestling data
    # AND where the captureDate in the Nestling data is different to Capture_data
    dplyr::filter(.data$captureDate == .data$captureDateNestling) %>%
    dplyr::mutate(mass = dplyr::case_when(is.na(.data$massNestling) ~ as.numeric(.data$mass),
                                          TRUE ~ as.numeric(.data$massNestling)),
                  headLength = dplyr::case_when(is.na(.data$headLengthNestling) ~ as.numeric(.data$headLength),
                                                TRUE ~ as.numeric(.data$headLengthNestling)),
                  totalWingLength = dplyr::case_when(is.na(.data$totalWingLengthNestling) ~ as.numeric(.data$totalWingLength),
                                                     TRUE ~ as.numeric(.data$totalWingLengthNestling)),
                  captureStage = "chick")
  ####

  # 5. Cases where the record in capture is for multiple chicks
  # The individual data is stored in nestling data
  flat_multi_chick_capture <- chick_data %>%
    dplyr::filter(!is.na(.data$lastRingNumber))

  expanded_multi_chick_capture <- flat_multi_chick_capture %>%
    # Determine series of chick ring numbers
    dplyr::mutate(ringNumber = purrr::map2(.x = .data$ringNumber,
                                           .y = .data$lastRingNumber,
                                           .f = ~ {

                                             # Determine ring series
                                             ring_string <- paste0(.x, ":", .y)

                                             # Parse text
                                             ring_series <- eval(parse(text = ring_string))

                                             # Pad chick ring numbers with leading 0
                                             output <-  stringr::str_pad(as.character(ring_series),
                                                                         width = nchar(.x),
                                                                         side = "left",
                                                                         pad = 0)

                                             # Set chick IDs to NA if the list of ring numbers is unlikely large
                                             # TODO: Check with data custodian.
                                             # Four cases seem to have typos. ringNumber:
                                             # - 403881
                                             # - 564023
                                             # - 956423
                                             # - 418760
                                             if(length(output) > 14) {

                                               output <- NA_character_

                                             }

                                             return(output)

                                           })) %>%
    tidyr::unnest(cols = .data$ringNumber) %>%
    dplyr::select("broodID",
                  "locationPlotID",
                  "ringSeries",
                  "ringNumber",
                  "captureYear",
                  "captureMonth",
                  "captureDay",
                  "speciesID",
                  "observedSex",
                  "age",
                  "recordedBy",
                  "captureType",
                  "birdStatus")

  # Now, for each recorded chick ring number determine the last 2 digits of the ring
  multirecord_captures <- expanded_multi_chick_capture %>%
    dplyr::mutate(last2DigitsRingNumber = stringr::str_sub(.data$ringNumber, start = -2),
                  captureStage = "chick") %>%
    # Join in all nestling data where the broodID and last2DigitsRingNumber is the same
    # N.B. We do left_join with broodID and last2DigitsRingNumber, so we can get multiple
    # records for each chick when they were captured more than once
    dplyr::left_join(nestling_data %>% dplyr::select("broodID", "captureDate", "captureTime",
                                                     "last2DigitsRingNumber", "mass", "totalWingLength",
                                                     "headLength", "leftTarsusLength", "rightTarsusLength",
                                                     "leftRectrix", "rightRectrix",
                                                     "leftP3", "rightP3", "chickAge"),
                     by = c("broodID", "last2DigitsRingNumber")) %>%
    dplyr::mutate(individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ .data$captureYear,
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)))

  ####

  # 6. Nestling records that don't correspond to any capture data
  # brood/ringNumber combos from individual captures
  single_capture_records <- indv_chick_capture %>%
    dplyr::mutate(broodRingNumber = paste(.data$broodID,
                                          stringr::str_sub(.data$ringNumber, start = -2),
                                          sep = "_")) %>%
    dplyr::pull(broodRingNumber)

  # brood/ringNumber combos from multi-captures
  multi_capture_records <- expanded_multi_chick_capture %>%
    dplyr::mutate(broodRingNumber = paste(.data$broodID,
                                          stringr::str_sub(.data$ringNumber, start = -2),
                                          sep = "_")) %>%
    dplyr::pull(broodRingNumber)

  nocapture_nestlings <- nestling_data %>%
    dplyr::mutate(broodRingNumber = paste(.data$broodID,
                                          .data$last2DigitsRingNumber,
                                          sep = "_")) %>%
    dplyr::filter(!broodRingNumber %in% c(single_capture_records, multi_capture_records))

  # 6a. Filter unringed individuals. These are just given records associated with a brood where
  # the individual ID is unknown.

  # TODO: Check with other developers how to deal with "unknown" individuals.
  # In more recently developed pipelines, unknown individualIDs are removed. Should we do the same here?
  # Or should we rethink the way we deal with this? For instance, use data custodian-defined IDs for
  # captureTagID & releaseTagID (given that they ARE ring numbers), and create our own
  # individualID? This way, "unringed" or "unknown" individuals do not have to be removed, but
  # users are able to distinguish them from ringed individuals and decide what to do with them
  # in their analyses.

  unringed_chicks <- nocapture_nestlings %>%
    dplyr::filter(toupper(.data$last2DigitsRingNumber) %in% LETTERS) %>%
    dplyr::mutate(individualID = NA_character_,
                  recordedBy = NA_character_,
                  observedSex = NA_character_,
                  age = "PP",
                  captureStage = "chick") %>%
    # Join in species information from brood_data
    dplyr::left_join(brood_data %>% dplyr::select("broodID", "speciesID"), by = "broodID")

  ringed_chicks_nocapture <- nocapture_nestlings %>%
    dplyr::filter(!toupper(.data$last2DigitsRingNumber) %in% LETTERS) %>%
    dplyr::mutate(individualID = NA_character_,
                  recordedBy = NA_character_,
                  observedSex = NA_character_,
                  age = "PP",
                  captureStage = "chick") %>%
    # Join in species information from brood_data
    dplyr::left_join(brood_data %>% dplyr::select("broodID", "speciesID"), by = "broodID")

  message(paste0("There are ",
                 nrow(ringed_chicks_nocapture),
                 " nestling records where we cannot translate last2Digits into an individualID"))

  if(return_errors){

    return(nocapture_nestlings %>%
             dplyr::filter(!toupper(.data$last2DigitsRingNumber) %in% LETTERS))

  }

  ####

  # Now that we have dealt with all 6 scenarios, we can join the data back together.
  capture_data_expanded <- dplyr::bind_rows(adult_capture, indv_chick_capture_only, indv_chick_multirecord,
                                            indv_chick_record_conflict, multirecord_captures, unringed_chicks,
                                            ringed_chicks_nocapture)

  captures <- capture_data_expanded %>%
    dplyr::mutate(mass = .data$mass / 10,
                  captureSiteID = "HAR",
                  releaseSiteID = "HAR",
                  # Set locationPlotIDs containing ? to NA
                  locationPlotID = dplyr::case_when(stringr::str_detect(string = .data$locationPlotID,
                                                                        pattern = "\\?") ~ NA_character_,
                                                    TRUE ~ .data$locationPlotID),
                  # Set plotID & locationID
                  # plotID = first two digits
                  # locationID = plotID_nestbox number, where nestbox number is last two digits
                  capturePlotID = paste("HAR",
                                        stringr::str_sub(string = .data$locationPlotID,
                                                         start = 1,
                                                         end = 2),
                                        sep = "_"),
                  captureLocationID = paste(.data$capturePlotID,
                                            stringr::str_sub(string = .data$locationPlotID,
                                                             start = 3,
                                                             end = 4),
                                            sep = "_"),
                  releasePlotID = .data$capturePlotID,
                  releaseLocationID = .data$captureLocationID,
                  studyID = "HAR-1",
                  # Assume that individuals with condition 'D' (dead) are recovered,
                  # rather than died during handling
                  # TODO: check with data custodian
                  captureAlive = dplyr::case_when(.data$condition == "D" ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$condition == "D" ~ FALSE,
                                                  TRUE ~ TRUE),
                  # Assume that individuals with captureMethod 'M' (other) are not physically captured
                  # TODO: check with data custodian
                  capturePhysical = dplyr::case_when(.data$captureMethod == "M" ~ FALSE,
                                                     TRUE ~ TRUE),
                  # Pad captureTime to be HH:SS
                  captureTime = stringr::str_pad(string = .data$captureTime,
                                                 width = 5,
                                                 side = "left",
                                                 pad = "0")) %>%
    # Remove duplicates that can arise from cases when captureDate is the same in Capture and Nestling data
    dplyr::distinct() %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureTagID = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       TRUE ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  releaseTagID = stringr::str_sub(.data$individualID, 5, nchar(.data$individualID)),
                  # Create captureID
                  captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    # Set non-conforming IDs to NA
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(stringr::str_sub(.data$individualID, 5,
                                                                                       nchar(.data$individualID)),
                                                                      "^[:alpha:]{1,2}[:digit:]{5,6}$") ~ .data$individualID,
                                                  TRUE ~ NA_character_))


  # Add optional variables
  output <- captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$captureStage,
                                                                                            protocol_version = "2.0") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}

  return(output)

}

#' Create individual table for Harjavalta, Finland.
#'
#' Create full individual data table in standard format for data from Harjavalta, Finland.
#'
#' @param capture_data Data frame. Output of \code{\link{create_capture_HAR}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_individual_HAR <- function(capture_data,
                                  optional_variables){

  # Take capture data and determine summary data for each individual
  individuals <- capture_data %>%
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::arrange(.data$individualID,
                   .data$captureYear,
                   .data$captureDate,
                   .data$captureTime) %>%
    # For every individual...
    dplyr::group_by(.data$individualID) %>%
    # ... determine species, first brood, ring date, ring stage
    # TODO: Check CCCCCC individuals with data custodian
    dplyr::summarise(speciesID = dplyr::case_when(length(unique(.data$speciesID)) == 2 ~ "CCCCCC",
                                                  TRUE ~ dplyr::first(.data$speciesID)),
                     firstBrood = dplyr::first(.data$broodID),
                     tagAge = dplyr::first(.data$age),
                     tagStage = dplyr::first(.data$captureStage),
                     tagDate = dplyr::first(.data$captureDate),
                     firstYear = dplyr::first(.data$captureYear),
                     tagSiteID = dplyr::first(.data$captureSiteID)) %>%
    # Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    # If capture date is NA, use year column from primary data for tagYear instead
    dplyr::mutate(tagYear = dplyr::case_when(is.na(.data$tagDate) ~ as.integer(.data$firstYear),
                                              TRUE ~ as.integer(lubridate::year(.data$tagDate))),
                  tagMonth = as.integer(lubridate::month(.data$tagDate)),
                  tagDay = as.integer(lubridate::day(.data$tagDate)),
                  # Determine stage at tagging, either chick (before fledging), subadult (1-year-old),
                  # or adult (2-year-old or older)
                  tagStage = dplyr::case_when(.data$tagAge %in% c("1", "+1") ~ "subadult",
                                               TRUE ~ .data$tagStage),
                  # Only assign brood ID if individual was caught as a chick
                  broodIDLaid = dplyr::case_when(.data$tagStage != "chick" ~ NA_character_,
                                                 TRUE ~ .data$firstBrood),
                  # Cross-fostering has been done, but there is no info on which chicks were swapped and two where
                  # TODO: Check with data custodian
                  broodIDFledged =.data$broodIDLaid,
                  siteID = .data$tagSiteID,
                  studyID = "HAR-1")

  # Add optional variables
  output <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = capture_data) else .}

  return(output)

}

#' Create location table for Harjavalta, Finland.
#'
#' Create full location data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#'
#' @return A data frame.
#' @export

create_location_HAR <- function(db){

  message("Extracting location data from paradox database")

  # Extract table "Paikat.db" which contains location data
  location_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Paikat.DB") %>%
    # Remove last 2 columns that have no info
    dplyr::select(-"Aukko", -"Malli") %>%
    # Rename columns to English (based on description provided by data custodian)
    dplyr::rename(year = "Vuos",
                  locationPlotID = "Nuro",
                  forestType = "Mety",
                  pinusSylvestris = "Manty",
                  piceaAbies = "Kuusi",
                  betulaSpp = "Koivu",
                  populusTremula = "Haapa",
                  sorbusAcuparia = "Pihlaja",
                  salixSpp = "Pajut",
                  juniperusCommunis = "Kataja",
                  alnusSpp = "Leppa",
                  prunusPadas = "Tuomi",
                  treeHeight = "Kork",
                  basalArea = "Totrel",
                  pineHeight = "Makor",
                  spruceHeight = "Kukor",
                  birchHeight = "Kokor",
                  pineBasalArea = "Marel",
                  spruceBasalArea = "Kurel",
                  birchBasalArea = "Korel",
                  latitude = "Leve",
                  longitude = "Pitu",
                  municipality = "Kunta",
                  locationName = "Paikka") %>%
    # Create locationID and plotID
    # plotID = first two digits
    # locationID = plotID_nestbox number, where nestbox number is last two digits
    dplyr::mutate(plotID = paste("HAR",
                                 stringr::str_sub(string = .data$locationPlotID,
                                                  start = 1,
                                                  end = 2),
                                 sep = "_"),
                  locationID = paste(.data$plotID,
                                     stringr::str_sub(string = .data$locationPlotID,
                                                      start = 3,
                                                      end = 4),
                                     sep = "_"))

  # Many nest boxes change location over the years
  # Assign new locationID when they do
  location_data <- location_data %>%
    # Store old locationID as locationCode
    dplyr::mutate(locationCode = .data$locationID) %>%
    dplyr::group_by(.data$locationPlotID) %>%
    # Concatenate latitude and longitude, set to factor to create unique indexes every time
    # the coordinates of a single locationID change
    # If the coordinates of a single locationID do not change, the index is not added to the new ID
    dplyr::mutate(location = paste(.data$latitude, .data$longitude, sep = ";"),
                  locationNumber = as.integer(forcats::as_factor(.data$location)),
                  locationID = dplyr::case_when(length(unique(.data$location)) > 1 ~ paste(.data$locationCode, .data$locationNumber, sep = "_"),
                                                TRUE ~ .data$locationCode)) %>%
    dplyr::ungroup()

  # Separate locations with and without coordinates
  location_nocoord <- location_data %>%
    dplyr::filter(is.na(.data$longitude))

  location_wcoord  <- location_data %>%
    dplyr::filter(!is.na(.data$longitude))

  # Read location data with coordinates as sf object in Finnish Coordinate system
  # Coordinates are in Finland Uniform Coordinate System (EPSG 2393)
  location_data_sf <- sf::st_as_sf(location_wcoord,
                                   coords = c("longitude", "latitude"),
                                   crs = 2393) %>%
    sf::st_transform(crs = 4326)

  location_full <- location_wcoord %>%
    dplyr::mutate(longitude = sf::st_coordinates(location_data_sf)[, 1],
                  latitude = sf::st_coordinates(location_data_sf)[, 2]) %>%
    dplyr::bind_rows(location_nocoord) %>%
    dplyr::mutate(locationType = "nest",
                  studyID = "HAR-1",
                  siteID = "HAR",
                  # TODO: check habitat type with data custodian
                  habitatID = NA_character_)

  return(location_full)

}

#' Create measurement table for Harjavalta, Finland.
#'
#' Create measurement data table in standard format for data from Harjavalta, Finland.
#'
#' @param capture_data Data frame. Output of \code{\link{create_capture_HAR}}.
#'
#' @return A data frame.
#'

create_measurement_HAR <- function(capture_data) {

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we use capture_data as input
  measurements <- capture_data %>%
    dplyr::select(recordID = "captureID",
                  siteID = "captureSiteID",
                  measurementDeterminedYear = "captureYear",
                  measurementDeterminedMonth = "captureMonth",
                  measurementDeterminedDay = "captureDay",
                  measurementDeterminedTime = "captureTime",
                  "recordedBy",
                  "totalWingLength",
                  "tarsusLength",
                  "leftTarsusLength",
                  "rightTarsusLength",
                  "mass",
                  "leftP3",
                  "rightP3",
                  "headLength",
                  "leftRectrix",
                  "rightRectrix") %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = "totalWingLength":"rightRectrix",
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(measurementMethod = dplyr::case_when(.data$measurementType == "tarsusLength" ~ "alternative",
                                                       .data$measurementType == "leftTarsusLength" ~ "left; alternative",
                                                       .data$measurementType == "rightTarsusLength" ~ "right; alternative",
                                                       .data$measurementType == "leftRectrix" ~ "left outermost rectrix",
                                                       .data$measurementType == "rightRectrix" ~ "right outermost rectrix",
                                                       .data$measurementType == "leftP3" ~ "left",
                                                       .data$measurementType == "rightP3" ~ "right"),
                  measurementType = dplyr::case_when(stringr::str_detect(.data$measurementType, "[t|T]arsusLength") ~ "tarsus",
                                                     stringr::str_detect(.data$measurementType, "Rectrix") ~ "tail length",
                                                     stringr::str_detect(.data$measurementType, "P3") ~ "p3",
                                                     .data$measurementType == "totalWingLength" ~ "total wing length",
                                                     .data$measurementType == "headLength" ~ "total head length",
                                                     TRUE ~ .data$measurementType),
                  measurementUnit = dplyr::case_when(.data$measurementType == "mass" ~ "g",
                                                     TRUE ~ "mm"),
                  measurementID = 1:dplyr::n(),
                  measurementSubject = "capture") %>%
    dplyr::arrange(.data$measurementDeterminedYear,
                   .data$measurementDeterminedMonth,
                   .data$measurementDeterminedDay,
                   .data$measurementDeterminedTime)

  return(measurements)

}

#' Create experiment table for Harjavalta, Finland.
#'
#' Create experiment data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param brood_data Data frame. Output of \code{\link{create_brood_HAR}}.
#'
#' @return A data frame.
#'

create_experiment_HAR <- function(db,
                                  brood_data) {

  experiments <- brood_data %>%
    dplyr::select(year = "observedLayYear",
                  month = "observedLayMonth",
                  day = "observedLayDay",
                  treatmentCode = "experimentID",
                  "broodID") %>%
    # Remove non-experimental broods
    dplyr::filter(!is.na(.data$treatmentCode)) %>%
    # Group by experiment code: year_experimentID
    dplyr::mutate(treatmentCode = as.integer(.data$treatmentCode)) %>%
    # Using information provided by the data custodian (in meta data file), distinguish treatments from experiments
    # E.g., 1994_1, 1994_2, 1994_3 are three treatments of the experiment carried out in 1994
    # whilst 2004_1 and 2004_2 seem to be two different experiments carried out in 2004.
    dplyr::mutate(experimentType = dplyr::case_when(.data$year == 1994 ~ "calcium experiment, brood size manipulation",
                                                    .data$year %in% 1995:1997 ~ NA_character_,
                                                    .data$year == 1998 & .data$treatmentCode %in% 1:3 ~ "carotene experiment, clutch size manipulation",
                                                    .data$year == 1998 & .data$treatmentCode %in% 4:5 ~ NA_character_,
                                                    .data$year == 1998 & .data$treatmentCode %in% 6:7 ~ "incubation cost experiment, clutch size manipulation",
                                                    .data$year == 1998 & .data$treatmentCode %in% 8:9 ~ "cost of immunization experiment",
                                                    .data$year == 1999 & .data$treatmentCode %in% 1:4 ~ "food supplementation",
                                                    .data$year == 1999 & .data$treatmentCode == 5 ~ NA_character_,
                                                    .data$year == 1999 & .data$treatmentCode %in% 6:7 ~ "immunization experiment, brood size manipulation",
                                                    .data$year %in% 2000:2003 ~ NA_character_,
                                                    .data$year == 2004 & .data$treatmentCode == 1 ~ "water and lutein supplementation",
                                                    .data$year == 2004 & .data$treatmentCode == 2 ~ "clutch size manipulation",
                                                    .data$year == 2005 & .data$treatmentCode %in% 1:4 ~ "food supplementation, clutch size manipulation",
                                                    .data$year == 2005 & .data$treatmentCode == 5 ~ "clutch size manipulation",
                                                    .data$year %in% 2006:2010 ~ NA_character_,
                                                    .data$year == 2011 & .data$treatmentCode %in% 1:5 ~ "iron exposure experiment, brood size manipulation",
                                                    .data$year == 2011 & .data$treatmentCode == 6 ~ NA_character_,
                                                    .data$year %in% 2012:2013 ~ NA_character_,
                                                    .data$year == 2014 ~ "calcium experiment",
                                                    .data$year == 2015 & .data$treatmentCode %in% 0:4 ~ "arsenic experiment",
                                                    .data$year == 2015 & .data$treatmentCode %in% 5:6 ~ NA_character_,
                                                    .data$year == 2016 ~ NA_character_,
                                                    .data$year == 2017 & .data$treatmentCode == 1 ~ NA_character_,
                                                    .data$year == 2017 & .data$treatmentCode == 2 ~ "brood size manipulation",
                                                    .data$year == 2018 ~ NA_character_),
                  treatmentDetails = dplyr::case_when(.data$year == 1994 & .data$treatmentCode == 1 ~ "control",
                                                      .data$year == 1994 & .data$treatmentCode == 2 ~ "calcium",
                                                      .data$year == 1994 & .data$treatmentCode == 3 ~ "nestling donor",
                                                      .data$year == 1998 & .data$treatmentCode == 1 ~ "control, water, eggs swapped",
                                                      .data$year == 1998 & .data$treatmentCode == 2 ~ "lutein, eggs swapped",
                                                      .data$year == 1998 & .data$treatmentCode == 3 ~ "egg donor",
                                                      .data$year == 1998 & .data$treatmentCode == 6 ~  "control, eggs swapped",
                                                      .data$year == 1998 & .data$treatmentCode == 7 ~ "+/- 2 eggs",
                                                      .data$year == 1998 & .data$treatmentCode == 8 ~ "saline control",
                                                      .data$year == 1998 & .data$treatmentCode == 9 ~ "diphteria-tetanus vaccination",
                                                      .data$year == 1999 & .data$treatmentCode == 1 ~ "control, polluted area",
                                                      .data$year == 1999 & .data$treatmentCode == 2 ~ "control unpolluted area",
                                                      .data$year == 1999 & .data$treatmentCode == 3 ~ "mealworms and fat, polluted area",
                                                      .data$year == 1999 & .data$treatmentCode == 4 ~ "mealworms and fat, unpolluted area",
                                                      .data$year == 1999 & .data$treatmentCode == 6 ~ "+/- 2 nestlings",
                                                      .data$year == 1999 & .data$treatmentCode == 7 ~ "control",
                                                      .data$year == 2004 & .data$treatmentCode == 1 ~ "water and lutein",
                                                      .data$year == 2004 & .data$treatmentCode == 2 ~ "-1 egg",
                                                      .data$year == 2005 & .data$treatmentCode == 1 ~ "caterpillars, mealworms, water; -1 egg",
                                                      .data$year == 2005 & .data$treatmentCode == 2 ~ "mealworms, water; -1 egg",
                                                      .data$year == 2005 & .data$treatmentCode == 3 ~ "mealworms, lutein, -1 egg",
                                                      .data$year == 2005 & .data$treatmentCode == 4 ~ "control, water; -1 egg",
                                                      .data$year == 2005 & .data$treatmentCode == 5 ~ "-1 egg",
                                                      .data$year == 2011 & .data$treatmentCode == 1 ~ "-1 nestling",
                                                      .data$year == 2011 & .data$treatmentCode == 2 ~ "-2 nestlings",
                                                      .data$year == 2011 & .data$treatmentCode == 3 ~ "-3 nestlings",
                                                      .data$year == 2011 & .data$treatmentCode == 4 ~ "control",
                                                      .data$year == 2011 & .data$treatmentCode == 5 ~ "trial",
                                                      .data$year == 2014 & .data$treatmentCode == 1 ~ "calcium added, -1 egg",
                                                      .data$year == 2014 & .data$treatmentCode == 2 ~ "control, -1 egg",
                                                      .data$year == 2014 & .data$treatmentCode == 3 ~ "control",
                                                      .data$year == 2015 & .data$treatmentCode == 0 ~ "trial",
                                                      .data$year == 2015 & .data$treatmentCode == 1 ~ "water, control",
                                                      .data$year == 2015 & .data$treatmentCode == 2 ~ "low",
                                                      .data$year == 2015 & .data$treatmentCode == 3 ~ "high",
                                                      .data$year == 2015 & .data$treatmentCode == 4 ~ "water, polluted",
                                                      .data$year == 2017 & .data$treatmentCode == 2 ~ "nestlings swapped")) %>%
    # Remove non-experimental codings
    dplyr::filter(!is.na(.data$experimentType)) %>%
    # Create experimentCode (year_treatmentCode), used to join experiment data into brood data
    dplyr::mutate(experimentCode = paste(.data$year, .data$treatmentCode, sep = "_"))

  # Information on persons who visited the nests can be found in Visitit.DB
  # These are likely the persons who conducted the experiments
  message("Extracting data on nest visits from paradox database")

  observers <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Visitit.DB") %>%
    # Rename columns to English (based on description provided by data custodian)
    dplyr::rename(year = "Vuos",
                  locationPlotID = "Nuro",
                  nestAttemptNumber = "Anro",
                  month = "Kk",
                  day = "Pv",
                  time = "Klo",
                  recordedBy = "Havno",
                  eggNumber = "Mun",
                  chickNumber = "Poik",
                  comments = "Komm",
                  breedingStage = "Tila",
                  ticks = "Ticks") %>%
    # Create unique BroodID with year_locationPlotID_nestAttemptNumber
    dplyr::mutate(broodID = paste(.data$year,
                                  .data$locationPlotID,
                                  .data$nestAttemptNumber,
                                  sep = "_")) %>%
    dplyr::group_by(.data$broodID) %>%
    # Alphabetize recorder IDs to ensure the same order of IDs across broods
    dplyr::arrange(.data$recordedBy, .by_group = TRUE) %>%
    # Concatenate unique observer IDs into a single string
    dplyr::summarise(recordedBy = paste(unique(.data$recordedBy), collapse = " | "),
                     .groups = "drop")

  # Add observer IDs to experiment data
  experiments <- experiments %>%
    dplyr::left_join(observers, by = "broodID") %>%
    # Create experiment IDs: year_index
    dplyr::arrange(.data$year, .data$treatmentCode) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(experimentID = paste(.data$year, as.integer(forcats::as_factor(.data$experimentType)),
                                       sep = "_")) %>%
    # Create treatment IDs (within experiment IDs): experimentID_index
    dplyr::group_by(.data$experimentID, .add = FALSE) %>%
    dplyr::mutate(treatmentID = paste(.data$experimentID, as.integer(forcats::as_factor(.data$treatmentCode)),
                                      sep = "_"),
                  # Determine the stage during which the experiment was conducted
                  # - if it involves eggs: incubation
                  # - if it involves nestlings: nestling period
                  # TODO: Check with data custodian
                  treatmentStage = dplyr::case_when(stringr::str_detect(string = .data$experimentType,
                                                                        pattern = "brood size") ~ "nestling",
                                                    any(stringr::str_detect(string = .data$treatmentDetails,
                                                                            pattern = "egg")) ~ "incubation"),
                  studyID = "HAR-1",
                  siteID = "HAR") %>%
    dplyr::ungroup()

  # Get start and end dates of experiments
  experiment_times <- experiments %>%
    dplyr::group_by(.data$experimentID) %>%
    # Start of experiment is set to the laying date of the first brood each year
    # Experiments are assumed to be within a year, so that experimentEndYear = experimentStartYear,
    # but experimentEndMonth and experimentEndDay are unknown
    # TODO: Check with data custodian
    dplyr::mutate(treatmentStartDate = lubridate::make_date(year = .data$year,
                                                            month = .data$month,
                                                            day = .data$day)) %>%

    dplyr::summarise(treatmentStartYear = as.integer(lubridate::year(dplyr::first(.data$treatmentStartDate))),
                     treatmentStartMonth = as.integer(lubridate::month(dplyr::first(.data$treatmentStartDate))),
                     treatmentStartDay = as.integer(lubridate::day(dplyr::first(.data$treatmentStartDate))),
                     treatmentEndYear = .data$treatmentStartYear,
                     # Create unique and sorted list of observer IDs, e.g., when different broods/nests in the same
                     # treatment group have been visited by different (but overlapping) groups of observers
                     recordedBy = paste(sort(unique(unlist(stringr::str_split(string = .data$recordedBy,
                                                                              pattern = "\\s\\|\\s")))),
                                        collapse = " | "),
                     .groups = "drop")

  # Add experiment times to experiment data table
  output <- experiments %>%
    dplyr::select("experimentID",
                  "treatmentID",
                  "experimentCode",
                  "studyID",
                  "siteID",
                  "experimentType",
                  "treatmentStage",
                  "treatmentDetails") %>%
    dplyr::distinct() %>%
    dplyr::left_join(experiment_times, by = "experimentID")

  return(output)

}

#----------------------#
# TODO: Condition == 'D'. Are these captures dead recoveries or did the birds die while handling?
# TODO: Capture method == 'M'. Are these sightings rather than physical captures?
# TODO: Ring numbers 000000. Should this be NA?
# TODO: Are nestlings' right tarsus length measures in 10*mm?
# TODO: Check chick ring series. Four nests must contain typos, because the resulting number of chicks is > 100.
# TODO: Verify habitatID
# TODO: Check plotID-locationID concatenation
# TODO: Check plotID-locationIDs in Capture data missing from Location data
# TODO: Check timing (start & end) of experiments, as well as the experimentStage
# TODO: How to treat sampling for DNA/biomarker analysis in Experiment_data table? And "egg" or "nestling" taken?
# TODO: Is there more cross-fostering info on individual chicks? To fill in broodIDLaid vs. broodIDFledged
# TODO: Some experiment/treatment codes are not in metadata. Check. E.g. 2014-4.
