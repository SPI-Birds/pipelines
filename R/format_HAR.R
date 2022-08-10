#'Construct standard format for data from Harjavalta, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: Data from Harjavalta contains information on 23 different species. We only include records for species with at least 50 broods throughout the study period: pied flycatcher, great tit, blue tit, coal tit, European crested tit, and common redstart. The few records for other species (e.g., Eurasian wryneck, common starling, house sparrow) are excluded.
#'
#'\strong{Age}: Chick age is listed as: PP (nestling), PM (fledgling), FL(unknown),
#'1, +1, 2, +2. Translation from field notebook would translate PP/PM into
#'EURING 1. PP are chicks in the nest. PM are chick left the nest but caught by
#'hand (i.e. still not able to fly). 1 = 3 (i.e. fully grown in first year).
#'2 = 5 (i.e. fully grown and known to be born last year).
#'+1/+2 are 4/6 respectively, at least 1/2 years old.
#'FL is unknown. We can't even attribute it to chick or adult.
#'For this we use EURING 2 (able to fly freely but otherwise unknown).
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
#'#1. Individual captured as adult (Age is NA or not PM/PP) (e.g. 00-829590).
#'
#'In this case, we assume that there is only information in Capture_data, and we
#'do not search for any information in Nestling_data.
#'
#'\item #2. Record of chick (Age is PM/PP) with no information in the last ring
#'number column AND no corresponding record in Nestling_data (i.e. no record
#'where BroodID and last 2 digits of the ring number were the same) (e.g.
#'HL-025681).
#'
#'In this case, we assume that the information in Capture_data is the correct
#'capture information.
#'
#'\item #3.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, with corresponding record in Nestling_data, BUT where the
#'capture dates in Nestling_data and Capture_data do not match (e.g. HA-30262)
#'
#'In this case, we assume that the individual was captured multiple times and
#'the records in Nestling_data and Capture_data are both true capture records.
#'All information from both tables is kept.
#'
#'\item #4.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, corresponding record in Nestling_data that was taken at exactly
#'the same capture date (e.g. HA-30103)
#'
#'Where there are measurements (e.g. mass, tarsus) in both Nestling_data and
#'Capture_data, we give precedence to the data from Nestling_data. We only use
#'data from Capture_data when there is no data in Nestling_data
#'
#'\item #5.	Record of chick (Age is PM/PP) with information in the last ring
#'number column which matches data in Nestling_data (e.g. HL-26103 - 26107)
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
#'collected on them (e.g. Mass). We give them an 'empty' record in Capture_data
#'(e.g. only IndvID, CaptureDate, CaptureTime).
#'
#'\item #7. Record in Nestling_data that has no ring number information (e.g. A,
#'B, C, D) (e.g. Brood 2011_0003_1)
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
#'number. Currently, we include them as captures with IndvID = NA. These can
#'still be used to estimate AvgChickMass, AvgTarsus for a given BroodID even
#'though the individual cannot be identified; however, we suspect these are
#'mistakes in the primary data.}
#'
#'\strong{Mass}: Mass of birds is measured in mg. This is converted
#'to grams to match other populations.
#'
#'\strong{Tarsus}: Tarsus length is measured for both left and right leg.
#'Generally, only left leg is reported and so is used here. Tarsus measurement
#'in adults can be either left or right leg.
#'
#'\strong{Sex}: Bird classified as 'likely male' or 'likely female' are simply
#'given code 'M' and 'F' respectively (i.e. this uncertainty is ignored).
#'
#'\strong{BroodID}: Unique BroodID values are generated using
#'BreedingSeason_LocationID_BroodID
#'
#'@inheritParams pipeline_params
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
                                           species_filter = species,
                                           optional_variables = optional_variables)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_HAR(db = db)

  #CURRENTLY ASSUMING THAT EACH LOCATION AND NEST BOX ARE IDENTICAL
  #GO THROUGH AND CHECK MORE THOROUGHLY

  # WRANGLE DATA FOR EXPORT

  ## Add average chick mass and tarsus to brood data

  Chick_avg <- Capture_data %>%
    dplyr::filter(dplyr::between(ChickAge, 14, 16)) %>%
    #Remove cases where tarsus or weight are 0 (make them NA)
    dplyr::mutate(Mass = dplyr::na_if(Mass, 0),
                  Tarsus = dplyr::na_if(Tarsus, 0)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA, NumberEggs = NA,
                     AvgChickMass = mean(Mass, na.rm = TRUE),
                     NumberChicksMass = n(),
                     AvgTarsus = mean(Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = n(),
                     OriginalTarsusMethod = "Alternative")

  #Join these into Brood_data
  #Only existing broods will be used.
  Brood_data <- left_join(Brood_data, Chick_avg, by = "BroodID") %>%
    dplyr::select(BroodID:NumberFledgedError, AvgEggMass:OriginalTarsusMethod, ExperimentID)

  Capture_data <- Capture_data %>%
    dplyr::select(-Sex, -BroodID)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HAR.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}

#' Create brood data table for Harjavalta, Finland.
#'
#' Create brood data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#'  @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_brood_HAR <- function(db,
                             species_filter,
                             optional_variables){

  message("Extracting brood data from paradox database")

  # Extract table "Pesat.db" which contains brood data
  # Rename columns to English (based on description provided by data owner)
  # Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  # see what is being removed.
  broods <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Pesat.DB") %>%
    dplyr::rename(year = .data$Vuos,
                  locationID = .data$Nuro,
                  nestAttemptNumber = .data$Anro,
                  speciesID = .data$Laji,
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
                  nestlingInjuries = .data$Jalat,
                  malePresent = .data$Koir,
                  experimentID = .data$Koe,
                  expData1 = .data$Olent,
                  expData2 = .data$Vlent,
                  deadParent = .data$Delfh,
                  eggShells = .data$Mkuor,
                  tempCode1 = .data$Tark,
                  tempCode2 = .data$Tark2) %>%
    # Remove unwanted columns
    dplyr::select(-.data$reasonFailed:-.data$malePresent,
                  -.data$expData1:-.data$tempCode2) %>%
    # Create IDs
    # Create unique BroodID with year_locationID_nestAttemptNumber
    dplyr::mutate(broodID = paste(.data$year,
                                  .data$locationID,
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
                                                                       pattern = "^[:alpha:]{1,2}[-][:digit:]{5,6}$") ~ paste0("HAR_", stringr::str_remove_all(.x, "[:punct:]")),
                                                   TRUE ~ NA_character_)

                                }),
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
                                                                        protocol_version = "1.2") else .} %>%
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
    # Rename columns to English (based on description provided by data owner)
    dplyr::rename(captureYear = .data$Vuos,
                  locationID = .data$Nuro,
                  nestAttemptNumber = .data$Anro,
                  captuerMonth = .data$Kk,
                  captureDay = .data$Pv,
                  captureTime = .data$Klo,
                  nestlingNumber = .data$Poik,
                  last2DigitsRingNumber = .data$Reng,
                  dead = .data$Dead,
                  totalWingLength = .data$Siipi,
                  mass = .data$Paino,
                  leftLegAbnormalities = .data$Vjalka,
                  rightLegAbnormalities = .data$Ojalka,
                  leftP3 = .data$Vkas,
                  rightP3 = .data$Okas,
                  leftRectrix = .data$Vpys,
                  rightRectrix = .data$Opys,
                  leftTarsusLength = .data$Vnil,
                  rightTarsusLength = .data$Onil,
                  leftTarsusWidth = .data$Vpak,
                  rightTarsusWidth = .data$Opak,
                  greatTitBreastYellow = .data$Vari,
                  luteinSupplementation = .data$Lkoe,
                  bloodSample = .data$Wb,
                  columnLengthBlood = .data$Tot,
                  lengthBlood = .data$Pun,
                  breastFeatherLutein = .data$FetLut,
                  nailClipping = .data$Varpaat,
                  geneticSex = .data$Sp,
                  headLength = .data$Head,
                  faecalSample1 = .data$Feces1,
                  faecalSample2 = .data$Feces2,
                  tempCode = .data$Tark) %>%
    # Create unique broodID (year_locationID_nestAttemptNumber)
    dplyr::mutate(broodID = paste(.data$year,
                                  .data$locationID,
                                  .data$nestAttemptNumber,
                                  sep = "_"),
                  captureDate = lubridate::make_date(year = .data$captureYear,
                                                     month = .data$captureMonth,
                                                     day = .data$captureDay),
                  captureTime = dplyr::na_if(x = paste0(.data$captureTime, ":00"),
                                             y = "NA:00")) %>%
    # Join hatch date data from brood data table
    dplyr::left_join(brood_data %>%  dplyr::select(.data$broodID, .data$observedHatchDate),
                     by = "broodID") %>%
    # Determine age at capture
    dplyr::mutate(chickAge = as.integer(.data$captureDate - .data$observedHatchDate))

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
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#' @param return_errors Logical. Return those records with errors in the ring sequence.
#'
#' @return A data frame.

create_capture_HAR    <- function(db,
                                  brood_data,
                                  species_filter,
                                  optional_variables,
                                  return_errors){

  # Extract nestling data
  nestling_data <- create_nestling_HAR(db = db,
                                       brood_data = brood_data)

  message("Extracting capture data from paradox database")

  # Extract table "Rengas.db" which contains ringing data
  # N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
  # e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
  # 662470, 662471, 662472, 662473
  # The number of nestlings ringed is stored in nestlingNumber (Poik).
  capture_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Rengas.DB") %>%
    # Rename columns to English (based on description provided by data owner)
    dplyr::rename(ringSeries = .data$Sarja,
                  ringNumber = .data$Mista,
                  captureType = .data$Tunnus,
                  captureYear = .data$Vuos,
                  captureMonth = .data$Kk,
                  captureDay = .data$Pv,
                  captureTime = .data$Klo,
                  locationID = .data$Nuro,
                  nestAttemptNumber = .data$Anro,
                  recordedBy = .data$Havno,
                  lastRingNumber = .data$Mihin,
                  speciesID = .data$Laji,
                  observedSex = .data$Suku,
                  sexMethod = .data$Sp,
                  age = .data$Ika,
                  ageMethod = .data$Ip,
                  ringType = .data$Rtapa,
                  condition = .data$Kunto,
                  birdStatus = .data$Tila,
                  captureMethod = .data$Ptapa,
                  nestlingNumber = .data$Poik,
                  totalWingLength = .data$Siipi,
                  mass = .data$Paino,
                  moult = .data$Sulsat,
                  fatScore = .data$Rasik,
                  extraInfo = .data$Lisa,
                  plumage = .data$Vari,
                  tailFeather = .data$Psulka,
                  columnLengthBlood = .data$Tot,
                  lengthBlood = .data$Pun,
                  tarsusLength = .data$Tarsus,
                  breastMuscle = .data$Lihas,
                  headLength = .data$Head,
                  ticks = .data$Ticks,
                  tempCode = .data$Tark)

  capture_data <- capture_data %>%
    # Create unique broodID (year_locationID_nestAttemptNumber)
    dplyr::mutate(broodID = paste(.data$year,
                                  .data$locationID,
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
    # Filter species and remove unknown species
    dplyr::filter(!is.na(.data$speciesID) & .data$speciesID %in% {{species_filter}}) %>%
    dplyr::mutate(observedSex = dplyr::case_when(.data$observedSex %in% c("N", "O") ~ "F",
                                                 .data$observedSex %in% c("K", "L") ~ "M"),
                  mass = dplyr::na_if(.data$Mass, 0),
                  totalWingLength = dplyr::na_if(.data$totalWingLength, 0))

  # We have eight scenarios we need to deal with in the capture data.
  # See an explanation in the help documentation:

  ####

  # 1. Individual adult captures
  adult_capture <- capture_data %>%
    dplyr::filter(!.data$age %in% c("PP", "PM") | is.na(.data$age)) %>%
    dplyr::mutate(captureType = "adult",
                  last2DigitsRingNumber = NA,
                  chickAge = NA) %>%
    dplyr::mutate(individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) #%>%
    # dplyr::select(individualID, captureYear, captureDate, captureTime, recordedBy,
    #               locationID, broodID, speciesID, observedSex, age, totalWingLength, mass, captureType,
    #               birdStatus, last2DigitsRingNumber, chickAge)

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
    dplyr::left_join(brood_data %>% dplyr::select(.data$broodID, .data$observedHatchDate),
                     by = "broodID") %>%
    dplyr::mutate(captureType = "chick",
                  last2DigitsRingNumber = NA,
                  chickAge = as.integer(.data$captureDate - .data$observedHatchDate),
                  individualID = paste0("HAR_",
                                        .data$ringSeries,
                                        .data$ringNumber)) #%>%
    # dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, Last2DigitsRingNr, ChickAge)

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
                       dplyr::select(.data$broodID, .data$last2DigitsRingNumber, captureDateNestling = .data$captureDate),
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
                       dplyr::select(.data$broodID, .data$last2DigitsRingNumber, .data$captureDate),
                     by = c("broodID", "last2DigitsRingNumber", "captureDate")) %>%
    dplyr::left_join(brood_data %>%
                       dplyr::select(.data$broodID, .data$observedHatchDate),
                     by = "broodID") %>%
    dplyr::mutate(chickAge = as.integer(.data$captureDate - .data$observedHatchDate)) #%>%
    # dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
    #               Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  #>><<<<<<<<<<<<>>>>>>>>>>>
  #>><<<<<<<<<<<<>>>>>>>>>>>
  #>><<<<<<<<<<<<>>>>>>>>>>>
  #>><<<<<<<<<<<<>>>>>>>>>>>

  # Go through the nestling data records and do the same thing
  unique_individual_nestlings <- Nestling_data %>%
    dplyr::left_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2)) %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, RingSeries, RingNumber, Species),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::filter(IndvID %in% non_matching_records) %>%
    dplyr::anti_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2)) %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, CaptureDate),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::mutate(ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = "5",
                  BirdStatus = NA_character_) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
                  Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  indv_chick_multirecord <- dplyr::bind_rows(unique_individual_captures, unique_individual_nestlings)

  #Go through and find the exact matches
  indv_chick_record_conflict <- indv_chick_capture %>%
    #Join Nestling data and filter those cases where the same date is present
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::left_join(dplyr::select(Nestling_data, BroodID, Last2DigitsRingNr, CaptureDateNestling = CaptureDate,
                                   CaptureTimeNestling = CaptureTime, MassNestling = Mass, WingLengthNestling = WingLength, HatchDate, ChickAge),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    #Filter those cases that are individual captures (i.e. no last ring number)
    #Find cases where there was mass and/or wing length in the nestling data
    #AND where the CaptureDate in the Nestling data is different to Capture_data
    dplyr::filter(CaptureDate == CaptureDateNestling) %>%
    dplyr::mutate(Mass = purrr::map2_dbl(.x = .$Mass, .y = .$MassNestling, .f = ~{

      if(is.na(..2)){

        return(..1)

      } else {

        return(..2)

      }

    }),
    WingLength = purrr::map2_dbl(.x = .$WingLength, .$WingLengthNestling,
                                 ~{

                                   if(is.na(..2)){

                                     return(..1)

                                   } else {

                                     return(..2)

                                   }

                                 })) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
                  Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #5. Cases where the record in capture is for multiple chicks
  #The individual data is stored in nestling data
  flat_multi_chick_capture <- chick_data %>%
    dplyr::filter(!is.na(LastRingNumber))

  ring_pb <- progress::progress_bar$new(total = nrow(flat_multi_chick_capture))

  expanded_multi_chick_capture <- flat_multi_chick_capture %>%
    #Split data into rowwise list
    purrr::pmap(~c(...)) %>%
    #map over each row
    purrr::map_dfr(function(current_row){

      ring_pb$tick()

      #Determine first part of Ringnumber
      ring_start <- stringr::str_sub(current_row["RingNumber"], end = -6)

      #We use the last 5 digits (rather than last 2) to deal with
      #cases where the ring series passes 10000 (e.g. 9999 - 0000).
      ring_ends  <- stringr::str_sub(current_row["RingNumber"], start = -5):stringr::str_sub(current_row["LastRingNumber"], start = -5)
      #Pad numbers with leading 0s to ensure they're all the right length
      ring_ends  <- stringr::str_pad(ring_ends, 5, pad = "0")

      #Create a list of all chicks in the series
      All_rings <- paste0(ring_start, ring_ends)

      return(tibble::tibble(LocationID = current_row["LocationID"], BroodID = current_row["BroodID"], RingSeries = current_row["RingSeries"],
                            RingNumber = All_rings, BreedingSeason = as.integer(current_row["BreedingSeason"]),
                            Species = current_row["Species"], Sex = current_row["Sex"],
                            Age = current_row["Age"], ObserverID = current_row["ObserverID"],
                            CaptureType = current_row["CaptureType"], BirdStatus = current_row["BirdStatus"]))

    })

  #Remove names so that it doesn't break tests
  #We need to come up with a more robust fix for this (i.e. change the above code!)
  expanded_multi_chick_capture <- expanded_multi_chick_capture %>%
    mutate(across(.cols = where(~!is.null(names(.))), .fns = ~setNames(., NULL)))

  #Now, for each recorded chick ring number determine the last 2 digits of the ring
  multirecord_captures <- expanded_multi_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  Capture_type = "Chick") %>%
    #Join in all nestling data where the broodID and Last2Digits is the same
    #N.B. We do left_join with BroodID and Last2Digits, so we can get multiple
    #records for each chick when they were captured more than once
    dplyr::left_join(Nestling_data %>% select(BroodID, CaptureDate, CaptureTime,
                                              Last2DigitsRingNr, WingLength,
                                              Mass, ChickAge), by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #6. Nestling records that don't correspond to any capture data
  #Brood/RingNumber combos from individual captures
  single_capture_records <- indv_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, stringr::str_sub(RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull(Brood_RingNr)

  #Brood/RingNumber combos from multi-captures
  multi_capture_records <- expanded_multi_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, stringr::str_sub(RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull(Brood_RingNr)

  nocapture_nestlings <- Nestling_data %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, Last2DigitsRingNr, sep = "_")) %>%
    dplyr::filter(!Brood_RingNr %in% c(single_capture_records, multi_capture_records))

  #6a. Filter unringed individuals. These are just given records associated with a brood where the individual ID is unknown.
  unringed_chicks <- nocapture_nestlings %>%
    dplyr::filter(toupper(Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_, ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, ChickAge)

  ringed_chicks_nocapture <- nocapture_nestlings %>%
    dplyr::filter(!toupper(Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_, ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, ChickAge)

  message(paste0("There are ", nrow(ringed_chicks_nocapture), " nestling records where we cannot translate Last2Digits into an IndvID"))

  if(return_errors){

    return(nocapture_nestlings %>%
             dplyr::filter(!toupper(Last2DigitsRingNr) %in% LETTERS))

  }

  ####

  #Now that we have dealt with all 6 scenarios, we can join the data back together.
  Capture_data_expanded <- dplyr::bind_rows(Adult_capture, indv_chick_capture_only, indv_chick_multirecord,
                                            indv_chick_record_conflict, multirecord_captures, unringed_chicks, ringed_chicks_nocapture)

  ####

  message("Calculating age at each capture...")

  Capture_data_output <- Capture_data_expanded %>%
    #Make Age_observed, that doesn't require any calculation, just uses observations at the time of capture
    #PP and PM = 1, these are chicks in nest and chicks outside the nest but not yet able to fly
    #1 = 3 (able to fly but in first year),
    #1+ = 4 (hatched atleast 1 year ago), 2 = 5 (known to have hatched last year),
    #2+ = 6 (hatched at least 2 years ago)
    #FL is 2. We cannot even distinguish it as adult or chick. We just know it's fully grown.
    dplyr::mutate(Mass = Mass/10,
                  CapturePopID = "HAR", CapturePlot = NA_character_,
                  ReleasePopID = "HAR", ReleasePlot = NA_character_,
                  Age_observed = dplyr::case_when(Age %in% c("PM", "PP") ~ 1L,
                                                  Age == "FL" ~ 2L,
                                                  Age == "1" ~ 3L,
                                                  Age == "+1" ~ 4L,
                                                  Age == "2" ~ 5L,
                                                  Age == "+2" ~ 6L)) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::mutate(Tarsus = NA_real_, OriginalTarsusMethod = NA_character_,
                  ## Set non-conforming IDs to NA
                  IndvID = dplyr::case_when(stringr::str_detect(.data$IndvID, "^[:alpha:]{1,2}[-][:digit:]{5,6}$") ~ .data$IndvID,
                                            TRUE ~ NA_character_)) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge, Sex, BroodID, CaptureType, BirdStatus) %>%
    #Remove duplicates that can arise from cases when CaptureDate is the same in Capture and Nestling
    dplyr::distinct()

  return(Capture_data_output)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  LastRingNumber_Brood <- Age <- NrNestlings <- RingNumber <- NULL
  Capture_type <- Last2DigitsRingNr <- Month <- Wing <- LeftTarsusLength <- NULL
  `.` <- Ring_Time <- Final_BreedingSeason <- Final_Month <- Final_Day <- Final_Time <- Day <- ischick <- NULL

}

#' Create individual table for Harjavalta, Finland.
#'
#' Create full individual data table in standard format for data from Harjavalta, Finland.
#'
#' @param capture_data Data frame. Output of \code{\link{create_capture_HAR}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_individual_HAR <- function(capture_data,
                                  species_filter,
                                  optional_variables){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }), PopID = "HAR",
              BroodIDLaid = first(BroodID),
              BroodIDFledged = BroodIDLaid,
              RingSeason = first(BreedingSeason),
              RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
              Sex = purrr::map_chr(.x = list(unique(stats::na.omit(Sex))), .f = ~{

                if(length(..1) == 0){

                  return(NA_character_)

                } else if(length(..1) == 1){

                  return(..1)

                } else {

                  return("C")

                }

              })) %>%
    dplyr::rowwise() %>%
    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
           BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

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
  Location_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Paikat.DB") %>%
    #Remove last 2 cols that have no info
    dplyr::select(-Aukko, -Malli) %>%
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  ForestType = Mety, PinusSylvestris = Manty,
                  PiceaAbies = Kuusi, Betulasp = Koivu,
                  PopulusTremula = Haapa,
                  SorbusAcuparia = Pihlaja,
                  Salixsp = Pajut, JuniperusCommunis = Kataja,
                  Alnussp = Leppa, PrunusPadas = Tuomi,
                  TreeHeight = Kork, BasalArea = Totrel,
                  PineHeight = Makor, SpruceHeight = Kukor,
                  BirchHeight = Kokor, PineBasalArea = Marel,
                  SpruceBasalArea = Kurel, BirchBasalArea = Korel,
                  Latitude = Leve, Longitude = Pitu, Municipality = Kunta,
                  LocationName = Paikka)

  #Separate locations with and without coordinates
  Location_nocoord <- Location_data %>%
    dplyr::filter(is.na(Longitude))

  Location_wcoord  <- Location_data %>%
    dplyr::filter(!is.na(Longitude))

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #Coordinates are in Finland Uniform Coordinate System (EPSG 2393)

  Location_data_sf <- sf::st_as_sf(Location_wcoord,
                                         coords = c("Longitude", "Latitude"),
                                         crs = 2393) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_rows(dplyr::bind_cols(dplyr::select(Location_wcoord, -Longitude, -Latitude),
                                    tibble(Longitude = sf::st_coordinates(Location_data_sf)[, 1]),
                                    tibble(Latitude = sf::st_coordinates(Location_data_sf)[, 2])),
                                    Location_nocoord)

  #The records of each Location should show the start/end season
  Location_data <- Location_full %>%
    dplyr::group_by(LocationID) %>%
    dplyr::arrange(BreedingSeason, .by_group = TRUE) %>%
    dplyr::summarise(NestboxID = unique(LocationID), PopID = "HAR", Latitude = as.numeric(first(Latitude)), Longitude = as.numeric(first(Longitude)),
                     LocationType = "NB", StartSeason = min(BreedingSeason), EndSeason = max(BreedingSeason), Habitat = "Evergreen") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
