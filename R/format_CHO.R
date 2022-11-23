#'Construct standard format for data from Choupal, Portugal.
#'
#'A pipeline to produce the standard format for the great tit population in
#'Choupal, Portugal, administered by the University of Coimbra.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{observedNumberFledged}: This population has no estimation of actual fledgling
#'numbers. The last time nests are counted is 14 days post hatching. We use this
#'as an estimation of fledgling numbers. This also affects
#'\emph{calculatedClutchType} as we use the estimate of fledgling numbers to
#'distinguish second/replacement clutches.
#'
#'\strong{Age}: Age records are used to inform the life stage at ringing, from which \emph{exactAge} and \emph{minimumAge} can be determined:
#'\itemize{
#'\item 'C', individuals ringed as chicks, which equals EURING code 1: 'Pullus: nestling or chick, unable to fly freely, still able to be caught by hand.'
#'\item 'first year', individuals ringed as subadults, which equals EURING code 5: 'Bird hatched last calendar year and now in its second calendar year.'
#'\item 'adult' or NA, individuals ringed as adults, which equals EURING code 6: 'Full-grown bird
#'hatched before last calendar year; year of hatching otherwise unknown.'
#'}
#'
#'\strong{observedClutchType}: In the raw data, there is no distinction between
#''second' and 'replacement' clutches. Any clutch recorded as '2nd' is assumed
#'to be a 'second' clutch under our definitions. 'calculatedClutchType' may
#'give a more appropriate estimate of clutch type for this data.
#'
#'\strong{observedClutchSize, observedBroodSize, observedNumberFledged}: We currently only use records
#'of clutch, brood, and fledgling numbers that are recorded explicitly in the
#'data. This means that there are some nests where chicks have capture records,
#'but the \emph{Brood data} table will not give any value of observedNumberFledged
#'(e.g., see broodID 2004_NA). These capture records should be included, but we
#'need to determine the amount of potential uncertainty around these records.
#'
#'\strong{individualID}: Individuals marked as "no ring", "not ringed" or "branco" are removed from Capture_data and Individual_data. Check with data owner on how to handle these.
#'
#'\strong{captureAlive, releaseAlive}: Assume all individuals were alive when captured and released.
#'
#'\strong{capturePhysical}: Assume all individuals were physically captured.
#'
#'\strong{captureTagID}: First captures of all individuals are assumed to be ringing events, and thus captureTagID is set to NA.
#'
#'\strong{locationID}: For individuals captured in mist nets (specified by
#'trapping method column), a single locationID "MN1" is used.
#'
#'\strong{startYear}: Assume all boxes were placed in the first year of the study.
#'
#'\strong{habitatID}: Assume that habitat type is 1.4: Forest - Temperate. Check with data owner.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_CHO <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R"){

  # Force choose_directory() if used
  force(db)

  # Add species filter
  if(is.null(species)){

    species_filter <- species_codes$speciesID

  } else {

    species_filter <- species

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user.
  start_time <- Sys.time()

  # Read in data with readxl
  all_data <- readxl::read_excel(paste(db, "CHO_PrimaryData.xlsx", sep = "/")) %>%
    # Clean all names with janitor into snake_case
    janitor::clean_names(case = "upper_camel") %T>%
    # There is one column with "º" that doesn't convert to ASCII with janitor
    # This appears the be a deeper issue than janitor (potentially in unicode translation)
    # Therefore, we will do the translation manually
    {colnames(.) <- iconv(colnames(.), "", "ASCII", sub = "")} %>%
    # Change species to "PARMAJ" because it's only PARMAJ in Choupal
    dplyr::mutate(speciesID = species_codes[which(species_codes$speciesCode == 10001), ]$speciesID,
                  siteID = "CHO",
                  plotID = NA_character_) %>%
    dplyr::filter(speciesID %in% species_filter) %>%
    # broodIDs are not unique (they are repeated each year)
    # We need to create unique IDs for each year using year_broodID
    dplyr::mutate(broodID = dplyr::case_when(.data$TrapingMethod == "mist net" ~ NA_character_,
                                             TRUE ~ paste(.data$Year, stringr::str_pad(.data$BroodId,
                                                                                       width = 3, pad = "0"),
                                                          sep = "_")),
                  # If individualID differs from expected format, set to NA
                  individualID = dplyr::case_when(stringr::str_detect(.data$Ring, "^[C][:digit:]{6}$") ~ .data$Ring,
                                                  TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(is.na(.data$individualID) ~ NA_character_,
                                                  TRUE ~ paste0("CHO_", .data$individualID)),
                  captureDate = lubridate::ymd(paste0(.data$Year, "-01-01")) + .data$JulianDate,
                  captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = format.POSIXct(.data$Time, format = "%H:%M:%S"),
                  chickAge = as.integer(dplyr::na_if(.data$ChickAge, "na")),
                  stage = dplyr::case_when(.data$Age == "C" ~ "chick",
                                           .data$Age == "first year" ~ "subadult",
                                           .data$Age == "adult" ~ "adult",
                                           TRUE ~ "adult"),
                  # If an individual was caught in a mist net give a generic LocationID (MN1)
                  # Otherwise, give the box number.
                  locationID = purrr::pmap_chr(.l = list(.data$TrapingMethod, as.character(.data$Box)),
                                               .f = ~{

                                                 if(..1 == "mist net"){

                                                   return("MN1")

                                                 } else {

                                                   return(stringr::str_pad(..2, width = 3, pad = "0"))

                                                 }

                                               })) %>%
    tibble::as_tibble()

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_CHO(data = all_data,
                                     optional_variables = optional_variables)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_CHO(data = all_data,
                                 optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_CHO(data = all_data,
                                           Capture_data = Capture_data,
                                           optional_variables = optional_variables)

  # MEASUREMENT DATA

  message("Compiling measurement information...")

  Measurement_data <- create_measurement_CHO(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_CHO(data = all_data)

  # EXPERIMENT DATA

  message("Compiling experiment information...")

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v2.0$Experiment_data[0,]

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[1, !(names(data_templates$v2.0$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Brood_data <- Brood_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[1, !(names(data_templates$v2.0$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Individual_data <- Individual_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[1, !(names(data_templates$v2.0$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

  # EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_CHO.csv"), row.names = FALSE)

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

#' Create brood data table for Choupal, Portugal.
#'
#' Create brood data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_brood_CHO <- function(data,
                             optional_variables = NULL){

  # The data is currently stored as capture data (i.e., each row is a capture)
  # This means there are multiple records for each brood, which we don't want.

  # Remove all mist-net captures
  data <- data %>%
    dplyr::filter(.data$TrapingMethod != "mist net")

  # Identify any adults caught on the brood
  # Reshape data so that maleID and femaleID are separate columns for each brood
  Parents <- data %>%
    # Remove all records with chicks
    dplyr::filter(.data$stage != "chick") %>%
    # Select only the Brood, Individual Id and their sex
    dplyr::select("broodID",
                  "individualID",
                  "Sex") %>%
    # Reshape data so that we have a maleID and femaleID column
    # Rather than an individual row for each parent capture
    tidyr::pivot_longer(cols = "individualID") %>%
    tidyr::pivot_wider(names_from = "Sex",
                       values_from = "value") %>%
    dplyr::rename(femaleID = "F",
                  maleID = "M") %>%
    dplyr::select(-"name") %>%
    dplyr::arrange(.data$broodID)

  # Determine whether clutches are 2nd clutch
  Brood_info <- data %>%
    # For each brood, if a clutch is listed as '2nd' make it 'second' otherwise
    # 'first'. For ClutchType_observed we are not giving broods the value
    # 'replacement' as we don't have enough info.
    dplyr::group_by(.data$broodID) %>%
    dplyr::summarise(observedClutchType = ifelse("2nd" %in% .data$SecondClutch, "second", "first"))

  # NB: Deprecated (v2.0)
  # Finally, we add in average mass and tarsus measured for all chicks at 14 - 16d
  # Subset only those chicks that were 14 - 16 days when captured.
  # avg_measure <- data %>%
  #   dplyr::filter(.data$Age == "C" & !is.na(.data$ChickAge) & dplyr::between(.data$ChickAge, 14, 16)) %>%
  #   #For every brood, determine the average mass and tarsus length
  #   dplyr::group_by(.data$BroodID) %>%
  #   dplyr::summarise(AvgChickMass = mean(.data$Weight, na.rm = TRUE),
  #                    NumberChicksMass = length(stats::na.omit(.data$Weight)),
  #                    AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
  #                    NumberChicksTarsus = length(stats::na.omit(.data$Tarsus))) %>%
  #   dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Alternative"))

  Brood_data <- data %>%
    # Join in information on parents and clutch type
    dplyr::left_join(Parents, by = "broodID") %>%
    dplyr::left_join(Brood_info, by = "broodID") %>%
    # Now we can melt and reshape our data
    # Remove columns that do not contain relevant brood info
    dplyr::select(-"CodeLine":-"Ring",
                  -"JulianDate":-"StanderdisedTime",
                  -"TrapingMethod",
                  -"BroodId":-"Smear",
                  -"TotalEggWeight", -"individualID") %>%
    # Turn all remaining columns to characters
    # melt/cast requires all values to be of the same type
    dplyr::mutate_all(as.character) %>%
    # Melt and cast data so that we return the first value of relevant data for each brood
    # e.g. laying date, clutch size etc.
    # I've checked manually and the first value is always correct in each brood
    dplyr::group_by(.data$broodID, .data$speciesID, .data$Year, .data$Site, .data$Box, .data$femaleID, .data$maleID) %>%
    dplyr::summarise(dplyr::across(.cols = everything(),
                                   .fns = first),
                     .groups = "drop") %>%
    # Convert LayDate and HatchDate to date objects
    dplyr::mutate(observedLayDate = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$LayingDateJulian),
                  observedLayYear = as.integer(lubridate::year(.data$observedLayDate)),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  minimumLayYear = NA_integer_,
                  minimumLayMonth = NA_integer_,
                  minimumLayDay = NA_integer_,
                  maximumLayYear = NA_integer_,
                  maximumLayMonth = NA_integer_,
                  maximumLayDay = NA_integer_,
                  observedClutchSize = as.integer(.data$FinalClutchSize),
                  minimumClutchSize = NA_integer_,
                  maximumClutchSize = NA_integer_,
                  observedHatchDate = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$HatchingDateJulian),
                  observedHatchYear = as.integer(lubridate::year(.data$observedHatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  minimumHatchYear = NA_integer_,
                  minimumHatchMonth = NA_integer_,
                  minimumHatchDay = NA_integer_,
                  maximumHatchYear = NA_integer_,
                  maximumHatchMonth = NA_integer_,
                  maximumHatchDay = NA_integer_,
                  observedBroodSize = as.integer(.data$NoChicksHatched),
                  minimumBroodSize = NA_integer_,
                  maximumBroodSize = NA_integer_,
                  observedFledgeYear = NA_integer_,
                  observedFledgeMonth = NA_integer_,
                  observedFledgeDay = NA_integer_,
                  minimumFledgeYear = NA_integer_,
                  minimumFledgeMonth = NA_integer_,
                  minimumFledgeDay = NA_integer_,
                  maximumFledgeYear = NA_integer_,
                  maximumFledgeMonth = NA_integer_,
                  maximumFledgeDay = NA_integer_,
                  observedNumberFledged = as.integer(.data$NoChicksOlder14D),
                  minimumNumberFledged = NA_integer_,
                  maximumNumberFledged = NA_integer_,
                  row = 1:dplyr::n(),
                  rowWarning = NA,
                  rowError = NA) %>%
    # In cases where LayingDateJulian is unknown but Year is known, we set observedLayYear = Year
    dplyr::mutate(observedLayYear = dplyr::case_when(is.na(.data$observedLayYear) & !is.na(.data$Year) ~ as.integer(.data$Year),
                                                     is.na(.data$observedLayYear) & is.na(.data$Year) ~ NA_integer_,
                                                     TRUE ~ .data$observedLayYear))

  # Add optional variables
  output <- Brood_data %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$Year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "2.0") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  return(output)

}

#' Create capture data table for Choupal, Portugal.
#'
#' Create capture data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_capture_CHO <- function(data,
                               optional_variables = NULL){

  # Take all data and add study site/plot info
  # There is only one study site/plot
  Capture_data <- data %>%
    dplyr::mutate(captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID) %>%
    # Filter out individuals without individualID
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureDate, .data$captureTime) %>%
    dplyr::group_by(.data$individualID) %>%
    # First captures are assumed to be ringing events, and thus captureTagID = NA.
    dplyr::mutate(captureTagID = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                  TRUE ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  # All releases are assumed to be alive (also see releaseAlive), so no NAs in releaseTagID
                  releaseTagID = stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))) %>%
    dplyr::ungroup() %>%
    # Arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureDate, .data$captureTime) %>%
    # Replace 'na' with NA in Sex
    dplyr::mutate(observedSex = dplyr::na_if(x = .data$Sex, y = "na"),
                  recordedBy = NA_character_,
                  # We have no information on status of captures/releases, so we assume all individuals were captured/released alive
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  # We also assume that all captures were physical captures
                  capturePhysical = TRUE,
                  treatmentID = NA_character_,
                  row = 1:dplyr::n(),
                  rowWarning = NA,
                  rowError = NA,
                  originalTarsusMethod = "Alternative") %>%
    # Select columns that are in the standard format
    # + measurement columns (needed for input of create_measurement_CHO())
    dplyr::select("individualID",
                  "captureTagID",
                  "releaseTagID",
                  "speciesID",
                  "observedSex",
                  "captureYear",
                  "captureMonth",
                  "captureDay",
                  "captureTime",
                  "recordedBy",
                  "locationID",
                  "capturePhysical",
                  "captureAlive",
                  "releaseAlive",
                  "captureSiteID",
                  "capturePlotID",
                  "releaseSiteID",
                  "releasePlotID",
                  "chickAge",
                  "treatmentID",
                  "row",
                  "rowWarning",
                  "rowError",
                  "stage",
                  mass = "Weight",
                  tarsus = "Tarsus",
                  "originalTarsusMethod",
                  wingLength = "Wing") %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("captureID", tidyselect::everything())

  # Add optional variables
  output <- Capture_data %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = ., Age = .data$stage, protocol_version = "2.0") %>% dplyr::select(dplyr::contains(c(names(Capture_data), optional_variables))) else .}

  return(output)

}

#' Create individual data table for Choupal, Portugal.
#'
#' Create individual data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#' @param Capture_data Data frame. Output from \code{\link{create_capture_CHO}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.

create_individual_CHO <- function(data,
                                  Capture_data,
                                  optional_variables = NULL){

  Individual_data <- data %>%
    # Filter out individuals without individualID
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureDate, .data$captureYear,
                   .data$captureMonth, .data$captureDay, .data$captureTime) %>%
    # For every individual
    dplyr::group_by(data$individualID) %>%
    # Determine first age, brood, ring year, month, day, and ring site of each individual
    dplyr::summarise(firstBrood = dplyr::first(.data$broodID),
                     ringDate = dplyr::first(.data$captureDate),
                     ringYear = as.integer(dplyr::first(.data$Year)),
                     ringMonth = as.integer(lubridate::month(.data$ringDate)),
                     ringDay = as.integer(lubridate::day(.data$ringDate)),
                     ringStage = dplyr::first(.data$stage),
                     speciesID = species_codes[which(species_codes$speciesCode == 10001), ]$speciesID,
                     ringSiteID = dplyr::first(.data$siteID)) %>%
    # Only assign a brood ID if they were first caught as a chick
    # Otherwise, the broodID will be their first clutch as a parent
    dplyr::mutate(broodIDLaid = dplyr::case_when(is.na(.data$ringStage) | .data$ringStage != "chick" ~ NA_character_,
                                                 TRUE ~ .data$firstBrood),
                  # We have no information on cross-fostering, so we assume the brood laid and ringed are the same
                  broodIDFledged = .data$broodIDLaid) %>%
    # NB: Sex calculation moved to standard utility function (v2.0)
    #dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row = 1:dplyr::n(),
                  rowWarning = NA,
                  rowError = NA)

  # Add optional variables
  output <- Individual_data %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = ., capture_data = Capture_data) else .}

  return(output)

}

#' Create location data table for Choupal, Portugal.
#'
#' Create location data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#'
#' @return A data frame.

create_location_CHO <- function(data){

  # There are no coordinates or box type information
  Location_data <- dplyr::tibble(locationID = stats::na.omit(unique(data$locationID))) %>%
    dplyr::mutate(locationType = dplyr::case_when(.data$locationID == "MN1" ~ "capture",
                                                  .data$locationID != "MN1" ~ "nest"),
                  siteID = "CHO",
                  decimalLatitude = NA_real_,
                  decimalLongitude = NA_real_,
                  startYear = 2003L,
                  endYear = NA_integer_,
                  habitatID = "1.4", # Formerly: HabitatType: Deciduous
                  row = 1:dplyr::n(),
                  rowWarning = NA,
                  rowError = NA) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Location_data[1, !(names(data_templates$v2.0$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Location_data))

  return(Location_data)

}

#' Create measurement data table for Choupal, Portugal.
#'
#' Create measurement data table in standard format for data from Choupal, Portugal.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_CHO}}.
#'
#' @return A data frame.

create_measurement_CHO <- function(Capture_data){

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we only use Capture_data as input
  Measurement_data <- Capture_data %>%
    dplyr::select(recordID = "captureID",
                  siteID = "captureSiteID",
                  measurementDeterminedYear = "captureYear",
                  measurementDeterminedMonth = "captureMonth",
                  measurementDeterminedDay = "captureDay",
                  measurementDeterminedTime = "captureTime",
                  "recordedBy",
                  "mass",
                  "tarsus",
                  "wingLength",
                  "originalTarsusMethod") %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement (i.e., mass, tarsus, and wing length) of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = c("mass", "tarsus", "wingLength"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::arrange(.data$measurementDeterminedYear, .data$measurementDeterminedMonth, .data$measurementDeterminedDay) %>%
    dplyr::mutate(measurementID = 1:dplyr::n(),
                  measurementSubject = "capture",
                  measurementUnit = dplyr::case_when(.data$measurementType == "mass" ~ "g",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "tarsus" ~ "alternative",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType from camel case to lower case & space-separated
                  measurementType = stringr::str_to_lower(gsub("([[:upper:]])", " \\1", .data$measurementType)),
                  row = 1:dplyr::n(),
                  rowWarning = NA,
                  rowError = NA) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Measurement_data[1, !(names(data_templates$v2.0$Measurement_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Measurement_data))

  return(Measurement_data)

}

#----------------------#
#TODO Check with data owner how to handle "no ring", "not ringed", or "branco" individuals
#TODO Check habitatID with data owner
