#' Construct standard format for data from Westerholz, Germany
#'
#' A pipeline to produce the standard format for the nest box population in Westerholz,
#' Germany, administered by Bart Kempenaers, Max Planck Institute for Biological Intelligence, Seewiesen, Germany.
#'
#' This section provides details on data management choices that are unique to this data. For a general description of the standard format please see \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#' \strong{broodID}: Brood IDs are of the form <year_nest box number_nesting attempt>.
#'
#' \strong{individualID}: Individual IDs are mostly 7 characters long, where the first and third are capital letters, and the remaining are digits. Other IDs (e.g., starting with X) are used too, but they seem to indicate IDs for individuals that died prematurely. Verify with data owner.
#'
#' \strong{captureRingNumber, releaseRingNumber}: First captures of all individuals are assumed to be ringing events, and thus captureRingNumber is set to NA. Only when individual IDs follow the expected format (see \strong{individualID}), the ID is considered to be a ring number and stored in captureRingNumber and/or releaseRingNumber. When, for example, the ID is longer than 7 characters, the ID is considered to be a place holder rather than a ring number, and captureRingNumber and releaseRingNumber are set to NA.
#'
#' \strong{treatmentID}: Some broods, adult captures, and chick captures are recorded as experimental. Treatment IDs are of the form <year_nest box number>. It seems that there is more info on experiments. Ask data owner.
#'
#' \strong{startYear}: Assume all boxes were placed in the first year of study.
#'
#' \strong{habitatID}: Assume that habitat type is 1.4: Forest - Temperate. Check with data owner.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format.
#'
#' @export
#'

format_WHZ <- function(db = choose_directory(),
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

  # Create a temporary in-memory RSQLite database to execute SQL queries on
  connection <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = ":memory:")

  # Clean SQL files
  queries <- paste0(db, "\\WHZ_PrimaryData_",
                    c("BREEDING", "NESTS", "ADULTS", "CHICKS",
                      "SEX", "BOX_geoCoordinates"),
                    ".sql") %>%
    purrr::map(.f = ~ clean_query_WHZ(.x)) %>%
    unlist()

  # Execute SQL queries on database
  purrr::walk(.x = queries,
              .f = ~ DBI::dbExecute(conn = connection, statement = .x))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_WHZ(connection = connection,
                                 optional_variables = optional_variables)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_WHZ(connection = connection,
                                     optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_WHZ(capture_data = Capture_data,
                                           brood_data = Brood_data,
                                           optional_variables = optional_variables)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_WHZ(connection = connection)

  # MEASUREMENT DATA

  message("Compiling measurement information...")

  Measurement_data <- create_measurement_WHZ(capture_data = Capture_data)

  # EXPERIMENT DATA

  message("Compiling experiment information...")

  Experiment_data <- create_experiment_WHZ(brood_data = Brood_data,
                                           capture_data = Capture_data)

  # Disconnect from database
  DBI::dbDisconnect(conn = connection)

  # WRANGLE DATA FOR EXPORT

  # - Brood data
  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  # - Capture data
  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Capture_data[1, !(names(data_templates$v1.2$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  # - Individual data
  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Individual_data[1, !(names(data_templates$v1.2$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

  # - Location data
  Location_data <- Location_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Location_data[1, !(names(data_templates$v1.2$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Location_data))

  # - Measurement data
  Measurement_data <- Measurement_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Measurement_data[1, !(names(data_templates$v1.2$Measurement_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Measurement_data))

  # - Experiment data
  Experiment_data <- Experiment_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Experiment_data[1, !(names(data_templates$v1.2$Experiment_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Experiment_data))


  # TIME

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WHZ.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WHZ.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WHZ.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_WHZ.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WHZ.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_WHZ.csv"), row.names = FALSE)

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

#' Create brood data table for Westerholz, Germany.
#'
#' Create brood data table in standard format for data from Westerholz, Germany.
#'
#' @param connection Connection the SQL database.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_WHZ <- function(connection,
                             optional_variables) {

  broods <- dplyr::tbl(connection, "BREEDING") %>%
    dplyr::rename(year = .data$year_,
                  femaleID = .data$IDfemale,
                  maleID = .data$IDmale,
                  observedClutchSize = .data$clutch,
                  observedBroodSize = .data$hatched,
                  observedNumberFledged = .data$fledged) %>%
    # Create IDs
    dplyr::mutate(siteID = "WHZ",
                  # LocationID: institutionID_nest box number
                  locationID = paste("WHZ", .data$box, sep = "_"),
                  # TODO: Check with data owner how to interpret experiment IDs
                  treatmentID = dplyr::case_when(!is.na(.data$experimental) ~ paste(.data$year, .data$box, sep = "_"),
                                                 TRUE ~ NA_character_)) %>%
    # Create broodID: year_nest box number_breeding attempt
    dplyr::group_by(.data$year, .data$box) %>%
    dbplyr::window_order(.data$firstEgg) %>%
    dplyr::mutate(broodID = dplyr::case_when(all(!is.na(.data$firstEgg)) ~ paste(.data$year, .data$box, dplyr::row_number(), sep = "_"),
                                             TRUE ~ paste(.data$year, .data$box, {.data$secondClutch + 1L}, sep = "_"))) %>%
    dplyr::ungroup() %>%
    # TODO: Check on how to interpret second clutches (secondClutch == 1) when first clutches
    # are not recorded
    dplyr::mutate(observedClutchType = dplyr::case_when(.data$secondClutch == 0 ~ "first",
                                                         .data$secondClutch == 1 ~ "second"),
                  observedLayYear = as.integer(lubridate::year(.data$firstEgg)),
                  observedLayMonth = as.integer(lubridate::month(.data$firstEgg)),
                  observedLayDay = as.integer(lubridate::day(.data$firstEgg)),
                  observedHatchYear = as.integer(lubridate::year(.data$hatchDate)),
                  observedHatchMonth = as.integer(lubridate::month(.data$hatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$hatchDate)),
                  observedFledgeYear = as.integer(lubridate::year(.data$fledgeDate)),
                  observedFledgeMonth = as.integer(lubridate::month(.data$fledgeDate)),
                  observedFledgeDay = as.integer(lubridate::day(.data$fledgeDate))) %>%
    # Above code is interpreted as SQL (for quicker run-time)
    # Force computation of the database query to run the remaining code
    dplyr::collect()

  output <- broods %>%
    # If femaleID & maleID differ from expected format, set to NA
    # Ensure that individuals are unique:
    # add institutionID as prefix to femaleID & maleID
    dplyr::mutate(dplyr::across(.cols = c(.data$femaleID, .data$maleID),
                                .fns = ~{

                                  dplyr::case_when(stringr::str_detect(.x, "^[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$") ~ paste0("WHZ_", .x),
                                                   TRUE ~ NA_character_)

                                }),
                  # Set speciesID
                  speciesID = species_codes$speciesID[species_codes$speciesCode == "10002"]) %>%
    # Add optional variables
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.2") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  return(output)

}


#' Create capture data table for Westerholz, Germany.
#'
#' Create capture data table in standard format for data from Westerholz, Germany.
#'
#' @param connection Connection the SQL database.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_WHZ <- function(connection,
                               optional_variables) {

  # Adults
  adults <- dplyr::tbl(connection, "ADULTS") %>%
    dplyr::rename(year = .data$year_,
                  individualID = .data$ID,
                  captureDate = .data$capture_date_time,
                  observedSex = .data$sex,
                  recordedBy = .data$author,
                  total_wing_length = .data$wing,
                  mass = .data$weight) %>%
    # Convert dates from character to datetime
    dplyr::mutate(captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  observedSex = dplyr::case_when(.data$observedSex == 1 ~ "M",
                                                 .data$observedSex == 2 ~ "F",
                                                 TRUE ~ .data$observedSex),
                  age = dplyr::case_when(.data$age == 1 ~ "subadult",
                                         .data$age == 2 ~ "adult")) %>%
    # Above code is interpreted as SQL (for quicker run-time)
    # Force computation of the database query to run the remaining code
    dplyr::collect()

  adults <- adults %>%
    # If individualID differs from expected format, set to NA
    # Ensure that individuals are unique: add institutionID as prefix to individualID
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(.data$individualID, "^[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$") ~ paste0("WHZ_", .data$individualID),
                                                  TRUE ~ NA_character_),
                  # In the case that individuals died in hand or during capture/measuring (see .data$comments),
                  # captureAlive = TRUE & releaseAlive = FALSE.
                  # In the case of other deaths, captureAlive = releaseAlive = FALSE
                  foundDead = dplyr::case_when(stringr::str_detect(.data$comments, "bird bag") ~ FALSE,
                                               stringr::str_detect(.data$comments, "died before") ~ FALSE,
                                               stringr::str_detect(.data$comments, "died during") ~ FALSE,
                                               stringr::str_detect(.data$comments, "died while") ~ FALSE,
                                               stringr::str_detect(.data$comments, "died after") ~ FALSE,
                                               stringr::str_detect(.data$comments, "died in hand") ~ FALSE,
                                               stringr::str_detect(.data$comments, "fell down") ~ FALSE,
                                               TRUE ~ TRUE),
                  captureAlive = dplyr::case_when(.data$dead == 1 & .data$foundDead == TRUE ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$dead == 1 ~ FALSE,
                                                  TRUE ~ TRUE),
                  # When adults were part of experimental treatments (experimental == 2),
                  # create treatment ID consisting of year and nest box number.
                  # TODO: Retrieve more experiment info from data owner.
                  treatmentID = dplyr::case_when(.data$experimental == 2 ~ paste(.data$year, .data$box, sep = "_"),
                                                 TRUE ~ NA_character_)) %>%
    dplyr::select(.data$individualID,
                  .data$captureDate,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$recordedBy,
                  .data$box,
                  .data$captureAlive,
                  .data$releaseAlive,
                  .data$treatmentID,
                  .data$observedSex,
                  .data$age,
                  .data$tarsus,
                  .data$mass,
                  .data$total_wing_length,
                  .data$P3)

  # Chicks
  chicks <- dplyr::tbl(connection, "CHICKS") %>%
    dplyr::rename(year = .data$year_,
                  individualID = .data$ID,
                  recordedBy = .data$author,
                  mass = .data$weight) %>%
    # Remove egg samples and unknown individualIDs
    # TODO: Check with data owner
    dplyr::filter(is.na(.data$dead_egg), is.na(.data$broken_egg), !is.na(.data$individualID),
                  dplyr::sql("(`sample` NOT LIKE '%egg%')"), dplyr::sql("(`sample` NOT LIKE '%tissue%')")) %>%
    # There are two date columns:
    # - date-time: Date and time of chick/egg measures
    # - collect-date: Date and time of dead chick/egg collection
    # If these dates do not match, there are multiple captures in a single row
    tidyr::pivot_longer(cols = c(.data$date_time, .data$collect_date),
                        names_to = "captureType",
                        values_to = "captureDate") %>%
    # Convert dates from character to datetime
    dplyr::mutate(captureDate = dplyr::case_when(.data$captureDate == "0000-00-00 00:00:00" ~ NA_character_,
                                                 is.na(.data$captureDate) ~ NA_character_,
                                                 TRUE ~ .data$captureDate),
                  captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate))) %>%
    # Remove captures with unknown dates
    dplyr::filter(!is.na(captureDate)) %>%
    # Keep one row, if date-time & collect-date match
    dplyr::distinct(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay,
                    .keep_all = TRUE) %>%
    dplyr::group_by(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay) %>%
    dplyr::mutate(captureAlive = dplyr::case_when(.data$captureType == "date_time" ~ TRUE,
                                                  .data$captureType == "collect_date" & .data$dead_chick == 1 ~ FALSE),
                  releaseAlive = dplyr::case_when(.data$captureType == "date_time" & is.na(.data$dead_chick) ~ TRUE,
                                                  .data$captureType == "date_time" & dplyr::n() > 1 ~ TRUE,
                                                  .data$captureType == "date_time" & .data$dead_chick == 1 & dplyr::n() == 1  ~ FALSE,
                                                  .data$captureType == "collect_date" ~ FALSE)) %>%
    dplyr::ungroup() %>%
    # When chicks were part of experimental treatments (experimental == 1),
    # create treatment ID consisting of year and nest box number.
    # TODO: Retrieve more experiment info from data owner.
    dplyr::mutate(treatmentID = dplyr::case_when(.data$experimental == 1 ~ paste(.data$year, .data$box, sep = "_"),
                                                 TRUE ~ NA_character_)) %>%
    # Above code is interpreted as SQL (for quicker run-time)
    # Force computation of the database query to run the remaining code
    dplyr::collect()

  chicks <- chicks %>%
    # If individualID differs from expected format, set to NA
    # Ensure that individuals are unique: add institutionID as prefix to individualID
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(.data$individualID, "^[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$|^X[:digit:]{8}$|^X[:digit:]{2}_[:digit:]{3}[:upper:]{0,1}_[:digit:]{4}$") ~ paste0("WHZ_", .data$individualID),
                                                  TRUE ~ NA_character_),
                  # SQLite does not have data type 'logical', so set 1 to TRUE and 0 to FALSE
                  dplyr::across(.cols = c(.data$captureAlive, .data$releaseAlive),
                                .fns = ~ {

                                  as.logical(.x)

                                }),
                  age = "chick",
                  total_wing_length = NA_real_,
                  P3 = NA_real_,
                  observedSex = NA_character_) %>%
    dplyr::select(.data$individualID,
                  .data$captureDate,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$recordedBy,
                  .data$box,
                  .data$captureAlive,
                  .data$releaseAlive,
                  .data$treatmentID,
                  .data$observedSex,
                  .data$age,
                  .data$tarsus,
                  .data$mass,
                  .data$total_wing_length,
                  .data$P3)

  # Bind adults and chicks
  captures <- dplyr::bind_rows(adults, chicks) %>%
    # Remove unknown individuals
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Set speciesID
    dplyr::mutate(speciesID = species_codes$speciesID[species_codes$speciesCode == "10002"],
                  # Set locationID: institutionID_nest box number
                  locationID = paste0("WHZ_", .data$box),
                  # Extract captureTime
                  captureTime = paste(stringr::str_pad(string = lubridate::hour(.data$captureDate),
                                                       width = 2,
                                                       pad = "0"),
                                      stringr::str_pad(string = lubridate::minute(.data$captureDate),
                                                       width = 2,
                                                       pad = "0"),
                                      sep = ":"),
                  # All possible capture methods (.data$catch_method;  box, snap trap, or mist net)
                  # seem to be physical captures, so we assume that they are
                  capturePhysical = TRUE,
                  # TODO: Check chick ages with data owner
                  chickAge = NA_integer_,
                  captureSiteID = "WHZ",
                  releaseSiteID = .data$captureSiteID) %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay) %>%
    dplyr::group_by(.data$individualID) %>%
    # First captures are assumed to be ringing events, and thus captureRingNumber = NA.
    # captureRingNumber and releaseRingNumber are only filled if individualID follows the expected format,
    # otherwise (e.g., when ID starts with X), they are set to NA.
    # TODO: Check with data owner
    dplyr::mutate(captureRingNumber = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       dplyr::row_number() != 1 & stringr::str_detect(.data$individualID, "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$", negate = TRUE) ~ NA_character_,
                                                       dplyr::row_number() != 1 & stringr::str_detect(.data$individualID, "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$") ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  releaseRingNumber = dplyr::case_when(stringr::str_detect(.data$individualID, "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$", negate = TRUE) ~ NA_character_,
                                                       TRUE ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  # set captureID
                  captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_"))

  # Add optional variables
  output <-  captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            protocol_version = "1.2") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}

}


#' Create individual data table for Westerholz, Germany.
#'
#' Create individual data table in standard format for data from Westerholz, Germany.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_WHZ}}.
#' @param brood_data Data frame. Output from \code{\link{create_brood_WHZ}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_individual_WHZ <- function(capture_data,
                                  brood_data,
                                  optional_variables) {

  # Add broodID info for chicks
  chicks <- capture_data %>%
    dplyr::filter(.data$age == "chick") %>%
    # Note, that year-box is not unique, as some boxes have had multiple broods in Brood data
    # There is currently no other way, however, to link chicks to the broods they're born in
    # TODO: Check with data owner.
    dplyr::left_join(brood_data %>% dplyr::select(.data$year, .data$box, .data$broodID),
                     by = c("captureYear" = "year", "box"))

  # Create a list of individuals from capture data
  individuals <- capture_data %>%
    dplyr::filter(.data$age != "chick") %>%
    # Add in chicks with brood info
    dplyr::bind_rows(chicks) %>%
    # Arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    # For every individual...
    dplyr::group_by(.data$individualID) %>%
    # ... determine first stage, brood, ring date of each individual
    dplyr::summarise(ringStage = dplyr::first(.data$age),
                     ringDate = dplyr::first(.data$captureDate),
                     firstBrood = dplyr::first(.data$broodID),
                     speciesID = dplyr::first(.data$speciesID)) %>%
    dplyr::mutate(ringYear = as.integer(lubridate::year(.data$ringDate)),
                  ringMonth = as.integer(lubridate::month(.data$ringDate)),
                  ringDay = as.integer(lubridate::day(.data$ringDate)),
                  # Only assign brood ID if individual was caught as a chick
                  broodIDLaid = dplyr::case_when(.data$ringStage != "chick" ~ NA_character_,
                                                 TRUE ~ .data$firstBrood),
                  # There is no information on cross-fostering, so we assume that the brood laid and fledged are the same
                  broodIDFledged =.data$broodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ringSiteID = "WHZ",
                  siteID = "WHZ")

  # Add optional variables
  output <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = capture_data) else .}

  return(output)

}


#' Create location data table for Westerholz, Germany.
#'
#' Create location data table in standard format for data from Westerholz, Germany.
#'
#' @param connection Connection the SQL database.
#'
#' @return A data frame.
#'

create_location_WHZ <- function(connection) {

  # Convert location data to sf
  coordinates <- dplyr::tbl(connection, "BOX_geoCoordinates") %>%
    # Force computation of the database query to run the remaining code
    dplyr::collect() %>%
    sf::st_as_sf(coords = c("x", "y"),
                 # Assign coordinate reference system according to data owner
                 crs = sf::st_crs("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs")) %>%
    # Transform conference reference system to longitudes/latitudes
    sf::st_transform(sf::st_crs("+proj=longlat"))

  # Create list of nest/capture locations
  locations <- dplyr::tbl(connection, "BOX_geoCoordinates") %>%
    # Force computation of the database query to run the remaining code
    dplyr::collect() %>%
    # LocationID: institutionID_nest box number
    dplyr::mutate(locationID = paste0("WHZ_", .data$box),
                  siteID = "WHZ",
                  # Add converted coordinates
                  decimalLongitude = sf::st_coordinates(coordinates)[,1],
                  decimalLatitude = sf::st_coordinates(coordinates)[,2],
                  # Are captures outside the breeding season also captured on the nest?
                  # TODO: check with data owner
                  locationType = "nest",
                  startYear = 2007L,
                  endYear = NA_integer_,
                  # TODO: habitat is set to 1.4 Forest - Temperate; check with data owner
                  habitatID = "1.4")

  return(locations)

}


#' Create individual data table for Westerholz, Germany.
#'
#' Create individual data table in standard format for data from Westerholz, Germany.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_WHZ}}.
#'
#' @return A data frame.
#'

create_measurement_WHZ <- function(capture_data) {

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we use capture_data as input
  measurements <- capture_data %>%
    dplyr::select(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay,
                  .data$tarsus,
                  .data$mass,
                  .data$total_wing_length,
                  .data$P3) %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = c(.data$tarsus, .data$mass, .data$total_wing_length, .data$P3),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(measurementID = 1:dplyr::n(),
                  measurementSubject = "capture",
                  measurementUnit = dplyr::case_when(.data$measurementType == "mass" ~ "g",
                                                     TRUE ~ "mm"),
                  # TODO: Check with data owner how tarsi are measured
                  measurementMethod = dplyr::case_when(.data$measurementType == "tarsus" ~ "alternative",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType from camel case to lower case & space-separated
                  # (e.g., totalWingLength -> total wing length)
                  measurementType = stringr::str_replace_all(string = .data$measurementType,
                                                             pattern = "\\_",
                                                             replacement = " ")) %>%
    dplyr::arrange(.data$measurementDeterminedYear,
                   .data$measurementDeterminedMonth,
                   .data$measurementDeterminedDay)

  return(measurements)

}


#' Create experiment data table for Westerholz, Germany.
#'
#' Create experiment data table in standard format for data from Westerholz, Germany.
#'
#' @param brood_data Data frame. Output from \code{\link{create_brood_WHZ}}.
#' @param capture_data Data frame. Output from \code{\link{create_capture_WHZ}}.
#'
#' @return A data frame.
#'

create_experiment_WHZ <- function(brood_data,
                                  capture_data) {

  # TODO: Retrieve more experiment information from data owner
  experiments <- brood_data %>%
    dplyr::select(.data$treatmentID,
                  experimentStartYear = .data$year,
                  .data$siteID,
                  experimentID = .data$experimental) %>%
    # Add experiment info from capture data
    dplyr::bind_rows(capture_data %>% dplyr::select(.data$treatmentID,
                                                    experimentStartYear = .data$captureYear,
                                                    siteID = .data$captureSiteID)) %>%
    # Remove unknown treatmentID
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    # Remove duplicates
    dplyr::distinct(.data$treatmentID,
                    .keep_all = TRUE) %>%
    # TODO: Some broods have two experimentIDs (comma-separated), check with data owner
    # When experimentID is either 0 or 1 (i.e., no actual ID) set to NA
    dplyr::mutate(experimentID = dplyr::case_when(.data$experimentID %in% c("0", "1") ~ NA_character_,
                                                  TRUE ~ .data$experimentID))

  return(experiments)

}


#' Clean syntax in SQL files
#'
#' WHZ primary data are stored in a series of SQL files, which contain redundant lines and syntax that is not correctly interpreted when reading into R. To execute the SQL statements, we first clean the SQL syntax.
#'
#' @param path Location of the SQL file.
#'
#' @return A list of characters with one element for each SQl statement.
#'
#' @example
#' \dontrun{
#' clean_query_WHZ("~/BREEDING.sql")
#' }
#'

clean_query_WHZ <- function(path){

  # Read lines from SQL file, and clean SQL query
  queries <- readLines(path) %>%
    # Remove metadata, comments
    stringr::str_remove_all(pattern = "--.*$") %>%
    stringr::str_remove_all(pattern = "/\\*.*?\\*/;") %>%
    stringr::str_remove_all(pattern = "\\\\'") %>%
    stringr::str_remove_all(pattern = "COMMENT.*(?=\\,)") %>%
    stringr::str_remove_all(pattern = "unsigned")

  clean_queries <- purrr::map(.x = queries,
                              .f = ~{

                                # Set primary key
                                dplyr::case_when(stringr::str_detect(string = .x,
                                                                     pattern = "NOT NULL AUTO_INCREMENT") ~ paste0("`", stringr::str_extract(.x, pattern = "(?<=[`]).*(?=[`])"), "`"," INTEGER PRIMARY KEY AUTOINCREMENT,"),
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "NOT NULL DEFAULT 0") ~ paste0("`", stringr::str_extract(.x, pattern = "(?<=[`]).*(?=[`])"), "`", " INTEGER PRIMARY KEY,"),
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "PRIMARY KEY") ~ "",
                                                 # Remove additional keys
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "KEY") ~ "",
                                                 # Remove table lock statements
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "LOCK") ~ "",
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "^\\)") ~ ");",
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "--[^\r\n]*") ~ "",
                                                 stringr::str_detect(string = .x,
                                                                     pattern = "/\\*.*?\\*/") ~ "",
                                                 TRUE ~ .x)

                              }) %>%
    purrr::discard(~ .x == "")

  # Split by statement
  output <- purrr::map(.x = split(x = clean_queries,
                                  f = cumsum(stringr::str_detect(string = clean_queries,
                                                                 pattern = "^[:upper:]+"))),
                       .f = ~{

                         unlist(.x) %>%
                           stringr::str_c(collapse = " ") %>%
                           # Remove line breaks, tabs, etc.
                           stringr::str_replace_all(pattern = "[\r\n\t\f\v]",
                                                    replacement = " ") %>%
                           # Remove redundant white space
                           stringr::str_replace_all(pattern = " +",
                                                    replacement = " ") %>%
                           # Remove redundant commas
                           stringr::str_replace_all(pattern = ", +(?=\\))",
                                                    replacement = " ")

                       })

  return(output)

}

#----------------------#
# TODO: How to deal with experimental IDs? Is there additional experimental data? What to do with two experiment IDs per brood?
# TODO: Check how to interpret clutch type (secondClutch). What if no first clutch is recorded prior to second clutch in same box?
# TODO: How to interpret captureTime 00:00:00?
# TODO: ChickIDs, what to do with IDs starting with X?
# TODO: Add what age are chicks ringed/measured? What are the chickAges?
# TODO: How to deal with egg samples?
# TODO: How to link chicks to the broods they're born in?
# TODO: Verify start year of locations
# TODO: Verify habitatID
# TODO: Verify locations of captures, especially when outside breeding season
# TODO: Check measurement units, tarsus method, other measures?
