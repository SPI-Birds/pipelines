#'Construct standard format for data from Dlouhá Loučka, Czechia.
#'
#'A pipeline to produce the standard format for the study site at
#'Dlouhá Loučka, Czechia, administered by the Palacky University.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#'\strong{speciesID}: Minority species (i.e., short-toed treecreeper, common redstart)
#'and species with imprecise identifiers (e.g., Parus, FX) are excluded.
#'There is a small number of hybrid flycatcher broods. For parents, we know their exact species (i.e., FICALB or FICHYP).
#'For broods and chicks, we know these are mixed broods, and marked as FICHIB. Check with data owner.
#'Further, in cases where the species of the father is not specified, it is assumed to be equal to the brood/mother.
#'
#'\strong{individualID}: Individual IDs start with 1 or 2 letters, followed by 4 to 8 digits. Check with data owner what the expected format is. Other IDs are removed.
#'
#'\strong{chicks}: Except for individualIDs, little information is known about chicks. No capture/ringing date and/or chick age, which hampers the calculation of an exact age. Also speciesID is not known for mixed/hybrid broods.
#'
#'\strong{captureAlive, releaseAlive}: All individuals are assumed to be captured and released alive.
#'
#'\strong{captureRingNumber}: First captures of all individuals are assumed to be ringing events, and thus captureRingNumber is set to NA.
#'
#'\strong{startYear}: Assume all boxes were placed in the first year of the study.
#'
#'\strong{habitatID}: Assume that habitat type is 1.4: Forest - Temperate. Check with data owner.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export
#'

format_DLO <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- c(species_codes$speciesID, "FICHIB") # Add Ficedula hybrids if no species selection is made

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  # Load data
  all_data <- suppressMessages(readxl::read_excel(paste0(db, "/DLO_PrimaryData.xlsx"),
                                                  guess_max = 4000, na = "NA") %>%
    # Convert all cols to snake_case
    janitor::clean_names() %>%
    # Create IDs
    dplyr::mutate(siteID = "DLO",
                  # Ensure unique plotIDs; add siteID prefix
                  plotID = paste0("DLO_", tolower(.data$site)),
                  # Ensure unique locationIDs; requires plot & nestbox
                  locationID = paste(.data$site, .data$nestbox, sep = "_"),
                  # Ensure unique broodIDs; requires laying date, plot, nestbox
                  # This accounts for multiple clutches in a single nestbox, in a single year
                  broodID = paste(.data$year, lubridate::month(.data$x1_egg_date),
                                  lubridate::day(.data$x1_egg_date), .data$site, .data$nestbox, sep = "_")) %>%
    # TODO: Uncertainty in species identification (e.g., PA?) is ignored; check with data owner
    dplyr::mutate(dplyr::across(.cols = c(.data$species, .data$m_sp),
                                .fns = ~{

                                  dplyr::case_when(.x == "CC" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                                   .x == "PM" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                                   grepl(pattern = "PA", x = .x) ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                                   grepl(pattern = "FA", x = .x) ~ species_codes$speciesID[species_codes$speciesCode == "10007"],
                                                   .x == "PP" ~ species_codes$speciesID[species_codes$speciesCode == "10008"],
                                                   .x == "SE" ~ species_codes$speciesID[species_codes$speciesCode == "10004"],
                                                   .x == "FH" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                                   .x == "PasMo" ~ species_codes$speciesID[species_codes$speciesCode == "10006"],
                                                   # TODO: Some species IDs are excluded--too few observations or unknown species; check with data owner
                                                   .x %in% c("CerBra", "FX", "fx", "Parus", "PhPh") ~ NA_character_,
                                                   is.na(.x) ~ NA_character_)

                                })) %>%
    # Convert dates & times
    dplyr::mutate(dplyr::across(.cols = c(.data$nest_building, .data$x1_egg_date, .data$hatching_date, .data$fledging_date,
                                          .data$date_of_predation_event, .data$date_f, .data$date_m),
                                .fns = ~{

                                  as.Date(.x)

                                }),
                  dplyr::across(.cols = c(.data$time_f, .data$time),
                                .fns = ~{

                                  format(.x, "%H:%M", tz = "UTC")

                                }),
                  # Fix format issues
                  # - Remove spaces in m_weight and set to numeric
                  m_weight = as.numeric(stringr::str_replace_all(.data$m_weight, " ", "")),
                  # - Replace , in m_weight by . and set to numeric
                  f_tarsus = as.numeric(stringr::str_replace_all(.data$f_tarsus, ",", ".")),
                  # - Replace / in m_weight by . and set to numeric
                  m_tarsus = as.numeric(stringr::str_replace_all(.data$m_tarsus, "/", "."))))

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_DLO(data = all_data,
                                 species_filter = species,
                                 optional_variables = optional_variables)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_DLO(data = all_data,
                                     species_filter = species,
                                     optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual data....")

  Individual_data <- create_individual_DLO(capture_data = Capture_data,
                                           species_filter = species,
                                           optional_variables = optional_variables)

  # LOCATION DATA

  message("Compiling location data....")

  Location_data <- create_location_DLO(data = all_data)

  # MEASUREMENT DATA

  message("Compiling measurement data....")

  Measurement_data <- create_measurement_DLO(capture_data = Capture_data)

  # EXPERIMENT DATA

  message("Compiling experiment information...")

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v1.2$Experiment_data[0,]

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Capture_data[1, !(names(data_templates$v1.2$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
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

  Measurement_data <- Measurement_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Measurement_data[1, !(names(data_templates$v1.2$Measurement_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v1.2$Measurement_data))

  # TIME

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


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


#' Create brood data table for Dlouhá Loučka, Czechia.
#'
#' Create brood data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param data Data frame. Primary data from Dlouhá Loučka, Czechia.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_DLO <- function(data,
                             species_filter,
                             optional_variables = NULL) {

  Brood_data <- data %>%
    # Remove columns that do not contain relevant brood info
    dplyr::select(-.data$source:-.data$notes_57) %>%
    # If femaleID & maleID differ from expected format, set to NA
    # and unify ring formats; both lower-case and upper-case letters are used for the same individual
    dplyr::mutate(femaleID = dplyr::case_when(stringr::str_detect(toupper(.data$f_ring), "^[:upper:]{1,2}[:digit:]{4,6}$") ~ toupper(.data$f_ring),
                                              TRUE ~ NA_character_),
                  maleID = dplyr::case_when(stringr::str_detect(toupper(.data$m_ring), "^[:upper:]{1,2}[:digit:]{4,6}$") ~ toupper(.data$m_ring),
                                              TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  femaleID = dplyr::case_when(is.na(.data$femaleID) ~ NA_character_,
                                              TRUE ~ paste0("DLO_", .data$femaleID)),
                  maleID = dplyr::case_when(is.na(.data$maleID) ~ NA_character_,
                                            TRUE ~ paste0("DLO_", .data$maleID)),
                  # Assign speciesID; identify mixed broods
                  speciesID = dplyr::case_when(.data$species == .data$m_sp ~ .data$species,
                                               is.na(.data$m_sp) ~ .data$species,
                                               .data$species == "FICALB" & .data$m_sp == "FICHYP" ~ "FICHIB",
                                               .data$species == "FICHYP" & .data$m_sp == "FICALB" ~ "FICHIB",
                                               TRUE ~ NA_character_),
                  observedLayYear = dplyr::case_when(is.na(.data$x1_egg_date) ~ as.integer(.data$year),
                                                     TRUE ~ as.integer(lubridate::year(.data$x1_egg_date))),
                  observedLayMonth = as.integer(lubridate::month(.data$x1_egg_date)),
                  observedLayDay = as.integer(lubridate::day(.data$x1_egg_date)),
                  # TODO: Clutch size 8+2: 8 PERATE + 2 Ficedula eggs (according to notes); check with data owner
                  observedClutchSize = dplyr::case_when(.data$clutch == "8+2" ~ NA_character_,
                                                        TRUE ~ .data$clutch),
                  observedClutchSize = as.integer(.data$observedClutchSize),
                  # TODO: Uncertainty in nesting attempt (observedClutchType) (e.g., f?) is ignored; check with data owner
                  observedClutchType = dplyr::case_when(.data$nesting_attempt %in% c("f", "F", "f?") ~ "first",
                                                        .data$nesting_attempt %in% c("r", "R") ~ "replacement",
                                                        .data$nesting_attempt %in% c("s", "S") ~ "second",
                                                        .data$nesting_attempt == "?" ~ NA_character_,
                                                        TRUE ~ NA_character_),
                  observedHatchYear = as.integer(lubridate::year(.data$hatching_date)),
                  observedHatchMonth = as.integer(lubridate::month(.data$hatching_date)),
                  observedHatchDay = as.integer(lubridate::day(.data$hatching_date)),
                  # NB: ? in number hatched is set to NA
                  observedBroodSize = as.integer(dplyr::na_if(.data$no_hatched, "?")),
                  observedFledgeYear = as.integer(lubridate::year(.data$fledging_date)),
                  observedFledgeMonth = as.integer(lubridate::month(.data$fledging_date)),
                  observedFledgeDay = as.integer(lubridate::day(.data$fledging_date)),
                  # NB: ? in number fledged is set to NA
                  observedNumberFledged = as.integer(dplyr::na_if(.data$no_fledged, "?"))) %>%
    # Filter species
    dplyr::filter(.data$speciesID %in% {species_filter})

  # Add optional variables
  output <- Brood_data %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.2") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  return(output)

}

#' Create capture data table for Dlouhá Loučka, Czechia.
#'
#' Create capture data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param data Data frame. Primary data from Dlouhá Loučka, Czechia.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_DLO <- function(data,
                               species_filter,
                               optional_variables = NULL) {

  # Retrieve capture information of parents
  parents <- data %>%
    # Pivot information on females and males into rows
    tidyr::pivot_longer(cols = c(.data$f_ring, .data$m_ring),
                        names_to = "sex",
                        values_to = "individualID") %>%
    # If individualID differs from expected format, set to NA
    # and unify ring formats; both lower-case and upper-case letters are used for the same individual
    dplyr::mutate(individualID = dplyr::case_when(stringr::str_detect(toupper(.data$individualID), "^[:upper:]{1,2}[:digit:]{4,6}$") ~ toupper(.data$individualID),
                                              TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(is.na(.data$individualID) ~ NA_character_,
                                              TRUE ~ paste0("DLO_", .data$individualID))) %>%
    # Remove unknown individualIDs
    dplyr::filter(!is.na(.data$individualID)) %>%
    dplyr::mutate(observedSex = dplyr::case_when(grepl(pattern = "^f", x = .data$sex) ~ "F",
                                                 grepl(pattern = "^m", x = .data$sex) ~ "M"),
                  speciesID = dplyr::case_when(.data$observedSex == "F" ~ .data$species,
                                               .data$observedSex == "M" ~ .data$m_sp),
                  captureDate = dplyr::case_when(.data$observedSex == "F" ~ .data$date_f,
                                                 .data$observedSex == "M" ~ .data$date_m),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ as.integer(.data$year),
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = dplyr::case_when(.data$observedSex == "F" ~ .data$time_f,
                                                 .data$observedSex == "M" ~ .data$time),
                  age = dplyr::case_when(.data$observedSex == "F" ~ .data$f_age,
                                         .data$observedSex == "M" ~ .data$m_age),
                  age = dplyr::case_when(.data$age %in% c("j", "J", "j?", "J?") ~ "subadult",
                                         .data$age %in% c("ad", "1K+") ~ "adult"),
                  wingLength = dplyr::case_when(.data$observedSex == "F" ~ .data$f_wing,
                                                .data$observedSex == "M" ~ .data$m_wing),
                  mass = dplyr::case_when(.data$observedSex == "F" ~ .data$f_weight,
                                          .data$observedSex == "M" ~ .data$m_weight),
                  tarsus = dplyr::case_when(.data$observedSex == "F" ~ .data$f_tarsus,
                                            .data$observedSex == "M" ~ .data$m_tarsus),
                  recordedBy = dplyr::case_when(.data$observedSex == "F" ~ .data$measured_f,
                                                .data$observedSex == "M" ~ .data$measured_m),
                  recordedBy = dplyr::case_when(.data$recordedBy == "Fernando" ~ .data$recordedBy,
                                                TRUE ~ toupper(stringr::str_replace(.data$recordedBy, "/", " | "))),
                  chickAge = NA_integer_) %>%
    dplyr::select(.data$siteID,
                  .data$plotID,
                  .data$broodID,
                  .data$individualID,
                  .data$speciesID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID,
                  .data$age,
                  .data$chickAge,
                  .data$wingLength,
                  .data$mass,
                  .data$tarsus)

  # Retrieve capture information of chicks
  chicks <- data %>%
    # Remove columns that do not contain relevant chick info
    dplyr::select(-.data$killed_female:-.data$protected, -.data$source:-.data$notes_57) %>%
    # Pivot information on chicks into rows
    tidyr::pivot_longer(cols = .data$x1nestling:.data$x12n,
                        names_to = NULL,
                        values_to = "individualID") %>%
    # Remove white space from individualID
    dplyr::mutate(individualID = stringr::str_replace_all(.data$individualID, " ", ""),
                  # If individualID differs from expected format, set to NA
                  # and unify ring formats; both lower-case and upper-case letters are used for the same individual
                  individualID = dplyr::case_when(stringr::str_detect(toupper(.data$individualID), "^[:upper:]{1,2}[:digit:]{4,8}$") ~ toupper(.data$individualID),
                                                  TRUE ~ NA_character_),
                  # Ensure that individuals are unique: add institutionID as prefix to individualID
                  individualID = dplyr::case_when(is.na(.data$individualID) ~ NA_character_,
                                                  TRUE ~ paste0("DLO_", .data$individualID))) %>%
    # Remove unknown individualIDs
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Assign speciesID; identify mixed broods
    dplyr::mutate(speciesID = dplyr::case_when(.data$species == .data$m_sp ~ .data$species,
                                               is.na(.data$m_sp) ~ .data$species,
                                               .data$species == "FICALB" & .data$m_sp == "FICHYP" ~ "FICHIB",
                                               .data$species == "FICHYP" & .data$m_sp == "FICALB" ~ "FICHIB",
                                               TRUE ~ NA_character_),
                  observedSex = NA_character_,
                  recordedBy = NA_character_,
                  # TODO: Check capture dates of chicks with data owner
                  captureDate = as.Date(NA_character_),
                  captureYear = as.integer(.data$year),
                  captureMonth = NA_integer_,
                  captureDay = NA_integer_,
                  captureTime = NA_character_,
                  age = "chick",
                  wingLength = NA_real_,
                  mass = NA_real_,
                  tarsus = NA_real_,
                  # TODO: Check chick age with data owner
                  chickAge = NA_integer_) %>%
    dplyr::select(.data$siteID,
                  .data$plotID,
                  .data$broodID,
                  .data$individualID,
                  .data$speciesID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID,
                  .data$age,
                  .data$chickAge,
                  .data$wingLength,
                  .data$mass,
                  .data$tarsus)

  # Combine parents & chicks info
  captures <- dplyr::bind_rows(parents, chicks) %>%
    # TODO: Capture & release are assumed to happen at the same plot
    dplyr::mutate(captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  # TODO: Individuals are assumed to be captured alive, without replacing rings
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  capturePhysical = TRUE) %>%
    # Arrange chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay, .data$captureTime) %>%
    dplyr::group_by(.data$individualID) %>%
    # First captures are assumed to be ringing events, and thus captureRingNumber = NA.
    dplyr::mutate(captureRingNumber = dplyr::case_when(dplyr::row_number() == 1 ~ NA_character_,
                                                       TRUE ~ stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))),
                  # All releases are assumed to be alive (also see releaseAlive), so no NAs in releaseRingNumber
                  releaseRingNumber = stringr::str_sub(.data$individualID, 5, nchar(.data$individualID))) %>%
    dplyr::ungroup() %>%
    # Filter species
    dplyr::filter(speciesID %in% {species_filter}) %>%
    # Create captureID
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$captureID, everything())

  # Add optional variables
  output <- captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            Year = .data$captureYear,
                                                                                            protocol_version = "1.2") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}

  return(output)

}


#' Create individual data table for Dlouhá Loučka, Czechia.
#'
#' Create individual data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_DLO}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_individual_DLO <- function(capture_data,
                                  species_filter,
                                  optional_variables = NULL) {

  # Create a list of individuals from capture data
  individuals <- capture_data %>%
    # Make captureDate
    dplyr::mutate(captureDate = lubridate::make_date(.data$captureYear, .data$captureMonth, .data$captureDay)) %>%
    # Arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureDate, .data$captureYear,
                   .data$captureMonth, .data$captureDay, .data$captureTime) %>%
    # For every individual ...
    dplyr::group_by(.data$individualID) %>%
    # ... determine first stage, brood, ring year, month, day, and ring site of each individual
    dplyr::summarise(firstBrood = dplyr::first(.data$broodID),
                     ringStage = dplyr::first(.data$age),
                     ringDate = dplyr::first(.data$captureDate),
                     ringYear = dplyr::first(.data$captureYear),
                     ringSiteID = dplyr::first(.data$siteID),
                     speciesID = dplyr::case_when(length(unique(.data$speciesID)) == 2 ~ "CCCCCC",
                                                  TRUE ~ dplyr::first(.data$speciesID))) %>%
    # Determine stage at ringing as either chick, subadult, or adult.
    dplyr::mutate(ringStage = dplyr::case_when(is.na(.data$ringStage) ~ "adult",
                                               TRUE ~ .data$ringStage),
                  ringYear = dplyr::case_when(is.na(.data$ringDate) ~ as.integer(.data$ringYear),
                                              TRUE ~ as.integer(lubridate::year(.data$ringDate))),
                  ringMonth = as.integer(lubridate::month(.data$ringDate)),
                  ringDay = as.integer(lubridate::day(.data$ringDate)),
                  # Only assign a brood ID if they were first caught as a chick
                  # Otherwise, the broodID will be their first clutch as a parent
                  broodIDLaid = dplyr::case_when(ringStage != "chick" ~ NA_character_,
                                                 TRUE ~ .data$firstBrood),
                  # We have no information on cross-fostering, so we assume the brood laid and ringed are the same
                  broodIDFledged = .data$broodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(siteID = "DLO") %>%
    # Filter species
    dplyr::filter(speciesID %in% {species_filter})

  # Add optional variables
  output <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = capture_data) else .}

}

#' Create location data table for Dlouhá Loučka, Czechia.
#'
#' Create location data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param data Data frame. Primary data from Dlouhá Loučka, Czechia.
#'
#' @return A data frame.
#'

create_location_DLO <- function(data) {

  # There are no coordinates or box type information
  locations <- data %>%
    dplyr::select(.data$siteID, .data$plotID, .data$locationID) %>%
    tidyr::drop_na() %>%
    dplyr::distinct() %>%
    dplyr::mutate(locationType = "nest",
                  decimalLatitude = NA_real_,
                  decimalLongitude = NA_real_,
                  startYear = as.integer(min(data$year)),
                  endYear = NA_integer_,
                  # TODO: habitat is set to 1.4 Forest -- Temperate; check with data owner
                  habitatID = "1.4")

  return(locations)

}

#' Create measurement data table for Dlouhá Loučka, Czechia.
#'
#' Create measurement data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_DLO}}.
#'
#' @return A data frame.
#'

create_measurement_DLO <- function(capture_data) {

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we use capture_data as input
  measurements <- capture_data %>%
    dplyr::select(recordID = .data$captureID,
                  siteID = .data$captureSiteID,
                  measurementDeterminedYear = .data$captureYear,
                  measurementDeterminedMonth = .data$captureMonth,
                  measurementDeterminedDay = .data$captureDay,
                  measurementDeterminedTime = .data$captureTime,
                  .data$recordedBy,
                  .data$mass,
                  .data$tarsus,
                  .data$wingLength) %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement (i.e., mass, tarsus, and wing length) of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = c("mass", "tarsus", "wingLength"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::arrange(.data$measurementDeterminedYear, .data$measurementDeterminedMonth, .data$measurementDeterminedDay) %>%
    dplyr::mutate(measurementID = 1:n(),
                  measurementSubject = "capture",
                  measurementUnit = dplyr::case_when(.data$measurementType == "mass" ~ "g",
                                                     TRUE ~ "mm"),
                  # TODO: Check with data owner how tarsi are measured
                  measurementMethod = dplyr::case_when(.data$measurementType == "tarsus" ~ "alternative",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType from camel case to lower case & space-separated (e.g., wingLength -> wing length)
                  measurementType = tolower(gsub("([[:upper:]])", " \\1", .data$measurementType)))

  return(measurements)

}

#----------------------#
#TODO: Check capture & release details
#TODO: Check with data owner that lower-case and upper-case ring numbers belong to the same individual
#TODO: Check some species IDs with data owner (e.g., Parus)
#TODO: Check nesting attempt uncertainty with data owner (e.g., f?)
#TODO: Check 8+2 clutch size
#TODO: Check additional chick info (e.g., chick age, capture dates)
#TODO: Check individualID variation in length of digits
#TODO: Check habitat type with data owner
#TODO: Check with data owner on how to interpret other measurements (e.g., tail, 3P-8P, patch1, patch2)
#TODO: Check units of measurements
#TODO: Check tarsus method
