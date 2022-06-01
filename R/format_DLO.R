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
#'\strong{captureAlive, releaseAlive}: All individuals are assumed to be captured and released alive.
#'
#'\strong{captureRingNumber}: First captures of all individuals are assumed to be ringing events, and thus captureRingNumber is set to NA.
#'
#'#'@inheritParams pipeline_params
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
  # We read data in as text to prevent coercion issues
  all_data <- readxl::read_excel(paste0(db, "/DLO_PrimaryData.xlsx"), col_types = "text", na = "NA") %>%
    # Convert all cols to snake_case
    janitor::clean_names() %>%
    # Create IDs
    dplyr::mutate(siteID = "DLO",
                  # Ensure unique plotIDs; add siteID prefix
                  plotID = paste0("DLO_", tolower(.data$site)),
                  # Ensure unique locationIDs; requires plot & nestbox
                  locationID = paste(.data$site, .data$nestbox, sep = "_"),
                  # Ensure unique broodIDs; requires year, plot, nestbox, laying_date
                  # This accounts for multiple clutches in a single nestbox, in a single year
                  broodID = paste(.data$year, .data$site, .data$nestbox, .data$x1_egg_date, sep = "_")) %>%
    # TODO: Uncertainty in species identification (e.g., PA?) is ignored; check with data owner
    dplyr::mutate_at(.vars = dplyr::vars(.data$species, .data$m_sp),
                     .funs = ~{

                       dplyr::case_when(. == "CC" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                        . == "PM" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                        grepl(pattern = "PA", x = .) ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                        grepl(pattern = "FA", x = .) ~ species_codes$speciesID[species_codes$speciesCode == "10007"],
                                        . == "PP" ~ species_codes$speciesID[species_codes$speciesCode == "10008"],
                                        . == "SE" ~ species_codes$speciesID[species_codes$speciesCode == "10004"],
                                        . == "FH" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                        . == "PasMo" ~ species_codes$speciesID[species_codes$speciesCode == "10006"],
                                        # TODO: Some species IDs are excluded--too few observations or unknown species; check with data owner
                                        . %in% c("CerBra", "FX", "fx", "Parus", "PhPh") ~ NA_character_,
                                        is.na(.) ~ NA_character_)

                       }) %>%
    # Convert dates & times
    dplyr::rowwise() %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$nest_building, .data$x1_egg_date, .data$hatching_date, .data$fledging_date,
                                         .data$date_of_predation_event, .data$date_f, .data$date_m),
                     .funs = ~{

                       if(is.na(..1)){

                         as.Date(NA)

                       } else {

                         janitor::excel_numeric_to_date(as.numeric(..1))

                       }

                     }) %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$time_f, .data$time),
                     .funs = ~{

                       format(as.POSIXct(Sys.Date() + as.numeric(..1)), "%H:%M:%S", tz = "UTC")

                     })

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_CHO(data = all_data,
                                 species_filter = species,
                                 optional_variables = optional_variables)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Capture_data[1, !(names(data_templates$v1.2$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Brood_data <- Brood_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Individual_data <- Individual_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Individual_data[1, !(names(data_templates$v1.2$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

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
    # Unify ring formats; both lower-case and upper-case letters are used for the same individual
    dplyr::mutate(femaleID = paste0("DLO_", toupper(.data$f_ring)),
                  maleID = paste0("DLO_", toupper(.data$m_ring)),
                  # Assign speciesID; identify mixed broods
                  speciesID = dplyr::case_when(.data$species == .data$m_sp ~ .data$species,
                                               is.na(.data$m_sp) ~ .data$species,
                                               .data$species == "FICALB" & .data$species == "FICHYP" ~ "FICHIB",
                                               .data$species == "FICHYP" & .data$species == "FICALB" ~ "FICHIB",
                                               TRUE ~ NA_character_),
                  observedLayYear = dplyr::case_when(is.na(.data$x1_egg_date) ~ as.integer(.data$year),
                                                     TRUE ~ as.integer(lubridate::year(.data$x1_egg_date))),
                  observedLayMonth = as.integer(lubridate::month(.data$x1_egg_date)),
                  observedLayDay = as.integer(lubridate::day(.data$x1_egg_date)),
                  # TODO: Clutch size 8+2: 8 PERATE + 2 Ficedula eggs (according to notes); check with data owner
                  observedClutchSize = dplyr::case_when(.data$clutch == "8+2" ~ "8",
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
                  observedNumberFledged = as.integer(dplyr::na_if(.data$no_fledged, "?")),
                  row = 1:n()) %>%
    # Filter species
    dplyr::filter(.data$speciesID %in% {{species_filter}})

  # Add optional variables
  output <- Brood_data %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$Year) else .} %>%
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
    # Unify ring formats; both lower-case and upper-case letters are used for the same individual
    dplyr::mutate(femaleID = paste0("DLO_", toupper(.data$f_ring)),
                  maleID = paste0("DLO_", toupper(.data$m_ring))) %>%
    # Pivot information on females and males into rows
    tidyr::pivot_longer(cols = c(.data$femaleID, .data$maleID),
                        names_to = "sex",
                        values_to = "individualID") %>%
    # Remove unknown individualIDs
    dplyr::filter(!is.na("individualID")) %>%
    dplyr::mutate(observedSex = dplyr::case_when(grepl(pattern = "female", x = .data$sex) ~ "F",
                                                 grepl(pattern = "male", x = .data$sex) ~ "M"),
                  speciesID = dplyr::case_when(.data$observedSex == "F" ~ .data$species,
                                               .data$observedSex == "M" ~ .data$m_sp),
                  captureDate = dplyr::case_when(.data$observedSex == "F" ~ as.Date(as.character(.data$date_f)),
                                                 .data$observedSex == "M" ~ as.Date(as.character(.data$date_m))),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ as.integer(.data$year),
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = dplyr::case_when(.data$observedSex == "F" ~ .data$time_f,
                                                 .data$observedSex == "M" ~ .data$time),
                  age = dplyr::case_when(.data$observedSex == "F" ~ .data$f_age,
                                         .data$observedSex == "M" ~ .data$m_age),
                  wingLength = dplyr::case_when(.data$observedSex == "F" ~ .data$f_wing,
                                                .data$observedSex == "M" ~ .data$m_wing),
                  mass = dplyr::case_when(.data$observedSex == "F" ~ .data$f_weight,
                                          .data$observedSex == "M" ~ .data$m_weight),
                  tarsus = dplyr::case_when(.data$observedSex == "F" ~ .data$f_tarsus,
                                            .data$observedSex == "M" ~ .data$m_tarsus),
                  recordedBy = dplyr::case_when(.data$observedSex == "F" ~ .data$measured_f,
                                                .data$observedSex == "M" ~ .data$measured_m),
                  recordedBy = dplyr::case_when(.data$recordedBy == "Fernando" ~ .data$recordedBy,
                                                TRUE ~ toupper(stringr::str_replace(.data$recordedBy, "/", " | ")))) %>%
    dplyr::select(.data$individualID,
                  .data$speciesID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID)

  # Retrieve capture information of chicks
  chicks <- data %>%
    # Remove columns that do not contain relevant chick info
    dplyr::select(-.data$killed_female:-.data$protected, -.data$source:-.data$notes_57) %>%
    # Pivot information on nestlings into rows
    tidyr::pivot_longer(cols = .data$x1nestling:.data$x12n,
                        names_to = NULL,
                        values_to = "individualID") %>%
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Unify ring formats; both lower-case and upper-case letters are used for the same individual
    dplyr::mutate(individualID = paste0("DLO_", toupper(.data$individualID)),
                  # Assign speciesID; identify mixed broods
                  speciesID = dplyr::case_when(.data$species == .data$m_sp ~ .data$species,
                                               is.na(.data$m_sp) ~ .data$species,
                                               .data$species == "FICALB" & .data$species == "FICHYP" ~ "FICHIB",
                                               .data$species == "FICHYP" & .data$species == "FICALB" ~ "FICHIB",
                                               TRUE ~ NA_character_),
                  observedSex = NA_character_,
                  recordedBy = NA_character_,
                  # TODO: Check capture dates of chicks with data owner
                  captureDate = as.Date(NA_character_),
                  captureYear = as.integer(.data$year),
                  captureMonth = NA_integer_,
                  captureDay = NA_integer_,
                  captureTime = NA_character_) %>%
    dplyr::select(.data$individualID,
                  .data$speciesID,
                  .data$observedSex,
                  .data$captureYear,
                  .data$captureMonth,
                  .data$captureDay,
                  .data$captureTime,
                  .data$recordedBy,
                  .data$locationID)

  # Combine parents & chicks info
  captures <- dplyr::bind_rows(parents, chicks) %>%
    # TODO: Capture & release are assumed to happen at the same plot
    dplyr::mutate(captureSiteID = .data$siteID,
                  releaseSiteID = .data$siteID,
                  capturePlotID = .data$plotID,
                  releasePlotID = .data$plotID,
                  # TODO: Individuals are assumed to be captured alive, without replacing rings
                  captureAlive = TRUE,
                  captureRelease = TRUE,
                  capturePhysical = TRUE,
                  # TODO: Check chick age with data owner
                  chickAge = NA_integer_) %>%
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
    dplyr::filter(speciesID %in% {{species_filter}})




}

#----------------------#
#TODO: Check capture & release details
#TODO: Check with data owner that lower-case and upper-case ring numbers belong to the same individual
#TODO: Check some species IDs with data owner (e.g., Parus)
#TODO: Check nesting attempt uncertainty with data owner (e.g., f?)
#TODO: Check 8+2 clutch size
#TODO: Check additional chick info (e.g., chick age, capture dates)
