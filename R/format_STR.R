#' Construct standard format for data from Strasbourg, France
#'
#' A pipeline to produce the standard format for the hole nesting bird populations in North-Eastern France in and around Strasbourg
#' (Strasbourg, Roberstau, Wantzenau) administered by Sylvie Massemin and Josefa Bleu
#' (Institut Pluridisciplinaire Hubert Curien - CNRS UMR 7178 & Université de Strasbourg).
#'
#' This pipeline is built using SPI-Birds' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard format v2.0.0}.
#'
#' This section provides details on data management choices that are unique to these data.
#'
#' TO GET ADAPTED
#'
#' \strong{exactAge, minimumAge}: based on age when tagged. exactAge is calculated for individuals tagged as "PUL". Individuals tagged as "2A" were assigned "subadult"
#' age (their age is known and refers as "as second year of life") and individuals tagged as "+2A" were assigned "adult". This helps calculting minimumAge. All others
#' values ("?", "+2?", "2A?") were set to NAs. Check with data custodian that "?" could never be assigned to an individual tagged before fledging.
#'
#' \strong{capturePhysical}: all individuals are assumed to be physical captures.
#'
#' \strong{treatmentID}: no experiments were conducted, resulting in NAs for treatmentID #to be checked with data custodians
#'
#' \strong{individualID}: individuals are banded with metal ring with 7 digits (adults for both species, chicks before 2022) or a "V" followed by 6 digits (for chicks starting 2022)
#'
#' \strong{brood data}: Empty nestboxes or nestboxes occupied by other species ("MAMM") are removed
#'
#' \strong{observedClutchType}: Only classified as "1" or "2" in the original dataset. Most likely correspond to breeding attempt ("1" or "2") in the associated nestbox
#' (regardless of female ID or first egg laid on the site)
#'
#' \strong{location data}: locationID for capture event (with mist net or clap-net) are assumed to be at the same coordinates for each site (only one ID per site)
#'
#' \strong{habitatID}: roughly defined based on data information "G1" for forest population (WAN), "J1" for downtown population (STR) and "J2" for suburban population (ROB)
#'#'
#' \strong{locationID}: generate a second row for the same nest box when there was a gap in monitoring nest box (e.g. if nest box was monitored since 2014, but not
#' monitored in 2019, the first row indicates startYear as 2014 and endYear as 2018; the second row indicates startYear as 2020 and endYear as NA)
#'
#' \strong{Experiment data}: Experiment data roughly described. Further information needed from data custodian
#'
#' \strong{ExperimentID}: accidental events which may affect breeding attempt are reported and detailed
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format (v2.0.0).
#'
#' @export
#'
format_STR <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)) {

    species <- species_codes$speciesID

  }

  if(is.null(pop)){

    pop <- c("STR", "WAN", "ROB")

  }


  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the developer & user
  start_time <- Sys.time()


  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_STR(db = db,
                                     species_filter = species,
                                     pop_filter = pop,
                                     optional_variables = optional_variables)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_STR(db = db,
                                 species_filter = species,
                                 pop_filter = pop,
                                 optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_STR(Capture_data,
                                           Brood_data,
                                           optional_variables = optional_variables)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_STR(db = db,
                                       Capture_data)

  # MEASUREMENT DATA

  message("Compiling measurement data...")

  Measurement_data <- create_measurement_STR(Capture_data)

  # EXPERIMENTAL DATA
  message("Compiling experimental data...")

  Experiment_data <- create_experiment_STR(Brood_data)

  # WRANGLE DATA FOR EXPORT
  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[1, !(names(data_templates$v2.0$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[1, !(names(data_templates$v2.0$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[1, !(names(data_templates$v2.0$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Location_data <- Location_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Location_data[1, !(names(data_templates$v2.0$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Location_data))

  Measurement_data <- Measurement_data %>%
    # Add row ID
    dplyr::mutate(row = 1:dplyr::n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Measurement_data[1, !(names(data_templates$v2.0$Measurement_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format
    dplyr::select(names(data_templates$v2.0$Measurement_data))

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

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_STR.csv"), row.names = FALSE)

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_STR.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_STR.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_STR.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_STR.csv"), row.names = FALSE)

    utils::write.csv(x = Experimental_data, file = paste0(path, "\\Experimental_data_STR.csv"), row.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Individual_data = Individual_data,
                Brood_data = Brood_data,
                Capture_data = Capture_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data,
                Experiment_data = Experiment_data))

  }

}

#' Create capture data table for Strasbourg, France.
#'
#' Create capture data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param db Location of primary data from Strasbourg.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#'  @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#' @return A data frame.
#'
create_capture_STR <- function(db,
                               species_filter,
                               pop_filter,
                               optional_variables) {

  captures <- readr::read_delim(paste0(db, "/STR_PrimaryData_Capture.csv"), col_types = my_cols(.default = 'c',
                                                                                                n = c(LT, TB, LP, MA))) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Peri-Urbain" ~ "ROB-1",
                                             TRUE ~ "WAN-1")) %>%
    dplyr::mutate(speciesID = dplyr::case_when(.$espece == "PARCAE" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .$espece == "PARMAJ" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               TRUE ~ NA_character_)) %>%
    dplyr::filter(.data$speciesID %in% species_filter) %>%
    dplyr::mutate(captureDate = suppressWarnings(as.Date(.data$date, format = "%d/%m/%Y")),
                  captureYear = as.integer(lubridate::year(.data$captureDate)),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = suppressWarnings(format(as.POSIXlt(strptime(.data$heure, "%H:%M", tz = "CEST"), format = "%H:%M:%OS", tz = "CEST"), format = "%H:%M", tz = "CEST")), #timezone is set to Paris time zone for summer time (CEST)
                  individualID = .data$bague,
                  captureTagID = dplyr::case_when(.data$action == "Baguage" ~ NA_character_,
                                                  TRUE ~ .data$individualID),
                  releaseTagID = .data$individualID,
                  # Reorganize colums related to measurements
                  Tarsus = .data$lt,
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$lt) ~ "Alternative"),
                  Wing_Length = .data$lp,
                  Head_Beak_Length = .data$tb,
                  Fat_Score = .data$ad,
                  Mass = .data$ma,
                  HandlingAgr = .data$agressivite) %>%
    dplyr::group_by(.data$bg) %>%
    # Anonymize observers
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age = dplyr::case_when(.data$age == "PUL" ~ "chick",
                                         .data$age == "2A" ~ "subadult",
                                         .data$age == "+2A" ~ "adult",
                                         TRUE ~ NA_character_),
                  chickAge = NA_integer_, #see with data custodian if they band chick at 15 days old
                  observedSex = dplyr::case_when(.data$sexe == "?" ~ "U",
                                                 is.na(.data$sexe) ~ "U",
                                                 TRUE ~ sexe),
                  geneticSex = NA_character_,
                  capturePhysical = TRUE, #check with data custodian
                  captureAlive = dplyr::case_when(.data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$es == "MORT" ~ FALSE,
                                                  .data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),
                  captureSiteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                                   .data$zone == "Peri-Urbain" ~ "ROB",
                                                   TRUE ~ "WAN"),
                  releaseSiteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                                   .data$zone == "Peri-Urbain" ~ "ROB",
                                                   TRUE ~ "WAN"),
                  capturePlotID = tolower(.data$site),
                  releasePlotID = tolower(.data$site),
                  captureLocationID = dplyr::case_when(.data$type_capture == "AU NID" ~ paste(.data$capturePlotID, tolower(.data$nichoir), "NB", sep = "_"),
                                                       TRUE ~ paste(.data$capturePlotID, "MN", sep = "_")),
                  releaseLocationID = dplyr::case_when(.data$type_capture == "AU NID" ~ paste(.data$capturePlotID, tolower(.data$nichoir), "NB", sep = "_"),
                                                       TRUE ~ paste(.data$capturePlotID, "MN", sep = "_")),
                  treatmentID = NA_character_,  #no experimental treatments recorded there
                  broodID2 = tolower(.data$id_nichee)) %>%  #allow to connect chick to their brood ID
    dplyr::filter(.data$captureSiteID %in% pop_filter) %>%
    # Arrange chronologically per individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$captureMonth, .data$captureDay) %>%
    # Create captureID
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup()  %>%
    dplyr::select(captureID, individualID, captureTagID, releaseTagID, speciesID, studyID, observedSex, captureDate, captureYear, captureMonth, captureDay, captureTime,
                  recordedBy, captureSiteID, releaseSiteID, capturePlotID, releasePlotID, captureLocationID, releaseLocationID, capturePhysical,
                  captureAlive, releaseAlive, chickAge, treatmentID, Age, Tarsus, OriginalTarsusMethod, Wing_Length, Head_Beak_Length, Fat_Score,
                  Mass, HandlingAgr, geneticSex, studyID, broodID2, type_capture) %>%
    # All individuals are banded with metal ring of 7 digits or a V followed by 6 digits
    dplyr::mutate(individualID = dplyr::case_when(nchar(.data$individualID) %in% c(7) & stringr::str_detect(.data$individualID, "^(V|[0-9])+[:digit:]+$")  ~ .data$individualID,
                                                  TRUE ~ NA_character_)) %>%
    # Remove any individual with ring number error
    dplyr::filter_at(dplyr::vars(individualID, speciesID), dplyr::all_vars(!is.na(.)))

  # Add optional variables
  Capture_data <- captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            ID = .data$individualID,
                                                                                            Age = .data$Age,
                                                                                            Date = .data$captureDate,
                                                                                            Year = .data$captureYear,
                                                                                            protocol_version = "2.0") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}

  return(Capture_data)

}


#' Create brood data table for Strasbourg, France.
#'
#' Create brood data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param db Location of primary data from Strasbourg.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#' @return A data frame.
#'

create_brood_STR <- function(db,
                             species_filter,
                             pop_filter,
                             optional_variables) {

  broods <-  readr::read_delim(paste0(db, "/STR_PrimaryData_Brood.csv"), col_types = my_cols(.default = 'c',
                                                                                             i = c(Saison, Ponte, TP, Non_eclos, P_eclos, P_Baguage, P_Envol))) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Peri-urbain" ~ "ROB-1",
                                             TRUE ~ "WAN-1")) %>%
    # Remove rows from empty nest boxes across the season or used by other species than birds (ants, hornets)
    dplyr::filter(!(is.na(.data$espece)  | espece == "MAMM")) %>%
    dplyr::mutate(speciesID = dplyr::case_when(.$espece == "PARCAE" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .$espece == "PARMAJ" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .$espece == "SITEUR" ~ species_codes$speciesID[which(species_codes$speciesCode == 10004)],
                                               .$espece == "PARATE" ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .$espece == "PAPALU" ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               .$espece == "PASDOM" ~ species_codes$speciesID[which(species_codes$speciesCode == 10032)],
                                               TRUE ~ NA_character_)) %>%
    # Filter species
    dplyr::filter(.data$speciesID %in% species_filter) %>%
    dplyr::mutate(plotID = tolower(.data$site),
                  locationID = paste(.data$plotID, tolower(.data$nichoir), "NB", sep = "_"),
                  siteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                            .data$zone == "Peri-urbain" ~ "ROB",
                                            TRUE ~ "WAN"),
                  maleID = dplyr::case_when(is.na(.data$id_male) ~ NA_character_,
                                            TRUE ~ .data$id_male),
                  femaleID = dplyr::case_when(is.na(.data$id_femelle) ~ NA_character_,
                                              TRUE ~ .data$id_femelle),
                  observedClutchType = dplyr::case_when(.data$ponte == "1" ~ "first", #Check with data custodians, I don't think "1" and "2" correspond to Clutch type
                                                        .data$ponte == "2" ~ "second",
                                                        TRUE ~ "replacement"),
                  observedLayDate = suppressWarnings(as.Date(.data$date_ponte, format = "%d/%m/%Y")),
                  observedLayYear = dplyr::case_when(is.na(.data$date_ponte) ~ as.integer(.data$saison),
                                                     TRUE ~ as.integer(lubridate::year(.data$observedLayDate))),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedClutchSize = dplyr::case_when(is.na(.data$tp) ~ NA_integer_,
                                                        TRUE ~ as.integer(.data$tp)),
                  observedHatchDate = suppressWarnings(as.Date(.data$date_eclosion, format = "%d/%m/%Y")),
                  observedHatchYear = dplyr::case_when(.data$statut %in% c("echec_oeuf", "echec_oeufs") ~ NA_integer_, #failed before hatching
                                                       is.na(.data$date_eclosion) & (.data$p_eclos != 0 | .data$p_envol !=0) ~ as.integer(.data$observedLayYear), #hatching event happened but unable to estimate hatching date
                                                       TRUE ~ as.integer(lubridate::year(.data$observedHatchDate))),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedBroodSize = dplyr::case_when(is.na(.data$p_eclos) ~ NA_integer_,
                                                       TRUE ~ as.integer(.data$p_eclos)),
                  observedFledgeDate = suppressWarnings(as.Date(.data$date_envol, format = "%d/%m/%Y")),
                  observedFledgeYear = dplyr::case_when(.data$statut == "echec_poussins" ~ NA_integer_, #failed between hatching and fledging
                                                        TRUE ~ as.integer(lubridate::year(.data$observedFledgeDate))),
                  observedFledgeMonth = as.integer(lubridate::month(.data$observedFledgeDate)),
                  observedFledgeDay = as.integer(lubridate::day(.data$observedFledgeDate)),
                  observedNumberFledged = dplyr::case_when(is.na(.data$p_envol) ~ NA_integer_,
                                                           TRUE ~ as.integer(.data$p_envol)),
                  # Describe reported experiments (for experiment table)
                  experimentID = dplyr::case_when(stringr::str_detect(.data$remarques, "probleme nichoir|probleme_nichoir|mort pdt la capture") ~ "br_1",
                                                  stringr::str_detect(.data$remarques, "Comportement poussins") ~ "br_2",
                                                  stringr::str_detect(.data$remarques, "Manip IMMUNO") ~ "br_3",
                                                  stringr::str_detect(.data$remarques, "Vitamines E") ~ "br_4"),
                  experimentType = dplyr::case_when(experimentID %in% c("br_1") ~ "non_experimental",
                                                    experimentID %in% c("br_2") ~ "behavioural_experiment",
                                                    experimentID %in% c("br_3") ~ "injection",
                                                    experimentID %in% c("br_4") ~ "supplemented_feeding"),
                  treatmentDetails = dplyr::case_when(experimentID == "br_1" ~ "accidental event resulting in failing breeding event",
                                                      experimentID == "br_2" ~ "10 minutes of handling nestling to film their behaviour before fledging event",
                                                      experimentID == "br_3" ~ "immunity challenge experiment on nestling, by injecting lipopolysaccharid before fledging event; nestlings in the brood either received LPS injection or control injection",
                                                      experimentID == "br_4" ~ "nestling food supplemented with vitamin E; nestlings in the brood were either supplemented with vitamin E, or with control food, or not supplemented at all",
                                                      TRUE ~ NA_character_),
                  treatmentID = dplyr::case_when(!is.na(experimentID) ~ paste(.data$observedLayYear, .data$siteID, experimentID, sep = "_"),
                                                 TRUE ~ NA_character_),
                  treatmentStage = NA_character_,
                  # Create a key to connect chick to their brood ID (for individual table)
                  broodID2 = tolower(.data$id_nichee)) %>%
    # Keep only known populations
    dplyr::filter(.data$siteID %in% pop_filter) %>%
    dplyr::arrange(.data$observedLayYear, .data$observedLayMonth, .data$observedLayDay) %>%
    dplyr::group_by(.data$locationID, .data$observedLayYear) %>%
    dplyr::mutate(broodID = paste(.data$observedLayYear, .data$locationID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(broodID, broodID2, speciesID, studyID, siteID, plotID, locationID, femaleID, maleID, observedClutchType, observedLayDate, observedLayYear,
                  observedLayMonth, observedLayDay, observedClutchSize, observedHatchYear, observedHatchMonth, observedHatchDay, observedBroodSize,
                  observedFledgeYear, observedFledgeMonth, observedFledgeDay, observedNumberFledged, experimentID, experimentType, treatmentDetails,
                  treatmentID, treatmentStage) %>%
    ## The normal number of characters in an individualID is 7 (7 digits or V+6 digits)
    ## Any individualIDs that have more than 8 characters or fewer than 6 characters and are not only numbers (or V+6 digits or O+7 digits) are set to NA
    dplyr::mutate(femaleID = dplyr::case_when(nchar(.data$femaleID) %in% c(7) & stringr::str_detect(.data$femaleID, "^(V|O|[0-9])+[:digit:]+$")  ~ .data$femaleID,
                                              TRUE ~ NA_character_),
                  maleID = dplyr::case_when(nchar(.data$maleID) %in% c(7) & stringr::str_detect(.data$maleID, "^(V|O|[0-9])+[:digit:]+$")  ~ .data$maleID,
                                            TRUE ~ NA_character_))

  # Add optional variables
  Brood_data <- broods %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$observedLayYear) else .} %>%
    # calculatedClutchType cannot be provided as number of fledglings are not recorded, so provide full NA column instead
    {if("calculatedClutchType" %in% optional_variables)  calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "2.0") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  return(Brood_data)

}

#' Create individual data table for Strasbourg, France.
#'
#' Create individual data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_STR}}.
#' @param brood_data Data frame. Output from \code{\link{create_brood_STR}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'
create_individual_STR <- function(Capture_data,
                                  Brood_data,
                                  optional_variables) {

  # Relate chicks to their brood ID
  broodAssignment <- Capture_data %>%
    dplyr::filter(is.na(.data$captureTagID) & .data$Age == "chick") %>%
    dplyr::select(individualID, broodID2) %>%
    dplyr::left_join(dplyr::select(Brood_data, broodID, broodID2), by = "broodID2", relationship = "many-to-one")

  individuals <- dplyr::left_join(Capture_data, broodAssignment, by = "individualID", relationship = "many-to-many") %>%
    # Arrange data chronologically for each individual
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    # Indicate any issue related to conflicted species ID
    dplyr::summarise(speciesID = purrr::map_chr(.x = list(stats::na.omit(unique(.data$speciesID))),
                                                .f = ~{

                                                  if(length(..1) == 1){

                                                    return(..1)

                                                  } else {

                                                    return("CONFLICTED")

                                                  }

                                                }),
                     # Determine stage/timing site of first capture for each individual
                     studyID = dplyr::first(.data$studyID),
                     siteID = dplyr::first(.data$captureSiteID),
                     tagSiteID = dplyr::first(.data$captureSiteID),
                     tagYear = dplyr::first(.data$captureYear),
                     tagMonth = dplyr::first(.data$captureMonth),
                     tagDay = dplyr::first(.data$captureDay),
                     tagStage = dplyr::first(.data$Age),
                     broodIDLaid = dplyr::first(.data$broodID),
                     broodIDFledged = dplyr::first(.data$broodID),
                     geneticSex = NA_character_) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(individualID, speciesID, studyID, siteID, tagSiteID, tagYear, tagMonth, tagDay, tagStage, geneticSex, broodIDLaid, broodIDFledged) %>%
    dplyr::group_by(.data$individualID) %>%
    # Select one row for each individual
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Add optional variables
  Individual_data <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = Capture_data) else .}

  return(Individual_data)

}


#' Create location data table for Strasbourg, France.
#'
#' Create location data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param db Location of primary data from Strasbourg.
#' @param Capture_data Data frame. Output from \code{\link{create_capture_STR}}.
#'
#' @return A data frame.
#'
create_location_STR <- function(db,
                                Capture_data) {

  # Create table for nest boxes
  nestbox_loc <- readr::read_delim(paste0(db, "/STR_PrimaryData_Location.csv"), show_col_types = FALSE) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(LocationID_join = paste(tolower(.data$site), tolower(.data$nichoir), "NB", sep = "_")) %>%
    dplyr::mutate(studyID = dplyr::case_when(.data$zone == "CV" ~ "STR-1",
                                             .data$zone == "Peri-urbain" ~ "ROB-1",
                                             TRUE ~ "WAN-1"),
                  siteID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                            .data$zone == "Peri-urbain" ~ "ROB",
                                            TRUE ~ "WAN"),
                  habitatID = dplyr::case_when(.data$zone == "CV" ~ "J1", #Check with data custodians for more details
                                               .data$zone == "Peri-urbain" ~ "J2",
                                               TRUE ~ "G1"),
                  decimalLatitude = .data$latitude,
                  decimalLongitude = .data$longitude,
                  startYear = dplyr::case_when(!is.na(.data$date_pose) ~ as.integer(.data$date_pose),
                                               TRUE ~ NA_integer_),
                  endYear = dplyr::case_when(!is.na(.data$date_retrait) ~ as.integer(.data$date_retrait),
                                             .data$remarques == "Non suivi depuis 2023" ~ 2022, #check with data custodians if this works
                                             TRUE ~ NA_integer_),
                  locationType = "nest",
                  locationDetails = dplyr::case_when(.data$type == "Schwegler" ~ "Schwegler nesting box",
                                                     .data$type == "Bois" ~ "Wooden nesting box",
                                                     .data$type == "Balcon" ~ "Balcony nesting box",
                                                     TRUE ~ NA_character_),
                  elevation = NA_real_,
                  # Add a column to detect rows that need to be duplicated to add information about changing nest box type or monitoring gaps
                  count = dplyr::case_when(stringr::str_detect(.data$remarques, "non suivi 2|bois jusqu|Balcon jusque") ~ 2,
                                           TRUE ~ 1)) %>%
    # Duplicate rows (based on count)
    tidyr::uncount(.data$count) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID_join) %>%
    # ... Add number 2 for duplicated rows (used afterwards to filter the right row)
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Change information on location type when nest box type was changed
    dplyr::mutate(locationDetails = dplyr::case_when(stringr::str_detect(.data$remarques, "bois jusqu'") & count == 1 ~ "Wooden nesting box",
                                                     stringr::str_detect(.data$remarques, "Balcon jusque") & count == 1 ~ "Balcony nesting box",
                                                     TRUE ~ locationDetails),
                  # Adjust startYear and endYear for case with monitoring gaps ("non suivi") or changes in nest box type ("bois jusqu'", "Balcon jusque")
                  endYear = dplyr::case_when(count == 1 & stringr::str_detect(.data$remarques, "non suivi 2020|non suivi 2020, 2021|non suivi 2020,2021|bois jusqu'en 2020") ~ 2019,
                                             count == 1 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021") ~ 2018,
                                             count == 1 & stringr::str_detect(.data$remarques, "non suivi 2021") ~ 2020,
                                             count == 1 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2018,
                                             count == 1 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2022,
                                             TRUE ~ endYear),
                  startYear = dplyr::case_when(count == 2 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021|non suivi 2020, 2021|non suivi 2021") ~ 2022,
                                               count == 2 & stringr::str_detect(.data$remarques, "non suivi 2020") ~ 2021,
                                               count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2020") ~ 2021,
                                               count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2019,
                                               count == 2 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2023,
                                               TRUE ~ startYear)) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID_join) %>%
    # ... create a location ID
    dplyr::mutate(locationID = paste(.data$LocationID_join, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID, decimalLatitude, decimalLongitude, elevation, startYear, endYear, habitatID)

  # Create table for mist net/clap-net captures (information available in capture_data table)
  mn_location <- Capture_data %>%
    # Exclude captures associated with a nest box (individuals captured on nest)
    dplyr::filter(.data$type_capture != "AU NID") %>%
    dplyr::mutate(decimalLatitude = NA_real_, #see data custodian for further details
                  decimalLongitude =  NA_real_,
                  locationType = "capture",
                  locationDetails = dplyr::case_when(.data$type_capture == "FILET VERTICAL" ~ "Mist net",
                                                     .data$type_capture == "CAGE-PIEGE" ~ "Clap-net trap",
                                                     TRUE ~ NA_character_),
                  elevation = NA_real_,
                  habitatID = dplyr::case_when(.data$captureSiteID == "ROB" ~ "J1.1", #Check with data custodians for more details
                                               .data$captureSiteID == "STR" ~ "J1.2",
                                               TRUE ~ "G1"),
                  siteID = .data$captureSiteID) %>%
    # For each location
    dplyr::group_by(.data$captureLocationID, .data$locationDetails) %>%
    # ... Detect first and last year of capture
    dplyr::mutate(startYear = dplyr::first(captureYear),
                  endYear = dplyr::last(captureYear)) %>%
    # Remove duplicate: this is a strong assumption that all captures with net were processed at the same coordinates for each site (to be confirm with data custodian)
    dplyr::slice(1) %>%
    dplyr::mutate(locationID = dplyr::case_when(.data$locationDetails == "Mist net" ~ paste(captureLocationID, 1, sep = "_"),
                                                TRUE ~ paste(captureLocationID, 2, sep = "_"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(locationID, locationType, locationDetails, studyID, siteID, decimalLatitude, decimalLongitude, elevation, startYear, endYear, habitatID)

  # Binding tables (location table for nest boxes and location table for mist nest captures)
  Location_data <- dplyr::bind_rows(nestbox_loc, mn_location) %>%
    dplyr::mutate(startYear = as.integer(.data$startYear),
                  endYear = as.integer(.data$endYear)) %>%
    dplyr::arrange(siteID, startYear)

  return(Location_data)

}


#' Create measurement data table for Strasbourg, France.
#'
#' Create measurement data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_STR}}.
#'
#' @return A data frame.
#'
create_measurement_STR <- function(Capture_data) {

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we use capture_data as input
  Measurement_data <- Capture_data %>%
    dplyr::mutate(Fat_Score = as.integer(.data$Fat_Score),
                  HandlingAgr = as.numeric(.data$HandlingAgr)) %>%
    dplyr::select(recordID = "captureID",
                  "studyID",
                  siteID = "captureSiteID",
                  measurementDeterminedYear = "captureYear",
                  measurementDeterminedMonth = "captureMonth",
                  measurementDeterminedDay = "captureDay",
                  measurementDeterminedTime = "captureTime",
                  "Tarsus",
                  "Wing_Length",
                  "Head_Beak_Length",
                  "Fat_Score",
                  "Handling_Docility" = "HandlingAgr",
                  "Mass",
                  "recordedBy") %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = c("Tarsus", "Wing_Length", "Head_Beak_Length", "Fat_Score", "Mass", "Handling_Docility"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(measurementID = 1:dplyr::n(),
                  measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "Mass" ~ "g",
                                                     .data$measurementType %in% c("Handling_Docility", "Fat_Score") ~ "no unit",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "Tarsus" ~ "alternative",
                                                       .data$measurementType == "Head_Beak_Length" ~ "length from the back of the head to the tip of the beak",
                                                       .data$measurementType == "Wing_Length" ~ "flattened, maximum chord from ESF guidelines",
                                                       .data$measurementType == "Fat_Score" ~ "fat score from ESF guidelines",
                                                       .data$measurementType == "Handling_Docility" ~ "behavioral score (0 to 3) of docility in hand",
                                                       TRUE ~ NA_character_),
                  # Convert measurementType to lower case & space-separated
                  # (e.g., wingLength -> wing length)
                  measurementType = tolower(gsub("([[:upper:]])", "\\1", .data$measurementType)),
                  measurementType = stringr::str_replace_all(string = .data$measurementType,
                                                             pattern = "\\_",
                                                             replacement = " ")) %>%
    dplyr::arrange(.data$measurementDeterminedYear,
                   .data$measurementDeterminedMonth,
                   .data$measurementDeterminedDay)

  return(Measurement_data)

}

#Experimental data

#' Create experimental data table for Strasbourg, France.
#'
#' Create experimental data table in the standard format (v2.0.0) for data from Strasbourg, France.
#'
#' @param Brood_data Data frame. Output from \code{\link{create_brood_STR}}.
#'
#' @return A data frame.
#'
create_experiment_STR <- function(Brood_data) {


  Experiment_data <- Brood_data %>%
    dplyr::select("treatmentID",
                  "experimentID",
                  "studyID",
                  "siteID",
                  "experimentType",
                  "treatmentDetails",
                  treatmentStartYear = "observedLayYear",
                  treatmentEndYear = "observedLayYear",
                  "treatmentStage") %>%
    dplyr::mutate(treatmentStartMonth =  NA_integer_,
                  treatmentStartDay = NA_integer_,
                  treatmentStartTime = NA_character_,
                  treatmentEndMonth = NA_integer_,
                  treatmentEndDay =  NA_integer_,
                  treatmentEndTime = NA_character_,
                  recordedBy =  NA_character_,
                  reference = NA_character_) %>%
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    # Remove duplicates
    dplyr::distinct(.data$treatmentID,
                    .keep_all = TRUE)

  return(Experiment_data)

}

#Function to assign characters to columns when importing data as csv files
#'@param class of variables for importing brood data and avoid logical classes for some columns
#'@return right class

my_cols <- function(..., .default = col_guess()) {
  dots <- dplyr::enexprs(...)
  colargs <- purrr::flatten_chr(unname(
    purrr::imap(dots, ~ {
      colnames <- dplyr::syms(.x)
      colnames <- colnames[colnames != dplyr::sym("c")]
      coltypes <- purrr::rep_along(colnames, .y)
      purrr::set_names(coltypes, colnames)
    })
  ))
  readr::cols(!!!colargs, .default = .default)
}
