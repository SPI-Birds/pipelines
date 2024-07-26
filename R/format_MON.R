#'Construct standard format for data from Montpellier, France.
#'
#'A pipeline to produce the standard format for the hole nesting bird
#'populations in Southern France (Rouviere, Montpellier, Corsica etc.),
#'administered by CNRS Montpellier (Anne Charmantier and colleagues).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{here}.
#'
#'\strong{individualID:} In Capture_data, any individuals given unidentified numbers
#'(i.e. containing 'no-ident') are given an individualID = NA. These records are still kept
#'as they contain information about broods but are not
#'integrated into the individual data table.
#'
#'\strong{observedSex:} We ignore uncertainty in sex (e.g. category 4 = Male probable
#'but not certain is just treated as M) for adults. For chicks which are more difficult to assess, we discarded information on sex.
#'
#'\strong{locationID:} For birds caught in boxes. Location is
#'Plot_NestboxNumber_NB. For birds caught around nest boxes (in mist net), locationID is Plot_NestboxNumber_MN
#'
#'\strong{broodID:} Broods are Year_locationID_clutchNumber =
#'Year_Plot_NestboxNumber_clutchNumber.
#'
#'\strong{measurementType:tarsus:} Left and right tarsus are measured. Right tarsus is
#'used where available. Left tarsus measures are only used where right
#'is missing. All tarsus is measured with Svensson's Alternative method;
#'however, be aware that there was a change in the technique used in 1989.
#'Pre-1989, tarsus was measured using a compass and ruler. 1989+ used
#'standard callipers. This can affect tarsus measurements and should be
#'accounted for in any analysis.
#'
#'\strong{measurementType:Culmen:} Beak length measured from skull base to the tip of the beak. This measure was stopped in 2014.
#'
#'\strong{measurementType:BeakLength} Beak length measured from nostril to the tip of the beak. This measure was stopped in 2023.
#'
#'\strong{Age:}  We translate observed age codes when first tagged into life stage (chick, subadult, adult) as follows:
#'\itemize{
#'
#'\item P0 (Poussin bagué au nichoir ou en cavité naturelle et recapturé en dehors du nid la même année/chick banded in box
#'or natural cavity then recaptured outside of their nest the same year): We give life stage code "subadult": An individual
#'was first tagged as a chick. The year of hatching in known with certainty. Any individual in the chick capture table is given life stage "chick".
#'
#'\item P1 (where N = 1) Individuals caught first as chick and caught again 1 season later
#'"subadult" for Age, and minimum/exactAge calculated from Age when first caught and first year of capture
#'
#'\item PN (where N > 1) Individuals caught first as chick and caught again N seasons
#'later. "adult" for Age, and minimum/exactAge calculated from Age when first caught and first year of capture
#'
#'\item J0 (Juvénile (oiseau de l’année) capturé et bagué entre le moment où il s’est
#'envolé du nichoir et le 31 décembre) (Juvenile (of this year) captured and tagged between
#'fledging and Dec 31st of that year) We give life-stage "subadult". The year of hatching is estimated (no exactAge) and uncertain (minimumAge = 0)
#'
#'\item J1 (Juvénile (oiseau de l’année dernière) capturé après le 1er janvier
#'et avant la mue post reproduction (juillet-août)) (Juvenile (of last year)
#'capture between Jan 1st and July-Aug) We give life stage "subadult": An individual was first tagged as an individual
#'with subadult (or immature) plumage. The year of hatching is estimated (no exactAge) and uncertain (minimumAge = 1)
#'
#'\item JN (where N > 1) Individuals caught as subadult and caught again N
#'season later. We give "adult" for Age, NA for exactAge, and minimumAge is calculated from Age when first caught and first year of capture.
#'
#'#'\item A1 (Adulte (oiseau né il y a au moins deux ans) An adult not captured as a chick or juvenile.
#'We give life stage "adult": An individual was first tagged as an individual with adult
#'plumage. The year of hatching is estimated and uncertain.
#'
#'\item AN (where N >= 0) An adult not captured as a chick or juvenile. We give
#'EURING code 4 + 2*N (i.e. of unknown age because it was not caught when it
#'could be accurately aged).
#'
#'\item IN (where N > 1) An adult not captured as a chick or juvenile. We give
#'"adult" for Age, NA for exactAge, and minimumAge is calculated from Age when first caught and first year of capture
#'
#'#'\item I1 : Individual tagged with uncertain age or undetermined age (subadult or
#'adult). We give NA for Age, exactAge, and minimumAge
#'
#'\item IN (where N > 1): Individual tagged with uncertain age or undetermined age (subadult or
#'adult). We give "adult" for Age, NA for exactAge, and minimumAge is calculated from Age when first caught and first
#'year of capture}  #to be discussed? }
#'
#'\strong{minimumAge:} We strictly say that we cannot accurately know the age
#'of an individual unless they were first tagged as "chick". Therefore,
#'although birds classified as "subadult" can be aged very accurately, we still treat
#'these as having age uncertain because they weren't caught as chicks. To be discussed
#'
#'\strong{broodIDLaid/Fledged:} BroodIDLaid is the brood in which an individual
#'was listed as a chick in the brood data table. The BroodIDFledged is the same as
#'BroodIDLaid unless origin and/or destination information is listed. In this case,
#'cross-fostering has occurred. BroodIDFledged is then the brood active at the destination
#'nest box at the time of capture. In any cases where multiple
#'broods are associated with an individual we currently select the first record and return a warning. We assume
#'the first nest recorded is more likely to be the 'true' nest until these are
#'corrected.
#'
#'\strong{releasePlotID/siteID:} Individuals transferred to aviaries are given releasePlotID/siteID
#''aviary'.
#'
#'\strong{ExperimentID:} Currently, broods are given an ExperimentID TRUE or FALSE.
#'Experiments will be expanded to include exact experimental details at a later stage.
#'
#'\strong{LocationID:} When individual is captured as chick or in nest the LocationID
#'is Plot_BoxNumber_NB. For non-chick captures, if there is no BoxNumber listed
#'we return LocationID NA. Even if it wasn't a capture in a nestbox, we have no
#'way of defining the location. If a BoxNumber is provided the LocationID is
#'Plot_BoxNumber_X where X is NB for nest box/winter roost captures and MN for
#'mist net or trap cage and caller captures.
#'
#'@inheritParams pipeline_params
#'@param verbose Should messages be printed during the pipeline?
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export

format_MON <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R",
                       verbose = FALSE){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  }

  if(is.null(pop)){

    pop <- c("MUR", "PIR", "ROU", "MON", "MTV", "MIS")

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_MON(db = db,
                                     species_filter = species,
                                     pop_filter = pop,
                                     optional_variables = optional_variables)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_MON(db = db,
                                 species_filter = species,
                                 pop_filter = pop,
                                 optional_variables = optional_variables)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_MON(Capture_data,
                                           Brood_data,
                                           optional_variables = optional_variables,
                                           verbose)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_MON(db = db,
                                       Capture_data,
                                       Brood_data)

  # MEASUREMENT DATA

  message("Compiling measurement data...")

  Measurement_data <- create_measurement_MON(Capture_data)

  # EXPERIMENTAL DATA
  message("Compiling experimental data...")

  Experiment_data <- create_experiment_data_MON(Capture_data,
                                                Brood_data)

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

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Experimental_data, file = paste0(path, "\\Experimental_data_MON.csv"), row.names = FALSE)

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

#' Create capture data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame with capture data

create_capture_MON <- function(db, species_filter, pop_filter, optional_variables){

  Full_capture_data <- readr::read_delim(paste0(db, "/MON_PrimaryData_MORPH.csv"), show_col_types = FALSE) %>%
    #There is a potential issue in excel that numbers are stored as text in the excel sheets.
    #These can easily be coerced back to numerics, but this throws many warnings,
    #which will masks any real problematic coercion issues (e.g. NA introduced by coercion)
    #Therefore, we read everything as text and coerce individually
    dplyr::mutate(dplyr::across(c(4, 8, 17, 38), as.integer)) %>%
    dplyr::mutate(dplyr::across(c(6, 7, 15, 19:25, 27, 28, 36), as.numeric)) %>%
    dplyr::mutate(speciesID = dplyr::case_when(.data$espece == "ble" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .data$espece == "noi" ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .data$espece == "cha" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$espece == "eto" ~ species_codes$speciesID[which(species_codes$speciesCode == 10033)],
                                               .data$espece == "grpj" ~ species_codes$speciesID[which(species_codes$speciesCode == 10034)],
                                               .data$espece == "grpd" ~ species_codes$speciesID[which(species_codes$speciesCode == 10016)],
                                               .data$espece == "hup" ~ species_codes$speciesID[which(species_codes$speciesCode == 10012)],
                                               .data$espece == "sit" ~ species_codes$speciesID[which(species_codes$speciesCode == 10004)],
                                               .data$espece == "moid" ~ species_codes$speciesID[which(species_codes$speciesCode == 10032)],
                                               .data$espece == "moif" ~ species_codes$speciesID[which(species_codes$speciesCode == 10006)],
                                               .data$espece == "non" ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               is.na(.data$espece) ~ NA_character_,
                                               TRUE ~ NA_character_)) %>%
    #Filter by species
    dplyr::filter(.data$speciesID %in% species_filter) %>%
    dplyr::mutate(captureDate = suppressWarnings(as.Date(.data$date_mesure, format = "%d/%m/%Y")),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ as.integer(.data$an),
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = suppressWarnings(format(as.POSIXlt(strptime(.data$heure, "%H:%M", tz = "CET"),
                                                                   format = "%H:%M:%OS", tz = "CET"),
                                                        format = "%H:%M", tz = "CET")), #timezone is set to Paris time zone for summer time (CEST)
                  individualID =  purrr::pmap_chr(.l = list(bague),
                                                  .f = ~{

                                                    if(grepl(pattern = "non-ident", x = ..1)){

                                                      return(NA_character_)

                                                    } else {

                                                      return(..1)

                                                    }

                                                  }),
                  captureTagID = dplyr::case_when(.data$action %in% c("bcj", "nb") ~ NA_character_,
                                                  TRUE ~ .data$individualID),
                  releaseTagID = .data$individualID,
                  Tarsus = purrr::map2_dbl(.x = tarsed, .y = tarseg,
                                           .f = ~{

                                             if(is.na(..1)){

                                               if(!is.na(..2)){

                                                 return(as.numeric(..2))

                                               } else {

                                                 return(NA_real_)

                                               }

                                             } else {

                                               return(as.numeric(..1))

                                             }

                                           }),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$tarsed) ~ "Alternative"),
                  Wing_Length = .data$aile,
                  Beak_Length = .data$becna,
                  Culmen = .data$culmen,
                  Mass = .data$poids,
                  HandlingAgr = .data$agressivite,
                  ObsAgr = .data$obs_agr,
                  Tail_Length = .data$queue,
                  recordedBy = .data$obs,
                  FoundDead = dplyr::case_when(.$etat_sante %in% c("M", "MMAN") ~ TRUE,
                                               .$action == "mor" ~ TRUE),
                  Destination = .data$dest,
                  ActionTaken = dplyr::case_when(.data$action == "bcj" ~ "Ringed",
                                                 .data$action == "mor" ~ "Dead",
                                                 .data$action == "nb" ~ "Not_Ringed",
                                                 .data$action == "ct" ~ "Caught (already ringed)"),
                  observedSex = dplyr::case_when(.data$sex %in% c(1, 4) ~ "M",
                                                 .data$sex %in% c(2, 5) ~ "F",
                                                 .data$sex == 3 ~ "U",
                                                 TRUE ~ NA_character_),
                  geneticSex = dplyr::case_when(.data$sex_g == 1 ~ "M",
                                                .data$sex_g == 2 ~ "F",
                                                TRUE ~ NA_character_),
                  CaptureMethod = dplyr::case_when(.data$fil == "0" ~ "Nest box",
                                                   .data$fil == "1" ~ "Mist net",
                                                   .data$fil == "2" ~ "Trap cage and caller",
                                                   .data$fil == "3" ~ "Winter roost"),
                  Status = dplyr::case_when(.data$etat_sante %in% c("B", "BMAN") ~ "Injured",
                                            .data$etat_sante %in% c("M", "MMAN") ~ "Dead",
                                            .data$etat_sante == "D" ~ "Disappeared",
                                            .data$etat_sante == "E" ~ "Healthy",
                                            .data$etat_sante == "MAL" ~ "Sick"),
                  capturePhysical = dplyr::case_when(stringr::str_detect(.data$commentaire, "(?i)visuel") ~ FALSE,
                                                     TRUE ~ TRUE),
                  captureAlive = dplyr::case_when(.data$etat_sante == "M" & .data$action == "mor" ~ FALSE,
                                                  .data$action == "mor" & is.na(.data$etat_sante) ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$action == "mor" ~ FALSE,
                                                  TRUE ~ TRUE),
                  capturePlotID = .data$lieu,
                  releasePlotID = dplyr::case_when(releaseAlive == FALSE ~ NA_character_,
                                                   TRUE ~ purrr::map2_chr(Destination, lieu, .f = ~{
                                                     if(is.na(..1)){

                                                       return(..2)

                                                     } else {

                                                       if(grepl(x = ..1, pattern = "voli")){

                                                         return("aviary")

                                                       } else {

                                                         return(..1)

                                                       }

                                                     }

                                                   })),
                  captureLocationID = purrr::pmap_chr(.l = list(lieu, nic, CaptureMethod), .f = ~{

                    if(is.na(..2)){

                      return(paste(..1, "MN", sep = "_"))

                    } else if(is.na(..3) || ..3 %in% c("Nest box", "Winter roost")){

                      return(paste(..1, ..2, "NB", sep = "_"))

                    } else if(..3 %in% c("Mist net", "Trap cage and caller")){

                      return(paste(..1, ..2, "MN", sep = "_"))

                    }
                  }),
                  releaseLocationID = dplyr::case_when(is.na(releasePlotID) ~ NA_character_,
                                                       TRUE ~ purrr::pmap_chr(.l = list(Destination, lieu, nic, captureLocationID), .f = ~{

                                                         if(is.na(..1)){

                                                           return(paste(..4))

                                                         } else {

                                                           if(grepl(x = ..1, pattern = "voli")){

                                                             return("aviary")

                                                           } else {

                                                             return(paste(..1, "MN", sep = "_"))

                                                           }

                                                         }

                                                       })),
                  studyID = paste(identify_PopID_MON(capturePlotID), "1", sep = "-"),  #Warning! For birds transferred to aviaries, studyID is changing overtime (for chicks specifically)
                  captureSiteID = identify_PopID_MON(capturePlotID),
                  releaseSiteID = dplyr::case_when(.data$releaseAlive == FALSE ~ NA_character_,
                                                   TRUE ~ identify_PopID_MON(releasePlotID)),
                  Age = dplyr::case_when(.$age %in% c("P0", "P1", "J0", "J1") ~ "subadult", #J0/P0 are considered "subadult", it needs to be integrated in calculated_age
                                         .$age %in% c("I1") ~ NA_character_, #"I1" means age was unknown when tagged (adult or subadult)
                                         TRUE ~ "adult"),
                  experimentID = dplyr::case_when(.data$exp_ad == "FMR" ~ "ad_1",
                                                  .data$exp_ad == "VACCIN" ~ "ad_2",
                                                  .data$exp_ad == "OF" ~ "ad_3",
                                                  .data$exp_ad == "TRANSFERT" ~ "ad_4",
                                                  .data$exp_ad == "COGNITION" ~ "ad_5",
                                                  .data$exp_ad == "OF/COGNITION" ~ "ad_6",
                                                  TRUE ~ NA_character_),
                  experimentType = dplyr::case_when(.data$exp_ad %in% c("FMR", "VACCIN") ~ "Injection",
                                                    .data$exp_ad %in% c("TRANSFERT") ~ "Translocation to aviaries",
                                                    .data$exp_ad %in% c("OF", "COGNITION", "OF/COGNITION") ~ "Behavioural_test",
                                                    TRUE ~ NA_character_),
                  treatmentDetails = dplyr::case_when(.data$exp_ad == "FMR" ~ "Metabolic measurement (doubly labled water)",
                                                      .data$exp_ad == "VACCIN" ~ "Vaccinated",
                                                      .data$exp_ad == "OF" ~ "Openfield",
                                                      .data$exp_ad == "TRANSFERT" ~ "Translocation",
                                                      .data$exp_ad == "COGNITION" ~ "Cognition test",
                                                      .data$exp_ad == "OF/COGNITION" ~ "Openfield & Cognition test",
                                                      TRUE ~ NA_character_),
                  treatmentStage = dplyr::case_when(!is.na(.data$exp_ad) ~ "Adult",
                                                    TRUE ~ NA_character_),
                  treatmentID = dplyr::case_when(!(is.na(.data$exp_ad)) ~ paste(captureYear, captureSiteID, experimentID, sep = "_"),
                                                 TRUE ~ NA_character_)) %>%
    dplyr::filter(.data$captureSiteID %in% pop_filter) %>%
    dplyr::select(individualID, captureTagID, releaseTagID, speciesID, observedSex, captureDate, captureYear, captureMonth, captureDay, captureTime,
                  recordedBy, Age, geneticSex, capturePhysical, captureAlive, releaseAlive, captureLocationID, releaseLocationID, studyID, captureSiteID,
                  releaseSiteID, capturePlotID, releasePlotID, treatmentID, experimentID, experimentType, treatmentDetails, treatmentStage, latitude, longitude, Tarsus,
                  OriginalTarsusMethod, Wing_Length, Beak_Length, Culmen, Mass, HandlingAgr, ObsAgr, Tail_Length)


  #Format the Capture data to match the standard protocol
  #There are multiple populations within this dataset these are
  # Muro (Corsica), including a deciduous and evergreen sub-population
  # Pirio (Corsica), evergreen only
  # Rouviere (mainly deciduous)
  # Montpellier city (urban habitat)
  # Mont Ventoux (only laying date and clutch size recorded)
  # Remnant populations <- these are not one biological population but are grouped together as "MIS" (including "aviaries" for birds
  # transferred to aviaries)


  #Do the same for the chick capture data
  #As above, we read all in as text and then coerce afterwards
  Chick_capture_data <- readr::read_delim(paste0(db, "/MON_PrimaryData_POUS.csv"), show_col_types = FALSE) %>%
    dplyr::mutate(dplyr::across(c(3, 14, 16), as.integer)) %>%
    dplyr::mutate(dplyr::across(c(5, 6, 12, 17, 19:21, 34, 36), as.numeric)) %>%
    dplyr::mutate(speciesID = dplyr::case_when(.data$espece == "ble" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .data$espece == "noi" ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .data$espece == "cha" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$espece == "eto" ~ species_codes$speciesID[which(species_codes$speciesCode == 10033)],
                                               .data$espece == "grpj" ~ species_codes$speciesID[which(species_codes$speciesCode == 10034)],
                                               .data$espece == "grpd" ~ species_codes$speciesID[which(species_codes$speciesCode == 10016)],
                                               .data$espece == "hup" ~ species_codes$speciesID[which(species_codes$speciesCode == 10012)],
                                               .data$espece == "sit" ~ species_codes$speciesID[which(species_codes$speciesCode == 10004)],
                                               .data$espece == "moid" ~ species_codes$speciesID[which(species_codes$speciesCode == 10032)],
                                               .data$espece == "moif" ~ species_codes$speciesID[which(species_codes$speciesCode == 10006)],
                                               .data$espece == "non" ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               is.na(.data$espece) ~ NA_character_,
                                               TRUE ~ NA_character_)) %>%
    #Filter by species
    #Also remove only the pops we know
    dplyr::filter(.data$speciesID %in% species_filter) %>%
    dplyr::mutate(captureDate = suppressWarnings(as.Date(.data$date_mesure, format = "%d/%m/%Y")),
                  captureYear = dplyr::case_when(is.na(.data$captureDate) ~ as.integer(.data$an),
                                                 TRUE ~ as.integer(lubridate::year(.data$captureDate))),
                  captureMonth = as.integer(lubridate::month(.data$captureDate)),
                  captureDay = as.integer(lubridate::day(.data$captureDate)),
                  captureTime = format(as.POSIXlt(strptime(.data$heure, "%H:%M", tz = "CET"),
                                                  format = "%H:%M:%OS", tz = "CET"),
                                       format = "%H:%M", tz = "CET"), #timezone is set to Paris time zone for summer time (CEST)
                  individualID = purrr::pmap_chr(.l = list(bague),
                                                 .f = ~{

                                                   if(grepl(pattern = "non-ident", x = ..1)){

                                                     return(NA_character_)

                                                   } else {

                                                     return(..1)

                                                   }

                                                 }),
                  captureTagID = dplyr::case_when(.data$action %in% c("bcj", "nb") ~ NA_character_,
                                                  TRUE ~ .data$individualID),
                  releaseTagID = .data$individualID,
                  Tarsus = purrr::map2_dbl(.x = tarsed, .y = tarseg,
                                           .f = ~{

                                             if(is.na(..1)){

                                               if(!is.na(..2)){

                                                 return(as.numeric(..2))

                                               } else {

                                                 return(NA_real_)

                                               }

                                             } else {

                                               return(as.numeric(..1))

                                             }

                                           }),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$tarsed) ~ "Alternative"),
                  Wing_Length = .data$aile,
                  Mass = .data$poids,
                  recordedBy = .data$obs,
                  FoundDead = dplyr::case_when(.data$etat_sante %in% c("MMAN", "D") ~ TRUE),
                  chickAge = as.integer(.data$age_plume),
                  Age = "chick",
                  observedSex = NA_character_, #ignore determined sex in chicks because it generates many errors
                  geneticSex = dplyr::case_when(.data$sex_g == 1 ~ "M",
                                                .data$sex_g == 2 ~ "F"),
                  Status = dplyr::case_when(.data$etat_sante == "M" ~ "Dead before fledging",
                                            .data$etat_sante == "D" ~ "Chick disappeared",
                                            .data$etat_sante == "E" ~ "Fledged"),
                  capturePhysical = TRUE, #there should not be any case of observing chicks without handling them
                  captureAlive = dplyr::case_when(.data$action == "mor" ~ FALSE,
                                                  TRUE ~ TRUE),
                  releaseAlive = dplyr::case_when(.data$etat_sante == "MMAN" ~ FALSE,
                                                  .data$action == "mor" ~ FALSE,
                                                  TRUE ~ TRUE),
                  OrigBoxNumber = purrr::map(orig, find_box),
                  DestBoxNumber = purrr::map(dest, find_box),
                  broodIDLaid = purrr::pmap_chr(.l = list(an, lieu, OrigBoxNumber),
                                                .f = ~{

                                                  if(length(..3) == 1 & all(is.na(..3))){

                                                    return(NA_character_)

                                                  } else {

                                                    if(length(..3) > 1){

                                                      return(paste(..1, ..3[1], ..3[2], sep = "_"))

                                                    } else {

                                                      return(paste(..1, ..2, ..3, sep = "_"))

                                                    }

                                                  }

                                                }),
                  broodIDFledged = purrr::pmap_chr(.l = list(an, lieu, DestBoxNumber),
                                                   .f = ~{

                                                     if(length(..3) == 1 & all(is.na(..3))){

                                                       return(NA_character_)

                                                     } else {

                                                       if(length(..3) == 2){

                                                         return(paste(..1, ..3[1], ..3[2], sep = "_"))

                                                       } else {

                                                         return(paste(..1, ..2, ..3, sep = "_"))

                                                       }

                                                     }

                                                   }),
                  capturePlotID = .data$lieu,
                  releasePlotID = dplyr::case_when(.data$releaseAlive == FALSE ~ NA_character_,
                                                   TRUE ~ purrr::map2_chr(.x = DestBoxNumber, .y = capturePlotID, ~{

                                                     if(length(..1) == 2){

                                                       return(..1[1])

                                                     } else {

                                                       return(..2)

                                                     }

                                                   })),
                  LocationID = paste(.data$lieu, .data$nic, "NB", sep = "_"),
                  captureLocationID = paste(capturePlotID, .data$nic, "NB", sep = "_"),
                  releaseLocationID = dplyr::case_when(.data$releaseAlive == FALSE ~ NA_character_,
                                                       TRUE ~ (purrr::pmap_chr(.l = list(lieu, DestBoxNumber, nic),
                                                                               .f = ~{

                                                                                 if(length(..2) == 1 & all(is.na(..2))){

                                                                                   return(paste(..1, ..3, "NB", sep = "_"))

                                                                                 } else {

                                                                                   if(length(..2) > 1){

                                                                                     return(paste(..2[1], ..2[2], "NB", sep = "_"))

                                                                                   } else {

                                                                                     return(paste(..1, ..2, "NB", sep = "_"))

                                                                                   }

                                                                                 }

                                                                               }))),
                  captureSiteID = identify_PopID_MON(capturePlotID),
                  releaseSiteID = dplyr::case_when(.data$releaseAlive == FALSE ~ NA_character_,
                                                   TRUE ~ identify_PopID_MON(releasePlotID)),
                  studyID = paste(identify_PopID_MON(capturePlotID), "1", sep = "-"),  #Warning! For birds displaced to aviaries, studyID is changing overtime (for chicks specifically)
                  ExperimentDescription3 = dplyr::case_when(.data$expou == "1" ~ "Manipulation affect selective value (e.g. cross foster)",
                                                            .data$expou == "0" ~ "No manipulation that affects selective value (e.g. weighing)"), #2 does not exist. Is it "0"?
                  experimentID = dplyr::case_when(.data$exp_p == "alimente" ~ "ch_1",
                                                  .data$exp_p == "poussintron" ~ "ch_2",
                                                  .data$exp_p == "poussintronfc" ~ "ch_3",
                                                  .data$exp_p == "transf" ~ "ch_4",
                                                  .data$exp_p == "fc" ~ "ch_5",
                                                  .data$exp_p == "biopsie" ~ "ch_6",
                                                  .data$exp_p == "fmr" ~ "ch_7",
                                                  .data$exp_p == "voliere" ~ "ch_8",
                                                  .data$exp_p == "gavage" ~ "ch_9",
                                                  .data$exp_p == "insuline" ~ "ch_10",
                                                  .data$exp_p == "antibio" ~ "ch_11",
                                                  is.na(.data$exp_p) & .data$expou == 1 ~ "ch_12",
                                                  TRUE ~ NA_character_),
                  experimentType = dplyr::case_when(.data$exp_p %in% c("alimente", "gavage", "antibio") ~ "via_diet",
                                                    .data$exp_p %in% c("biopsie") ~ "Surgery",
                                                    .data$exp_p %in% c("insuline", "fmr") ~ "via_injection",
                                                    .data$exp_p %in% c("transf", "voliere") ~ "Translocation",
                                                    .data$exp_p %in% c("fc") ~ "Measurement",
                                                    .data$exp_p %in% c("poussintron", "poussintronfc") ~ "Behavioural_test",
                                                    is.na(.data$exp_p) & .data$expou == 1 ~ "undescribed_exp",
                                                    TRUE ~ NA_character_),
                  treatmentDetails = dplyr::case_when(.data$exp_p == "alimente" ~ "Supplemental feeding",
                                                      .data$exp_p == "poussintron" ~ "Behavioural test",
                                                      .data$exp_p == "poussintronfc" ~ "Behavioural test and heart rate",
                                                      .data$exp_p == "transf" ~ "Translocation",
                                                      .data$exp_p == "fc" ~ "Heart rate monitored",
                                                      .data$exp_p == "biopsie" ~ "Underwent biopsy",
                                                      .data$exp_p == "fmr" ~ "Metabolic measurement (doubly labled water)",
                                                      .data$exp_p == "voliere" ~ "Bred in aviary",
                                                      .data$exp_p == "gavage" ~ "Chick force-feeding supplement",
                                                      .data$exp_p == "insuline" ~ "Insulin injection",
                                                      .data$exp_p == "antibio" ~ "Treated with antibiotics in food supplement",
                                                      is.na(.data$exp_p) & .data$expou == 1 ~ "No information provided about experiment - TBD",
                                                      TRUE ~ NA_character_),
                  treatmentStage = dplyr::case_when(!is.na(experimentID) ~ "Nestling",
                                                    TRUE ~ NA_character_),
                  treatmentID = dplyr::case_when(.data$expou == "1" ~ paste(captureYear, captureSiteID, experimentID, sep = "_"),
                                                 TRUE ~ NA_character_)) %>%
    dplyr::filter(.data$captureSiteID %in% pop_filter) %>%
    dplyr::select(individualID, captureTagID, releaseTagID, speciesID, observedSex, captureDate, captureYear, captureMonth, captureDay, captureTime,
                  recordedBy, captureSiteID, releaseSiteID, capturePlotID, releasePlotID, LocationID, captureLocationID, releaseLocationID, capturePhysical,
                  captureAlive, releaseAlive, Age, chickAge, treatmentID, Tarsus, OriginalTarsusMethod, Wing_Length, Mass, geneticSex, broodIDLaid, broodIDFledged,
                  studyID, treatmentID, experimentID, experimentType, treatmentDetails, treatmentStage, longitude, latitude)

  Capture_data <- dplyr::bind_rows(Full_capture_data, Chick_capture_data) %>%
    dplyr::arrange(.data$captureYear, .data$captureMonth, .data$captureDay) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    calc_age(ID = individualID, Age = Age, Date = captureDate, Year = captureYear, protocol_version = "2.0", showpb = TRUE) %>%
    dplyr::select(captureID, individualID, captureTagID, releaseTagID, speciesID, studyID, observedSex, captureDate, captureYear, captureMonth, captureDay, captureTime,
                  recordedBy, captureSiteID, releaseSiteID, capturePlotID, releasePlotID, LocationID, captureLocationID, releaseLocationID, capturePhysical,
                  captureAlive, releaseAlive, chickAge, treatmentID, exactAge, minimumAge, Age, Tarsus, OriginalTarsusMethod, Wing_Length, Beak_Length, Culmen,
                  Mass, HandlingAgr, ObsAgr, Tail_Length, geneticSex, broodIDLaid, broodIDFledged, studyID, experimentID, experimentType, treatmentDetails,
                  treatmentStage, longitude, latitude) %>%
    ##The normal number of characters in an individualID is 7, and could be 7 digitss or a "V" followed by 6 digits.
    ##Any individualID that have fewer than 6 or more than 8 characters, not only digits or V followed by digits are set to NA
    dplyr::mutate(individualID = dplyr::case_when(nchar(.data$individualID) %in% c(6,7,8) & stringr::str_detect(.data$individualID, "^(V|O|[0-9])+[:digit:]+$")  ~ .data$individualID,
                                                  TRUE ~ NA_character_)) %>%
    dplyr::filter_at(dplyr::vars(individualID, speciesID), dplyr::all_vars(!is.na(.)))

  return(Capture_data)

}

#' Create brood data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'  protocol}.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame with Brood data

create_brood_MON <- function(db, species_filter, pop_filter, optional_variables){

  Brood_data <- readr::read_delim(paste0(db, "/MON_PrimaryData_DEMO.csv"), col_types = my_cols(.default = 'c',
                                                                                                 i = c(an, expou, proto, np, grpo, pulecl, pulenv),
                                                                                                 n = c(latitude, longitude)
  )
  ) %>%
    dplyr::mutate_at(.vars = dplyr::vars(21:36), as.character) %>% #ensure ring number is read as a character
    dplyr::mutate(speciesID = dplyr::case_when(.data$espece == "ble" ~ species_codes$speciesID[which(species_codes$speciesCode == 10002)],
                                               .data$espece == "noi" ~ species_codes$speciesID[which(species_codes$speciesCode == 10005)],
                                               .data$espece == "cha" ~ species_codes$speciesID[which(species_codes$speciesCode == 10001)],
                                               .data$espece == "eto" ~ species_codes$speciesID[which(species_codes$speciesCode == 10033)],
                                               .data$espece == "grpj" ~ species_codes$speciesID[which(species_codes$speciesCode == 10034)],
                                               .data$espece == "grpd" ~ species_codes$speciesID[which(species_codes$speciesCode == 10016)],
                                               .data$espece == "hup" ~ species_codes$speciesID[which(species_codes$speciesCode == 10012)],
                                               .data$espece == "sit" ~ species_codes$speciesID[which(species_codes$speciesCode == 10004)],
                                               .data$espece == "moid" ~ species_codes$speciesID[which(species_codes$speciesCode == 10032)],
                                               .data$espece == "moif" ~ species_codes$speciesID[which(species_codes$speciesCode == 10006)],
                                               .data$espece == "non" ~ species_codes$speciesID[which(species_codes$speciesCode == 10008)],
                                               is.na(.data$espece) ~ NA_character_,
                                               TRUE ~ NA_character_),
                  plotID = .data$lieu,
                  BoxNumber = .data$nic,
                  locationID = paste(.data$plotID, .data$BoxNumber, "NB", sep = "_"),
                  studyID = paste(identify_PopID_MON(plotID), "1", sep = "-"),
                  siteID = identify_PopID_MON(plotID),
                  maleID = dplyr::case_when(.data$mbag %in% c(0, 1) ~ NA_character_,
                                            is.na(.data$mbag) ~ NA_character_,
                                            TRUE ~ .data$mbag),
                  femaleID = dplyr::case_when(.data$fbag %in% c(0, 1) ~ NA_character_,
                                              is.na(.data$fbag) ~ NA_character_,
                                              TRUE ~ .data$fbag),
                  observedClutchType = dplyr::case_when(.data$np == "1" ~ "first",
                                                        .data$np == "2" ~ "second",
                                                        TRUE ~ "replacement"), #there are two cases with np = 4 (second replacement clutch)
                  observedLayDate = suppressWarnings(as.Date(.data$date_ponte, format = "%d/%m/%Y")),
                  observedLayYear = dplyr::case_when(is.na(.data$date_ponte) ~ as.integer(.data$an),
                                                     TRUE ~ as.integer(lubridate::year(.data$observedLayDate))),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedClutchSize = dplyr::case_when(is.na(.data$grpo) ~ NA_integer_,
                                                        TRUE ~ as.integer(.data$grpo)),
                  observedHatchDate = suppressWarnings(as.Date(.data$date_eclo, format = "%d/%m/%Y")),
                  observedHatchYear = dplyr::case_when(is.na(.data$date_eclo) & .data$pulecl == 0 ~ NA_integer_, #if abandoned before hatching
                                                       is.na(.data$date_eclo) & (.data$pulecl != 0 | .data$pulenv !=0) ~ as.integer(.data$observedLayYear), #hatching event happened but unable to estimate hatching date
                                                       TRUE ~ as.integer(lubridate::year(.data$observedHatchDate))),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)),
                  observedBroodSize = dplyr::case_when(is.na(.data$pulecl) ~ NA_integer_,
                                                       TRUE ~ as.integer(.data$pulecl)),
                  observedFledgeYear = dplyr::case_when(!is.na(.data$mort) |  .data$mort == "NCT" | .data$pulenv == 0 ~ NA_integer_, #no information on fledging event
                                                        TRUE ~ as.integer(.data$observedLayYear)), #information is not recorded, but year can at least be estimated
                  observedNumberFledged = dplyr::case_when(is.na(.data$pulenv) ~ NA_integer_,
                                                           TRUE ~ as.integer(.data$pulenv)),
                  ParasiteTreatment = dplyr::case_when(.data$extnt == "T" ~ "Treated against Protocalliphora larvae", #corrected to fit actual values / Should be integrated to Brood_Experiment_Description
                                                       .data$extnt == "NT" ~ "Untreated against Protocalliphora larvae"),
                  ExperimentDescription1 = dplyr::case_when(.data$expou == "0" ~ "No intervention that will affect chick selective value", #corrected to fit actual values
                                                            .data$expou == "1" ~ "Intervention that can affect chick selective value"),
                  ExperimentDescription2 = dplyr::case_when(is.na(.data$experience) ~ NA_character_,
                                                            .data$experience %in% c("femelle BMAN") ~ "acc_ad",
                                                            .data$experience %in% c("-1w BMAN", "-1w cassé femelle", "-9w MMAN") ~ "acc_egg",
                                                            .data$experience %in% c("œuf", "ouef", "prelevé", "reduction_w_volontaire", "MARCEL", "marceL", "common garden", "MARCEL/ UT") ~ "transfert_egg",
                                                            .data$experience %in% c("antibio") ~ "exp_chick",
                                                            .data$experience %in% c("transfert_pouss") ~ "transfert_chick",
                                                            .data$experience %in% c("accident_ad", "AD capt incub", "blesséadulte", "mort adulte", "blessure Adulte") ~ "acc_ad",
                                                            .data$experience %in% c("-2w", "reduction_w_nonvolontaire", "augmentattion_w_nonvolontaire", "augmentation_w_nonvolontaire", "accident_w", "man", "MAN", "-1w") ~ "acc_egg",
                                                            .data$experience %in% c("erreurmanip", "-1p", "tache_cog +1p") ~ "acc_chick",
                                                            .data$experience %in% c("deux espèces") ~ "mixed-species_brood",
                                                            stringr::str_detect(.data$experience, "bouchon") ~ "exp_NB"),
                  ExperimentDescription2 = dplyr::case_when(!is.na(.data$experience) ~ ExperimentDescription2,
                                                            is.na(.data$experience) & .data$explique %in% c("femelle BMAN", "mort en main male", "mort_M", "mort_ M", "mort_F", "blessure_M_10jour de voliere", "blessure_F_pattecassee", "blessure_F_hemorragie") ~ "acc_ad",
                                                            is.na(.data$experience) & .data$explique %in% c("-1w BMAN", "-1w cassé femelle", "1 œuf cassé", "-9w MMAN") ~ "acc_egg",
                                                            is.na(.data$experience) & stringr::str_detect(.data$explique, "(?i)p") & stringr::str_detect(.data$explique, "(?i)w") ~ "transfert_egg&chick",
                                                            is.na(.data$experience) & stringr::str_detect(.data$explique, "(?i)p") ~ "transfert_chick",
                                                            is.na(.data$experience) & stringr::str_detect(.data$explique, "(?i)w") ~ "transfert_egg",
                                                            is.na(.data$experience) & is.na(.data$explique) & .data$expou == 1 ~ "undescribed_exp"),
                  ExperimentDescription3 = dplyr::case_when(.data$extnt == "T" & !is.na(ExperimentDescription2) ~ paste(ExperimentDescription2, "exp_nest", sep = "&"),
                                                            .data$extnt == "T" & is.na(ExperimentDescription2) ~ "exp_nest",
                                                            TRUE ~ ExperimentDescription2),
                  experimentID = dplyr::case_when(ExperimentDescription3 == "acc_ad" ~ "br_1",
                                                  ExperimentDescription3 == "acc_egg" ~ "br_2",
                                                  ExperimentDescription3 == "transfert_egg" ~ "br_3",
                                                  ExperimentDescription3 == "exp_chick" ~ "br_4",
                                                  ExperimentDescription3 == "transfert_chick" ~ "br_5",
                                                  ExperimentDescription3 == "mixed_species_brood" ~ "br_6",
                                                  ExperimentDescription3 == "exp_NB" ~ "br_7",
                                                  ExperimentDescription3 == "transfert_egg&chick" ~ "br_8",
                                                  ExperimentDescription3 == "undescribed_exp" ~ "br_9",
                                                  ExperimentDescription3 == "exp_nest" ~ "br_10",
                                                  ExperimentDescription3 == "undescribed_exp&exp_nest" ~ "br_11",
                                                  ExperimentDescription3 == "transfert_chick&exp_nest" ~ "br_12",
                                                  ExperimentDescription3 == "transfert_egg&chick&exp_nest" ~ "br_13",
                                                  TRUE ~ NA_character_),
                  experimentType = dplyr::case_when(experimentID %in% c("br_1", "br_2", "br_6") ~ "Not_experimental",
                                                    experimentID %in% c("br_3", "br_5", "br_8") ~ "Translocation",
                                                    experimentID %in% c("br_4") ~ "Via_diet",
                                                    experimentID %in% c("br_7")~ "Plugged_nestbox",
                                                    experimentID %in% c("br_10", "br_11") ~ "Nest_treatment",
                                                    experimentID %in% c("br_12", "br_13") ~ "Translocation & Nest treatment",
                                                    experimentID %in% c("br_9") ~ "To_be_determined",
                                                    TRUE ~ NA_character_),
                  treatmentDetails = dplyr::case_when(experimentID == "br_1" ~ "Adult breeder was accidentally injured during manipulation, potentially affecting chick selective value",
                                                      experimentID == "br_2" ~ "One or several eggs in the clutch were accidentally broken during manipulation, potentially affecting chick selective value",
                                                      experimentID == "br_6" ~ "Non-experimental mixed-species brood",
                                                      experimentID == "br_3" ~ "One or more eggs were removed or added to the (not incubated) clutch under experimental process",
                                                      experimentID == "br_5" ~ "One or more chicks were removed or added to the brood under experimental process",
                                                      experimentID == "br_8" ~ "One or more eggs were removed or added to the clutch, then one or more chicks were removed or added to the brood, under experimental process",
                                                      experimentID == "br_4" ~ "each chick of the brood was treated through feeding with antibiotic (or control), under experimental process",
                                                      experimentID == "br_7" ~ "nestbox entry was plugged until mid-april, under experimental process",
                                                      experimentID == "br_10" ~ "Nest treated against Protocalliphora larvae",
                                                      experimentID == "br_9" ~ "Needs investigation to determine the type of experiment (apparently affecting chick selective value) (TBD)",
                                                      experimentID == "br_11" ~ "Nest treated against Protocalliphora larvae + other undescribed experiment (TBD)",
                                                      experimentID == "br_12" ~ "One or more chicks were removed or added to the brood under experimental process + Nest treated against Protocalliphora larvae",
                                                      experimentID == "br_13" ~ "One or more eggs were removed or added to the (non incubated) clutch + One or more chicks were removed or added to the brood + Nest treated against Protocalliphora larvae",
                                                      TRUE ~ NA_character_),
                  treatmentID = dplyr::case_when(!is.na(experimentID) ~ paste(observedLayYear, siteID, experimentID, sep = "_"),
                                                 TRUE ~ NA_character_),
                  treatmentStage = dplyr::case_when(experimentID %in% c("br_1", "br_5", "br_4", "br_10", "br_11", "br_12") ~ "Brood",
                                                    experimentID %in% c("br_3") ~ "Laying",
                                                    experimentID %in% c("br_2") ~ "Clutch",
                                                    experimentID %in% c("br_6") ~ "Clutch & Brood",
                                                    experimentID %in% c("br_7") ~ "Pre-laying",
                                                    experimentID %in% c("br_8", "br_13") ~ "Laying & Brood",
                                                    TRUE ~ NA_character_),
                  NumberParasites = .data$proto,
                  FailureCause = dplyr::case_when(.data$mort == "ABA" ~ "Abandoned",
                                                  .data$mort == "MAN" ~ "Manipulated by researchers",
                                                  .data$mort == "PRE" ~ "Predation",
                                                  .data$mort == "CLI" ~ "Climatic event (e.g. storm)",
                                                  .data$mort == "MAL" ~ "Sickness",
                                                  .data$mort == "NCT" ~ "Fledging event not checked")) %>%
    dplyr::filter(.data$siteID %in% pop_filter) %>%
    dplyr::arrange(.data$observedLayYear, .data$observedLayMonth, .data$observedLayDay) %>%
    dplyr::group_by(.data$observedLayYear, .data$locationID) %>%
    dplyr::mutate(broodID = paste(.data$observedLayYear, .data$locationID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(broodID, speciesID, studyID, siteID, plotID, locationID, femaleID, maleID, observedClutchType, observedLayDate, observedLayYear,
                  observedLayMonth, observedLayDay, observedClutchSize, observedHatchYear, observedHatchMonth, observedHatchDay, observedBroodSize,
                  observedFledgeYear, observedNumberFledged, experimentID, experimentType, treatmentDetails, treatmentID, treatmentStage, pulbag1:pulbag14, BoxNumber) %>%
    ## The normal number of characters in an individualID is 7
    ## Any individualIDs that have more than 8 characters or fewer than 6 characters and are not only numbers (or V+6 digits or O+7 digits) are set to NA
    dplyr::mutate(femaleID = dplyr::case_when(nchar(femaleID) %in% c(6,7,8) & stringr::str_detect(femaleID, "^(V|O|[0-9])+[:digit:]+$")  ~ femaleID,
                                              TRUE ~ NA_character_),
                  maleID = dplyr::case_when(nchar(maleID) %in% c(6,7,8) & stringr::str_detect(maleID, "^(V|O|[0-9])+[:digit:]+$")  ~ maleID,
                                            TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(.data$speciesID)) %>%
    calc_season(data = ., season = .data$observedLayYear) %>%
    calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "2.0") %>%
    calc_nestattempt(data = ., season = .data$breedingSeason)

  return(Brood_data)

}

#' Create individual data table for Montpellier
#'
#' @param Capture_data Capture data generated by \code{\link{create_capture_MON}}.
#' @param Brood_data Capture data generated by \code{\link{create_brood_MON}}.
#' @param verbose When chicks with duplicate nests are found, should a message be printed?
#'
#' @return A data frame with Individual data

create_individual_MON <- function(Capture_data, Brood_data, optional_variables, verbose){

  broodAssignment <- Brood_data %>%
    dplyr::select(broodIDLaid = broodID, pulbag1:pulbag14) %>%
    tidyr::pivot_longer(cols = pulbag1:pulbag14, names_to = "ChickNr", values_to = "individualID") %>%
    dplyr::select(-"ChickNr") %>%
    #Remove broods where there were no ringed chicks
    dplyr::filter(!is.na(.data$individualID))

  #Identify any cases where a individual was cross-fostered
  #It had info in the orig and/or dest column
  cross_foster <- Capture_data %>%
    dplyr::filter(!is.na(.data$broodIDLaid) | !is.na(.data$broodIDFledged))

  #Create progress bar to track cross-foster assignment
  pb <- progress::progress_bar$new(total = nrow(cross_foster))

  cross_foster  <- cross_foster %>%
    dplyr::select(individualID, captureLocationID, captureDate, broodIDLaid, broodIDFledged) %>%
    #Run through each example and determine the destination
    #There are three possible situations
    #1. Individual caught in the genetic brood where the destionation brood is listed
    #2. Individual caught in the foster brood where the origin brood is listed
    #3. Individual with both origin and desination brood listed
    #Use apply because it doesn't coerce dates to numbers!!!!!
    apply(1, function(x, Brood_data){

      pb$tick()

      #If the destination brood has been given
      if(!is.na(x["broodIDFledged"])){

        split_info <- unlist(stringr::str_split(x["broodIDFledged"], pattern = "_"))
        captureDate <- as.Date(x["captureDate"])

        #If it has been transferred to an aviary, list this
        if (stringr::str_detect(string = split_info[1], pattern = "voli")) {

          return(tibble::tibble(individualID = x["individualID"], broodIDFledged = "aviary"))

        }

        #Find the nest that was active in the period of capture
        possible_nest <- Brood_data %>%
          dplyr::mutate(observedLayDate = lubridate::make_date(.data$observedLayYear,
                                                               .data$observedLayMonth,
                                                               .data$observedLayDay)) %>%
          dplyr::filter(.data$breedingSeason == split_info[1],
                        .data$plotID == split_info[2],
                        .data$BoxNumber == split_info[3],
                        .data$observedLayDate < captureDate)

        if(nrow(possible_nest) == 1){

          return(tibble::tibble(individualID = x["individualID"], broodIDFledged = possible_nest$broodID))

        } else if(nrow(possible_nest) > 1 & length(unique(possible_nest$BoxNumber)) == 1){

          possible_nest <- possible_nest %>%
            dplyr::slice(dplyr::n())

        } else {

          if(verbose){

            print(x["individualID"])
            print(captureDate)
            print(split_info)
            print(possible_nest, width = Inf)

            message("CROSS-FOSTERING DESTINATION RETURNS ERROR (MON DATA). This may be because more than one cross-fostering event
               is listed, or the destination nest doesn't exist. This record is skipped")

          }

          return(NULL)

        }

      } else if(!is.na(x["broodIDLaid"])){

        split_info <- unlist(stringr::str_split(x["captureLocationID"], pattern = "_"))
        captureDate <- as.Date(x["captureDate"])

        #Find the nest that was active in the period of capture
        possible_nest <- Brood_data %>%
          dplyr::mutate(observedLayDate = lubridate::make_date(.data$observedLayYear,
                                                               .data$observedLayMonth,
                                                               .data$observedLayDay)) %>%
          dplyr::filter(.data$breedingSeason == lubridate::year(captureDate),
                        .data$plotID == split_info[1],
                        .data$BoxNumber == split_info[2],
                        .data$observedLayDate < captureDate)

        if(nrow(possible_nest) > 1){

          stop("MORE THAN ONE POSSIBLE NEST IDENTIFIED FOR CROSS-FOSTERING (MON DATA)")

        } else {

          if(verbose){

            print(x["individualID"])
            print(captureDate)
            print(split_info)
            print(possible_nest, width = Inf)

            message("CROSS-FOSTERING DESTINATION RETURNS ERROR (MON DATA). This may be because more than one cross-fostering event
               is listed, or the destination nest doesn't exist. This record is skipped")

          }

          return(NULL)

        }

      }

    }, Brood_data) %>%
    dplyr::bind_rows()

  Individual_data <- Capture_data %>%
    dplyr::arrange(.data$individualID, .data$captureDate) %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(speciesID = purrr::map_chr(.x = list(stats::na.omit(unique(.data$speciesID))),
                                                .f = ~{

                                                  if(length(..1) == 1){

                                                    return(..1)

                                                  } else {

                                                    return("CONFLICTED")

                                                  }

                                                }),
                     studyID = dplyr::first(.data$studyID),
                     siteID = dplyr::first(.data$captureSiteID), #I would remove this as site can change across life (for now, set to be similar to tagSiteID)
                     tagSiteID = dplyr::first(.data$captureSiteID),
                     tagYear =  dplyr::first(.data$captureYear),
                     tagMonth =  dplyr::first(.data$captureMonth),
                     tagDay =  dplyr::first(.data$captureDay),
                     tagStage =  dplyr::first(.data$Age),
                     geneticSex = purrr::pmap_chr(.l = list(list(unique(.data$observedSex)), list(unique(.data$geneticSex))),
                                                  .f = ~{

                                                    #Firstly, check genetic sex results
                                                    #Are there any non-NA cases?
                                                    if(length(stats::na.omit(..2)) > 0){

                                                      #If there's just one sex record return it
                                                      if(length(stats::na.omit(..2)) == 1){

                                                        return(stats::na.omit(..2))

                                                        #Otherwise, check if we can use observed sex instead
                                                      } else {

                                                        #If there is one record of observed sex use this
                                                        if(length(stats::na.omit(..1)) == 1){

                                                          return(stats::na.omit(..1))

                                                          #Otherwise, we need to say the sex is conflicted
                                                        } else {

                                                          return("C")

                                                        }

                                                      }

                                                      #If there is no genetic sex
                                                    } else {

                                                      return(NA_character_)

                                                    }

                                                  }

                     ))


  #Join in Brood data for all individuals
  Individual_data_single_records <- dplyr::left_join(Individual_data, broodAssignment, by = "individualID") %>%
    #Join in cross-fostering info
    dplyr::left_join(cross_foster %>%
                       dplyr::select("individualID", "broodIDFledged"),
                     by = "individualID") %>%
    #If no BroodIDFledged is listed, make it the same as BroodIDLaid
    dplyr::mutate(broodIDFledged = dplyr::case_when(is.na(.data$broodIDFledged) ~ .data$broodIDLaid,
                                                    TRUE ~ .data$broodIDFledged)) %>%
    #Remove duplicates that will occur due to multiple values being in dest/orig
    dplyr::distinct() %>%
    dplyr::select(individualID, speciesID, studyID, siteID, tagSiteID, tagYear, tagMonth, tagDay, tagStage, geneticSex, broodIDLaid, broodIDFledged)

  #Identify those cases where an individual has multiple records in individual data
  duplicates <- Individual_data_single_records %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::pull(individualID)

  #If we are showing warnings, return this warning message
  ## TODO: Maybe this should be removed so it can be picked up by quality checks
  if (verbose) {

    purrr::pwalk(.l = list(duplicates),
                 .f = ~{

                   message(paste0("Individual ", ..1, " has more than one potential BroodID"))

                 })

  }

  #For any duplicate cases, we just take the first, which should be the most recent one
  Individual_data <- Individual_data_single_records %>%
    dplyr::group_by(.data$individualID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    calc_sex(individual_data = ., capture_data = Capture_data)

  return(Individual_data)

}


#' Create location data table for Montpellier
#'
#' @param Capture_data Capture data generated by \code{\link{create_capture_MON}}.
#' @param db Location of primary data from Montpellier.
#' @param Brood_data Capture data generated by \code{\link{create_brood_MON}}.
#'
#' @return A data frame with Individual data

create_location_MON <- function(db, Capture_data, Brood_data){

  #Load lat/long for nest boxes
  nestbox_latlong <- readr::read_delim(paste0(db, "/MON_PrimaryData_NestBoxLocation.csv"), show_col_types = FALSE) %>%
    dplyr::filter(!is.na(.data$latitude)) %>%
    dplyr::mutate(LocationID_join = paste(.data$abr_station, .data$nichoir, sep = "_"),
                  startYear = dplyr::case_when(!is.na(.data$an_installation) ~ as.integer(.data$an_installation),
                                               TRUE ~ NA_integer_),
                  endYear = dplyr::case_when(!is.na(.$an_retrait) ~ as.integer(.data$an_retrait),
                                             TRUE ~ NA_integer_),
                  locationDetails1 = dplyr::case_when(stringr::str_detect(LocationID_join, "_0") ~ "Missing information about nestbox, location corresponds to the centroid of the site",
                                                      TRUE ~ NA_character_),
                  locationDetails = dplyr::case_when(.$comment == "suivi_discontinu" ~ "Occasional monitoring",
                                                     .$comment == "disparu" ~ "Stolen or lost",
                                                     .$comment == "enlevé" ~ "Removed",
                                                     .$comment %in% c("projet_DLI", "ponctuel_experienceSC", "projet_nichoirAC") ~ "Temporarily installed for experimental projects",
                                                     TRUE ~ locationDetails1)) %>%
    dplyr::mutate(dplyr::across(c("latitude":"longitude"), as.numeric)) %>%
    dplyr::select(LocationID_join, latitude, longitude, startYear, endYear, locationDetails)


  #There are some nestboxes outside the study area
  nestbox_latlong_outside <- readr::read_delim(paste0(db, "//MON_PrimaryData_OffSiteLocation.csv"), show_col_types = FALSE) %>%
    dplyr::filter(!is.na(.data$la)) %>%
    dplyr::mutate(LocationID_join = paste(.data$st, .data$ni_localisation, sep = "_"),
                  latitude = .data$la,
                  longitude = .data$lo,
                  startYear = dplyr::case_when(!is.na(.data$an_installation) ~ as.integer(.data$an_installation),
                                               TRUE ~ NA_integer_),
                  endYear = dplyr::case_when(!is.na(.data$an_retrait) ~ as.integer(.data$an_retrait),
                                             TRUE ~ NA_integer_),
                  locationDetails = dplyr::case_when(.data$comment == "Projet_antibio_HD" ~ "Temporarily installed for experimental projects",
                                                     TRUE ~ NA_character_)) %>%
    dplyr::mutate(dplyr::across(c("latitude":"longitude"), as.numeric)) %>%
    #There are some replicate groups, compress them to one record per location
    dplyr::group_by(.data$LocationID_join) %>%
    dplyr::slice(1) %>%
    dplyr::select(LocationID_join, latitude, longitude, startYear, endYear, locationDetails)

  all_nestbox_latlong <- dplyr::bind_rows(nestbox_latlong, nestbox_latlong_outside) %>%
    dplyr::distinct()

  #Captures that have a latitude and longitude are mist netting outside of study areas
  #Identify unique locations
  #And give them a unique LocationID (hs_n).
  #Use PopID MIS, because these are captures outside
  #of any main study area.

  #If we don't include "MIS" as a population, we will never have these. So we need to check that outside locations exist
  #Otherwise we will get errors trying to manipulate data with no rows
  if(any(!is.na(Capture_data$longitude))){

    outside_locations <- Capture_data %>%
      dplyr::filter(!is.na(.data$longitude)) %>%
      dplyr::select("captureYear", "captureLocationID", "latitude", "longitude") %>%
      dplyr::filter(!(stringr::str_detect(captureLocationID, "NB"))) %>% #avoids duplicates for nestboxes considered "outside" (hs) but identified and with GPS coordinates (already covered in "all_nestbox_latlong")
      dplyr::group_by(.data$latitude, .data$longitude) %>%
      dplyr::summarise(startYear = as.integer(min(.data$captureYear)),
                       endYear = as.integer(max(.data$captureYear)), #mist net captures are usually occasional
                       studyID = "MIS-1",
                       siteID = "MIS",
                       locationType = "capture",
                       locationDetails = NA_character_,
                       habitatID = NA_character_,
                       elevation = NA_real_) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(locationID = paste("hs", 1:dplyr::n(), "MN", sep = "_")) %>%
      dplyr::select("locationID", "locationType", "locationDetails", "studyID", "siteID", "decimalLatitude" = "latitude",
                    "decimalLongitude" = "longitude", "elevation", "startYear", "endYear", "habitatID")

  } else {

    outside_locations <- NULL

  }

  #For cases where no lat/long are available
  #We need to link the lat/long from separate file (nestbox_latlong above)
  inside_locations <- Capture_data %>%
    dplyr::filter(is.na(.data$longitude) & !is.na(.data$captureLocationID)) %>%
    dplyr::mutate(LocationID_join = paste(stringr::str_split_i(.data$captureLocationID, pattern = "_", i = 1),
                                          stringr::str_split_i(.data$captureLocationID, pattern = "_", i = 2),
                                          sep = "_")) %>%
    dplyr::select("year" = "captureYear", "locationID" = "captureLocationID", "LocationID_join", "studyID", "siteID" = "captureSiteID")

  #Also for broods only identified in the brood data table
  nest_locations <- Brood_data %>%
    dplyr::mutate(LocationID_join = paste(stringr::str_split_i(.data$locationID, pattern = "_", i = 1),
                                          stringr::str_split_i(.data$locationID, pattern = "_", i = 2),
                                          sep = "_")) %>%
    dplyr::select("year" = "observedLayYear", "locationID", "LocationID_join", "studyID", "siteID")

  #Combine to have all locations without lat/long
  non_latlong_locations <- dplyr::bind_rows(inside_locations, nest_locations) %>%
    dplyr::left_join(all_nestbox_latlong, by = "LocationID_join") %>%
    dplyr::filter(!stringr::str_detect(locationID, "MN")) %>% #delete mistnet captures at nestboxes
    dplyr::distinct(.data$LocationID_join, .keep_all = TRUE) %>% #avoiding duplicates
    dplyr::group_by(.data$locationID) %>%
    dplyr::summarise(locationID = unique(.data$locationID),
                     locationType = "nest",
                     studyID = unique(.data$studyID),
                     siteID = unique(.data$siteID),
                     decimalLatitude = dplyr::first(.data$latitude),
                     decimalLongitude = dplyr::first(.data$longitude),
                     elevation = NA_real_,
                     startYear = unique(.data$startYear),
                     endYear =  unique(.data$endYear),
                     locationDetails = unique(.data$locationDetails),
                     Plot = stringr::str_split(.data$locationID,
                                               pattern = "_",
                                               simplify = TRUE)[1],
                     habitatID = purrr::map_chr(.x = unique(locationID), .f = ~{
                       plot <- stringr::str_split(.data$locationID, pattern = "_", simplify = TRUE)[1]

                       if(plot %in% c("ava", "fel", "mur")) {

                         return("G1.72")

                       } else if(plot %in% c("rou")) {

                         return("G1.7111")

                       } else if(plot %in% c("fil", "ari", "gra", "pir", 'tua')) {

                         return("G2.1215")

                       } else if(plot %in% c("ven")) {

                         return("G3.323")

                       } else if(plot %in% c("bot")) {

                         return("I2.12")

                       } else if(plot %in% c("cef", "fac")) {

                         return("J1.2")

                       } else if(plot %in% c("font", "mas", "mos")) {

                         return("J1.1")

                       } else if(plot %in% c("gram")) {

                         return("I2.23")

                       } else if(plot %in% c("zoo")) {

                         return("I2.11")

                       } else {

                         return(NA_character_)

                       }

                     }))

  Location_data <- dplyr::bind_rows(outside_locations, non_latlong_locations)

  return(Location_data)

}

#' Create measurement data table for Montpellier
#'
#' @param Capture_data Capture data generated by \code{\link{create_capture_MON}}.
#' @return A data frame with measurement data

create_measurement_MON <- function(Capture_data) {

  # Measurements are only taken of individuals (during captures), not of locations,
  # so we use Capture_data as input
  Measurement_data <- Capture_data %>%
    dplyr::select(recordID = "captureID",
                  "studyID",
                  siteID = "captureSiteID",
                  measurementDeterminedYear = "captureYear",
                  measurementDeterminedMonth = "captureMonth",
                  measurementDeterminedDay = "captureDay",
                  measurementDeterminedTime = "captureTime",
                  "Tarsus",
                  "Wing_Length",
                  "Beak_Length",
                  "Culmen",
                  "Tail_Length",
                  Handling_Docility = "HandlingAgr",
                  "Mass",
                  "recordedBy",
                  "ObsAgr") %>%
    # Measurements in Capture data are stored as columns, but we want each individual measurement as a row
    # Therefore, we pivot each separate measurement of an individual to a row
    # NAs are removed
    tidyr::pivot_longer(cols = c("Tarsus", "Wing_Length", "Beak_Length", "Tail_Length", "Culmen", "Mass", "Handling_Docility"),
                        names_to = "measurementType",
                        values_to = "measurementValue",
                        values_drop_na = TRUE) %>%
    dplyr::mutate(measurementID = 1:dplyr::n(),
                  measurementSubject = "capture",
                  measurementAccuracy = NA_real_,
                  measurementUnit = dplyr::case_when(.data$measurementType == "Mass" ~ "g",
                                                     .data$measurementType == "Handling_Docility" ~ "no unit",
                                                     TRUE ~ "mm"),
                  measurementMethod = dplyr::case_when(.data$measurementType == "Tarsus" ~ "alternative",
                                                       .data$measurementType == "Wing_Lentgth" ~ "flattened, maximum chord, following ESF guidelines",
                                                       .data$measurementType == "Beak_Length" ~ "beak length from nostril to tip of beak",
                                                       .data$measurementType == "Culmen" ~ "beak length from skull base to tip of beak",
                                                       .data$measurementType == "Handling_Docility" ~ "behavioral score (0 to 3) of docility in hand",
                                                       TRUE ~ NA_character_),
                  measurementType = replace(.data$measurementType, .data$measurementType == "Culmen", "Beak_Length"), #rename "culmen" to state clearly this is a way of measuring beak length
                  recordedBy = dplyr::case_when(.data$measurementType == "Handling_Docility" ~ .data$ObsAgr,
                                                TRUE ~ .data$recordedBy),
                  # Convert measurementType to lower case & space-separated
                  # (e.g., wingLength -> wing length)
                  measurementType = tolower(gsub("([[:upper:]])", "\\1", .data$measurementType)),
                  measurementType = stringr::str_replace_all(string = .data$measurementType,
                                                             pattern = "\\_",
                                                             replacement = " ")) %>%
    dplyr::select(-"ObsAgr") %>%
    dplyr::arrange(.data$measurementDeterminedYear,
                   .data$measurementDeterminedMonth,
                   .data$measurementDeterminedDay)

  return(Measurement_data)

}

#Experimental data

#' Create experiment data table for Montpellier, France.
#'
#' Create experiment data table in standard format for data from Montpellier, France.
#'
#' @param Brood_data Data frame. Output from \code{\link{create_brood_MON}}.
#' @param Capture_data Data frame. Output from \code{\link{create_capture_MON}}.
#'
#' @return A data frame.
#'

create_experiment_data_MON <- function(Capture_data, Brood_data){

  # TODO: (as data administrator) when a complete and detailed table of experiments is ready as primary data, adapt script to fit it
  Experiment_data <- Brood_data %>%
    dplyr::select("treatmentID",
                  "experimentID",
                  "studyID",
                  "siteID",
                  "experimentType",
                  treatmentStartYear = "observedLayYear",
                  treatmentEndYear = "observedLayYear",
                  "treatmentDetails",
                  "treatmentStage") %>%
    dplyr::bind_rows(Capture_data %>%
                       dplyr::select("treatmentID",
                                     "experimentID",
                                     "studyID",
                                     siteID = "captureSiteID",
                                     "experimentType",
                                     treatmentStartYear = "captureYear",
                                     treatmentEndYear = "captureYear",
                                     "treatmentDetails",
                                     "treatmentStage")) %>%
    dplyr::mutate(treatmentStartMonth = dplyr::case_when(.data$experimentID == "br_7" ~ 2, #I have started to complete information for recent and easy experiments
                                                         TRUE ~ NA_integer_),
                  treatmentStartDay = dplyr::case_when(.data$experimentID == "br_7" & .data$treatmentStartYear == 2022 ~ 24,
                                                       .data$experimentID == "br_7" & .data$treatmentStartYear == 2023 ~ 22,
                                                       TRUE ~ NA_integer_),
                  treatmentStartTime = NA_character_,
                  treatmentEndMonth = dplyr::case_when(.data$experimentID == "br_7" ~ 4,
                                                       TRUE ~ NA_integer_),
                  treatmentEndDay = dplyr::case_when(.data$experimentID == "br_7" ~ 11,
                                                     TRUE ~ NA_integer_),
                  treatmentEndTime = NA_character_,
                  recordedBy = dplyr::case_when(.data$experimentID == "br_7" ~ "AG",
                                                TRUE ~ NA_character_),
                  treatmentStartDay = as.integer(treatmentStartDay),
                  treatmentStartMonth = as.integer(treatmentStartMonth),
                  treatmentEndDay = as.integer(treatmentEndDay),
                  treatmentEndMonth = as.integer(treatmentEndMonth),
                  reference = NA_character_) %>%
    dplyr::filter(!is.na(.data$treatmentID)) %>%
    # Remove duplicates
    dplyr::distinct(.data$treatmentID,
                    .keep_all = TRUE)


  return(Experiment_data)

}


#'Avoid parsing issue when importing demo.csv file
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

#' Translate plots into corresponding study populations.
#'
#' Plots in MON are translated into one of 7 populations, including
#' 'aviary' (for birds transferred to aviaries) and "MIS" for
#' miscellaneous populations.
#'
#' @param variable Plot name.
#'
#' @return Three letter PopID or 'aviary'
#' @export
#'
#' @examples
#' identify_PopID_MON("ava")
identify_PopID_MON <- function(variable){

  dplyr::case_when(variable %in% c("ava", "fel", "mur", "fil", "ari", "gra") ~ "MUR",
                   variable %in% c("pir", "tua") ~ "PIR",
                   variable == "rou" ~ "ROU",
                   variable %in% c("bot", "cef", "fac", "font",
                                   "gram", "mas", "mos", "val", "zoo") ~ "MON",
                   variable == "ven" ~ "MTV",
                   variable %in% c("hs", "aul", "mes", "aig", "bon", "cap", "crt", "gen", "mal",
                                   "mau", "mrt", "olm", "pac", "pie", "pog", "pon",
                                   "pre", "pue", "sfl", "stb", "tcb", "tcv", "vic", "vol") ~ "MIS",
                   grepl(x = variable, pattern = "voli|aviary") ~ "MIS")

}

#' Cut origin/destination box information into plot/box number
#'
#' In MON primary data, the origin/destination of a cross-fostering or
#' translocation can have different formats:
#'
#' - Box number only. This can be numeric (11) or alpha-numeric (11bis). - Plot
#' and box number. This is alpha numeric (ava13) - No information (NA)
#'
#' We need to identify when the box number starts (the first numeric) and
#' separate plot and box number if both are present.
#'
#' @param string A vector. The origin/destination column.
#' @param position Start position to search along the character string.
#'
#' @return A vector of length 1 (for box number only) or 2 (for plot and box
#'   number respectively).
#' @export
#'
#' @examples
#' #Return box number if only box number is provided
#' find_box("11")
#'
#' #Return separate plot and box number if both provided
#' find_box("ava11")
#'
#' #Still return box number of it is alpha-numeric
#' find_box("ava11bis")
#'
#' #Return NA if no information is provided
#' find_box(NA)
find_box <- function(string, position = 1){

  if(is.na(string) | position == (nchar(string) + 1)){

    return(NA_character_)

  }

  split_string <- strsplit(string, "")[[1]]

  if(is.na(suppressWarnings(as.numeric(split_string[position])))){

    return(find_box(string = string, position = position + 1))

  } else {

    if(position == 1){

      return(paste(split_string[position:nchar(string)], collapse = ""))

    } else {

      return(c(paste(split_string[1:(position-1)], collapse = ""),
               paste(split_string[position:nchar(string)], collapse = "")))

    }

  }

}
