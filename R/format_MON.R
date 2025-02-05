#'Construct standard format for data from Montpellier, France.
#'
#'A pipeline to produce the standard format for the hole nesting bird
#'populations in Southern France (Rouviere, Montpellier, Corsica etc.),
#'administered by CNRS Montpellier (Anne Charmantier and colleagues).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{IndvID:} In Capture_data, any individuals given unidentified numbers
#'(i.e. containing 'no-ident') are given an IndvID = NA. These records are still kept
#'as they contain information about broods (e.g. AvgChickMass) but are not
#'integrated into the individual data table.
#'
#'\strong{Sex:} We ignore uncertainty in sex (e.g. category 4 = Male probable
#'but not certain is just treated as M).
#'
#'\strong{LocationID:} For birds caught in boxes. Location is
#'Plot_NextboxNumber.
#'
#'\strong{BroodID:} Broods are BreedingSeason_LocationID_ClutchType_observed =
#'BreedingSeason_Plot_NestboxNumber_ClutchType_observed.
#'
#'\strong{Tarsus:} Left and right tarsus are measured. Right tarsus is
#'used where available. Left tarsus measures are only used where right
#'is missing. All tarsus is measured with Svensson's Alternative method;
#'however, be aware that there was a change in the technique used in 1989.
#'Pre-1989, tarsus was measured using a compass and ruler. 1989+ used
#'standard callipers. This can affect tarsus measurements and should be
#'accounted for in any analysis.
#'
#'\strong{Age:} We translate observed age codes into EURING codes as follows:
#'\itemize{
#'
#'\item P0 (Poussin bagué au nichoir ou en cavité naturelle/chick banded in box
#'or natural cavity): We give EURING code 1: Nestling or chick unable to fly
#'freely. Any individual in the chick capture table is given Age 1.
#'
#'\item J0 (Juvénile (oiseau de l’année) capturé entre le moment où il s’est
#'envolé du nichoir et le 31 décembre) (Juvenile (of this year) captured between
#'fledging and Dec 31st of that year) We give EURING code 3: First-year:
#'full-grown bird hatched in the breeding season of this calendar year.
#'
#'\item PN (where N > 0) Individuals caught first as chick and caught again N seasons
#'later We give EURING code 3 + 2*N (i.e. of known age because it was caught
#'when it could be accurately aged)
#'
#'\item J1 (Juvénile (oiseau de l’année dernière) capturé après le 1er janvier
#'et avant la mue post reproduction (juillet-août)) (Juvenile (of last year)
#'capture between Jan 1st and July-Aug) We give EURING code 5: 2nd year: a bird
#'hatched last calendar year and now in its second calendar year.
#'
#'\item JN (where N > 1) Individuals caught as juvenile and caught again N
#'season later We give EURING code 3 + 2*N, as above.
#'
#'\item AN (where N >= 0) An adult not captured as a chick or juvenile. We give
#'EURING code 4 + 2*N (i.e. of unknown age because it was not caught when it
#'could be accurately aged).
#'
#'\item IN (where N > 1): An adult not captured as a chick or juvenile. We give
#'EURING code 4 + 2*1-N (i.e. of unknown age because it was not caught when it
#'could be accurately aged). }
#'
#'\strong{Age_calculated:} We strictly say that we canot accurately know the age
#'of an individual unless they were caught in their first year. Therefore,
#'although birds classified as J1 can be aged very accurately, we still treat
#'these as having age uncertain because they weren't caught as chicks.
#'
#'\strong{BroodIDLaid/Fledged:} BroodIDLaid is the brood in which an individual
#'was listed as a chick in the brood data table. The BroodIDFledged is the same as
#'BroodIDLaid unless origin and/or destination information is listed. In this case,
#'cross-fostering has occurred. BroodIDFledged is then the brood active at the destination
#'nest box at the time of capture. In any cases where multiple
#'broods are associated with an individual we currently select the first record and return a warning. We assume
#'the first nest recorded is more likely to be the 'true' nest until these are
#'corrected.
#'
#'\strong{ReleasePlot/PopID:} Individuals transferred to aviaries are given ReleasePlot/PopID
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
#'@return 4 data tables in the standard format (version 1.0.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_MON <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R",
                       verbose = FALSE){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.0.0"

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  if(is.null(pop)){

    pop <- c("MUR", "PIR", "ROU", "MON", "MTV", "MIS")

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_MON(db = db, species_filter = species, pop_filter = pop)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_MON(db = db, species_filter = species, pop_filter = pop)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_MON(Capture_data, Brood_data, verbose)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_MON(db = db, Capture_data, Brood_data)

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>%
    dplyr::select("IndvID":"ChickAge")

  #Remove chick rings from brood data
  #Add NAs for Avg measurements until we add them later
  Brood_data <- Brood_data %>%
    dplyr::select(-"pulbag1":-"pulbag14") %>%
    dplyr::mutate(AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_,
                  NumberChicksTarsus = NA_integer_) %>%
    dplyr::select("BroodID":"NumberFledgedError", "AvgEggMass":"NumberChicksTarsus", "ExperimentID")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>% dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_MON.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MON.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_MON.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data,
                protocol_version = protocol_version))

  }

}

#' Create capture data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame with capture data

create_capture_MON <- function(db, species_filter, pop_filter){

  Full_capture_data <- utils::read.csv(paste0(db, "\\", "MON_PrimaryData_MORPH.csv"), na.strings = "") %>%
    #There is a potential issue in excel that numbers are stored as text in the excel sheets.
    #These can easily be coerced back to numerics, but this throws many warnings,
    #which will masks any real problematic coercion issues (e.g. NA introduced by coercion)
    #Therefore, we read everything as text and coerce individually
    dplyr::mutate(dplyr::across(c(4, 8, 17, 38), as.integer)) %>%
    dplyr::mutate(dplyr::across(c(6, 7, 15, 19:25, 27, 28, 36), as.numeric)) %>%
    dplyr::mutate(Species = dplyr::case_when(.data$espece == "ble" ~ species_codes$Species[species_codes$SpeciesID == 14620],
                                             .data$espece == "noi" ~ species_codes$Species[species_codes$SpeciesID == 14610],
                                             .data$espece == "cha" ~ species_codes$Species[species_codes$SpeciesID == 14640],
                                             .data$espece == "eto" ~ "STARLING",
                                             .data$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .data$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .data$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .data$espece == "hup" ~ "CRESTED TIT",
                                             .data$espece == "sit" ~ species_codes$Species[species_codes$SpeciesID == 14790],
                                             .data$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .data$espece == "moid" ~ "HOUSE SPARROW",
                                             .data$espece == "moif" ~ species_codes$Species[species_codes$SpeciesID == 15980],
                                             .data$espece == "non" ~ species_codes$Species[species_codes$SpeciesID == 14400])) %>%
    #Filter by species
    dplyr::filter(.data$Species %in% species_filter) %>%
    dplyr::mutate(CaptureDate = suppressWarnings(as.Date(.data$date_mesure, format = "%d/%m/%Y")),
                  CaptureTime = suppressWarnings(
                    dplyr::na_if(paste(stringr::str_pad((24*as.numeric(.data$heure)) %/% 1, width = 2, pad = "0"),
                                       stringr::str_pad(round(((24*as.numeric(.data$heure)) %% 1) * 60), width = 2, pad = "0"),
                                       sep = ":"), "NA:NA")
                  ),
                  BreedingSeason = .data$an,
                  IndvID = purrr::pmap_chr(.l = list(.data$bague),
                                           .f = ~{

                                             if(grepl(pattern = "non-ident", x = ..1)){

                                               return(NA_character_)

                                             } else {

                                               return(..1)

                                             }

                                           }),
                  WingLength = .data$aile,
                  BeakLength = .data$becna,
                  Mass = .data$poids,
                  ObserverID = .data$obs,
                  Destination = .data$dest,
                  ActionTaken = dplyr::case_when(.data$action == "bcj" ~ "Ringed",
                                                 .data$action == "mor" ~ "Dead",
                                                 .data$action == "nb" ~ "Not_Ringed",
                                                 .data$action == "ct" ~ "Caught (already ringed)"),
                  ObservedSex = dplyr::case_when(.data$sex %in% c(1, 4) ~ "M",
                                                 .data$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.data$sex_g == 1 ~ "M",
                                                .data$sex_g == 2 ~ "F"),
                  CaptureMethod = dplyr::case_when(.data$fil == "0" ~ "Nest box",
                                                   .data$fil == "1" ~ "Mist net",
                                                   .data$fil == "2" ~ "Trap cage and caller",
                                                   .data$fil == "3" ~ "Winter roost")) %>%
    dplyr::mutate(LocationID = dplyr::case_when(is.na(.data$nic) ~ NA_character_,
                                                is.na(.data$CaptureMethod) ~ paste(.data$lieu, .data$nic, "NB", sep = "_"),
                                                .data$CaptureMethod %in% c("Nest box", "Winter roost") ~ paste(.data$lieu, .data$nic, "NB", sep = "_"),
                                                .data$CaptureMethod %in% c("Mist net", "Trap cage and caller") ~ paste(.data$lieu, .data$nic, "MN", sep = "_")),
                  Age_observed = purrr::pmap_int(.l = list(.data$age),
                                                 .f = ~{

                                                   if(is.na(..1)){

                                                     return(NA_integer_)

                                                   } else {

                                                     split_age <- strsplit(..1, split = "")

                                                     letter <- toupper(split_age[[1]][1])
                                                     number <- as.integer(split_age[[1]][2])

                                                     if(letter == "P"){

                                                       # FIXME: there is one instance with only "P"
                                                       # new issue related to 2021 data update
                                                       if(is.na(number)) {

                                                         return(NA_integer_)

                                                       } else {

                                                         if(number == 0L) {

                                                           return(1L)

                                                         } else {

                                                           return(3L + 2L*number)

                                                         }

                                                       }

                                                     } else if(letter == "J"){

                                                       return(3L + 2L*number)

                                                     } else if(letter == "A"){

                                                       return(4L + 2L*number)

                                                     } else if(letter == "I"){

                                                       return(4L + 2L*(number - 1L))

                                                     }

                                                   }

                                                 }),
                  ExperimentDescription1 = dplyr::case_when(.data$exp_ad == "FMR" ~ "Metabolic measurement",
                                                            .data$exp_ad == "VACCIN" ~ "Vaccinated",
                                                            .data$exp_ad == "OF" ~ "Openfield??",
                                                            .data$exp_ad == "TRANSFERT" ~ "Translocation",
                                                            .data$exp_ad == "COGNITION" ~ "Cognition test??"),
                  Status = dplyr::case_when(.data$etat_sante %in% c("B", "BMAN") ~ "Injured",
                                            .data$etat_sante %in% c("M", "MMAN") ~ "Dead",
                                            .data$etat_sante == "D" ~ "Chick dead",
                                            .data$etat_sante == "E" ~ "Healthy",
                                            .data$etat_sante == "MAL" ~ "Sick"),
                  FoundDead = dplyr::case_when(.data$etat_sante %in% c("M", "MMAN") ~ TRUE,
                                               .data$action == "mor" ~ TRUE))


  #Format the Capture data to match the standard protocol
  #There are multiple populations within this dataset these are
  # Muro (Corsica), including a deciduous and evergreen sub-population
  # Pirio (Corsica), evergreen only
  # Rouviere (mainly deciduous)
  # Montpellier city (urban habitat)
  # Mont Ventoux (only laying date and clutch size recorded)
  # Remnant populations <- these are not one biological population but are grouped together

  Full_capture_data <- Full_capture_data %>%
    #If there is information in the 'Destination' column, add a different value for ReleasePopID/Plot
    dplyr::mutate(ReleasePlot = dplyr::case_when(is.na(.data$Destination) ~ .data$lieu,
                                                 grepl(pattern = "voli", x = .data$Destination) ~ "aviary",
                                                 TRUE ~ .data$Destination)) %>%
    dplyr::mutate(CapturePopID = identify_PopID_MON(.data$lieu),
                  ReleasePopID = identify_PopID_MON(.data$ReleasePlot),
                  CapturePlot = .data$lieu,
                  Tarsus = dplyr::case_when(is.na(.data$tarsed) & is.na(.data$tarseg) ~ NA_real_,
                                            is.na(.data$tarsed) & !is.na(.data$tarseg) ~ as.numeric(.data$tarseg),
                                            TRUE ~ .data$tarsed)) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Alternative")) %>%
    dplyr::filter(.data$CapturePopID %in% pop_filter) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "FoundDead", "ObserverID", "LocationID",
                  "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot", "Mass", "Tarsus",
                  "OriginalTarsusMethod", "WingLength", "Age_observed", "ObservedSex", "GeneticSex",
                  "ExperimentDescription1", "latitude", "longitude", "CaptureMethod")



  #Do the same for the chick capture data
  #As above, we read all in as text and then coerce afterwards
  Chick_capture_data <- readr::read_delim(paste0(db, "/MON_PrimaryData_POUS.csv"), show_col_types = FALSE) %>%
    dplyr::mutate(dplyr::across(c(3, 14, 16), as.integer)) %>%
    dplyr::mutate(dplyr::across(c(5, 6, 12, 17, 19:21, 34, 36), as.numeric)) %>%
    dplyr::mutate(Species = dplyr::case_when(.data$espece == "ble" ~ species_codes$Species[species_codes$SpeciesID == 14620],
                                             .data$espece == "noi" ~ species_codes$Species[species_codes$SpeciesID == 14610],
                                             .data$espece == "cha" ~ species_codes$Species[species_codes$SpeciesID == 14640],
                                             .data$espece == "eto" ~ "STARLING",
                                             .data$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .data$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .data$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .data$espece == "hup" ~ "CRESTED TIT",
                                             .data$espece == "sit" ~ species_codes$Species[species_codes$SpeciesID == 14790],
                                             .data$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .data$espece == "moid" ~ "HOUSE SPARROW",
                                             .data$espece == "moif" ~ species_codes$Species[species_codes$SpeciesID == 15980],
                                             .data$espece == "non" ~ species_codes$Species[species_codes$SpeciesID == 14400])) %>%
    #Filter by species
    #Also remove only the pops we know
    dplyr::filter(.data$Species %in% species_filter) %>%
    dplyr::mutate(CaptureDate = suppressWarnings(as.Date(.data$date_mesure, format = "%d/%m/%Y")),
                  CaptureTime = suppressWarnings(
                    dplyr::na_if(paste(stringr::str_pad((24*as.numeric(.data$heure)) %/% 1, width = 2, pad = "0"),
                                       stringr::str_pad(round(((24*as.numeric(.data$heure)) %% 1) * 60),
                                                        width = 2, pad = "0"),
                                       sep = ":"), "NA:NA")),
                  BreedingSeason = .data$an,
                  IndvID = dplyr::case_when(grepl(pattern = "no_ident", x = .data$bague) ~ NA_character_,
                                            TRUE ~ .data$bague),
                  Mass = .data$poids,
                  ObserverID = .data$obs,
                  ChickAge = as.integer(.data$age_plume),
                  ObservedSex = dplyr::case_when(.data$sex %in% c(1, 4) ~ "M",
                                                 .data$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.data$sex_g == 1 ~ "M",
                                                .data$sex_g == 2 ~ "F"),
                  Age_observed = 1L,
                  ExperimentDescription1 = dplyr::case_when(.data$expou == "1" ~ "Manipulation affect selective value (e.g. cross foster)",
                                                            .data$expou == "2" ~ "No manipulation that affects selective value (e.g. weighing)"),
                  ExperimentDescription2 = dplyr::case_when(.data$exp_p == "alimente" ~ "Supplemental feeding",
                                                            .data$exp_p == "poussintron" ~ "Behavioural test",
                                                            .data$exp_p == "poussintronfc" ~ "Behavioural test and heart rate",
                                                            .data$exp_p == "transf" ~ "Translocation",
                                                            .data$exp_p == "fc" ~ "Heart rate monitored",
                                                            .data$exp_p == "biopsie" ~ "Underwent biopsy",
                                                            .data$exp_p == "fmr" ~ "Doubly labled water",
                                                            .data$exp_p == "voliere" ~ "Bred in aviary",
                                                            .data$exp_p == "gavage" ~ "Chick force-feeding supplement",
                                                            .data$exp_p == "inuline" ~ "Insulin injection"),
                  Status = dplyr::case_when(.data$etat_sante == "M" ~ "Dead",
                                            .data$etat_sante == "D" ~ "Chick dead",
                                            .data$etat_sante == "E" ~ "Healthy"),
                  FoundDead = dplyr::case_when(.data$etat_sante %in% c("M", "MMAN", "D") ~ TRUE),
                  LocationID = paste(.data$lieu, .data$nic, "NB", sep = "_"),
                  CapturePopID = identify_PopID_MON(.data$lieu),
                  CapturePlot = .data$lieu,
                  Tarsus = dplyr::case_when(is.na(.data$tarsed) & is.na(.data$tarseg) ~ NA_real_,
                                                                      is.na(.data$tarsed) & !is.na(.data$tarseg) ~ as.numeric(.data$tarseg),
                                                                      TRUE ~ .data$tarsed)) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$tarsed) ~ "Alternative"),
                  WingLength = NA_real_,
                  OrigBoxNumber = purrr::map(.x = .data$orig, .f = find_box),
                  DestBoxNumber = purrr::map(.x = .data$dest, .f = find_box),
                  BroodIDLaid = purrr::pmap_chr(.l = list(.data$BreedingSeason,
                                                          .data$lieu,
                                                          .data$OrigBoxNumber),
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
                  BroodIDFledged = purrr::pmap_chr(.l = list(.data$BreedingSeason,
                                                             .data$lieu,
                                                             .data$DestBoxNumber),
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

                                                   })) %>%
    #If there is a destination box, give it a different ReleasePlot/PopID
    dplyr::mutate(ReleasePlot = purrr::map2_chr(.x = .data$DestBoxNumber,
                                                .y = .data$CapturePlot,
                                                .f = ~{

                                                  if(length(.x) == 2){

                                                    return(.x[1])

                                                  } else {

                                                    return(.y)

                                                  }

                                                }),
    ReleasePopID = identify_PopID_MON(.data$ReleasePlot)) %>%
    dplyr::filter(.data$CapturePopID %in% pop_filter) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime", "FoundDead", "ObserverID",
                  "LocationID", "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot", "Mass", "Tarsus",
                  "OriginalTarsusMethod", "WingLength", "Age_observed", "ChickAge", "ObservedSex", "GeneticSex",
                  "ExperimentDescription1", "ExperimentDescription2", "BroodIDLaid", "BroodIDFledged")

  Capture_data <- dplyr::bind_rows(Full_capture_data, Chick_capture_data) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "FoundDead", "ObserverID", "LocationID", "CapturePopID", "CapturePlot", "ReleasePopID",
                  "ReleasePlot", "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated", "ChickAge", "ObservedSex", "GeneticSex",
                  "BroodIDLaid", "BroodIDFledged", "latitude", "longitude", "CaptureMethod",
                  "ExperimentDescription1", "ExperimentDescription2")

  Capture_data <- Capture_data %>%
  ## The normal number of characters in an IndvID is 7
  ## Any IndvIDs that have more than 8 characters or fewer than 6 characters and are not only numbers are set to NA
  dplyr::mutate(IndvID = dplyr::case_when(nchar(.data$IndvID) %in% c(6,7,8) & stringr::str_detect(.data$IndvID, "^[:digit:]+$")  ~ .data$IndvID,
                                             TRUE ~ NA_character_)) %>%

    ## Filter out NAs from IndvID
    dplyr::filter(!is.na(.data$IndvID))

  return(Capture_data)

}

#' Create brood data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame with Brood data

create_brood_MON <- function(db, species_filter, pop_filter){

  Brood_data <- utils::read.csv(paste0(db, "\\", "MON_PrimaryData_DEMO.csv"), na.strings = "") %>%
    dplyr::mutate(dplyr::across(c(21:36), as.character)) %>%
    dplyr::mutate(Species = dplyr::case_when(.data$espece == "ble" ~ species_codes$Species[which(species_codes$SpeciesID == 14620)],
                                             .data$espece == "noi" ~ species_codes$Species[which(species_codes$SpeciesID == 14610)],
                                             .data$espece == "cha" ~ species_codes$Species[which(species_codes$SpeciesID == 14640)],
                                             .data$espece == "eto" ~ "STARLING",
                                             .data$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .data$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .data$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .data$espece == "hup" ~ "CRESTED TIT",
                                             .data$espece == "sit" ~ species_codes$Species[which(species_codes$SpeciesID == 14790)],
                                             .data$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .data$espece == "moid" ~ "HOUSE SPARROW",
                                             .data$espece == "moif" ~ species_codes$Species[which(species_codes$SpeciesID == 15980)],
                                             .data$espece == "non" ~ species_codes$Species[which(species_codes$SpeciesID == 14400)]),
                  Plot = .data$lieu,
                  BoxNumber = .data$nic,
                  LocationID = paste(.data$Plot, .data$BoxNumber, "NB", sep = "_"),
                  BreedingSeason = as.integer(.data$an),
                  LayDate = suppressWarnings(as.Date(.data$date_ponte, format = "%d/%m/%Y")),
                  BroodID = paste(.data$BreedingSeason, .data$Plot, .data$BoxNumber, .data$np,
                                  sep = "_"),
                  ClutchType_observed = dplyr::case_when(.data$np == "1" ~ "first",
                                                         .data$np == "2" ~ "second",
                                                         .data$np == "3" ~ "replacement"),
                  ClutchSize = as.integer(.data$grpo),
                  HatchDate = suppressWarnings(as.Date(.data$date_eclo, format = "%d/%m/%Y")),
                  BroodSize = as.integer(.data$pulecl),
                  NumberFledged = as.integer(.data$pulenv),
                  CauseFailure = .data$mort,
                  MaleID = .data$mbag,
                  FemaleID = .data$fbag,
                  ParasiteTreatment = dplyr::case_when(.data$extnt == "1" ~ "Protected",
                                                       .data$extnt == "2" ~ "Unprotected"),
                  Brood_ExperimentDescription1 = dplyr::case_when(.data$expou == "1" ~ "No intervention that will affect breeding success",
                                                                  .data$expou == "2" ~ "Intervention that can influence number of eggs or quality of chicks"),
                  Brood_ExperimentDescription2 = .data$experience,
                  Crossfostering_treatment = .data$explique,
                  NumberParasites = .data$proto,
                  FailureCause = dplyr::case_when(.data$mort == "ABA" ~ "Abandoned",
                                                  .data$mort == "MAN" ~ "Manipulated by researchers",
                                                  .data$mort == "PRE" ~ "Predation",
                                                  .data$mort == "CLI" ~ "Climatic event (e.g. storm)",
                                                  .data$mort == "MAL" ~ "Sickness",
                                                  .data$mort == "NCT" ~ "Not checked??"),
                  LayDateError = NA_integer_,
                  ClutchSizeError = NA_integer_,
                  HatchDateError = NA_integer_,
                  BroodSizeError = NA_integer_,
                  FledgeDate = as.Date(NA),
                  FledgeDateError = NA_integer_,
                  NumberFledgedError = NA_integer_) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    #Only include capture pop and plot for now, until we work out how to code translocations
    dplyr::mutate(PopID = identify_PopID_MON(.data$lieu)) %>%
    dplyr::arrange(.data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(ExperimentID = dplyr::case_when((!is.na(.data$Crossfostering_treatment) | !is.na(.data$Brood_ExperimentDescription2) | .data$ParasiteTreatment == "Treated" | .data$expou == "2") ~ "TRUE",
                                                  (is.na(.data$Crossfostering_treatment) & is.na(.data$Brood_ExperimentDescription2) & .data$ParasiteTreatment != "Treated" & .data$expou != "2") ~ "FALSE")) %>%
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    #Keep all chick codes because we will use these for individual data table and remove later
    #Keep box number to link to Capture data
    dplyr::select("BroodID", "PopID", "BreedingSeason", "Species", "Plot",
                  "LocationID", "FemaleID", "MaleID", "ClutchType_observed",
                  "ClutchType_calculated", "LayDate", "LayDateError",
                  "ClutchSize", "ClutchSizeError", "HatchDate", "HatchDateError",
                  "BroodSize", "BroodSizeError", "FledgeDate", "FledgeDateError",
                  "NumberFledged", "NumberFledgedError", "ExperimentID",
                  "pulbag1":"pulbag14", "BoxNumber")

    ## Set non-standard IDs to NA
    Brood_data <- Brood_data %>%
      ## The normal number of characters in an IndvID is 7
      ## Any IndvIDs that have more than 8 characters or fewer than 6 characters and are not only numbers are set to NA
      dplyr::mutate(FemaleID = dplyr::case_when(nchar(.data$FemaleID) %in% c(6,7,8) & stringr::str_detect(.data$FemaleID, "^[:digit:]+$")  ~ .data$FemaleID,
                                                    TRUE ~ NA_character_),
                    MaleID = dplyr::case_when(nchar(.data$MaleID) %in% c(6,7,8) & stringr::str_detect(.data$MaleID, "^[:digit:]+$")  ~ .data$MaleID,
                                                    TRUE ~ NA_character_))

  return(Brood_data)

}

#' Create individual data table for Montpellier
#'
#' @param Capture_data Capture data generated by \code{\link{create_capture_MON}}.
#' @param Brood_data Capture data generated by \code{\link{create_brood_MON}}.
#' @param verbose When chicks with duplicate nests are found, should a message be printed?
#'
#' @return A data frame with Individual data

create_individual_MON <- function(Capture_data, Brood_data, verbose){

  BroodAssignment <- Brood_data %>%
    dplyr::select("BroodIDLaid" = "BroodID", "pulbag1":"pulbag14") %>%
    tidyr::pivot_longer(cols = "pulbag1":"pulbag14", names_to = "ChickNr", values_to = "IndvID") %>%
    dplyr::select(-"ChickNr") %>%
    #Remove broods where there were no ringed chicks
    dplyr::filter(!is.na(.data$IndvID))

  #Identify any cases where a individual was cross-fostered
  #It had info in the orig and/or dest column
  Cross_foster <- Capture_data %>%
    dplyr::filter(!is.na(.data$BroodIDLaid) | !is.na(.data$BroodIDFledged))

  #Create progress bar to track cross-foster assignment
  pb <- progress::progress_bar$new(total = nrow(Cross_foster))

  Cross_foster  <- Cross_foster %>%
    dplyr::select("IndvID", "LocationID", "CaptureDate", "BroodIDLaid", "BroodIDFledged") %>%
    #Run through each example and determine the destination
    #There are three possible situations
    #1. Individual caught in the genetic brood where the destionation brood is listed
    #2. Individual caught in the foster brood where the origin brood is listed
    #3. Individual with both origin and desination brood listed
    #Use apply because it doesn't coerce dates to numbers!!!!!
    apply(1, function(x, Brood_data){

      pb$tick()

      #If the destination brood has been given
      if(!is.na(x["BroodIDFledged"])){

        split_info <- unlist(stringr::str_split(x["BroodIDFledged"], pattern = "_"))
        CaptureDate <- as.Date(x["CaptureDate"])

        #If it has been transferred to an aviary, list this
        if (stringr::str_detect(string = split_info[1], pattern = "voli")) {

          return(tibble::tibble(IndvID = x["IndvID"], BroodIDFledged = "aviary"))

        }

        #Find the nest that was active in the period of capture
        possible_nest <- Brood_data %>%
          dplyr::filter(.data$BreedingSeason == split_info[1],
                        .data$Plot == split_info[2],
                        .data$BoxNumber == split_info[3],
                        .data$LayDate < CaptureDate)

        if(nrow(possible_nest) == 1){

          return(tibble::tibble(IndvID = x["IndvID"], BroodIDFledged = possible_nest$BroodID))

        } else if(nrow(possible_nest) > 1 & length(unique(possible_nest$BoxNumber)) == 1){

          possible_nest <- possible_nest %>%
            dplyr::slice(dplyr::n())

        } else {

          if(verbose){

            print(x["IndvID"])
            print(CaptureDate)
            print(split_info)
            print(possible_nest, width = Inf)

            message("CROSS-FOSTERING DESTINATION RETURNS ERROR (MON DATA). This may be because more than one cross-fostering event
               is listed, or the destination nest doesn't exist. This record is skipped")

          }

          return(NULL)

        }

      } else if(!is.na(x["BroodIDLaid"])){

        split_info <- unlist(stringr::str_split(x["LocationID"], pattern = "_"))
        CaptureDate <- as.Date(x["CaptureDate"])

        #Find the nest that was active in the period of capture
        possible_nest <- Brood_data %>%
          dplyr::filter(.data$BreedingSeason == lubridate::year(CaptureDate),
                        .data$Plot == split_info[1],
                        .data$BoxNumber == split_info[2],
                        .data$LayDate < CaptureDate)

        if(nrow(possible_nest) > 1){

          stop("MORE THAN ONE POSSIBLE NEST IDENTIFIED FOR CROSS-FOSTERING (MON DATA)")

        } else {

          if(verbose){

            print(x["IndvID"])
            print(CaptureDate)
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
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(stats::na.omit(unique(.data$Species))),
                                            .f = ~{

                                              if(length(..1) == 1){

                                                return(..1)

                                              } else {

                                                return("CONFLICTED")

                                              }

                                            }),
                  PopID = dplyr::first(.data$CapturePopID),
                  RingSeason = dplyr::first(.data$BreedingSeason),
                  RingAge = dplyr::case_when(is.na(dplyr::first(Age_observed)) ~ NA_character_,
                                             dplyr::first(Age_observed) <= 3 ~ "chick",
                                             dplyr::first(Age_observed) > 3 ~ "adult"),
                  Sex = purrr::pmap_chr(.l = list(list(unique(.data$ObservedSex)),
                                                  list(unique(.data$GeneticSex))),
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

                                            #Check if there are any non-NA observed records
                                            if(length(stats::na.omit(..1)) > 0){

                                              #If there is just one, return that
                                              if(length(stats::na.omit(..1)) == 1){

                                                return(stats::na.omit(..1))

                                              } else {

                                                #Otherwise it is conflicted
                                                return("C")

                                              }

                                            } else {

                                              #If no sex data is available, return NA
                                              return(NA_character_)

                                            }

                                          }

                                        }))

  #Join in Brood data for all individuals
  Individual_data_single_records <- dplyr::left_join(Individual_data, BroodAssignment, by = "IndvID") %>%
    #Join in cross-fostering info
    dplyr::left_join(Cross_foster %>%
                       dplyr::select("IndvID", "BroodIDFledged"),
                     by = "IndvID") %>%
    #If no BroodIDFledged is listed, make it the same as BroodIDLaid
    dplyr::mutate(BroodIDFledged = dplyr::case_when(is.na(.data$BroodIDFledged) ~ .data$BroodIDLaid,
                                                    TRUE ~ .data$BroodIDFledged)) %>%
    #Remove duplicates that will occur due to multiple values being in dest/orig
    dplyr::distinct() %>%
    dplyr::select("IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged", "RingSeason", "RingAge", "Sex")

  #Identify those cases where an individual has multiple records in individual data
  duplicates <- Individual_data_single_records %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::pull("IndvID")

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
    dplyr::group_by(.data$IndvID) %>%
    dplyr::slice(1)

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
  nestbox_latlong <- utils::read.csv(paste0(db, "\\", "MON_PrimaryData_NestBoxLocation.csv"), na.strings = "") %>%
    dplyr::filter(!is.na(.data$latitude)) %>%
    dplyr::mutate(LocationID_join = paste(.data$abr_station, .data$nichoir, sep = "_")) %>%
    dplyr::select("LocationID_join", "latitude", "longitude") %>%
    dplyr::mutate(dplyr::across(c("latitude":"longitude"), as.numeric))

  #There are some nestboxes outside the study area
  nestbox_latlong_outside <- utils::read.csv(paste0(db, "\\", "MON_PrimaryData_OffSiteLocation.csv"), na.strings = "") %>%
    dplyr::filter(!is.na(.data$la)) %>%
    dplyr::mutate(LocationID_join = paste(.data$st, .data$ni_localisation, sep = "_"),
                  latitude = .data$la,
                  longitude = .data$lo) %>%
    dplyr::select("LocationID_join", "latitude", "longitude") %>%
    dplyr::mutate(dplyr::across(c("latitude":"longitude"), as.numeric)) %>%
    #There are some replicate groups, compress them to one record per location
    dplyr::group_by(.data$LocationID_join) %>%
    dplyr::slice(1)

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
      dplyr::select("BreedingSeason", "latitude", "longitude") %>%
      dplyr::group_by(.data$latitude, .data$longitude) %>%
      dplyr::summarise(StartSeason = min(.data$BreedingSeason),
                       EndSeason = NA_integer_,
                       PopID = "MIS",
                       NestboxID = NA_character_,
                       LocationType = "MN",
                       Habitat = NA_character_) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(LocationID = paste("hs", 1:dplyr::n(), sep = "_")) %>%
      dplyr::select("LocationID", "NestboxID", "LocationType", "PopID", "Latitude" = "latitude",
                    "Longitude" = "longitude", "StartSeason", "EndSeason", "Habitat")

  } else {

    outside_locations <- NULL

  }

  #For cases where no lat/long are available
  #We need to link the lat/long from separate file (nestbox_latlong above)
  inside_locations <- Capture_data %>%
    dplyr::filter(is.na(.data$longitude) & !is.na(.data$LocationID)) %>%
    dplyr::mutate(LocationID_join = paste(stringr::str_split_i(.data$LocationID, pattern = "_", i = 1),
                                          stringr::str_split_i(.data$LocationID, pattern = "_", i = 2),
                                          sep = "_")) %>%
    dplyr::select("BreedingSeason", "LocationID", "LocationID_join", "PopID" = "CapturePopID")

  #Also for broods only identified in the brood data table
  nest_locations <- Brood_data %>%
    dplyr::mutate(LocationID_join = paste(stringr::str_split_i(.data$LocationID, pattern = "_", i = 1),
                                          stringr::str_split_i(.data$LocationID, pattern = "_", i = 2),
                                          sep = "_")) %>%
    dplyr::select("BreedingSeason", "LocationID", "LocationID_join", "PopID")

  #Combine to have all locations without lat/long
  non_latlong_locations <- dplyr::bind_rows(inside_locations, nest_locations) %>%
    dplyr::left_join(all_nestbox_latlong, by = "LocationID_join") %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::summarise(NestboxID = dplyr::case_when(grepl(unique(.data$LocationID),
                                                        pattern = "NB") ~ unique(.data$LocationID),
                                                  TRUE ~ NA_character_),
                     LocationType = stringr::str_split(unique(.data$LocationID),
                                                       pattern = "_", simplify = TRUE)[3],
                     PopID = unique(.data$PopID),
                     Latitude = dplyr::first(.data$latitude),
                     Longitude = dplyr::first(.data$longitude),
                     StartSeason = min(.data$BreedingSeason),
                     EndSeason = NA_integer_,
                     Plot = stringr::str_split(.data$LocationID,
                                               pattern = "_",
                                               simplify = TRUE)[1],
                     Habitat = dplyr::case_when(.data$Plot %in% c("ava", "fel", "mur", "rou") ~ "deciduous",
                                                .data$Plot %in% c("fil", "ari", "gra",
                                                                  "pir", "tua") ~ "evergreen",
                                                .data$Plot %in% c("bot", "cef", "fac", "font",
                                                                  "gram", "mas", "mos", "val", "zoo") ~ "urban",
                                                TRUE ~ NA_character_)) %>%
    dplyr::select(-"Plot")

  Location_data <- dplyr::bind_rows(outside_locations, non_latlong_locations)

  return(Location_data)

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
                   grepl(x = variable, pattern = "voli|aviary") ~ "aviary")

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
