#' Construct standard summary for data from Wytham Woods, UK.
#'
#' A pipeline to produce a standard output for the nest box population in Wytham
#' Woods, UK, administered by Edward Grey Institute Oxford (Ben Sheldon).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{LocationID}: Unique locations are defined by using the nestbox IDs. These include
#'plot information as nest box numbers are not unique across plots.
#'
#'\strong{ExperimentID}: There are experiment codes given, I have tried to
#' adapt these to the ExperimentID categories described in the standard
#' protocol; however, these need to be checked by data owners. For now I assume:
#' - Egg manipulation (code 2) is clutch size manipulation.
#' - Chick manipulation (code 3) is cross fostering.
#' - Alter temperature in nest box (code 7) only affects phenology.
#' - Feeding manipulation (code 8) affects phenology.
#' - Altering parasites, predation, competition and territory quality (codes 9, 11, 12, 13) all affect survival.
#'
#'\strong{Species}: We include nests form blue tits, great tits, coal tits,
#'marsh tits, and nuthatches. Currently, mixed broods are treated as having no
#'species (NA), but we will fix this. There is one brood with Species 'w',
#'which I suspect is willow tit. This is currently ignored.
#'
#'\strong{AvgEggMass}: There are two columns with mass data, one is a 'legacy'
#'column. When these overlap, they can differ up to 5g! I assume the 'legacy'
#'column is less preferred (due to it's name). I only use this data where no
#'other egg mass data is provided.
#'
#'\strong{AvgChickMass}: Average egg mass is recorded for each brood; however,
#'this may include mass measurements taken outside of our focal period (14 - 16 days old).
#'Therefore, this column is ignored and AvgChickMass is calculated manually from
#'capture data.
#'
#'\strong{HatchDate}: As with AvgEggMass, there is also a 'legacy' column for
#'hatch date. This is only used if the regular column is empty.
#'
#'\strong{Age_observed}: Ages are assumed to follow EURING system and are
#'included unchanged. The only exception to this is '3J', which is an old EURING
#'code. This is treated as EURING age 3.
#'
#'\strong{Tarsus}: Tarsus method is either 'M', 'S', or NA. We have assumed that
#''M' and NA are the Oxford min/max method and converted accordingly. Currently, we treat
#''S' as Svensson's Alternative and do not convert. This needs to be clarified with
#'data owner.
#'
#'\strong{Sex_observed, Sex_calculated}: Any uncertainty in sex is ignored. For example, 'm?' is treated as male.
#'
#'\strong{BroodIDLaid and BroodIDFledged}: There are (some) cases where BroodIDLaid/Fledged
#'have conflicting records for an individual. In these cases, BroodIDLaid/Fledged is
#'simply listed as 'CONFLICTED'.
#'
#'\strong{CaptureAlive, ReleaseAlive}: All individuals are assumed to be captured and released alive.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#' @export

format_WYT <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  start_time <- Sys.time()

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_WYT(db = db, species_filter = species)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_WYT(db = db, Brood_data = Brood_data, species_filter = species)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_WYT(Capture_data)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_WYT(Brood_data, Capture_data)

  # WRANGLE DATA FOR EXPORT

  # Calculate average chick mass/tarsus
  avg_chick_mass <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16) & !is.na(.data$Mass)) %>%
    dplyr::group_by(.data$BroodIDLaid) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass = dplyr::n())

  avg_chick_tarsus <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16) & !is.na(.data$Tarsus)) %>%
    dplyr::group_by(.data$BroodIDLaid) %>%
    dplyr::summarise(AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = dplyr::first(.data$OriginalTarsusMethod))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_chick_mass, by = c("BroodID" = "BroodIDLaid")) %>%
    dplyr::left_join(avg_chick_tarsus, by = c("BroodID" = "BroodIDLaid")) %>%
    # Keep only necessary columns
    dplyr::select(tidyselect::contains(names(brood_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(brood_data_template[0, !(names(brood_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    # Reorder columns
    dplyr::select(names(brood_data_template))

  # Remove unneeded columns in Capture data
  Capture_data <- Capture_data %>%
    # Keep only necessary columns
    dplyr::select(tidyselect::contains(names(capture_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    # Reorder columns
    dplyr::select(names(capture_data_template))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("\nAll tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("\nSaving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WYT.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WYT.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WYT.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WYT.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_WYT.txt"),
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

#' Create brood data table for Wytham Woods
#'
#' @param db Location of primary data from Whytham Woods.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame with Brood data

create_brood_WYT <- function(db, species_filter){

  Brood_data_raw <- utils::read.csv(paste0(db, "/WYT_PrimaryData_Brood.csv"), header = T,
                              sep = ",", stringsAsFactors = FALSE) %>%
    janitor::clean_names()

  Brood_data <- Brood_data_raw %>%
    #Rename columns to meet our standard format
    dplyr::mutate(BreedingSeason = .data$year,
                  LocationID = toupper(.data$nestbox),
                  PopID = "WYT",
                  Plot = toupper(.data$section),
                  Species = dplyr::case_when(.data$species == "b" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$species == "g" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$species == "c" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             .data$species == "n" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$species == "m" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species)) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    dplyr::mutate(LayDate_observed = as.Date(.data$lay_date, format = "%d/%m/%Y"),
                  # Uncertainty in LayDate_observed (stored in lay_date_uncertainty) is currently not used
                  # TODO: ask data owner
                  HatchDate_observed = dplyr::case_when(
                    !is.na(.data$hatch_date) ~ as.Date(as.character(.data$hatch_date),
                                                       format = "%d/%m/%Y"),
                    is.na(.data$hatch_date) ~ as.Date(as.character(.data$legacy_april_hatch_date),
                                                      format = "%d/%m/%Y")),
                  NumberEggs = .data$num_eggs_weighed,
                  AvgEggMass = dplyr::case_when(
                    !is.na(.data$total_egg_weight) & !is.na(.data$num_eggs_weighed) ~ .data$total_egg_weight / .data$num_eggs_weighed,
                    TRUE ~ .data$legacy_average_egg_weight),
                  ClutchSize_observed = .data$clutch_size,
                  BroodSize_observed = .data$num_chicks,
                  NumberFledged_observed = .data$num_fledglings,
                  FemaleID =  toupper(dplyr::na_if(.data$mother, "")),
                  MaleID = toupper(dplyr::na_if(.data$father, "")),
                  ExperimentID = dplyr::case_when(.data$experiment_codes == "1" ~ "UNKOWN",
                                                  .data$experiment_codes == "2" ~ "COHORT",
                                                  .data$experiment_codes == "3" ~ "PARENTAGE",
                                                  .data$experiment_codes == "7" ~ "PHENOLOGY",
                                                  .data$experiment_codes == "8" ~ "PHENOLOGY",
                                                  .data$experiment_codes == "9" ~ "SURVIVAL",
                                                  .data$experiment_codes == "11" ~ "SURVIVAL",
                                                  .data$experiment_codes == "12" ~ "SURVIVAL",
                                                  .data$experiment_codes == "13" ~ "SURVIVAL"),
                  BroodID = toupper(.data$pnum)) %>%
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_observed = NA_character_,
                  ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"))

  return(Brood_data)

}

#' Create capture data table for Wytham Woods
#'
#' @param db Location of primary data from Whytham Woods.
#' @param Brood_data Brood data generated by \code{\link{create_brood_WYT}}.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame with Capture data

create_capture_WYT <- function(db, Brood_data, species_filter){

  # Load chick capture data
  Chick_captures_old <- readxl::read_xlsx(paste0(db, "/WYT_PrimaryData_Capture.xlsx"),
                                          col_types = "text", sheet = "1947-2012",
                                          na = c("unringed", "unrrunt", "UNRRUNT")) %>%
    janitor::clean_names()

  Chick_captures_old <- Chick_captures_old %>%
    dplyr::mutate(Species = dplyr::case_when(toupper(.data$species_code) == "GRETI" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             toupper(.data$species_code) == "BLUTI" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             toupper(.data$species_code) == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             toupper(.data$species_code) == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species)) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date_time)),
                  CaptureTime = NA_character_,
                  BreedingSeason = dplyr::case_when(is.na(.data$CaptureDate) ~ as.numeric(stringr::str_sub(.data$pnum, start = 1, end = 4)),
                                                    TRUE ~ lubridate::year(.data$CaptureDate))) %>%
    dplyr::mutate(IndvID = toupper(.data$ring_number),
                  CapturePopID = "WYT",
                  CapturePlot = stringr::str_remove_all(string = toupper(.data$pnum), pattern = "\\d"),
                  LocationID = stringr::str_sub(toupper(.data$pnum), start = 6),
                  # Release is just NA for now because we need to find the location with cross-fostering
                  ReleasePopID = "WYT",
                  ReleasePlot = NA_character_,
                  ObserverID = NA_character_,
                  Mass = as.numeric(.data$weight),
                  WingLength = as.numeric(.data$wing_length),
                  Age_observed = dplyr::case_when(.data$age %in% c("3", "3J") ~ 3L,
                                                  .data$age == "0" ~ NA_integer_,
                                                  .data$age == "1" ~ 1L,
                                                  .data$age == "2" ~ 2L,
                                                  .data$age == "4" ~ 4L,
                                                  .data$age == "5" ~ 5L,
                                                  .data$age == "6" ~ 6L)) %>%
    # Add tarsus. We are assuming that when tarsus method is "S" this is Svensson's Alternative and doesn't need conversion
    # Otherwise (NA or "M") assume it's Oxford.
    # TODO: Need to check this with Ella.
    dplyr::mutate(Tarsus = dplyr::case_when(is.na(.data$tarsus_length) ~ NA_real_,
                                            is.na(.data$tarsus_length_method) ~ convert_tarsus(as.numeric(.data$tarsus_length), method = "Oxford"),
                                            .data$tarsus_length_method == "M" ~ convert_tarsus(as.numeric(.data$tarsus_length), method = "Oxford"),
                                            TRUE ~ as.numeric(.data$tarsus_length)),
                  OriginalTarsusMethod = dplyr::case_when(is.na(.data$tarsus_length) ~ NA_character_,
                                                          is.na(.data$tarsus_length_method) ~ "Oxford",
                                                          .data$tarsus_length_method == "M" ~ "Oxford",
                                                          TRUE ~ "Alternative")) %>%
    # Include sex and brood info for linked to Brood_data and Individual_data
    # Sex 'n' is assumed to be a typo and treated as male
    # As always, we ignore sex uncertainty
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl(toupper(.data$sex), pattern = "F") ~ "F",
                                                  grepl(toupper(.data$sex), pattern = "M|N") ~ "M")) %>%
    dplyr::mutate(BroodIDLaid = dplyr::case_when(!.data$Age_observed %in% c(1, 3) ~ NA_character_,
                                                 .data$Age_observed %in% c(1, 3) & is.na(.data$origin_pnum) ~ toupper(.data$pnum),
                                                 .data$Age_observed %in% c(1, 3) & !is.na(.data$origin_pnum) ~ toupper(.data$origin_pnum)),
                  BroodIDFledged = dplyr::case_when(!.data$Age_observed %in% c(1, 3) ~ NA_character_,
                                                    TRUE ~ toupper(.data$pnum))) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate",
                  "CaptureTime", "ObserverID", "LocationID", "CapturePopID",
                  "CapturePlot", "ReleasePopID", "ReleasePlot", "Mass",
                  "Tarsus", "OriginalTarsusMethod", "WingLength",
                  "Age_observed", "Sex_observed", "BroodIDLaid", "BroodIDFledged")

  Chick_captures_new <- readxl::read_xlsx(paste0(db, "/WYT_PrimaryData_Capture.xlsx"),
                                          col_types = "text", sheet = "2013-2018",
                                          na = c("unringed", "unrrunt", "UNRRUNT")) %>%
    janitor::clean_names()

  Chick_captures_new <- Chick_captures_new %>%
    dplyr::mutate(Species = dplyr::case_when(toupper(.data$bto_species_code) == "GRETI" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             toupper(.data$bto_species_code) == "BLUTI" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             toupper(.data$bto_species_code) == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             toupper(.data$bto_species_code) == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             toupper(.data$bto_species_code) == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species),
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date_time) %/% 1),
                  CaptureTime = paste(stringr::str_pad(string = ((as.numeric(.data$date_time) %% 1) * 24) %/% 1,
                                                       width = 2, pad = "0"),
                                      stringr::str_pad(string = round((((as.numeric(.data$date_time) %% 1) * 24) %% 1) * 60),
                                                       width = 2, pad = "0"), sep = ":"),
                  BreedingSeason = dplyr::case_when(is.na(.data$CaptureDate) ~ as.numeric(stringr::str_sub(.data$pnum, start = 1, end = 4)),
                                                    TRUE ~ lubridate::year(.data$CaptureDate))) %>%
    dplyr::mutate(IndvID = toupper(.data$bto_ring),
                  CapturePopID = "WYT",
                  CapturePlot = stringr::str_remove_all(string = toupper(.data$pnum), pattern = "\\d"),
                  LocationID = stringr::str_sub(toupper(.data$pnum), start = 6),
                  # Release is just NA for now because we need to find the location with cross-fostering
                  ReleasePopID = "WYT",
                  ReleasePlot = NA_character_,
                  ObserverID = NA_character_,
                  Mass = as.numeric(.data$weight),
                  WingLength = as.numeric(.data$wing_length),
                  # Currently just treat ages as is (assume they are EURING codes)
                  Age_observed = as.integer(.data$age)) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    # Add tarsus. We are assuming that when tarsus method is "S" this is Svensson's Alternative and doesn't need conversion
    # Otherwise (NA or "M") assume it's Oxford.
    # TODO: Need to check this with Ella.
    dplyr::mutate(Tarsus = dplyr::case_when(is.na(.data$tarsus_length) ~ NA_real_,
                                            is.na(.data$tarsus_length_method) ~ convert_tarsus(as.numeric(.data$tarsus_length), method = "Oxford"),
                                            toupper(.data$tarsus_length_method) == "M" ~ convert_tarsus(as.numeric(.data$tarsus_length), method = "Oxford"),
                                            TRUE ~ as.numeric(.data$tarsus_length)),
                  OriginalTarsusMethod = dplyr::case_when(is.na(.data$tarsus_length) ~ NA_character_,
                                                          is.na(.data$tarsus_length_method) ~ "Oxford",
                                                          toupper(.data$tarsus_length_method) == "M" ~ "Oxford",
                                                          TRUE ~ "Alternative")) %>%
    # Include sex and brood info for linked to Brood_data and Individual_data
    # Sex 'n' is assumed to be a typo and treated as male
    # As always, we ignore sex uncertainty
    dplyr::mutate(Sex_observed = .data$sex)  %>%
    dplyr::mutate(BroodIDLaid = dplyr::case_when(!.data$Age_observed %in% c(1, 3) ~ NA_character_,
                                                 TRUE ~ toupper(.data$pnum)),
                  BroodIDFledged = dplyr::case_when(!.data$Age_observed %in% c(1, 3) ~ NA_character_,
                                                    TRUE ~ toupper(.data$pnum))) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate",
                  "CaptureTime", "ObserverID", "LocationID", "CapturePopID",
                  "CapturePlot", "ReleasePopID", "ReleasePlot", "Mass",
                  "Tarsus", "OriginalTarsusMethod", "WingLength",
                  "Age_observed", "Sex_observed", "BroodIDLaid", "BroodIDFledged")

  # Combine old and new
  Capture_data <- dplyr::bind_rows(Chick_captures_new, Chick_captures_old) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate_observed"),
                     by = c("BroodIDLaid" = "BroodID")) %>%
    dplyr::mutate(ChickAge = dplyr::case_when(.data$Age_observed == 1 ~ as.integer(.data$CaptureDate - .data$HatchDate_observed),
                                              TRUE ~ NA_integer_),
                  # We have no information on status of captures/releases,
                  # so we assume all individuals were captured/released alive
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%
    # Arrange by IndvID and CaptureDate and add unique CaptureID
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup()

  return(Capture_data)

}

#' Create individual data table for Wytham Woods
#'
#' @param Capture_data Capture data generated by \code{\link{create_capture_WYT}}.
#'
#' @return A data frame with Individual data

create_individual_WYT <- function(Capture_data){

  # For every individual determine their unchanged individual information
  Individual_data <- Capture_data %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = dplyr::case_when(length(unique(stats::na.omit(.data$Species))) > 1 ~ "CCCCCC",
                                                TRUE ~ dplyr::first(stats::na.omit(.data$Species))),
                     PopID = "WYT",
                     BroodIDLaid = dplyr::case_when(length(unique(stats::na.omit(.data$BroodIDLaid))) == 1 ~ dplyr::first(stats::na.omit(.data$BroodIDLaid)),
                                                    length(unique(stats::na.omit(.data$BroodIDLaid))) > 1 ~ "CONFLICTED",
                                                    TRUE ~ NA_character_),
                     BroodIDFledged = dplyr::case_when(length(unique(stats::na.omit(.data$BroodIDFledged))) == 1 ~ dplyr::first(stats::na.omit(.data$BroodIDFledged)),
                                                       length(unique(stats::na.omit(.data$BroodIDFledged))) > 1 ~ "CONFLICTED",
                                                       TRUE ~ NA_character_),
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = dplyr::case_when(all(is.na(.data$Age_calculated)) ~ NA_character_,
                                                any(.data$Age_calculated %in% c(1, 3)) ~ "chick",
                                                min(.data$Age_calculated) == 2 ~ NA_character_,
                                                TRUE ~ "adult"),
                     Sex_calculated = dplyr::case_when(length(unique(stats::na.omit(.data$Sex_observed))) == 2 ~ "C",
                                                       TRUE ~ dplyr::first(stats::na.omit(.data$Sex_observed))),
                     .groups = "drop") %>%
    # Keep only necessary columns
    dplyr::select(tidyselect::contains(names(individual_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(individual_data_template[0, !(names(individual_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    # Reorder columns
    dplyr::select(names(individual_data_template))

  return(Individual_data)

}

#' Create location data table for Wytham Woods
#'
#' @param Brood_data Brood data generated by \code{\link{create_brood_WYT}}.
#' @param Capture_data Capture data generated by \code{\link{create_capture_WYT}}.
#'
#' @return A data frame with Location data

create_location_WYT <- function(Brood_data, Capture_data){

  Location_data <- tibble::tibble(LocationID = unique(c(Brood_data$LocationID, Capture_data$LocationID)),
                                  NestboxID = unique(c(Brood_data$LocationID, Capture_data$LocationID)),
                                  LocationType = "NB",
                                  PopID = "WYT",
                                  Latitude = NA_real_,
                                  Longitude = NA_real_,
                                  StartSeason = 1947L,
                                  EndSeason = NA_integer_,
                                  HabitatType = "deciduous")

  return(Location_data)

}
