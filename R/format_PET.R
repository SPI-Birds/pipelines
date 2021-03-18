#' Construct standard format data from Institute of Biology, Karelian Research Centre, Russian Academy of Sciences, Petrozavodsk, Russia
#'
#' A pipeline to produce the standard format for bird study population
#' at the Institute of Biology, Karelian Research Centre, Russian Academy of Sciences,
#' Petrozavodsk, Russia, administered by
#' Tolstoguzov Andrey.
#'
#' Dataset contains data of 3 species: Great tit (Parus major), Pied Flycatcher (Ficedula hypoleuca),
#'  Wryneck (Jynx torquilla).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'
#' \strong{xxx}: Comment.
#'
#'
#' @inheritParams pipeline_params
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export


#### --------------------------------------------------------------------------~
#### QUESTIONS DATA OWNERS
####
# ### PARMAJ
# Information regarding the individual and capture data: capture date, measurements (weight, tarsus, method used for tarsus measurements)
# (both adults and chicks)
# Tarsus method
# Age: what do the numbers refer to? Age in years or some code (different from Euring?)
# Chicks: rings in the 2019, line V, nestbox 1: XX 84, Ring numbers: 996-85000; 801-805
#
# ### FICHYP
# individual capture data, chick measurements?
# Symbols "бк" in the column of Nestling rings?
# Age of females?
#

#### QUESTIONS LIAM
# function to extract chick rings from parmaj data

#### --------------------------------------------------------------------------~

# db = "~/Dropbox/POSTDOC/POSTDOC_NIOO_NETHERLANDS_2020/SPI-birds_project/data_pipeline_temp_copy/PET_Petrozavodsk_Russia"

format_PET <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #### Force user to select directory
  force(db)

  #### Determine species codes for filtering
  if(is.null(species)){
    species <- species_codes$Species
  }
  start_time <- Sys.time()

  #### Primary data

  message("Importing primary data Great tit ...")

  parmaj_data <- readxl::read_excel(path = paste0(db, "/PET_PrimaryData.xls"),
                                 na = c("NA", "nA"),
                                 sheet = 2) %>%  ## Note: This does not work on Mac: sheet = "Great tits"
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(PopID = "PET",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(.data$NoNestBox),
                  LocationID = .data$NestboxID,
                  BroodID = as.character(paste(.data$BreedingSeason, .data$TheLineOfArtificialNestBoxes,
                                               .data$NestboxID, .data$StartDateOfLaying1May1,
                                         sep = "_")),
                  Species = species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                  FemalesRingSeries = stringr::str_replace(.data$FemalesRingSeries,
                                                           pattern = " ", replacement = ""),
                  FemaleID = if_else(!is.na(.data$FemalesRingSeries) & !is.na(.data$FemalesRing),
                                     as.character(paste0(.data$FemalesRingSeries, .data$FemalesRing)),
                                     NA_character_),
                  MalesRingSeries = stringr::str_replace(.data$MalesRingSeries,
                                                           pattern = " ", replacement = ""),
                  MaleID = if_else(!is.na(.data$MalesRingSeries) & !is.na(.data$MalesRing),
                                     as.character(paste0(.data$MalesRingSeries, .data$MalesRing)),
                                     NA_character_),
                  SeriesOfNestlingsRings = stringr::str_replace_all(.data$SeriesOfNestlingsRings,
                                                                    pattern = " ", replacement = ""),
                  NestlingRings = stringr::str_replace_all(.data$NestlingRings,
                                                           pattern = " ", replacement = ""),
                  StartDateOfLaying1May1 = as.numeric(.data$StartDateOfLaying1May1)) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$FemalesRingSeries,
                  -.data$FemalesRing,
                  -.data$MalesRingSeries,
                  -.data$MalesRing,
                  -.data$NoString,
                  -.data$NoNestBox,
                  -.data$NestedEarlier18,
                  -.data$NestedEarlier23) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  everything()) %>%
    dplyr::distinct()


  message("Importing primary data Pied flycatcher ...")

  fichyp_data <- readxl::read_excel(path =  paste0(db, "/PET_PrimaryData.xls"),
                                    na = c("NA", "nA"),
                                    sheet = 1) %>% ## Note: This does not work on Mac: sheet = "Pied flycatcher"
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(PopID = "PET",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(.data$NoNestBox),
                  LocationID = .data$NestboxID,
                  BroodID = as.character(paste(.data$BreedingSeason, .data$TheLineOfArtificialNestBoxes,
                                               .data$NestboxID, .data$StartDateOfLaying1May1,
                                               sep = "_")),
                  Species = species_codes[which(species_codes$SpeciesID == 13490), ]$Species,
                  FemaleID = if_else(!is.na(.data$FemalesRingSeries) & !is.na(.data$FemalesRing),
                                     as.character(paste0(.data$FemalesRingSeries, .data$FemalesRing)),
                                     NA_character_),
                  MaleID = NA_character_,
                  NestlingRings = stringr::str_replace_all(.data$NestlingRings,
                                                           pattern = " ", replacement = ""),
                  StartDateOfLaying1May1 = as.integer(.data$StartDateOfLaying1May1),
                  #### In two cases the hatching date is "0"
                  HatchingDate1May1 = if_else(HatchingDate1May1 == "0",
                                              NA_integer_,
                                              as.integer(.data$HatchingDate1May1))) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$FemalesRingSeries,
                  -.data$FemalesRing,
                  -.data$NoString,
                  -.data$NoNestBox) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  everything()) %>%
    dplyr::distinct()


  message("Importing primary data Wryneck ...")

  jyntor_data <- readxl::read_excel(path =  paste0(db, "/PET_PrimaryData.xls"),
                                    na = c("NA", "nA"),
                                    sheet = 3) %>%  ## Note: This does not work on Mac: sheet = "Wryneck"
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(PopID = "PET",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(.data$NoNestBox),
                  LocationID = .data$NestboxID,
                  BroodID = as.character(paste(.data$BreedingSeason, .data$TheLineOfArtificialNestBoxes,
                                               .data$NestboxID, .data$StartDateOfLaying1May1,
                                               sep = "_")),
                  #### ADD WRYNECK TO SPECIES CODES !!!
                  # Species = species_codes[which(species_codes$SpeciesID == XXX), ]$Species,
                  Species = "JYNTOR",
                  FemalesRingSeries = stringr::str_replace(.data$FemalesRingSeries,
                                                           pattern = " ", replacement = ""),
                  FemaleID = if_else(!is.na(.data$FemalesRingSeries) & !is.na(.data$FemalesRing),
                                     as.character(paste0(.data$FemalesRingSeries, .data$FemalesRing)),
                                     NA_character_),
                  MaleID = NA_character_,
                  NestlingRings = stringr::str_replace_all(.data$NestlingRings,
                                                           pattern = " ", replacement = ""),
                  StartDateOfLaying1May1 = as.numeric(.data$StartDateOfLaying1May1)) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$FemalesRingSeries,
                  -.data$FemalesRing,
                  -.data$NoString,
                  -.data$NoNestBox) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  everything()) %>%
    dplyr::distinct()



  #### BROOD DATA

  message("Compiling brood information...")
  Brood_data <- create_brood_PET(pet_data)


  #### CAPTURE DATA

  message("Compiling capture information...")
  Capture_data <- create_capture_PET(Brood_data)


  #### INDIVIDUAL DATA

  message("Compiling individual information...")
  Individual_data <- create_individual_PET(Capture_data)


  #### LOCATION DATA

  message("Compiling location information...")
  Location_data <- create_location_PET(pet_data)


  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  #### EXPORT DATA

  if(output_type == "csv"){
    message("Saving .csv files...")
    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PET.csv"), row.names = F)
    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_PET.csv"), row.names = F)
    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PET.csv"), row.names = F)
    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PET.csv"), row.names = F)
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


#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~

#### BROOD DATA

#' Create brood data table for the populations from Institute of Biology, Karelian Research Centre, Russian Academy of Sciences, Petrozavodsk, Russia.
#'
#' Create a brood data table in standard format for great tits, pied flycatchers and Wryneck in Institute of Biology, Russia.
#' @param pet_data Data frame. Primary data from Institute of Biology, Russia.
#' @return A data frame.


create_brood_PET <- function(parmaj_data,
                             fichyp_data,
                             jyntor_data) {

  Brood_data_parmaj <-
    parmaj_data %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$RingDate1May1St - 1,
                  LayDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$StartDateOfLaying1May1 - 1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed =  as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                                format = "%Y-%m-%d") + .data$HatchingDate1May1 - 1,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = NA_integer_,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberOfRingedFledlings),
                  NumberFledged = as.integer(.data$NumberOfRingedFledlings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_,
                  NumberChicksTarsus = NA_integer_,
                  #### Ask data owner for the tarsus method
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  ClutchType_observed = NA_character_) %>%
    #### Calculate clutch type
    # dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #### Rename
    dplyr::rename(LayDate_observed = LayDate,
                  NumberFledged_observed = NumberFledged) %>%
    #### Final arrangement
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate_observed, LayDate_min, LayDate_max,
                  ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                  HatchDate_observed, HatchDate_min, HatchDate_max,
                  BroodSize_observed, BroodSize_min, BroodSize_max,
                  FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                  NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                  AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID) %>%
    distinct()


  Brood_data_fichyp <-
    fichyp_data %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$RingDate1May1St - 1,
                  LayDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$StartDateOfLaying1May1 - 1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed =  as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                                format = "%Y-%m-%d") + .data$HatchingDate1May1 - 1,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$NumberOfHatchedNestlings),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberOfRingedFledlings),
                  NumberFledged = as.integer(.data$NumberOfFledlings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_,
                  NumberChicksTarsus = NA_integer_,
                  #### Ask data owner for the tarsus method
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  ClutchType_observed = NA_character_) %>%
    #### Calculate clutch type
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #### Rename
    dplyr::rename(LayDate_observed = LayDate,
                  NumberFledged_observed = NumberFledged) %>%
    #### Final arrangement
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate_observed, LayDate_min, LayDate_max,
                  ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                  HatchDate_observed, HatchDate_min, HatchDate_max,
                  BroodSize_observed, BroodSize_min, BroodSize_max,
                  FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                  NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                  AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID) %>%
    distinct()


  Brood_data_jyntor <-
    jyntor_data %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$RingDate1May1St - 1,
                  LayDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$StartDateOfLaying1May1 - 1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed =  as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                                format = "%Y-%m-%d") + .data$HatchingDate1May1 - 1,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$NumberOfHatchedNestlings),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberOfRingedFledlings),
                  NumberFledged = as.integer(.data$NumberOfFledlings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_,
                  NumberChicksTarsus = NA_integer_,
                  #### Ask data owner for the tarsus method
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  ClutchType_observed = NA_character_) %>%
    #### Calculate clutch type
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #### Rename
    dplyr::rename(LayDate_observed = LayDate,
                  NumberFledged_observed = NumberFledged) %>%
    #### Final arrangement
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate_observed, LayDate_min, LayDate_max,
                  ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                  HatchDate_observed, HatchDate_min, HatchDate_max,
                  BroodSize_observed, BroodSize_min, BroodSize_max,
                  FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                  NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                  AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID) %>%
    distinct()


  Brood_data <-
    bind_rows(Brood_data_parmaj,
              Brood_data_fichyp,
              Brood_data_jyntor)

  return(Brood_data)

}


#### CAPTURE DATA

#' Create capture data table for the populations from Institute of Biology, Karelian Research Centre, Russian Academy of Sciences, Petrozavodsk, Russia.
#'
#' Create a capture data table in standard format for great tits, pied flycatchers and Wryneck in Institute of Biology, Russia.
#' @param Brood_data Data frame. Brood_data from Institute of Biology, Russia.
#' @return A data frame.


create_capture_PET <- function(parmaj_data,
                               fichyp_data,
                               jyntor_data) {

  #### ADULT DATA
  #### ------------------------------------------------------------------------~

  Capture_data_parmaj_adults <-
    parmaj_data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$FemaleID, .data$MaleID,
                  .data$FemalesAge, .data$MalesAge,
                  .data$BroodID, .data$LocationID,
                  .data$TheLineOfArtificialNestBoxes,
                  .data$StartDateOfLaying1May1) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1),
                  Age = case_when(.data$Sex_observed == "F" ~ as.integer(.data$FemalesAge),
                                  .data$Sex_observed == "M" ~ as.integer(.data$MalesAge))) %>%
    #### Remove records where the partner is not known
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### ASK DATA OWNER >> til they respond, create capture date
    dplyr::mutate(CaptureDate = as.Date(paste0(.data$BreedingSeason, "-04-01"))) %>%
    #### Create new variables
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$StartDateOfLaying1May1) %>% ## Use .data$CaptureDate if the data owner provides one. Now use StartDateOfLaying1May1.
    dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  CaptureTime  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  WingLength = NA_real_,
                  ChickAge = NA_integer_,
                  BroodIDLaid = .data$BroodID,
                  ObserverID = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  #### Ask data owner
                  # OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  #### Ask data owner, this is only a temporary solution
                  Age_observed = case_when(.data$Age == 1L ~ 5L,
                                           .data$Age == 2L ~ 7L)) %>%
    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())
    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(.data$CaptureID, .data$IndvID, .data$Species,
                  .data$Sex_observed, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID)


  Capture_data_fishyp_adults <-
    fichyp_data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$FemaleID, .data$MaleID,
                  .data$BroodID, .data$LocationID,
                  .data$TheLineOfArtificialNestBoxes,
                  .data$StartDateOfLaying1May1) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    #### Remove records where the partner is not known (actually, there are no MaleID for this dataset)
    dplyr::filter(!is.na(.data$IndvID)) %>%
    ####
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1)) %>%
    #### ASK DATA OWNER >> til they respond, create capture date
    dplyr::mutate(CaptureDate = as.Date(paste0(.data$BreedingSeason, "-04-01"))) %>%
    #### Create new variables
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$StartDateOfLaying1May1) %>% ## Use .data$CaptureDate if the data owner provides one. Now use StartDateOfLaying1May1.
    dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  CaptureTime  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  WingLength = NA_real_,
                  ChickAge = NA_integer_,
                  BroodIDLaid = .data$BroodID,
                  ObserverID = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  #### Ask data owner
                  # OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  #### Ask dataowner
                  Age_observed = NA_integer_) %>%
    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())
    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(.data$CaptureID, .data$IndvID, .data$Species,
                  .data$Sex_observed, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID)


  Capture_data_jyntor_adults <-
    jyntor_data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$FemaleID, .data$MaleID,
                  .data$BroodID, .data$LocationID,
                  .data$TheLineOfArtificialNestBoxes,
                  .data$StartDateOfLaying1May1) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1),
                  Age = NA_character_) %>%
    #### Remove records where the partner is not known
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### ASK DATA OWNER >> til they respond, create capture date
    dplyr::mutate(CaptureDate = as.Date(paste0(.data$BreedingSeason, "-04-01"))) %>%
    #### Create new variables
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$StartDateOfLaying1May1) %>% ## Use .data$CaptureDate if the data owner provides one. Now use StartDateOfLaying1May1.
    dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                  CaptureTime  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  WingLength = NA_real_,
                  ChickAge = NA_integer_,
                  BroodIDLaid = .data$BroodID,
                  ObserverID = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  #### Ask data owner
                  # OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_,
                  #### Ask data owner, this is only a temporary solution
                  Age_observed = case_when(.data$Age == 1L ~ 5L,
                                           .data$Age == 2L ~ 7L)) %>%
    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())
    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(.data$CaptureID, .data$IndvID, .data$Species,
                  .data$Sex_observed, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID)



  #### CHICK DATA PARMAJ
  #### ------------------------------------------------------------------------~
  #### solve brood 2019_V_1_1 ring number series change

  Capture_data_parmaj_chicks_temp <-
    parmaj_data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$BroodID, .data$LocationID,
                  .data$TheLineOfArtificialNestBoxes,
                  .data$StartDateOfLaying1May1,
                  .data$SeriesOfNestlingsRings, .data$NestlingRings) %>%
    dplyr::filter(!is.na(.data$SeriesOfNestlingsRings) & !is.na(.data$NestlingRings)) %>%

    #### SOLVE THIS, TEMP REMOVE
    dplyr::filter(BroodID != "2019_V_1_1") %>%

    #### Separate series and ring codes
    tidyr::separate(col = .data$SeriesOfNestlingsRings, into = c("series1", "series2"),
                    sep = ";", remove = FALSE) %>% #OK
    tidyr::separate(col = .data$NestlingRings, into = c("rings1", "rings2"),
                    sep = ";", remove = FALSE) %>% #OK
    tidyr::separate(col = .data$rings1, into = c("rings1_start", "rings1_end"),
                    sep = "-", remove = FALSE, convert = TRUE) %>%
    tidyr::separate(col = .data$rings2, into = c("rings2_start", "rings2_end"),
                    sep = "-", remove = FALSE, convert = TRUE)

  #### -----------------------------------
  #### Get the individual rings per brood
  #### -----------------------------------

  #### Series 1 of rings
  #### -----------------------------------
  seqs_rings1 <- mapply(FUN = function(a, b) {
    (seq(from = a, to = b, by = 1))},
    a = Capture_data_parmaj_chicks_temp$rings1_start,
    b = Capture_data_parmaj_chicks_temp$rings1_end)

  names(seqs_rings1) <- Capture_data_parmaj_chicks_temp$BroodID

  #### Transform to data frame
  seqs_rings1_tib <-
    seqs_rings1 %>%
    enframe(name = "BroodID", value = "Ring") %>%
    unnest(cols = c(Ring))

  captures_parmaj_chicks_rings1 <-
    Capture_data_parmaj_chicks_temp %>%
    full_join(seqs_rings1_tib, by = "BroodID") %>%
    dplyr::mutate(IndvID = paste0(series1, Ring))

  #### Series 2 of rings
  #### -----------------------------------
  #### Get numbers of ring series
  seqs_rings2 <- mapply(FUN = function(a, b) {
    (seq(from = a, to = b, by = 1))},
    a = Capture_data_parmaj_chicks_temp$rings2_start[!is.na(Capture_data_parmaj_chicks_temp$rings2)],
    b = Capture_data_parmaj_chicks_temp$rings2_end[!is.na(Capture_data_parmaj_chicks_temp$rings2)])

  names(seqs_rings2) <- Capture_data_parmaj_chicks_temp$BroodID[!is.na(Capture_data_parmaj_chicks_temp$rings2)]

  #### Transform to data frame
  seqs_rings2_tib <-
    seqs_rings2 %>%
    enframe(name = "BroodID", value = "Ring") %>%
    unnest(cols = c(Ring))

  captures_parmaj_chicks_rings2 <-
    Capture_data_parmaj_chicks_temp %>%
    inner_join(seqs_rings2_tib, by = "BroodID") %>%
    dplyr::mutate(IndvID = case_when(!is.na(series1) & is.na(series2) ~ paste0(series1, Ring),
                                     !is.na(series1) & !is.na(series2) ~ paste0(series2, Ring)))

  #### Join both series rings
  Capture_data_parmaj_chicks <-
    bind_rows(captures_parmaj_chicks_rings1,
              captures_parmaj_chicks_rings2) %>%
  #### ASK DATA OWNER >> til they respond, create capture date
  dplyr::mutate(CaptureDate = as.Date(paste0(.data$BreedingSeason, "-04-01")),
                Sex_observed = NA_character_,
                Age = 1L) %>%
  #### Create new variables
  dplyr::group_by(.data$IndvID) %>%
  dplyr::arrange(.data$BreedingSeason, .data$StartDateOfLaying1May1) %>% ## Use .data$CaptureDate if the data owner provides one. Now use StartDateOfLaying1May1.
  dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Plot = .data$TheLineOfArtificialNestBoxes,
                CaptureTime  = NA_character_,
                CaptureAlive = TRUE,
                ReleaseAlive = .data$CaptureAlive,
                CapturePopID = .data$PopID,
                CapturePlot  = .data$Plot,
                ReleasePopID = ifelse(ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                ReleasePlot  = ifelse(ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                WingLength = NA_real_,
                ChickAge = NA_integer_,
                BroodIDLaid = .data$BroodID,
                ObserverID = NA_character_,
                Mass = NA_real_,
                Tarsus = NA_real_,
                #### Ask data owner
                # OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                OriginalTarsusMethod = NA_character_,
                ExperimentID = NA_character_,
                #### Ask data owner, this is only a temporary solution
                Age_observed = 5L) %>%
  #### USE THE NEW VERSION OF THE FUNCTION
  # dplyr::mutate(Age_calculated = calc_age())
  #### OLD VERSION OF THE FUNCTION
  calc_age(ID = IndvID,
           Age = Age_observed,
           Date = CaptureDate,
           Year = BreedingSeason,
           showpb = TRUE) %>%
  #### Final arrangement
  dplyr::select(.data$CaptureID, .data$IndvID, .data$Species,
                .data$Sex_observed, .data$BreedingSeason,
                .data$CaptureDate, .data$CaptureTime,
                .data$ObserverID, .data$LocationID,
                .data$CaptureAlive, .data$ReleaseAlive,
                .data$CapturePopID, .data$CapturePlot,
                .data$ReleasePopID, .data$ReleasePlot,
                .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                .data$WingLength, .data$Age_observed, .data$Age_calculated,
                .data$ChickAge, .data$ExperimentID)


  #### CHICK DATA FICHYP
  #### ------------------------------------------------------------------------~
  #### solve rings "бк" ring number series change

  #### This ring is probably wrong?? is too long VT76392096

  Capture_data_fichyp_chicks_temp <-
    fichyp_data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$BroodID, .data$LocationID,
                  .data$TheLineOfArtificialNestBoxes,
                  .data$StartDateOfLaying1May1,
                  .data$NestlingRings) %>%
    dplyr::filter(!is.na(.data$NestlingRings)) %>%

    #### SOLVE THIS, TEMP REMOVE
    dplyr::filter(NestlingRings != "бк") %>%
    dplyr::filter(NestlingRings != "VT76-260-63") %>%


    #### Separate series and ring codes
    tidyr::separate(col = .data$NestlingRings, into = c("rings1", "rings2"),
                    sep = ";", remove = FALSE) %>%
    tidyr::separate(col = .data$rings1, into = c("rings1_start", "rings1_end"),
                    sep = "-", remove = FALSE, convert = TRUE) %>%
    dplyr::mutate(series1 = str_sub(rings1_start, 1, nchar(rings1_start)-nchar(rings1_end)),
                  rings1_start = str_sub(rings1_start, nchar(series1)+1, nchar(rings1_start))) %>%
    tidyr::separate(col = .data$rings2, into = c("rings2_start", "rings2_end"),
                    sep = "-", remove = FALSE, convert = TRUE) %>%
    dplyr::mutate(series2 = str_sub(rings2_start, 1, nchar(rings2_start)-nchar(rings2_end)),
                  rings2_start = str_sub(rings2_start, nchar(series2)+1, nchar(rings2_start)))

  #### -----------------------------------
  #### Get the individual rings per brood
  #### -----------------------------------

  #### Series 1 of rings
  #### -----------------------------------
  seqs_rings1 <- mapply(FUN = function(a, b) {
    (seq(from = a, to = b, by = 1))},
    a = Capture_data_fichyp_chicks_temp$rings1_start[!is.na(Capture_data_fichyp_chicks_temp$series1)],
    b = Capture_data_fichyp_chicks_temp$rings1_end[!is.na(Capture_data_fichyp_chicks_temp$series1)])

  names(seqs_rings1) <- Capture_data_fichyp_chicks_temp$BroodID[!is.na(Capture_data_fichyp_chicks_temp$series1)]

  #### Transform to data frame
  seqs_rings1_tib <-
    seqs_rings1 %>%
    enframe(name = "BroodID", value = "Ring") %>%
    unnest(cols = c(Ring))

  captures_fichyp_chicks_rings1 <-
    Capture_data_fichyp_chicks_temp %>%
    full_join(seqs_rings1_tib, by = "BroodID") %>%
    dplyr::mutate(IndvID = case_when(!is.na(series1) ~ paste0(series1, Ring),
                                     is.na(series1) ~ paste0(rings1)))


  #### Series 2 of rings
  #### -----------------------------------
  #### Get numbers of ring series
  seqs_rings2 <- mapply(FUN = function(a, b) {
    (seq(from = a, to = b, by = 1))},
    a = Capture_data_fichyp_chicks_temp$rings2_start[!is.na(Capture_data_fichyp_chicks_temp$series2)],
    b = Capture_data_fichyp_chicks_temp$rings2_end[!is.na(Capture_data_fichyp_chicks_temp$series2)])

  names(seqs_rings2) <- Capture_data_fichyp_chicks_temp$BroodID[!is.na(Capture_data_fichyp_chicks_temp$series2)]

  #### Transform to data frame
  seqs_rings2_tib <-
    seqs_rings2 %>%
    enframe(name = "BroodID", value = "Ring") %>%
    unnest(cols = c(Ring))

  captures_fichyp_chicks_rings2 <-
    Capture_data_fichyp_chicks_temp %>%
    inner_join(seqs_rings2_tib, by = "BroodID") %>%
    dplyr::mutate(IndvID = paste0(series2, Ring))


# here join both and finish






  #### ------------------------
  #### FINAL CAPTURE DATA
  #### ------------------------
  Capture_data <- bind_rows(Capture_data_parmaj_adults,
                            # Capture_data_parmaj_chicks,
                            # Capture_data_fishyp_chicks,
                            Capture_data_fishyp_adults,
                            Capture_data_jyntor_adults)

  return(Capture_data)
}


#### INDIVIDUAL DATA

#' Create individual table for Institute of Biology, Russia.
#'
#' Create full individual data table in standard format for data from Institute of Biology, Russia.
#' @param Capture_data Data frame, output of create_capture_PET function.
#' @return A data frame.


create_individual_PET <- function(Capture_data){


  ### solve the broodId >> get from input/Brood data
  ### get for all species

  Individual_data_ <-
    Capture_data %>%
    #### Format and create new data columns
    group_by(.data$IndvID) %>%
    dplyr::summarise(Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(.data$Sex_observed))),
                                                     .f = ~{
                                                       if(length(..1) == 0){
                                                         return(NA_character_)
                                                       } else if(length(..1) == 1){
                                                         return(..1)
                                                       } else {
                                                         return("C")
                                                       }
                                                     }),
                     Sex_genetic = NA_character_,
                     Species = first(.data$Species),
                     PopID = first(.data$CapturePopID),
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = if_else(.data$Age_observed == 1L, "chick", "adult"),
                     BroodIDLaid = if_else(.data$Age_observed == 1L, .data$BroodID, NA_character_),
                     BroodIDFledged = .data$BroodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_calculated, .data$Sex_genetic)

  return(Individual_data)

}


#### LOCATION DATA

#' Create location data table for for the populations from Institute of Biology, Karelian Research Centre, Russian Academy of Sciences, Petrozavodsk, Russia.
#'
#' Create location data table in standard format for data from Institute of Biology, Russia.
#' @param data Data frame pet_data with primary data from Institute of Biology, Russia.
#' @return A data frame.


create_location_PET <- function(pet_data) {

  Location_data <-
    bind_rows(dplyr::select(.data = parmaj_data,
                            .data$BreedingSeason, .data$NestboxID,
                            .data$PopID, .data$TheLineOfArtificialNestBoxes),
              dplyr::select(.data = fichyp_data,
                            .data$BreedingSeason, .data$NestboxID,
                            .data$PopID, .data$TheLineOfArtificialNestBoxes),
              dplyr::select(.data = jyntor_data,
                            .data$BreedingSeason, .data$NestboxID,
                            .data$PopID, .data$TheLineOfArtificialNestBoxes)) %>%
    #### Remove cases where no nestbox is indicated
    filter(!is.na(.data$NestboxID)) %>%
    group_by(.data$TheLineOfArtificialNestBoxes, .data$NestboxID) %>%
    arrange(.data$BreedingSeason) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     #### Not sure about this
                     LocationID = unique(.data$TheLineOfArtificialNestBoxes),
                     PopID = unique(.data$PopID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType  = NA_character_, ### it is a botanical garden with pines and deciduous trees...
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select(.data$LocationID, .data$NestboxID,
                  .data$LocationType, .data$PopID,
                  .data$Latitude, .data$Longitude,
                  .data$StartSeason, .data$EndSeason, .data$HabitatType) %>%
    distinct()


  return(Location_data)

}
