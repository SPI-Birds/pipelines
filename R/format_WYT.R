#' Construct standard summary for data from Wytham Woods, UK.
#'
#' A pipeline to produce a standard output for the nest box population in Wytham
#' Woods, UK, administered by Edward Grey Institute Oxford (Ben Sheldon).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see XXXXX
#' PLACE HOLDER!
#'
#' \strong{ExperimentID}: There are experiment codes given, I have tried to
#' adapt these to the ExperimentID categories described in the standard
#' protocol; however, these need to be checked by data owners. For now I assume:
#' - Egg manipulation (code 2) is clutch size manipulation.
#' - Chick manipulation (code 3) is cross fostering.
#' - Alter temperature in nest box (code 7) only affects phenology.
#' - Feeding manipulation (code 8) affects phenology.
#' - Altering parasites, predation, competition and territory quality (codes 9, 11, 12, 13) all affect survival.
#'
#' \strong{Species}: We include nests form blue tits, great tits, coal tits,
#' marsh tits, and nuthatches. Currently, mixed broods are treated as having no
#' species (NA), but we will fix this. There is one brood with Species 'w',
#' which I suspect is willow tit. This is currently ignored.
#'
#' \strong{AvgEggMass}: There are two columns with mass data, one is a 'legacy'
#' column. When these overlap, they can differ up to 5g! I assume the 'legacy'
#' column is less prefered (due to it's name). I only use this data where no
#' other egg mass data is provided.
#'
#' \strong{AvgChickMass}: As with AvgChickMass, there is also a 'legacy' column.
#' This is only used if the regular column is empty.
#'
#' \strong{HatchDate}: As with AvgEggMass, there is also a 'legacy' column for
#' hatch date. This is only used if the regular column is empty.
#'
#'@inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export

format_WYT <- function(db = utils::choose.dir(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       debug = FALSE){

  #Force user to select directory
  force(db)

  start <- Sys.time()

  message("Importing primary data...")

  WYT_data <- utils::read.csv(paste0(db, "/Wytham breeding data.csv"), header = T, sep = ",", stringsAsFactors = FALSE) %>%
    janitor::clean_names()

  pb <- dplyr::progress_estimated(n = nrow(WYT_data)*3)

  WYT_data <- WYT_data %>%
    #Rename columns to meet our standard format
    dplyr::mutate(BreedingSeason = year, LocationID = nestbox,
                  PopID = "WYT", Plot = toupper(section),
                  Species = dplyr::case_when(.$species == "b" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .$species == "g" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .$species == "c" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                             .$species == "n" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .$species == "m" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code),
                  LayingDate = as.Date(lay_date, format = "%d/%m/%Y"),
                  HatchDate = purrr::pmap_chr(.l = list(hatch_date, legacy_april_hatch_date),
                                              .f = ~{

                                                pb$print()$tick()

                                                if(!is.na(..1)){

                                                  return(as.character(..1))

                                                } else {

                                                  return(as.character(..2))

                                                }

                                              }),
                  NumberEggs = num_eggs_weighed,
                  AvgEggMass = purrr::pmap_dbl(.l = list(total_egg_weight, num_eggs_weighed, legacy_average_egg_weight),
                                               .f = ~{

                                                 pb$print()$tick()

                                                 if(!is.na(..1) & !is.na(..2)){

                                                   return(..1/..2)

                                                 } else {

                                                   return(..3)

                                                 }

                                               }),
                  ClutchSize = clutch_size,
                  BroodSize = num_chicks,
                  NumberFledged = num_fledglings,
                  AvgChickMass = purrr::pmap_dbl(.l = list(mean_chick_weight, legacy_mean_fledge_weight),
                                                 .f = ~{

                                                   pb$print()$tick()

                                                   if(!is.na(..1)){

                                                     return(..1)

                                                   } else {

                                                     return(..2)

                                                   }

                                                 }),
                  NumberChicksMass = num_chicks_ringed,
                  FemaleID = mother,
                  MaleID = father,
                  ExperimentID = dplyr::case_when(.$experiment_codes == "1" ~ "UNKOWN",
                                                  .$experiment_codes == "2" ~ "COHORT",
                                                  .$experiment_codes == "3" ~ "PARENTAGE",
                                                  .$experiment_codes == "7" ~ "PHENOLOGY",
                                                  .$experiment_codes == "8" ~ "PHENOLOGY",
                                                  .$experiment_codes == "9" ~ "SURVIVAL",
                                                  .$experiment_codes == "11" ~ "SURVIVAL",
                                                  .$experiment_codes == "12" ~ "SURVIVAL",
                                                  .$experiment_codes == "13" ~ "SURVIVAL"),
                  BroodID = paste(BreedingSeason, LocationID)) %>%
    #Separate out chick ids into different columns. N.B. Currently, there is only
    #the chick id column and no actual info. Therefore, it's hard to know what the
    #max possible number of columns we should include are. I just go with 22 (max
    #number of fledglings ever recorded), although this seems excessive!!
    #I'm basing all this code (e.g. sep used) on the dead chick ids columns.
    #I assume when chick numbers are provided, it will be in the same format.
    tidyr::separate(chick_ids, into = paste0("chick_", seq(1:22)), sep = " ,") %>%
    as_tibble()

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- create_brood_WYT(data = WYT_data)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Capture_data <- create_capture_WYT(WYT_data)


}

create_brood_WYT <- function(data){

  Brood_data <- data %>%
    dplyr::mutate(ClutchType_observed = NA,
                  ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE),
                  LayingDateError = NA,
                  ClutchSizeError = NA,
                  HatchDateError = NA,
                  BroodSizeError = NA,
                  FledgeDate = NA,
                  FledgeDateError = NA,
                  NumberFledgedError = NA,
                  AvgTarsus = NA,
                  NumberChicksTarsus = NA) %>%
    dplyr::select(PopID, BreedingSeason,
                  Species, Plot,
                  LocationID, BroodID,
                  FemaleID, MaleID,
                  ClutchType_observed,
                  ClutchType_calc,
                  LayingDate, LayingDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgChickTarsus, NumberChicksTarsus,
                  ExperimentID)

  return(Brood_data)

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calc <- NULL
  LayingDate <- LayingDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- NumberChicksMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

}

create_capture_WYT <- function(data){

  Capture_data <- data %>%
    dplyr::select(FemaleID, MaleID,
                  chick_1:chick_22,
                  Species, BreedingSeason,
                  LocationID, PopID, Plot) %>%
    reshape2::melt(id.vars = c("Species", "BreedingSeason",
                               "LocationID", "PopID", "Plot"),
                   value.name = "IndvID") %>%
    dplyr::filter(!is.na(IndvID) & IndvID != "") %>%
    #Keep as tibble so we can save list columns (needed for sex and age below)
    as_tibble()

  pb <- dplyr::progress_estimated(n = nrow(Capture_data))

  Capture_data <- Capture_data %>%
    dplyr::mutate(CaptureDate = NA, CaptureTime = NA,
                  CapturePopID = PopID, CapturePlot = Plot,
                  ReleasePopID = PopID, ReleasePlot = Plot,
                  Mass = NA, Tarsus = NA, WingLength = NA,
                  ChickAge = NA,
                  Sex_Age = purrr::map(.x = variable,
                                       .f = ~{

                                         pb$print()$tick()

                                         if(grepl(pattern = "Female",
                                                  x = ..1)){

                                           return(tibble(Sex = "F", Age_obsv = NA))

                                         } else if(grepl(pattern = "Male",
                                                         x = ..1)){

                                           return(tibble(Sex = "M", Age_obsv = NA))

                                         } else {

                                           return(tibble(Sex = "U", Age_obsv = 1))

                                         }

                                       })) %>%
    tidyr::unnest(cols = "Sex_Age") %>%
    calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate,
             Year = BreedingSeason, showpb = TRUE) %>%
    dplyr::select(IndvID, Species, BreedingSeason, LocationID,
                  CaptureDate:WingLength, Age_obsv,
                  Age_calculated, ChickAge)

  return(Capture_data)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_obsv <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  FemaleID <- MaleID <- chick_1 <- chick_22 <- NULL

}
