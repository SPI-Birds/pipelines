##' Construct standard format for data from Kilingi Nomme, Estonia.
#'
#' A pipeline to produce the standard format for the great tit population
#' in Kilingi Nomme, Estonia, administered by Agu Leivits (until 1995) and
#' Raivo Mänd, Vallo Tilgar, Marko Mägi and Jaanis Lodjak (from 1995).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'
#'\strong{BroodID}: BroodID has two formats in the final Brood_data. In the original primary data
#' until year 1992, the brood has individual numerical ID. In the original primary data from data
#' after the year 1995, there is no BroodID - therefore we create one using the "Breeding season_NestboxID_BreedingAttempt".
#'
#'\strong{CaptureDate}: Date of capture for the primary data until the year 1992 are not available in the digitized format,
#'therefore the CaptureDate was artificially created as the 1st of May of the correspondent breeding year ("BreedingYear-05-01").
#'
#'\strong{LayDate}: In the primary data, laying date is the date on which the first egg of a clutch is laid.
#'Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.Use negative numbers for laying dates in March.
#'
#'\strong{HatchDate_observed}: In the primary data (only data after 1995), hatch date is expressed as days after 30 of April: 1 = May 1st, 31 = June 1st, etc.
#'
#'\strong{AvgTarsus}: Only available for data after 1995, where the measurements at 15 days old chicks are used.
#'
#'\strong{ChickAge}: Note from data owner for the data until the 1992: Chicks ringed from age of 7-6 day up to fledging, most frequently in age of 8-10 day.
#'We used 8 days.
#'
#'\strong{Location_data}: Coordinates of nestboxes can be provided by the data owner, we do not have them at this point.
#'
#' @inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#' @export

format_KIL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  #Avoid with scientific notation (creates unusable IDs)
  original_options <- options(scipen = 200)

  #Make sure we revert back to original scientific notation afterwards
  #Otherwise, we are overwriting the users local settings
  on.exit(options(original_options), add = TRUE, after = FALSE)

  ## TODO: If there are multiple option/setup changes we need to make, this could be in
  # a function (e.g. pipeline_setup)

  #Force user to select directory
  force(db)

  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()


  #### Primary data
  # There are 2 excel files for data before 1992 and other for data after 1995.
  # The file before 1992 contains separate sheets with BroodData, AdultData, NestlingData.
  # The file after 1995 contains only one sheet, other file from 1995 includes data
  # about nestlings.

  #### Set R option for when to use scientific notation
  options(scipen = 100)

  message("Importing primary data ... 1/4")

  #### Load brood data before 1992
  brood_data_til92 <-
    readxl::read_excel(path = paste0(db, "/KIL_PrimaryData_to1992.xlsx"), sheet = "BroodData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(Species = species_codes[which(species_codes$speciesEURINGCode == 14640), ]$Species,
                  PopID = "KIL",
                  BreedingSeason = as.integer(.data$Year),
                  LocationID = tolower(.data$NestboxId)) %>%
    dplyr::select(-"Year",
                  -"NestboxId")


  message("Importing primary data ... 2/4")

  #### Load adult data before 1992
  adults_data_til92 <-
    readxl::read_excel(path = paste0(db, "/KIL_PrimaryData_to1992.xlsx"), sheet = "AdultData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(IndvID  = as.character(.data$AdultId),
                  Species = species_codes[which(species_codes$speciesEURINGCode == 14640), ]$Species,
                  PopID = "KIL",
                  BreedingSeason = .data$RingDate,
                  Age = tolower(.data$Age)) %>%
    dplyr::select(-"AdultId",
                  -"RingDate")


  message("Importing primary data ... 3/4")

  #### Load chick data before 1992
  chicks_data_til92 <-
    readxl::read_excel(path = paste0(db, "/KIL_PrimaryData_to1992.xlsx"), sheet = "NestlingData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(IndvID  = as.character(.data$NestlingRingNumber),
                  Species = species_codes[which(species_codes$speciesEURINGCode == 14640), ]$Species,
                  PopID = "KIL",
                  BreedingSeason = .data$RingDate) %>%
    dplyr::select(-"NestlingRingNumber",
                  -"RingDate")

  message("Importing primary data ... 4/4")

  #### Load brood data after 1995
  kil_data_95 <-
    readxl::read_excel(path = paste0(db, "/KIL_PrimaryData_from1995_GT.xlsx"), sheet = 1) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Species = species_codes[which(species_codes$speciesEURINGCode == 14640), ]$Species,
                  PopID = "KIL",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = tolower(.data$NestId),
                  FemaleID = as.character(.data$FemaleRingNo),
                  MaleID = as.character(.data$MaleRingNo),
                  BreedingAttempt = dplyr::case_when(.data$Brood == "First" ~ 1L,
                                                     .data$Brood == "Second" ~ 2L),
                  BroodID = paste(.data$Year, .data$NestboxID, .data$BreedingAttempt,
                                  sep = "_")) %>%
    #### Reorder columns
    dplyr::select("BreedingSeason",
                  "Species",
                  "PopID",
                  tidyselect::everything(),
                  -"NestId",
                  -"FemaleRingNo",
                  -"MaleRingNo",
                  -"Year")

  ### Prepare info about individuals/captures from data from 1995
  adults_data_95 <- kil_data_95 %>%
    dplyr::select("Species", "PopID", "BreedingSeason",
                  "FemaleID", "MaleID", "BroodID", "NestboxID",
                  "FemaleAgeEuringCode", "FemaleCatchDate1May1St",
                  "MaleAgeEuringCode", "MaleCatchDate1May1St",
                  "AdultFemaleTarsusMm", "AdultMaleTarsusMm",
                  "AdultFemaleMassG", "AdultMaleMassG",
                  "AdultFemaleWingMm", "AdultMaleWingMm") %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1),
                  Tarsus = dplyr::case_when(.data$Sex_observed == "F" ~ AdultFemaleTarsusMm,
                                            .data$Sex_observed == "M" ~ AdultMaleTarsusMm),
                  Mass = dplyr::case_when(.data$Sex_observed == "F" ~ AdultFemaleMassG,
                                          .data$Sex_observed == "M" ~ AdultMaleMassG),
                  WingLength = dplyr::case_when(.data$Sex_observed == "F" ~ AdultFemaleWingMm,
                                                .data$Sex_observed == "M" ~ AdultMaleWingMm),
                  CatchDate1May1St = dplyr::case_when(.data$Sex_observed == "F" ~ FemaleCatchDate1May1St,
                                                      .data$Sex_observed == "M" ~ MaleCatchDate1May1St),
                  CaptureDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                        format = "%Y-%m-%d") + .data$CatchDate1May1St - 1,
                  CaptureTime = NA_character_,
                  AgeEuringCode = dplyr::case_when(.data$Sex_observed == "F" ~ as.integer(FemaleAgeEuringCode),
                                                   .data$Sex_observed == "M" ~ as.integer(MaleAgeEuringCode))) %>%
    #### Remove records where the partner is not known
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::select(-"AdultFemaleTarsusMm",
                  -"AdultMaleTarsusMm",
                  -"AdultFemaleMassG",
                  -"AdultMaleMassG",
                  -"AdultFemaleWingMm",
                  -"AdultMaleWingMm",
                  -"FemaleCatchDate1May1St",
                  -"MaleCatchDate1May1St",
                  -"FemaleAgeEuringCode",
                  -"MaleAgeEuringCode")


  #### Load data after 1995 chicks
  chicks_data_95 <-
    readxl::read_excel(path = paste0(db, "/KIL_PrimaryData_from1995_GT_nestlings.xls"),
                       sheet = 1,
                       col_types = c("numeric", "text", "numeric",
                                     "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "text", "numeric")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(NestlingTarsus15D = stringr::str_replace(.data$NestlingTarsus15D,
                                                           pattern = ",",
                                                           replacement = ".")) %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Species = species_codes[which(species_codes$speciesEURINGCode == 14640), ]$Species,
                  PopID   = "KIL",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = tolower(.data$NestId),
                  BroodID = paste(.data$Year, .data$NestboxID, .data$BreedingAttempt,
                                  sep = "_"),
                  IndvID = as.character(.data$NestlingRingNo),
                  CaptureDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                     format = "%Y-%m-%d") + .data$RingDate1May1St - 1,
                  CaptureTime = dplyr::if_else(!is.na(.data$RingTime),
                                               (stringr::str_replace(sprintf("%05.2f", .data$RingTime),
                                                                     pattern = "\\.",
                                                                     replacement = ":")),
                                               NA_character_),
                  AgeEuringCode = 1L,
                  Mass = as.numeric(.data$NestlingMass15D),
                  Tarsus = as.numeric(.data$NestlingTarsus15D)) %>%
    dplyr::select(-"Year",
                  -"NestId",
                  -"NestlingRingNo",
                  -"RingDate",
                  -"RingDate1May1St",
                  -"RingTime")




  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_KIL(brood_data_til92,
                                 kil_data_95)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_KIL(brood_data_til92,
                                     adults_data_til92,
                                     chicks_data_til92,
                                     adults_data_95,
                                     chicks_data_95)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_KIL(adults_data_til92,
                                           chicks_data_til92,
                                           adults_data_95,
                                           chicks_data_95)

  # LOCATION DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_KIL(kil_data_95,
                                       Brood_data)


  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_KIL.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_KIL.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_KIL.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_KIL.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_KIL.txt"),
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


#' Create brood data table for Kilingi Nomme, Estonia.
#'
#' Create brood data table in the standard format (v1.1.0) for data from Kilingi Nomme, Estonia.
#'
#' @param brood_data_til92 Data frame. Primary brood data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param kil_data_95 Data frame. Primary brood data from Kilingi Nomme, Estonia from 1995 onwards.
#'
#' @return A data frame.
#'

create_brood_KIL <- function(brood_data_til92, kil_data_95) {

  #### Data from 1971 to 1992
  Brood_data_til92 <-
    brood_data_til92 %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(BroodID = as.character(.data$BroodId),
                  Plot = .data$LocationID,
                  FemaleID = as.character(.data$FemaleId),
                  MaleID = as.character(.data$MaleId),
                  ClutchType_observed = as.character(tolower(.data$ClutchType)),
                  ## for new version of calc_clutchtype
                  LayDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                                             format = "%Y-%m-%d") + .data$LayingDate - 1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed =  as.Date(NA),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = NA_integer_,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed =  as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  ## for new version of calc_clutchtype
                  NumberFledged_observed = as.integer(.data$NumberFledglings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_,
                  NumberChicksTarsus = NA_integer_,
                  #### The method used in this population is the Svensson Standard method, but
                  #### data were not recorded in earlier years
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_) %>%
    #### Even though, there are no dates for the clutch type calculation
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-"BroodId",
                  -"FemaleId",
                  -"FemaleDate",
                  -"MaleId",
                  -"MaleDate",
                  -"LayingDate",
                  -"FemaleRecruits",
                  -"MaleRecruits",
                  -"AverageMass",
                  -"AverageTarsus") %>%

    #### Final arrangement
    dplyr::select("BroodID", "PopID", "BreedingSeason", "Species",
                  "Plot", "LocationID",
                  "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min", "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min", "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
                  "AvgEggMass", "NumberEggs", "AvgChickMass", "NumberChicksMass",
                  "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod", "ExperimentID")



  #### Data from 1995
  Brood_data_95 <-
    kil_data_95 %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(LocationID = .data$NestboxID,
                  Plot = .data$LocationID,
                  ClutchType_observed = as.character(tolower(.data$Brood)),
                  #### Calculate laying date
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$LayingDate1April1St - 1,
                  LayDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$LayingDate1April1St - 1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  #### Calculate hatching date
                  HatchDate_observed = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                               format = "%Y-%m-%d") + .data$HatchDate1May1St - 1,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$NoOfHatchlings),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberFledglings),
                  NumberFledged_observed = as.integer(.data$NoOfFledglings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  #### Protocol: Average mass in grams of all chicks in the brood measured at 14 - 16 days.
                  #### Here we use the measurements at 15 days.
                  AvgChickMass = .data$NestlingMassDay15G,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = .data$NestlingTarsusDay15Mm,
                  NumberChicksTarsus = NA_integer_,
                  OriginalTarsusMethod = dplyr::if_else(!is.na(.data$AvgTarsus), "alternative", NA_character_),
                  ExperimentID = NA_character_) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%

    #### Final arrangement
    dplyr::select("BroodID", "PopID", "BreedingSeason", "Species",
                  "Plot" , "LocationID",
                  "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min", "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min", "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
                  "AvgEggMass", "NumberEggs",
                  "AvgChickMass", "NumberChicksMass",
                  "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod", "ExperimentID")


  Brood_data <-
    Brood_data_til92 %>%
    dplyr::bind_rows(Brood_data_95)

  return(Brood_data)

}



#' Create capture data table for Kilingi Nomme, Estonia.
#'
#' Create capture data table in the standard format (v1.1.0) for data from Kilingi Nomme, Estonia.
#'
#' @param brood_data_til92 Data frame. Primary brood data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param adults_data_til92 Data frame. Primary adult capture data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param chicks_data_til92 Data frame. Primary chick capture data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param adults_data_95 Data frame. Primary adult capture data from Kilingi Nomme, Estonia from 1995 onwards.
#' @param chicks_data_95 Data frame. Primary chick capture data from Kilingi Nomme, Estonia from 1995 onwards.
#'
#' @return A data frame.
#'

create_capture_KIL <- function(brood_data_til92,
                               adults_data_til92,
                               chicks_data_til92,
                               adults_data_95,
                               chicks_data_95) {

  ### Subset brood data to get location (nestbox) ID
  brood_data_til92_temp <-
    brood_data_til92 %>%
    dplyr::select("BroodId", "Species", "PopID", "BreedingSeason", "LocationID")


  #### Adults
  #### Data from 1971 to 1992
  capture_adults_til92 <-
    adults_data_til92 %>%
    dplyr::mutate(Sex_observed = .data$Sex,
                  #### Create the date, as there is no information
                  CaptureDate  = as.Date(paste0(.data$BreedingSeason, "-05-01")),
                  CaptureTime  = NA_character_,
                  ObserverID   = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  #### The method used in this population is the Svensson Standard method, but
                  #### for adults no data were recorded in earlier years
                  OriginalTarsusMethod = NA_character_,
                  Age_observed = dplyr::case_when(.data$Age == "1" ~ 5L,
                                                  .data$Age == "2" ~ 7L,
                                                  .data$Age == "3" ~ 9L,
                                                  .data$Age == "4" ~ 11L,
                                                  .data$Age == "5" ~ 13L,
                                                  .data$Age == "6" ~ 15L,
                                                  .data$Age == "7" ~ 17L,
                                                  .data$Age == "2y+" ~ 6L,
                                                  .data$Age == "3y+" ~ 8L,
                                                  .data$Age == "4y+" ~ 10L,
                                                  .data$Age == "5y+" ~ 12L,
                                                  .data$Age == "6y+" ~ 14L,
                                                  .data$Age == "7y+" ~ 16L,
                                                  .data$Age == "8y+" ~ 18L,
                                                  .data$Age == "9y+" ~ 20L),
                  ChickAge = NA_integer_,
                  ExperimentID = NA_character_) %>%
    dplyr::select(-"Age",
                  -"Sex",
                  -"Status")


  #### Chicks
  #### Data from 1971 to 1992
  capture_chicks_til92 <-
    chicks_data_til92 %>%
    dplyr::mutate(Sex_observed = .data$Sex,
                  #### Create the date, as there is no information
                  CaptureDate = as.Date(paste0(.data$BreedingSeason, "-05-01")),
                  CaptureTime = NA_character_,
                  ObserverID  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  Mass = as.numeric(.data$Mass),
                  Tarsus = convert_tarsus(as.numeric(.data$Tarsus), method = "Standard"),
                  OriginalTarsusMethod = dplyr::if_else(!is.na(.data$Tarsus), "Standard", NA_character_),
                  WingLength = NA_real_,
                  Age_observed = 1L,
                  ChickAge = 8L,
                  ExperimentID = NA_character_) %>%
    dplyr::select(-"Sex")


  #### Merge with brood data to get LocationID
  capture_til92 <-
    capture_adults_til92 %>%
    dplyr::bind_rows(capture_chicks_til92) %>%
    dplyr::left_join(brood_data_til92_temp,
                     by = c("Species", "PopID", "BreedingSeason", "BroodId")) %>%
    dplyr::select(-"BroodId")


  #### Adults (breeding pairs)
  #### Data after 1995
  capture_adults_95 <-
    adults_data_95 %>%
    dplyr::mutate(ObserverID = NA_character_,
                  LocationID = .data$NestboxID,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  OriginalTarsusMethod = dplyr::if_else(!is.na(.data$Tarsus), "alternative", NA_character_),
                  WingLength = NA_real_,
                  Age_observed = as.integer(.data$AgeEuringCode),
                  ChickAge = NA_integer_,
                  ExperimentID = NA_character_) %>%
    dplyr::select(-"CatchDate1May1St",
                  -"AgeEuringCode",
                  -"BroodID")


  #### Chicks
  #### Data after 1995
  capture_chicks_95 <-
    chicks_data_95 %>%
    dplyr::mutate(ObserverID = NA_character_,
                  LocationID = .data$NestboxID,
                  Sex_observed = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  OriginalTarsusMethod = dplyr::if_else(!is.na(.data$Tarsus), "alternative", NA_character_),
                  Age_observed = 1L,
                  ChickAge = 15L,
                  ExperimentID = NA_character_) %>%
    dplyr::select(-"NoFledglings",
                  -"NestlingMass15D",
                  -"NestlingTarsus15D",
                  -"BreedingAttempt",
                  -"AgeEuringCode",
                  -"BroodID")


  #### Join all capture data
  Capture_data <-
    capture_til92 %>%
    dplyr::bind_rows(capture_adults_95) %>%
    dplyr::bind_rows(capture_chicks_95) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$CaptureDate, .by_group = TRUE) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%

    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())

    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select("CaptureID", "IndvID", "Species", "Sex_observed", "BreedingSeason",
                  "CaptureDate", "CaptureTime", "ObserverID", "LocationID",
                  "CaptureAlive", "ReleaseAlive", "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot", "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated", "ChickAge", "ExperimentID")

  return(Capture_data)

}


#' Create individual data table for Kilingi Nomme, Estonia.
#'
#' Create individual data table in the standard format (v1.1.0) for data from Kilingi Nomme, Estonia.
#'
#' @param adults_data_til92 Data frame. Primary adult capture data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param chicks_data_til92 Data frame. Primary chick capture data from Kilingi Nomme, Estonia from 1971 to 1992.
#' @param adults_data_95 Data frame. Primary adult capture data from Kilingi Nomme, Estonia from 1995 onwards.
#' @param chicks_data_95 Data frame. Primary chick capture data from Kilingi Nomme, Estonia from 1995 onwards.
#'
#' @return A data frame.
#'

create_individual_KIL <- function(adults_data_til92,
                                  chicks_data_til92,
                                  adults_data_95,
                                  chicks_data_95) {

  #### Adults
  #### Data from 1971 to 1992
  indv_adults_til92 <-
    adults_data_til92 %>%
    dplyr::rename(RingSeason = .data$BreedingSeason) %>%
    dplyr::mutate(RingAge = "adult",
                  Sex_observed = .data$Sex,
                  BroodIDLaid = NA_character_,
                  BroodIDFledged = NA_character_) %>%
    dplyr::select(-"Status",
                  -"Sex",
                  -"Age",
                  -"BroodId") %>%
    #### Final arrangement
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_observed")


  #### Chicks
  #### Data from 1971 to 1992
  indv_chicks_til92 <-
    chicks_data_til92 %>%
    dplyr::rename(RingSeason = .data$BreedingSeason) %>%
    dplyr::mutate(RingAge = "chick",
                  BroodIDLaid = as.character(.data$BroodId),
                  BroodIDFledged = as.character(.data$BroodId),
                  Sex_observed = .data$Sex) %>%
    dplyr::select(-"Mass",
                  -"Tarsus",
                  -"Sex",
                  -"BroodId") %>%
    #### Final arrangement
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_observed")


  #### Adults
  #### Data from 1995
  indv_adults_95 <-
    adults_data_95  %>%
    dplyr::rename(RingSeason = .data$BreedingSeason) %>%
    dplyr::mutate(RingAge = "adult",
                  BroodIDLaid = NA_character_,
                  BroodIDFledged = NA_character_) %>%
    #### Final arrangement
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_observed")


  #### Chicks
  #### Data from 1995
  indv_chicks_95 <-
    chicks_data_95  %>%
    dplyr::rename(RingSeason = .data$BreedingSeason) %>%
    dplyr::mutate(RingAge = "chick",
                  BroodIDLaid = as.character(.data$BroodID),
                  BroodIDFledged = as.character(.data$BroodID),
                  Sex_observed = NA_character_) %>%
    #### Final arrangement
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_observed")


  Individual_data <-
    indv_chicks_til92 %>%
    dplyr::bind_rows(indv_adults_til92) %>%
    dplyr::bind_rows(indv_adults_95) %>%
    dplyr::bind_rows(indv_chicks_95) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$IndvID, .data$RingSeason, .by_group = TRUE) %>%
    dplyr::summarise(Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                     .f = ~{
                                                       if(length(..1) == 0){
                                                         return(NA_character_)
                                                       } else if(length(..1) == 1){
                                                         return(..1)
                                                       } else {
                                                         return("C")
                                                       }
                                                     }),
                     Species = dplyr::first(.data$Species),
                     PopID = dplyr::first(.data$PopID),
                     RingSeason = dplyr::first(.data$RingSeason),
                     RingAge = dplyr::first(.data$RingAge),
                     BroodIDLaid = dplyr::first(.data$BroodIDLaid),
                     BroodIDFledged = dplyr::first(.data$BroodIDFledged),
                     Sex_genetic = NA_character_) %>%
    dplyr::ungroup() %>%
    #### Final arrangement
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_calculated", "Sex_genetic")

  return(Individual_data)

}


#' Create location data table for Kilingi Nomme, Estonia.
#'
#' Create location data table in the standard format (v1.1.0) for data from Kilingi Nomme, Estonia.
#'
#' @param kil_data_95 Data frame. Primary brood data from Kilingi Nomme, Estonia from 1995 onwards.
#' @param Brood_data Data frame. Brood data generated by \code{\link{create_brood_KIL}}.
#'
#' @return A data frame.
#'

create_location_KIL <- function(kil_data_95, Brood_data) {

  #### Get habitat type for data after 1995
  loc_data_95 <-
    kil_data_95 %>%
    dplyr::select("BreedingSeason", "Habitat", "NestboxID", "PopID") %>%
    dplyr::mutate(NestboxID = tolower(.data$NestboxID),
                  LocationID = .data$NestboxID,
                  HabitatType = dplyr::case_when(.data$Habitat == "Deciduous forest" ~ "deciduous",
                                                 .data$Habitat == "Coniferous forest" ~ "evergreen")) %>%
    dplyr::select(-"Habitat")


  Location_data <-
    Brood_data %>%
    dplyr::select("BreedingSeason", "LocationID", "PopID") %>%
    dplyr::mutate(NestboxID = .data$LocationID) %>%
    dplyr::distinct() %>%
    dplyr::full_join(loc_data_95, by = c("BreedingSeason", "LocationID", "NestboxID", "PopID")) %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::arrange(.data$BreedingSeason, .by_group = TRUE) %>%
    dplyr::reframe(StartSeason = dplyr::first(.data$BreedingSeason),
                   #### CHECK WITH DATA OWNER
                   # EndSeason = last(.data$BreedingSeason))
                   EndSeason = NA_integer_,
                   LocationID = unique(.data$LocationID),
                   NestboxID  = unique(.data$NestboxID),
                   HabitatType = unique(.data$HabitatType),
                   PopID = unique(.data$PopID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select("LocationID", "NestboxID", "LocationType", "PopID",
                  "Latitude", "Longitude", "StartSeason", "EndSeason", "HabitatType")

  return(Location_data)

}
