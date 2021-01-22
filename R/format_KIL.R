#' Construct standard format for data from Kilingi Nomme, Estonia.
#'
#' A pipeline to produce the standard format for the great tit population
#' in Kilingi Nomme, Estonia, administered by Agu Leivits (until 1995) and
#' Raivo Mänd, Vallo Tilgar, Marko Mägi and Jaanis Lodjak (from 1995).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'
#'\strong{BroodID}: BroodID has two formats in the final Brood_data. In the original primary data
#' until year 1992, the brood has individual numerical ID. In the original primary data from data
#' from the year 1995, there is no BroodID - therefore we create one using the "Breeding season_NestboxID".
#'
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export

#### --------------------------------------------------------------------------~
#### Questions for data owners
# Confirm species "PARMAJ"
# OriginalTarsusMethod ? (both datasets)
# til1992: We assume that the BroodIDLaid is the same as BroodIDFledged, need to ask data owner.
# Capture date?
# Dates Laying and Hatching in the data from 1995 seem flipped.
#### --------------------------------------------------------------------------~

format_KIL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()


  #### Primary data
  # No general primary data, there are 2 excel files for data before 1992 and other
  # for data after 1995.
  # The file before 1992 contains separate sheets with BroodData, AdultData, NestlingData.
  # The file after 1995 contain only one sheet.

  #### Load data after 1995
  kil_data_95 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_from1995_GT.xlsx"), sheet = 1) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Species = species_codes$Species[1],  ## confirm
                  PopID   = "KIL",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = tolower(.data$NestId),
                  FemaleID = as.character(.data$FemaleRingNo),
                  MaleID = as.character(.data$MaleRingNo),
                  BroodID = paste(.data$Year, .data$NestboxID, sep = "_")) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  everything(),
                  -.data$NestId)





  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_KIL()

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_KIL()

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_KIL()

  # LOCATION DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_KIL()


  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_KIL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_KIL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_KIL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_KIL.csv"), row.names = F)

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

# Brood_data <- create_brood_KIL()

create_brood_KIL <- function() {

  #### Data from 1971 to 1992
  brood_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "BroodData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(BroodID = as.character(.data$BroodId),
                  Species = species_codes$Species[1],  ## confirm
                  PopID   = "KIL",
                  BreedingSeason = as.integer(.data$Year),
                  LocationID = tolower(.data$NestboxId),
                  Plot = .data$LocationID,
                  FemaleID = as.character(.data$FemaleId),
                  MaleID = as.character(.data$MaleId),
                  ClutchType_observed = as.character(tolower(.data$ClutchType)),
                  #### Calculate laying date
                  # Laying date is the date on which the first egg of a clutch is laid.
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  # Use negative numbers for laying dates in March.
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$LayDate - 1,
                  LayDate = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$LayingDate - 1,
                  LayDate_min = NA_character_,
                  LayDate_max = NA_character_,
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = NA,
                  HatchDate_min = NA_character_,
                  HatchDate_max = NA_character_,
                  BroodSize_observed = NA_integer_,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = NA_character_,
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberFledglings),
                  NumberFledged = as.integer(.data$NumberFledglings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA,
                  NumberChicksMass = NA_integer_,
                  AvgTarsus = NA,
                  NumberChicksTarsus = NA,
                  OriginalTarsusMethod = NA,
                  ExperimentID = NA_character_) %>%
    #### Even though, there are no dates for the clutch type calculation
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$NestboxId,
                  -.data$BroodId,
                  -.data$FemaleId,
                  -.data$FemaleDate,
                  -.data$MaleId,
                  -.data$MaleDate,
                  -.data$LayingDate,
                  -.data$Year,
                  -.data$FemaleRecruits,
                  -.data$MaleRecruits,
                  -.data$AverageMass,
                  -.data$AverageTarsus) %>%
    #### Rename
    dplyr::rename(LayDate_observed = .data$LayDate,
                  NumberFledged_observed = .data$NumberFledged) %>%
    #### Final arrangement
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason, .data$Species,
                  .data$Plot, .data$LocationID,
                  .data$FemaleID, MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed, .data$LayDate_min, .data$LayDate_max,
                  .data$ClutchSize_observed, .data$ClutchSize_min, .data$ClutchSize_max,
                  .data$HatchDate_observed, .data$HatchDate_min, .data$HatchDate_max,
                  .data$BroodSize_observed, .data$BroodSize_min, .data$BroodSize_max,
                  .data$FledgeDate_observed, .data$FledgeDate_min, .data$FledgeDate_max,
                  .data$NumberFledged_observed, .data$NumberFledged_min, .data$NumberFledged_max,
                  .data$AvgEggMass, .data$NumberEggs, .data$AvgChickMass, .data$NumberChicksMass,
                  .data$AvgTarsus, .data$NumberChicksTarsus,
                  .data$OriginalTarsusMethod, .data$ExperimentID)



  #### Data from 1995
  brood_data_95 <-
    kil_data_95 %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(LocationID = .data$NestboxID,
                  Plot = .data$LocationID,
                  ClutchType_observed = as.character(tolower(.data$Brood)),
                  #### Calculate laying date
                  # Laying date is the date on which the first egg of a clutch is laid.
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + .data$LayingDate1April1St - 1,
                  LayDate = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                                    format = "%Y-%m-%d") + .data$LayingDate1April1St - 1,
                  LayDate_min = NA_character_,
                  LayDate_max = NA_character_,
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  #### Calculate hatching date
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  HatchDate_observed = as.Date(paste(.data$BreedingSeason, "04-01", sep = "-"),
                                               format = "%Y-%m-%d") + .data$HatchDate1April1St - 1,
                  HatchDate_min = NA_character_,
                  HatchDate_max = NA_character_,
                  BroodSize_observed = as.integer(.data$NoOfHatchlings),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = NA_character_,
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberFledglings),
                  NumberFledged = as.integer(.data$NoOfFledglings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA,
                  NumberEggs = NA_integer_,
                  #### Protocol: Average mass in grams of all chicks in the brood measured at 14 - 16 days.
                  #### Here we use the measurements at 15 days.
                  AvgChickMass = .data$NestlingMassDay15G,
                  NumberChicksMass = NA_integer_,
                  #### Average tarsus length in millimeters of all chicks in the brood measured
                  #### at 14 - 16 days after hatching.
                  #### Here we use the measurements at 15 days.
                  AvgTarsus = .data$NestlingTarsusDay15Mm,
                  NumberChicksTarsus = NA,
                  #### Ask data owner
                  OriginalTarsusMethod = NA,
                  ExperimentID = NA_character_) %>%
    #### Even though, there are no dates for the clutch type calculation
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #### Rename
    dplyr::rename(LayDate_observed = .data$LayDate,
                  NumberFledged_observed = .data$NumberFledged) %>%
    #### Final arrangement
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason, .data$Species,
                  .data$Plot, .data$LocationID,
                  .data$FemaleID, MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed, .data$LayDate_min, .data$LayDate_max,
                  .data$ClutchSize_observed, .data$ClutchSize_min, .data$ClutchSize_max,
                  .data$HatchDate_observed, .data$HatchDate_min, .data$HatchDate_max,
                  .data$BroodSize_observed, .data$BroodSize_min, .data$BroodSize_max,
                  .data$FledgeDate_observed, .data$FledgeDate_min, .data$FledgeDate_max,
                  .data$NumberFledged_observed, .data$NumberFledged_min, .data$NumberFledged_max,
                  .data$AvgEggMass, .data$NumberEggs, .data$AvgChickMass, .data$NumberChicksMass,
                  .data$AvgTarsus, .data$NumberChicksTarsus,
                  .data$OriginalTarsusMethod, .data$ExperimentID)


  Brood_data <-
    brood_data_til92 %>%
    dplyr::bind_rows(brood_data_95)

  return(Brood_data)

}



#### CAPTURE DATA

reate_capture_KIL <- function() {

  ### Probably better use brood data for this

  # capture_adults

  #### Adults
  #### Data from 1971 to 1992
  adults_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "AdultData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename(RingSeason = .data$RingDate) %>%
    dplyr::mutate(IndvID = as.character(.data$AdultId),
                  Species = species_codes$Species[1],
                  RingAge = "adult",
                  PopID   = "KIL",
                  Sex_observed = .data$Sex,
                  BreedingSeason = as.integer(Year),
                  # CaptureDate
                  CaptureTime = NA_character_,
                  ObserverID = NA_character_,
                  # LocationID >>> no nestbox info, look into the brood data
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, CapturePlot, NA_character_),
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  OriginalTarsusMethod = NA_character_,
                  WingLength = NA_real_,
                  # Age_observed
                  # Age_calculated
                  # ChickAge
                  ExperimentID = NA_character_)


    # #### Create new variables
    # dplyr::group_by(IndvID) %>%
    # dplyr::arrange(Year, CaptureDate) %>%
    # dplyr::mutate(CaptureID = paste(IndvID, row_number(), sep = "_")) %>%
    # dplyr::ungroup() %>%


  # capture_chick

}


#### SOLVE: There is no date available, only the year.
#### Create fake date?
# CaptureDate = as.Date(paste0(BreedingSeason, "-05-01")),


# CaptureID
# IndvID
# Species
# Sex_observed
# BreedingSeason
# CaptureDate
# CaptureTime
# ObserverID
# LocationID
# CaptureAlive
# ReleaseAlive
# CapturePopID
# CapturePlot
# ReleasePopID
# ReleasePlot
# Mass
# Tarsus
# OriginalTarsusMethod
# WingLength
# Age_observed
# Age_calculated
# ChickAge
# ExperimentID


#### INDIVIDUAL DATA

create_individual_KIL <- function() {

  #### Adults
  #### Data from 1971 to 1992
  adults_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "AdultData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename(RingSeason = .data$RingDate) %>%
    dplyr::mutate(IndvID = as.character(.data$AdultId),
                  RingAge = "adult",
                  Species = species_codes$Species[1],
                  PopID   = "KIL",
                  Sex_observed = .data$Sex,
                  BroodIDLaid = NA_character_,
                  BroodIDFledged = NA_character_) %>%
    dplyr::select(-.data$Status,
                  -.data$AdultId,
                  -.data$Sex,
                  -.data$Age,
                  -.data$BroodId) %>%
    #### Final arrangement
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_observed)


  #### Chicks
  #### Data from 1971 to 1992
  chicks_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "NestlingData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename(RingSeason = .data$RingDate) %>%
    dplyr::mutate(IndvID = as.character(.data$NestlingRingNumber),
                  BroodIDLaid = as.character(.data$BroodId),
                  RingAge = "chick",
                  Species = species_codes$Species[1],
                  PopID   = "KIL",
                  BroodIDFledged = as.character(.data$BroodId),
                  Sex_observed = .data$Sex) %>%
    dplyr::select(-.data$Mass,
                  -.data$Tarsus,
                  -.data$NestlingRingNumber,
                  -.data$Sex,
                  -.data$BroodId) %>%
    #### Final arrangement
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_observed)


  #### We assume that the BroodIDLaid is the same as BroodIDFledged,
  #### need to ask data owner.


  #### Data from 1995
  adults_data_95 <-
    kil_data_95 %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$FemaleID, .data$MaleID) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    #### Remove records where the partner is not known
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::rename(RingSeason = BreedingSeason) %>%
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1),
                  RingAge = "adult",
                  BroodIDLaid = NA_character_,
                  BroodIDFledged = NA_character_) %>%
    #### Final arrangement
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_observed)


  Indv_data <-
    adults_data_til92 %>%
    dplyr::bind_rows(chicks_data_til92) %>%
    dplyr::bind_rows(adults_data_95) %>%
    group_by(.data$IndvID) %>%
    arrange(.data$IndvID, .data$RingSeason) %>%
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
                     Species = first(.data$Species),
                     PopID = first(.data$PopID),
                     RingSeason = first(.data$RingSeason),
                     RingAge = first(.data$RingAge),
                     BroodIDLaid = first(.data$BroodIDLaid),
                     BroodIDFledged = first(.data$BroodIDFledged),
                     Sex_genetic = NA_character_) %>%
    ungroup() %>%
    #### Final arrangement
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_calculated, .data$Sex_genetic)

  return(Indv_data)

}



#### LOCATION DATA

create_location_KIL <- function() {

  #### Get habitat type for data after 1995
  loc_data_95 <-
    kil_data_95 %>%
    dplyr::select(.data$BreedingSeason, .data$Habitat, .data$NestboxID, .data$PopID) %>%
    dplyr::mutate(NestboxID = tolower(.data$NestboxID),
                  LocationID = .data$NestboxID,
                  HabitatType = dplyr::case_when(.data$Habitat == "Deciduous forest" ~ "deciduous",
                                                 .data$Habitat == "Coniferous forest" ~ "evergreen")) %>%
    dplyr::select(-.data$Habitat)


  Location_data <-
    Brood_data %>%
    dplyr::select(.data$BreedingSeason, .data$LocationID, .data$PopID) %>%
    dplyr::mutate(NestboxID = .data$LocationID) %>%
    dplyr::full_join(loc_data_95, by = c("BreedingSeason", "LocationID", "NestboxID", "PopID")) %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::arrange(.data$BreedingSeason) %>%
    dplyr::summarise(StartSeason = first(.data$BreedingSeason),
                     #### CHECK WITH DATA OWNER
                     # EndSeason = last(.data$BreedingSeason))
                     EndSeason = NA_character_,
                     LocationID = unique(.data$LocationID),
                     NestboxID = unique(.data$NestboxID),
                     HabitatType = unique(.data$HabitatType),
                     PopID = unique(.data$PopID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  Latitude  = 58.11667,
                  Longitude = -25.08333) %>%
    #### Final arrangement
    dplyr::select(LocationID, NestboxID, LocationType, PopID,
                  Latitude, Longitude, StartSeason, EndSeason, HabitatType)

  return(Location_data)

}




