#' Construct standard format data from MTA-PE Evolutionary Ecology Group, University of Pannonia, Hungary (MTA)
#'
#' A pipeline to produce the standard format for bird study population
#' at the MTA-PE Evolutionary Ecology Group, Hungary, administered by
#' Gabor Seres & András Liker.
#' The data include 5 sites: Veszprém, Balatonfüred, Szentgál, Vilma-puszta and Gulya-domb.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{xxx}:
#'
#' \strong{Latitude, Longitude} The exact coordinates of the nestboxes are not available.
#' Data owner provided an map of the location, which can be georeferenced in the case of
#' interest or necessity of the data user. Currently, only general coordinates
#' for the location are provided.
#'
#' @inheritParams pipeline_params
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export


#### --------------------------------------------------------------------------~
#### QUESTIONS DATA OWNERS
#### There are 10 records with no nestbox indicated, could you check that?
# Broods: 244, 310, 382, 431, 547, 474, 599, 707, 974, 1097
#### Coordinates of nestboxes?
#### There are few strange male IDs ???
# "alu_gyurus"  "BPAB"  "BPAS" "father_Vi37" "gyurutlen"=="ringless"
# "A000" ??? appears a lot
#### OriginalTarsusMethod
#
#
#### QUESTIONS LIAM
#
#
#### --------------------------------------------------------------------------~

format_MTA <- function(db = choose_directory(),
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
  message("Importing primary data...")

  mta_data <- readxl::read_excel(path =  paste0(db, "/MTA_PrimaryData.xlsx"),
                                 na = c("NA", "nA")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(PopID = "MTA",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(.data$NestboxNumber),
                  BroodID = as.character(.data$BroodId),
                  LocationID = NestboxID,
                  #### Ask data owner: There are few strange IDs ???
                  # Females: "ZRKA" "mother_Vi27" "mother_Vi37" "ringless"
                  FemaleID = as.character(if_else(.data$FemaleId %in% c("gyurutlen", "ringless"),
                                                  "unringed", .data$FemaleId)),
                  # Males: "alu_gyurus"  "BPAB"  "BPAS" "father_Vi37" "gyurutlen" "ringless"
                  MaleID = as.character(if_else(.data$MaleId %in% c("gyurutlen", "ringless"),
                                                "unringed", .data$MaleId))) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$FemaleColorId,
                  -.data$FemaleId,
                  -.data$MaleColorId,
                  -.data$MaleId,
                  -.data$BroodId,
                  -.data$NestboxNumber) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  everything()) %>%
    dplyr::distinct()


  #### BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_MTA(mta_data)


  #### CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_MTA(Brood_data)


  #### INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_MTA(Capture_data)


  #### LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_MTA(mta_data)


  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")
    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MTA.csv"), row.names = F)
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

create_brood_MTA <- function(data) {

  Brood_data <-
    data %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Plot = .data$Plot,
                  ClutchType_observed = as.character(tolower(.data$ClutchTypeObserved)),
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(.data$LayingDate),
                  LayDate = as.Date(.data$LayingDate),
                  LayDate_min = as.Date(.data$LayDate - .data$LayingDateError),
                  LayDate_max = as.Date(.data$LayDate + .data$LayingDateError),
                  ClutchSize_observed = as.integer(.data$ClutchSize),
                  ClutchSize_min = ClutchSize_observed - ClutchSizeError,
                  ClutchSize_max = ClutchSize_observed + ClutchSizeError,
                  HatchDate_observed = as.Date(.data$Hatchdate),
                  HatchDate_min = as.Date(.data$HatchDate_observed - .data$HatchdateError),
                  HatchDate_max = as.Date(.data$HatchDate_observed + .data$HatchdateError),
                  BroodSize_observed = as.integer(.data$BroodSize),
                  BroodSize_min = .data$BroodSize_observed - .data$BroodSizeError,
                  BroodSize_max = .data$BroodSize_observed + .data$BroodSizeError,
                  #### Check if date of ringing may be considered as fledge date?
                  FledgeDate_observed = as.integer(.data$RingagedNestlings),
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(.data$NumberFledglings),
                  NumberFledged = as.integer(.data$NumberFledged),
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
                  ExperimentID = if_else(.data$ExperimentId == "no_experiment",
                                         NA_character_,
                                         .data$ExperimentId)) %>%
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

  return(Brood_data)

}


#### CAPTURE DATA

#' Create capture data table for blue tits in MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' Create a capture data table in standard format for blue tits in MTA-PE Evolutionary Ecology Group, Hungary.
#' @param Brood_data Data frame. Brood_data from MTA-PE Evolutionary Ecology Group, Hungary.
#' @return A data frame.
#'


create_capture_MTA <- function(data) {

  Capture_data <-
    data %>%
    dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                  .data$FemaleID, .data$MaleID,
                  .data$BroodID, .data$LocationID, .data$Plot,
                  .data$ExperimentID) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%
    dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1)) %>%
    #### Remove records where the partner is not known
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### Remove unringed individuals
    dplyr::filter(.data$IndvID != "unringed") %>%
    #### Create capture date
    dplyr::mutate(CaptureDate = as.Date(paste0(BreedingSeason, "-06-01"))) %>%
    #### Create new variables
    dplyr::group_by(IndvID) %>%
    dplyr::arrange(BreedingSeason, CaptureDate) %>%
    dplyr::mutate(CaptureID = paste(IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CaptureTime  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = CaptureAlive,
                  CapturePopID = PopID,
                  CapturePlot  = Plot,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, CapturePlot, NA_character_),
                  #### Ask data owner
                  # OriginalTarsusMethod = ifelse(!is.na(Tarsus), "Alternative", NA_character_),
                  WingLength = NA_real_,
                  Age_observed = NA_integer_,
                  ChickAge = NA_integer_,
                  BroodIDLaid = BroodID,
                  ObserverID = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  OriginalTarsusMethod = NA_character_) %>%
    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())
    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(CaptureID, IndvID, Species, Sex_observed, BreedingSeason,
                  CaptureDate, CaptureTime, ObserverID, LocationID,
                  CaptureAlive, ReleaseAlive, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge, ExperimentID)

  return(Capture_data)

}


#### INDIVIDUAL DATA

#' Create individual table for MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' Create full individual data table in standard format for data from MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' @param data Capture_data, output of create_capture_MTA function.
#'
#' @return A data frame.


create_individual_MTA <- function(data){

  Individual_data <-
    data %>%
    #### Format and create new data columns
    group_by(IndvID) %>%
    dplyr::summarise(Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex_observed))),
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
                     Species = first(Species),
                     PopID = "MTA",
                     RingSeason = min(BreedingSeason),
                     RingAge = "adult",
                     BroodIDLaid = NA_character_,
                     BroodIDFledged = BroodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge, Sex_calculated, Sex_genetic)

  return(Individual_data)

}

#### LOCATION DATA

#' Create location data table for MTA-PE, Hungary.
#'
#' Create location data table in standard format for data from MTA-PE, Hungary.
#'
#' @param data Data frame mta_data with primary data from MTA-PE, Hungary.
#'
#' @return A data frame.

create_location_MTA <- function(data) {

  Location_data <-
    data %>%
    dplyr::select(.data$BreedingSeason, .data$NestboxID, .data$PopID, .data$Site) %>%
    #### Remove cases where no nestbox is indicated >> check with data owner
    filter(!is.na(NestboxID)) %>%
    group_by(.data$NestboxID) %>%
    arrange(.data$BreedingSeason) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     LocationID = unique(.data$NestboxID),
                     PopID = unique(.data$PopID),
                     Site = unique(.data$Site)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType = case_when(.data$Site %in% c("Veszprem", "Balatonfured") ~ "urban",
                                          .data$Site %in% c("Vilma.puszta", "Szentgal_erdo") ~ "deciduous",
                                          .data$Site == "Gulya.domb" ~ NA_character_),
                  #### Only general coordinates for the location
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select(LocationID, NestboxID, LocationType, PopID,
                  Latitude, Longitude, StartSeason, EndSeason, HabitatType)

  return(Location_data)

}

