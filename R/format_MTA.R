#' Construct standard format data from MTA-PE Evolutionary Ecology Group, University of Pannonia, Hungary (MTA)
#'
#' A pipeline to produce the standard format for bird study population
#' at the MTA-PE Evolutionary Ecology Group, Hungary, administered by
#' Gabor Seres & András Liker.
#' The data include 5 sites, we treat them as separate populations:
#' Veszprém VES, Balatonfüred BAL, Szentgál SZE, Vilma-puszta VIL and Gulya-domb GUL.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' This is a temporary pipeline, as the data owners are updating their storage system.
#'
#' \strong{Individual_data}: Currently, only the information about adults is available, there is no
#' data included regarding chicks ringed.
#'
#' \strong{Capture_date}: No capture date is available, we created artificial date as 1st of April in the format "BreedingSeason-04-01"
#'
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
#### Capture date??
#### Check if date of ringing may be considered as fledge date?
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
    dplyr::mutate(PopID = case_when(.data$Site == "Balatonfured"  ~ "BAL",
                                    .data$Site == "Szentgal_erdo" ~ "SZE",
                                    .data$Site == "Vilma.puszta"  ~ "VIL",
                                    .data$Site == "Veszprem"   ~ "VES",
                                    .data$Site == "Gulya.domb" ~ "GUL"),
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(.data$NestboxNumber),
                  BroodID = as.character(.data$BroodId),
                  LocationID = .data$NestboxID,
                  Species = species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                  #### Ask data owner: There are few strange IDs ???
                  # Females: "ZRKA" "mother_Vi27" "mother_Vi37" "ringless"
                  FemaleID = as.character(if_else(.data$FemaleId %in% c("gyurutlen", "ringless",
                                                                        "alu_gyuru", "A000"),
                                                  "unringed", .data$FemaleId)),
                  # Males: "alu_gyurus"  "BPAB"  "BPAS" "father_Vi37" "gyurutlen" "ringless"
                  MaleID = as.character(if_else(.data$MaleId %in% c("gyurutlen", "ringless",
                                                                    "alu_gyurus", "A000"),
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

#' Create brood data table for great tits in MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' Create a capture data table in standard format for great tits in MTA-PE Evolutionary Ecology Group, Hungary.
#' @param mta_data Data frame. Primary data from MTA-PE Evolutionary Ecology Group, Hungary.
#' @return A data frame.

create_brood_MTA <- function(mta_data) {

  Brood_data <-
    mta_data %>%
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
    dplyr::rename(LayDate_observed = .data$LayDate,
                  NumberFledged_observed = .data$NumberFledged) %>%
    #### Final arrangement
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason, .data$Species, .data$Plot, .data$LocationID,
                  .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed, .data$LayDate_min, .data$LayDate_max,
                  .data$ClutchSize_observed, .data$ClutchSize_min, .data$ClutchSize_max,
                  .data$HatchDate_observed, .data$HatchDate_min, .data$HatchDate_max,
                  .data$BroodSize_observed, .data$BroodSize_min, .data$BroodSize_max,
                  .data$FledgeDate_observed, .data$FledgeDate_min, .data$FledgeDate_max,
                  .data$NumberFledged_observed, .data$NumberFledged_min, .data$NumberFledged_max,
                  .data$AvgEggMass, .data$NumberEggs, .data$AvgChickMass, .data$NumberChicksMass,
                  .data$AvgTarsus, .data$NumberChicksTarsus, .data$OriginalTarsusMethod, .data$ExperimentID) %>%
    distinct()

  return(Brood_data)

}


#### CAPTURE DATA

#' Create capture data table for great tits in MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' Create a capture data table in standard format for great tits in MTA-PE Evolutionary Ecology Group, Hungary.
#' @param Brood_data Data frame. Brood_data from MTA-PE Evolutionary Ecology Group, Hungary.
#' @return A data frame.


create_capture_MTA <- function(Brood_data) {

  Capture_data <-
    Brood_data %>%
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
    #### ASK DATA OWNER >> til they respond, create capture date
    dplyr::mutate(CaptureDate = as.Date(paste0(.data$BreedingSeason, "-04-01"))) %>%
    #### Create new variables
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$CaptureDate) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CaptureTime  = NA_character_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  WingLength = NA_real_,
                  Age_observed = NA_integer_,
                  ChickAge = NA_integer_,
                  BroodIDLaid = .data$BroodID,
                  ObserverID = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  #### Ask data owner
                  # OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                  OriginalTarsusMethod = NA_character_) %>%

    #### USE THE NEW VERSION OF THE FUNCTION
    # dplyr::mutate(Age_calculated = calc_age())

    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason,
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

  return(Capture_data)

}


#### INDIVIDUAL DATA

#' Create individual table for MTA-PE Evolutionary Ecology Group, Hungary.
#'
#' Create full individual data table in standard format for data from MTA-PE Evolutionary Ecology Group, Hungary.
#' @param Capture_data Data frame, output of create_capture_MTA function.
#' @return A data frame.


create_individual_MTA <- function(Capture_data){

  Individual_data <-
    Capture_data %>%
    #### Format and create new data columns
    group_by(.data$IndvID) %>%
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
                     Sex_genetic = NA_character_,
                     Species = first(.data$Species),
                     PopID = first(.data$CapturePopID),
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = "adult",
                     BroodIDLaid = NA_character_,
                     BroodIDFledged = .data$BroodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_calculated, .data$Sex_genetic)

  return(Individual_data)

}

#### LOCATION DATA

#' Create location data table for MTA-PE, Hungary.
#'
#' Create location data table in standard format for data from MTA-PE, Hungary.
#' @param mta_data Data frame mta_data with primary data from MTA-PE, Hungary.
#' @return A data frame.


create_location_MTA <- function(mta_data) {

  Location_data <-
    mta_data %>%
    dplyr::select(.data$BreedingSeason, .data$NestboxID, .data$PopID, .data$Site) %>%
    #### Remove cases where no nestbox is indicated >> check with data owner
    filter(!is.na(.data$NestboxID)) %>%
    group_by(.data$NestboxID) %>%
    arrange(.data$BreedingSeason) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     LocationID = unique(.data$NestboxID),
                     PopID = unique(.data$PopID),
                     Site = unique(.data$Site)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType = case_when(.data$PopID %in% c("VES", "BAL") ~ "urban",
                                          .data$PopID %in% c("VIL", "SZE") ~ "deciduous",
                                          .data$PopID == "GUL" ~ NA_character_),
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select(.data$LocationID, .data$NestboxID,
                  .data$LocationType, .data$PopID,
                  .data$Latitude, .data$Longitude,
                  .data$StartSeason, .data$EndSeason, .data$HabitatType)

  return(Location_data)

}
