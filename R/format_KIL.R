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

#### Questions for data owners
# OriginalTarsusMethod (both datasets)



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
  # No general primary data, there are several sheets corresponding to specific data.

  #### Data after 1995
  kil_data_95 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_from1995_GT.xlsx"), sheet = 1) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(Species = "PARMAJ",  ## confirm
                  PopID = "KIL",
                  BreedingSeason = as.integer(Year),
                  NestboxID = tolower(NestId),
                  FemaleID = as.character(FemaleRingNo),
                  MaleID = as.character(MaleRingNo),
                  BroodID = paste(Year, NestboxID, sep = "_")) %>%
    #### Reorder columns
    dplyr::select(BreedingSeason,
                  Species,
                  PopID,
                  everything(),
                  - NestId)





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
    dplyr::mutate(BroodID = as.character(BroodId),
                  Species = "PARMAJ",  ## confirm
                  PopID = "KIL",
                  BreedingSeason = as.integer(Year),
                  LocationID = tolower(NestboxId),
                  Plot = LocationID,
                  FemaleID = as.character(FemaleId),
                  MaleID = as.character(MaleId),
                  ClutchType_observed = as.character(tolower(ClutchType)),
                  #### Calculate laying date
                  # Laying date is the date on which the first egg of a clutch is laid.
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  # Use negative numbers for laying dates in March.
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(BreedingSeason, "04-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + LayDate - 1,
                  LayDate = as.Date(paste(BreedingSeason, "04-01", sep = "-"),
                                    format = "%Y-%m-%d") + LayingDate - 1,
                  LayDate_min = NA_character_,
                  LayDate_max = NA_character_,
                  ClutchSize_observed = as.integer(ClutchSize),
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
                  # NumberFledged_observed = as.integer(NumberFledglings),
                  NumberFledged = as.integer(NumberFledglings),
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
    dplyr::select(-NestboxId,
                  -BroodId,
                  -FemaleId,
                  -FemaleDate,
                  -MaleId,
                  -MaleDate,
                  -LayingDate,
                  -Year,
                  -FemaleRecruits,
                  -MaleRecruits,
                  -AverageMass,
                  -AverageTarsus) %>%
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
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)



  #### Data from 1995
  brood_data_95 <-
    kil_data_95 %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(LocationID = NestboxID,
                  Plot = LocationID,
                  ClutchType_observed = as.character(tolower(Brood)),
                  #### Calculate laying date
                  # Laying date is the date on which the first egg of a clutch is laid.
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  ## for new version of calc_clutchtype
                  # LayDate_observed = as.Date(paste(BreedingSeason, "04-01", sep = "-"),
                  #                            format = "%Y-%m-%d") + LayingDate1April1St - 1,
                  LayDate = as.Date(paste(BreedingSeason, "04-01", sep = "-"),
                                    format = "%Y-%m-%d") + LayingDate1April1St - 1,
                  LayDate_min = NA_character_,
                  LayDate_max = NA_character_,
                  ClutchSize_observed = as.integer(ClutchSize),
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  #### Calculate hatching date
                  # Expressed as days after 31st of March: 1 = April 1st, 31 = May 1st, etc.
                  HatchDate_observed = as.Date(paste(BreedingSeason, "04-01", sep = "-"),
                                               format = "%Y-%m-%d") + HatchDate1April1St - 1,
                  HatchDate_min = NA_character_,
                  HatchDate_max = NA_character_,
                  BroodSize_observed = as.integer(NoOfHatchlings),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = NA_character_,
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  ## for new version of calc_clutchtype
                  # NumberFledged_observed = as.integer(NumberFledglings),
                  NumberFledged = as.integer(NoOfFledglings),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA,
                  NumberEggs = NA_integer_,
                  #### Protocol: Average mass in grams of all chicks in the brood measured at 14 - 16 days.
                  #### Here we use the measurements at 15 days.
                  AvgChickMass = NestlingMassDay15G,
                  NumberChicksMass = NA_integer_,
                  #### Average tarsus length in millimeters of all chicks in the brood measured
                  #### at 14 - 16 days after hatching.
                  #### Here we use the measurements at 15 days.
                  AvgTarsus = NestlingTarsusDay15Mm,
                  NumberChicksTarsus = NA,
                  #### Ask data owner
                  OriginalTarsusMethod = NA,
                  ExperimentID = NA_character_) %>%
    #### Even though, there are no dates for the clutch type calculation
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
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)


  Brood_data <-
    brood_data_til92 %>%
    dplyr::bind_rows(brood_data_95)

  return(Brood_data)

}



#### CAPTURE DATA

reate_capture_KIL <- function() {

  # capture_adults

  # capture_chick

}


#### SOLVE: There is no date available, only the year.
#### Create fake date?
# CaptureDate = as.Date(paste0(BreedingSeason, "-05-01")),



#### INDIVIDUAL DATA

create_individual_KIL <- function() {


  #### Adults
  #### Data from 1971 to 1992
  adults_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "AdultData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename(RingSeason = RingDate) %>%
    dplyr::mutate(IndvID = as.character(AdultId),
                  RingAge = "adult",
                  Species = "PARMAJ",
                  PopID = "KIL",
                  Sex_observed = Sex,
                  BroodIDLaid = NA_character_,
                  BroodIDFledged = NA_character_) %>%
    dplyr::select(- Status,
                  - AdultId,
                  - Sex,
                  - Age,
                  - BroodId)

  #### Chicks
  #### Data from 1971 to 1992
  chicks_data_til92 <-
    readxl::read_excel(path =  paste0(db, "/KIL_Data_to1992.xlsx"), sheet = "NestlingData") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename(RingSeason = RingDate) %>%
    dplyr::mutate(IndvID = as.character(NestlingRingNumber),
                  # BroodIDLaid = as.character(BroodId),
                  RingAge = "chick",
                  Species = "PARMAJ",
                  PopID = "KIL",
                  BroodIDFledged = as.character(BroodId),
                  Sex_observed = Sex) %>%
    dplyr::select(- Mass,
                  - Tarsus,
                  - NestlingRingNumber)

   #### We assume that the BroodIDLaid is the same as BroodIDFledged,
   #### need to ask data owner.

  Indv_data_til92 <-
    adults_data_til92 %>%
    full_join(chicks_data_til92,
              by = c("RingSeason", "IndvID", "RingAge", "Species", "PopID",
                     "Sex_observed", "BroodIDFledged")) %>%
    group_by(IndvID) %>%
    arrange(IndvID, RingSeason) %>%
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
                     Species = first(Species),
                     PopID = first(PopID),
                     RingSeason = first(RingSeason),
                     RingAge = first(RingAge),
                     BroodIDLaid = first(BroodIDLaid),
                     BroodIDFledged = first(BroodIDFledged),
                     Sex_genetic = NA_character_) %>%
    ungroup() %>%
    #### Final arrangement
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge, Sex_calculated, Sex_genetic)



  #### Data from 1995


}



#### LOCATION DATA

create_location_KIL <- function() {





}




