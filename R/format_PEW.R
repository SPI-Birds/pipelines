#' Construct standard format data from Peerdsbos West, Belgium (PEW)
#'
#' A pipeline to produce the standard format for bird study population
#' at the Peerdsbos West, Belgium, administered by Wendt Müller
#' (previously by Arne Iserbyt).
#'
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{IndvID}: Generally, the unique ID is an 8-character digit number reflecting
#' the metal ring number. In 4 cases, this information was not provided (NA), therefore
#' those observations were removed from the dataset (correspond to chicks ringed in
#' the year 2017 in the nestboxes 97, 98, 102, kk12).
#' Several individuals were not identified by the unique ring number, only by
#' the information reflecting the sex, nestbox and year (i.e. "Unknown_M2019NestK97").
#' Those IndvID were unified as "unringed" and are included in the Brood_data,
#' but not included in the Individual_data.
#'
#' \strong{NumberFledged} Information provided by the data owner: The number of
#' fledged nestlings were not consistently monitored. However, nestlings were
#' ringed at day 14, so very close to fledging. In almost all cases, the number
#' of ringed chicks will be the same as number of fledged chicks.
#'
#' \strong{BroodSize_observed} We used the information provided in the raw data
#' in the column indicating the number of chicks at day 3.
#'
#' \strong{HatchDate_observed} For several broods the hatch date was not provided.
#' When neither hatch date nor capture date were provided, we input fake date
#' ("breeding season-06-01", 1st of June of the corresponding breeding season).
#'
#' \strong{CaptureDate} For several chicks, where the capture date was indicated
#' as "D14", the capture date was calculated as hatch date + 14 days.
#' For several adult individuals, where the capture date was missing,
#' the fake date was input ("breeding season, 04-01", 1st of April of the corresponding
#' breeding season).
#'
#' \strong{Latitude, Longitude} The exact coordinates of the nestboxes are not available.
#' Data owner provided a map of the location, which can be georeferenced in case of
#' interest or necessity of the data user.
#'
#' \strong{NOTES} Until we recive the final data from the data owner, we are aware
#' of the following issues in the final standardized data:
#'
#' There are two records in the Brood_data for the BroodID "2017_109", with different number of chicks and dates
#' (currently we do not know which one is the correct one).
#'
#' There are two records in the Brood_data for the BroodID "2015_24", the correct one is the one where
#' FemaleID is 13619319 and MaleID is 11714676.
#'
#' There are two records in the Brood_data for the BroodID "2016_60", the correct one is the one where
#' FemaleID is 13617052 and MaleID should be unknow.
#' Data owner note: Female 12706296 in box 60 in 2016 is erroneous and can be removed from the data.
#' Female 13617052 was the only female in box 60 in 2016.
#' She layed a complete clutch, which never hatched.
#' The male was never caught and remains unknown.
#'
#' There are two records in the Brood_data for the BroodID "2017_49", the correct one is the one where
#' FemaleID 13619466 is and MaleID is 13617031.
#' Data owner: The only couple in 49 in 2017 was 13619466 (female) and  13617031 (male).
#' partner 12706106 should be removed (or replaced by the true female 13619466).
#'
#' Based on previous comment of data owner, the capture of the IndvID 12706296
#' in box 60 in 2016 is erroneous and can be removed from the data.
#'
#' In the Capture_data, there are 6 erroneous records related to the nest 109 from 2017
#' (related to the Brood_data double record for the BroodID "2017_109"):
#' - 1 extra record for the male 13617008
#' - 2 extra records for the female 13619461
#' - 1 extra record for each chick 14156383, 14156384, 14156385
#'
#'
#' @inheritParams pipeline_params
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export


format_PEW <- function(db = choose_directory(),
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

  pew_data <- readxl::read_excel(path =  paste0(db, "/PEW_PrimaryData.xlsx"),
                                 col_types = c("text", "text", "text",
                                               "text", "text", "numeric", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "text", "text",
                                               "text", "text", "numeric", "numeric",
                                               "text", "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric"),
                                 na = c("NA", "na")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(BreedingSeason = as.integer(.data$Year),
                  #### Handle specifically the date column to preserve the information
                  #### regarding the D14 values for chicks
                  Date_temp = ifelse(.data$Date == "D14", NA_character_, .data$Date),
                  D14Chicks = ifelse(.data$Date == "D14", "yes", "no"),
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$Date_temp),
                                                               date_system = "modern"),
                  Tarsus = as.numeric(.data$Tarsus),
                  Mass = as.numeric(.data$Mass),
                  ClutchSize = as.integer(stringr::str_replace(string = .data$ClutchSize,
                                                               pattern = "\\?",
                                                               replace = "")),
                  NumberOfRingedChicks = case_when(.data$DateRingingChicks %in% c("verlaten incubatie", "all dead d1") ~ 0L,
                                                   .data$HatchDateD0 %in% c("abandoned", "bumblebee nest", "dead embryos") ~ 0L,
                                                   .data$NumberOfRingedChicks == "all dead" ~ 0L,
                                                   TRUE ~ as.integer(.data$NumberOfRingedChicks)),
                  HatchDateD0 = janitor::excel_numeric_to_date(as.numeric(.data$HatchDateD0),
                                                               date_system = "modern"),
                  DateRingingChicks = janitor::excel_numeric_to_date(as.numeric(.data$DateRingingChicks),
                                                                     date_system = "modern"),
                  NoOfChicksD3 = as.integer(.data$NoOfChicksD3),
                  BroodMassD3  = as.numeric(.data$BroodMassD3),
                  NewRing = toupper(.data$NewRing),
                  Species = species_codes[which(species_codes$SpeciesID == 14620), ]$Species,
                  PopID = "PEW",
                  NestboxID = tolower(.data$Nest),
                  BroodID = ifelse(.data$Method %in% c("Catch adults nestbox", "ChickRinging",
                                                 "Catch incubation"),
                                   paste(.data$Year, .data$NestboxID, sep = "_"), NA)) %>%
    #### Rename variables to standardized format
    dplyr::rename(IndvID = .data$Id,
                  Sex = .data$Seks,
                  ObserverID = .data$Measurer) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-.data$ObservationTimeH,
                  -.data$FeatherCollection ,
                  -.data$BreathRateTime50Breaths,
                  -.data$FeathersPartner,
                  -.data$BreathRatePartnerTime50Breaths,
                  -.data$BloodSample,
                  -.data$TimeBloodSample,
                  -.data$BloodSampleDuration,
                  -.data$VisitRateVisitsH,
                  -.data$VrPartner,
                  -.data$VisitsAlternated,
                  -.data$VisitsAlternatedPartner,
                  -.data$VisitsSync10,
                  -.data$ChickAgeOfBehavObserv,
                  -.data$MateStrategy,
                  -.data$Nest,
                  -.data$MassPartner,
                  -.data$AgePartner,
                  -.data$TarsusPartner,
                  -.data$Date,
                  -.data$Date_temp) %>%
    #### Reorder columns
    dplyr::select(.data$BreedingSeason,
                  .data$Species,
                  .data$PopID,
                  .data$IndvID,
                  .data$Sex,
                  .data$Age,
                  everything()) %>%
    #### Remove observations without ID
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### Change ID of unringed individuals to generic "unringed"
    dplyr::mutate(IndvID = ifelse(nchar(.data$IndvID) > 8, "unringed", .data$IndvID),
                  PartnerId = ifelse(nchar(.data$PartnerId) > 8, "unringed", .data$PartnerId)) %>%

    # #### This part of the code can be removed after fixing the data from data owner
    # #### Corrected information from data owner
    # dplyr::mutate(Sex = ifelse(.data$IndvID == "11714676", "Male", .data$Sex),
    #               PartnerId = ifelse(.data$PartnerId == "12706296" & .data$BroodID == "2016_60",
    #                                  NA_character_, .data$PartnerId),
    #               # Data owner: The only couple in 49 in 2017 was 13619466 (female) and  13617031 (male).
    #               # partner 12706106 should be removed (or replaced by the true female 13619466).
    #               PartnerId = ifelse(.data$PartnerId == "12706106" & .data$BroodID == "2017_49",
    #                                  "13619466", .data$PartnerId),
    #               # (2016_60) Data owner: FEMALE 12706296 in box 60 in 2016
    #               # is erroneous and can be removed from the data.
    #               # Female 13617052 was the only female in box 60 in 2016.
    #               # She layed a complete clutch, which never hatched.
    #               # The male was never caught and remains unknown.
    #               rem = ifelse((.data$IndvID == "12706296" & .data$BroodID == "2016_60"),
    #                            "yes", "no")) %>%
    # dplyr::filter(.data$rem == "no" | is.na(.data$rem)) %>%
    # dplyr::select(-.data$rem) %>%
    # ### This part of the code can be removed after fixing the data from data owner


    dplyr::mutate(DateEgg1 = janitor::excel_numeric_to_date(as.numeric(.data$DateEgg1),
                                                            date_system = "modern")) %>%
    dplyr::distinct()


  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_PEW(data = pew_data)


  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_PEW(pew_data, Brood_data)


  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_PEW(Capture_data)


  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_PEW(pew_data)


  #### FINAL ARRANGEMENT
  Capture_data <-
    Capture_data %>%
    dplyr::filter(!is.na(.data$CaptureDate))

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PEW.csv"), row.names = F)

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


#' Create brood data table for Peerdsbos West, Belgium.
#'
#' Create brood data table in standard format for blue tit data from Peerdsbos West, Belgium.
#' @param data Data frame pew_data with primary data from Peerdsbos West, Belgium.
#'
#' @return A data frame.

create_brood_PEW <- function(data) {

  parents_brood_data <-
    data %>%
    #### Exclude non-breeding data, exclude chicks
    dplyr::filter(.data$Method %in% c("Catch adults nestbox", "Catch incubation")) %>%
    #### Rename variables
    dplyr::rename(LocationID = .data$NestboxID) %>%
    #### Get IDs of females and males
    tidyr::pivot_wider(names_from = .data$Sex,
                       values_from = .data$IndvID) %>%
    dplyr::rename(FemaleID = .data$Female,
                  MaleID   = .data$Male) %>%
    dplyr::mutate(FemaleID = ifelse(is.na(.data$FemaleID) & !is.na(.data$MaleID),
                                    .data$PartnerId, .data$FemaleID),
                  MaleID   = ifelse(is.na(.data$MaleID) & !is.na(.data$FemaleID),
                                    .data$PartnerId, .data$MaleID)) %>%
    dplyr::mutate(ExperimentID = dplyr::case_when(.data$Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                    "COHORT; PARENTAGE",
                                                  .data$Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                  .data$Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                    "SURVIVAL; PARENTAGE")) %>%
    #### Remove unnecessary variables which may cause duplicated rows
    #### Exclude also Date column, as for few broods, there may be
    #### several catches of parents, but the brood parameters are the same
    dplyr::select(-c(.data$Age, .data$PartnerId, .data$NumberTransponder,
                     .data$NewRing, .data$CaptureDate, .data$NeophobiaTransponder,
                     .data$Tarsus, .data$Mass, .data$ObserverID,
                     .data$Experiment, .data$D14Chicks)) %>%
    #### Remove duplicated rows (as we get one row for males and females for the same brood)
    dplyr::distinct() %>%
    #### Remove rows with no information about the brood
    dplyr::filter(!(is.na(.data$ClutchSize) & is.na(.data$DateEgg1) & is.na(.data$HatchDateD0) &
                      is.na(.data$NumberOfRingedChicks) & is.na(.data$DateRingingChicks) &
                      is.na(.data$NoOfChicksD3) & is.na(.data$BroodMassD3))) %>%


    ### This part of the code can be removed after fixing the data from data owner
    ### Remove one specific case causing double register for the same BroodID
    # dplyr::filter(!(.data$BroodID == "2017_109" & .data$Method == "Catch incubation")) %>%
    ### This part of the code can be removed after fixing the data from data owner


    #### Create new variables
    dplyr::mutate(Plot = NA_character_,
                  # LayDate_observed = DateEgg1, ## for new version of calc_clutchtype
                  LayDate = .data$DateEgg1,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$ClutchSize,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$HatchDateD0,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  #### For few broods, there is number of chicks on day 3
                  BroodSize_observed = .data$NoOfChicksD3,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  # NumberFledged_observed = NumberOfRingedChicks, ## for new version of calc_clutchtype
                  #### Correction regarding one brood from data owner:
                  NumberFledged = ifelse(.data$BroodID == "2015_79", 0L, .data$NumberOfRingedChicks),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  #### Metadata states that only the first clutches are recorded
                  ClutchType_observed = "first") %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE))


  #### Get chick measurements per brood
  chicks_measurements <-
    data %>%
    dplyr::filter(.data$Method == "ChickRinging") %>%
    dplyr::select(.data$IndvID, .data$BroodID, .data$Tarsus, .data$Mass) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(as.numeric(.data$Mass), na.rm = TRUE),
                     NumberChicksMass = sum(!is.na(.data$Mass)),
                     AvgTarsus = mean(as.numeric(.data$Tarsus), na.rm = TRUE),
                     NumberChicksTarsus = sum(!is.na(.data$Tarsus)),
                     OriginalTarsusMethod = ifelse(!is.na(.data$AvgTarsus), "Alternative", NA_character_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AvgTarsus = ifelse(.data$AvgTarsus == "NaN", NA, .data$AvgTarsus),
                  AvgChickMass = ifelse(.data$AvgChickMass == "NaN", NA, .data$AvgChickMass),
                  NumberChicksMass = ifelse(.data$NumberChicksMass == 0, NA, .data$NumberChicksMass),
                  NumberChicksTarsus = ifelse(.data$NumberChicksTarsus == 0, NA, .data$NumberChicksTarsus))


  #### Join parents and chick data
  Brood_data <-
    parents_brood_data %>%
    left_join(chicks_measurements, by = "BroodID") %>%
    #### Rename
    dplyr::rename(LayDate_observed = .data$LayDate,
                  NumberFledged_observed = .data$NumberFledged) %>%
    #### Final arrangement
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                  .data$Species, .data$Plot, .data$LocationID,
                  .data$FemaleID, .data$MaleID,
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

  return(Brood_data)

}

#' Create capture data table for blue tits in Peerdsbos West, Belgium.
#'
#' Create a capture data table in standard format for blue tits in Peerdsbos West, Belgium.
#' @param pew_data Data frame. Data frame pew_data with primary data from Peerdsbos West, Belgium.
#' @param Brood_data Data frame. Brood_data from Peerdsbos West, Belgium.
#' @return A data frame.

create_capture_PEW <- function(pew_data, Brood_data) {

  Brood_data_sel <-
    Brood_data %>%
    # dplyr::select(BreedingSeason, Species, PopID, BroodID, LocationID,
    dplyr::select(.data$Species, .data$PopID, .data$BroodID,
                  .data$LocationID, .data$HatchDate_observed)


  Capture_data_temp <-
    pew_data %>%
    #### Remove unringed individuals
    dplyr::filter(.data$IndvID != "unringed") %>%
    #### Rename variables
    dplyr::rename(LocationID = .data$NestboxID) %>%
    dplyr::select(-.data$DateRingingChicks) %>%
    dplyr::mutate(CaptureTime  = NA_character_,
                  Sex_observed = ifelse(.data$Sex == "Chick", NA_character_, substr(.data$Sex, 1, 1)),
                  CaptureAlive = ifelse(.data$Method != "Found dead", TRUE, FALSE),
                  ReleaseAlive = .data$CaptureAlive,
                  CapturePopID = .data$PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(.data$ReleaseAlive == TRUE, .data$CapturePlot, NA_character_),
                  OriginalTarsusMethod = ifelse(!is.na(.data$Tarsus), "Alternative", NA_character_),
                  WingLength = NA_real_,
                  ExperimentID = dplyr::case_when(.data$Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                    "COHORT; PARENTAGE",
                                                  .data$Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                  .data$Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                    "SURVIVAL; PARENTAGE"),
                  Age_observed = case_when(.data$Age == "0" & .data$Method == "ChickRinging" ~ 1L,
                                           .data$Age == "1" ~ 5L,
                                           .data$Age == ">=2" ~ 6L,
                                           #### Account for controls in January & February
                                           .data$Age == "1" & .data$Method == "Night Control Winter" &
                                             lubridate::month(.data$CaptureDate) %in% c(1, 2) ~ 6L,
                                           #### Account for controls in January & February
                                           .data$Age == ">=2" & .data$Method == "Night Control Winter" &
                                             lubridate::month(.data$CaptureDate) %in% c(1, 2) ~ 8L,
                                           is.na(.data$Age) ~ 4L))



  Capture_data <-
    Capture_data_temp %>%
    left_join(Brood_data_sel,
              by = c("Species", "PopID", "LocationID", "BroodID")) %>%
    #### Calculate Capture date for chicks from Hatch date
    #### Create fake Hatch_date for several broods
    dplyr::mutate(HatchDate_observed = dplyr::if_else(is.na(.data$HatchDate_observed) & is.na(.data$CaptureDate) &
                                                 .data$Method == "ChickRinging",
                                               as.Date(paste0(.data$BreedingSeason, "-06-01")),
                                               .data$HatchDate_observed),
                  CaptureDate = case_when(is.na(.data$CaptureDate) & .data$D14Chicks == "yes" ~ .data$HatchDate_observed + 14,
                                          #### Create fake capture dates
                                          is.na(.data$CaptureDate) ~ as.Date(paste0(.data$BreedingSeason, "-04-01"), "%Y-%m-%d"),
                                          TRUE ~ .data$CaptureDate)) %>%
    #### The age of captured chicks in days since hatching
    dplyr::mutate(ChickAge = dplyr::if_else(.data$Age_observed == 1L,
                                     as.integer(difftime(.data$CaptureDate, .data$HatchDate_observed,
                                                         units = "days")),
                                     NA_integer_),
                  BroodIDLaid = .data$BroodID) %>%
    distinct() %>%
    #### Create CaptureID
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$Year, .data$CaptureDate, .by_group = TRUE) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, row_number(), sep = "_")) %>%
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
    dplyr::select(.data$CaptureID, .data$IndvID, .data$Species,
                  .data$Sex_observed, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID) %>%
    distinct()

  return(Capture_data)

}

#' Create individual table for Peerdsbos West, Belgium.
#'
#' Create full individual data table in standard format for data from Peerdsbos West, Belgium.
#'
#' @param data Capture_data, output of create_capture_PEW function.
#'
#' @return A data frame.

create_individual_PEW <- function(data){

  Individual_data <-
    data %>%
    #### Remove unringed individuals
    dplyr::filter(.data$IndvID != "unringed") %>%
    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%
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
                     PopID = "PEW",
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = ifelse(min(.data$Age_observed) == 1, "chick", "adult"),
                     BroodIDLaid = ifelse(.data$RingAge == "chick",
                                          paste(.data$BreedingSeason, .data$LocationID, sep = "_"),
                                          NA_character_),
                     BroodIDFledged = .data$BroodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_calculated, .data$Sex_genetic)

  return(Individual_data)

}


#' Create location data table for Peerdsbos West, Belgium.
#'
#' Create location data table in standard format for data from Peerdsbos West, Belgium.
#'
#' @param data Data frame pew_data with primary data from Peerdsbos West, Belgium.
#'
#' @return A data frame.

create_location_PEW <- function(data) {

  Location_data <-
    data %>%
    dplyr::select(.data$BreedingSeason, .data$CaptureDate, .data$NestboxID, .data$PopID) %>%
    dplyr::group_by(.data$NestboxID) %>%
    dplyr::arrange(.data$BreedingSeason, .data$CaptureDate, .by_group = TRUE) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     LocationID = unique(.data$NestboxID),
                     PopID = unique(.data$PopID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType = "deciduous",
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select(.data$LocationID, .data$NestboxID,
                  .data$LocationType, .data$PopID,
                  .data$Latitude, .data$Longitude,
                  .data$StartSeason, .data$EndSeason, .data$HabitatType)

  return(Location_data)

}
