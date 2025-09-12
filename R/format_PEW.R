#' Construct standard format data from Peerdsbos West, Belgium (PEW)
#'
#' A pipeline to produce the standard format for bird study population
#' at the Peerdsbos West, Belgium, administered by Wendt Müller
#' (previously by Arne Iserbyt).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
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
#' \strong{ClutchSize_observed} Where clutch size has ?, recorded value is used
#' as ClutchSize_observed. ClutchSize_min/max is defined by No of chicks D3.
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
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#' @export


format_PEW <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

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
                                               "text", "text", "text", "text",
                                               "text", "skip", "numeric", "skip",
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
                  Mass = as.numeric(dplyr::case_when(stringr::str_detect(.data$Mass, pattern = "dead") ~ NA_character_,
                                                     TRUE ~ .data$Mass)),
                  NumberOfRingedChicks = as.integer(dplyr::case_when(.data$DateRingingChicks %in% c("verlaten incubatie", "all dead d1") ~ "0",
                                                                     .data$HatchDateD0 %in% c("abandoned", "bumblebee nest", "dead embryos") ~ "0",
                                                                     .data$NumberOfRingedChicks == "all dead" ~ "0",
                                                                     .data$NumberOfRingedChicks == "joris" ~ "0",
                                                                     TRUE ~ .data$NumberOfRingedChicks)),
                  #Anything that can't be coerced to numeric should be NA, so we coercion warnings are expected.
                  HatchDateD0 = janitor::excel_numeric_to_date(suppressWarnings(as.numeric(.data$HatchDateD0)),
                                                               date_system = "modern"),
                  DateRingingChicks = janitor::excel_numeric_to_date(suppressWarnings(as.numeric(.data$DateRingingChicks)),
                                                                     date_system = "modern"),
                  NoOfChicksD3 = as.integer(dplyr::case_when(.data$NoOfChicksD3 == "dead" ~ "0",
                                                             TRUE ~ .data$NoOfChicksD3)),
                  BroodMassD3  = as.numeric(dplyr::case_when(.data$BroodMassD3 == "dead" ~ NA_character_,
                                                             TRUE ~ .data$BroodMassD3)),
                  NewRing = toupper(.data$NewRing),
                  Species = species_codes[which(species_codes$speciesEURINGCode == 14620), ]$Species,
                  PopID = "PEW",
                  NestboxID = tolower(.data$Nest),
                  BroodID = ifelse(.data$Method %in% c("Catch adults nestbox", "ChickRinging",
                                                       "Catch incubation"),
                                   paste(.data$Year, .data$NestboxID, sep = "_"), NA)) %>%
    #### Rename variables to standardized format
    dplyr::rename("IndvID" = "Id",
                  "Sex" = "Seks",
                  "ObserverID" = "Measurer") %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-"ObservationTimeH",
                  -"FeatherCollection" ,
                  -"BreathRateTime50Breaths",
                  -"FeathersPartner",
                  -"BreathRatePartnerTime50Breaths",
                  -"BloodSample",
                  -"TimeBloodSample",
                  -"BloodSampleDuration",
                  -"VisitRateVisitsH",
                  -"VrPartner",
                  -"VisitsAlternated",
                  -"VisitsAlternatedPartner",
                  -"VisitsSync10",
                  -"ChickAgeOfBehavObserv",
                  -"Nest",
                  -"MassPartner",
                  -"AgePartner",
                  -"TarsusPartner",
                  -"Date",
                  -"Date_temp") %>%
    #### Reorder columns
    dplyr::select("BreedingSeason",
                  "Species",
                  "PopID",
                  "IndvID",
                  "Sex",
                  "Age",
                  tidyselect::everything()) %>%
    #### Remove observations without ID
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### Change ID of unringed individuals to be NA
    dplyr::mutate(IndvID = ifelse(nchar(.data$IndvID) > 8, NA_character_, .data$IndvID),
                  PartnerId = ifelse(nchar(.data$PartnerId) > 8, NA_character_, .data$PartnerId)) %>%
    dplyr::mutate(DateEgg1 = janitor::excel_numeric_to_date(suppressWarnings(as.numeric(.data$DateEgg1)),
                                                            date_system = "modern")) %>%
    dplyr::distinct()


  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_PEW(pew_data)


  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_PEW(pew_data, Brood_data)


  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_PEW(Capture_data, protocol_version)


  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_PEW(pew_data, protocol_version)


  #### FINAL ARRANGEMENT
  Brood_data <- Brood_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Brood_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Brood_data))

  Capture_data <- Capture_data %>%
    dplyr::filter(!is.na(.data$CaptureDate)) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Capture_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Capture_data))


  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PEW.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_PEW.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PEW.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PEW.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_PEW.txt"),
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

#' Create brood data table for Peerdsbos West, Belgium.
#'
#' Create brood data table in standard format for blue tit data from Peerdsbos West, Belgium.
#' @param data Data frame. Primary data from Peerdsbos West, Belgium.
#'
#' @return A data frame.

create_brood_PEW <- function(data) {

  # 6 duplicated BroodIDs - fix with data owner
  ##FIXME: Duplicated BroodID 2015_24, 2016_60: both parents female
  ##FIXME: Duplicated BroodID 2017_109: different clutch sizes
  ##FIXME: Duplicated BroodIDs 2016_78, 2017_79: different females
  ##FIXME: Duplicated BroodID 2017_49: different partner association
  primary_data <- data %>%
    dplyr::distinct(.data$BroodID, .keep_all = TRUE)

  parent_info <- primary_data %>%
    #### Exclude non-breeding data, exclude chicks
    dplyr::filter(.data$Method %in% c("Catch adults nestbox", "Catch incubation")) %>%
    dplyr::select("BroodID", "IndvID", "PartnerId", "Sex") %>%
    tidyr::pivot_longer(cols = c("IndvID", "PartnerId")) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(Sex = dplyr::case_when(.data$name == "IndvID" ~ .data$Sex,
                                         .data$name == "PartnerId" & .data$Sex == "Female" ~ "Male",
                                         .data$name == "PartnerId" & .data$Sex == "Male" ~ "Female")) %>%
    dplyr::select(-"name") %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "Sex", values_from = "value", values_fn = list) %>%
    tidyr::unnest(cols = c("Female", "Male")) %>%
    dplyr::rename("FemaleID" = "Female", "MaleID" = "Male")

  parents_brood_data <- primary_data %>%
    #### Exclude non-breeding data, exclude chicks
    dplyr::filter(.data$Method %in% c("Catch adults nestbox", "Catch incubation")) %>%
    #### Rename variables
    dplyr::rename("LocationID" = "NestboxID") %>%
    dplyr::mutate(ExperimentID = dplyr::case_when(.data$Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                    "COHORT;PARENTAGE",
                                                  .data$Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                  .data$Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                    "SURVIVAL;PARENTAGE")) %>%
    #### Remove unnecessary variables which may cause duplicated rows
    #### Exclude also Date column, as for few broods, there may be
    #### several catches of parents, but the brood parameters are the same
    dplyr::select(-"IndvID", -"Sex", -"Age", -"PartnerId", -"NumberTransponder", -"Method", -"NewRing",
                  -"CaptureDate", -"Tarsus", -"Mass", -"ObserverID", -"Experiment", -"D14Chicks") %>%
    dplyr::distinct() %>%
    #### Remove rows with no information about the brood
    dplyr::filter(!(is.na(.data$ClutchSize) & is.na(.data$DateEgg1) & is.na(.data$HatchDateD0) &
                      is.na(.data$NumberOfRingedChicks) & is.na(.data$DateRingingChicks) &
                      is.na(.data$NoOfChicksD3) & is.na(.data$BroodMassD3))) %>%
    #Join in parentage data
    dplyr::left_join(parent_info,
                     by = "BroodID") %>%
    #### Create new variables
    dplyr::mutate(Plot = NA_character_,
                  LayDate_observed = .data$DateEgg1, ## for new version of calc_clutchtype
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  #If there is a ? in clutch size, ignore it for observed.
                  #Where there is ? we use No Chicks at D3 as the possible min
                  #Max will be NA
                  ClutchSize_observed = as.integer(stringr::str_replace(string = .data$ClutchSize,
                                                                        pattern = "\\?",
                                                                        replace = "")),
                  ClutchSize_min = dplyr::case_when(stringr::str_detect(.data$ClutchSize, pattern = "\\?") ~ as.integer(.data$NoOfChicksD3),
                                                    TRUE ~ NA_integer_),
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
                  #### Correction regarding one brood from data owner:
                  ## FIXME: Get this corrected in data rather than in file?
                  NumberFledged_observed = ifelse(.data$BroodID == "2015_79", 0L, .data$NumberOfRingedChicks),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  #### Metadata states that only the first clutches are recorded
                  ClutchType_observed = "first") %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"))


  #### Get chick measurements per brood
  chicks_measurements <- primary_data %>%
    dplyr::filter(.data$Method == "ChickRinging") %>%
    dplyr::select("IndvID", "BroodID", "Tarsus", "Mass") %>%
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
  Brood_data <- parents_brood_data %>%
    dplyr::left_join(chicks_measurements, by = "BroodID") %>%
    #### Final arrangement
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID",
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
    dplyr::select("Species", "PopID", "BroodID",
                  "LocationID", "HatchDate_observed") %>%
    dplyr::distinct()


  Capture_data_temp <-
    pew_data %>%
    #Captures without an ID are no use to us
    dplyr::filter(!is.na(.data$IndvID)) %>%
    #### Rename variables
    dplyr::rename("LocationID" = "NestboxID") %>%
    dplyr::select(-"DateRingingChicks") %>%
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
                                                    "COHORT;PARENTAGE",
                                                  .data$Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                  .data$Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                    "SURVIVAL;PARENTAGE"),
                  Age_observed = dplyr::case_when(.data$Age == "0" & .data$Method == "ChickRinging" ~ 1L,
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
    dplyr::left_join(Brood_data_sel,
                     by = c("Species", "PopID", "LocationID", "BroodID")) %>%
    #### Calculate Capture date for chicks from Hatch date
    #### Create fake Hatch_date for several broods
    dplyr::mutate(HatchDate_observed = dplyr::if_else(is.na(.data$HatchDate_observed) & is.na(.data$CaptureDate) &
                                                        .data$Method == "ChickRinging",
                                                      as.Date(paste0(.data$BreedingSeason, "-06-01")),
                                                      .data$HatchDate_observed),
                  CaptureDate = dplyr::case_when(is.na(.data$CaptureDate) & .data$D14Chicks == "yes" ~ .data$HatchDate_observed + 14,
                                                 #### Create fake capture dates
                                                 is.na(.data$CaptureDate) ~ as.Date(paste0(.data$BreedingSeason, "-04-01"), "%Y-%m-%d"),
                                                 TRUE ~ .data$CaptureDate)) %>%
    #### The age of captured chicks in days since hatching
    dplyr::mutate(ChickAge = dplyr::if_else(.data$Age_observed == 1L,
                                            as.integer(difftime(.data$CaptureDate, .data$HatchDate_observed,
                                                                units = "days")),
                                            NA_integer_),
                  BroodIDLaid = .data$BroodID) %>%
    dplyr::distinct() %>%
    #### Create CaptureID
    dplyr::group_by(.data$IndvID) %>%
    dplyr::arrange(.data$Year, .data$CaptureDate, .by_group = TRUE) %>%
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
    dplyr::select("CaptureID", "IndvID", "Species",
                  "Sex_observed", "BreedingSeason",
                  "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID",
                  "CaptureAlive", "ReleaseAlive",
                  "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot",
                  "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated",
                  "ChickAge", "ExperimentID") %>%
    dplyr::distinct()

  return(Capture_data)

}

#' Create individual table for Peerdsbos West, Belgium.
#'
#' Create full individual data table in standard format for data from Peerdsbos West, Belgium.
#'
#' @param data Capture_data, output of create_capture_PEW function.
#' @param protocol_version Character string. The version of the standard protocol on which this pipeline is based.
#'
#' @return A data frame.

create_individual_PEW <- function(data,
                                  protocol_version){

  Individual_data <- data %>%
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
                     Species = dplyr::first(.data$Species),
                     PopID = "PEW",
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = ifelse(min(.data$Age_observed) == 1, "chick", "adult"),
                     BroodIDLaid = ifelse(.data$RingAge == "chick",
                                          paste(.data$BreedingSeason, .data$LocationID, sep = "_"),
                                          NA_character_),
                     BroodIDFledged = .data$BroodIDLaid) %>%
    dplyr::ungroup() %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Individual_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Individual_data))

  return(Individual_data)

}


#' Create location data table for Peerdsbos West, Belgium.
#'
#' Create location data table in standard format for data from Peerdsbos West, Belgium.
#'
#' @param data Data frame pew_data with primary data from Peerdsbos West, Belgium.
#' @param protocol_version Character string. The version of the standard protocol on which this pipeline is based.
#'
#' @return A data frame.

create_location_PEW <- function(data,
                                protocol_version) {

  Location_data <-
    data %>%
    dplyr::select("BreedingSeason", "CaptureDate", "NestboxID", "PopID") %>%
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
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Location_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Location_data))

  return(Location_data)

}
