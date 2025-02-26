library(tidyverse)
library(dplyr)
#'Construct standard format for data from Forstenrieder park, Germany
#'
#'A pipeline to produce the standard format for the nest box population in Forstenrieder park,
#'Germany, administered by Niels Dingemanse.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Species code 0 refers to 'unknown' and is treated as NA and removed.
#'
#'\strong{ClutchType_observed}: Code 4 'Replacement second' is counted as 'second' in our method because it has come after at least
#'one successful clutch. Code 5 'Probably replacement first or second' and Code 0 'unknown' are considered unknown and treated as NA.
#'
#'\strong{BroodSize}: BroodSize is the number of chicks that hatched (maximum number of chicks between d1 and d3/4)
#'BroodSize_min is equal to BroodSize observed.
#'BroodSize_max is ClutchSize_observed - the number of unhatched eggs.
#'
#'\strong{NumberFledged}: Use NumberFledged to determine observed number of fledglings.
#'NumberFledged_min is equal to BroodSize_observed.
#'NumberFledged_max is the number of chicks measured on d14(PARMAJ)/15(CYACAE)
#'
#'#'\strong{FledgeDate}: Use NumberFledged to determine observed number of fledglings.
#'FledgeDate_min is equal to FledgeDate_observed - FledgedCheckInterval (time since the last fledge check)
#'FledgeDate_max is equal to FledgeDate_observed
#'
#'#'#'\strong{LocationID}: Due to an ongoing density manipulation experiment (since 2020), nest box coordinates and entrance sizes can change (GT, BT or closed) between years. Therefore each location ID is unique every year.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_FOR <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Assign to database location
  db <- paste0(gsub("\\\\", "/", db), "\\FOR_PrimaryData.accdb")

  #Assign species for filtering
  #If no species are specified, all species are included
  if(is.null(species)){

    species_filter <- species_codes$Species#There is no internal data or script to open it from the directory.

  } else {

    species_filter <- species

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  ###N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the FOR database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(),
                               .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))
  #DBI::dbDisconnect(connection)
  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_FOR(connection)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_FOR(Brood_data, connection)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_FOR(Capture_data = Capture_data,
                                           Brood_data = Brood_data,
                                           connection = connection)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_FOR(Capture_data, connection)

  # WRANGLE DATA FOR EXPORT

  #Calculate average mass per brood. Blue tits were measured on D15 and Great tits on D14
  AvgMass <- Capture_data %>%
    dplyr::filter(.data$ChickAge >= 14L & !is.na(.data$Mass)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Mass),
                     NumberChicksMass = dplyr::n())

  #Calculate average tarsus per brood
  AvgTarsus <- Capture_data %>%
    dplyr::filter(.data$ChickAge >= 14L & !is.na(.data$Tarsus)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgTarsus = mean(.data$Tarsus),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = "Alternative")#check what is the name of the method we use

  #Add average mass and tarsus to brood data
   Brood_data <- Brood_data %>%
    dplyr::left_join(AvgMass, by = "BroodID") %>%
    dplyr::left_join(AvgTarsus, by = "BroodID") %>%
    dplyr::select("BroodID":"NumberEggs", "AvgChickMass",
                  "NumberChicksMass", "AvgTarsus",
                  "NumberChicksTarsus", "OriginalTarsusMethod", "ExperimentID")

  #Remove BroodID, no longer needed
  Capture_data <- Capture_data %>%
    dplyr::select("CaptureID":"Age_observed", "Age_calculated", "ChickAge", "ExperimentID")

  #Disconnect from database
  DBI::dbDisconnect(connection)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if (output_type == 'csv') {

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_FOR.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_FOR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_FOR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_FOR.csv"), row.names = F)

    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}

#' Create brood data table for Forstenrieder park, Germany.
#'
#' Create brood data table in standard format for data from Forstenrieder park, Germany.
#'
#' @param connection Connection the SQL database.
#'
#' @return A data frame.

create_brood_FOR   <- function(connection) {

  Catches_M <- dplyr::tbl(connection, "Catches") %>%
    dplyr::select("BroodID", "MaleID" = "RingNumber", "Sex") %>%
    dplyr::filter(!is.na(.data$BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$Sex == 2L) %>%
    dplyr::select(-"Sex")

  Catches_F <- dplyr::tbl(connection, "Catches") %>%
    dplyr::select("BroodID", "FemaleID" = "RingNumber", "Sex") %>%
    dplyr::filter(!is.na(.data$BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$Sex == 1L) %>%
    dplyr::select(-"Sex")

  Brood_data <- dplyr::tbl(connection, "Broods") %>%
    dplyr::left_join(Catches_F,
                     by = c("BroodID")) %>%
    dplyr::collapse() %>%
    dplyr::left_join(Catches_M,
                     by = c("BroodID")) %>%
    dplyr::collapse() %>%

    dplyr::collect() %>%
    # Remove -99 and replace with NA
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(~is.numeric(.)),
                                .fns = ~dplyr::na_if(., -99))) %>%
    # For day data, remove any cases >= 500. These equate to NA
    dplyr::mutate(dplyr::across(.cols = c("HatchDay", "FledgeDay"),
                                .fns = ~{

                                  dplyr::case_when(. >= 500 ~ NA_integer_,
                                                   TRUE ~ .)

                                })) %>%

    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  LocationID=paste(.data$NestBox, .data$BroodYear,sep="_"),
                  BreedingSeason = .data$BroodYear,
                  PopID = "FOR",
                  EndMarch = as.Date(paste(.data$BroodYear, "03", "31", sep = "-")),
                  LayDate_observed = .data$EndMarch + .data$FirstEggDay,
                  LayDate_max = .data$LayDate_observed,
                  LayDate_min = .data$EndMarch + (.data$FirstEggDay - .data$FirstEggDayError),
                  ClutchSize_observed = .data$ClutchSize,
                  ClutchSize_min = .data$ClutchSize_observed,
                  ClutchSize_max = .data$ClutchSize_observed,
                  HatchDate_observed = .data$EndMarch + .data$HatchDay,
                  HatchDate_max = .data$HatchDate_observed,
                  HatchDate_min = .data$EndMarch + (.data$HatchDay - .data$HatchDayError),
                  BroodSize_observed = .data$NumberHatched,
                  BroodSize_min = .data$NumberHatched,
                  BroodSize_max = .data$ClutchSize_observed- .data$UnhatchedEggs,
                  FledgeDate_observed = .data$EndMarch + .data$FledgeDay,
                  FledgeDate_min = .data$FledgeDate_observed- .data$FledgeCheckInterval,
                  FledgeDate_max = .data$FledgeDate_observed,
                  NumberFledged_observed = .data$NumberFledged,
                  NumberFledged_min = .data$NumberFledged,
                  NumberFledged_max = .data$NumberFledged,
                  Species = dplyr::case_when(.data$Species == 1L ~ !!species_codes$Species[species_codes$SpeciesID == "14640"],
                                             .data$Species == 2L ~ !!species_codes$Species[species_codes$SpeciesID == "14620"],
                                             .data$Species == 3L ~ !!species_codes$Species[species_codes$SpeciesID == "14610"],
                                             .data$Species == 4L ~ !!species_codes$Species[species_codes$SpeciesID == "14790"]),
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  ExperimentID = as.character(NA)) %>%

    # Determine clutch type
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"),
                  ClutchType_observed = dplyr::case_when(.data$ClutchNumber == 1L ~ "first",
                                                         .data$ClutchNumber %in% c(2L, 4L) ~ "second",
                                                         .data$ClutchNumber %in% c(3L, 5L, 6L) ~ "replacement")) %>%

    ###We need to remove  the BroodIDS for nests in which nothing happened (generated automatically by the db). In these nests CS is 0 and LD is unknown, no clutch type or species either, no data

    dplyr::filter(!is.na(.data$Species),
                  .data$ClutchSize_observed>0,
           !is.na(.data$LayDate_observed),
           !is.na(.data$ClutchType_observed))%>%


    # Arrange columns

    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID" , "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min",
                  "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min",
                  "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min",
                  "NumberFledged_max",
                  "AvgEggMass", "NumberEggs",
                  "ExperimentID") %>%
    # Convert to correct formats
    dplyr::mutate(dplyr::across(c("Plot":"MaleID"),
                                as.character)) %>%
    dplyr::mutate(dplyr::across(c("LayDate_observed", "HatchDate_observed", "FledgeDate_observed"),
                                as.Date))

  return(Brood_data)

}

#' Create capture data table for Forstenrieder park, Germany
#'
#' Create capture data table in standard format for data from Forstenrieder park, Germany.
#'
#' @param Brood_data Data frame. Output from \code{\link{create_brood_FOR}}.
#' @param connection Connection the SQL database.
#'
#' @return A data frame.

create_capture_FOR <- function(Brood_data, connection) {

  Catches_table <- dplyr::tbl(connection, "Catches")

  Chick_catch_tables <- dplyr::tbl(connection, "Chicks")

  Nestbox_capture <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::select("NestBox"= "NestBoxID", "CapturePlot" = "Plot")

  #Adult captures
  Adult_capture <- Catches_table %>%
    dplyr::left_join(Nestbox_capture, by = "NestBox") %>%
    dplyr::collapse() %>%
    dplyr::collect() %>%
    dplyr::filter(.data$CatchYear<2024)%>%#empty rows were created for 2024 to prepare for the field season. remove these.
    dplyr::filter(!is.na(.data$RingNumber))%>%#remove individuals without bird ids (escaped before the ring was read)
    dplyr::mutate(Species = dplyr::case_when(.data$Species == 1L ~ !!species_codes$Species[species_codes$SpeciesID == "14640"],
                                             .data$Species == 2L ~ !!species_codes$Species[species_codes$SpeciesID == "14620"],
                                             .data$Species == 3L ~ !!species_codes$Species[species_codes$SpeciesID == "14610"],
                                             .data$Species == 4L ~ !!species_codes$Species[species_codes$SpeciesID == "14790"]),
                  CaptureTime = dplyr::na_if(paste(lubridate::hour(.data$CatchTime),
                                                   lubridate::minute(.data$CatchTime), sep = ":"), "NA:NA"),
                  IndvID = as.character(.data$RingNumber),
                  BreedingSeason = .data$CatchYear,
                  CaptureDate = as.Date(.data$CatchDate),
                  CapturePopID = "FOR",
                  ReleasePopID = "FOR",
                  CapturePlot = as.character(.data$Plot),
                  ReleasePlot = .data$CapturePlot,
                  LocationID = paste(.data$NestBox,.data$CatchYear,sep="_"),
                  CaptureAlive=TRUE,
                  ReleaseAlive=dplyr::if_else(.data$Adultdeath==1 & as.Date(.data$DateAdultdeath) == .data$CaptureDate, FALSE,TRUE),
                  ReleaseAlive=dplyr::if_else(is.na(.data$ReleaseAlive),TRUE,.data$ReleaseAlive),
                  OriginalTarsusMethod = "Alternative",
                  Age_observed = dplyr::case_match(.data$AgeObserved,## These age categories match our EURING codes exactly
                                                   7 ~ NA_integer_,
                                                   0 ~ NA_integer_,
                                                   .default = .data$AgeObserved),
                  ChickAge = NA_integer_,
                  ObserverID = as.character(dplyr::na_if(.data$MorphologyObserver, -99L)),
                  BroodID = as.character(.data$BroodID),
                  Sex_observed = dplyr::case_when(.data$Sex == 1L ~ "F",
                                                  .data$Sex == 2L ~ "M",
                                                  TRUE ~ NA_character_),
                  ExperimentID = as.character(NA),
                  Mass=round(.data$BodyMass,2),
                  Tarsus=round(.data$Tarsus,1),
                  WingLength=round(.data$WingLength),1) %>%
    dplyr::select("IndvID",
                  "Species",
                  "Sex_observed",
                  "BreedingSeason",
                  "CaptureDate",
                  "CaptureTime",
                  "ObserverID",
                  "LocationID",
                  "CaptureAlive",
                  "ReleaseAlive",
                  "CapturePopID",
                  "CapturePlot",
                  "ReleasePopID",
                  "ReleasePlot",
                  "Mass",
                  "Tarsus",
                  "OriginalTarsusMethod",
                  "WingLength",
                  "Age_observed",
                  "ChickAge",
                  "ExperimentID",
                  "BroodID")
  #Chick captures
  Chick_capture <- Chick_catch_tables %>%
    #dplyr::filter(ChickYear<2024)%>%
    dplyr::collect()  %>%
    dplyr::filter(!is.na(.data$RingNumber))%>%#remove individuals without bird ids (escaped before the ring was read)

  #empty rows were created for 2024 to prepare for the field season. remove these.
    dplyr::mutate(EndMarch = as.Date(paste(.data$ChickYear, "03", "31", sep = "-")),
                  CapturePopID = "FOR",
                  ReleasePopID = "FOR",
                  IndvID=as.character(.data$RingNumber),
                  Species =  dplyr::case_when(.data$Species == 1L ~ !!species_codes$Species[species_codes$SpeciesID == "14640"],
                                              .data$Species == 2L ~ !!species_codes$Species[species_codes$SpeciesID == "14620"]))%>%
    dplyr::collect()  %>%
    dplyr::mutate(Sex_observed = NA_character_,
                  ExperimentID = as.character(NA),
                  BreedingSeason = .data$ChickYear,
                  CaptureDate=as.Date(.data$RingingDate),
                  CaptureTime = dplyr::na_if(paste(lubridate::hour(.data$Day14BodyMassTime),
                                                   lubridate::minute(.data$Day14BodyMassTime), sep = ":"), "NA:NA"),)%>%
  dplyr::collect()   %>%
  dplyr::mutate(ObserverID = as.character(dplyr::na_if(.data$Day14Observer, -99L)),
                  LocationID = paste(.data$NestBox,.data$ChickYear,sep="_") ,
                  CaptureAlive=TRUE,
                  ReleaseAlive=TRUE,
                  CapturePlot=as.character(.data$Plot),
                  ReleasePlot=as.character(.data$Plot),
                  Mass = round(.data$Day14BodyMass,2),
                  Tarsus=round(.data$Day14Tarsus,1),
                  WingLength = round(.data$Day14Wing,1),
                  OriginalTarsusMethod = "Alternative",
                  Age_observed=1L,
                  ChickAge = .data$ChickRingingDay-.data$HatchDay,
                  BroodID=as.character(.data$BroodID))%>%
    dplyr::collapse() %>%
    dplyr::collect() %>%
    dplyr::select("IndvID",
                  "Species",
                  "Sex_observed",
                  "BreedingSeason",
                  "CaptureDate",
                  "CaptureTime",
                  "ObserverID",
                  "LocationID",
                  "CaptureAlive",
                  "ReleaseAlive",
                  "CapturePopID",
                  "CapturePlot",
                  "ReleasePopID",
                  "ReleasePlot",
                  "Mass",
                  "Tarsus",
                  "OriginalTarsusMethod",
                  "WingLength",
                  "Age_observed",
                  "ChickAge",
                  "ExperimentID",
                  "BroodID")

  Capture_data <- dplyr::bind_rows(Adult_capture, Chick_capture) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    #Arrange by ID/Date and add unique capture ID
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::select("CaptureID", tidyselect::everything())

  return(Capture_data)

}

#' Create individual data table for Forstenrieder Park, Germany
#'
#' Create individual data table in standard format for data from Forstenrieder Park, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_FOR}}.
#' @param Brood_data Data frame. Output from \code{\link{create_brood_FOR}}.
#' @param connection Connection the SQL database.
#'
#' @return A data frame.

create_individual_FOR <- function(Capture_data, Brood_data, connection) {


  Sex_calc <- Capture_data %>%
    dplyr::filter(!is.na(.data$Sex_observed)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$Sex_observed)),
                     unique_sex = list(unique(.data$Sex_observed))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                    TRUE ~ .data$unique_sex[[1]])) %>%
    dplyr::ungroup() %>%
    dplyr::select("IndvID", "Sex_calculated")

  Brood_info <- dplyr::tbl(connection, "Chicks") %>%
    dplyr::select("IndvID" = "RingNumber", "BroodIDLaid" = "BroodID", "BroodIDFledged" = "BroodID") %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

  Individual_data <- Capture_data %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                              .f = ~{

                                                if(length(..1) == 0){

                                                  return(NA_character_)

                                                } else if(length(..1) == 1){

                                                  return(..1)

                                                } else {

                                                  return("CCCCCC")

                                                }

                                              }),
                     PopID = "FOR",
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = ifelse(min(.data$Age_observed) != 1L | is.na(min(.data$Age_observed)),
                                      "adult", "chick")) %>%
    dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::left_join(Brood_info, by = "IndvID") %>%
    dplyr::mutate(Sex_genetic=NA_character_)%>%
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_calculated", "Sex_genetic")

  return(Individual_data)

}

#' Create location data table for Forstenrieder Park, Germany.
#'
#' Create location data table in standard format for data from Forstenrieder Park, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_FOR}}.
#' @param connection Connection the SQL database.
#'
#' @return A data frame.

create_location_FOR <- function(Capture_data, connection) {

  Location_data <- dplyr::tbl(connection, "Coordinates_19_24") %>%
    dplyr::collect() %>%
    dplyr::mutate(LocationID = as.character(.data$BoxIDYear),
                  NestboxID = as.character(.data$BoxIDYear),
                  Latitude = as.numeric(.data$Latitude), ## Needed because these variables are stored as
                  Longitude = as.numeric(.data$Longitude), ## named vectors, not regular numeric vectors
                  LocationType = "NB",
                  PopID = "FOR",
                  StartSeason = as.integer(.data$Year),
                  EndSeason = as.integer(.data$Year),
                  HabitatType = "deciduous") %>%
    dplyr::select("LocationID",
                  "NestboxID",
                  "LocationType",
                  "PopID",
                  "Latitude",
                  "Longitude",
                  "StartSeason",
                  "EndSeason",
                  "HabitatType")

  return(Location_data)

}
