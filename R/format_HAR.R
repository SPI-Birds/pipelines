#'Construct standard format for data from Harjavalta, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: Data from Harjavalta contains information on 23 different
#'hole nesting species; however, only 4 of these (great tit, blue tit, coal tit,
#'pied flycatcher) have >100 nest records. Only data from these 4 species is
#'considered.
#'
#'\strong{Age}: Chick age is listed as: PP (nestling), PM (fledgling), FL(unknown),
#'1, +1, 2, +2. Translation from field notebook would translate PP/PM into
#'EURING 1. PP are chicks in the nest. PM are chick left the nest but caught by
#'hand (i.e. still not able to fly). 1 = 3 (i.e. fully grown in first year).
#'2 = 5 (i.e. fully grown and known to be born last year).
#'+1/+2 are 4/6 respectively, at least 1/2 years old.
#'FL is unknown. We can't even attribute it to chick or adult.
#'For this we use EURING 2 (able to fly freely but otherwise unknown).
#'
#'\strong{LayDateError & HatchDateError}: Accuracy of laying and hatch date
#'are given as categories: 0-1; 1-2; 2-3; 'inaccurate'. Where error is a range,
#'the more conservative error is used (i.e. 0-1 is recorded as 1).
#'Cases listed as 'inaccurate' have an error of at least a week.
#'Therefore, these ones are given Lay/HatchDateError of 7. Dates in these
#'cases are highly inaccurate and shouldn't be considered for any phenology
#'analysis.
#'
#'\strong{Capture data:} Linking nestling and adult capture
#'data can be difficult. There are eight different scenarios we need to
#'consider. Each of these is described below, with our solution: \itemize{ \item
#'#1. Individual captured as adult (Age is NA or not PM/PP) (e.g. 00-829590).
#'
#'In this case, we assume that there is only information in Capture_data, and we
#'do not search for any information in Nestling_data.
#'
#'\item #2. Record of chick (Age is PM/PP) with no information in the last ring
#'number column AND no corresponding record in Nestling_data (i.e. no record
#'where BroodID and last 2 digits of the ring number were the same) (e.g.
#'HL-025681).
#'
#'In this case, we assume that the information in Capture_data is the correct
#'capture information.
#'
#'\item #3.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, with corresponding record in Nestling_data, BUT where the
#'capture dates in Nestling_data and Capture_data do not match (e.g. HA-30262)
#'
#'In this case, we assume that the individual was captured multiple times and
#'the records in Nestling_data and Capture_data are both true capture records.
#'All information from both tables is kept.
#'
#'\item #4.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, corresponding record in Nestling_data that was taken at exactly
#'the same capture date (e.g. HA-30103)
#'
#'Where there are measurements (e.g. mass, tarsus) in both Nestling_data and
#'Capture_data, we give precedence to the data from Nestling_data. We only use
#'data from Capture_data when there is no data in Nestling_data
#'
#'\item #5.	Record of chick (Age is PM/PP) with information in the last ring
#'number column which matches data in Nestling_data (e.g. HL-26103 - 26107)
#'
#'We assume that there is a continuous ring series used (e.g. 26103, 26104,
#'26105, 26106, 26107) and link all corresponding records from Nestling_data
#'where the BroodID and last 2 digits of the ring number match.
#'
#'\item #6.	Record of chick (Age is PM/PP) with information in the last ring
#'number column but no matches to data in Nestling_data (e.g. HL-364911 -
#'364916).
#'
#'We assume the individuals were captured and ringed, but there was no data
#'collected on them (e.g. Mass). We give them an 'empty' record in Capture_data
#'(e.g. only IndvID, CaptureDate, CaptureTime).
#'
#'\item #7. Record in Nestling_data that has no ring number information (e.g. A,
#'B, C, D) (e.g. Brood 2011_0003_1)
#'
#'We assume these were chicks captured before they were old enough to be ringed.
#'We keep these as a legitimate capture record as they can tell us something
#'about chick growth.
#'
#'\item #8.	Record in Nestling_data that has ring number information, but there
#'is no corresponding record of chicks with those rings being captured from that
#'brood in Capture_data (e.g. Brood 1985_2081_1 with individuals 71, 72, 73,
#'74).
#'
#'We cannot determine the identity of the captured individual without a matching
#'record in Capture_data because we only know the last 2 digits of the ring
#'number. Currently, we include them as captures with IndvID = NA. These can
#'still be used to estimate AvgChickMass, AvgTarsus for a given BroodID even
#'though the individual cannot be identified; however, we suspect these are
#'mistakes in the primary data.}
#'
#'\strong{Mass}: Mass of birds is measured in mg. This is converted
#'to grams to match other populations.
#'
#'\strong{Tarsus}: Tarsus length is measured for both left and right leg.
#'Generally, only left leg is reported and so is used here. Tarsus measurement
#'in adults can be either left or right leg.
#'
#'\strong{Sex}: Bird classified as 'likely male' or 'likely female' are simply
#'given code 'M' and 'F' respectively (i.e. this uncertainty is ignored).
#'
#'\strong{BroodID}: Unique BroodID values are generated using
#'BreedingSeason_LocationID_BroodID
#'
#'@inheritParams pipeline_params
#'@param return_errors Logical (TRUE/FALSE). If true, return all records of
#'nestling with no corresponding ringing data.
#'
#'@return 4 data tables in the standard format (version 1.0.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_HAR <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R",
                       return_errors = FALSE){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.0.0"

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # BROOD DATA
  #Extract Harjavalta brood data

  message("Compiling brood data....")

  Brood_data <- create_brood_HAR(db = db, species_filter = species)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_HAR(db = db, Brood_data = Brood_data,
                                     species_filter = species, return_errors = return_errors)

  if(return_errors){

    return(Capture_data)

  }

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_HAR(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_HAR(db = db)

  #CURRENTLY ASSUMING THAT EACH LOCATION AND NEST BOX ARE IDENTICAL
  #GO THROUGH AND CHECK MORE THOROUGHLY

  # WRANGLE DATA FOR EXPORT

  ## Add average chick mass and tarsus to brood data

  Chick_avg <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%
    #Remove cases where tarsus or weight are 0 (make them NA)
    dplyr::mutate(Mass = dplyr::na_if(.data$Mass, 0),
                  Tarsus = dplyr::na_if(.data$Tarsus, 0)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgEggMass = NA,
                     NumberEggs = NA,
                     AvgChickMass = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass = dplyr::n(),
                     AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = "Alternative")

  #Join these into Brood_data
  #Only existing broods will be used.
  Brood_data <- dplyr::left_join(Brood_data, Chick_avg, by = "BroodID") %>%
    dplyr::select("BroodID":"NumberFledgedError", "AvgEggMass":"OriginalTarsusMethod", "ExperimentID")

  Capture_data <- Capture_data %>%
    dplyr::select(-"Sex", -"BroodID")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>% dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_HAR.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HAR.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_HAR.txt"),
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

#' Create brood data table for Harjavalta, Finland.
#'
#' Create brood data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_HAR <- function(db, species_filter){

  message("Extracting brood data from paradox database")

  #Extract brood data
  #Rename columns to English (based on description provided by Tapio Eeva)
  #Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  #see what is being removed.
  Brood_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Pesat.DB") %>%
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro",
                  "BroodID" = "Anro", "Species" = "Laji",
                  "ClutchType_observed" = "Pesa", "FemaleID" = "Naaras", "MaleID" = "Koiras",
                  "LayDate_day" = "Mpv", "LayDate_month" = "Mkk", "LayDateError" = "Mtar",
                  "HatchDate_day" = "Kpv", "HatchDate_month" = "Kkk", "HatchDateError" = "Ktar",
                  "Incubation" = "Halku", "ClutchSize" = "Mulu", "BroodSize" = "Kuor",
                  "NumberFledged" = "Lent", "ReasonFailed" = "Tsyy",
                  "NestlingInjuries" = "Jalat", "MalePresent" = "Koir",
                  "ExperimentID" = "Koe", "ExpData1" = "Olent",
                  "ExpData2" = "Vlent", "DeadParent" = "Delfh",
                  "EggShells" = "Mkuor", "TempCode1" = "Tark",
                  "TempCode2" = "Tark2") %>%
    #Remove unwanted columns
    dplyr::select(-"ReasonFailed":-"MalePresent", -"ExpData1":-"TempCode2") %>%
    #Create unique BroodID with year_locationID_BroodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$LocationID, .data$BroodID, sep = "_")) %>%
    #Convert species codes to letter codes
    dplyr::mutate(Species = dplyr::case_when(.data$Species == "FICHYP" ~ species_codes$Species[which(species_codes$SpeciesID == 13490)],
                                             .data$Species == "PARCAE" ~ species_codes$Species[which(species_codes$SpeciesID == 14620)],
                                             .data$Species == "PARMAJ" ~ species_codes$Species[which(species_codes$SpeciesID == 14640)],
                                             .data$Species == "PARATE" ~ species_codes$Species[which(species_codes$SpeciesID == 14610)])) %>%
    dplyr::filter(!is.na(.data$Species) & .data$Species %in% species_filter) %>%
    #Add pop and plot id
    dplyr::mutate(PopID = "HAR",
                  Plot = NA_character_) %>%
    #Adjust clutch type observed to meet our wording
    dplyr::mutate(ClutchType_observed = dplyr::case_when(.data$ClutchType_observed == 1 ~ "first",
                                                         .data$ClutchType_observed %in% c(2, 3, 6) ~ "replacement",
                                                         .data$ClutchType_observed == 5 ~ "second")) %>%
    #Create calendar date for laying date and hatch date
    dplyr::mutate(LayDate = as.Date(paste(.data$LayDate_day, .data$LayDate_month, .data$BreedingSeason, sep = "/"),
                                    format = "%d/%m/%Y"),
                  HatchDate  = as.Date(paste(.data$HatchDate_day, .data$HatchDate_month, .data$BreedingSeason, sep = "/"),
                                       format = "%d/%m/%Y")) %>%
    dplyr::arrange(.data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(LayDateError = dplyr::case_when(.data$LayDateError == "1" ~ 1L,
                                                  .data$LayDateError == "2" ~ 2L,
                                                  .data$LayDateError == "3" ~ 3L,
                                                  .data$LayDateError == "4" ~ 7L),
                  HatchDateError = dplyr::case_when(.data$HatchDateError == "1" ~ 1L,
                                                    .data$HatchDateError == "2" ~ 2L,
                                                    .data$HatchDateError == "3" ~ 3L,
                                                    .data$HatchDateError == "4" ~ 7L),
                  FledgeDate = as.Date(NA),
                  ClutchSizeError = NA_real_,
                  BroodSizeError = NA_real_,
                  FledgeDateError = NA_real_,
                  NumberFledgedError = NA_real_,
                  BroodSize = as.integer(.data$BroodSize)) %>%
    #Arrange columns correctly
    dplyr::select("BroodID", "PopID", "BreedingSeason", "Species", "Plot", "LocationID",
                  "FemaleID", "MaleID", "ClutchType_observed", "ClutchType_calculated",
                  "LayDate", "LayDateError", "ClutchSize", "ClutchSizeError",
                  "HatchDate", "HatchDateError", "BroodSize", "BroodSizeError",
                  "FledgeDate", "FledgeDateError", "NumberFledged", "NumberFledgedError",
                  "ExperimentID")

  ## Set non-conforming IDs to NA
  Brood_data <- Brood_data %>%
    dplyr::mutate(dplyr::across(c("FemaleID", "MaleID"),
                                ~dplyr::case_when(stringr::str_detect(., "^[:alpha:]{1,2}[-][:digit:]{5,6}$") ~ .,
                                                  TRUE ~ NA_character_)))

  return(Brood_data)

}

#' Create nestling data capture table for Harjavalta, Finland.
#'
#' Create nestling data capture table for data from Harjavalta, Finland. This is
#' used inside \code{\link{create_capture_HAR}}.
#'
#' @param Brood_data Output of \code{\link{create_brood_HAR}}.
#' @param db Location of primary data from Harjavalta, Finland.
#'
#' @return A data frame.

create_nestling_HAR <- function(db, Brood_data){

  message("Extracting nestling ringing data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Nestling_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Pullit.DB") %>%
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro", "BroodID" = "Anro",
                  "Month" = "Kk", "Day" = "Pv", "Time" = "Klo",
                  "NrNestlings" = "Poik", "Last2DigitsRingNr" = "Reng",
                  "Dead" = "Dead", "WingLength" = "Siipi",
                  "Mass" = "Paino", "LeftLegAbnormal" = "Vjalka",
                  "RightLegAbnormal" = "Ojalka", "Left3Primary" = "Vkas",
                  "Right3Primary" = "Okas", "LeftRectrix" = "Vpys",
                  "RightRectrix" = "Opys", "LeftTarsusLength" = "Vnil",
                  "RightTarsusLength" = "Onil", "LeftTarsusWidth" = "Vpak",
                  "RightTarsusWidth" = "Opak", "GTBreastYellow" = "Vari",
                  "Lutein" = "Lkoe", "BloodSample" = "Wb",
                  "ColLengthBlood" = "Tot", "LengthBlood" = "Pun",
                  "BreastFeatherLutein" = "FetLut",
                  "NailClipping" = "Varpaat", "Sex" = "Sp",
                  "HeadLength" = "Head")

  #Remove unwanted columns
  Nestling_data <- Nestling_data %>%
    dplyr::select("BreedingSeason":"Mass", "LeftTarsusLength", "Sex") %>%
    #Create unique broodID (BreedingSeason_LocationID_BroodID)
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$LocationID, .data$BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(.data$Day, .data$Month, .data$BreedingSeason, sep = "/"),
                                        format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(.data$Time, ":00"), "NA:00")) %>%
    #Join hatch date data from brood data table
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate"),
                     by = "BroodID") %>%
    #Determine age at capture
    dplyr::mutate(ChickAge = as.integer(.data$CaptureDate - .data$HatchDate))

  return(Nestling_data)

}

#' Create capture table for Harjavalta, Finland.
#'
#' Create full capture data table in standard format for data from Harjavalta, Finland.
#'
#' @param Brood_data Output of \code{\link{create_brood_HAR}}.
#' @param db Location of primary data from Harjavalta.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#' @param return_errors Logical. Return those records with errors in the ring sequence.
#'
#' @return A data frame.

create_capture_HAR    <- function(db, Brood_data, species_filter, return_errors){

  Nestling_data <- create_nestling_HAR(db = db, Brood_data = Brood_data)

  message("Extracting capture data from paradox database")

  #Extract table "Rengas.db" which contains adult ringing data
  ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
  #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
  # 662470, 662471, 662472, 662473
  # The number of nestlings ringed is stored in NrNestlings.
  Capture_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Rengas.DB") %>%
    dplyr::rename("RingSeries" = "Sarja", "RingNumber" = "Mista",
                  "CaptureType" = "Tunnus", "BreedingSeason" = "Vuos",
                  "Month" = "Kk", "Day" = "Pv", "Time" = "Klo",
                  "LocationID" = "Nuro", "BroodID" = "Anro",
                  "ObserverID" = "Havno", "LastRingNumber" = "Mihin",
                  "Species" = "Laji", "Sex" = "Suku",
                  "Sex_method" = "Sp", "Age" = "Ika",
                  "Age_method" = "Ip", "RingType" = "Rtapa",
                  "Condition" = "Kunto", "BirdStatus" = "Tila",
                  "CaptureMethod" = "Ptapa", "NrNestlings" = "Poik",
                  "WingLength" = "Siipi", "Mass" = "Paino", "Moult" = "Sulsat",
                  "FatScore" = "Rasik", "ExtraInfo" = "Lisa",
                  "Plumage" = "Vari", "TailFeather" = "Psulka",
                  "ColLengthBlood" = "Tot",
                  "LengthBlood" = "Pun", "BreastMuscle" = "Lihas",
                  "HeadLength" = "Head")


  Capture_data <- Capture_data %>%
    #Create unique broodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$LocationID, .data$BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(.data$Day, .data$Month, .data$BreedingSeason, sep = "/"),
                                        format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(.data$Time, ":00"), "NA:00")) %>%
    #Convert species codes to EUring codes and then remove only the major species
    dplyr::mutate(Species = dplyr::case_when(.data$Species == "FICHYP" ~ species_codes$Species[which(species_codes$SpeciesID == 13490)],
                                             .data$Species == "PARCAE" ~ species_codes$Species[which(species_codes$SpeciesID == 14620)],
                                             .data$Species == "PARMAJ" ~ species_codes$Species[which(species_codes$SpeciesID == 14640)],
                                             .data$Species == "PARATE" ~ species_codes$Species[which(species_codes$SpeciesID == 14610)]),
                  ) %>%
    dplyr::filter(!is.na(.data$Species) & .data$Species %in% species_filter) %>%
    dplyr::mutate(Sex = dplyr::case_when(.data$Sex %in% c("N", "O") ~ "F",
                                         .data$Sex %in% c("K", "L") ~ "M"),
                  Mass = dplyr::na_if(.data$Mass, 0),
                  WingLength = dplyr::na_if(.data$WingLength, 0))

  #We have eight scenarios we need to deal with in the capture data.
  #See an explanation in the help documentation:

  ####

  #1. Individual adult captures
  Adult_capture <- Capture_data %>%
    dplyr::filter(!.data$Age %in% c("PP", "PM") | is.na(.data$Age)) %>%
    dplyr::mutate(Capture_type = "Adult",
                  Last2DigitsRingNr = NA,
                  ChickAge = NA,
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  ####

  message("Determining chick ring numbers...")

  #Isolate only chick captures
  chick_data <- dplyr::filter(Capture_data, .data$Age %in% c("PP", "PM"))

  #2-4. Individual chick captures
  indv_chick_capture <- chick_data %>%
    dplyr::filter(is.na(.data$LastRingNumber))

  #2. No information in Nestling data
  #In this case, it doesn't matter whether mass/wing length is NA, they are still legit capture records
  indv_chick_capture_only <- indv_chick_capture %>%
    #Filter only those where the BroodID_RingNumber combo is not found in Nestling data
    dplyr::filter(!paste(.data$BroodID, stringr::str_sub(.data$RingNumber, start = -2), sep = "_") %in% paste(Nestling_data$BroodID, Nestling_data$Last2DigitsRingNr, sep = "_")) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate"),
                     by = "BroodID") %>%
    #Join in Brood_data (with HatchDate) so we can determine ChickAge
    dplyr::mutate(Capture_type = "Chick",
                  Last2DigitsRingNr = NA,
                  ChickAge = as.integer(.data$CaptureDate - .data$HatchDate),
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  ####

  #3 - 4. Individual chick captures with that also have records in Nestling data
  #These can be 3) where the records were at different dates. In thise case, both records are used
  # or 4) where the records are on the same date. In this case, nestling record is used
  #unless data are missing

  #First, identify those cases where the individual chick record is in both tables
  non_matching_records <- indv_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2),
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::semi_join(Nestling_data %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "CaptureDateNestling" = "CaptureDate"),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::pull("IndvID")

  #Next, go through capture data records and return all the cases where these
  #chicks had a record in capture data that didn't match anything in nestling data
  unique_individual_captures <- indv_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2),
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::filter(.data$IndvID %in% non_matching_records) %>%
    dplyr::anti_join(Nestling_data %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "CaptureDate"),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate"), by = "BroodID") %>%
    dplyr::mutate(ChickAge = as.integer(.data$CaptureDate - .data$HatchDate)) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  #Go through the nestling data records and do the same thing
  unique_individual_nestlings <- Nestling_data %>%
    dplyr::left_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2)) %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "RingSeries", "RingNumber", "Species"),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::filter(.data$IndvID %in% non_matching_records) %>%
    dplyr::anti_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2)) %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "CaptureDate"),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::mutate(ObserverID = NA_character_,
                  Sex = NA_character_,
                  Age = "PP",
                  CaptureType = "5",
                  BirdStatus = NA_character_) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  indv_chick_multirecord <- dplyr::bind_rows(unique_individual_captures, unique_individual_nestlings)

  #Go through and find the exact matches
  indv_chick_record_conflict <- indv_chick_capture %>%
    #Join Nestling data and filter those cases where the same date is present
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2),
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::left_join(Nestling_data %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr",
                                     "CaptureDateNestling" = "CaptureDate",
                                     "CaptureTimeNestling" = "CaptureTime", "MassNestling" = "Mass",
                                     "WingLengthNestling" = "WingLength", "HatchDate", "ChickAge"),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    #Filter those cases that are individual captures (i.e. no last ring number)
    #Find cases where there was mass and/or wing length in the nestling data
    #AND where the CaptureDate in the Nestling data is different to Capture_data
    dplyr::filter(.data$CaptureDate == .data$CaptureDateNestling) %>%
    dplyr::mutate(Mass = dplyr::case_when(is.na(.data$MassNestling) ~ .data$Mass,
                                          TRUE ~ .data$MassNestling),
                  WingLength = dplyr::case_when(is.na(.data$WingLengthNestling) ~ .data$WingLength,
                                                TRUE ~ .data$WingLengthNestling)) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  ####

  #5. Cases where the record in capture is for multiple chicks
  #The individual data is stored in nestling data
  flat_multi_chick_capture <- chick_data %>%
    dplyr::filter(!is.na(.data$LastRingNumber))

  expanded_multi_chick_capture <- flat_multi_chick_capture %>%
    # Determine series of chick ring numbers
    dplyr::mutate(RingNumber = purrr::map2(.x = .data$RingNumber,
                                           .y = .data$LastRingNumber,
                                           .f = ~ {

                                             # Determine ring series
                                             ring_string <- paste0(.x, ":", .y)

                                             # Parse text
                                             ring_series <- eval(parse(text = ring_string))

                                             # Pad chick ring numbers with leading 0
                                             output <-  stringr::str_pad(as.character(ring_series),
                                                                         width = nchar(.x),
                                                                         side = "left",
                                                                         pad = 0)

                                             # Set chick IDs to NA if the list of ring numbers is unlikely large
                                             # TODO: Check with data custodian.
                                             # Four cases seem to have typos. RingNumber:
                                             # - 403881
                                             # - 564023
                                             # - 956423
                                             # - 418760
                                             if(length(output) > 14) {

                                               output <- NA_character_

                                             }

                                             return(output)

                                           })) %>%
    tidyr::unnest(cols = "RingNumber") %>%
    dplyr::select("LocationID", "BroodID", "RingSeries", "RingNumber",
                  "BreedingSeason", "Species", "Sex", "Age",
                  "ObserverID", "CaptureType", "BirdStatus") %>%
    dplyr::distinct()


  #Remove names so that it doesn't break tests
  #We need to come up with a more robust fix for this (i.e. change the above code!)
  expanded_multi_chick_capture <- expanded_multi_chick_capture %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(~!is.null(names(.))),
                                .fns = ~stats::setNames(., NULL)))

  #Now, for each recorded chick ring number determine the last 2 digits of the ring
  multirecord_captures <- expanded_multi_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2),
                  Capture_type = "Chick") %>%
    #Join in all nestling data where the broodID and Last2Digits is the same
    #N.B. We do left_join with BroodID and Last2Digits, so we can get multiple
    #records for each chick when they were captured more than once
    dplyr::left_join(Nestling_data %>%
                       dplyr::select("BroodID", "CaptureDate", "CaptureTime",
                                     "Last2DigitsRingNr", "WingLength",
                                     "Mass", "ChickAge"),
                     by = c("BroodID", "Last2DigitsRingNr"),
                     ## TODO: Check BroodID 2005_0210_1 with data custodian
                     # 1 record with LastRing 707300 & 1 record with LastRing 707699
                     relationship = "many-to-many") %>%
    dplyr::mutate(IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "Last2DigitsRingNr", "ChickAge")

  ####

  #6. Nestling records that don't correspond to any capture data
  #Brood/RingNumber combos from individual captures
  single_capture_records <- indv_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(.data$BroodID,
                                       stringr::str_sub(.data$RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull("Brood_RingNr")

  #Brood/RingNumber combos from multi-captures
  multi_capture_records <- expanded_multi_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(.data$BroodID,
                                       stringr::str_sub(.data$RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull("Brood_RingNr")

  nocapture_nestlings <- Nestling_data %>%
    dplyr::mutate(Brood_RingNr = paste(.data$BroodID, .data$Last2DigitsRingNr, sep = "_")) %>%
    dplyr::filter(!.data$Brood_RingNr %in% c(single_capture_records, multi_capture_records))

  #6a. Filter unringed individuals. These are just given records associated with a brood where the individual ID is unknown.
  unringed_chicks <- nocapture_nestlings %>%
    dplyr::filter(toupper(.data$Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_,
                  ObserverID = NA_character_,
                  Sex = NA_character_,
                  Age = "PP",
                  CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "Species"),
                     by = "BroodID") %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "ChickAge")

  ringed_chicks_nocapture <- nocapture_nestlings %>%
    dplyr::filter(!toupper(.data$Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_,
                  ObserverID = NA_character_,
                  Sex = NA_character_,
                  Age = "PP",
                  CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "Species"),
                     by = "BroodID") %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "ChickAge")

  message(paste0("There are ", nrow(ringed_chicks_nocapture), " nestling records where we cannot translate Last2Digits into an IndvID"))

  if(return_errors){

    return(nocapture_nestlings %>%
             dplyr::filter(!toupper(.data$Last2DigitsRingNr) %in% LETTERS))

  }

  ####

  #Now that we have dealt with all 6 scenarios, we can join the data back together.
  Capture_data_expanded <- dplyr::bind_rows(Adult_capture, indv_chick_capture_only, indv_chick_multirecord,
                                            indv_chick_record_conflict, multirecord_captures, unringed_chicks,
                                            ringed_chicks_nocapture)

  ####

  message("Calculating age at each capture...")

  Capture_data_output <- Capture_data_expanded %>%
    #Make Age_observed, that doesn't require any calculation, just uses observations at the time of capture
    #PP and PM = 1, these are chicks in nest and chicks outside the nest but not yet able to fly
    #1 = 3 (able to fly but in first year),
    #1+ = 4 (hatched atleast 1 year ago), 2 = 5 (known to have hatched last year),
    #2+ = 6 (hatched at least 2 years ago)
    #FL is 2. We cannot even distinguish it as adult or chick. We just know it's fully grown.
    dplyr::mutate(Mass = .data$Mass/10,
                  CapturePopID = "HAR",
                  CapturePlot = NA_character_,
                  ReleasePopID = "HAR",
                  ReleasePlot = NA_character_,
                  Age_observed = dplyr::case_when(.data$Age %in% c("PM", "PP") ~ 1L,
                                                  .data$Age == "FL" ~ 2L,
                                                  .data$Age == "1" ~ 3L,
                                                  .data$Age == "+1" ~ 4L,
                                                  .data$Age == "2" ~ 5L,
                                                  .data$Age == "+2" ~ 6L)) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::mutate(Tarsus = NA_real_,
                  OriginalTarsusMethod = NA_character_,
                  ## Set non-conforming IDs to NA
                  IndvID = dplyr::case_when(stringr::str_detect(.data$IndvID, "^[:alpha:]{1,2}[-][:digit:]{5,6}$") ~ .data$IndvID,
                                            TRUE ~ NA_character_)) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot", "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated", "ChickAge",
                  "Sex", "BroodID", "CaptureType", "BirdStatus") %>%
    #Remove duplicates that can arise from cases when CaptureDate is the same in Capture and Nestling
    dplyr::distinct()

  return(Capture_data_output)

}

#' Create individual table for Harjavalta, Finland.
#'
#' Create full individual data table in standard format for data from Harjavalta, Finland.
#'
#' @param Capture_data Output of \code{\link{create_capture_HAR}}.
#'
#' @return A data frame.

create_individual_HAR <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                              .f = ~{

                                                if(length(..1) == 0){

                                                  return(NA_character_)

                                                } else if(length(..1) == 1){

                                                  return(..1)

                                                } else {

                                                  return("CONFLICTED")

                                                }

                                              }),
                     PopID = "HAR",
                     BroodIDLaid = dplyr::first(.data$BroodID),
                     BroodIDFledged = .data$BroodIDLaid,
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = ifelse(any(.data$Age_calculated %in% c(1, 3)), "chick",
                                      ifelse(min(.data$Age_calculated) == 2, NA_character_, "adult")),
                     Sex = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex))),
                                          .f = ~{

                                            if(length(..1) == 0){

                                              return(NA_character_)

                                            } else if(length(..1) == 1){

                                              return(..1)

                                            } else {

                                              return("C")

                                            }

                                          })) %>%
    dplyr::rowwise() %>%
    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(.data$RingAge == "chick", .data$BroodIDLaid, NA_character_),
                  BroodIDFledged = .data$BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(.data$RingSeason, .data$IndvID)

  return(Indv_data)

}

#' Create location table for Harjavalta, Finland.
#'
#' Create full location data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#'
#' @return A data frame.
#' @export

create_location_HAR <- function(db){

  message("Extracting location data from paradox database")

  # Extract table "Paikat.db" which contains location data
  Location_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Paikat.DB") %>%
    #Remove last 2 cols that have no info
    dplyr::select(-"Aukko", -"Malli") %>%
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro",
                  "ForestType" = "Mety", "PinusSylvestris" = "Manty",
                  "PiceaAbies" = "Kuusi", "Betulasp" = "Koivu",
                  "PopulusTremula" = "Haapa",
                  "SorbusAcuparia" = "Pihlaja",
                  "Salixsp" = "Pajut", "JuniperusCommunis" = "Kataja",
                  "Alnussp" = "Leppa", "PrunusPadas" = "Tuomi",
                  "TreeHeight" = "Kork", "BasalArea" = "Totrel",
                  "PineHeight" = "Makor", "SpruceHeight" = "Kukor",
                  "BirchHeight" = "Kokor", "PineBasalArea" = "Marel",
                  "SpruceBasalArea" = "Kurel", "BirchBasalArea" = "Korel",
                  "Latitude" = "Leve", "Longitude" = "Pitu", "Municipality" = "Kunta",
                  "LocationName" = "Paikka")

  #Separate locations with and without coordinates
  Location_nocoord <- Location_data %>%
    dplyr::filter(is.na(.data$Longitude))

  Location_wcoord  <- Location_data %>%
    dplyr::filter(!is.na(.data$Longitude))

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #Coordinates are in Finland Uniform Coordinate System (EPSG 2393)

  Location_data_sf <- sf::st_as_sf(Location_wcoord,
                                   coords = c("Longitude", "Latitude"),
                                   crs = 2393) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_rows(
    dplyr::bind_cols(Location_wcoord %>%
                       dplyr::select(-"Longitude", -"Latitude"),
                     tibble::tibble(Longitude = sf::st_coordinates(Location_data_sf)[, 1]),
                     tibble::tibble(Latitude = sf::st_coordinates(Location_data_sf)[, 2])),
    Location_nocoord)

  #The records of each Location should show the start/end season
  Location_data <- Location_full %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::arrange(.data$BreedingSeason, .by_group = TRUE) %>%
    dplyr::summarise(NestboxID = unique(.data$LocationID),
                     PopID = "HAR",
                     Latitude = as.numeric(dplyr::first(.data$Latitude)),
                     Longitude = as.numeric(dplyr::first(.data$Longitude)),
                     LocationType = "NB",
                     StartSeason = min(.data$BreedingSeason),
                     EndSeason = max(.data$BreedingSeason),
                     Habitat = "Evergreen") %>%
    dplyr::select("LocationID", "NestboxID", "LocationType", "PopID",
                  "Latitude", "Longitude", "StartSeason", "EndSeason", "Habitat")

  return(Location_data)

}
