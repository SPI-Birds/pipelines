#'Construct standard format for data from Kevo, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Kevo, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species:} We only include records for great tit, pied flycatcher,
#'Siberan tit (Parus cinctus), and Common redstart (Phoenicurus phoenicurus).
#'All these species have at least 350 capture records. There are also records of
#'blue tits, spotted flycatcher (Muscicapa striata), willow tit (Parus
#'montanus), and Eurasian wryneck (Jynx torquilla); however, these species are
#'captured at most 13 times over >30 years. Therefore, they are excluded.
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
#'\strong{Capture data:} As with Harjavalta, linking nestling and adult capture
#'data can be difficult. There are eight different scenarios we need to
#'consider. Each of these is described below, with our solution: \itemize{ \item
#'#1. Individual captured as adult (Age is NA or not PM/PP) (e.g. HL-137181).
#'
#'In this case, we assume that there is only information in Capture_data, and we
#'do not search for any information in Nestling_data.
#'
#'\item #2. Record of chick (Age is PM/PP) with no information in the last ring
#'number column AND no corresponding record in Nestling_data (i.e. no record
#'where BroodID and last 2 digits of the ring number were the same) (e.g.
#'HL-568201).
#'
#'In this case, we assume that the information in Capture_data is the correct
#'capture information.
#'
#'\item #3.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, with corresponding record in Nestling_data, BUT where the
#'capture dates in Nestling_data and Capture_data do not match (e.g. V-478577)
#'
#'In this case, we assume that the individual was captured multiple times and
#'the records in Nestling_data and Capture_data are both true capture records.
#'All information from both tables is kept.
#'
#'\item #4.	Record of chick (Age is PM/PP) with no information in the last ring
#'number column, corresponding record in Nestling_data that was taken at exactly
#'the same capture date (e.g. JL-112052)
#'
#'Where there are measurements (e.g. mass, tarsus) in both Nestling_data and
#'Capture_data, we give precedence to the data from Capture_data. We only use
#'data from Nestling_data when there is no data in Capture_data
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
#'@inheritParams pipeline_params
#'@param return_errors Logical. Should capture data include those individuals with
#'errors in ring sequences?
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_KEV <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R",
                       return_errors = FALSE){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # BROOD DATA

  message("\n Compiling brood data....")

  Brood_data <- create_brood_KEV(db = db, species_filter = species)

  # CAPTURE DATA

  message("\n Compiling capture data....")

  Capture_data <- create_capture_KEV(db = db, Brood_data = Brood_data,
                                     species_filter = species, return_errors = return_errors)

  if(return_errors){

    return(Capture_data)

  }

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_KEV(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_KEV(db = db)

  # WRANGLE DATA FOR EXPORT

  ## Add average chick mass and tarsus to brood data
  Chick_avg <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%
    #Remove cases where tarsus or weight are 0 (make them NA)
    dplyr::mutate(Mass = dplyr::na_if(.data$Mass, 0),
                  Tarsus = dplyr::na_if(.data$Tarsus, 0)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA,
                     NumberEggs = NA,
                     AvgChickMass = mean(.data$Mass, na.rm = T)/10,
                     NumberChicksMass = dplyr::n(),
                     AvgTarsus = mean(.data$Tarsus, na.rm = T),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = "Alternative")

  ##Determine parents of every brood
  Parents <- Capture_data %>%
    dplyr::filter(.data$BirdStatus == "P" & !is.na(.data$Sex)) %>%
    dplyr::select("BroodID", "IndvID", "Sex") %>%
    dplyr::distinct()

  #There can be cases where an brood has multiple female/male parents
  #This may be due to mis-sexing or typos in IndvID records (e.g. V-622002 v. V-622022)
  #We check these are report them
  double_parents <- Parents %>%
    dplyr::group_by(.data$BroodID, .data$Sex) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data$n > 1)

  NA_broods <- double_parents$BroodID

  message(paste0("There are ", nrow(double_parents), " broods where there is more than one possible parent"))

  #In these cases, we just say we can't know the parents and leave it as NA
  Parents <- Parents %>%
    dplyr::filter(!.data$BroodID %in% NA_broods) %>%
    tidyr::pivot_wider(names_from = "Sex", values_from = "IndvID") %>%
    dplyr::rename("FemaleID" = "F", "MaleID" = "M")

  #Join avg measurements and parents to brood data
  Brood_data <- dplyr::left_join(Brood_data, Chick_avg, by = "BroodID") %>%
    dplyr::left_join(Parents, by = "BroodID") %>%
    #Now we can do clutchtype_calculated (we couldn't do this until we have FemaleID)
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select("BroodID", "PopID", "BreedingSeason", "Species", "Plot", "LocationID",
                  "FemaleID", "MaleID", "ClutchType_observed", "ClutchType_calculated",
                  "LayDate", "LayDateError", "ClutchSize", "ClutchSizeError",
                  "HatchDate", "HatchDateError", "BroodSize", "BroodSizeError",
                  "FledgeDate", "FledgeDateError", "NumberFledged", "NumberFledgedError",
                  "AvgEggMass", "NumberEggs",
                  "AvgChickMass", "NumberChicksMass", "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod", "ExperimentID")

  Capture_data <- Capture_data %>%
    dplyr::select(-"Sex", -"BroodID", -"CaptureType", -"BirdStatus")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_KEV.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_KEV.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>% dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_KEV.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_KEV.csv"), row.names = FALSE)

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

#' Create brood data table for Kevo, Finland.
#'
#' Create brood data table in standard format for data from Kevo, Finland.
#'
#' @param db Location of primary data from Kevo
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_KEV <- function(db, species_filter){

  message("Extracting brood data from paradox database")

  #Extract table "Pesat.db" which contains brood data
  Brood_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Nests.DB") %>%
    #Rename columns to English (based on description provided by data owner)
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro",
                  "BroodID" = "Anro", "Species" = "Laji",
                  "ClutchType_observed" = "Pesa", "Habitat_Type_simple" = "Mety",
                  "Dist_to_humans" = "Dist", "Altitude" = "Mpy",
                  "Habitat_Type_detailed" = "Puut", "Habitat_Type_detailed2" = "Vart",
                  "LayDate_day" = "Mpv", "LayDate_month" = "Mkk", "LayDateError" = "Mtar",
                  "HatchDate_day" = "Kpv", "HatchDate_month" = "Kkk", "HatchDateError" = "Ktar",
                  "Incubation" = "Halku", "ClutchSize" = "Mulu", "BroodSize" = "Kuor",
                  "NumberRinged" = "Reng", "NumberFledged" = "Lent", "ReasonFailed" = "Tsyy")

  Brood_data <- Brood_data %>%
    #Create unique BroodID with year_locationID_BroodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$LocationID, .data$BroodID, sep = "_")) %>%
    #Convert species codes to letter codes
    dplyr::mutate(Species = dplyr::case_when(.data$Species == "FICHYP" ~ species_codes$Species[which(species_codes$SpeciesID == 13490)],
                                             .data$Species == "PARMAJ" ~ species_codes$Species[which(species_codes$SpeciesID == 14640)],
                                             .data$Species == "PARCIN" ~ species_codes$Species[which(species_codes$SpeciesID == 14480)],
                                             .data$Species == "PHOPHO" ~ species_codes$Species[which(species_codes$SpeciesID == 11220)])) %>%
    dplyr::filter(!is.na(.data$Species) & .data$Species %in% species_filter) %>%
    #Add pop and plot id
    dplyr::mutate(PopID = "KEV",
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
                  BroodSize = as.integer(.data$BroodSize),
                  ExperimentID = NA_character_)

  return(Brood_data)

}

#' Create nestling data capture table for Kevo, Finland.
#'
#' Create nestling data capture table for data from Kevo, Finland. This is
#' used inside \code{\link{create_capture_HAR}}.
#'
#' @param Brood_data Output of \code{\link{create_brood_KEV}}.
#' @param db Location of primary data from Kevo, Finland.
#'
#' @return A data frame.

create_nestling_KEV <- function(db, Brood_data){

  message("Extracting nestling ringing data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Nestling_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Nestlings.DB") %>%
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro",
                  "BroodID" = "Anro", "Month" = "Kk", "Day" = "Pv", "Time" = "Klo",
                  "NrNestlings" = "Poik", "Last2DigitsRingNr" = "Reng",
                  "WingLength" = "Siipi", "Mass" = "Paino",
                  "MassType" = "Totpain")

  #Remove unwanted columns
  Nestling_data <- Nestling_data %>%
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
    dplyr::mutate(ChickAge = as.integer(.data$CaptureDate - .data$HatchDate),
                  Mass = dplyr::na_if(.data$Mass, 0),
                  WingLength = dplyr::na_if(.data$WingLength, 0))

  return(Nestling_data)

}

#' Create capture table for Kevo, Finland.
#'
#' Create full capture data table in standard format for data from Kevo, Finland.
#'
#' @param Brood_data Output of \code{\link{create_brood_KEV}}.
#' @param db Location of primary data from Kevo.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#' @param return_errors Logical. Return those records with errors in the ring sequence.
#'
#' @return A data frame.

create_capture_KEV <- function(db, Brood_data, species_filter, return_errors){

  Nestling_data <- create_nestling_KEV(db = db, Brood_data = Brood_data)

  message("Extracting capture data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Capture_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Ringings.DB") %>%
    #Change colnames to English to make data management more understandable
    ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
    #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
    # 662470, 662471, 662472, 662473
    # The number of nestlings ringed is stored in NrNestlings.
    dplyr::rename("CaptureType" = "Tunnus", "RingSeries" = "Sarja",
                  "RingNumber" = "Mista", "LastRingNumber" = "Mihin",
                  "BreedingSeason" = "Vuos", "Month" = "Kk", "Day" = "Pv", "Time" = "Klo",
                  "ObserverID" = "Havno", "LocationID" = "Nuro", "BroodID" = "Anro",
                  "Species" = "Laji", "Sex" = "Suku", "Sex_method" = "Sp",
                  "Age" = "Ika", "Age_method" = "Ip", "Condition" = "Kunto",
                  "BirdStatus" = "Tila", "CaptureMethod" = "Ptapa",
                  "NrNestlings" = "Poik", "WingLength" = "Siipi",
                  "Mass" = "Paino", "Moult" = "Sulsat",
                  "FatScore" = "Rasika")

  Capture_data <- Capture_data %>%
    #Create unique broodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$LocationID, .data$BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(.data$Day, .data$Month, .data$BreedingSeason, sep = "/"),
                                        format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(.data$Time, ":00"), "NA:00")) %>%
    #Convert species codes to EURING codes and then remove only the major species
    dplyr::mutate(Species = dplyr::case_when(.data$Species == "FICHYP" ~ species_codes$Species[which(species_codes$SpeciesID == 13490)],
                                             .data$Species == "PARMAJ" ~ species_codes$Species[which(species_codes$SpeciesID == 14640)],
                                             .data$Species == "PARCIN" ~ species_codes$Species[which(species_codes$SpeciesID == 14480)],
                                             .data$Species == "PHOPHO" ~ species_codes$Species[which(species_codes$SpeciesID == 11220)])) %>%
    dplyr::filter(!is.na(.data$Species) & .data$Species %in% species_filter) %>%
    dplyr::mutate(Sex = dplyr::case_when(.data$Sex %in% c("N", "O") ~ "F",
                                         .data$Sex %in% c("K", "L") ~ "M"),
                  Mass = dplyr::na_if(.data$Mass, 0),
                  WingLength = dplyr::na_if(.data$WingLength, 0))

  #We have eight scenarios we need to deal with in the capture data.
  #See an explanation in the help documentation:

  ####

  #1. Individual adult captures
  Adult_capture    <- Capture_data %>%
    dplyr::filter(!.data$Age %in% c("PP", "PM") | is.na(.data$Age)) %>%
    dplyr::mutate(Capture_type = "Adult",
                  Last2DigitsRingNr = NA,
                  ChickAge = NA) %>%
    dplyr::mutate(IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
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
    dplyr::mutate(Brood_ring = paste(.data$BroodID, stringr::str_sub(.data$RingNumber, start = -2), sep = "_")) %>%
    dplyr::anti_join(Nestling_data %>%
                       dplyr::mutate(Brood_ring = paste(.data$BroodID, .data$Last2DigitsRingNr, sep = "_")) %>%
                       dplyr::select("Brood_ring"),
                     by = "Brood_ring") %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate"), by = "BroodID") %>%
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
                       dplyr::select("BroodID", "HatchDate"),
                     by = "BroodID") %>%
    dplyr::mutate(ChickAge = as.integer(.data$CaptureDate - .data$HatchDate)) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species",
                  "Sex", "Age", "WingLength", "Mass", "CaptureType", "BirdStatus", "ChickAge")

  #Go through the nestling data records and do the same thing
  unique_individual_nestlings <- Nestling_data %>%
    dplyr::left_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2)) %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "RingSeries", "RingNumber", "Species"),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::filter(IndvID %in% non_matching_records) %>%
    dplyr::anti_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2)) %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "CaptureDate"),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::mutate(ObserverID = NA_character_,
                  Sex = NA_character_,
                  Age = "PP",
                  CaptureType = "5",
                  BirdStatus = NA_character_) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species",
                  "Sex", "Age", "WingLength", "Mass", "CaptureType", "BirdStatus", "ChickAge")

  indv_chick_multirecord <- dplyr::bind_rows(unique_individual_captures, unique_individual_nestlings)

  #Go through and find the exact matches
  indv_chick_record_conflict <- indv_chick_capture %>%
    #Join Nestling data and filter those cases where the same date is present
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(.data$RingNumber, start = -2),
                  IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::left_join(Nestling_data %>%
                       dplyr::select("BroodID", "Last2DigitsRingNr", "CaptureDateNestling" = "CaptureDate",
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
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species",
                  "Sex", "Age", "WingLength", "Mass", "CaptureType", "BirdStatus", "ChickAge")

  ####

  #5 - 6. Cases where the record in capture is for multiple chicks
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
                                             # One cases seem to have typos. RingNumber:
                                             # - 815079

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
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(.data$RingSeries, .data$RingNumber, sep = "-")) %>%
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "BirdStatus", "ChickAge")

  ####

  #7 - 8. Nestling records that don't correspond to any capture data
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

  #7. Filter unringed individuals. These are just given records associated with a brood where the individual ID is unknown.
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
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species", "Sex", "Age",
                  "WingLength", "Mass", "CaptureType", "ChickAge")

  #8. Filter ringed individuals with no capture records.
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
    dplyr::select("IndvID", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "BroodID", "Species", "Sex", "Age",
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
    dplyr::mutate(Mass = .data$Mass/10,
                  CapturePopID = "KEV",
                  CapturePlot = NA_character_,
                  ReleasePopID = "KEV",
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
                  OriginalTarsusMethod = NA_character_) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot", "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated", "ChickAge", "Sex",
                  "BroodID", "CaptureType", "BirdStatus") %>%
    dplyr::distinct()

  return(Capture_data_output)

}

#' Create individual table for Kevo, Finland.
#'
#' Create full individual data table in standard format for data from Kevo, Finland.
#'
#' @param Capture_data Output of \code{\link{create_capture_HAR}}.
#'
#' @return A data frame.

create_individual_KEV <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(Species))),
                                              .f = ~{

                                                if(length(..1) == 0){

                                                  return(NA_character_)

                                                } else if(length(..1) == 1){

                                                  return(..1)

                                                } else {

                                                  return("CONFLICTED")

                                                }

                                              }),
                     PopID = "KEV",
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

#' Create location table for Kevo, Finland.
#'
#' Create full location data table in standard format for data from Kevo, Finland.
#'
#' @param db Location of primary data from Kevo.
#'
#' @return A data frame.
#' @export

create_location_KEV <- function(db){

  message("Extracting location data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Location_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Locations.DB") %>%
    dplyr::rename("BreedingSeason" = "Vuos", "LocationID" = "Nuro",
                  "Latitude" = "Leve", "Longitude" = "Pitu",
                  "BoxMaterial" = "Pontlaji",
                  "NestboxTree" = "Puulaji",
                  "NestboxCavityArea" = "Pohja",
                  "NestboxHoleDiameter" = "Aukko",
                  "Municipality" = "Kunta",
                  "Plot" = "Paikka")

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #Coordinates are in Finland Uniform Coordinate System (EPSG 2393)
  Location_data_sf <- sf::st_as_sf(Location_data,
                                   coords = c("Longitude", "Latitude"),
                                   crs = 2393) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_cols(Location_data %>% dplyr::select(-"Longitude", -"Latitude"),
                                    tibble::tibble(Longitude = sf::st_coordinates(Location_data_sf)[, 1]),
                                    tibble::tibble(Latitude = sf::st_coordinates(Location_data_sf)[, 2]))

  #In Kevo, it seems each row is a record of a nestbox in a given year
  #This should tell us about nest box changes over time and Start/EndSeason
  #Need to check with Tapio.
  Location_data <- Location_full %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::summarise(NestboxID = unique(.data$LocationID),
                     PopID = "KEV",
                     Latitude = as.numeric(dplyr::first(.data$Latitude)),
                     Longitude = as.numeric(dplyr::first(.data$Longitude)),
                     LocationType = "NB",
                     StartSeason = min(.data$BreedingSeason),
                     EndSeason = max(.data$BreedingSeason),
                     Habitat = "Mixed") %>%
    dplyr::select("LocationID", "NestboxID", "LocationType", "PopID",
                  "Latitude", "Longitude", "StartSeason", "EndSeason", "Habitat")

  return(Location_data)

}
