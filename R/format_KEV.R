#'Construct standard format for data from Kevo, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Kevo, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species:} We only include records for great tit, pied flycatcher,
#'Siberan tit (Parus cinctus), and Common redstart (Phoenicurus phoenicurus).
#'All these species have at least 350 capture records. There are also records of
#'blue tits, spotted flycatcher (Muscicapa striata), willow tit (Parus
#'montanus), and Eurasian wryneck (Jynx torquilla); however, these species are
#'captured at most 13 times over >30 years. Therefore, they are excluded.
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
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_KEV <- function(db = utils::choose.dir(),
                       species = NULL,
                         pop = NULL,
                         path = ".",
                         debug = FALSE,
                         output_type = "csv"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # BROOD DATA

  message("\n Compiling brood data....")

  Brood_data <- create_brood_KEV(db = db, species_filter = species)

  # CAPTURE DATA

  message("\n Compiling capture data....")

  Capture_data <- create_capture_KEV(db = db, Brood_data = Brood_data, species_filter = species)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_KEV(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_KEV(db = db)

  # WRANGLE DATA FOR EXPORT

  ## Add average chick mass and tarsus to brood data

  Chick_avg <- Capture_data %>%
    dplyr::filter(dplyr::between(ChickAge, 14, 16)) %>%
    #Remove cases where tarsus or weight are 0 (make them NA)
    dplyr::mutate(Mass = dplyr::na_if(Mass, 0),
                  Tarsus = dplyr::na_if(Tarsus, 0)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA, NumberEggs = NA,
                     AvgChickMass = mean(Mass, na.rm = T)/10,
                     NumberChicksMass = n(),
                     AvgTarsus = mean(Tarsus, na.rm = T),
                     NumberChicksTarsus = n(),
                     OriginalTarsusMethod = "Alternative")

  #Join these into Brood_data
  #Only existing broods will be used.
  Brood_data <- left_join(Brood_data, Chick_avg, by = "BroodID") %>%
    dplyr::select(BroodID:NumberFledgedError, AvgEggMass:OriginalTarsusMethod, ExperimentID)

  Capture_data <- Capture_data %>%
    dplyr::select(-Sex, -BroodID)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_KEV.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_KEV.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HAR.csv"), row.names = F)

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
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_KEV <- function(db, species_filter){

  message("Extracting brood data from paradox database")

  #Extract table "Pesat.db" which contains brood data
  Brood_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Nests.DB") %>%
    #Rename columns to English (based on description provided by data owner)
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  BroodID = Anro, Species = Laji,
                  ClutchType_observed = Pesa, Habitat_Type_simple = Mety,
                  Dist_to_humans = Dist, Altitude = Mpy,
                  Habitat_Type_detailed = Puut, Habitat_Type_detailed2 = Vart,
                  LayDate_day = Mpv, LayDate_month = Mkk, LayDateError = Mtar,
                  HatchDate_day = Kpv, HatchDate_month = Kkk, HatchDateError = Ktar,
                  Incubation = Halku, ClutchSize = Mulu, BroodSize = Kuor,
                  NumberRinged = Reng, NumberFledged = Lent, ReasonFailed = Tsyy)

  Brood_data <- Brood_data %>%
    #Create unique BroodID with year_locationID_BroodID
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_")) %>%
    #Convert species codes to letter codes
    dplyr::mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                             Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             Species == "PARCIN" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14480)],
                                             Species == "PHOPHO" ~ Species_codes$Code[which(Species_codes$SpeciesID == 11220)])) %>%
    dplyr::filter(!is.na(Species) & Species %in% species_filter) %>%
    #Add pop and plot id
    dplyr::mutate(PopID = "KEV", Plot = NA) %>%
    #Adjust clutch type observed to meet our wording
    dplyr::mutate(ClutchType_observed = dplyr::case_when(ClutchType_observed == 1 ~ "first",
                                                         ClutchType_observed %in% c(2, 3, 6) ~ "replacement",
                                                         ClutchType_observed == 5 ~ "second")) %>%
    #Create calendar date for laying date and hatch date
    dplyr::mutate(LayDate = as.Date(paste(LayDate_day, LayDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  HatchDate  = as.Date(paste(HatchDate_day, HatchDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  FemaleID = NA_character_, MaleID = NA_character_) %>%
    #ClutchType_calculated will only be first or replacement because we don't know FemaleID.
    #Need to ask Tapio about this
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(LayDateError = as.numeric(LayDateError),
                  HatchDateError = as.numeric(HatchDateError),
                  FledgeDate = as.Date(NA), ClutchSizeError = NA_real_, BroodSizeError = NA_real_,
                  FledgeDateError = NA_real_, NumberFledgedError = NA_real_,
                  BroodSize = as.integer(BroodSize), ExperimentID = NA_character_) %>%
    #Arrange columns correctly
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated, LayDate, LayDateError,
                  ClutchSize, ClutchSizeError, HatchDate, HatchDateError,
                  BroodSize, BroodSizeError, FledgeDate, FledgeDateError, NumberFledged, NumberFledgedError,
                  ExperimentID)

  return(Brood_data)

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL
  ReasonFailed <- MalePresent <- ExpData1 <- TempCode2 <- NULL
  LayDate_day <- LayDate_month <- HatchDate_day <- HatchDate_month <- NULL

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
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  BroodID = Anro, Month = Kk, Day = Pv, Time = Klo,
                  NrNestlings = Poik, Last2DigitsRingNr = Reng,
                  WingLength = Siipi, Mass = Paino,
                  MassType = Totpain)

  #Remove unwanted columns
  Nestling_data <- Nestling_data %>%
    #Create unique broodID (BreedingSeason_LocationID_BroodID)
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(Time, ":00"), "NA:00")) %>%
    #Join hatch date data from brood data table
    dplyr::left_join(select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    #Determine age at capture
    dplyr::mutate(ChickAge = as.integer(CaptureDate - HatchDate),
                  Mass = dplyr::na_if(Mass, 0),
                  WingLength = dplyr::na_if(WingLength, 0))

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
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_capture_KEV    <- function(db, Brood_data, species_filter){

  Nestling_data <- create_nestling_KEV(db = db, Brood_data = Brood_data)

  message("Extracting capture data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Capture_data <- extract_paradox_db(path = db, file_name = "KEV_PrimaryData_Ringings.DB") %>%
    #Change colnames to English to make data management more understandable
    ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
    #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
    # 662470, 662471, 662472, 662473
    # The number of nestlings ringed is stored in NrNestlings.
    dplyr::rename(CaptureType = Tunnus, RingSeries = Sarja,
                  RingNumber = Mista, LastRingNumber = Mihin,
                  BreedingSeason = Vuos, Month = Kk, Day = Pv, Time = Klo,
                  ObserverID = Havno, LocationID = Nuro, BroodID = Anro,
                  Species = Laji, Sex = Suku, Sex_method = Sp,
                  Age = Ika, Age_method = Ip, Condition = Kunto,
                  BirdStatus = Tila, CaptureMethod = Ptapa,
                  NrNestlings = Poik, WingLength = Siipi,
                  Mass = Paino, Moult = Sulsat,
                  FatScore = Rasika)

  Capture_data <- Capture_data %>%
    #Create unique broodID
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(Time, ":00"), "NA:00")) %>%
    #Convert species codes to EURING codes and then remove only the major species
    dplyr::mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                             Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             Species == "PARCIN" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14480)],
                                             Species == "PHOPHO" ~ Species_codes$Code[which(Species_codes$SpeciesID == 11220)])) %>%
    dplyr::filter(!is.na(Species) & Species %in% species_filter) %>%
    dplyr::mutate(Sex = dplyr::case_when(Sex %in% c("N", "O") ~ "F",
                                         Sex %in% c("K", "L") ~ "M"),
                  Mass = dplyr::na_if(Mass, 0),
                  WingLength = dplyr::na_if(WingLength, 0))

  #We have eight scenarios we need to deal with in the capture data.
  #See an explanation in the help documentation:

  ####

  #1. Individual adult captures
  Adult_capture    <- Capture_data %>%
    dplyr::filter(!Age %in% c("PP", "PM") | is.na(Age)) %>%
    dplyr::mutate(Capture_type = "Adult", Last2DigitsRingNr = NA, ChickAge = NA) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, Last2DigitsRingNr, ChickAge)

  ####

  message("Determining chick ring numbers...")

  #Isolate only chick captures
  chick_data <- filter(Capture_data, Age %in% c("PP", "PM"))

  #2-4. Individual chick captures
  indv_chick_capture <- chick_data %>%
    dplyr::filter(is.na(LastRingNumber))

  #2. No information in Nestling data
  #In this case, it doesn't matter whether mass/wing length is NA, they are still legit capture records
  indv_chick_capture_only <- indv_chick_capture %>%
    #Filter only those where the BroodID_RingNumber combo is not found in Nestling data
    dplyr::filter(!paste(BroodID, stringr::str_sub(RingNumber, start = -2), sep = "_") %in% paste(Nestling_data$BroodID, Nestling_data$Last2DigitsRingNr, sep = "_")) %>%
    dplyr::left_join(select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    #Join in Brood_data (with HatchDate) so we can determine ChickAge
    dplyr::mutate(Capture_type = "Chick", Last2DigitsRingNr = NA,
                  ChickAge = as.integer(CaptureDate - HatchDate),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, Last2DigitsRingNr, ChickAge)

  ####

  #3. Individual chick captures with separate records in Nestling data
  #In this case, it doesn't matter whether mass/wing length is NA, they are still legit capture records
  indv_chick_multirecord_combined <- indv_chick_capture %>%
    #Join Nestling data.
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::left_join(dplyr::select(Nestling_data, BroodID, Last2DigitsRingNr, CaptureDateNestling = CaptureDate,
                                   CaptureTimeNestling = CaptureTime, MassNestling = Mass, WingLengthNestling = WingLength, HatchDate, ChickAgeNestling = ChickAge),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    #Filter those cases where it's an individual capture (i.e. no last ring number)
    #Find cases where the date of capture between the two is different
    dplyr::filter(CaptureDate != CaptureDateNestling) %>%
    #Make a chick age for the capture date in the capture data using hatch date as well
    dplyr::mutate(ChickAge = as.integer(CaptureDate - HatchDate)) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureType, BirdStatus, ObserverID, LocationID, BroodID, Species, Sex, Age,
                  WingLength, Mass, CaptureDate, CaptureTime, CaptureDateNestling, CaptureTimeNestling, MassNestling, WingLengthNestling, ChickAgeNestling, ChickAge)

  #There is not a 1 to 1 relationship. There are often more nestling records than there are capture records
  #Therefore, we need to separate out the records from capture and nestling data tables
  indv_chick_multirecord_capture_data <- indv_chick_multirecord_combined %>%
    dplyr::select(-contains("Nestling")) %>%
    dplyr::filter(!duplicated(.))

  indv_chick_multirecord_nestling_data <- indv_chick_multirecord_combined %>%
    dplyr::select(IndvID:Age, contains("Nestling")) %>%
    dplyr::rename_at(.vars = vars(contains("Nestling")), .funs = ~{

      stringr::str_replace(string = ..1, pattern = "Nestling", replacement = "")

    })

  #Combine these two together
  indv_chick_multirecord <- dplyr::bind_rows(indv_chick_multirecord_capture_data, indv_chick_multirecord_nestling_data) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #4. Individuals with measurement records in both capture and nestling.
  #Where measurements are present in both we assume that the capture data record takes precedence.
  #Where measurements are NA in capture, we give nestling data precedence
  indv_chick_record_conflict <- indv_chick_capture %>%
    #Join Nestling data and filter those cases where the same date is present
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::left_join(dplyr::select(Nestling_data, BroodID, Last2DigitsRingNr, CaptureDateNestling = CaptureDate,
                                   CaptureTimeNestling = CaptureTime, MassNestling = Mass, WingLengthNestling = WingLength, HatchDate, ChickAge),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    #Filter those cases that are individual captures (i.e. no last ring number)
    #Find cases where there was mass and/or wing length in the nestling data
    #AND where the CaptureDate in the Nestling data is different to Capture_data
    dplyr::filter(is.na(LastRingNumber) & CaptureDate == CaptureDateNestling) %>%
    dplyr::mutate(Mass = purrr::map2_dbl(.x = .$Mass, .y = .$MassNestling, .f = ~{

                    if(is.na(..1)){

                      return(..2)

                    } else {

                      return(..1)

                    }

                  }),
                  WingLength = purrr::map2_dbl(.x = .$WingLength, .$WingLengthNestling,
                  ~{

                    if(is.na(..1)){

                      return(..2)

                    } else {

                      return(..1)

                    }

                  })) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #5 - 6. Cases where the record in capture is for multiple chicks
  #The individual data is stored in nestling data
  flat_multi_chick_capture <- chick_data %>%
    dplyr::filter(!is.na(LastRingNumber))

  ring_pb <- dplyr::progress_estimated(n = nrow(flat_multi_chick_capture))

  expanded_multi_chick_capture <- flat_multi_chick_capture %>%
    #Split data into rowwise list
    purrr::pmap(~c(...)) %>%
    #map over each row
    purrr::map_dfr(function(current_row){

      ring_pb$print()$tick()

      #Determine first part of Ringnumber
      ring_start <- stringr::str_sub(current_row["RingNumber"], end = -6)

      #We use the last 5 digits (rather than last 2) to deal with
      #cases where the ring series passes 10000 (e.g. 9999 - 0000).
      ring_ends  <- stringr::str_sub(current_row["RingNumber"], start = -5):stringr::str_sub(current_row["LastRingNumber"], start = -5)
      #Pad numbers with leading 0s to ensure they're all the right length
      ring_ends  <- stringr::str_pad(ring_ends, 5, pad = "0")

      #Create a list of all chicks in the series
      All_rings <- paste0(ring_start, ring_ends)

      return(tibble::tibble(LocationID = current_row["LocationID"], BroodID = current_row["BroodID"], RingSeries = current_row["RingSeries"],
                            RingNumber = All_rings, BreedingSeason = as.integer(current_row["BreedingSeason"]),
                            Species = current_row["Species"], Sex = current_row["Sex"],
                            Age = current_row["Age"], ObserverID = current_row["ObserverID"],
                            CaptureType = current_row["CaptureType"], BirdStatus = current_row["BirdStatus"]))

                   })

    #Now, for each recorded chick ring number determine the last 2 digits of the ring
    multirecord_captures <- expanded_multi_chick_capture %>%
      dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  Capture_type = "Chick") %>%
    #Join in all nestling data where the broodID and Last2Digits is the same
    #N.B. We do left_join with BroodID and Last2Digits, so we can get multiple
    #records for each chick when they were captured more than once
    dplyr::left_join(Nestling_data %>% select(BroodID, CaptureDate, CaptureTime,
                                              Last2DigitsRingNr, WingLength,
                                              Mass, ChickAge), by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #7 - 8. Nestling records that don't correspond to any capture data
  #Brood/RingNumber combos from individual captures
  single_capture_records <- indv_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, stringr::str_sub(RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull(Brood_RingNr)

  #Brood/RingNumber combos from multi-captures
  multi_capture_records <- expanded_multi_chick_capture %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, stringr::str_sub(RingNumber, start = -2), sep = "_")) %>%
    dplyr::pull(Brood_RingNr)

  nocapture_nestlings <- Nestling_data %>%
    dplyr::mutate(Brood_RingNr = paste(BroodID, Last2DigitsRingNr, sep = "_")) %>%
    dplyr::filter(!Brood_RingNr %in% c(single_capture_records, multi_capture_records))

  #7. Filter unringed individuals. These are just given records associated with a brood where the individual ID is unknown.
  unringed_chicks <- nocapture_nestlings %>%
    dplyr::filter(toupper(Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_, ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, ChickAge)

  #8. Filter ringed individuals with no capture records.
  ringed_chicks_nocapture <- nocapture_nestlings %>%
    dplyr::filter(!toupper(Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_, ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, ChickAge)

  message(paste0("There are ", nrow(ringed_chicks_nocapture), " nestling records where we cannot translate Last2Digits into an IndvID"))

  ####

  #Now that we have dealt with all 6 scenarios, we can join the data back together.
  Capture_data_expanded <- dplyr::bind_rows(Adult_capture, indv_chick_capture_only, indv_chick_multirecord,
                                            indv_chick_record_conflict, multirecord_captures, unringed_chicks, ringed_chicks_nocapture)

  ####

  message("Calculating age at each capture...")

  Capture_data_output <- Capture_data_expanded %>%
    dplyr::mutate(Mass = Mass/10,
                  CapturePopID = "KEV", CapturePlot = NA_character_,
                  ReleasePopID = "KEV", ReleasePlot = NA_character_,
                  ischick = dplyr::case_when(Age == "PP" ~ 1L,
                                             Age %in% c("PM", "FL") ~ 3L)) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    #Make Age_observed, that doesn't require any calculation, just uses observations at the time of capture
    #Assume that PP = 1, PM/FL = 3 (known to hatch this year), 1 = 5 (known to hatch last year),
    #1+ = 4 (hatched atleast 1 year ago), 2 = 7 (known to hatch 2 years ago),
    #2+ = 6 (hatched at least 2 years ago)
    dplyr::mutate(Age_observed = dplyr::case_when(Age %in% c("PM", "FL") ~ 3,
                                                  Age == "PP" ~ 1,
                                                  Age == "1" ~ 5,
                                                  Age == "+1" ~ 4,
                                                  Age == "2" ~ 7,
                                                  Age == "+2" ~ 6),
                  Tarsus = NA_real_, OriginalTarsusMethod = NA_character_) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge, Sex, BroodID, CaptureType, BirdStatus)

  return(Capture_data_output)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  LastRingNumber_Brood <- Age <- NrNestlings <- RingNumber <- NULL
  Capture_type <- Last2DigitsRingNr <- Month <- Wing <- LeftTarsusLength <- NULL
  `.` <- Ring_Time <- Final_BreedingSeason <- Final_Month <- Final_Day <- Final_Time <- Day <- ischick <- NULL

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
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }), PopID = "KEV",
    BroodIDLaid = first(BroodID), BroodIDFledged = BroodIDLaid,
    RingSeason = first(BreedingSeason), RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", "adult"),
    Sex = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{

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
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA_character_),
                  BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

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
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  Latitude = Leve, Longitude = Pitu,
                  BoxMaterial = Pontlaji,
                  NestboxTree = Puulaji,
                  NestboxCavityArea = Pohja,
                  NestboxHoleDiameter = Aukko,
                  Municipality = Kunta,
                  Plot = Paikka)

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #We use zone 4 for now, because it seems to get the German point in the right place...but this really doesn't work!!
  Location_data_sf_zone4 <- sf::st_as_sf(Location_data,
                                         coords = c("Longitude", "Latitude"),
                                         crs = 2394) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_cols(dplyr::select(Location_data, -Longitude, -Latitude),
                                                     tibble(Longitude = sf::st_coordinates(Location_data_sf_zone4)[, 1]),
                                                     tibble(Latitude = sf::st_coordinates(Location_data_sf_zone4)[, 2]))

  #In Kevo, it seems each row is a record of a nestbox in a given year
  #This should tell us about nest box changes over time and Start/EndSeason
  #Need to check with Tapio.
  Location_data <- Location_full %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestboxID = unique(LocationID), PopID = "KEV", Latitude = as.numeric(first(Latitude)), Longitude = as.numeric(first(Longitude)),
                     LocationType = "NB", StartSeason = min(BreedingSeason), EndSeason = max(BreedingSeason), Habitat = "Mixed") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
