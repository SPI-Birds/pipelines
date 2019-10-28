#'Construct standard format for data from Harjavalta, Finland.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
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
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_HAR <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_HAR(db = db, species_filter = species)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_HAR(db = db, Brood_data = Brood_data, species_filter = species)

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
    dplyr::filter(dplyr::between(ChickAge, 14, 16)) %>%
    #Remove cases where tarsus or weight are 0 (make them NA)
    dplyr::mutate(Mass = dplyr::na_if(Mass, 0),
                  Tarsus = dplyr::na_if(Tarsus, 0)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA, NumberEggs = NA,
                     AvgChickMass = mean(Mass, na.rm = TRUE),
                     NumberChicksMass = n(),
                     AvgTarsus = mean(Tarsus, na.rm = TRUE),
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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_HAR.csv"), row.names = F)

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

#' Create brood data table for Harjavalta, Finland.
#'
#' Create brood data table in standard format for data from Harjavalta, Finland.
#'
#' @param db Location of primary data from Harjavalta.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_HAR <- function(db, species_filter){

  message("Extracting brood data from paradox database")

  #Extract brood data
  #Rename columns to English (based on description provided by Tapio Eeva)
  #Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  #see what is being removed.
  Brood_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Nests.DB") %>%
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  BroodID = Anro, Species = Laji,
                  ClutchType_observed = Pesa, FemaleID = Naaras, MaleID = Koiras,
                  LayDate_day = Mpv, LayDate_month = Mkk, LayDateError = Mtar,
                  HatchDate_day = Kpv, HatchDate_month = Kkk, HatchDateError = Ktar,
                  Incubation = Halku, ClutchSize = Mulu, BroodSize = Kuor,
                  NumberFledged = Lent, ReasonFailed = Tsyy,
                  NestlingInjuries = Jalat, MalePresent = Koir,
                  ExperimentID = Koe, ExpData1 = Olent,
                  ExpData2 = Vlent, DeadParent = Delfh,
                  EggShells = Mkuor, TempCode1 = Tark,
                  TempCode2 = Tark2) %>%
    #Remove unwanted columns
    dplyr::select(-ReasonFailed:-MalePresent, -ExpData1:-TempCode2) %>%
    #Create unique BroodID with year_locationID_BroodID
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_")) %>%
    #Convert species codes to letter codes
    dplyr::mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                      Species == "PARCAE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                      Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                      Species == "PARATE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)])) %>%
    dplyr::filter(!is.na(Species) & Species %in% species_filter) %>%
    #Add pop and plot id
    dplyr::mutate(PopID = "HAR", Plot = NA) %>%
    #Adjust clutch type observed to meet our wording
    dplyr::mutate(ClutchType_observed = dplyr::case_when(ClutchType_observed == 1 ~ "first",
                                                  ClutchType_observed %in% c(2, 3, 6) ~ "replacement",
                                                  ClutchType_observed == 5 ~ "second")) %>%
    #Create calendar date for laying date and hatch date
    dplyr::mutate(LayDate = as.Date(paste(LayDate_day, LayDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
           HatchDate  = as.Date(paste(HatchDate_day, HatchDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y")) %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayDate) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(LayDateError = dplyr::case_when(LayDateError == "1" ~ 1L,
                                                  LayDateError == "2" ~ 2L,
                                                  LayDateError == "3" ~ 3L,
                                                  LayDateError == "4" ~ 7L),
                  HatchDateError = dplyr::case_when(HatchDateError == "1" ~ 1L,
                                                    HatchDateError == "2" ~ 2L,
                                                    HatchDateError == "3" ~ 3L,
                                                    HatchDateError == "4" ~ 7L),
                  FledgeDate = as.Date(NA), ClutchSizeError = NA_real_, BroodSizeError = NA_real_,
                  FledgeDateError = NA_real_, NumberFledgedError = NA_real_,
                  BroodSize = as.integer(BroodSize)) %>%
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
  Nestling_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Nestlings.DB") %>%
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro, BroodID = Anro,
                  Month = Kk, Day = Pv, Time = Klo,
                  NrNestlings = Poik, Last2DigitsRingNr = Reng,
                  Dead = Dead, WingLength = Siipi,
                  Mass = Paino, LeftLegAbnormal = Vjalka,
                  RightLegAbnormal = Ojalka, Left3Primary = Vkas,
                  Right3Primary = Okas, LeftRectrix = Vpys,
                  RightRectrix = Opys, LeftTarsusLength = Vnil,
                  RightTarsusLength = Onil, LeftTarsusWidth = Vpak,
                  RightTarsusWidth = Opak, GTBreastYellow = Vari,
                  Lutein = Lkoe, BloodSample = Wb,
                  ColLengthBlood = Tot, LengthBlood = Pun,
                  BreastFeatherLutein = FetLut,
                  NailClipping = Varpaat, Sex = Sp,
                  HeadLength = Head)

  #Remove unwanted columns
  Nestling_data <- Nestling_data %>%
    dplyr::select(BreedingSeason:Mass, LeftTarsusLength, Sex) %>%
    #Create unique broodID (BreedingSeason_LocationID_BroodID)
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(Time, ":00"), "NA:00")) %>%
    #Join hatch date data from brood data table
    dplyr::left_join(select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    #Determine age at capture
    dplyr::mutate(ChickAge = as.integer(CaptureDate - HatchDate))

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
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_capture_HAR    <- function(db, Brood_data, species_filter){

  Nestling_data <- create_nestling_HAR(db = db, Brood_data = Brood_data)

  message("Extracting capture data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
  #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
  # 662470, 662471, 662472, 662473
  # The number of nestlings ringed is stored in NrNestlings.
  Capture_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Ringings.DB") %>%
    dplyr::rename(RingSeries = Sarja, RingNumber = Mista,
                  CaptureType = Tunnus, BreedingSeason = Vuos,
                  Month = Kk, Day = Pv, Time = Klo,
                  LocationID = Nuro, BroodID = Anro,
                  ObserverID = Havno, LastRingNumber = Mihin,
                  Species = Laji, Sex = Suku,
                  Sex_method = Sp, Age = Ika,
                  Age_method = Ip, RingType = Rtapa,
                  Condition = Kunto, BirdStatus = Tila,
                  CaptureMethod = Ptapa, NrNestlings = Poik,
                  WingLength = Siipi, Mass = Paino, Moult = Sulsat,
                  FatScore = Rasik, ExtraInfo = Lisa,
                  Plumage = Vari, TailFeather = Psulka,
                  ColLengthBlood = Tot,
                  LengthBlood = Pun, BreastMuscle = Lihas,
                  HeadLength = Head)

  Capture_data <- Capture_data %>%
    #Create unique broodID
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(Time, ":00"), "NA:00")) %>%
    #Convert species codes to EUring codes and then remove only the major species
    dplyr::mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                      Species == "PARCAE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                      Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                      Species == "PARATE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)]),
                  ) %>%
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

  #3 - 4. Individual chick captures with that also have records in Nestling data
  #These can be 3) where the records were at different dates. In thise case, both records are used
  # or 4) where the records are on the same date. In this case, nestling record is used
  #unless data are missing

  #First, identify those cases where the individual chick record is in both tables
  non_matching_records <- indv_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::semi_join(Nestling_data %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, CaptureDateNestling = CaptureDate),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::pull(IndvID)

  #Next, go through capture data records and return all the cases where these
  #chicks had a record in capture data that didn't match anything in nestling data
  unique_individual_captures <- indv_chick_capture %>%
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::filter(IndvID %in% non_matching_records) %>%
    dplyr::anti_join(Nestling_data %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, CaptureDate),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::left_join(Brood_data %>%
                       select(BroodID, HatchDate), by = "BroodID") %>%
    dplyr::mutate(ChickAge = as.integer(CaptureDate - HatchDate)) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
                  Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  #Go through the nestling data records and do the same thing
  unique_individual_nestlings <- Nestling_data %>%
    dplyr::left_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2)) %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, RingSeries, RingNumber, Species),
                     by = c("BroodID", "Last2DigitsRingNr")) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::filter(IndvID %in% non_matching_records) %>%
    dplyr::anti_join(indv_chick_capture %>%
                       dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2)) %>%
                       dplyr::select(BroodID, Last2DigitsRingNr, CaptureDate),
                     by = c("BroodID", "Last2DigitsRingNr", "CaptureDate")) %>%
    dplyr::mutate(ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = "5",
                  BirdStatus = NA_character_) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
                  Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  indv_chick_multirecord <- dplyr::bind_rows(unique_individual_captures, unique_individual_nestlings)

  #Go through and find the exact matches
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
    dplyr::filter(CaptureDate == CaptureDateNestling) %>%
    dplyr::mutate(Mass = purrr::map2_dbl(.x = .$Mass, .y = .$MassNestling, .f = ~{

      if(is.na(..2)){

        return(..1)

      } else {

        return(..2)

      }

    }),
    WingLength = purrr::map2_dbl(.x = .$WingLength, .$WingLengthNestling,
                                 ~{

                                   if(is.na(..2)){

                                     return(..1)

                                   } else {

                                     return(..2)

                                   }

                                 })) %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species,
                  Sex, Age, WingLength, Mass, CaptureType, BirdStatus, ChickAge)

  ####

  #5. Cases where the record in capture is for multiple chicks
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

  #6. Nestling records that don't correspond to any capture data
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

  #6a. Filter unringed individuals. These are just given records associated with a brood where the individual ID is unknown.
  unringed_chicks <- nocapture_nestlings %>%
    dplyr::filter(toupper(Last2DigitsRingNr) %in% LETTERS) %>%
    dplyr::mutate(IndvID = NA_character_, ObserverID = NA_character_, Sex = NA_character_, Age = "PP", CaptureType = NA_character_) %>%
    #Join in Species information from Brood_data
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    dplyr::select(IndvID, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, BroodID, Species, Sex, Age, WingLength, Mass, CaptureType, ChickAge)

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
    #Make Age_observed, that doesn't require any calculation, just uses observations at the time of capture
    #PP and PM = 1, these are chicks in nest and chicks outside the nest but not yet able to fly
    #1 = 3 (able to fly but in first year),
    #1+ = 4 (hatched atleast 1 year ago), 2 = 5 (known to have hatched last year),
    #2+ = 6 (hatched at least 2 years ago)
    #FL is 2. We cannot even distinguish it as adult or chick. We just know it's fully grown.
    dplyr::mutate(Mass = Mass/10,
                  CapturePopID = "HAR", CapturePlot = NA_character_,
                  ReleasePopID = "HAR", ReleasePlot = NA_character_,
                  Age_observed = dplyr::case_when(Age %in% c("PM", "PP") ~ 1L,
                                                  Age == "FL" ~ 2L,
                                                  Age == "1" ~ 3L,
                                                  Age == "+1" ~ 4L,
                                                  Age == "2" ~ 5L,
                                                  Age == "+2" ~ 6L)) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::mutate(Tarsus = NA_real_, OriginalTarsusMethod = NA_character_) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge, Sex, BroodID, CaptureType, BirdStatus) %>%
    #Remove duplicates that can arise from cases when CaptureDate is the same in Capture and Nestling
    dplyr::distinct()

  return(Capture_data_output)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  LastRingNumber_Brood <- Age <- NrNestlings <- RingNumber <- NULL
  Capture_type <- Last2DigitsRingNr <- Month <- Wing <- LeftTarsusLength <- NULL
  `.` <- Ring_Time <- Final_BreedingSeason <- Final_Month <- Final_Day <- Final_Time <- Day <- ischick <- NULL

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

    }), PopID = "HAR",
              BroodIDLaid = first(BroodID),
              BroodIDFledged = BroodIDLaid,
              RingSeason = first(BreedingSeason),
              RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
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
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
           BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

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

  #Extract table "Pullit.db" which contains brood data
  Location_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Locations.DB") %>%
    #Remove last 2 cols that have no info
    dplyr::select(-Aukko, -Malli) %>%
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  ForestType = Mety, PinusSylvestris = Manty,
                  PiceaAbies = Kuusi, Betulasp = Koivu,
                  PopulusTremula = Haapa,
                  SorbusAcuparia = Pihlaja,
                  Salixsp = Pajut, JuniperusCommunis = Kataja,
                  Alnussp = Leppa, PrunusPadas = Tuomi,
                  TreeHeight = Kork, BasalArea = Totrel,
                  PineHeight = Makor, SpruceHeight = Kukor,
                  BirchHeight = Kokor, PineBasalArea = Marel,
                  SpruceBasalArea = Kurel, BirchBasalArea = Korel,
                  Latitude = Leve, Longitude = Pitu, Municipality = Kunta,
                  LocationName = Paikka)

  #Separate locations with and without coordinates
  Location_nocoord <- Location_data %>%
    dplyr::filter(is.na(Longitude))

  Location_wcoord  <- Location_data %>%
    dplyr::filter(!is.na(Longitude))

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #Coordinates are in Finland Uniform Coordinate System (EPSG 2393)

  Location_data_sf <- sf::st_as_sf(Location_wcoord,
                                         coords = c("Longitude", "Latitude"),
                                         crs = 2393) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_rows(dplyr::bind_cols(dplyr::select(Location_wcoord, -Longitude, -Latitude),
                                    tibble(Longitude = sf::st_coordinates(Location_data_sf)[, 1]),
                                    tibble(Latitude = sf::st_coordinates(Location_data_sf)[, 2])),
                                    Location_nocoord)

  #The records of each Location should show the start/end season
  Location_data <- Location_full %>%
    dplyr::group_by(LocationID) %>%
    dplyr::arrange(BreedingSeason, .by_group = TRUE) %>%
    dplyr::summarise(NestboxID = unique(LocationID), PopID = "HAR", Latitude = as.numeric(first(Latitude)), Longitude = as.numeric(first(Longitude)),
                     LocationType = "NB", StartSeason = min(BreedingSeason), EndSeason = max(BreedingSeason), Habitat = "Evergreen") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
