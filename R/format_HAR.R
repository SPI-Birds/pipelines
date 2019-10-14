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
#'\strong{Age}: Chick age is listed as: PP (nestling), PM (fledgling), 1, 1+, 2,
#'2+. The numbers are said to follow the EURING system. I'm not sure which
#'version of the EURING code this would be. I assume that 1 = first year bird,
#'1+ = >first year bird, 2 = second year bird, 2+ = >second year bird. It's not
#'clear to me the distinction between PP, PM, and 1. For now, only PP and PM are
#'counted as being caught as chicks (i.e. with exact hatching year known),
#'everything else is assumed to be an estimate. This does mean that we have some
#'cases where the Age_calculated is 1 and the Age_observed is >= 5.
#'
#'\strong{LayDateError & HatchDateError}: Accuracy of laying and hatch date
#'are given as categories: 0-1; 1-2; 2-3; >3 (N.B. Need to check this with
#'Tapio). More conservative error is used (i.e. 0-1 is recorded as 1). For now
#'>3 is recorded as 4. N.B. Need to check this with Tapio!!
#'
#'\strong{Capture_data}: Combining capture data is fairly difficult with this
#'dataset: \itemize{ \item All data from adults when first ringed and recaptured
#'are stored in the 'Ringing' table (Rengas.db). This is regular capture data
#'that we can use without adjustment.
#'
#'\item All data from chick captures are stored in the 'Nestling' table
#'(Pullit.db). HOWEVER, the 'Nestling' table does not include information on the
#'ring number nor the species of each individual, only the BroodID and the last
#'2 digits of the ring number.
#'
#'\item When chicks were ringed for the first time, the ringing of ALL the
#'chicks is recorded as one entry in 'Ringing'. The first and last ring number
#'of the chicks ringed in the nest is given. Therefore, to determine the
#'individual ID of every chick capture from 'Nestling' we need to link it to the
#'correct BroodID and determine its ring number by taking the first x-2
#'characters of the given ring numbers and adding the last 2 digits from the
#''Nestling' table.
#'
#'\item Some chicks in 'Nestling' were captured and measured, but no last 2
#'digits of ring number is provided, I assume these were not ringed (e.g. too
#'young). These data can be associated with a Brood and species, but not with an
#'individual.
#'
#'\item HOWEVER, the 'Nestling' and 'Ringing' tables don't always match. In some
#'cases (>5000), there is a record of chicks being ringed in the 'Ringing'
#'table, but no record of these chicks in the 'Nestling' table (e.g. BroodID:
#'2005_1908_1 or 2018_1216_1).
#'
#'\item In some cases (~50), there is information on chicks being captured and
#'given a ring number in the 'Nestling' table, but no record of this capture in
#'the 'Ringing' table (e.g. BroodID: 2008_0222_1 or 2018_1378_2).
#'
#'\item In some cases, there is a record of chicks being ringed in 'Ringing',
#'but many of these chicks are missing in the 'Nestling' table (e.g. BroodID:
#'2007_1714_1).
#'
#'\item In some cases, chicks were recorded in 'Ringing' but have no matching
#'BroodID in Brood_data (e.g. 2004_1012_0).
#'
#'\item In some cases, one BroodID is used in the 'Ringing' table and another in
#'the 'Nestling' table even though the ring numbers are the same (e.g.
#'2018_0323_1/2). I think this conflict of BroodID is what causes most of the
#'problems above.
#'
#'}
#'
#'How do we deal with all these conflicts? For now, if a chick was listed in the
#''Ringing' table but has no record in the 'Nestlings' table, we assume it was
#'ringed but no measurements were taken (e.g. tarsus) or the measurements were
#'not found because the BroodID was entered incorrectly. If a chick was captured
#'in 'Nestlings' but is not recorded in 'Ringing' for now these will be excluded
#'because they won't join to any corresponding data in 'Ringing'. These cases
#'are the most problematic because to give these chicks a true ring number we
#'need to know the rest of the ring number (not just last 2 digits). If they
#'have no record in the Ringing table, it is impossible to include this
#'information! For records where no ring number is given (e.g. A), we include
#'only those where the BroodID is in either the 'Ringing' table or the 'Brood'
#'table. This ensures that the unringed chick is of the right species.
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

  # GENERATE DEBUG REPORT

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "HAR", Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Indv_data = Individual_data)

  }

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
    #Treat all NAs as true unknowns (check with Tapio that these are NAs and not 0s)
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayDate) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(LayDateError = as.numeric(LayDateError),
                  HatchDateError = as.numeric(HatchDateError),
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
                  HeadLength = Head) %>%
    #Create unique broodID
    dplyr::mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_"),
                  CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
                  CaptureTime = dplyr::na_if(paste0(Time, ":00"), "NA:00")) %>%
    #Convert species codes to EUring codes and then remove only the major species
    dplyr::mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                      Species == "PARCAE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                      Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                      Species == "PARATE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)])) %>%
    dplyr::filter(!is.na(Species) && Species %in% species_filter) %>%
    dplyr::mutate(Sex = dplyr::case_when(Sex %in% c("N", "O") ~ "F",
                                  Sex %in% c("K", "L") ~ "M"),
                  Mass = dplyr::na_if(Mass, 0),
                  WingLength = dplyr::na_if(WingLength, 0))

  #We have six scenarios we need to deal with in the capture data:
  #1.
  #Individual adult captures (Age is NA or != PP/PM). In this case, all the data
  #is in Capture_data

  #2.
  #Individual chick captures (Age == PP/PM and there is no
  #last ring number, Mass/WingLength are recorded ONLY in capture data). In this case, all the
  #data is in Capture_data

  #3.
  #Individual chick capture with separate record in nestling data (Age == PP/PM and there is no
  #last ring number. Mass/WingLength are recorded in both Capture_data and Nestling_data but the capture date is different).
  #Need to take a record from both Capture_data and Nestling_data

  #4.
  #Individual chick capture with conflicting record in nestling data (Age == PP/PM and there is no
  #last ring number. Mass/WingLength are recorded in both Capture_data and Nestling_data and the capture date is identical).
  #Records from Capture_data take precedence

  #5.
  #Multi-chick captures (Age == PP/PM where there is
  #a last ring number). In this case, part of the data is in Nestling_data.

  #6.
  #Records in nestling data that have no associated information in capture data.
  #This falls into two categories

  #6a. Nestling captured but not ringed. In this case, the ring number is listed as e.g. A
  #6b. Nestling captured and ringed, but no infomation in capture data.

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
    #AND where the CaptureDate in the Nestling data is the same as Capture_data
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
    dplyr::mutate(Mass = Mass/10,
                  CapturePopID = "HAR", CapturePlot = NA_character_,
                  ReleasePopID = "HAR", ReleasePlot = NA_character_,
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
    dplyr::summarise(Species = first(Species), PopID = "HAR",
              BroodIDLaid = first(BroodID),
              BroodIDFledged = BroodIDLaid,
              RingSeason = as.integer(first(lubridate::year(CaptureDate))),
              RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", "adult"),
              Sex = first(Sex)) %>%
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
  #We use zone 4 for now, because it seems to get the German point in the right place...but this really doesn't work!!

  # Location_data_sf_zone0 <- sf::st_as_sf(Location_wcoord,
  #              coords = c("Longitude", "Latitude"),
  #              crs = 3386) %>%
  #   sf::st_transform(crs = 4326)
  #
  # Location_data_sf_zone1 <- sf::st_as_sf(Location_wcoord,
  #                                        coords = c("Longitude", "Latitude"),
  #                                        crs = 2391) %>%
  #   sf::st_transform(crs = 4326)
  #
  # Location_data_sf_zone2 <- sf::st_as_sf(Location_wcoord,
  #                                        coords = c("Longitude", "Latitude"),
  #                                        crs = 2392) %>%
  #   sf::st_transform(crs = 4326)
  #   # sf::st_coordinates()
  #
  # Location_data_sf_nozone <- sf::st_as_sf(Location_wcoord,
  #                                        coords = c("Longitude", "Latitude"),
  #                                        crs = 2393) %>%
  #   sf::st_transform(crs = 4326)
  #
  Location_data_sf_zone4 <- sf::st_as_sf(Location_wcoord,
                                         coords = c("Longitude", "Latitude"),
                                         crs = 2394) %>%
    sf::st_transform(crs = 4326)
  #
  # Location_data_sf_zone5 <- sf::st_as_sf(Location_wcoord,
  #                                        coords = c("Longitude", "Latitude"),
  #                                        crs = 3387) %>%
  #   sf::st_transform(crs = 4326)
  #
  # Location_data_sf_other <- sf::st_as_sf(Location_wcoord,
  #                                        coords = c("Longitude", "Latitude"),
  #                                        crs = 4123) %>%
  #   sf::st_transform(crs = 4326)

  # ggplot() +
  #   geom_polygon(data = subset(map_data("world"), region %in% c("Morocco", "France", "Algeria", "Finland", "Sweden", "Denmark", "Germany", "Poland", "Lithuania") | (region == "Russia" & subregion == "32")), aes(group = group, x = long, y = lat), fill = "white", colour = "black") +
  #   geom_sf(data = filter(Location_data_sf_zone0, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko"))) +
  #   geom_sf(data = filter(Location_data_sf_zone1, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "red") +
  #   geom_sf(data = filter(Location_data_sf_zone2, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "blue") +
  #   geom_sf(data = filter(Location_data_sf_nozone,LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "orange") +
  #   geom_sf(data = filter(Location_data_sf_zone4, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "green") +
  #   geom_sf(data = filter(Location_data_sf_zone5, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "grey") +
  #   geom_sf(data = filter(Location_data_sf_other, LocationName %in% c("Tanska", "Ranska", "Germany", "Marokko")), colour = "yellow")

  Location_full <- dplyr::bind_rows(dplyr::bind_cols(dplyr::select(Location_wcoord, -Longitude, -Latitude),
                                    tibble(Longitude = sf::st_coordinates(Location_data_sf_zone4)[, 1]),
                                    tibble(Latitude = sf::st_coordinates(Location_data_sf_zone4)[, 2])),
                                    Location_nocoord)

  #For now, I give each LocationID_LocationName a unique record
  #These DO NOT link to Brood_data or Capture_data, need to ask Tapio how this should be done.
  #Where a location has multiple years of data, take these to be the Start/EndSeason
  Location_data <- Location_full %>%
    dplyr::mutate(LocationID = paste(LocationName, LocationID, sep = "_")) %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestboxID = unique(LocationID), PopID = "HAR", Latitude = as.numeric(first(Latitude)), Longitude = as.numeric(first(Longitude)),
                     LocationType = "NB", StartSeason = min(BreedingSeason), EndSeason = max(BreedingSeason), Habitat = "Evergreen") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
