#'Construct standard summary for data from Harjavalta, Finland.
#'
#'A pipeline to produce a standard output for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku. Output
#'follows the HNB standard breeding data format.
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
#'\strong{LayingDateError & HatchDateError}: Accuracy of laying and hatch date
#'are given as categories: 0-1; 1-2; 2-3; >3 (N.B. Need to check this with
#'Tapio). More conservative error is provided (i.e. 0-1 is recorded as 1). For
#'now >3 is recorded as 4. N.B. Need to check this with Tapio!!
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
#'\item In some cases, chicks were ringed with two different series and there
#'are two different records of chick ringing.
#'
#'\item In some cases, chicks were recorded in 'Ringing' but have no matching
#'BroodID in Brood_data (e.g. 2004_1012_0).
#'
#'\item In some cases, one BroodID is used in the 'Ringing' table and another in
#'the 'Nestling' table even though the ring numbers are the same (e.g.
#'2018_0323_1/2).
#'
#'}
#'
#'How do we deal with all these conflicts? For now, we assume that if a chick
#'was listed as 'ringed' but has no record in the 'Nestlings' table, we assume
#'it was ringed but no measurements were taken (e.g. tarsus) or the measurements
#'were not found because the BroodID was entered incorrectly. If a chick was
#'captured in 'Nestlings' but is not recorded in 'Ringing' for now these will be
#'excluded because they won't join to the data in 'Ringing'. These cases are the
#'most problematic because to give these chicks a true ring number we need to
#'know the rest of the ring number (not just last 2 digits). If they have no
#'record in ringing, it is impossible to include this information! For records
#'where no ring number is given (e.g. A), we include only those where the
#'BroodID is in either the 'Ringing' table or the 'Brood' table. This ensures
#'that the unringed chick is of the right species.
#'
#'\strong{Mass}: Mass of birds appears to be measured in mg. This is converted
#'to grams to match other populations.
#'
#'\strong{Tarsus}: Tarsus length is measured for both left and right leg.
#'Generally, only left leg is reported and so is used here. Tarsus measurement
#'in adults can be either left or right leg.
#'
#'\strong{Sex}: Bird classified as 'likely male' or 'likely female' are simply
#'given code 'M' and 'F' respectively (i.e. this uncertainty is ignored).
#'
#'@inheritParams pipeline_params
#'
#'@return Generates 4 .csv files with data in a standard format.
#'@export
#'@import reticulate
#'@examples
#'\dontrun{
#'#Just get data about great tits
#'format_HAR(Species = "PARMAJ")
#'
#'#Get data on great and blue tits
#'format_HAR(Species = c("PARMAJ", "CYACAE"))
#'
#'}

format_HAR <- function(db = choose.dir(),
                       species = NULL,
                       path = ".",
                       debug = FALSE){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood data....")

  Brood_data <- create_brood_HAR(db = db, species_filter = species)

  ################
  # CAPTURE DATA #
  ################

  #Ringing data for nestlings and adults is stored separately.
  #First we will extract nestling info so we can determine average mass/tarsus

  message("Compiling capture data....")

  Capture_data <- create_capture_HAR(db = db, Brood_data = Brood_data, species_filter = species)

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual data...")

  Individual_data <- create_individual_HAR(Capture_data = Capture_data)

  #################
  # LOCATION DATA #
  #################

  message("Compiling location data...")

  Location_data <- create_location_HAR(db = db)

  ###CURRENTLY ASSUMING THAT EACH LOCATION AND NEST BOX ARE IDENTICAL
  ###GO THROUGH AND CHECK MORE THOROUGHLY

  ###########################
  # WRANGLE DATA FOR EXPORT #
  ###########################

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

  #########
  # DEBUG #
  #########

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "HAR", Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Indv_data = Individual_data)

  }

  ###############
  # EXPORT DATA #
  ###############

  message("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = F)

  write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HAR.csv"), row.names = F)

  write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_HAR.csv"), row.names = F)

  write.csv(x = Location_data, file = paste0(path, "\\Location_data_HAR.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

}

################################################################################################################

#' Create brood data table for Harjavalta.
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
#' @export

create_brood_HAR <- function(db, species_filter){

  message("Extracting brood data from paradox database")

  #Extract table "Pesat.db" which contains brood data
  Brood_data <- extract_paradox_db(path = db, file_name = "Pesat.DB")

  #Rename columns to English (based on description provided by Tapio Eeva)
  #Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  #see what is being removed.
  colnames(Brood_data) <- c("BreedingSeason", "LocationID", "BroodID", "Species", "ClutchType_observed",
                            "FemaleID", "MaleID", "LayingDate_day", "LayingDate_month",
                            "LayingDateError", "HatchDate_day",
                            "HatchDate_month",
                            "HatchDateError", "Incubation",
                            "ClutchSize", "BroodSize",
                            "NumberFledged", "ReasonFailed",
                            "NestlingInjuries", "MalePresent",
                            "ExperimentID", "ExpData1",
                            "ExpData2", "DeadParent",
                            "EggShells", "TempCode1",
                            "TempCode2")

  Brood_data <- Brood_data %>%
    #Remove unwanted columns
    select(-ReasonFailed:-MalePresent, -ExpData1:-TempCode2) %>%
    #Create unique BroodID with year_locationID_BroodID
    mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_")) %>%
    #Convert species codes to letter codes
    mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                      Species == "PARCAE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                      Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                      Species == "PARATE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)])) %>%
    filter(!is.na(Species) & Species %in% species_filter) %>%
    #Add pop and plot id
    mutate(PopID = "HAR", Plot = NA) %>%
    #Adjust clutch type observed to meet our wording
    mutate(ClutchType_observed = dplyr::case_when(ClutchType_observed == 1 ~ "first",
                                                  ClutchType_observed %in% c(2, 3, 6) ~ "replacement",
                                                  ClutchType_observed == 5 ~ "second")) %>%
    #Create calendar date for laying date and hatch date
    mutate(LayingDate = as.Date(paste(LayingDate_day, LayingDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y"),
           HatchDate  = as.Date(paste(HatchDate_day, HatchDate_month, BreedingSeason, sep = "/"), format = "%d/%m/%Y")) %>%
    #Treat all NAs as true unknowns (check with Tapio that these are NAs and not 0s)
    dplyr::arrange(BreedingSeason, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #Add Fledge date (it is NA, this isn't recorded)
    mutate(FledgeDate = NA, ClutchSizeError = NA, BroodSizeError = NA,
           FledgeDateError = NA, NumberFledgedError = NA) %>%
    #Arrange columns correctly
    select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID, FemaleID, MaleID,
           ClutchType_observed, ClutchType_calculated, LayingDate, LayingDateError,
           ClutchSize, ClutchSizeError, HatchDate, HatchDateError,
           BroodSize, BroodSizeError, FledgeDate, FledgeDateError, NumberFledged, NumberFledgedError,
           ExperimentID)

  return(Brood_data)

}

################################################################################################################

#' Create nestling data capture table for Harjavalta.
#'
#' Create nestling data capture table for data from Harjavalta, Finland. This is
#' used inside \code{\link{create_capture_HAR}}.
#'
#' @param Brood_data Output of \code{\link{create_brood_HAR}}.
#' @param db Location of primary data from Harjavalta.
#'
#' @return A data frame.
#' @export

create_nestling_HAR <- function(db, Brood_data){

  message("Extracting nestling ringing data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Nestling_data <- extract_paradox_db(path = db, file_name = "Pullit.DB")

  #Rename into English to make data management more readable
  colnames(Nestling_data) <- c("BreedingSeason", "LocationID", "BroodID",
                               "Month", "Day", "Time", "NrNestlings",
                               "Last2DigitsRingNr", "Dead",
                               "Wing", "Mass", "LeftLegAbnormal",
                               "RightLegAbnormal", "Left3Primary",
                               "Right3Primary", "LeftRectrix",
                               "RightRectrix", "LeftTarsusLength",
                               "RightTarsusLength", "LeftTarsusWidth",
                               "RightTarsusWidth", "GTBreastYellow",
                               "Lutein", "BloodSample",
                               "ColLengthBlood", "LengthBlood",
                               "BreastFeatherLutein",
                               "NailClipping", "Sex",
                               "HeadLength", "Feces1", "Feces2")

  #Remove unwanted columns
  Nestling_data <- Nestling_data %>%
    select(BreedingSeason:Mass, LeftTarsusLength,
           Sex) %>%
    #Create unique broodID (BreedingSeason_LocationID_BroodID)
    mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_")) %>%
    #Create a date object for time of measurement
    mutate(CatchDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y")) %>%
    #Join hatch date data from brood data table above
    left_join(select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    #Determine age at capture
    mutate(ChickAge = as.numeric(CatchDate - HatchDate))

  return(Nestling_data)

}

################################################################################################################

#' Create capture table for Harjavalta.
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
#' @export

create_capture_HAR    <- function(db, Brood_data, species_filter){

  Nestling_data <- create_nestling_HAR(db = db, Brood_data = Brood_data)

  message("Extracting capture data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Capture_data <- extract_paradox_db(path = db, file_name = "Rengas.DB")

  #Change colnames to English to make data management more understandable
  ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
  #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
  # 662470, 662471, 662472, 662473
  # The number of nestlings ringed is stored in NrNestlings.
  colnames(Capture_data) <- c("RingSeries", "RingNumber",
                              "FirstRing", "BreedingSeason",
                              "Month", "Day", "Time",
                              "LocationID", "BroodID",
                              "ObserverID", "LastRingNumber_Brood",
                              "Species", "Sex",
                              "Sex_method", "Age",
                              "Age_method", "RingType",
                              "Condition", "BirdStatus",
                              "CaptureMethod", "NrNestlings",
                              "WingLength", "Mass", "Moult",
                              "FatScore", "ExtraInfo",
                              "Plumage", "TailFeather",
                              "ColLengthBlood",
                              "LengthBlood", "Tarsus", "BreastMuscle",
                              "HeadLength", "Tick")

  Capture_data <- Capture_data %>%
    #Create unique broodID
    mutate(BroodID = paste(BreedingSeason, LocationID, BroodID, sep = "_")) %>%
    #Remove cols that are not needed
    select(RingSeries:BroodID, LastRingNumber_Brood:Sex, Age, NrNestlings:Mass, Tarsus, ObserverID) %>%
    #Convert species codes to EUring codes and then remove only the major species
    mutate(Species = dplyr::case_when(Species == "FICHYP" ~ Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                                      Species == "PARCAE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                      Species == "PARMAJ" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                      Species == "PARATE" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)])) %>%
    filter(!is.na(Species) & Species %in% species_filter) %>%
    mutate(Sex = dplyr::case_when(Sex %in% c("N", "O") ~ "F",
                                  Sex %in% c("K", "L") ~ "M"))

  #There are 4 nests where one of either RingNumber or LastRingNumber_Brood is wrong
  #Ask Tapio about these, currently we just remove them to make things work.
  Capture_data <- Capture_data %>%
    filter(!BroodID %in% c("2011_1604_1", "2013_2134_1", "2013_1341_1", "2006_1630_1"))
  # Capture_data[which(Capture_data$BroodID %in% c("2011_1604_1", "2013_2134_1", "2013_1341_1", "2006_1630_1") &
  #                      Capture_data$FirstRing == "5"), ]$RingNumber <- c(403681, 464023, 417760, 956723)

  #We are only interested in the adult ringing data from this database. The
  #chick data is all in nestlings. Chick ringing has age == "PP", all others are assumed to be adults (even Age = NA).
  Adult_capture    <- Capture_data %>%
    dplyr::filter(Age != "PP" | is.na(Age)) %>%
    dplyr::mutate(Capture_type = "Adult", Last2DigitsRingNr = NA, ChickAge = NA) %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(IndvID, BreedingSeason:Time, ObserverID, LocationID, BroodID, Species:Age, WingLength:Tarsus, Capture_type, Last2DigitsRingNr, ChickAge)

  message("Determining chick ring numbers...")

  chick_data <- filter(Capture_data, Age == "PP")

  ring_pb <- dplyr::progress_estimated(n = nrow(chick_data))

  #Subset all info on chick captures
  #This is needed because it contains the full ring information
  Ringed_chick_capture <- purrr::pmap_df(.l = chick_data,
                                         .f = ~{

                                           ring_pb$print()$tick()

                                           #If there is no 'last ring number' (col 10)
                                           if(is.na(..10)){

                                             #Assume only one chick was ringed
                                             All_rings <- ..2

                                           } else {

                                             #Determine first part of Ringnumber
                                             ring_start <- stringr::str_sub(..2, end = -6)

                                             #We use the last 3 digits (rather than last 2) to deal with
                                             #cases where the ring series passes 10000 (e.g. 9999 - 0000).
                                             ring_ends  <- stringr::str_sub(..2, start = -5):stringr::str_sub(..10, start = -5)
                                             #Pad numbers with leading 0s to ensure they're all the right length
                                             ring_ends  <- stringr::str_pad(ring_ends, 5, pad = "0")

                                             #Create a list of all chicks in the series
                                             All_rings <- paste0(ring_start, ring_ends)

                                           }

                                           #N.B. We include Year, Month, Day,
                                           #This data is needed if chicks
                                           #don't have info in nestling table
                                           #in these cases we still want to
                                           #know when the chick was supposedly
                                           #captured even though it doesn't
                                           #have any additional capture
                                           #information.

                                           #N.B. There are a few cases (<100)
                                           #where the capture data for chicks also has winglength/mass/tarsus
                                           #It seems these are cases where only one chick was captured (e.g.340826 in 1993_1304_1),
                                           #Or where multiple chicks were captured but only one measured (e.g. 428222 in 2011_0219_1).
                                           #We assume that all this info is ALSO stored in Nestling data, and so we don't include it
                                           return(tibble::tibble(RingSeries = ..1,
                                                                 RingNumber = All_rings, Ring_Year = ..4,
                                                                 Ring_Month = ..5, Ring_Day = ..6, Ring_Time = ..7,
                                                                 BroodID = ..9, Species = ..11, Sex = ..12, Age = ..13))

                                         }) %>%
    #Now, for each recorded chick ring number determine the last 2 digits of the ring
    dplyr::mutate(Last2DigitsRingNr = stringr::str_sub(RingNumber, start = -2),
                  Capture_type = "Ringed_chick") %>%
    #Join in all nestling data where the broodID and Last2Digits is the same
    #N.B. We do left_join with BroodID and Last2Digits, so we can get multiple records for each chick
    dplyr::left_join(Nestling_data %>% select(BreedingSeason, BroodID, Month:Time, Last2DigitsRingNr, WingLength = Wing,
                                              Mass, Tarsus = LeftTarsusLength, ChickAge), by = c("BroodID", "Last2DigitsRingNr"))

  pb <- dplyr::progress_estimated(n = nrow(Ringed_chick_capture))

  Ringed_chick_capture <- Ringed_chick_capture %>%
    dplyr::bind_cols(purrr::pmap_df(.l = list(.$BreedingSeason, .$Month, .$Day, .$Time, .$Ring_Year, .$Ring_Month, .$Ring_Day, .$Ring_Time, .$BroodID),
                                    .f = ~{

                                      pb$print()$tick()

                                      if(is.na(..1)){

                                        return(tibble::tibble(Final_BreedingSeason = ..5,
                                                              Final_Month = ..6,
                                                              Final_Day = ..7,
                                                              Final_Time = ..8))

                                      } else {

                                        return(tibble::tibble(Final_BreedingSeason = ..1,
                                                              Final_Month = ..2,
                                                              Final_Day = ..3,
                                                              Final_Time = ..4))

                                      }

                                    })) %>%


    #There are multiple cases where chicks were listed as being ringed in Ringing (Rengas.db)
    #But they are not recorded anywhere in nestlings (Pullit.db)
    #When this is the case, use the capture data from Rengas.db.
    # rowwise() %>%
    # mutate(BreedingSeason = ifelse(is.na(BreedingSeason), Ring_Year, BreedingSeason),
    #        Month = ifelse(is.na(Month), Ring_Month, Month),
    #        Day = ifelse(is.na(Day), Ring_Day, Day),
    #        #For time, only add it if BreedingSeason is missing
    #        #There are cases where there are nestling capture records but they don't include times
    #        Time = ifelse(is.na(BreedingSeason), Ring_Time, Time)) %>%
    # ungroup() %>%
    dplyr::mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
    dplyr::select(-RingSeries:-Ring_Time, -BreedingSeason:-Time) %>%
    dplyr::rename(BreedingSeason = Final_BreedingSeason,
                  Month = Final_Month,
                  Day = Final_Day,
                  Time = Final_Time)


  #Create a third data frame that is all the unringed records We still want to
  #associate them with a Brood (and species), but there is no ring number to
  #use. Link the BroodID from all unringed chicks with the Brood_data and
  #Capture_data table so we can know the species is correct.
  Unringed_chick_capture <- Nestling_data %>%
    dplyr::filter(grepl(paste(c(LETTERS, "\\?"), collapse = "|"), Last2DigitsRingNr)) %>%
    dplyr::left_join(select(Brood_data, BroodID, Species), by = "BroodID") %>%
    #Filter any cases where no species info was detected
    #i.e. the brood wasn't from one of the 4 species.
    dplyr::filter(!is.na(Species)) %>%
    dplyr::mutate(Age = "PP", Capture_type = "Unringed_chick", IndvID = NA) %>%
    dplyr::select(IndvID, BreedingSeason, BroodID:Time,
           Species, Sex, Age, WingLength = Wing, Mass, Tarsus = LeftTarsusLength, Capture_type, Last2DigitsRingNr, ChickAge)

  #Join the ringed chick, unringed chick, and adult capture data together
  Capture_data_expand <- dplyr::bind_rows(Adult_capture, Ringed_chick_capture, Unringed_chick_capture)

  #Check the expected number of adults is correct
  if(nrow(dplyr::filter(Capture_data, Age != "PP" | is.na(Age))) != nrow(dplyr::filter(Capture_data_expand, Capture_type == "Adult"))){

    warning("Number of adults in capture table is different to expected")

  }

  #Check number of unringed chicks is correct
  if(nrow(dplyr::filter(Nestling_data, grepl(paste(c(LETTERS, "\\?"), collapse = "|"), Last2DigitsRingNr) & BroodID %in% unique(Brood_data$BroodID))) !=
     nrow(dplyr::filter(Capture_data_expand, Capture_type == "Unringed_chick"))){

    warning("Number of unringed chicks in capture table is different to expected")

  }

  #Check number of unringed chicks is correct
  if(nrow(dplyr::filter(Nestling_data, !grepl(paste(c(LETTERS, "\\?"), collapse = "|"), Last2DigitsRingNr) & BroodID %in% unique(Capture_data$BroodID))) !=
     nrow(dplyr::filter(Capture_data_expand, Capture_type == "Ringed_chick"))){

    warning("Number of unringed chicks in capture table is different to expected")
    warning("The number of ring chicks expected and observed differs because there are cases where:
              a) chicks are recorded in Capture_data but not Nestling_data and
              b) chicks are recorded in Nestling_data but not Capture data. We can't really
              do anything about this programatically. These are probably typos that need to be fixed.")

  }

  message("Calculating age at each capture...")

  Capture_data_expand <- Capture_data_expand %>%
    ungroup() %>%
    dplyr::mutate(Mass = Mass/10, Tarsus = na_if(y = 0, x = Tarsus)) %>%
    #Create capture date
    dplyr::mutate(CaptureDate = as.Date(paste(Day, Month, BreedingSeason, sep = "/"), format = "%d/%m/%Y")) %>%
    #Create capture time
    dplyr::mutate(CaptureTime = na_if(paste(Time, "00", sep = ":"), "NA:00"),
           CapturePopID = "HAR", CapturePlot = NA,
           ReleasePopID = "HAR", ReleasePlot = NA,
           Age = dplyr::case_when(Age %in% c("PP", "PM", "FL") ~ 1)) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    dplyr::arrange(IndvID, CaptureDate) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = Age, Date = CaptureDate, Year = BreedingSeason) %>%
    #Make AgeObsv, that doesn't require any calculation, just uses observations at the time of capture
    #Assume that PP = 1, PM/FL = 3 (known to hatch this year), 1 = 5 (known to hatch last year),
    #1+ = 4 (hatched atleast 1 year ago), 2 = 7 (known to hatch 2 years ago),
    #2+ = 6 (hatched at least 2 years ago)
    dplyr::mutate(Age_observed = dplyr::case_when(Age %in% c("PM", "FL") ~ 3,
                                              Age == "PP" ~ 1,
                                              Age == "1" ~ 5,
                                              Age == "1+" ~ 4,
                                              Age == "2" ~ 7,
                                              Age == "2+" ~ 6),
                  OriginalTarsusMethod = "Alternative") %>%
    select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot,
           ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge, Sex, BroodID)

}

################################################################################################################

#' Create individual table for Harjavalta.
#'
#' Create full individual data table in standard format for data from Harjavalta, Finland.
#'
#' @param Capture_data Output of \code{\link{create_capture_HAR}}.
#'
#' @return A data frame.
#' @export

create_individual_HAR <- function(Capture_data){

  #Take capture data and determine general data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    group_by(IndvID) %>%
    summarise(Species = first(Species), PopID = "HAR",
              BroodIDLaid = first(BroodID),
              BroodIDFledged = BroodIDLaid,
              RingSeason = first(lubridate::year(CaptureDate)),
              RingAge = first(Age_calculated),
              Sex = first(Sex)) %>%
    rowwise() %>%
    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    mutate(BroodIDLaid = ifelse(RingAge %in% c(1, 3), BroodIDLaid, NA),
           BroodIDRinged = BroodIDLaid) %>%
    arrange(RingSeason, IndvID)

  return(Indv_data)

}

################################################################################################################

#' Create location table for Harjavalta.
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
  Location_data <- extract_paradox_db(path = db, file_name = "Paikat.DB")

  #Remove last 2 cols that have no info
  Location_data <- Location_data %>%
    dplyr::select(-Aukko, -Malli)

  #Rename columns to make data management more understandable
  colnames(Location_data) <- c("BreedingSeason", "LocationID",
                              "ForestType", "PinusSylvestris",
                              "PiceaAbies", "Betulasp",
                              "PopulusTremula",
                              "SorbusAcuparia",
                              "Salixsp", "JuniperusCommunis",
                              "Alnussp", "PrunusPadas",
                              "TreeHeight", "BasalArea",
                              "PineHeight", "SpruceHeight",
                              "BirchHeight", "PineBasalArea",
                              "SpruceBasalArea", "BirchBasalArea",
                              "Latitude", "Longitude", "Municipality",
                              "LocationName")

  Location_data <- Location_data %>%
    mutate(NestboxID = LocationID, PopID = "HAR",
           LocationType = NA, StartSeason = NA, EndSeason = NA, Habitat = "Evergreen") %>%
    select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
