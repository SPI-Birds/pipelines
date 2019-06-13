#'Construct standard summary for data from Harjavalta, Finland.
#'
#'A pipeline to produce a standard output for the hole nesting bird population
#'in Harjavalta, Finland, administered by the University of Turku. Output
#'follows the HNB standard breeding data format.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see XXXXX
#'PLACE HOLDER!
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
#'dataset:
#'- All data from adults when first ringed and recaptured are stored in the
#' 'Ringing' table (Rengas.db). This is regular capture data that we can use
#'without adjustment.
#'- All data from chick captures are stored in the 'Nestling' table (Pullit.db).
#'HOWEVER, the 'Nestling' table does not include information on the ring number
#'nor the species of each individual, only the BroodID and the last 2 digits of
#'the ring number.
#'- When chicks were ringed for the first time, the ringing of ALL the chicks is
#'recorded as one entry in 'Ringing'. The first and last ring number of the
#'chicks ringed in the nest is given. Therefore, to determine the individual ID of
#'every chick capture from 'Nestling' we need to link it to the correct BroodID
#'and determine its ring number by taking the first x-2 characters of the
#'given ring numbers and adding the last 2 digits from the 'Nestling' table.
#'- Some chicks in 'Nestling' were captured and measured, but have no last 2
#'digits of ring number is provided, I assume these were not ringed (e.g. too
#'young). These data can be associated with a Brood and species, but not with an
#'individual.
#'- HOWEVER, the 'Nestling' and 'Ringing' tables don't always match. In some
#'cases (>5000), there is a record of chicks being ringed in the 'Ringing' table, but no record of
#'these chicks in the 'Nestling' table (e.g. BroodID: 2005_1908_1 or 2018_1216_1).
#'- In some cases (~50), there is information on chicks being captured and given
#'a ring number in the 'Nestling' table, but no record of this capture in the
#' 'Ringing' table (e.g. BroodID: 2008_0222_1 or 2018_1378_2).
#'- In some cases, there is a record of chicks being ringed in 'Ringing',
#'but many of these chicks are missing in the 'Nestling' table (e.g. BroodID: 2007_1714_1).
#'- In some cases, chicks were ringed with two different series and there are two different
#'records of chick ringing.
#'- In some cases, checks were recorded in 'Ringing' but have no matching
#'BroodID in Brood_data (e.g. 2004_1012_0).
#'- In some cases, one BroodID is used in the 'Ringing' table and another in the 'Nestling' table
#'even though the ring numbers are the same (e.g. 2018_0323_1/2)
#'- How do we deal with all these conflicts? For now, we assume that if a chick
#'was listed as 'ringed' but has no record in the 'Nestlings' table, we assume
#'it was ringed but not measurements were taken (e.g. tarsus) or the
#'measurements were not found because the BroodID was entered incorrectly. If a
#'chick was captured in 'Nestlings' but is not recorded in 'Ringing' for now
#'these will be excluded because they won't join to the data in 'Ringing'. These
#'cases are the most problematic because to give these chicks a true ring number
#'we need to know the rest of the ring number (not just last 2 digits). If they
#'have no record in ringing, it is impossible to include this information!
#'For records where no ring number is given (e.g. A), we include only those where
#'the BroodID is in either the 'Ringing' table or the 'Brood' table. This ensures
#'that the unringed chick is of the right species.
#'
#'\strong{Mass}: Mass of birds appears to be measured in mg. This is converted
#'to grams to match other populations.
#'
#'\strong{Tarsus}: Tarsus length is measured for both left and right leg. Only
#'left leg is reported.
#'@param db Location of database file.
#'@param Species A numeric vector. Which species should be included (EUring
#'  codes)? If blank will return all major species (see details below).
#'@param path Location where output csv files will be saved.
#'
#'@return Generates 5 .csv files with data in a standard format.
#'@export
#'@import reticulate

format_Harjavalta <- function(db = NULL,
                              Species = NULL,
                              path = "."){

  #Find database path
  if(is.null(db)){

    message("Please select the directory where database files are located...")

    db <- choose.dir()

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  ##############
  # BROOD DATA #
  ##############

  message("Extracting brood data from paradox database")

  #Extract table "Pesat.db" which contains brood data
  Brood_data <- extract_paradox_db(path = db, file_name = "Pesat.DB")

  message("Compiling brood data....")

  #Rename columns to English (based on description provided by Tapio Eeva)
  #Many of these are subsequently removed, but it makes it easier for non-Finnish speakers to
  #see what is being removed.
  colnames(Brood_data) <- c("SampleYear", "LocationID", "BroodID", "Species", "ClutchType_observed",
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

  Brood_data_output <- Brood_data %>%
    #Remove unwanted columns
    select(-ReasonFailed:-MalePresent, -ExpData1:-TempCode2) %>%
    #Create unique BroodID with year_locationID_BroodID
    mutate(BroodID = paste(SampleYear, LocationID, BroodID, sep = "_")) %>%
    #Convert species codes to EUring codes and then remove only the major species
    rowwise() %>%
    mutate(Species = ifelse(Species == "FICHYP", Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                            ifelse(Species == "PARCAE", Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                   ifelse(Species == "PARMAJ", Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                          ifelse(Species == "PARATE", Species_codes$Code[which(Species_codes$SpeciesID == 14610)], NA))))) %>%
    filter(!is.na(Species)) %>%
    #Add pop and plot id
    mutate(PopID = "HAR", Plot = NA) %>%
    #Adjust clutch type observed to meet our wording
    #N.B. There is one listed as '12' this is currently made into NA but need to check with Tapio
    mutate(ClutchType_observed = ifelse(ClutchType_observed == 1, "first",
                                         ifelse(ClutchType_observed %in% c(2, 3, 6), "replacement",
                                                ifelse(ClutchType_observed == 5, "second", NA)))) %>%
    #Create calendar date for laying date and hatch date
    mutate(LayingDate = as.Date(paste(LayingDate_day, LayingDate_month, SampleYear, sep = "/"), format = "%d/%m/%Y"),
           HatchDate  = as.Date(paste(HatchDate_day, HatchDate_month, SampleYear, sep = "/"), format = "%d/%m/%Y")) %>%
    ungroup() %>%
    #Go through and change all NA fledge numbers to 0s
    ### NEED TO CHECK THIS IS OK
    rowwise() %>%
    mutate(NumberFledged = ifelse(is.na(NumberFledged), 0, NumberFledged)) %>%
    ungroup() %>%
    group_by(SampleYear, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(SampleYear, Species, FemaleID) %>%
    group_by(SampleYear, Species, FemaleID) %>%
    mutate(total_fledge = cumsum(NumberFledged), row = 1:n()) %>%
    ungroup()

    clutchtype <- dplyr::progress_estimated(n = nrow(Brood_data_output))

    #Calculate clutch type with decision rules
    Brood_data_output <- Brood_data_output %>%
      mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       nr_fledge_now = .$NumberFledged,
                                                       LD = .$LayingDate),
                                             .f = function(rows, femID, cutoff_date, nr_fledge_before, nr_fledge_now, LD){

                                               clutchtype$tick()$print()

                                               #Firstly, check if the nest has a LD
                                               #If not, we cannot calculate BroodType

                                               if(is.na(LD)){

                                                 return(NA)

                                               }

                                               #Next, check if the female is banded
                                               #If a female is unbanded we assume the nest can NEVER be secondary
                                               #If she had had a successful clutch before she would have been caught and banded
                                               #Therefore it can only be first or replacement (based on 30d rule)
                                               if(is.na(femID)){

                                                 if(lubridate::ymd(LD) > lubridate::ymd(cutoff_date)){

                                                   return("replacement")

                                                 } else {

                                                   return("first")

                                                 }

                                               }

                                               #If she is banded, then we need to apply all rules
                                               #If it's the first nest recorded for this female in this year...
                                               if(rows == 1){

                                                 #If it doesn't meet the 30 day rule, then name it as replacement
                                                 if(lubridate::ymd(LD) > lubridate::ymd(cutoff_date)){

                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, we assume it was the first clutch
                                                   return("first")

                                                 }

                                                 #If it's NOT the first nest of the season for this female
                                               } else {

                                                 #If there have been no fledglings before this point..
                                                 if(nr_fledge_before - nr_fledge_now == 0){

                                                   #Then it is a replacement
                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, it is a secondary clutch
                                                   return("second")

                                                 }

                                               }

                                             })) %>%
      #Add Fledge date (it is NA, this isn't recorded)
      mutate(FledgeDate = NA, ClutchSizeError = NA, BroodSizeError = NA,
             FledgeDateError = NA) %>%
      #Arrange columns correctly
      select(SampleYear, Species, PopID, Plot, LocationID, BroodID, FemaleID, MaleID,
             ClutchType_observed, ClutchType_calc, LayingDate, LayingDateError,
             ClutchSize, ClutchSizeError, HatchDate, HatchDateError,
             BroodSize, BroodSizeError, FledgeDate, FledgeDateError,NumberFledged, ExperimentID)



    #################
    # NESTLING DATA #
    #################

    #Ringing data for nestlings and adults is stored separately.
    #First we will extract nestling info so we can determine average mass/tarsus

    message("Extracting nestling ringing data from paradox database")

    #Extract table "Pullit.db" which contains brood data
    Nestling_data <- extract_paradox_db(path = db, file_name = "Pullit.DB")

    #Rename into English to make data management more readable
    colnames(Nestling_data) <- c("SampleYear", "LocationID", "BroodID",
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
    Nestling_data_output <- Nestling_data %>%
      select(SampleYear:Mass, LeftTarsusLength,
             Sex) %>%
      #Create unique broodID (SampleYear_LocationID_BroodID)
      mutate(BroodID = paste(SampleYear, LocationID, BroodID, sep = "_")) %>%
      #Create a date object for time of measurement
      mutate(CatchDate = as.Date(paste(Day, Month, SampleYear, sep = "/"), format = "%d/%m/%Y")) %>%
      #Join hatch date data from brood data table above
      left_join(select(Brood_data_output, BroodID, HatchDate), by = "BroodID") %>%
      #Determine age at capture
      mutate(ChickAge = as.numeric(CatchDate - HatchDate))

    #Determine average mass and tarsus between 14 - 16 days old
    #I just use left tarsus for now, need to check with Marcel
    Chick_avg <- Nestling_data_output %>%
      filter(between(ChickAge, 14, 16)) %>%
      #Remove cases where tarsus or weight are 0 (make them NA)
      mutate(Mass = na_if(Mass, 0),
             LeftTarsusLength = na_if(LeftTarsusLength, 0)) %>%
      group_by(BroodID) %>%
      summarise(AvgMass = mean(Mass, na.rm = T)/10,
                AvgTarsus = mean(LeftTarsusLength, na.rm = T))

    #Join these into Brood_data
    #Only existing broods will be used.
    Brood_data_output <- left_join(Brood_data_output, Chick_avg, by = "BroodID")

    ################
    # CAPTURE DATA #
    ################

    message("Extracting capture data from paradox database")

    #Extract table "Pullit.db" which contains brood data
    Capture_data <- extract_paradox_db(path = db, file_name = "Rengas.DB")

    #Change colnames to English to make data management more understandable
    ##N.B. LastRingNumber_Brood = the end of the ringing series when ringing chicks
    #e.g. a record with RingNumber = 662470 and LastRingNumber_Brood = 662473 had three ringed chicks:
    # 662470, 662471, 662472, 662473
    # The number of nestlings ringed is stored in NrNestlings.
    colnames(Capture_data) <- c("RingSeries", "RingNumber",
                                "FirstRing", "SampleYear",
                                "Month", "Day", "Time",
                                "LocationID", "BroodID",
                                "Observer", "LastRingNumber_Brood",
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

    Capture_data_output <- Capture_data %>%
      #Create unique broodID
      mutate(BroodID = paste(SampleYear, LocationID, BroodID, sep = "_")) %>%
      #Create capture date
      mutate(CaptureDate = as.Date(paste(Day, Month, SampleYear, sep = "/"), format = "%d/%m/%Y")) %>%
      #Create capture time
      mutate(CaptureTime = lubridate::hm(na_if(paste(Time, "00", sep = ":"), "NA:00"), quiet = TRUE)) %>%
      #Remove cols that are not needed
      select(RingSeries:BroodID, LastRingNumber_Brood:Sex, Age, NrNestlings:Mass, Tarsus) %>%
      #Convert species codes to EUring codes and then remove only the major species
      rowwise() %>%
      mutate(Species = ifelse(Species == "FICHYP", Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                              ifelse(Species == "PARCAE", Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                     ifelse(Species == "PARMAJ", Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                            ifelse(Species == "PARATE", Species_codes$Code[which(Species_codes$SpeciesID == 14610)], NA))))) %>%
      filter(!is.na(Species))

    #There are 4 nests where one of either RingNumber or LastRingNumber_Brood is wrong
    #Ask Tapio about these, currently, we just correct RingNumber to make them work.
    Capture_data_output[which(Capture_data_output$BroodID %in% c("2011_1604_1", "2013_2134_1", "2013_1341_1", "2006_1630_1") & Capture_data_output$FirstRing == "5"), ]$RingNumber <- c(403681, 464023, 417760, 956723)

    #We are only interested in the adult ringing data from this database. The
    #chick data is all in nestlings. Chick ringing has age == "PP", all others are assumed to be adults (even Age = NA).
    Adult_capture    <- filter(Capture_data_output, Age != "PP" | is.na(Age)) %>%
      mutate(RingNumber = RingNumber, Capture_type = "Adult", Last2DigitsRingNr = NA) %>%
      select(RingSeries, RingNumber, SampleYear:Time, BroodID, Species:Age, WingLength:Tarsus, Capture_type, Last2DigitsRingNr)

    #Subset all info on chick captures
    #This is needed because it contains the full ring information
    Ringed_chick_capture <- purrr::pmap_df(.l = filter(Capture_data_output, Age == "PP"),
                                           .f = ~{

                                             #If there is no 'last ring number' (col 10)
                                             if(is.na(..10)){

                                               #Assume only one chick was ringed
                                               All_rings <- ..2

                                             } else {

                                               #Determine first part of Ringnumber
                                               ring_start <- substr(..2, start = 1, stop = nchar(..2) - 3)

                                               #We use the last 3 digits (rather than last 2) to deal with
                                               #cases where the ring series passes 100 (e.g. 99 - 00).
                                               ring_ends  <- substr(..2, start = nchar(..2) - 2, stop = nchar(..2)):substr(..10, start = nchar(..10) - 2, stop = nchar(..10))
                                               #Pad numbers with leading 0s to ensure they're all the right length
                                               ring_ends  <- stringr::str_pad(ring_ends, 3, pad = "0")

                                               #Otherwise, create a list of all chicks in the series
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
                                                                   Ring_Month = ..5, Ring_Day = ..6,
                                                                   BroodID = ..9, Species = ..11, Sex = ..12, Age = ..13))

                                           }) %>%
      #Now, for each recorded chick ring number determine the last 2 digits of the ring
      mutate(Last2DigitsRingNr = substr(RingNumber, start = nchar(RingNumber) - 1, stop = nchar(RingNumber)),
             Capture_type = "Ringed_chick") %>%
      #Join in all nestling data where the broodID and Last2Digits is the same
      #N.B. We do left_join with BroodID and Last2Digits, so we can get multiple records for each chick
      left_join(Nestling_data_output %>% select(SampleYear, BroodID, Month:Time, Last2DigitsRingNr, WingLength = Wing,
                                                Mass, Tarsus = LeftTarsusLength), by = c("BroodID", "Last2DigitsRingNr"))

    #Create a third data frame that is all the unringed records
    #We still want to associate them with a Brood (and species), but there is no ring number to use.
    #Link the BroodID from all unringed chicks with the Brood_data table so we can know the species.
    Unringed_chick_capture <- left_join(filter(Nestling_data_output, grepl(paste(c(LETTERS, "\\?"), collapse = "|"), Last2DigitsRingNr)),
                                   select(Brood_data_output, BroodID, Species), by = "BroodID") %>%
      mutate(RingSeries = NA, RingNumber = NA, Age = "PP", Capture_type = "Unringed_chick") %>%
      select(RingSeries, RingNumber, SampleYear, BroodID:Time,
             Species, Sex, Age, WingLength = Wing, Mass, Tarsus = LeftTarsusLength, Capture_type, Last2DigitsRingNr)

    #Join the ringed chick, unringed chick, and adult capture data together
    Capture_data_expand <- dplyr::bind_rows(Adult_capture, Ringed_chick_capture, Unringed_chick_capture)

    #We only want nestling data that is from broods in the capture data (because these are subset to just our species)
    # Nestling_capture <- filter(Nestling_data_output, BroodID %in% unique(Capture_data_output$BroodID))





    #Expand data so that ringed chicks each have their own row.
    Capture_data_expand <- purrr::pmap_df(.l = list(row_nr = 1:nrow(Capture_data_output)), function(row_nr, capture_data, nestling_data){

      print(row_nr)


      if(is.na(capture_data[row_nr, ]$NrNestlings) & is.na(capture_data[row_nr, ]$LastRingNumber_Brood)){

        #For now, we give a dummy chick age because it allows us to check how many adult/chick measures there are
        #We can't use NA because some chicks also have NA age.
        return(capture_data[row_nr, ] %>% mutate(ChickAge = 365))

      } else {

        #Otherwise, if chicks have been ringed (i.e. NrNestlings or LastRingNumber_Brood are filled)...
        #Add all data from the nestling table instead of the currently saved info

        ##N.B. In Nestling_data, there are cases where last ring number 'A/B'. I suspect these are cases where a chick is measured but not ringed.
        #For now, all individuals with last ring number that contains letters or ? is just given NA instead.
        chick_captures <- nestling_data[nestling_data$BroodID == capture_data[row_nr, ]$BroodID, ]

        output <- tibble(RingSeries = capture_data[row_nr, ]$RingSeries,
                         RingNumber = ifelse(grepl(paste(LETTERS, collapse = "|"), chick_captures$Last2DigitsRingNr), NA,
                                             paste0(substr(capture_data[row_nr, ]$RingNumber, 1, nchar(capture_data[row_nr, ]$RingNumber) - 2), chick_captures$Last2DigitsRingNr)),
                         FirstRing = NA, SampleYear = chick_captures$SampleYear, Month = chick_captures$Month, Day = chick_captures$Day,
                         Time = chick_captures$Time, LocationID = chick_captures$LocationID, BroodID = chick_captures$BroodID,
                         LastRingNumber_Brood = NA, Species = capture_data[row_nr, ]$Species,
                         Sex = chick_captures$Sex, Age = "PP", NrNestlings = NA, WingLength = chick_captures$Wing,
                         Mass = chick_captures$Mass, Tarsus = chick_captures$LeftTarsusLength, ChickAge = chick_captures$ChickAge)

        return(output)

      }
    }, capture_data = Capture_data_output, nestling_data = Nestling_data_output)

    Capture_data_expand <- Capture_data_expand %>%
      #Create IndvID as a combo of RingSeries and RingNumber
      mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
      #Add pop and plot id
      mutate(CapturePopID = "HAR", CapturePlot = NA,
             ReleasePopID = "HAR", ReleasePlot = NA,
             #Divide mass by 10 to get back to grams
             Mass = Mass/10)


    #STILL NEED TO GO THROUGH AND ADD MIN AGE AND INCLUDE CHICK INFO!

    ################
    # NESTBOX DATA #
    ################

    message("Extracting location data from paradox database")

    #Extract table "Pullit.db" which contains brood data
    Nestbox_data <- extract_paradox_db(path = db, file_name = "Paikat.DB")

    #Remove last 2 cols that have no info
    Nestbox_data <- Nestbox_data %>%
      select(-Aukko, -Malli)

    #Rename columns to make data management more understandable
    colnames(Nestbox_data) <- c("SampleYear", "LocationID",
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

    Nestbox_data_output <- Nestbox_data %>%
      mutate(NestboxID = LocationID, PopID = "HAR",
             NestBoxType = NA, StartYear = NA, EndYear = NA) %>%
      select(LocationID, NestboxID, NestBoxType, PopID, Latitude, Longitude, StartYear, EndYear)

    ###CURRENTLY ASSUMING THAT EACH LOCATION AND NEST BOX ARE IDENTICAL
    ###GO THROUGH AND CHECK MORE THOROUGHLY

    ###############
    # EXPORT DATA #
    ###############

    message("Saving .csv files...")

    write.csv(x = Brood_data_output, file = paste0(path, "\\Brood_data_HAR.csv"), row.names = F)

    # write.csv(x = Indv_data %>% select(-RingNumber), file = paste0(path, "\\Indv_data_HAR.csv"), row.names = F)
    #
    # write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_HAR.csv"), row.names = F)
    #
    # write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_HAR.csv"), row.names = F)

    time <- difftime(Sys.time(), start_time, units = "sec")

    message(paste0("All tables generated in ", round(time, 2), " seconds"))



}
