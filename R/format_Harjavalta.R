#' Construct standard summary for data from Harjavalta, Finland.
#'
#' A pipeline to produce a standard output for the hole nesting bird population
#' in Harjavalta, Finland, administered by the University of Turku.
#' Output follows the HNB standard breeding data format.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' \strong{Species}: Data from Harjavalta contains information on 23 different hole nesting species;
#' however, only 4 of these (great tit, blue tit, coal tit, pied flycatcher) have >100 nest records.
#' Only data from these 4 species is reported.
#'
#' \strong{Mass}: Mass of birds appears to be measured in mg. This is converted to grams to match other populations.
#'
#' \strong{Tarsus}: Tarsus length is measured for both left and right leg. Only left leg is reported.
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import reticulate

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
                            "LayingDate_accuracy", "HatchDate_day",
                            "HatchDate_month",
                            "HatchDate_accuracy", "Incubation",
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
    rowwise() %>%
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
      mutate(FledgeDate = NA) %>%
      #Arrange columns correctly
      select(SampleYear, Species, PopID, Plot, LocationID, BroodID, FemaleID, MaleID,
             ClutchType_observed, ClutchType_calc, LayingDate,
             ClutchSize, HatchDate, BroodSize, FledgeDate, NumberFledged)


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
      select(SampleYear:Mass, LeftTarsusLength:RightTarsusLength,
             Sex) %>%
      #Create unique broodID (SampleYear_LocationID_BroodID)
      mutate(BroodID = paste(SampleYear, LocationID, BroodID, sep = "_")) %>%
      #Create a date object for time of measurement
      mutate(CatchDate = as.Date(paste(Day, Month, SampleYear, sep = "/"), format = "%d/%m/%Y")) %>%
      #Filter only those nestlings from nests in brood data
      filter(BroodID %in% unique(Brood_data_output$BroodID)) %>%
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
    Brood_data_output <- left_join(Brood_data_output, Chick_avg, by = "BroodID")

    ################
    # CAPTURE DATA #
    ################

    message("Extracting capture data from paradox database")

    #Extract table "Pullit.db" which contains brood data
    Capture_data <- extract_paradox_db(path = db, file_name = "Rengas.DB")

    #Change colnames to English to make data management more understandable
    colnames(Capture_data) <- c("RingSeries", "RingNumber",
                                "FirstRing", "SampleYear",
                                "Month", "Day", "Time",
                                "LocationID", "BroodID",
                                "Observer", "RingNumber_Brood",
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
      #Create IndvID as a combo of RingSeries and RingNumber
      mutate(IndvID = paste(RingSeries, RingNumber, sep = "-")) %>%
      #Convert species codes to EUring codes and then remove only the major species
      rowwise() %>%
      mutate(Species = ifelse(Species == "FICHYP", Species_codes$Code[which(Species_codes$SpeciesID == 13490)],
                              ifelse(Species == "PARCAE", Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                     ifelse(Species == "PARMAJ", Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                            ifelse(Species == "PARATE", Species_codes$Code[which(Species_codes$SpeciesID == 14610)], NA))))) %>%
      filter(!is.na(Species)) %>%
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
