#' Construct standard summary for data from Harjavalta, Finland.
#'
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

  ################
  # CAPTURE DATA #
  ################



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
