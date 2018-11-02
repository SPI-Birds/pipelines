#' Construct standard summary for main NIOO populations
#'
#' @param db Location of database
#' @param Species Which species should be included? (default to great and blue tit)
#' @param path Location where output files will be saved.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import dplyr
#' @import DBI
#' @import purrr

format_NIOO <- function(db = NULL, Species = c(14620, 14640), path = "."){

  if(R.version$arch == "x86_64"){

    stop("On Windows, this process will only work with a 32bit verison of R. \n
         Please go to Tools > Global Options to change the R version.")

  }

  if(is.null(db)){

    print("Please choose a database file...")

    db <- file.choose()

  }

  start_time <- Sys.time()

  print("Connecting to database...")

  ###N.B. THIS SEEMS TO REQUIRE R 32bit, it returns errors in 64bit
  #Connect to the NIOO database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  #################
  # LOCATION DATA #
  #################

  #We first need to compile location information (and area names) as this will be included with capture and brood data

  #For now, we are subsetting the data to just include the main study sites.
  main_sites <- c("Buunderkamp", "Lichtenbeek", "Westerheide", "Hoge Veluwe", "Warnsborn", "Vlieland", "Oosterhout", "Liesbosch")

  #Extract the area table
  #Areas are inside populations (e.g. Hoge Veluwe has multiple area codes)
  Area_data     <- tbl(connection, "dbo_tbl_Area") %>%
    collect() %>%
    #Only include areas that correspond to the main sites specified above
    filter(grepl(paste(main_sites, collapse = "|"), Name)) %>%
    #Change names to just use the three letter codes of the sites
    mutate(PopID = purrr::map_chr(.x = Name, .f = function(x, main_sites){

      site_name <- main_sites[unlist(lapply(main_sites, function(y){

        grepl(y, x)

      }))]

      return(toupper(substr(site_name, 1, 3)))

    }, main_sites))

  #Extract the nestbox locations.
  #Nestbox locations are WITHIN areas, which are within populations.
  Locations <- tbl(connection, "dbo_tbl_Location") %>%
    collect() %>%
    #For now, we use the full UserPlaceName e.g. HV.0506.0 instead of splitting it up.
    #This is probably easier because it will make sure that every nestbox ID is unique.
    #Join in the names from the area table
    left_join(dplyr::select(Area_data, AreaID = ID, PopID), by = "AreaID")

  ################
  # CAPTURE DATA #
  ################

  #This contains info on what the different age codes mean.
  #Age_data <- tbl(connection, "dbo_tl_Age") %>%
    #select(Age_join = ID, Age = Description)

  print("Compiling capture information...")

  #Capture data includes all times an individual was captured (with measurements like mass, tarsus etc.).
  #This will include first capture as nestling (for residents)
  #This means there will be multiple records for a single individual.
  Capture_data <- tbl(connection, "dbo_tbl_Capture") %>%
    select(CaptureID = ID, IndvID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    left_join(tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
                select(CaptureID, CaptureDate, SpeciesID, Weight, Tarsus, Wing_Length), by = "CaptureID") %>%
    #Filter just GT and BT
    filter(SpeciesID %in% Species) %>%
    #Create a new column that has the 2 letter ID for species
    #Include information about the tarsus length measurement method
    mutate(Species = ifelse(SpeciesID == 14620, "BT", ifelse(SpeciesID == 14640, "GT", NA)),
           TarsusMethod = NA) %>%
    #Join in info on the capture type. This includes:
    #-Measurements of eggs
    #-Measurements of nestlings/juveniles (i.e. with down)
    #-Measurements of any bird that is already ringed
    left_join(tbl(connection, "dbo_tl_CaptureType") %>%
                select(CaptureType = ID, Name), by = "CaptureType") %>%
    #Remove only the basic info we need
    # -CaptureDate
    # -CaptureLocation
    # -IndividualNumber
    # -SpeciesID
    # -Weight
    # -Tarsus
    # -Wing_Length
    # -Age
    select(CaptureDate, Species, Type = Name, IndvID, CaptureLocation,
           ReleaseLocation, Weight, Tarsus, TarsusMethod, WingLength = Wing_Length) %>%
    #Arrange by species, indv and date
    arrange(Species, IndvID, CaptureDate) %>%
    collect() %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    left_join(dplyr::select(Locations, CaptureLocation = ID, CapturePop = PopID), by = "CaptureLocation") %>%
    left_join(dplyr::select(Locations, ReleaseLocation = ID, ReleasePop = PopID), by = "ReleaseLocation") %>%
    #Arrange columns
    select(CaptureDate, Species, Type, IndvID, CapturePop, ReleasePop, Mass = Weight, Tarsus, TarsusMethod, WingLength)


  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  #Create table with description of sex codes
  Sex_data <- tbl(connection, "dbo_tl_Sexe") %>%
    #Keep just the Sex ID and its description
    select(Sexe = ID, Sex = Description)

  #Determine the first capture type of each individual (i.e. nestling/egg or breeding bird)
  First_capture <- Capture_data %>%
    group_by(IndvID) %>%
    summarise(Status = ifelse(any(Type != "Ringed"), "Resident", "Immigrant"))

  Indv_data   <- tbl(connection, "dbo_tbl_Individual") %>%
    #Subset only chosen species
    filter(SpeciesID %in% Species) %>%
    #Create a new column that has the 2 letter ID for species
    mutate(Species = ifelse(SpeciesID == 14620, "BT", "GT")) %>%
    #Join in first Type at first capture
    #Remove basic info that we want:
    # - BroodID
    # - GeneticBroodID (for cross fostering experiments)
    # - Species
    # - Sex
    # - RingYear
    # - RingAge
    # - RingNumber
    select(GeneticBroodID, BroodID, Species, IndvID = ID, RingNumber, RingYear, RingAge, Sexe) %>%
    #Add in sex description
    left_join(Sex_data, by = "Sexe") %>%
    #Remove old sex info
    select(-Sexe) %>%
    collect() %>%
    #Join in info on when the individual was first captured (i.e. do we know if it was born in the population or not?)
    left_join(First_capture, by = "IndvID") %>%
    #Use map to sort out brood laid and brood fledged
    mutate(BroodIDLaid = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                          #If there is no genetic brood listed but there is a regular broodID, assume these are the same
                                          .f = ~ifelse(is.na(.y) & !is.na(.x), .x, .y)),

           BroodIDFledged = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                          #If there is a genetic broodID listed by no regular brood ID assume these are the same.
                                          .f = ~ifelse(!is.na(.y) & is.na(.x), .y, .x))) %>%
    select(BroodIDLaid, BroodIDFledged, Species, IndvID, RingNumber, RingYear, RingAge, Status, Sex)

  ############################

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species
    filter(BroodSpecies %in% Species & BroodLocationID %in% Location_data$ID) %>%
    #Create a new column that has the 2 letter ID for species
    mutate(Species = ifelse(BroodSpecies == 14620, "BT", "GT")) %>%
    #Extract basic info that we want:
    # - BroodYear
    # - BroodSpecies
    # - BroodLocationID (will be translated to an area name)
    # - RingNumberFemale
    # - RingNumberMale
    # - LayDate
    # - ClutchSize
    # - HatchDate
    # - NumberHatched
    # - FledgeDate
    # - NumberFledged
    select(BroodLocation = BroodLocationID, BroodYear, Species, BroodID = ID, ClutchType = BroodType, Female_ring = RingNumberFemale, Male_ring = RingNumberMale,
           LayingDate = LayDate, ClutchSize, HatchDate, NumberHatched, FledgeDate, NumberFledged) %>%
    #Collect data so we can use map functions
    #This needs to be done because we are applying our function rowwise.
    collect() %>%
    #Join in ID numbers for the parents of the brood from the individual table above
    left_join(select(Indv_data, Female_ring = RingNumber, FemaleID = IndvID), by = "Female_ring") %>%
    left_join(select(Indv_data, Male_ring = RingNumber, MaleID = IndvID), by = "Male_ring") %>%
    #Join location info (including site ID and nestbox ID)
    left_join(dplyr::select(Locations, BroodLocation  = ID, NestboxName = UserPlaceName, PopID), by = "BroodLocation") %>%
    select(BroodYear, Species, PopID, BroodID, NestboxName, FemaleID, MaleID, ClutchType, LayingDate, ClutchSize, HatchDate, NumberHatched, FledgeDate, NumberFledged) %>%
    arrange(PopID, BroodYear, Species)

  ################
  # NESTBOX DATA #
  ################

  print("Compiling nestbox information...")

  #Extract information on nestbox locations
  Nestbox_data <- tbl(connection, "dbo_tbl_NestboxAppearance") %>%
    collect() %>%
    #Join together information on the nestbox locations (e.g. latitude, longitude, nestbox name) and information on each nestbox that was there (e.g. how long before it was replaced).
    #This is necessary because one nestbox location could have multiple nestboxes erected at it over the study period.
    left_join(select(Locations, Location = ID, NestboxName = UserPlaceName, Latitude, Longitude, PopID),
              by = "Location") %>%
    select(NestboxName, NestboxID = ID, PopID, Latitude, Longitude, StartYear, EndYear)

  ###################
  # POPULATION DATA #
  ###################

  #Create a final data frame that has summary information about each population.

  print("Compiling population summary information...")

  Pop_data <- Brood_data %>%
    #For each population, determine the first and last year that broods were recorded and the species observed
    group_by(PopID) %>%
    summarise(StartYear = min(BroodYear), EndYear = max(BroodYear), Species = paste(unique(Species)[order(unique(Species))], collapse = ",")) %>%
    #Determine the number of unique nestbox locations in each population
    left_join(Nestbox_data %>%
                group_by(PopID) %>%
                summarise(TotalNestbox = length(unique(NestboxName))), by = "PopID") %>%
    #arrange in alphabetical order
    arrange(PopID) %>%
    #Include real names (also in alphabetical order)
    mutate(PopName = main_sites[order(main_sites)]) %>%
    #Arrange columns into a nice order
    select(PopID, PopName, StartYear, EndYear, Species, TotalNestbox)

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_NIOO.csv"), row.names = F)

  write.csv(x = Indv_data %>% select(-RingNumber), file = paste0(path, "\\Indv_data_NIOO.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_NIOO.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_NIOO.csv"), row.names = F)

  write.csv(x = Pop_data, file = paste0(path, "\\Summary_data_NIOO.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
