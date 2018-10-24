#' Construct standard summary for Hoge Veluwe population
#'
#' @param db Location of database
#'
#' @return
#' @export
#' @import dbplyr
#' @import DBI
#' @import magrittr
#'
#' @examples

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
  Area_data     <- tbl(connection, "dbo_tbl_Area") %>%
    collect() %>%
    #Only include areas that correspond to the main sites specified above
    filter(grepl(paste(main_sites, collapse = "|"), Name))

  #Extract the location table
  Location_data <- tbl(connection, "dbo_tbl_Location") %>%
    #Filter out only those locations that are within the main areas
    filter(AreaID %in% Area_data$ID) %>%
    collect() %>%
    #Join in the names from the area table
    left_join(dplyr::select(Area_data, AreaID = ID, Name), by = "AreaID") %>%
    #Change names to just use the three letter codes of the sites
    mutate(Name = purrr::map_chr(.x = Name, .f = function(x, main_sites){

      site_name <- main_sites[unlist(lapply(main_sites, function(y){

        grepl(y, x)

      }))]

      return(toupper(substr(site_name, 1, 3)))

    }, main_sites))

  ################
  # CAPTURE DATA #
  ################

  #This contains info on what the different age codes mean.
  #Age_data <- tbl(connection, "dbo_tl_Age") %>%
    #select(Age_join = ID, Age = Description)

  print("Compiling capture information...")

  Capture_data <- tbl(connection, "dbo_tbl_Capture") %>%
    #Join in CaptureData table with weight, tarsus and wing_length
    select(CaptureID = ID, Indv_ID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    left_join(tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
                select(CaptureID, CaptureDate, SpeciesID, Weight, Tarsus, Wing_Length), by = "CaptureID") %>%
    #Filter just GT and BT
    filter(SpeciesID %in% Species) %>%
    #Create a new column that has the 2 letter ID for species
    mutate(Species = ifelse(SpeciesID == 14620, "BT", ifelse(SpeciesID == 14640, "GT", NA))) %>%
    #Remove only the basic info we need
    # -CaptureDate
    # -CaptureLocation
    # -IndividualNumber
    # -SpeciesID
    # -Weight
    # -Tarsus
    # -Wing_Length
    # -Age
    select(CaptureDate, Species, Type = CaptureType, Indv_ID, CaptureLocation,
           ReleaseLocation, Weight, Tarsus, Wing_Length) %>%
    #Arrange by species, indv and date
    arrange(Species, Indv_ID, CaptureDate) %>%
    collect() %>%
    #Include three letter population codes for capture and release location
    left_join(dplyr::select(Location_data, CaptureLocation = ID, CaptureName = Name), by = "CaptureLocation") %>%
    left_join(dplyr::select(Location_data, ReleaseLocation = ID, ReleaseName = Name), by = "ReleaseLocation") %>%
    #Arrange columns
    select(CaptureDate, Species, Type, Indv_ID, CaptureLocation = CaptureName, ReleaseLocation = ReleaseName, Weight, Tarsus, Wing_Length)


  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  Sex_data <- tbl(connection, "dbo_tl_Sexe") %>%
    #Just select the Sex ID and its description
    select(Sexe = ID, Sex = Description)

  #Determine the first capture type of each individual (i.e. nestling or breeding bird)
  First_capture <- Capture_data %>%
    group_by(Indv_ID) %>%
    summarise(Status = Type[1])

  #Subset only broods of designated species
  Indv_data   <- tbl(connection, "dbo_tbl_Individual") %>%
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
    select(GeneticBroodID, BroodID, Species, Indv_ID = ID, RingNumber, RingYear, RingAge, Sexe) %>%
    #Add in sex description
    left_join(Sex_data, by = "Sexe") %>%
    #Remove old sex info
    select(-Sexe) %>%
    collect() %>%
    left_join(First_capture, by = "Indv_ID") %>%
    #Use map to sort our brood laid and brood fledged
    mutate(BroodID_laid = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                          #If there is no genetic brood listed but there is a regular broodID, assume these are the same
                                          .f = ~ifelse(is.na(.y) & !is.na(.x), .x, .y)),

           BroodID_fledged = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                             #If there is a genetic broodID listed by no regular brood ID make them the same.
                                          .f = ~ifelse(!is.na(.y) & is.na(.x), .y, .x))) %>%
    select(BroodID_laid, BroodID_fledged, Species, Indv_ID, RingNumber, RingYear, RingAge, Status, Sex)

  ############################

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  #Subset only broods of designated species
  Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
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
    select(BroodLocation = BroodLocationID, Brood_year = BroodYear, Species, BroodID = ID, ClutchType = BroodType, Female_ring = RingNumberFemale, Male_ring = RingNumberMale,
           LayingDate = LayDate, ClutchSize, HatchDate, NumberHatched, FledgeDate, NumberFledged) %>%
    #Collect data so we can use map functions
    #This needs to be done because we are applying our function rowwise.
    collect() %>%
    #Join in ID numbers from the individual table above
    left_join(select(Indv_data, Female_ring = RingNumber, FemaleID = Indv_ID), by = "Female_ring") %>%
    left_join(select(Indv_data, Male_ring = RingNumber, MaleID = Indv_ID), by = "Male_ring") %>%
    #Join location info
    left_join(dplyr::select(Location_data, BroodLocation  = ID, Name), by = "BroodLocation") %>%
    select(Brood_year, Species, BroodLocation = Name, BroodID, FemaleID, MaleID, ClutchType, LayingDate, ClutchSize, HatchDate, NumberHatched, FledgeDate, NumberFledged)

  ################
  # NESTBOX DATA #
  ################

  Nestbox_data <- tbl(connection, "dbo_tbl_NestboxAppearance") %>%
    select(NestboxID = ID, Location, StartYear, EndYear) %>%
    collect() %>%
    #Extract latitude and longitude from location data
    left_join(select(Location_data, Location = ID, Latitude, Longitude), by = "Location") %>%
    select(-Location)

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data.csv"), row.names = F)

  write.csv(x = Indv_data %>% select(-RingNumber), file = paste0(path, "\\Indv_data.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
