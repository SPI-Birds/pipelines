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

format_NIOO <- function(db = NULL,
                        Species = NULL,
                        path = "."){

  #This is not needed. Just check that the Access driver and version of R are both 64 bit
  # if(R.version$arch == "x86_64"){
  #
  #   stop("On Windows, this process will onl y work with a 32bit verison of R. \n
  #        Please go to Tools > Global Options to change the R version.")
  #
  # }

  if(is.null(db)){

    print("Please choose a database file...")

    db <- file.choose()

  }

  start_time <- Sys.time()

  print("Connecting to database...")

  ###N.B. AS ABOVE. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the NIOO database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  #################
  # LOCATION DATA #
  #################

  #We first need to compile location information (and area names) as this will be included with capture and brood data

  #For now, we are subsetting the data to just include the main study sites.
  main_sites <- c("Buunderkamp", "Lichtenbeek", "Westerheide", "Hoge Veluwe", "Warnsborn", "Vlieland", "Oosterhout", "Liesbosch")

  #Extract the corresponding areas from the AreaGroup table
  Locations <- tbl(connection, "dbo_tl_AreaGroup") %>%
    collect() %>%
    filter(grepl(pattern = paste(main_sites, collapse = "|"), Name)) %>%
    select(AreaGroup = ID, Name) %>%
    #Create three letter PopID code
    mutate(PopID = purrr::map_chr(.x = Name,
                                  .f = function(.x){

                                    toupper(substr(.x, start = 1, stop = 3))

                                  })) %>%
    #Join in all the study areas (i.e. 'plots' within each population) that are included in these 8 target populations
    left_join(tbl(connection, "dbo_tx_Area_AreaGroup") %>% select(Area, AreaGroup) %>% collect(), by = "AreaGroup") %>%
    rename(AreaID = Area) %>%
    #Join in all locations (i.e. nest boxes) that were inside each plot (i.e. AreaID) within each population (i.e. AreaGroup)
    left_join(tbl(connection, "dbo_tbl_Location") %>% select(ID, UserPlaceName, AreaID, Latitude, Longitude) %>% collect(),
              by = "AreaID")

  ################
  # SPECIES DATA #
  ################

  #Create a subset of chosen species
  #Where argument 'species' is unused, include all species in the table
  if(is.null(Species)){

    Species <- Species_codes$SpeciesID

  }

  Species_codes <- filter(Species_codes, SpeciesID %in% Species)

  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  #Create table with description of sex codes
  Sex_data <- tbl(connection, "dbo_tl_Sexe") %>%
    #Keep just the Sex ID and its description
    select(Sexe = ID, Sex = Description)

  Indv_data   <- tbl(connection, "dbo_tbl_Individual") %>%
    #Subset only chosen species
    filter(SpeciesID %in% Species) %>%
    #Remove only basic info that we want:
    # - BroodID
    # - GeneticBroodID (for cross fostering experiments)
    # - Species
    # - Sex
    # - RingYear
    # - RingAge
    # - RingNumber
    select(GeneticBroodID, BroodID, SpeciesID, IndvID = ID, RingNumber, RingYear, RingAge, Sexe) %>%
    #Add in sex description
    left_join(Sex_data, by = "Sexe") %>%
    #Remove old sex info
    select(-Sexe) %>%
    collect() %T>%
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    #Include species letter codes for all species
    mutate(Species = purrr::map_chr(.x = .$SpeciesID,
                                    .f = function(.x){

                                      species_pb$tick()$print()

                                      return(as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"]))

                                    })) %>%
    #Use map to sort out brood laid and brood fledged
    mutate(BroodIDLaid = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                         #If there is no genetic brood listed but there is a regular broodID, assume these are the same
                                         .f = ~ifelse(is.na(.y) & !is.na(.x), .x, .y)),

           BroodIDRinged = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                           #If there is a genetic broodID listed by no regular brood ID assume these are the same.
                                           .f = ~ifelse(!is.na(.y) & is.na(.x), .y, .x)),
           Sex = purrr::map_chr(.x = .$Sex,
                                .f = function(.x){

                                  ifelse(grepl(pattern = "male", .x), "M",
                                         ifelse(grepl(pattern = "F", .x), "female", "U"))

                                })) %>%
    select(BroodIDLaid, BroodIDRinged, Species, IndvID, RingNumber, RingYear, RingAge, Sex)

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
    select(CaptureID = ID, CaptureDate, CaptureTime, IndvID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    left_join(tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
                select(CaptureID, SpeciesID, Weight, Tarsus, Wing_Length), by = "CaptureID") %>%
    #Filter just GT and BT
    filter(SpeciesID %in% Species) %>%
    #Remove cases where eggs were weighed
    filter(CaptureType %in% c(1, 2)) %>%
    #Remove only the basic info we need
    # -CaptureDate
    # -CaptureLocation
    # -IndividualNumber
    # -SpeciesID
    # -Weight
    # -Tarsus
    # -Wing_Length
    # -Age
    select(CaptureID, CaptureDate, CaptureTime, SpeciesID, IndvID, CaptureLocation,
           ReleaseLocation, Weight, Tarsus, WingLength = Wing_Length) %>%
    #Join in information on when the individual was first ringed (left join from the IndvData)
    #Need to collect to prevent warnings about different file sources
    collect() %>%
    left_join(select(Indv_data, IndvID, RingYear, RingAge), by = "IndvID") %>%
    #Determine different in age at each capture (in years)
    mutate(MinAge = lubridate::year(lubridate::ymd(CaptureDate)) - RingYear) %>%
    #Adjust this value to account for age at ringing
    mutate(MinAge = toupper(as.hexmode(purrr::pmap_dbl(.l = list(.x = RingAge,
                                                   .y = MinAge),
                                         .f = function(.x, .y){

                                           if(is.na(.x) | .x == 0){

                                             return(NA)

                                           } else {

                                             if(.x < 4){

                                               return(3 + 2*(.y))

                                             } else {

                                               return(.x + 2*(.y))

                                             }

                                           }

                                         })))) %T>%
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    #Adjust to include information on the age at first capture
    mutate(#Include species letter codes for all species
           Species = purrr::map_chr(.x = .$SpeciesID,
                                    .f = function(.x){

                                      species_pb$tick()$print()

                                      as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"])

                                    }),
           CapturePlot = NA, ReleasePlot = NA) %>%
    #Arrange by species, indv and date
    arrange(Species, IndvID, CaptureDate) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    left_join(dplyr::select(Locations, CaptureLocation = ID, CapturePopID = PopID), by = "CaptureLocation") %>%
    left_join(dplyr::select(Locations, ReleaseLocation = ID, ReleasePopID = PopID), by = "ReleaseLocation") %>%
    #Arrange columns
    select(CaptureID, CaptureDate, CaptureTime, Species, CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, IndvID, Mass = Weight, Tarsus, WingLength, MinAge)

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species
    filter(BroodSpecies %in% Species & BroodLocationID %in% Locations$ID) %>%
    #Link the ClutchType info to text output.
    left_join(tbl(connection, "dbo_tl_BroodType") %>% select(BroodType = ID, Description), by = "BroodType") %>%
    #Extract basic info that we want:
    # - SampleYear
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
    select(BroodLocation = BroodLocationID, SampleYear = BroodYear, BroodSpecies, BroodID = ID, ClutchType_observed = Description, Female_ring = RingNumberFemale, Male_ring = RingNumberMale,
           LayingDate = LayDate, ClutchSize, HatchDate, BroodSize = NumberHatched, FledgeDate, NumberFledged) %>%
    #Collect data so we can use map functions
    #This needs to be done because we are applying our function rowwise.
    collect() %T>%
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    #Include species letter codes for all species
    mutate(Species = purrr::map_chr(.x = .$BroodSpecies,
                                    .f = function(.x){

                                      species_pb$print()$tick()

                                      return(as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"]))

                                    }),
           Plot = NA,
           LayingDate = lubridate::ymd(LayingDate)) %>%
    #Remove SpeciesID
    select(-BroodSpecies) %>%
    #Adjust ClutchType names to fit "first", "second", "replacement".
    #This means that we make things like 'different species inside one clutch' into NA/unknown.
    mutate(ClutchType_observed = purrr::map_chr(.x = .$ClutchType_observed,
                                                .f = function(.x){

                                                  ifelse(grepl(pattern = "replacement", .x), "replacement",
                                                         ifelse(grepl(pattern = "second clutch after|probably second|third clutch", .x), "second",
                                                                ifelse(grepl(pattern = "first clutch", .x), "first", NA)))

                                                })) %>%
    #Join in ID numbers for the parents of the brood from the individual table above
    left_join(select(Indv_data, Female_ring = RingNumber, FemaleID = IndvID) %>%
                filter(Female_ring != ""), by = "Female_ring") %>%
    left_join(select(Indv_data, Male_ring = RingNumber, MaleID = IndvID) %>%
                filter(Male_ring != ""), by = "Male_ring") %>%
    #Join location info (including site ID and nestbox ID)
    left_join(dplyr::select(Locations, BroodLocation  = ID, NestboxID = UserPlaceName, PopID), by = "BroodLocation") %>%
    arrange(PopID, SampleYear, Species)

  clutchtype <- dplyr::progress_estimated(n = nrow(Brood_data))

  #Determine laydate cut off for each species and the cumulative sum
  Brood_data <- Brood_data %>%
    #Go through and change all NA fledge numbers to 0s
    rowwise() %>%
    mutate(NumberFledged = ifelse(is.na(NumberFledged), 0, NumberFledged)) %>%
    ungroup() %>%
    group_by(SampleYear, Species) %>%
    mutate(cutoff = min(LayingDate, na.rm = T) + 30) %>%
    arrange(SampleYear, Species, NestboxID) %>%
    group_by(SampleYear, Species, NestboxID) %>%
    mutate(total_fledge = cumsum(NumberFledged), row = 1:n()) %>%
    ungroup() %>%
    mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       nr_fledge_now = .$NumberFledged,
                                                       LD = .$LayingDate),
                                             .f = function(rows, cutoff_date, nr_fledge_before, nr_fledge_now, LD){

                                               clutchtype$tick()$print()

                                               if(rows == 1){

                                                 if(is.na(LD)){

                                                   return(NA)

                                                 } else {

                                                   if(lubridate::ymd(LD) > lubridate::ymd(cutoff_date)){

                                                     return("replacement")

                                                   } else {

                                                     return("first")

                                                   }

                                                 }

                                               } else {

                                                 if(nr_fledge_before - nr_fledge_now == 0){

                                                   return("replacement")

                                                 } else {

                                                   return("second")

                                                 }

                                               }

                                             })) %>%
    select(SampleYear, Species, PopID, Plot, NestboxID, BroodID, FemaleID, MaleID, ClutchType_observed, ClutchType_calc, LayingDate, ClutchSize, HatchDate, BroodSize, FledgeDate, NumberFledged)


    #This takes hours and still needs to be checked. Run this at a later point.
    # #Determine average mass and tarsus of each brood
    # #First we need to detemine the ID of all individuals that fledged in each brood.
    # avg_mass_qry <- Indv_data %>%
    #   filter(!is.na(BroodIDFledged)) %>%
    #   group_by(BroodIDFledged) %>%
    #   summarise(all_chicks = list(unique(IndvID))) %T>%
    #   {avg_mass_pb <<- dplyr::progress_estimated(n = nrow(.)*2)} %>%
    #   #Then we need to go through each row and determine mass and tarsus of these individuals as chicks in capture data
    #   mutate(avg_mass = purrr::map_dbl(.x = .$all_chicks,
    #                                    .f = function(.x, Capture_data){
    #
    #                                      avg_mass_pb$tick()$print()
    #
    #                                      Capture_data %>%
    #                                        filter(IndvID %in% .x & !is.na(Mass) & Type == "Unringed") %>%
    #                                        summarise(avg_mass = mean(Mass, na.rm = T)) %>%
    #                                        pull(avg_mass)
    #
    #                                    }, Capture_data),
    #          avg_tarsus = purrr::map_dbl(.x = .$all_chicks,
    #                                      .f = function(.x, Capture_data){
    #
    #                                        avg_mass_pb$tick()$print()
    #
    #                                        Capture_data %>%
    #                                          filter(IndvID %in% .x & !is.na(Tarsus) & Type == "Unringed") %>%
    #                                          summarise(avg_tarsus = mean(Tarsus, na.rm = T)) %>%
    #                                          pull(avg_tarsus)
    #
    #                                      }, Capture_data))
    #
    # #Join this data into the brood data
    # Brood_data <- Brood_data %>%
    #   left_join(avg_mass_qry %>% select(-all_chicks), by = "BroodIDFledged")


  ################
  # NESTBOX DATA #
  ################

  print("Compiling nestbox information...")

  #Extract information on nestbox locations
  Nestbox_data <- tbl(connection, "dbo_tbl_NestboxAppearance") %>%
    collect() %>%
    #Join together information on the nestbox locations (e.g. latitude, longitude, nestbox name) and information on each nestbox that was there (e.g. how long before it was replaced).
    #This is necessary because one nestbox location could have multiple nestboxes erected at it over the study period.
    left_join(select(Locations, Location = ID, Latitude, Longitude, PopID),
              by = "Location") %>%
    select(LocationID = Location, NestboxID = ID, NestBoxType, PopID, Latitude, Longitude, StartYear, EndYear)

  ###################
  # POPULATION DATA #
  ###################

  #Create a final data frame that has summary information about each population.

  print("Compiling population summary information...")

  Pop_data <- Brood_data %>%
    #For each population, determine the first and last year that broods were recorded and the species observed
    group_by(PopID) %>%
    summarise(StartYear = min(SampleYear), EndYear = max(SampleYear), Species = paste(unique(Species)[order(unique(Species))], collapse = ",")) %>%
    #Determine the number of unique nestbox locations in each population
    left_join(Nestbox_data %>%
                group_by(PopID) %>%
                summarise(TotalNestbox = length(unique(BoxNumber))), by = "PopID") %>%
    #arrange in alphabetical order
    arrange(PopID) %>%
    #Include real names (also in alphabetical order)
    mutate(PopName = as.character(main_sites[order(main_sites)])) %>%
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
