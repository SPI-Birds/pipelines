#' Construct standard summary for NIOO data.
#'
#' A pipeline to produce a standard output for 8 hole-nesting bird study populations
#' at the Netherlands Institute of Ecology (NIOO-KNAW).
#' Output follows the HNB standard breeding data format.
#'
#' This section provides details on data management choices that are unique to the NIOO database.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' \strong{Species}: By default the pipeline will include great tit \emph{Parus major}; blue tit \emph{Cyanistes caeruleus};
#' pied flycatcher \emph{Ficedula hypoleuca}; Eurasian nuthatch \emph{Sitta europaea};
#' coal tit \emph{Periparus ater}; and tree sparrow \emph{Passer montanus}.
#'
#' \strong{Populations}: This pipeline extracts data for 8 populations managed by
#' NIOO-KNAW: Buunderkamp, Lichtenbeek, Westerheide, Hoge Veluwe, Warnsborn, Vlieland, Oosterhout, and Liesbosch.
#'
#' \strong{Sex}: We condense sex information to only include groups M, F, and U (unknown) following the EUring standard.
#' Uncertainty in sex was ignored (e.g. 'male?' or 'female?').
#'
#' \strong{Measurement error}: For BroodSize, NumberFledged XXXX FILL IN a best estimate is provided.
#' Best estimate is halfway between the minimum and maximum possible value. \emph{N.B.:} This means that the best estimate will not necessarily be an integer.
#' Error is provided in BroodSizeError, NumberFledgedError etc. this is the absolute error (+/-) around the best estimate.
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import dplyr
#' @import DBI
#' @import purrr

format_NIOO <- function(db = NULL,
                        Species = NULL,
                        path = "."){

  #Assign database location if none given.
  if(is.null(db)){

    print("Please choose a database file...")

    db <- file.choose()

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  print("Connecting to database...")

  ###N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the NIOO database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  #################
  # LOCATION DATA #
  #################

  #We first need to compile location information (and area names) as this will be included with all data tables.

  #List the main study sites.
  main_sites <- c("Buunderkamp", "Lichtenbeek", "Westerheide", "Hoge Veluwe", "Warnsborn", "Vlieland", "Oosterhout", "Liesbosch")

  #Extract the corresponding areas from the AreaGroup table
  Locations <- tbl(connection, "dbo_tl_AreaGroup") %>%
    collect() %>%
    filter(grepl(pattern = paste(main_sites, collapse = "|"), Name)) %>%
    select(AreaGroup = ID, Name) %>%
    #Create three letter PopID code for each AreaGroup (i.e. population).
    mutate(PopID = purrr::map_chr(.x = Name,
                                  .f = function(.x){

                                    toupper(substr(.x, start = 1, stop = 3))

                                  })) %>%
    #Join in all the Areas within each AreaGroup (i.e. 'plots' within each population).
    left_join(tbl(connection, "dbo_tx_Area_AreaGroup") %>% select(Area, AreaGroup) %>% collect(), by = "AreaGroup") %>%
    rename(AreaID = Area) %>%
    #Join in all locations that are inside each Area within each AreaGroup (i.e. nest boxes/mist net locations in each plot within each population).
    left_join(tbl(connection, "dbo_tbl_Location") %>% select(ID, UserPlaceName, AreaID, Latitude, Longitude) %>% collect(),
              by = "AreaID")

  ################
  # SPECIES DATA #
  ################

  #Create a subset of the chosen species
  #Where argument 'species' is unused, include all species in the table (listed in description)
  if(is.null(Species)){

    Species <- Species_codes$SpeciesID

  }

  Species_codes <- filter(Species_codes, SpeciesID %in% Species)

  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant).
  #This only includes data that DOES NOT CHANGE over the individual's lifetime.

  #Create table with description of sex codes
  Sex_data <- tbl(connection, "dbo_tl_Sexe") %>%
    select(Sexe = ID, Sex = Description)

  Indv_data   <- tbl(connection, "dbo_tbl_Individual") %>%
    #Subset only chosen species
    filter(SpeciesID %in% Species) %>%
    #Select only the basic info that we want:
    # - Individual ID
    # - GeneticBroodID
    # - BroodID (for cross fostering experiments)
    # - Species
    # - Sex
    # - RingYear (year of first ringing)
    # - RingAge (EUring age at first ringing)
    # - RingNumber
    select(IndvID = ID, GeneticBroodID, BroodID, SpeciesID, Sexe, RingYear, RingAge, RingNumber) %>%
    #Add in sex description
    left_join(Sex_data, by = "Sexe") %>%
    #Remove old sex info
    select(-Sexe) %>%
    collect() %>%
    #Add in the first capture location
    #This is needed to determine which population the bird belongs too.
    left_join(tbl(connection, "dbo_tbl_Capture") %>% arrange(Individual, CaptureDate, CaptureTime) %>% select(IndvID = Individual, CaptureLocation) %>% group_by(IndvID) %>%
              collect() %>% slice(1), by = "IndvID") %>%
    #Relate the capturelocation to the three letter PopID
    left_join(select(Locations, PopID, CaptureLocation = ID), by = "CaptureLocation") %T>%
    #Create a progress bar for converting speciesID to species letter codes.
    #Include species letter codes for all species
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    mutate(Species = purrr::map_chr(.x = .$SpeciesID,
                                    .f = function(.x){

                                      species_pb$tick()$print()

                                      return(as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"]))

                                    })) %>%
    #Sort out brood laid and brood fledged so that both columns are filled.
    mutate(BroodIDLaid = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                         #If there is no genetic brood listed but there is a regular broodID, assume these are the same
                                         .f = ~ifelse(is.na(.y) & !is.na(.x), .x, .y)),

           BroodIDRinged = purrr::map2_dbl(.x = BroodID, .y = GeneticBroodID,
                                           #If there is a genetic broodID listed by no regular brood ID assume these are the same.
                                           .f = ~ifelse(!is.na(.y) & is.na(.x), .y, .x)),
           #Transform sex information into EUring standard (M, F, U)
           Sex = purrr::map_chr(.x = .$Sex,
                                .f = function(.x){

                                  ifelse(grepl(pattern = "male", .x), "M",
                                         ifelse(grepl(pattern = "F", .x), "female", "U"))

                                })) %>%
    select(IndvID, RingNumber, Species, PopID, BroodIDLaid, BroodIDRinged, RingYear, RingAge, Sex)

  ################
  # CAPTURE DATA #
  ################

  print("Compiling capture information...")

  #Capture data includes all times an individual was captured (with measurements like mass, tarsus etc.).
  #This will include first capture as nestling
  #This can include multiple records for a single individual.
  Capture_data <- tbl(connection, "dbo_tbl_Capture") %>%
    select(CaptureID = ID, CaptureDate, CaptureTime, IndvID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    left_join(tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
                select(CaptureID, SpeciesID, Weight, Tarsus, Wing_Length), by = "CaptureID") %>%
    #Filter target species
    filter(SpeciesID %in% Species) %>%
    #Remove cases of egg measurement
    #We are only interested in captures of chicks and adults.
    filter(CaptureType %in% c(1, 2)) %>%
    #Select only the basic info we need
    # -CaptureID (unique ID of capture event)
    # -CaptureDate
    # -CaptureTime
    # -Individual ID
    # -Species
    # -Capture Location
    # -Release Location (for translocation)
    # -Weight
    # -Tarsus
    # -Wing_Length
    select(CaptureID, CaptureDate, CaptureTime, IndvID, SpeciesID, CaptureLocation,
           ReleaseLocation, Weight, Tarsus, WingLength = Wing_Length) %>%
    collect() %>%
    #Join in information on when the individual was first ringed (left join from the IndvData)
    #This is used to determine the age of each individual (EUring) at the time of capture
    left_join(select(Indv_data, IndvID, RingYear, RingAge), by = "IndvID") %>%
    #Determine the time between first capture and current capture (in years)
    mutate(MinAge = lubridate::year(lubridate::ymd(CaptureDate)) - RingYear) %>%
    #Adjust this value to account for the age of the bird when first ringed
    #Using EUring codes, so we account for certainty in the age.
    mutate(MinAge = toupper(as.hexmode(purrr::pmap_dbl(.l = list(.x = RingAge,
                                                   .y = MinAge),
                                         .f = function(.x, .y){

                                           #If the age at ringing was unknown make no age estimate
                                           if(is.na(.x) | .x == 0){

                                             return(NA)

                                           } else {

                                             #If the individual was in it's first year when ringed
                                             if(.x < 4){

                                               #Use categories where age is certain (5, 7, etc.)
                                               return(3 + 2*(.y))

                                             } else {

                                               #If it was not caught in first year, use categories where age is uncertain
                                               #(6, 8)
                                               return(.x + 2*(.y))

                                             }

                                           }

                                         })))) %T>%
    #Include species letter codes for all species
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    mutate(Species = purrr::map_chr(.x = .$SpeciesID,
                                    .f = function(.x){

                                      species_pb$tick()$print()

                                      as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"])

                                    }),
           CapturePlot = NA, ReleasePlot = NA) %>%
    #Arrange by species, indv and date/time
    arrange(Species, IndvID, CaptureDate, CaptureTime) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    left_join(dplyr::select(Locations, CaptureLocation = ID, CapturePopID = PopID), by = "CaptureLocation") %>%
    left_join(dplyr::select(Locations, ReleaseLocation = ID, ReleasePopID = PopID), by = "ReleaseLocation") %>%
    #Arrange columns
    select(CaptureID, CaptureDate, CaptureTime, IndvID, Species, CapturePopID, CaptureLocation, ReleasePopID, ReleaseLocation, Mass = Weight, Tarsus, WingLength, MinAge)

  ##############
  # BROOD DATA #
  ##############

  #This data will include 1 row for every recorded brood.

  print("Compiling brood information...")

  Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species in main areas
    filter(BroodSpecies %in% Species & BroodLocationID %in% Locations$ID) %>%
    #Link the ClutchType description (e.g. first, second, replacement)
    left_join(tbl(connection, "dbo_tl_BroodType") %>% select(BroodType = ID, Description), by = "BroodType") %>%
    #Extract basic info that we want:
    # - SampleYear
    # - BroodID
    # - BroodSpecies
    # - BroodLocation
    # - RingNumberFemale
    # - RingNumberMale
    # - Clutch Type (e.g. first, second, replacement)
    # - LayDate (calendar date)
    # - ClutchSize
    # - HatchDate
    # - BroodSize
    # - FledgeDate
    # - NumberFledged
    select(SampleYear = BroodYear, BroodID = ID, BroodSpecies, BroodLocation = BroodLocationID, Female_ring = RingNumberFemale, Male_ring = RingNumberMale,
           ClutchType_observed = Description, LayingDate = LayDate, LayingDateError = LayDateDeviation,
           ClutchSize, HatchDate, BroodSize = NumberHatched, BroodSizeError = NumberHatchedDeviation,
           FledgeDate, NumberFledged, NumberFledgedError = NumberFledgedDeviation) %>%
    collect() %>%
    #Account for error in brood size
    mutate(BroodSizeError = BroodSizeError/2, NumberFledgedError = NumberFledgedError/2, LayingDateError = LayingDateError/2,
           BroodSize = BroodSize + BroodSizeError,
           NumberFledged = NumberFledged + NumberFledgedError,
           Mar1 = lubridate::ymd(paste0(SampleYear, "-3-1")),
           LayingDate = as.numeric((lubridate::ymd(LayingDate) - Mar1) + LayingDateError)) %T>%
    #Turn species into letter codes
    {species_pb <<- dplyr::progress_estimated(n = nrow(.))} %>%
    #Include species letter codes for all species
    mutate(Species = purrr::map_chr(.x = .$BroodSpecies,
                                    .f = function(.x){

                                      species_pb$print()$tick()

                                      return(as.character(Species_codes[which(Species_codes$SpeciesID == .x), "Code"]))

                                    }),
           Plot = NA) %>%
    #Adjust ClutchType names to fit "first", "second", "replacement".
    #We ignore any uncertainty (e.g. "probably second" is just listed as "second")
    #ClutchTypes like 'different species inside one clutch' are listed as NA.
    mutate(ClutchType_observed = purrr::map_chr(.x = .$ClutchType_observed,
                                                .f = function(.x){

                                                  ifelse(grepl(pattern = "replacement", .x), "replacement",
                                                         ifelse(grepl(pattern = "second clutch after|probably second|third clutch", .x), "second",
                                                                ifelse(grepl(pattern = "first clutch", .x), "first", NA)))

                                                })) %>%
    #Make individuals with no ring number into NA
    mutate(Female_ring = purrr::map_chr(.x = .$Female_ring,
                                        .f = ~ifelse(.x == "0000000000"|.x == "",
                                                     NA, .x)),
           Male_ring = purrr::map_chr(.x = .$Male_ring,
                                      .f = ~ifelse(.x == "0000000000"|.x == "",
                                                   NA, .x))) %>%
    ########### N.B. CURRENTLY THERE ARE A FEW (~25) RING NUMBERS THAT ARE ASSIGNED TO 2 INDIVIDUALS
    ########### THIS MEANS THAT WE WILL GET A FEW DUPLICATE RECORDS WITH THIS APPROACH
    ########### THESE NEED TO BE ADDRESSED IN THE DATABASE BEFORE THEY CAN BE FIXED HERE
    #Join in ID numbers for the parents of the brood from the individual table above
    left_join(select(Indv_data, Female_ring = RingNumber, FemaleID = IndvID) %>%
                filter(Female_ring != ""), by = "Female_ring") %>%
    left_join(select(Indv_data, Male_ring = RingNumber, MaleID = IndvID) %>%
                filter(Male_ring != ""), by = "Male_ring") %>%
    #Join location info (including site ID and nestbox ID)
    left_join(dplyr::select(Locations, BroodLocation = ID, PopID), by = "BroodLocation") %>%
    arrange(PopID, SampleYear, Species, FemaleID)

  #Calcualte ClutchType manually using known laying date and fledgling information
  clutchtype <- dplyr::progress_estimated(n = nrow(Brood_data))

  Brood_data <- Brood_data %>%
    #Go through and change all NA fledge numbers to 0s
    ### NEED TO SEND LOUIS A MESSAGE ABOUT THESE.
    rowwise() %>%
    mutate(NumberFledged = ifelse(is.na(NumberFledged), 0, NumberFledged)) %>%
    ungroup() %>%
    #Determine the 30 day cut-off for all species
    ### NEED TO GO THROUGH AND DETERMINE WHETHER THE 30 DAY CUT OFF IS REASONABLE FOR ALL SPECIES
    ### LOOK AT THIS LATER
    group_by(PopID, SampleYear, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
           warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(SampleYear, Species, FemaleID) %>%
    group_by(SampleYear, Species, FemaleID) %>%
    mutate(total_fledge = cumsum(NumberFledged), row = 1:n()) %>%
    ungroup() %>%
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
    select(SampleYear, Species, PopID, Plot, LocationID = BroodLocation, BroodID, FemaleID, MaleID, ClutchType_observed, ClutchType_calc, LayingDate, LayingDateError,
           ClutchSize, HatchDate, BroodSize, BroodSizeError, FledgeDate, NumberFledged, NumberFledgedError)

  #Next, we calculate mean mass, tarsus for all chicks in the brood
  #AT 14-16 DAYS POST HATCHING!!!
  #IF THEY ARE NOT CAUGHT DURING THIS TIME, THEN LEAVE AS NA.
  avg_mass <- Brood_data %>%
    left_join(left_join(select(Capture_data, CaptureDate, IndvID, Mass, Tarsus), select(Indv_data, IndvID, BroodID = BroodIDRinged), by = "IndvID"), by = "BroodID") %>%
    #Filter those that were not caught at 14 - 16 days
    mutate(CaptureDate = lubridate::ymd(CaptureDate),
           HatchDate = lubridate::ymd(HatchDate)) %>%
    filter(CaptureDate > (HatchDate + 14) & CaptureDate < (HatchDate + 16)) %>%
    group_by(BroodID) %>%
    summarise(AvgMass = mean(Mass, na.rm = T),
              AvgTarsus = mean(Tarsus, na.rm = T))

  #Join this average mass/tarsus data back into the brood data table
  Brood_data <- Brood_data %>%
    left_join(avg_mass, by = "BroodID")

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

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_NIOO.csv"), row.names = F)

  write.csv(x = Indv_data %>% select(-RingNumber), file = paste0(path, "\\Indv_data_NIOO.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_NIOO.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_NIOO.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
