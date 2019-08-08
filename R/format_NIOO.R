#'Construct standard summary for NIOO data.
#'
#'A pipeline to produce a standard output for 8 hole-nesting bird study
#'populations at the Netherlands Institute of Ecology (NIOO-KNAW). Output
#'follows the HNB standard breeding data format.
#'
#'This section provides details on data management choices that are unique to
#'the NIOO database. For a general description of the standard format please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: By default the pipeline will include great tit \emph{Parus
#'major}; blue tit \emph{Cyanistes caeruleus}; pied flycatcher \emph{Ficedula
#'hypoleuca}; Eurasian nuthatch \emph{Sitta europaea}; coal tit \emph{Periparus
#'ater}; and tree sparrow \emph{Passer montanus}.
#'
#'\strong{Populations}: This pipeline extracts data for 8 populations managed by
#'NIOO-KNAW: Buunderkamp, Lichtenbeek, Westerheide, Hoge Veluwe, Warnsborn,
#'Vlieland, Oosterhout, and Liesbosch.
#'
#'\strong{Sex}: We condense sex information to only include groups M, F, and U
#'(unknown) following the EUring standard. Uncertainty in sex was ignored (e.g.
#''male?' or 'female?').
#'
#'\strong{Measurement error}: For BroodSize, NumberFledged XXXX FILL IN a best
#'estimate is provided. Best estimate is halfway between the minimum and maximum
#'possible value. \emph{N.B.:} This means that the best estimate will not
#'necessarily be an integer. Error is provided in BroodSizeError,
#'NumberFledgedError etc. this is the absolute error (+/-) around the best
#'estimate.
#'
#'\strong{CapturePlot, ReleasePlot, LocationID}: NIOO data gives CaptureLocation
#'and ReleaseLocation (e.g. which nest box was a check transfered to during crossfoster).
#'Currently, we use these as the Capture/ReleasePlot. In Capture_data
#'we specify the LocationID as the same as the CapturePlot. We need to fix
#'this to distinguish between Location (the smallest scale), Plot (middle scale),
#'and PopID (largest scale).
#'
#'\strong{Capture_data}Capture_data has information on the accuracy of capture dates.
#'For now, I am only including those individuals where capture date is known with
#'100% accuracy, otherwise this might affect the calculated age of individuals
#'during capture.
#'
#'\strong{AvgEggMass} Egg measurements are included in the NIOO database, but these are a bit more difficult to include
#'because they aren't associated with a given brood (they can be weighed before and after a cross fostering). For now,
#'we don't include this data, but we hope to in the future. Therefore, AvgEggMass is currently just NA.
#'
#'
#'@inheritParams pipeline_params
#'
#'@return Generates 4 .csv files with data in a standard format.
#'@export
#'@import dplyr
#'@import DBI
#'@import purrr

format_NIOO <- function(db = utils::choose.dir(),
                        species = NULL,
                        pop = NULL,
                        path = ".",
                        output_type = "csv"){

  #Force user to select directory
  force(db)

  db <- paste0(db, "\\NIOO_database.accdb")

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  message("Connecting to database...")

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
  Locations <- dplyr::tbl(connection, "dbo_tl_AreaGroup") %>%
    dplyr::collect() %>%
    dplyr::filter(grepl(pattern = paste(main_sites, collapse = "|"), Name)) %>%
    dplyr::select(AreaGroup = ID, Name) %>%
    #Create three letter PopID code for each AreaGroup (i.e. population).
    dplyr::mutate(PopID = purrr::map_chr(.x = Name,
                                  .f = function(.x){

                                    toupper(substr(.x, start = 1, stop = 3))

                                  })) %>%
    #Join in all the Areas within each AreaGroup (i.e. 'plots' within each population).
    dplyr::left_join(tbl(connection, "dbo_tx_Area_AreaGroup") %>%
                       dplyr::select(Area, AreaGroup) %>%
                       dplyr::collect(), by = "AreaGroup") %>%
    dplyr::rename(AreaID = Area) %>%
    #Join in all locations that are inside each Area within each AreaGroup (i.e. nest boxes/mist net locations in each plot within each population).
    dplyr::left_join(tbl(connection, "dbo_tbl_Location") %>%
                       dplyr::select(ID, UserPlaceName, AreaID, Latitude, Longitude) %>%
                       dplyr::collect(),
              by = "AreaID")

  ###################################
  # SPECIES AND POPUALATION FILTERS #
  ###################################

  #Create a subset of the chosen species
  #Where argument 'species' is unused, include all species in the table (listed in description)
  if(is.null(species)){

    species <- Species_codes$SpeciesID

  } else {

    species <- Species_codes[Species_codes$Code %in% species, ]$SpeciesID

  }

  if(is.null(pop)){

    pop <- unique(Locations$ID)

  }

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant).
  #This only includes data that DOES NOT CHANGE over the individual's lifetime.

  #Create table with description of sex codes
  Sex_data <- dplyr::tbl(connection, "dbo_tl_Sexe") %>%
    dplyr::select(Sexe = ID, Sex = Description)

  Individual_data   <- dplyr::tbl(connection, "dbo_tbl_Individual") %>%
    #Filter only required species
    dplyr::filter(SpeciesID %in% species) %>%
    #Select only the basic info that we want:
    # - Individual ID
    # - GeneticBroodID
    # - BroodID (for cross fostering experiments)
    # - Species
    # - Sex
    # - RingSeason (year of first ringing)
    # - RingAge (EUring age at first ringing)
    # - RingNumber
    dplyr::select(IndvID = ID, GeneticBroodID, BroodID, SpeciesID, Sexe, RingSeason = RingYear, RingAge, RingNumber) %>%
    #Add in sex description
    #N.B. Our approach, if we are joining in data
    #before we call 'collect' then we use left_join
    #otherwise, we use case_when
    dplyr::left_join(Sex_data, by = "Sexe") %>%
    #Remove old sex info
    dplyr::select(-Sexe) %>%
    dplyr::collect() %>%
    #Add sex from standard protocol
    #Add a ring age observed for determining Age_observed in Capture_data
    dplyr::mutate(Sex = dplyr::case_when(.$Sex %in% c(1, 3, 5) ~ "F",
                                         .$Sex %in% c(2, 4, 6) ~ "M"),
                  RingAgeObsv = RingAge) %>%
    #Add in the first capture location
    #This is needed to determine which population the bird belongs too.
    dplyr::left_join(tbl(connection, "dbo_tbl_Capture") %>%
                       dplyr::arrange(Individual, CaptureDate, CaptureTime) %>%
                       dplyr::select(IndvID = Individual, CaptureLocation) %>%
                       dplyr::group_by(IndvID) %>%
                       dplyr::collect() %>%
                       dplyr::slice(1), by = "IndvID") %>%
    #Relate the capturelocation to the three letter PopID
    dplyr::left_join(dplyr::select(Locations, PopID, CaptureLocation = ID), by = "CaptureLocation") %>%
    #Filter only chosen pop
    dplyr::filter(PopID %in% pop)

  Individual_data <- Individual_data %>%
    dplyr::mutate(Species = dplyr::case_when(.$SpeciesID == 14400 ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             .$SpeciesID == 14640 ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .$SpeciesID == 13490 ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                             .$SpeciesID == 14620 ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .$SpeciesID == 14790 ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .$SpeciesID == 15980 ~ Species_codes[Species_codes$SpeciesID == 15980, ]$Code,
                                             .$SpeciesID == 14610 ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code)) %>%
    #Sort out brood laid and brood fledged so that both columns are filled.
    mutate(BroodIDLaid = purrr::map2_chr(.x = BroodID, .y = GeneticBroodID,
                                         #If there is no genetic brood listed but there is a regular broodID, assume these are the same
                                         .f = ~ifelse(is.na(.y) & !is.na(.x), .x, .y)),

           BroodIDFledged = purrr::map2_chr(.x = BroodID, .y = GeneticBroodID,
                                           #If there is a genetic broodID listed by no regular brood ID assume these are the same.
                                           .f = ~ifelse(!is.na(.y) & is.na(.x), .y, .x))) %>%
    dplyr::select(IndvID, RingNumber, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, RingAgeObsv, Sex) %>%
    #Convert RingAge into either chick or adult
    dplyr::mutate(RingAge = dplyr::case_when(.$RingAge %in% c(1, 2, 3) ~ "chick",
                                             .$RingAge > 3 ~ "adult"))

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  #Capture data includes all times an individual was captured (with measurements like mass, tarsus etc.).
  #This will include first capture as nestling
  #This can include multiple records for a single individual.
  Capture_data <- dplyr::tbl(connection, "dbo_tbl_Capture") %>%
    dplyr::select(CaptureID = ID, AccuracyOfDate, CaptureDate, CaptureTime, IndvID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    dplyr::left_join(dplyr::tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
                       dplyr::select(CaptureID, SpeciesID, Observer, Weight, Tarsus, Wing_Length), by = "CaptureID") %>%
    #Filter target species
    dplyr::filter(SpeciesID %in% species & AccuracyOfDate == 1 & CaptureType %in% c(1, 2)) %>%
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
    dplyr::select(CaptureID, CaptureDate, CaptureTime, IndvID, SpeciesID, CaptureLocation,
                  ReleaseLocation, Observer, Weight, Tarsus, WingLength = Wing_Length) %>%
    dplyr::collect() %>%
    #Join in information on when the individual was first ringed (left join from the IndvData)
    #This is used to determine the age of each individual (EUring) at the time of capture
    dplyr::left_join(dplyr::select(Individual_data, IndvID, RingSeason, RingAgeObsv), by = "IndvID") %>%
    #Convert RingAgeObsv into Age_observed
    #These numbers should convert exactly, be we need to check them explicitly when I have
    #wifi again!
    #Add BreedingSeason
    dplyr::mutate(Age_observed = RingAgeObsv,
                  BreedingSeason = lubridate::year(CaptureDate)) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason, showpb = TRUE) %>%
    #Determine the time between first capture and current capture (in years)
    # mutate(MinAge = lubridate::year(lubridate::ymd(CaptureDate)) - RingSeason) %>%
    #Adjust this value to account for the age of the bird when first ringed
    #Using EUring codes, so we account for certainty in the age.
    # mutate(MinAge = toupper(as.hexmode(purrr::pmap_dbl(.l = list(.x = RingAge,
    #                                                              .y = MinAge),
    #                                                    .f = function(.x, .y){
    #
    #                                                      #If the age at ringing was unknown make no age estimate
    #                                                      if(is.na(.x) | .x == 0){
    #
    #                                                        return(NA)
    #
    #                                                      } else {
    #
    #                                                        #If the individual was in it's first year when ringed
    #                                                        if(.x < 4){
    #
    #                                                          #Use categories where age is certain (5, 7, etc.)
    #                                                          return(3 + 2*(.y))
    #
    #                                                        } else {
    #
    #                                                          #If it was not caught in first year, use categories where age is uncertain
    #                                                          #(6, 8)
    #                                                          return(.x + 2*(.y))
    #
    #                                                        }
    #
    #                                                      }
    #
    #                                                    })))) %T>%
    #Include species letter codes for all species
    dplyr::ungroup() %>%
    dplyr::mutate(Species = dplyr::case_when(.$SpeciesID == 14400 ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             .$SpeciesID == 14640 ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .$SpeciesID == 13490 ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                             .$SpeciesID == 14620 ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .$SpeciesID == 14790 ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .$SpeciesID == 15980 ~ Species_codes[Species_codes$SpeciesID == 15980, ]$Code,
                                             .$SpeciesID == 14610 ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                  #Add original tarsus method
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$Tarsus) ~ "Alternative"),
                  ChickAge = NA, ObserverID = as.character(Observer)) %>%
    #Arrange by species, indv and date/time
    dplyr::arrange(Species, IndvID, CaptureDate, CaptureTime) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    dplyr::left_join(dplyr::select(Locations, CaptureLocation = ID, CapturePopID = PopID), by = "CaptureLocation") %>%
    dplyr::left_join(dplyr::select(Locations, ReleaseLocation = ID, ReleasePopID = PopID), by = "ReleaseLocation") %>%
    dplyr::filter(CapturePopID %in% pop) %>%
    dplyr::mutate(LocationID = CaptureLocation) %>%
    #Arrange columns
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot = CaptureLocation,
                  ReleasePopID, ReleasePlot = ReleaseLocation,
                  Mass = Weight, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge)

  ##############
  # BROOD DATA #
  ##############

  #This data will include 1 row for every recorded brood.

  message("Compiling brood information...")

  Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species in main areas
    filter(BroodSpecies %in% species & BroodLocationID %in% !!Locations$ID) %>%
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
           LayingDate = lubridate::ymd(LayingDate) + LayingDateError,
           HatchDate = lubridate::ymd(HatchDate),
           FledgeDate = lubridate::ymd(FledgeDate)) %>%
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
    left_join(select(Individual_data, Female_ring = RingNumber, FemaleID = IndvID) %>%
                filter(Female_ring != ""), by = "Female_ring") %>%
    left_join(select(Individual_data, Male_ring = RingNumber, MaleID = IndvID) %>%
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

                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   return("first")

                                                 }

                                               }

                                               #If she is banded, then we need to apply all rules
                                               #If it's the first nest recorded for this female in this year...
                                               if(rows == 1){

                                                 #If it doesn't meet the 30 day rule, then name it as replacement
                                                 if(LD > cutoff_date){

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
    #Add extra columns where data was not provided
    #N.B. Need to go through and include experiment ID
    mutate(ClutchSizeError = NA, HatchDateError = NA, FledgeDateError = NA, ExperimentID = NA,
           BroodLocation = as.character(BroodLocation), BroodID = as.character(BroodID),
           FemaleID = as.character(FemaleID), MaleID = as.character(MaleID)) %>%
    select(SampleYear, Species, PopID, Plot, LocationID = BroodLocation, BroodID, FemaleID, MaleID, ClutchType_observed, ClutchType_calc, LayingDate, LayingDateError,
           ClutchSize, ClutchSizeError, HatchDate, HatchDateError, BroodSize, BroodSizeError, FledgeDate, FledgeDateError, NumberFledged, NumberFledgedError, ExperimentID)

  #Next, we calculate mean mass, tarsus for all chicks in the brood
  #AT 14-16 DAYS POST HATCHING!!!
  #IF THEY ARE NOT CAUGHT DURING THIS TIME, THEN LEAVE AS NA.
  avg_mass <- Brood_data %>%
    left_join(left_join(select(Capture_data, CaptureDate, IndvID, Mass, Tarsus),
                        dplyr::select(Individual_data, IndvID, BroodID = BroodIDFledged), by = "IndvID"), by = "BroodID") %>%
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

  message("Compiling nestbox information...")

  #Extract information on nestbox locations
  Location_data <- tbl(connection, "dbo_tbl_NestboxAppearance") %>%
    collect() %>%
    #Join together information on the nestbox locations (e.g. latitude, longitude, nestbox name) and information on each nestbox that was there (e.g. how long before it was replaced).
    #This is necessary because one nestbox location could have multiple nestboxes erected at it over the study period.
    left_join(select(Locations, Location = ID, Latitude, Longitude, PopID),
              by = "Location") %>%
    select(LocationID = Location, NestboxID = ID, NestBoxType, PopID, Latitude, Longitude, StartYear, EndYear) %>%
    dplyr::mutate(LocationID = as.character(LocationID))
  #REMOVE UNWANTED COLUMNS AND CHANGE FORMATS
  Individual_data <- Individual_data %>%
    dplyr::mutate(IndvID = as.character(IndvID)) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge)

  Capture_data <- Capture_data %>%
    dplyr::mutate(IndvID = as.character(IndvID),
                  LocationID = as.character(LocationID),
                  CaptureDate = lubridate::ymd(CaptureDate))

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Individual_data %>% select(-RingNumber), file = paste0(path, "\\Individual_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_NIOO.csv"), row.names = F)

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
