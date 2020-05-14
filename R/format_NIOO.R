#'Construct standard format for NIOO data.
#'
#'A pipeline to produce the standard format for 8 hole-nesting bird study
#'populations at the Netherlands Institute of Ecology (NIOO-KNAW).
#'
#'This section provides details on data management choices that are unique to
#'the NIOO database. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: By default the pipeline will include great tit \emph{Parus
#'major}; blue tit \emph{Cyanistes caeruleus}; pied flycatcher \emph{Ficedula
#'hypoleuca}; Eurasian nuthatch \emph{Sitta europaea}; coal tit \emph{Periparus
#'ater}; and tree sparrow \emph{Passer montanus}. Other minority species are
#'excluded.
#'
#'\strong{Populations}: This pipeline extracts data for 8 populations managed by
#'NIOO-KNAW: Buunderkamp, Lichtenbeek, Westerheide, Hoge Veluwe, Warnsborn,
#'Vlieland, Oosterhout, and Liesbosch.
#'
#'\strong{Sex}: We condense sex information to only include groups M, F, and C
#'(conflicting). Uncertainty in sex was ignored (e.g.
#''male?' or 'female?').
#'
#'\strong{Measurement error}: For BroodSize, NumberFledged, and LayDate a best
#'estimate is provided. Best estimate is halfway between the minimum and maximum
#'possible value. \emph{N.B.:} In cases where the best estimate is not an integer,
#'we provide the lower value (e.g. 2.5 is recorded as 2).
#'Error is provided in BroodSizeError,
#'NumberFledgedError, and LayDateError this is the absolute error (+/-) around the best
#'estimate.
#'
#'\strong{CapturePlot, ReleasePlot, LocationID}: NIOO data gives CaptureLocation
#'and ReleaseLocation of each capture.
#'Each location is within a plot, specified by AreaID. We use this AreaID
#'information as the Capture/ReleasePlot
#'information.
#'
#'\strong{Capture_data}: Capture_data has information on the accuracy of capture dates.
#'Values >1 are inaccurate and are ignored. A value of 0 is unknown. These are likely
#'mistakes in the primary data (i.e. the accuracy should always be known). For now,
#'we include captures with accuracy of both 0 and 1 in the final data.
#'
#'\strong{AvgEggMass} Egg measurements are included in the NIOO database, but these are a bit more difficult to include
#'because they aren't associated with a given brood (they can be weighed before and after a cross fostering). For now,
#'we don't include this data, but we hope to in the future. Therefore, AvgEggMass is currently just NA.
#'
#'\strong{ChickAge:} For every capture, we estimate the age of a chick as the difference between the hatch date
#'taken from BroodIDFledged (in Individual_data) and the CaptureDate. We include chick ages for all individuals
#'up until 30 days post hatching to accomodate possible late fledging.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates 4 .csv files or R data frames in the SPI-Birds standard format.
#'@export
#'@import dplyr
#'@import DBI
#'@import purrr

format_NIOO <- function(db = choose_directory(),
                        species = NULL,
                        pop = NULL,
                        path = ".",
                        output_type = "R"){

  #Force user to select directory
  force(db)

  db <- paste0(db, "/NIOO_PrimaryData.accdb")

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  message("Connecting to database...")

  ###N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the NIOO database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  # LOCATION DATA

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
                                         function(.x){

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

  # SPECIES AND POPUALATION FILTERS

  #Create a subset of the chosen species
  #Where argument 'species' is unused, include all species in the table (listed in description)
  if(is.null(species)){

    species_filter <- Species_codes$SpeciesID

  } else {

    species_filter <- Species_codes[Species_codes$Code %in% species, ]$SpeciesID

  }

  if(is.null(pop)){

    pop_filter <- unique(Locations$PopID)

  } else {

    pop_filter <- pop

  }

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_NIOO(connection, Locations, species_filter, pop_filter)

  # BROOD DATA

  #This data will include 1 row for every recorded brood.

  message("Compiling brood information...")

  Brood_data <- create_brood_NIOO(connection, Individual_data, Locations, species_filter, pop_filter)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_NIOO(connection, Brood_data, Individual_data, Locations, species_filter, pop_filter)

  # NESTBOX DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_NIOO(connection, Locations, species_filter, pop_filter)

  # WRANGLE DATA FOR SAVING

  #Calculate mean mass, tarsus for all chicks in the brood
  #AT 14-16 DAYS POST HATCHING!!!
  avg_mass <- Brood_data %>%
    #Join mass and tarsus data for chicks by linking to the brood in which they were born
    dplyr::left_join(dplyr::left_join(dplyr::select(Capture_data, CaptureDate, IndvID, Mass, Tarsus),
                                      dplyr::select(Individual_data, IndvID, BroodID = BroodIDFledged), by = "IndvID"), by = "BroodID") %>%
    #Filter those that were not caught at 14 - 16 days
    dplyr::mutate(CaptureDate = lubridate::ymd(CaptureDate)) %>%
    dplyr::filter(CaptureDate >= (HatchDate + 14) & CaptureDate <= (HatchDate + 16)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA_real_, NumberEggs = NA_integer_, AvgChickMass = mean(Mass, na.rm = TRUE),
                     NumberChicksMass = length(stats::na.omit(Mass)),
                     AvgTarsus = mean(Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(Tarsus)),
                     OriginalTarsusMethod = "Alternative")

  #Join this average mass/tarsus data back into the brood data table
  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_mass, by = "BroodID")  %>%
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID = BroodLocation, FemaleID, MaleID, ClutchType_observed, ClutchType_calculated, LayDate, LayDateError,
                  ClutchSize, ClutchSizeError, HatchDate, HatchDateError, BroodSize, BroodSizeError, FledgeDate, FledgeDateError, NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass, AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)

  # REMOVE UNWANTED COLUMNS AND CHANGE FORMATS
  Individual_data <- Individual_data %>%
    dplyr::mutate(IndvID = as.character(IndvID)) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge, Sex)

  Capture_data <- Capture_data %>%
    dplyr::mutate(IndvID = as.character(IndvID),
                  LocationID = as.character(LocationID),
                  CapturePlot = as.character(CapturePlot),
                  ReleasePlot = as.character(ReleasePlot),
                  CaptureDate = lubridate::ymd(CaptureDate))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_NIOO.csv"), row.names = F)

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

#' Create individual data table for NIOO pipeline.
#'
#' Create individual data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_individual_NIOO <- function(database, location_data, species_filter, pop_filter){

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant).
  #This only includes data that DOES NOT CHANGE over the individual's lifetime.

  #Create table with description of sex codes
  Sex_data <- dplyr::tbl(database, "dbo_tl_Sexe") %>%
    dplyr::select(Sexe = ID, Sex = Description)

  Individual_data   <- dplyr::tbl(database, "dbo_tbl_Individual") %>%
    #Filter only required species
    dplyr::filter(SpeciesID %in% species_filter) %>%
    #Select only the basic info that we want:
    # - Individual ID
    # - GeneticBroodID
    # - BroodID (for cross fostering experiments)
    # - Species
    # - Sex
    # - RingSeason (year of first ringing)
    # - RingAge (EURING age at first ringing)
    # - RingNumber
    dplyr::select(IndvID = ID, GeneticBroodID, BroodID, SpeciesID, Sexe, RingSeason = RingYear, RingAge, RingNumber) %>%
    #Add in sex description. Sex categories are described in "dbo_tl_Sexe"
    dplyr::collect() %>%
    #Add sex from standard protocol
    #Add a ring age observed for determining Age_observed in Capture_data
    dplyr::mutate(Sex = dplyr::case_when(.$Sexe %in% c(1, 3, 5) ~ "F",
                                         .$Sexe %in% c(2, 4, 6) ~ "M")) %>%
    #Add in the first capture location
    #This is needed to determine which population the bird belongs too.
    dplyr::left_join(tbl(database, "dbo_tbl_Capture") %>%
                       dplyr::arrange(Individual, CaptureDate, CaptureTime) %>%
                       dplyr::select(IndvID = Individual, CaptureLocation) %>%
                       dplyr::group_by(IndvID) %>%
                       dplyr::collect() %>%
                       dplyr::slice(1), by = "IndvID") %>%
    #Relate the capturelocation to the three letter PopID
    dplyr::left_join(dplyr::select(location_data, PopID, CaptureLocation = ID), by = "CaptureLocation") %>%
    #Filter only chosen pop
    dplyr::filter(PopID %in% pop_filter)

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
    dplyr::select(IndvID, RingNumber, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex) %>%
    #Convert RingAge into either chick or adult
    dplyr::mutate(RingAge = dplyr::case_when(.$RingAge %in% c(1, 2, 3) ~ "chick",
                                             .$RingAge > 3 ~ "adult"))

  return(Individual_data)

}

#' Create capture data table for NIOO pipeline.
#'
#' Create capture data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param Individual_data Data frame generated by
#'   \code{\link{create_individual_NIOO}}.
#' @param Brood_data Data frame generated by
#'   \code{\link{create_brood_NIOO}}.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_capture_NIOO <- function(database, Brood_data, Individual_data, location_data, species_filter, pop_filter){

  #Capture data includes all times an individual was captured (with measurements like mass, tarsus etc.).
  #This will include first capture as nestling
  #This can include multiple records for a single individual.
  Capture_data <- dplyr::tbl(database, "dbo_tbl_Capture") %>%
    dplyr::select(CaptureID = ID, AccuracyOfDate, CaptureDate, CaptureTime, IndvID = Individual, CaptureLocation, ReleaseLocation, CaptureType) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    dplyr::left_join(dplyr::tbl(database, "dbo_vw_MI_CaptureCaptureData") %>%
                       dplyr::select(CaptureID, SpeciesID, Observer, Weight, Tarsus, Wing_Length, Age), by = "CaptureID") %>%
    #Filter target species
    dplyr::filter(SpeciesID %in% species_filter & AccuracyOfDate %in% c(0, 1) & CaptureType %in% c(1, 2)) %>%
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
                ReleaseLocation, Observer, Weight, Tarsus, WingLength = Wing_Length, Age) %>%
    dplyr::collect() %>%
    #Join in information on when the individual was first ringed (left join from the IndvData)
    #This is used to determine the age of each individual (EURING) at the time of capture
    dplyr::left_join(dplyr::select(Individual_data, IndvID, RingSeason), by = "IndvID") %>%
    dplyr::mutate(Age_observed = as.integer(Age),
                  CaptureDate = as.Date(CaptureDate),
                  BreedingSeason = as.integer(lubridate::year(CaptureDate))) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason, showpb = TRUE) %>%
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
                  ObserverID = as.character(Observer)) %>%
    #Arrange by species, indv and date/time
    dplyr::arrange(Species, IndvID, CaptureDate, CaptureTime) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    dplyr::left_join(dplyr::select(location_data, CapturePlot = AreaID, CaptureLocation = ID, CapturePopID = PopID), by = "CaptureLocation") %>%
    dplyr::left_join(dplyr::select(location_data, ReleasePlot = AreaID, ReleaseLocation = ID, ReleasePopID = PopID), by = "ReleaseLocation") %>%
    dplyr::filter(CapturePopID %in% pop_filter) %>%
    #Make mass and tarsus into g and mm
    dplyr::mutate(LocationID = CaptureLocation, Mass = dplyr::na_if(Weight/100, y = 0), Tarsus = dplyr::na_if(Tarsus/10, 0))

  #Join in hatch date for each brood where an individual fledged
  Capture_data <- Capture_data %>%
    dplyr::left_join(dplyr::select(Individual_data, IndvID, BroodID = BroodIDFledged), by = "IndvID") %>%
    dplyr::left_join(dplyr::select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    #Determine difference between hatch and capture date for all individuals
    #that were ~before fledging (we'll say up until 30 days because this covers all possibilites)
    dplyr::mutate(ChickAge = purrr::pmap_int(.l = list(HatchDate, CaptureDate, IndvID),
                                             .f = ~{

                                               x <- as.integer(difftime(..2, ..1))

                                               if(!is.na(x) & between(x, 0, 30)){

                                                 return(x)

                                               } else {

                                                 return(NA_integer_)

                                               }

                                             })) %>%
    #Arrange columns
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot,
                  Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed, Age_calculated, ChickAge)

  return(Capture_data)

}

#' Create brood data table for NIOO pipeline.
#'
#' Create brood data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param Individual_data Data frame generated by
#'   \code{\link{create_individual_NIOO}}.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_brood_NIOO <- function(database, Individual_data, location_data, species_filter, pop_filter){

  target_locations <- dplyr::filter(location_data, PopID %in% pop_filter)

  Brood_data  <- dplyr::tbl(database, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species in designated population
    dplyr::filter(BroodSpecies %in% species_filter & BroodLocationID %in% !!target_locations$ID) %>%
    #Link the ClutchType description (e.g. first, second, replacement)
    dplyr::left_join(dplyr::tbl(database, "dbo_tl_BroodType") %>%
                       dplyr::select(BroodType = ID, Description), by = "BroodType") %>%
    #Extract basic info that we want:
    # - BreedingSeason
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
  dplyr::select(BreedingSeason = BroodYear, BroodID = ID, BroodSpecies, BroodLocation = BroodLocationID, Female_ring = RingNumberFemale, Male_ring = RingNumberMale,
                ClutchType_observed = Description, LayDate = LayDate, LayDateError = LayDateDeviation,
                ClutchSize, HatchDate, BroodSize = NumberHatched, BroodSizeError = NumberHatchedDeviation,
                FledgeDate, NumberFledged, NumberFledgedError = NumberFledgedDeviation, ExperimentID = ExperimentCode) %>%
    dplyr::collect() %>%
    #Join PopID (including site ID and nestbox ID) and filter only the pop(s) of interest
    dplyr::left_join(dplyr::select(location_data, Plot = AreaID, BroodLocation = ID, PopID), by = "BroodLocation") %>%
    #Account for error in brood size
    dplyr::mutate(BroodSizeError = BroodSizeError/2, NumberFledgedError = NumberFledgedError/2,
                  LayDateError = LayDateError/2,
                  BroodSize = as.integer(BroodSize + BroodSizeError),
                  NumberFledged = as.integer(NumberFledged + NumberFledgedError),
                  LayDate = lubridate::ymd(LayDate) + LayDateError,
                  HatchDate = lubridate::ymd(HatchDate),
                  FledgeDate = lubridate::ymd(FledgeDate),
                  ExperimentID = as.character(!is.na(dplyr::na_if(ExperimentID, ""))),
                  Plot = as.character(Plot)) %>%
    #Include species letter codes for all species
    dplyr::mutate(Species = dplyr::case_when(.$BroodSpecies == 14400 ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             .$BroodSpecies == 14640 ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .$BroodSpecies == 13490 ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                             .$BroodSpecies == 14620 ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .$BroodSpecies == 14790 ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .$BroodSpecies == 15980 ~ Species_codes[Species_codes$SpeciesID == 15980, ]$Code,
                                             .$BroodSpecies == 14610 ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                  #Adjust ClutchType names to fit "first", "second", "replacement".
                  #We ignore any uncertainty (e.g. "probably second" is just listed as "second")
                  #ClutchTypes like 'different species inside one clutch' are listed as NA.
                  ClutchType_observed = dplyr::case_when(grepl(pattern = "replacement", .$ClutchType_observed) ~ "replacement",
                                                         grepl(pattern = "second clutch after|probably second|third clutch", .$ClutchType_observed) ~ "second",
                                                         grepl(pattern = "first clutch", .$ClutchType_observed) ~ "first")) %>%
    #Make individuals with no ring number into NA
    dplyr::mutate(Female_ring = purrr::map_chr(.x = .$Female_ring,
                                               .f = ~ifelse(.x == "0000000000"|.x == "",
                                                            NA, .x)),
                  Male_ring = purrr::map_chr(.x = .$Male_ring,
                                             .f = ~ifelse(.x == "0000000000"|.x == "",
                                                          NA, .x))) %>%
    ########### N.B. CURRENTLY THERE ARE A FEW (~25) RING NUMBERS THAT ARE ASSIGNED TO 2 INDIVIDUALS
    ########### THIS MEANS THAT WE WILL GET A FEW DUPLICATE RECORDS WITH THIS APPROACH
    ########### THESE NEED TO BE ADDRESSED IN THE DATABASE BEFORE THEY CAN BE FIXED HERE
    #Join in ID numbers for the parents of the brood from the individual table above
    dplyr::left_join(dplyr::select(Individual_data, Female_ring = RingNumber, FemaleID = IndvID) %>%
                       dplyr::filter(Female_ring != ""), by = "Female_ring") %>%
    dplyr::left_join(select(Individual_data, Male_ring = RingNumber, MaleID = IndvID) %>%
                       dplyr::filter(Male_ring != ""), by = "Male_ring") %>%
    dplyr::arrange(PopID, BreedingSeason, Species, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = TRUE)) %>%
  #Add extra columns where data was not provided
  dplyr::mutate(ClutchSizeError = NA_real_, HatchDateError = NA_real_, FledgeDateError = NA_real_,
                BroodLocation = as.character(BroodLocation), BroodID = as.character(BroodID),
                FemaleID = as.character(FemaleID), MaleID = as.character(MaleID))

  return(Brood_data)

}

#' Create location data table for NIOO pipeline.
#'
#' Create location data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_location_NIOO <- function(database, location_data, species_filter, pop_filter){

  #Extract information on nestbox locations
  Location_data <- dplyr::tbl(database, "dbo_tbl_NestboxAppearance") %>%
    dplyr::collect() %>%
    #Join together information on the nestbox locations (e.g. latitude, longitude, nestbox name) and information on each nestbox that was there (e.g. how long before it was replaced).
    #This is necessary because one nestbox location could have multiple nestboxes erected at it over the study period.
    dplyr::right_join(dplyr::select(location_data, Location = ID, Latitude, Longitude, PopID),
                     by = "Location") %>%
    dplyr::filter(PopID %in% pop_filter) %>%
    dplyr::select(LocationID = Location, NestboxID = ID, LocationType = NestBoxType, PopID, Latitude, Longitude, StartSeason = StartYear, EndSeason = EndYear) %>%
    dplyr::mutate(LocationID = as.character(LocationID),
                  NestboxID = as.character(NestboxID),
                  LocationType = dplyr::case_when(.$LocationType %in% c(0:22, 40:41) ~ "NB",
                                                  .$LocationType %in% c(90, 101) ~ "MN"),
                  Habitat = dplyr::case_when(.$PopID %in% c("VLI", "HOG", "WES", "BUU") ~ "Mixed",
                                             .$PopID %in% c("OOS", "LIE", "WAR") ~ "Deciduous")) %>%
    dplyr::arrange(LocationID, StartSeason)

  return(Location_data)

}
