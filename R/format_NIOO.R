#'Construct standard format for NIOO data.
#'
#'A pipeline to produce the standard format for 8 hole-nesting bird study
#'populations at the Netherlands Institute of Ecology (NIOO-KNAW).
#'
#'This section provides details on data management choices that are unique to
#'the NIOO database. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
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
#'N.B This is no longer applied as it led to many individuals that were considered 'never captured'.
#'Will talk to data owner about this.
#'
#'\strong{AvgEggMass} Egg measurements are included in the NIOO database, but these are a bit more difficult to include
#'because they aren't associated with a given brood (they can be weighed before and after a cross fostering). For now,
#'we don't include this data, but we hope to in the future. Therefore, AvgEggMass is currently just NA.
#'
#'\strong{ChickAge:} For every capture, we estimate the age of a chick as the difference between the hatch date
#'taken from BroodIDFledged (in Individual_data) and the CaptureDate. We include chick ages for all individuals
#'up until 30 days post hatching to accommodate possible late fledging.
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

    species_filter <- species_codes$SpeciesID

  } else {

    species_filter <- species_codes[species_codes$Species %in% species, ]$SpeciesID

  }

  if(is.null(pop)){

    pop_filter <- unique(Locations$PopID)

  } else {

    pop_filter <- pop

  }

  # BROOD DATA

  #This data will include 1 row for every recorded brood.

  message("Compiling brood information...")

  Brood_data <- create_brood_NIOO(connection, Locations, species_filter, pop_filter)

  #Move capture data first.
  #This allows us to remove egg only captures and unusual population translocations
  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_NIOO(connection, Brood_data, Locations, species_filter, pop_filter)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_NIOO(connection, Capture_data, Locations, species_filter, pop_filter)

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
    dplyr::filter(CaptureDate >= (HatchDate_observed + 14) & CaptureDate <= (HatchDate_observed + 16)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgEggMass = NA_real_, NumberEggs = NA_integer_, AvgChickMass = mean(Mass, na.rm = TRUE),
                     NumberChicksMass = length(stats::na.omit(Mass)),
                     AvgTarsus = mean(Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(Tarsus)),
                     OriginalTarsusMethod = "Alternative")

  #Join this average mass/tarsus data back into the brood data table
  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_mass, by = "BroodID") %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(brood_data_template[1, !(names(brood_data_template) %in% names(.))]) %>%
    ## Reorder columns
    dplyr::select(names(brood_data_template))

  # REMOVE UNWANTED COLUMNS AND CHANGE FORMATS
  Individual_data <- Individual_data %>%
    dplyr::mutate(dplyr::across(.cols = ends_with("ID"), .fns = ~as.character(.)))

  Capture_data <- Capture_data %>%
    dplyr::mutate(IndvID = as.character(IndvID),
                  LocationID = as.character(LocationID),
                  CapturePlot = as.character(CapturePlot),
                  ReleasePlot = as.character(ReleasePlot),
                  CaptureDate = lubridate::ymd(CaptureDate)) %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(capture_data_template))

  Brood_data <- Brood_data %>%
    dplyr::mutate(dplyr::across(.cols = ends_with("ID"), .fns = ~as.character(.)))

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

#' Create brood data table for NIOO pipeline.
#'
#' Create brood data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_brood_NIOO <- function(database, location_data, species_filter, pop_filter){

  target_locations <- dplyr::filter(location_data, PopID %in% pop_filter)

  Male_rings <- dplyr::tbl(database, "dbo_tbl_Individual") %>%
    dplyr::select(MaleID = .data$ID, Male_ring = .data$RingNumber)

  Female_rings <- dplyr::tbl(database, "dbo_tbl_Individual") %>%
    dplyr::select(FemaleID = .data$ID, Female_ring = .data$RingNumber)

  Brood_data <- dplyr::tbl(database, "dbo_tbl_Brood") %>%
    #Subset only broods of designated species in designated population
    dplyr::filter(.data$BroodSpecies %in% species_filter & .data$BroodLocationID %in% !!target_locations$ID) %>%
    #Link the ClutchType description (e.g. first, second, replacement)
    dplyr::left_join(dplyr::tbl(database, "dbo_tl_BroodType") %>%
                       dplyr::select(BroodType = .data$ID, .data$Description), by = "BroodType") %>%
    dplyr::mutate(Female_ring = dplyr::sql("IIF(RingNumberFemale = '0000000000' OR RingNumberFemale = '', NULL, CStr(RingNumberFemale))"),
                  Male_ring = dplyr::sql("IIF(RingNumberMale = '0000000000' OR RingNumberMale = '', NULL, Cstr(RingNumberMale))"),
                  ##FIXME: Translate ExperimentID to the standard format
                  ExperimentID = dplyr::sql("IIF(IsNull(ExperimentCode) OR ExperimentCode = '', Null, CStr(ExperimentCode))")) %>%
    dplyr::left_join(Male_rings, by = "Male_ring") %>%
    dplyr::left_join(Female_rings, by = "Female_ring") %>%
    dplyr::collect() %>%
    dplyr::mutate(HatchDate_observed = lubridate::ymd(.data$HatchDate),
                  ##FIXME: Translate HatchDateAccuracy into min & max
                  LayDate_observed = lubridate::ymd(.data$LayDate),
                  LayDate_min = .data$LayDate_observed - .data$LayDateDeviation,
                  LayDate_max = .data$LayDate_observed + .data$LayDateDeviation,
                  FledgeDate_observed = lubridate::ymd(.data$FledgeDate),
                  ##FIXME: Translate HatchDateAccuracy into min & max
                  ClutchSize_observed = .data$ClutchSize,
                  ClutchSize_min = .data$ClutchSizeMinimum,
                  BroodSize_observed = .data$NumberHatched,
                  BroodSize_min = .data$NumberHatched - .data$NumberHatchedDeviation,
                  BroodSize_max = .data$NumberHatched + .data$NumberHatchedDeviation,
                  NumberFledged_observed = .data$NumberFledged,
                  NumberFledged_min = .data$NumberFledged - .data$NumberFledgedDeviation,
                  NumberFledged_max = .data$NumberFledged + .data$NumberFledgedDeviation,
                  ClutchType_observed = .data$Description,
                  BreedingSeason = .data$BroodYear,
                  BroodID = as.character(.data$ID),
                  LocationID = as.character(.data$BroodLocationID)) %>%
    dplyr::left_join(dplyr::select(location_data, Plot = .data$AreaID, BroodLocationID = .data$ID, .data$PopID), by = "BroodLocationID") %>%
    dplyr::mutate(Species = dplyr::case_when(.$BroodSpecies == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .$BroodSpecies == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .$BroodSpecies == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .$BroodSpecies == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .$BroodSpecies == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .$BroodSpecies == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .$BroodSpecies == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species),
                  #Adjust ClutchType names to fit "first", "second", "replacement".
                  #We ignore any uncertainty (e.g. "probably second" is just listed as "second")
                  #ClutchTypes like 'different species inside one clutch' are listed as NA.
                  ClutchType_observed = dplyr::case_when(grepl(pattern = "replacement", .$ClutchType_observed) ~ "replacement",
                                                         grepl(pattern = "second clutch after|probably second|third clutch", .$ClutchType_observed) ~ "second",
                                                         grepl(pattern = "first clutch", .$ClutchType_observed) ~ "first"),
                  Plot = as.character(.data$Plot)) %>%
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = TRUE, protocol_version = "1.1")) %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0 ,!(names(brood_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(brood_data_template))

  return(Brood_data)

}

#' Create capture data table for NIOO pipeline.
#'
#' Create capture data table in standard format for data from NIOO.
#'
#' @param database Connection to NIOO Access database.
#' @param Brood_data Data frame generated by
#'   \code{\link{create_brood_NIOO}}.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_capture_NIOO <- function(database, Brood_data, location_data, species_filter, pop_filter){

  RawCaptures <- dplyr::tbl(database, "dbo_tbl_Capture") %>%
    #Filter out egg captures. Reduce records early
    dplyr::filter(.data$CaptureType %in% c(1, 2)) %>%
    #Reduce to only necessary columns
    dplyr::select(CaptureID = .data$ID, .data$CaptureDate,
                  .data$CaptureTime, IndvID = .data$Individual,
                  .data$CaptureLocation, .data$ReleaseLocation) %>%
    #Join in weight, tarsus and wing_length from secondary capture data table.
    dplyr::left_join(dplyr::tbl(database, "dbo_vw_MI_CaptureCaptureData") %>%
                       dplyr::select(.data$CaptureID, .data$SpeciesID, .data$Observer,
                                     .data$Weight, .data$Tarsus,
                                     .data$Wing_Length, .data$Age), by = "CaptureID") %>%
    #Join in Individual data so that we have an associated brood (used to determine chick age)
    dplyr::left_join(dplyr::tbl(database, "dbo_tbl_Individual") %>%
                       dplyr::select(IndvID = .data$ID, .data$BroodID), by = "IndvID") %>%
    #Now that we have joined species information, filter unwanted species out
    dplyr::filter(.data$SpeciesID %in% species_filter) %>%
    #Convert CaptureDate into DateTime and extract BreedingSeason information
    dplyr::mutate(CaptureDateTime = dplyr::sql("IIF(CaptureDate <> '', CDate(CaptureDate), NULL)"),
                  BreedingSeason = dplyr::sql("Year(CaptureDateTime)"))

  #Create a summary for each individual with their ringing season (i.e. the min BreedingSeason)
  RingSeason_summary <- RawCaptures %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(RingSeason = min(.data$BreedingSeason, na.rm = TRUE))

  #Join this information back in so that we know the ringing season
  Captures_w_RingSeason <- RawCaptures %>%
    dplyr::left_join(RingSeason_summary, by = "IndvID")

  #Now we can collect the data and continue in dplyr
  Capture_data <- Captures_w_RingSeason %>%
    dplyr::collect() %>%
    dplyr::mutate(Age_observed = as.integer(.data$Age)) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason, showpb = TRUE) %>%
    #Include species letter codes for all species
    dplyr::ungroup() %>%
    dplyr::mutate(Species = dplyr::case_when(.$SpeciesID == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .$SpeciesID == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .$SpeciesID == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .$SpeciesID == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .$SpeciesID == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .$SpeciesID == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .$SpeciesID == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species),
                  #Add original tarsus method
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$Tarsus) ~ "Alternative"),
                  ObserverID = as.character(.data$Observer)) %>%
    #Arrange by species, indv and date/time
    dplyr::arrange(.data$Species, .data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    dplyr::left_join(dplyr::select(location_data, CapturePlot = .data$AreaID, CaptureLocation = .data$ID, CapturePopID = .data$PopID), by = "CaptureLocation") %>%
    dplyr::left_join(dplyr::select(location_data, ReleasePlot = .data$AreaID, ReleaseLocation = .data$ID, ReleasePopID = .data$PopID), by = "ReleaseLocation") %>%
    ## TODO: There are 7 other individuals (480602-8) that have a CaptureLocation = 432
    #In individual data (i.e. Hoge Veluwe)
    #But in capture data, their capture location is 8681, which corresponds to Heikamp.
    #Is Heikamp inside HOG? Is it a different location? If so, are these mistakes?
    dplyr::filter(.data$CapturePopID %in% pop_filter) %>%
    #Make mass and tarsus into g and mm
    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  LocationID = as.character(.data$CaptureLocation),
                  Mass = dplyr::na_if(.data$Weight/100, y = 0),
                  Tarsus = dplyr::na_if(.data$Tarsus/10, 0))

  # Join in hatch date for each brood where an individual fledged
  # Do this later once we complete Individual_data
  Capture_data <- Capture_data %>%
    dplyr::left_join(dplyr::select(Brood_data, .data$BroodID, .data$HatchDate_observed), by = "BroodID") %>%
    #Determine difference between hatch and capture date for all individuals
    #that were ~before fledging (we'll say up until 30 days because this covers all possibilites)
    dplyr::mutate(diff = as.integer(lubridate::ymd(.data$CaptureDate) - .data$HatchDate_observed),
                  ChickAge = dplyr::case_when(!is.na(.data$diff) & between(.data$diff, 0, 30) ~ .data$diff,
                                              TRUE ~ NA_integer_),
                  CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"),
                  CaptureAlive = TRUE, ReleaseAlive = TRUE, ##FIXME: Ask Marcel about dead captures
                  ExperimentID = NA_character_) ##FIXME: Ask Marcel about individual only experiments.

  return(Capture_data)

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
#' @param Capture_data Data frame. Capture data output from pipeline.
#'
#' @return A data frame.

create_individual_NIOO <- function(database, Capture_data, location_data, species_filter, pop_filter){

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant).

  individuals <- dplyr::tbl(database, "dbo_tbl_Individual") %>%
    #Filter only required species
    #Remove individual records that are from tissue samples (i.e. ring number is TS)
    dplyr::filter(.data$SpeciesID %in% species_filter & dplyr::sql("RingNumber NOT LIKE 'TS*'")) %>%
    #Translate Sexe into F or M
    ## FIXME: Which sex should this be grouped as?
    #Convert ring age to adult or chick
    #Create BroodIDFledged and Laid
    dplyr::mutate(Sex_calculated = dplyr::sql("IIF(Sexe = 1 OR Sexe = 3 OR Sexe = 5, 'F', IIF(Sexe = 2 OR Sexe = 4 OR Sexe = 6, 'M', Null))"),
                  RingAge_category = dplyr::sql("IIF(RingAge IN (1, 2, 3), 'chick', IIF(RingAge > 3, 'adult', Null))"),
                  BroodIDLaid = dplyr::sql("IIF(IsNull(GeneticBroodID), BroodID, GeneticBroodID)"),
                  BroodIDFledged = dplyr::sql("IIF(IsNull(BroodID), GeneticBroodID, BroodID)"),
                  IndvID = .data$ID) %>%
    dplyr::collect()

  # Determine first captures (after removing eggs)
  first_captures <- dplyr::tbl(database, "dbo_tbl_Capture") %>%
    dplyr::filter(.data$CaptureType == 1L | .data$CaptureType == 2L) %>%
    dplyr::group_by(.data$Individual) %>%
    dplyr::summarise(FirstCaptureLocation = dplyr::sql("First(CaptureLocation)")) %>%
    dplyr::rename(IndvID = "Individual") %>%
    dplyr::collect()

    # Join first capture location into individual data
  Individual_data <- individuals %>%
    dplyr::left_join(first_captures, by = "IndvID") %>%
    # Relate the capturelocation to the three letter PopID
    dplyr::left_join(dplyr::select(location_data, .data$PopID, FirstCaptureLocation = .data$ID),
                     by = "FirstCaptureLocation") %>%
    #Filter only chosen pops
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    #Convert numbers to species codes
    dplyr::mutate(Species = dplyr::case_when(.$SpeciesID == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .$SpeciesID == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .$SpeciesID == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .$SpeciesID == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .$SpeciesID == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .$SpeciesID == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .$SpeciesID == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species),
                  RingAge = .data$RingAge_category,
                  RingSeason = .data$RingYear,
                  BroodIDLaid = as.character(.data$BroodIDLaid),
                  BroodIDFledged = as.character(.data$BroodIDFledged)) %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0, !(names(individual_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(individual_data_template))

  return(Individual_data)

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
    dplyr::right_join(dplyr::select(location_data, Location = .data$ID, .data$Latitude, .data$Longitude, .data$PopID),
                      by = "Location") %>%
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    dplyr::select(LocationID = .data$Location, NestboxID = .data$ID,
                  LocationType = .data$NestBoxType, .data$PopID,
                  .data$Latitude, .data$Longitude,
                  StartSeason = .data$StartYear, EndSeason = .data$EndYear) %>%
    dplyr::mutate(LocationID = as.character(.data$LocationID),
                  NestboxID = as.character(.data$NestboxID),
                  LocationType = dplyr::case_when(.data$LocationType %in% c(0:22, 40:41) ~ "NB",
                                                  .data$LocationType %in% c(90, 101) ~ "MN"),
                  HabitatType = dplyr::case_when(.data$PopID %in% c("VLI", "HOG", "WES", "BUU") ~ "mixed",
                                                 .data$PopID %in% c("OOS", "LIE", "WAR") ~ "deciduous")) %>%
    dplyr::arrange(.data$LocationID, .data$StartSeason) %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(location_data_template[0, !(names(location_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(location_data_template))


  return(Location_data)

}
