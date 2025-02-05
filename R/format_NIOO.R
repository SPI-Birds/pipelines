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
#'ater}; tree sparrow \emph{Passer montanus}; and redstart \emph{Phoenicurus phoenicurus}. Other minority species are
#'excluded.
#'
#'\strong{Populations}: This pipeline extracts data for 7 populations managed by
#'NIOO-KNAW: Buunderkamp, Westerheide, Hoge Veluwe, Warnsborn,
#'Vlieland, Oosterhout, and Liesbos.
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
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export
#'@import rlang
#'@importFrom dplyr `%>%`
#'@importFrom utils read.csv

format_NIOO <- function(db = choose_directory(),
                        species = NULL,
                        pop = NULL,
                        path = ".",
                        output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  #Force user to select directory
  force(db)

  # Create path to Access database file
  dsn <- paste0(gsub("\\\\", "/", db), "/NIOO_PrimaryData.accdb")

  # Record start time to estimate processing time of pipeline
  start_time <- Sys.time()

  message("Extracting Access tables...")

  # Connect to Access database and export relevant tables to a selected output directory
  access_tables <- c("dbo_tl_AreaGroup", "dbo_tx_Area_AreaGroup", "dbo_tbl_Location",
                     "dbo_tbl_Individual", "dbo_tl_BroodType", "dbo_tbl_Brood", "dbo_tbl_Capture",
                     "dbo_vw_MI_CaptureCaptureData", "dbo_tbl_NestboxAppearance")

  table_dir <- paste0(db, "/NIOO_PrimaryData_tables")

  export_access_db(dsn,
                   table = access_tables,
                   output_dir = table_dir)

  # LOCATION DATA

  # We first need to compile location information (and area names) as this will be included with all data tables.

  # List the main study sites.
  main_sites <- c("Buunderkamp", "Westerheide", "Hoge Veluwe",
                  "Warnsborn", "Vlieland", "Oosterhout", "Liesbos")

  # Extract the corresponding areas from the AreaGroup table
  loc_start_new <- Sys.time()
  Locations <- read.csv(paste0(table_dir, "/", "dbo_tl_AreaGroup", ".csv")) %>%
    dplyr::filter(grepl(x = .data$Name, pattern = paste(main_sites, collapse = "|"))) %>%
    dplyr::select("AreaGroup" = "ID", "Name") %>%
    # Create three letter PopID code for each AreaGroup (i.e. population)
    dplyr::mutate(PopID = toupper(substr(.data$Name, start = 1, stop = 3))) %>%
    #Join in all the Areas within each AreaGroup (i.e. 'plots' within each population).
    dplyr::left_join(read.csv(paste0(table_dir, "/", "dbo_tx_Area_AreaGroup", ".csv")) %>%
                       dplyr::select("Area", "AreaGroup"),
                     by = "AreaGroup") %>%
    dplyr::rename("AreaID" = "Area") %>%
    #Join in all locations that are inside each Area within each AreaGroup (i.e. nest boxes/mist net locations in each plot within each population).
    dplyr::left_join(read.csv(paste0(table_dir, "/", "dbo_tbl_Location", ".csv")) %>%
                       dplyr::select("ID", "UserPlaceName", "AreaID", "Latitude", "Longitude"),
                     by = "AreaID")
  (loc_duration_new <- Sys.time() - loc_start_new)

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
  brood_start_new <- Sys.time()
  Brood_data <- create_brood_NIOO(table_dir, Locations, species_filter, pop_filter)
  (brood_duration_new <- Sys.time() - brood_start_new)

  #Move capture data first.
  #This allows us to remove egg only captures and unusual population translocations
  # CAPTURE DATA

  message("Compiling capture information...")
  capture_start_new <- Sys.time()
  Capture_data <- create_capture_NIOO(table_dir, Brood_data, Locations, species_filter, pop_filter)
  (capture_duration_new <- Sys.time() - capture_start_new)
  # INDIVIDUAL DATA

  message("Compiling individual information...")
  individual_start_new <- Sys.time()
  Individual_data <- create_individual_NIOO(table_dir, Capture_data, Locations, species_filter, pop_filter)
  (individual_duration_new <- Sys.time() - individual_start_new)
  # NESTBOX DATA

  message("Compiling nestbox information...")
  location_start_new <- Sys.time()
  Location_data <- create_location_NIOO(table_dir, Locations, species_filter, pop_filter)
  (location_duration_new <- Sys.time() - location_start_new)
  # WRANGLE DATA FOR SAVING

  #Calculate mean mass, tarsus for all chicks in the brood
  #AT 14-16 DAYS POST HATCHING!!!
  avg_mass <- Brood_data %>%
    #Join mass and tarsus data for chicks by linking to the brood in which they were born
    dplyr::left_join(dplyr::left_join(Capture_data %>%
                                        dplyr::select("CaptureDate", "IndvID", "Mass", "Tarsus"),
                                      Individual_data %>%
                                        dplyr::select("IndvID", "BroodID" = "BroodIDFledged"),
                                      by = "IndvID"),
                     by = "BroodID") %>%
    #Filter those that were not caught at 14 - 16 days
    dplyr::mutate(CaptureDate = lubridate::ymd(.data$CaptureDate)) %>%
    dplyr::filter(.data$CaptureDate >= (.data$HatchDate_observed + 14),
                  .data$CaptureDate <= (.data$HatchDate_observed + 16)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgEggMass = NA_real_,
                     NumberEggs = NA_integer_,
                     AvgChickMass = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass = length(stats::na.omit(.data$Mass)),
                     AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(.data$Tarsus)),
                     OriginalTarsusMethod = "Alternative")

  #Join this average mass/tarsus data back into the brood data table
  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_mass,
                     by = "BroodID") %>%
    ## Keep only necessary columns
    dplyr::select(tidyselect::contains(names(brood_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(brood_data_template[1, !(names(brood_data_template) %in% names(.))]) %>%
    ## Reorder columns
    dplyr::select(names(brood_data_template))

  # REMOVE UNWANTED COLUMNS AND CHANGE FORMATS
  Individual_data <- Individual_data %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::ends_with("ID"), .fns = ~as.character(.)))

  Capture_data <- Capture_data %>%
    dplyr::mutate(IndvID = as.character(.data$IndvID),
                  LocationID = as.character(.data$LocationID),
                  CapturePlot = as.character(.data$CapturePlot),
                  ReleasePlot = as.character(.data$ReleasePlot),
                  CaptureDate = lubridate::ymd(.data$CaptureDate)) %>%
    ## Keep only necessary columns
    dplyr::select(tidyselect::contains(names(capture_data_template))) %>%
    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(capture_data_template))

  Brood_data <- Brood_data %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::ends_with("ID"), .fns = ~as.character(.)))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_NIOO.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_NIOO.csv"), row.names = F)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_NIOO.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data,
                protocol_version = protocol_version))

  }

}

#' Create brood data table for NIOO pipeline.
#'
#' Create brood data table in standard format for data from NIOO.
#'
#' @param dir Path to directory containing the relevant table exports from the NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_brood_NIOO <- function(dir, location_data, species_filter, pop_filter){

  target_locations <- location_data %>%
    dplyr::filter(.data$PopID %in% pop_filter)

  # Individual data for ring numbers
  individuals <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Individual", ".csv")) %>%
    # Replace "\x85" (which is the Windows-1252 code point for the ellipsis '...') by three distinct points,
    # which arises when we use Jackcess (which uses the UTF-16LE encoding scheme) to export the Access tables to csv
    dplyr::mutate(dplyr::across(.cols = c("RingNumber", "IndividualNumber"),
                                .fns = ~{

                                  stringr::str_replace(.x, "\x85", "...")

                                }))

  # FIXME fix multiple ring number assignments
  # There are multiple cases where 1 ring number is assigned to multiple individual IDs
  # To avoid this leading to brood duplicates, we ignore them for now
  duplicated_rings <- individuals %>%
    dplyr::filter(.data$RingNumber != "", !is.na(.data$RingNumber)) %>%
    dplyr::filter(duplicated(.data$RingNumber)) %>%
    dplyr::pull(.data$RingNumber)

  Male_rings <- individuals %>%
    dplyr::select("MaleID" = "ID", "Male_ring" = "RingNumber") %>%
    dplyr::filter(.data$Male_ring != "", !is.na(.data$Male_ring), !(.data$Male_ring %in% !!duplicated_rings))

  Female_rings <- individuals %>%
    dplyr::select("FemaleID" = "ID", "Female_ring" = "RingNumber") %>%
    dplyr::filter(.data$Female_ring != "", !is.na(.data$Female_ring), !(.data$Female_ring %in% !!duplicated_rings))

  Brood_types <- utils::read.csv(paste0(dir, "/", "dbo_tl_BroodType", ".csv")) %>%
    dplyr::select("BroodType" = "ID", "Description")

  Brood_data <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Brood", ".csv"), na.strings = c("", "NA")) %>%
    #Subset only broods of designated species in designated population
    dplyr::filter(.data$BroodSpecies %in% species_filter & .data$BroodLocationID %in% !!target_locations$ID) %>%
    #Set unknown ring numbers to NA
    dplyr::mutate(Female_ring = dplyr::case_when(.data$RingNumberFemale == "0000000000" ~ NA_character_,
                                                 .data$RingNumberFemale == "" ~ NA_character_,
                                                 TRUE ~ .data$RingNumberFemale),
                  Male_ring = dplyr::case_when(.data$RingNumberMale == "0000000000" ~ NA_character_,
                                               .data$RingNumberMale == "" ~ NA_character_,
                                               TRUE ~ .data$RingNumberMale)) %>%
    #Link the ClutchType description (e.g. first, second, replacement)
    dplyr::left_join(Brood_types,
                     by = "BroodType") %>%
    dplyr::left_join(Male_rings,
                     by = "Male_ring") %>%
    dplyr::left_join(Female_rings,
                     by = "Female_ring") %>%
    # Breeding values (LayDate, NumberHatched, NumberFledged) in the original data come with an observed value and a deviation value
    # The observed value is the *minimum* observed value,
    # The deviation value indicates the interval from observed value to maximum value
    # e.g., if NumberFledged = 6 and NumberFledgedDeviation = 2, the maximum number of fledged individuals is 6 + 2 = 8.
    # There is no explicit information about minimum values (except for ClutchSizeMinimum)
    ##FIXME: Translate HatchDateAccuracy into min & max
    ##FIXME: Translate ExperimentID to the standard format
    dplyr::mutate(ExperimentID = dplyr::na_if(.data$ExperimentCode, c("")),
                  HatchDate_observed = lubridate::ymd(.data$HatchDate),
                  LayDate_observed = lubridate::ymd(.data$LayDate),
                  LayDate_max = .data$LayDate_observed + .data$LayDateDeviation,
                  FledgeDate_observed = lubridate::ymd(.data$FledgeDate),
                  ClutchSize_observed = .data$ClutchSize,
                  ClutchSize_min = .data$ClutchSizeMinimum,
                  BroodSize_observed = .data$NumberHatched,
                  BroodSize_max = .data$NumberHatched + .data$NumberHatchedDeviation,
                  NumberFledged_observed = .data$NumberFledged,
                  NumberFledged_max = .data$NumberFledged + .data$NumberFledgedDeviation,
                  ClutchType_observed = .data$Description,
                  BreedingSeason = .data$BroodYear,
                  BroodID = as.character(.data$ID),
                  LocationID = as.character(.data$BroodLocationID)) %>%
    dplyr::left_join(location_data %>%
                       dplyr::select("Plot" = "AreaID", "BroodLocationID" = "ID", "PopID"),
                     by = "BroodLocationID") %>%
    dplyr::mutate(Species = dplyr::case_when(.data$BroodSpecies == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .data$BroodSpecies == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$BroodSpecies == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .data$BroodSpecies == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$BroodSpecies == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$BroodSpecies == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .data$BroodSpecies == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             .data$BroodSpecies == 11220 ~ species_codes[species_codes$SpeciesID == 11220, ]$Species),
                  #Adjust ClutchType names to fit "first", "second", "replacement".
                  #We ignore any uncertainty (e.g. "probably second" is just listed as "second")
                  #ClutchTypes like 'different species inside one clutch' are listed as NA.
                  ClutchType_observed = dplyr::case_when(grepl(pattern = "replacement", .data$ClutchType_observed) ~ "replacement",
                                                         grepl(pattern = "second clutch after|probably second|third clutch", .data$ClutchType_observed) ~ "second",
                                                         grepl(pattern = "first clutch", .data$ClutchType_observed) ~ "first"),
                  Plot = as.character(.data$Plot)) %>%
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = TRUE, protocol_version = "1.1"))

  return(Brood_data)

}

#' Create capture data table for NIOO pipeline.
#'
#' Create capture data table in standard format for data from NIOO.
#'
#' @param dir Path to directory containing the relevant table exports from the NIOO Access database.
#' @param Brood_data Data frame generated by
#'   \code{\link{create_brood_NIOO}}.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_capture_NIOO <- function(dir, Brood_data, location_data, species_filter, pop_filter){

  # Individual data for associated broods
  individuals <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Individual", ".csv")) %>%
    # Replace "\x85" (which is the Windows-1252 code point for the ellipsis '...') by three distinct points,
    # which arises when we use Jackcess (which uses the UTF-16LE encoding scheme) to export the Access tables to csv
    dplyr::mutate(dplyr::across(.cols = c("RingNumber", "IndividualNumber"),
                                .fns = ~{

                                  stringr::str_replace(.x, "\x85", "...")

                                }))

  RawCaptures <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Capture", ".csv")) %>%
    #Filter out egg captures. Reduce records early
    dplyr::filter(.data$CaptureType %in% c(1, 2)) %>%
    #Reduce to only necessary columns
    dplyr::select("CaptureID" = "ID", "CaptureDate",
                  "CaptureTime", "IndvID" = "Individual",
                  "CaptureLocation", "ReleaseLocation") %>%
    #Join in weight, tarsus and p3 from secondary capture data table.
    dplyr::left_join(utils::read.csv(paste0(dir, "/", "dbo_vw_MI_CaptureCaptureData", ".csv")) %>%
                       dplyr::select("CaptureID", "SpeciesID", "Observer",
                                     "Weight", "Tarsus",
                                     "P3_Length", "Age"),
                     by = "CaptureID") %>%
    #Join in Individual data so that we have an associated brood (used to determine chick age)
    dplyr::left_join(individuals %>%
                       dplyr::select("IndvID" = "ID", "BroodID"),
                     by = "IndvID") %>%
    #Now that we have joined species information, filter unwanted species out
    dplyr::filter(.data$SpeciesID %in% species_filter) %>%
    #Convert CaptureDate into date object and extract BreedingSeason information
    dplyr::mutate(CaptureDate = lubridate::as_date(.data$CaptureDate),
                  BreedingSeason = as.integer(lubridate::year(.data$CaptureDate)))

  #Create a summary for each individual with their ringing season (i.e. the min BreedingSeason)
  RingSeason_summary <- RawCaptures %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(RingSeason = min(.data$BreedingSeason, na.rm = TRUE))

  #Join this information back in so that we know the ringing season
  Captures_w_RingSeason <- RawCaptures %>%
    dplyr::left_join(RingSeason_summary,
                     by = "IndvID")

  #Now we can collect the data and continue in dplyr
  Capture_data <- Captures_w_RingSeason %>%
    dplyr::mutate(Age_observed = as.integer(.data$Age)) %>%
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason,
             showpb = TRUE) %>%
    #Include species letter codes for all species
    dplyr::ungroup() %>%
    dplyr::mutate(Species = dplyr::case_when(.data$SpeciesID == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .data$SpeciesID == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$SpeciesID == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .data$SpeciesID == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$SpeciesID == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$SpeciesID == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .data$SpeciesID == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             .data$SpeciesID == 11220 ~ species_codes[species_codes$SpeciesID == 11220, ]$Species),
                  #Add original tarsus method
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Alternative"),
                  ObserverID = as.character(.data$Observer)) %>%
    #Arrange by species, indv and date/time
    dplyr::arrange(.data$Species, .data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    #Include three letter population codes for both the capture and release location (some individuals may have been translocated e.g. cross-fostering)
    dplyr::left_join(location_data %>%
                       dplyr::select("CapturePlot" = "AreaID", "CaptureLocation" = "ID",
                                     "CapturePopID" = "PopID"),
                     by = "CaptureLocation") %>%
    dplyr::left_join(location_data %>%
                       dplyr::select("ReleasePlot" = "AreaID", "ReleaseLocation" = "ID",
                                     "ReleasePopID" = "PopID"),
                     by = "ReleaseLocation") %>%
    dplyr::filter(.data$CapturePopID %in% pop_filter) %>%
    #Make mass in g, and tarsus and wing length in mm
    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  LocationID = as.character(.data$CaptureLocation),
                  Mass = dplyr::na_if(.data$Weight/100, y = 0),
                  Tarsus = dplyr::na_if(.data$Tarsus/10, y = 0),
                  WingLength = dplyr::na_if(.data$P3_Length/10, y = 0))

  # Join in hatch date for each brood where an individual fledged
  # Do this later once we complete Individual_data
  Capture_data <- Capture_data %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate_observed"),
                     by = "BroodID") %>%
    #Determine difference between hatch and capture date for all individuals
    #that were ~before fledging (we'll say up until 30 days because this covers all possibilities)
    dplyr::mutate(diff = as.integer(lubridate::ymd(.data$CaptureDate) - .data$HatchDate_observed),
                  ChickAge = dplyr::case_when(!is.na(.data$diff) & dplyr::between(.data$diff, 0, 30) ~ .data$diff,
                                              TRUE ~ NA_integer_),
                  CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"),
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE, ##FIXME: Ask Marcel about dead captures
                  ExperimentID = NA_character_) ##FIXME: Ask Marcel about individual only experiments.

  return(Capture_data)

}

#' Create individual data table for NIOO pipeline.
#'
#' Create individual data table in standard format for data from NIOO.
#'
#' @param dir Path to directory containing the relevant table exports from the NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#' @param Capture_data Data frame. Capture data output from pipeline.
#'
#' @return A data frame.

create_individual_NIOO <- function(dir, Capture_data, location_data, species_filter, pop_filter){

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant).

  individuals <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Individual", ".csv")) %>%
    # Replace "\x85" (which is the Windows-1252 code point for the ellipsis '...') by three distinct points,
    # which arises when we use Jackcess (which uses the UTF-16LE encoding scheme) to export the Access tables to csv
    dplyr::mutate(dplyr::across(.cols = c("RingNumber", "IndividualNumber"),
                                .fns = ~{

                                  stringr::str_replace(.x, "\x85", "...")

                                })) %>%
    #Filter only required species
    #Remove individual records that are from tissue samples (i.e. ring number is TS)
    dplyr::filter(.data$SpeciesID %in% species_filter & stringr::str_detect(.data$RingNumber, "^TS.*", negate = TRUE)) %>%
    dplyr::filter(.data$RingNumber != "", !is.na(.data$RingNumber)) %>%
    #Translate Sexe into F or M
    ## FIXME: Which sex should this be grouped as?
    #Convert ring age to adult or chick
    #Create BroodIDFledged and Laid
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$Sexe %in% c(1, 3, 5) ~ "F",
                                                    .data$Sexe %in% c(2, 4, 6) ~ "M",
                                                    TRUE ~ NA_character_),
                  RingAge_category = dplyr::case_when(.data$RingAge %in% c(1, 2, 3) ~ "chick",
                                                      .data$RingAge > 3 ~ "adult",
                                                      TRUE ~ NA_character_),
                  BroodIDLaid = dplyr::if_else(is.na(.data$GeneticBroodID), .data$BroodID, .data$GeneticBroodID),
                  BroodIDFledged = dplyr::if_else(is.na(.data$BroodID), .data$GeneticBroodID, .data$BroodID),
                  IndvID = .data$ID)

  # Determine first captures (after removing eggs)
  first_captures <- utils::read.csv(paste0(dir, "/", "dbo_tbl_Capture", ".csv")) %>%
    dplyr::filter(.data$CaptureType == 1L | .data$CaptureType == 2L) %>%
    dplyr::group_by(.data$Individual) %>%
    # Add first capture dates for 9 individuals with ring number but no RingYear in the original individual data table
    dplyr::summarise(FirstCaptureLocation = dplyr::first(.data$CaptureLocation),
                     FirstCaptureDate = dplyr::first(.data$CaptureDate)) %>%
    dplyr::rename(IndvID = "Individual")

    # Join first capture location into individual data
  Individual_data <- individuals %>%
    dplyr::left_join(first_captures,
                     by = "IndvID") %>%
    # Relate the capturelocation to the three letter PopID
    dplyr::left_join(location_data %>%
                       dplyr::select("PopID", "FirstCaptureLocation" = "ID"),
                     by = "FirstCaptureLocation") %>%
    #Filter only chosen pops
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    #Convert numbers to species codes
    dplyr::mutate(Species = dplyr::case_when(.data$SpeciesID == 14400 ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .data$SpeciesID == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$SpeciesID == 13490 ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .data$SpeciesID == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$SpeciesID == 14790 ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$SpeciesID == 15980 ~ species_codes[species_codes$SpeciesID == 15980, ]$Species,
                                             .data$SpeciesID == 14610 ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             .data$SpeciesID == 11220 ~ species_codes[species_codes$SpeciesID == 11220, ]$Species),
                  RingAge = .data$RingAge_category,
                  # If RingYear is empty, use first capture date
                  RingSeason = dplyr::case_when(is.na(.data$RingYear) ~ as.integer(lubridate::year(.data$FirstCaptureDate)),
                                                TRUE ~ as.integer(.data$RingYear)),
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
#' @param dir Path to directory containing the relevant table exports from the NIOO Access database.
#' @param location_data Data frame with location codes and corresponding PopID.
#' @param species_filter Species six letter codes from the standard protocol.
#'   Used to filter the data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'   Used to filter the data.
#'
#' @return A data frame.

create_location_NIOO <- function(dir, location_data, species_filter, pop_filter){

  #Extract information on nestbox locations
  Location_data <- utils::read.csv(paste0(dir, "/", "dbo_tbl_NestboxAppearance", ".csv")) %>%
    #Join together information on the nestbox locations (e.g. latitude, longitude, nestbox name) and information on each nestbox that was there (e.g. how long before it was replaced).
    #This is necessary because one nestbox location could have multiple nestboxes erected at it over the study period.
    dplyr::right_join(location_data %>%
                        dplyr::select("Location" = "ID", "Latitude", "Longitude", "PopID"),
                      by = "Location") %>%
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    dplyr::select("LocationID" = "Location", "NestboxID" = "ID", "LocationType" = "NestBoxType",
                  "PopID", "Latitude", "Longitude",
                  "StartSeason" = "StartYear", "EndSeason" = "EndYear") %>%
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
