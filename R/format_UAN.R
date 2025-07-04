#'Construct standard format for data from the Univeristy of Antwerp.
#'
#'A pipeline to produce the standard format for 2 hole-nesting bird study
#'populations administered by the University of Antwerp.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{ClutchType_observed}: The raw data distinguishes second and third
#'nests and first, second, and third replacements. We group these all as 'second'
#'and 'replacement' respectively.
#'
#'\strong{ClutchSize_min, ClutchSize_max}: The raw data includes a column to determine whether
#'clutch size was counted with or without a brooding female. The presence of a
#'brooding female can effect the uncertainty in the count. After discussions
#'with the data owner clutch size counted with a brooding female is given an error
#'of +/- 2, so that ClutchSize_min = ClutchSize_observed - 2, and ClutchSize_max = ClutchSize_observed + 2.
#'
#'\strong{ExperimentID}: Experimental codes are provided in their original
#'format. These still need to be translated into our experimental groups.
#'
#'\strong{Tarsus}: Tarsus is measured using Svennson's Standard in early years
#'and Svennson's Alternative in later years. When Svennson's Alternative is
#'available this is used, otherwise we use converted Svensson's Standard, using
#'\code{\link[pipelines]{convert_tarsus}}.
#'
#'\strong{Age}:
#'For Age_observed: \itemize{
#'\item If a capture has a recorded
#'ChickAge or the Capture Type is listed as 'chick' it is given a EURING code 1:
#'nestling or chick, unable to fly freely, still able to be caught by hand.
#'\item Recorded value 1 (first calendar year) is given a EURING code 3:
#'full-grown bird hatched in the breeding season of this calendar year.
#'\item Recorded value 2 (second calendar year) is given a EURING code 5:
#'a bird hatched last calendar year and now in its second calendar year.
#'\item Recorded value 3 (>first calendar year) is given a EURING code 4:
#'full-grown bird hatched before this calendar year;
#'year of hatching otherwise unknown.
#'\item Recorded value 4 (>second calendar year) is given a EURING code 6:
#'full-grown bird hatched before last calendar year; year of hatching otherwise
#'unknown.
#'\item After discussing with data owners, recorded value 5 (full grown age unknown)
#'is given NA.
#'}
#'
#'For Age_calculated \itemize{
#'\item Any capture record with EURING <= 3 is considered to have a known age
#'(i.e. EURING codes 5, 7, 9, etc.). We consider identification in the nest
#'or as fledglings to be reliable indicators of year of hatching.
#'\item Any capture record with EURING >3 is considered to have an uncertain age
#'(i.e. EURING codes 4, 6, 8, etc.). We consider aging of adults to be too
#'uncertain.
#'}
#'
#'\strong{Sex_observed, Sex_calculated}: Any uncertainty in sex is ignored. For example, 'M?' is treated as male.
#'
#'\strong{ReleaseAlive}: Individuals who were captured alive are assumed to be released alive.
#'
#'\strong{Latitude and Longitude}: Location data is stored in Lambert72 CRS.
#'This has been converted to WGS84 to be standard with other systems.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_UAN <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force choose_directory() if used
  db <- force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  # Assign populations for filtering
  if(is.null(pop)){

    pop <- c("BOS", "PEE")

  }

  start_time <- Sys.time()

  message("\nLoading all files")

  BOX_info <- readxl::read_excel(paste0(db, "\\", "UAN_PrimaryData_BOX.xlsx"),
                                 col_types = c("text", "numeric", "numeric",
                                               "numeric", "numeric", "text",
                                               "text", "text", "text", "numeric",
                                               "numeric", "numeric"))

  BROOD_info <- readxl::read_excel(paste0(db, "\\", "UAN_PrimaryData_BR.xlsx"),
                                  col_types = c(rep("text", 6),
                                                rep("numeric", 6),
                                                rep("text", 5),
                                                "numeric", "text",
                                                rep("numeric", 4),
                                                rep("text", 7),
                                                "numeric", "numeric",
                                                "text", "numeric",
                                                rep("text", 5))) %>%
    dplyr::mutate(dplyr::across(.cols = c("LD", "WD"),
                                .fns = ~{

                                  janitor::excel_numeric_to_date(as.numeric(.x))

                                }))

  INDV_info <- readxl::read_excel(paste0(db, "\\", "UAN_PrimaryData_IND.xlsx"),
                                  col_types = c("text", "text", "text",
                                                "numeric", "text", "text",
                                                "text", "text", "text",
                                                "numeric", rep("text", 3),
                                                "list", "text", "list", "text",
                                                "list", "text", "list", "text",
                                                rep("numeric", 8), "text", "text")) %>%
    dplyr::mutate(dplyr::across(.cols = c("vd", "klr1date", "klr2date",
                                  "klr3date", "klr4date"),
                                .fns = ~{

                                  janitor::excel_numeric_to_date(as.numeric(.x))

                                }))

  ## TODO: Should PLOT_info be loaded and used?
  PLOT_info <- readxl::read_excel(paste0(db, "\\", "UAN_PrimaryData_PLOT.xlsx"),
                                  col_types = c(rep("text", 4),
                                                rep("numeric", 6),
                                                rep("text", 3)))

  CAPTURE_info <- readxl::read_excel(paste0(db, "\\", "UAN_PrimaryData_vG.xlsx"),
                                     col_types = c(rep("text", 11),
                                                   "numeric", "text",
                                                   rep("text", 3),
                                                   rep("numeric", 6),
                                                   rep("text", 6),
                                                   rep("numeric", 3),
                                                   "text", "numeric",
                                                   rep("text", 6))) %>%
    dplyr::mutate(VD = janitor::excel_numeric_to_date(as.numeric(.data$VD)))

  # Rename columns
  BROOD_info <- BROOD_info %>%
    dplyr::rename("BroodID" = "NN",
                  "Species" = "SOORT",
                  "Plot" = "GB",
                  "NestboxNumber" = "PL",
                  "LayDate_observed" = "LD",
                  "ClutchSizeError" = "JAE",
                  "ClutchSize_observed" = "AE",
                  "NrUnhatchedChicks" = "AEN",
                  "BroodSize_observed" = "NP",
                  "NrDeadChicks" = "PD",
                  "NumberFledged_observed" = "PU",
                  "LDInterruption" = "LO",
                  "ClutchType_observed" = "TY",
                  "MaleID" = "RM",
                  "FemaleID" = "RW",
                  "Unknown" = "AW",
                  "ChickWeighAge" = "WD",
                  "ChickWeighTime" = "WU",
                  "ObserverID" = "ME",
                  "NumberChicksMass" = "GN",
                  "AvgTarsus" = "GT",
                  "AvgChickMass" = "GG",
                  "AvgChickBodyCondition" = "CON",
                  "PopID" = "SA",
                  "NestNumberFirstBrood" = "NNN1",
                  "NestNumberSecondBrood" = "NNN2",
                  "NestNumberThirdBrood" = "NNN3",
                  "NestNumberBigamousMale" = "NNBI",
                  "NestNumberTrigamousMale" = "NNTRI",
                  "StageAbandoned" = "VERL",
                  "Longitude" = "coorx",
                  "Latitude" = "coory",
                  "Comments" = "comm",
                  "BreedingSeason" = "year",
                  "LocationID" = "gbpl",
                  # TODO: For now we ignore the "exp_" columns, but when we update UAN pipeline to v2.0, these become relevant
                  "ExperimentID" = "exp",
                  "ExperimentMan" = "exp_man",
                  "ExperimentDat" = "exp_dat",
                  "ExperimentObs" = "exp_obs") %>%
    dplyr::mutate(ClutchSize_observed = as.integer(.data$ClutchSize_observed),
                  BroodSize_observed = as.integer(.data$BroodSize_observed),
                  NumberFledged_observed = as.integer(.data$NumberFledged_observed),
                  NumberChicksMass = as.integer(.data$NumberChicksMass),
                  BreedingSeason = as.integer(.data$BreedingSeason))

  #Rename columns to make it easier to understand
  CAPTURE_info <- CAPTURE_info %>%
    dplyr::rename("Species" = "SOORT",
                  "IndvID" = "RN",
                  "MetalRingStatus" = "NRN",
                  "ColourRing" = "KLR",
                  "ColourRingStatus" = "NKLR",
                  "TagType" = "TAGTY",
                  "TagID" = "TAG",
                  "TagStatus" = "NTAG",
                  "BroodID" = "NN",
                  "Sex" = "GS",
                  "CaptureDate" = "VD",
                  "Age_observed" =  "LT",
                  "CapturePlot" = "GB",
                  "NestBoxNumber" = "PL",
                  "CaptureMethod" = "VW",
                  "ObserverID" = "ME",
                  "WingLength" = "VLL",
                  "Mass" = "GEW",
                  "CaptureTime" = "UUR",
                  "TarsusStandard" = "TA",
                  "BeakLength" = "BL",
                  "BeakHeight" = "BH",
                  "MoultStatus" = "DMVL",
                  "DNASample" = "BLOED",
                  "MoultScore" = "RUI",
                  "Comments" = "COMM",
                  "PrimaryKey" = "RECNUM",
                  "SplitRing" = "SPLIT",
                  "TarsusAlt" = "TANEW",
                  "Longitude" = "COORX",
                  "Latitude" = "COORY",
                  "CapturePopID" = "SA",
                  "Ticks" = "TEEK",
                  "OldColourRing" = "klr_old",
                  "LocationID" = "gbpl",
                  # TODO: For now we ignore the "exp" columns, but when we update UAN pipeline to v2.0, these become relevant
                  "ExperimentID" = "exp",
                  "ExperimentRem" = "exp_rem",
                  "ExperimentCap" = "exp_cap",
                  # TODO: This column refers to how the bird was found; released alive, hurt, dead, died during capture, etc.
                  # Codes might be shared with us in the future.
                  "Status" = "VRSTAT") %>%
    dplyr::mutate(Age_observed = as.integer(.data$Age_observed))

  INDV_info <- INDV_info %>%
    dplyr::rename("Species" = "soort",
                  "IndvID" = "rn",
                  "Sex" = "sex",
                  "BirthYear" = "gbj",
                  "BirthYearKnown" = "cgj",
                  "CaptureType" = "mode",
                  "PlotID" = "gb",
                  "FirstCaptureDate" = "vd",
                  "MetalRingStatus" = "nrn",
                  "TotalRecords" = "n",
                  "TagCode" = "pit",
                  "TagPlacementDate" = "pitdate",
                  "ColourRing1" = "klr1",
                  "ColourRing1PlacementDate" = "klr1date",
                  "ColourRing2" = "klr2",
                  "ColourRing2PlacementDate" = "klr2date",
                  "ColourRing3" = "klr3",
                  "ColourRing3PlacementDate" = "klr3date",
                  "ColourRing4" = "klr4",
                  "ColourRing4PlacementDate" = "klr4date",
                  "MolecularSex" = "molsex",
                  "MedianWingLength" = "vll_med",
                  "NrWingLengthObservations" = "vll_n",
                  "MedianTarsusStandard" = "cta_med",
                  "NrTarsusStandardObservations" = "cta_n",
                  "MedianTarsusAlt" = "ctanew_med",
                  "NrTarsusAltObservations" = "ctanew_n",
                  "MedianAllTarsus" = "tarsus_med",
                  "AllTarsusObservations" = "tarsus_n",
                  "TarsusMethod" = "tarsus_ty",
                  "PopID" = "SA")

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_UAN(BROOD_info, CAPTURE_info, species, pop)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_UAN(CAPTURE_info, species, pop)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_UAN(INDV_info, Capture_data, species)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_UAN(BOX_info)

  # WRANGLE DATA FOR EXPORT

  # We need to check that AvgChickMass and AvgTarsus are correct (i.e. it only uses chicks 14 - 16 days)
  avg_chick_data <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass_capture = mean(.data$Mass, na.rm = TRUE),
                     AvgTarsus_capture = mean(.data$Tarsus, na.rm = TRUE),
                     .groups = "drop")

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_chick_data, by = "BroodID") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(AvgChickMass = ifelse(!is.na(.data$AvgChickMass_capture), .data$AvgChickMass_capture, .data$AvgChickMass),
                  AvgTarsus = ifelse(!is.na(.data$AvgTarsus_capture), .data$AvgTarsus_capture, .data$AvgTarsus)) %>%
    dplyr::select(-"AvgChickMass_capture",
                  -"AvgTarsus_capture") %>%
    dplyr::select(names(brood_data_template)) %>%
    dplyr::ungroup()

  Capture_data <- Capture_data %>%
    dplyr::select(-"BroodID")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("\nAll tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("\nSaving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_UAN.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_UAN.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_UAN.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_UAN.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_UAN.txt"),
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

#' Create brood data table for data from University of Antwerp, Belgium.
#'
#' Create brood data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param BROOD_info Data frame. Primary brood data from University of Antwerp.
#' @param CAPTURE_info Data frame. Primary capture data from University of Antwerp.
#' @param species_filter 6 letter species codes for filtering data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'
#' @return A data frame.

create_brood_UAN <- function(BROOD_info, CAPTURE_info, species_filter, pop_filter){

  # For every brood in the capture data table, determine whether measurements were
  # taken with Svensson's standard or alternative
  Tarsus_method <- CAPTURE_info %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(TarsusAlt = length(stats::na.omit(.data$TarsusAlt)) > 0,
                     TarsusStd = length(stats::na.omit(.data$TarsusStandard)) > 0,
                     .groups = "drop") %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(.data$TarsusAlt == "TRUE" ~ "Alternative",
                                                          .data$TarsusAlt != "TRUE" & .data$TarsusStd == "TRUE" ~ "Standard",
                                                          .data$TarsusAlt != "TRUE" & .data$TarsusStd != "TRUE" ~ NA_character_)) %>%
    dplyr::select(-"TarsusAlt", -"TarsusStd")

  # Create a table with brood information
  Brood_data <- BROOD_info %>%
    # Convert columns to expected values
    dplyr::mutate(PopID = dplyr::case_when(.data$PopID == "FR" ~ "BOS",
                                           .data$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.data$Species == "pm" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == "pc" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species),
                  ClutchType_observed = dplyr::case_when(.data$ClutchType_observed %in% c(1, 9) ~ "first",
                                                         .data$ClutchType_observed %in% c(2, 6, 7) ~ "second",
                                                         .data$ClutchType_observed %in% c(3, 4, 5, 8) ~ "replacement"),
                  ClutchSizeError = dplyr::case_when(.data$ClutchSizeError == "J" ~ 0,
                                                     .data$ClutchSizeError == "N" ~ 2),
                  ClutchSize_min = .data$ClutchSize_observed - .data$ClutchSizeError,
                  ClutchSize_max = .data$ClutchSize_observed + .data$ClutchSizeError) %>%
    # Keep filtered species
    dplyr::filter(.data$Species %in% species_filter) %>%
    # Coerce BroodID to be character, convert LayDate
    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  LayDate_observed = lubridate::ymd(.data$LayDate_observed),
                  NumberChicksTarsus = .data$NumberChicksMass) %>%
    # Calculate clutchtype, assuming NAs are true unknowns
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE, protocol_version = "1.1")) %>%
    dplyr::left_join(Tarsus_method,
                     by = "BroodID") %>%
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    # Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(brood_data_template[1, !(names(brood_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(brood_data_template))

  return(Brood_data)

}

#' Create capture data table for data from University of Antwerp, Belgium.
#'
#' Create capture data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param CAPTURE_info Data frame. Primary capture data from University of Antwerp.
#' @param species_filter 6 letter species codes for filtering data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'
#' @return A data frame.

create_capture_UAN <- function(CAPTURE_info, species_filter, pop_filter){

  # Capture data includes all times an individual was captured (with measurements
  # like mass, tarsus etc.). This will include first capture as nestling (for
  # residents) This means there will be multiple records for a single individual.
  Capture_data <- CAPTURE_info %>%
    # Adjust species and PopID
    dplyr::mutate(CapturePopID = dplyr::case_when(.data$CapturePopID == "FR" ~ "BOS",
                                                  .data$CapturePopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.data$Species == "pm" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == "pc" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species)) %>%
    # Keep filtered species
    dplyr::filter(.data$Species %in% species_filter) %>%
    # Make tarsus length into standard method (Svensson Alt)
    # Firstly, convert the Svennson's standard measures to Svennson's Alt.
    # Then only use this converted measure when actual Svennson's Alt is unavailable.
    dplyr::mutate(TarsusStandard = convert_tarsus(.data$TarsusStandard, method = "Standard"),
                  # Add tarsus and original tarsus method
                  Tarsus = dplyr::case_when(!is.na(.data$TarsusAlt) ~ .data$TarsusAlt,
                                            is.na(.data$TarsusAlt) & !is.na(.data$TarsusStandard) ~ .data$TarsusStandard,
                                            TRUE ~ NA_real_),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$TarsusAlt) ~ "Alternative",
                                                          !is.na(.data$TarsusStandard) ~ "Standard",
                                                          TRUE ~ NA_character_)) %>%
    # Convert date/time
    dplyr::mutate(CaptureDate = lubridate::ymd(.data$CaptureDate),
                  BreedingSeason = as.integer(lubridate::year(.data$CaptureDate)),
                  CaptureTime = dplyr::na_if(paste(.data$CaptureTime %/% 1,
                                                   stringr::str_pad(string = round((.data$CaptureTime %% 1)*60),
                                                                    width = 2,
                                                                    pad = "0"), sep = ":"), "NA:NA"),
                  ReleasePopID = .data$CapturePopID,
                  ReleasePlot = .data$CapturePlot) %>%
    # TODO: Uncertainty in sex observations (M?, W?) is ignored
    dplyr::mutate(Sex_observed = dplyr::case_when(.data$Sex %in% c("M", "M?") ~ "M",
                                                  .data$Sex %in% c("W", "W?") ~ "F",
                                                  TRUE ~ NA_character_)) %>%
    # Calculate age at capture and chick age based on the LT column
    dplyr::mutate(Age_observed_new = dplyr::case_when((is.na(.data$Age_observed) | .data$Age_observed == 0) & .data$CaptureMethod %in% c("P", "PP") ~ 1L,
                                                      (is.na(.data$Age_observed) | .data$Age_observed == 0) & !(.data$CaptureMethod %in% c("P", "PP")) ~ NA_integer_,
                                                      .data$Age_observed > 5 ~ 1L,
                                                      .data$Age_observed == 1 ~ 3L,
                                                      .data$Age_observed == 2 ~ 5L,
                                                      .data$Age_observed == 3 ~ 4L,
                                                      .data$Age_observed == 4 ~ 6L,
                                                      TRUE ~ NA_integer_),
                  ChickAge = dplyr::case_when(.data$Age_observed > 5 ~ .data$Age_observed,
                                              TRUE ~ NA_integer_)) %>%
    # CaptureMethod "DG" is found dead. No information on status on release,
    # so individuals captured alive are assumed alive when released
    # TODO: Verify with Frank Adriaensen
    dplyr::mutate(CaptureAlive = dplyr::case_when(.data$CaptureMethod == "DG" ~ FALSE,
                                                  TRUE ~ TRUE),
                  ReleaseAlive = .data$CaptureAlive) %>%
    # Determine age at first capture for every individual
    dplyr::mutate(ischick = dplyr::case_when(.data$Age_observed_new <= 3 ~ 1L)) %>%
    calc_age(ID = .data$IndvID, Age = .data$ischick,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::filter(.data$CapturePopID %in% pop_filter) %>%
    # Replace Age_observed with Age_observed_new which has been converted to EURING codes
    dplyr::mutate(Age_observed = .data$Age_observed_new) %>%
    # Arrange by IndvID and CaptureDate and add unique CaptureID
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    # Arrange columns
    # Keep only necessary columns, and BroodID (to be used when creating the individual table)
    dplyr::select(tidyselect::contains(c(names(capture_data_template), "BroodID"))) %>%
    # Add missing columns
    dplyr::bind_cols(capture_data_template[1, !(names(capture_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(capture_data_template), "BroodID")

  return(Capture_data)

}

#' Create individual data table for data from University of Antwerp, Belgium.
#'
#' Create individual data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param INDV_info Data frame. Primary individual data from University of Antwerp.
#' @param Capture_data Output of \code{\link{create_capture_UAN}}.
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_individual_UAN <- function(INDV_info, Capture_data, species_filter){

  # Take capture data and determine summary data for each individual
  individuals <- Capture_data %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                                TRUE ~ dplyr::first(.data$Species)),
                     PopID = dplyr::first(.data$CapturePopID),
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = dplyr::case_when(dplyr::first(.data$Age_observed) == 1 ~ "chick",
                                                dplyr::first(.data$Age_observed) != 1 ~ "adult",
                                                is.na(dplyr::first(.data$Age_observed)) ~ "adult"),
                     BroodIDLaid = dplyr::case_when(RingAge == "chick" ~ dplyr::first(.data$BroodID),
                                                    RingAge == "adult" ~ NA_character_),
                     BroodIDFledged = .data$BroodIDLaid,
                     .groups = "drop") %>%
    dplyr::arrange(.data$RingSeason, .data$IndvID)

  # Retrieve sex information from primary data
  Indv_sex_primary <- INDV_info %>%
    dplyr::mutate(Sex = dplyr::case_when(.data$Sex %in% c(1, 3) ~ "M",
                                         .data$Sex %in% c(2, 4) ~ "F"),
                  Sex_genetic = dplyr::case_when(.data$MolecularSex == 1 ~ "M",
                                                 .data$MolecularSex == 2 ~ "F")) %>%
    dplyr::select("IndvID", "Sex", "Sex_genetic")

  # Retrieve sex information from capture data
  Indv_sex_capture <- Capture_data %>%
    dplyr::filter(!is.na(.data$Sex_observed)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$Sex_observed)),
                     unique_sex = list(unique(.data$Sex_observed))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                    TRUE ~ .data$unique_sex[[1]])) %>%
    dplyr::ungroup() %>%
    dplyr::select("IndvID", "Sex_calculated")

  # For now, we use sex as recorded in primary data (Sex; even if Sex_calculated gives another value)
  # For individuals without sex recorded in primary data, we use Sex_calculated
  # TODO: Verify with Frank Adriaensen
  Sex_calc <- Indv_sex_primary %>%
    dplyr::left_join(Indv_sex_capture,
                     by = "IndvID") %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(!is.na(.data$Sex) ~ .data$Sex,
                                                    is.na(.data$Sex) ~ .data$Sex_calculated)) %>%
    dplyr::select("IndvID", "Sex_calculated", "Sex_genetic")

  Indv_data <- individuals %>%
    dplyr::left_join(Sex_calc,
                     by = "IndvID") %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    # Keep only necessary columns
    dplyr::select(tidyselect::contains(names(individual_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(individual_data_template[1, !(names(individual_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(individual_data_template))

  return(Indv_data)

}

#' Create location data table for data from University of Antwerp, Belgium.
#'
#' Create location data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param BOX_info Data frame. Primary location data from University of Antwerp.
#'
#' @return A data frame.

create_location_UAN <- function(BOX_info){

  Location_data <- BOX_info %>%
    dplyr::mutate(LocationID = .data$GBPL,
                  LocationType = dplyr::case_when(.data$TYPE %in% c("pc", "pm", "cb", "se") ~ "NB",
                                                  is.na(.data$TYPE) ~ "NB",
                                                  .data$TYPE %in% c("PMO", "&", "FPT") ~ "MN"),
                  NestboxID = dplyr::case_when(.data$LocationType == "NB" ~ .data$LocationID,
                                               TRUE ~ NA_character_),
                  PopID = dplyr::case_when(.data$SA == "FR" ~ "BOS",
                                           .data$SA == "PB" ~ "PEE"),
                  Latitude = .data$Y_deg,
                  Longitude = .data$X_deg,
                  Latitude_Lambert = .data$Y,
                  Longitude_Lambert = .data$X,
                  StartSeason = as.integer(.data$YEARFIRST),
                  EndSeason = as.integer(.data$YEARLAST),
                  HabitatType = "deciduous",
                  HasCoords = as.factor(!is.na(.data$Latitude_Lambert))) %>%
    # Split into two groups whether they have coordinates or not
    split(f = .$HasCoords)

  # For the group without coordinates in degrees but with Lambert, we use Lambert coordinates instead.
  # Turn it into an sf object and change the CRS to be WGS84
  true_coords <- sf::st_as_sf(Location_data$'TRUE',
                              coords = c("Longitude_Lambert", "Latitude_Lambert"),
                              crs = 31370) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates()

  Location_data$'TRUE'$Longitude <- true_coords[, 1]
  Location_data$'TRUE'$Latitude <- true_coords[, 2]

  Location_data <- dplyr::bind_rows(Location_data) %>%
    # Keep only necessary columns
    dplyr::select(tidyselect::contains(names(location_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(location_data_template[1, !(names(location_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(location_data_template))

  return(Location_data)

}
