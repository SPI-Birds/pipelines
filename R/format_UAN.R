#'Construct standard format for data from the Univeristy of Antwerp.
#'
#'A pipeline to produce the standard format for 2 hole-nesting bird study
#'populations administered by the University of Antwerp.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{ClutchType_observed}: The raw data distinguishes second and third
#'nests and first, second, and third replacements. We group these all as 'second'
#'and 'replacement' respectively.
#'
#'\strong{ClutchSizeError}: The raw data includes a column to determine whether
#'clutch size was counted with or without a brooding female. The presence of a
#'brooding female can effect the uncertainty in the count. After discussions
#'with the data owner clutch size counted with a brooding female is given an error
#'of 2.
#'
#'\strong{ExperimentID}: Experimental codes are provided in their original
#'format. These still need to be translated into our experimental groups.
#'
#'\strong{Tarsus}: Tarsus is measured using Svennson's Standard in early years
#'and Svennson's Alternative in later years. When Svennson's Alternative is
#'available this is used, otherwise we use converted Svensson's Standard, using
#'\code{\link[pipelines]{convert_tarsus}}.
#'
#'\strong{Age}: For Age_observed: \itemize{
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
#'
#'For Age_calculated \itemize{
#'\item Any capture record with EURING <= 3 is considered to have a known age
#'(i.e. EURING codes 5, 7, 9, etc.). We consider identification in the nest
#'or as fledglings to be reliable indicators of year of hatching.
#'\item Any capture record with EURING >3 is considered to have an uncertain age
#'(i.e. EURING codes 4, 6, 8, etc.). We consider aging of adults to be too
#'uncertain.
#'}
#'}
#'
#'\strong{Latitude and Longitude}: Location data is stored in Lambert72 CRS.
#'This has been converted to WGS84 to be standard with other systems.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_UAN <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

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

  message("\n Loading all files")

  all_files <- list.files(path = db, pattern = ".xlsx", full.names = TRUE)

  BOX_info <- readxl::read_excel(all_files[grepl("BOX", all_files)],
                                 col_types = c("text", "numeric", "numeric",
                                               "numeric", "numeric", "text",
                                               "text", "text", "text", "numeric",
                                               "numeric"))

  BROOD_info <- readxl::read_excel(all_files[grepl("BR", all_files)],
                                   col_types = c("text", "text", "text",
                                                 "text", "text", "text",
                                                 "numeric", "numeric",
                                                 "numeric", "numeric",
                                                 "numeric", "numeric",
                                                 "text", "text", "text",
                                                 "text", "text", "numeric", "text",
                                                 "numeric", "numeric", "numeric",
                                                 "numeric", "text", "text", "text",
                                                 "text", "text", "text", "text", "text",
                                                 "numeric", "numeric", "text", "numeric",
                                                 "text")) %>%
    dplyr::mutate_at(.vars = vars(LD, WD), function(x){

      janitor::excel_numeric_to_date(as.numeric(x))

    })

  INDV_info <- readxl::read_excel(all_files[grepl("IND", all_files)],
                                  col_types = c("text", "text", "text",
                                                "numeric", "text", "text",
                                                "text", "text", "text",
                                                "numeric", rep("text", 3),
                                                "list", "text", "list", "text",
                                                "list", "text", "list", "text",
                                                rep("numeric", 8), "text")) %>%
    dplyr::mutate_at(.vars = vars(vd, klr1date, klr2date, klr3date, klr4date), function(x){

      janitor::excel_numeric_to_date(as.numeric(x))

    })

  PLOT_info <- readxl::read_excel(all_files[grepl("PL", all_files)],
                                  col_types = c(rep("text", 4),
                                                rep("numeric", 6),
                                                rep("text", 3)))

  CAPTURE_info <- readxl::read_excel(all_files[grepl("VG", all_files)],
                                     col_types = c(rep("text", 11),
                                                   "numeric", "text",
                                                   rep("text", 3),
                                                   rep("numeric", 6),
                                                   rep("text", 6),
                                                   rep("numeric", 3),
                                                   "text", "numeric",
                                                   rep("text", 3))) %>%
    dplyr::mutate(VD = janitor::excel_numeric_to_date(as.numeric(VD)))

  # Rename columns
  BROOD_info <- dplyr::mutate(BROOD_info, BroodID = NN, Species = SOORT, Plot = GB, NestboxNumber = PL,
                              LayDate_observed = LD, ClutchSizeError = JAE,
                              ClutchSize_observed = as.integer(AE), NrUnhatchedChicks = AEN,
                              BroodSize_observed = as.integer(NP), NrDeadChicks = PD,
                              NumberFledged_observed = as.integer(PU), LDInterruption = LO,
                              ClutchType_observed = TY, MaleID = RM, FemaleID = RW,
                              Unknown = AW, ChickWeighAge = WD, ChickWeighTime = WU,
                              ObserverID = ME, NumberChicksMass = as.integer(GN),
                              AvgTarsus = GT, AvgChickMass = GG,
                              AvgChickBodyCondition = CON, PopID = SA,
                              NestNumberFirstBrood = NNN1,
                              NestNumberSecondBrood = NNN2,
                              NestNumberThirdBrood = NNN3,
                              NestNumberBigamousMale = NNBI,
                              NestNumberTrigamousMale = NNTRI,
                              StageAbandoned = VERL,
                              # TODO: Ask Frank Adriaensen what the experiment codes mean
                              ExperimentID = exp,
                              Longitude = coorx, Latitude = coory,
                              Comments = comm, BreedingSeason = as.integer(year),
                              LocationID = gbpl)

  #Rename columns to make it easier to understand
  CAPTURE_info <- dplyr::mutate(CAPTURE_info, Species = SOORT,
                                IndvID = RN, MetalRingStatus = NRN,
                                ColourRing = KLR, ColourRingStatus = NKLR,
                                TagType = TAGTY, TagID = TAG,
                                TagStatus = NTAG, BroodID = NN,
                                Sex = GS, CaptureDate = VD,
                                Age_observed = as.integer(LT),
                                CapturePlot = GB, NestBoxNumber = PL,
                                CaptureMethod = VW,
                                ObserverID = ME, WingLength = VLL,
                                Mass = GEW, CaptureTime = UUR,
                                TarsusStandard = TA,
                                BeakLength = BL, BeakHeight = BH,
                                MoultStatus = DMVL,
                                DNASample = BLOED,
                                MoultScore = RUI, Comments = COMM,
                                PrimaryKey = RECNUM, SplitRing = SPLIT,
                                TarsusAlt = TANEW, Longitude = COORX,
                                Latitude = COORY, CapturePopID = SA,
                                Ticks = TEEK,
                                # TODO: Ask Frank Adriaensen what the experiment codes mean
                                ExperimentID = exp,
                                OldColourRing = klr_old,
                                LocationID = gbpl)

  INDV_info <- dplyr::mutate(INDV_info, Species = soort,
                             IndvID = rn, Sex = sex,
                             BirthYear = gbj, BirthYearKnown = cgj,
                             CaptureType = mode, PlotID = gb,
                             FirstCaptureDate = vd,
                             MetalRingStatus = nrn,
                             TotalRecords = n,
                             TagCode = pit, TagPlacementDate = pitdate,
                             ColourRing1 = klr1, ColourRing1PlacementDate = klr1date,
                             ColourRing2 = klr2, ColourRing2PlacementDate = klr2date,
                             ColourRing3 = klr3, ColourRing3PlacementDate = klr3date,
                             ColourRing4 = klr4, ColourRing4PlacementDate = klr4date,
                             MolecularSex = molsex, MedianWingLength = vll_med,
                             NrWingLengthObservations = vll_n,
                             MedianTarsusStandard = cta_med,
                             NrTarsusStandardObservations = cta_n,
                             MedianTarsusAlt = ctanew_med,
                             NrTarsusAltObservations = ctanew_n,
                             MedianAllTarsus = tarsus_med,
                             AllTarsusObservations = tarsus_n,
                             TarsusMethod = tarsus_ty)

  # BROOD DATA

  message("\n Compiling brood information...")

  Brood_data <- create_brood_UAN(BROOD_info, CAPTURE_info, species, pop)

  # CAPTURE DATA

  message("\n Compiling capture information...")

  Capture_data <- create_capture_UAN(CAPTURE_info, species, pop)

  # INDIVIDUAL DATA

  message("\n Compiling individual information...")

  Individual_data <- create_individual_UAN(INDV_info, Capture_data, species)

  # LOCATION DATA

  message("\n Compiling location information...")

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
    dplyr::select(-.data$AvgChickMass_capture, -.data$AvgTarsus_capture) %>%
    dplyr::select(names(brood_data_template)) %>%
    dplyr::ungroup()

  Capture_data <- Capture_data %>%
    dplyr::select(-.data$BroodID)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("\n Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_UAN.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_UAN.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_UAN.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_UAN.csv"), row.names = F)

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

#' Create brood data table for data from University of Antwerp, Belgium.
#'
#' Create brood data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param CAPTURE_info Capture data table from the raw data
#' @param species_filter 6 letter species codes for filtering data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'
#' @return A data frame.

create_brood_UAN <- function(data, CAPTURE_info, species_filter, pop_filter){

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
    dplyr::select(-.data$TarsusAlt, -.data$TarsusStd)

  # Create a table with brood information
  Brood_data <- data %>%
    # Convert columns to expected values
    dplyr::mutate(PopID = dplyr::case_when(.data$PopID == "FR" ~ "BOS",
                                           .data$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.data$Species == "pm" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .data$Species == "pc" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species),
                  ClutchType_observed = dplyr::case_when(.data$ClutchType_observed %in% c(1, 9) ~ "first",
                                                         .data$ClutchType_observed %in% c(2, 6, 7) ~ "second",
                                                         .data$ClutchType_observed %in% c(3, 4, 5, 8) ~ "replacement"),
                  # TODO: Verify with Frank Adriaensen whether this is a correct interpretation of their error column
                  ClutchSizeError = dplyr::case_when(.data$ClutchSizeError == "J" ~ 0,
                                                     .data$ClutchSizeError == "N" ~ 2),
                  ClutchSize_min = .data$ClutchSize_observed - .data$ClutchSizeError,
                  ClutchSize_max = .data$ClutchSize_observed + .data$ClutchSizeError) %>%
    # Keep filtered species
    dplyr::filter(Species %in% species_filter) %>%
    # Coerce BroodID to be character, convert LayDate
    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  LayDate_observed = lubridate::ymd(.data$LayDate_observed),
                  NumberChicksTarsus = .data$NumberChicksMass) %>%
    # Calculate clutchtype, assuming NAs are true unknowns
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE, protocol_version = "1.1")) %>%
    dplyr::left_join(Tarsus_method, by = "BroodID") %>%
    dplyr::filter(PopID %in% pop_filter) %>%
    # Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(brood_data_template[, !(names(brood_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(brood_data_template))

  return(Brood_data)

}

#' Create capture data table for data from University of Antwerp, Belgium.
#'
#' Create capture data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param species_filter 6 letter species codes for filtering data.
#' @param pop_filter Population three letter codes from the standard protocol.
#'
#' @return A data frame.

create_capture_UAN <- function(data, species_filter, pop_filter){

  # Capture data includes all times an individual was captured (with measurements
  # like mass, tarsus etc.). This will include first capture as nestling (for
  # residents) This means there will be multiple records for a single individual.
  Capture_data <- data %>%
    # Adjust species and PopID
    dplyr::mutate(CapturePopID = dplyr::case_when(.data$CapturePopID == "FR" ~ "BOS",
                                                  .data$CapturePopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.data$Species == "pm" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .data$Species == "pc" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species)) %>%
    # Keep filtered species
    dplyr::filter(Species %in% species_filter) %>%
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
                  CaptureTime = na_if(paste(.data$CaptureTime %/% 1,
                                            stringr::str_pad(string = round((.data$CaptureTime %% 1)*60),
                                                             width = 2,
                                                             pad = "0"), sep = ":"), "NA:NA"),
                  ReleasePopID = .data$CapturePopID, ReleasePlot = .data$CapturePlot) %>%
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
    # Determine age at first capture for every individual
    dplyr::mutate(ischick = dplyr::case_when(.data$Age_observed_new <= 3 ~ 1L)) %>%
    calc_age(ID = .data$IndvID, Age = .data$ischick, Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::filter(CapturePopID %in% pop_filter) %>%
    # Arrange columns
    # Replace Age_observed with Age_observed_new which has been converted to EURING codes
    dplyr::mutate(Age_observed = .data$Age_observed_new) %>%
    # Keep only necessary columns, and BroodID (to be used when creating the individual table)
    dplyr::select(dplyr::contains(c(names(capture_data_template), "BroodID"))) %>%
    # Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(capture_data_template), "BroodID")

  return(Capture_data)

}

#' Create individual data table for data from University of Antwerp, Belgium.
#'
#' Create individual data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param Capture_data Output of \code{\link{create_capture_UAN}}.
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_individual_UAN <- function(data, Capture_data, species_filter){

  # Take capture data and determine summary data for each individual
  Indv_info <- Capture_data %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                                TRUE ~ dplyr::first(.data$Species)),
                     PopID = dplyr::first(.data$CapturePopID),
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = dplyr::case_when(dplyr::first(.data$Age_observed) == 1 ~ "chick",
                                                dplyr::first(.data$Age_observed) != 1 ~ "adult",
                                                is.na(dplyr::first(.data$Age_observed)) ~ "adult"),
                     BroodIDLaid = dplyr::case_when(RingAge == "chick" ~ first(.data$BroodID),
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
    dplyr::select(.data$IndvID, .data$Sex, .data$Sex_genetic)

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
    dplyr::select(.data$IndvID, .data$Sex_calculated)

  # For now, we use sex as recorded in primary data (Sex; even if Sex_calculated gives another value)
  # For individuals without sex recorded in primary data, we use Sex_calculated
  # TODO: Verify with Frank Adriaensen
  Sex_calc <- Indv_sex_primary %>%
    dplyr::left_join(Indv_sex_capture, by = "IndvID") %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(!is.na(.data$Sex) ~ .data$Sex,
                                                    is.na(.data$Sex) ~ .data$Sex_calculated)) %>%
    dplyr::select(.data$IndvID, .data$Sex_calculated, .data$Sex_genetic)

  Indv_data <- Indv_info %>%
    dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::filter(Species %in% species_filter) %>%
    # Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(individual_data_template[,!(names(individual_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(individual_data_template))

  return(Indv_data)

}

#' Create location data table for data from University of Antwerp, Belgium.
#'
#' Create location data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param BOX_info Data frame. Primary data from University of Antwerp.
#'
#' @return A data frame.

create_location_UAN <- function(BOX_info){

  Location_data <- BOX_info %>%
    dplyr::mutate(LocationID = .data$GBPL,
                  LocationType = dplyr::case_when(.data$TYPE %in% c("pc", "pm", "cb") | is.na(.data$TYPE) ~ "NB",
                                                  .data$TYPE == "FPT" ~ "FD",
                                                  .data$TYPE %in% c("PMO", "&") ~ "MN")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(NestboxID = ifelse(.data$LocationType == "NB", .data$LocationID, NA_character_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PopID = dplyr::case_when(.$SA == "FR" ~ "BOS",
                                           .$SA == "PB" ~ "PEE"),
                  Latitude = .data$Y_deg,
                  Longitude = .data$X_deg,
                  Latitude_Lambert = .data$Y,
                  Longitude_Lambert = .data$X,
                  StartSeason = as.integer(.data$YEARFIRST), EndSeason = as.integer(.data$YEARLAST),
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
    dplyr::select(dplyr::contains(names(location_data_template))) %>%
    # Add missing columns
    dplyr::bind_cols(location_data_template[,!(names(location_data_template) %in% names(.))]) %>%
    # Reorder columns
    dplyr::select(names(location_data_template))

  return(Location_data)

}
