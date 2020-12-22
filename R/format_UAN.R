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
#'\code{\link[SPIbirds]{convert_tarsus}}.
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

  #Force choose_directory() if used
  db <- force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- species_codes$Species

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

  ## Rename columns
  BROOD_info <- dplyr::mutate(BROOD_info, BroodID = NN, Species = SOORT, Plot = GB, NestboxNumber = PL,
                              LayDate = LD, ClutchSizeError = JAE,
                              ClutchSize = as.integer(AE), NrUnhatchedChicks = AEN,
                              BroodSize = as.integer(NP), NrDeadChicks = PD,
                              NumberFledged = as.integer(PU), LDInterruption = LO,
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
                                Ticks = TEEK, ExperimentID = exp,
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

  Brood_data <- create_brood_UAN(BROOD_info, CAPTURE_info, species)

  # CAPTURE DATA

  message("\n Compiling capture information...")

  Capture_data <- create_capture_UAN(CAPTURE_info, species)

  # INDIVIDUAL DATA

  message("\n Compiling individual information...")

  Individual_data <- create_individual_UAN(INDV_info, CAPTURE_info, species)

  # LOCATION DATA

  message("\n Compiling location information...")

  Location_data <- create_location_UAN(BOX_info)

  #WRANGLE DATA FOR EXPORT

  #We need to check that AvgChickMass and AvgTarsus are correct (i.e. it only uses chicks 14 - 16 days)
  avg_chick_data <- Capture_data %>%
    dplyr::filter(between(ChickAge, 14, 16)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass_capture = mean(Mass, na.rm = TRUE), AvgTarsus_capture = mean(Tarsus, na.rm = TRUE))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_chick_data, by = "BroodID") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(AvgChickMass = ifelse(!is.na(AvgChickMass_capture), AvgChickMass_capture, AvgChickMass),
                  AvgTarsus = ifelse(!is.na(AvgTarsus_capture), AvgTarsus_capture, AvgTarsus)) %>%
    dplyr::select(-AvgChickMass_capture, -AvgTarsus_capture)

  Capture_data <- Capture_data %>%
    dplyr::select(-BroodID)

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
#'
#' @return A data frame.

create_brood_UAN <- function(data, CAPTURE_info, species_filter){

  #For every brood in the capture data table, determine whether measurements were
  #taken with Svensson's standard or alternative
  Tarsus_method <- CAPTURE_info %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(TarsusAlt = length(stats::na.omit(TarsusAlt)) > 0,
                     TarsusStd = length(stats::na.omit(TarsusStandard)) > 0) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(TarsusAlt == "TRUE" ~ "Alternative",
                                                          TarsusAlt != "TRUE" & TarsusStd == "TRUE" ~ "Standard",
                                                          TarsusAlt != "TRUE" & TarsusStd != "TRUE" ~ NA_character_)) %>%
    dplyr::select(-TarsusAlt, -TarsusStd)

  #Create a table with brood information.
  clutchtype <- progress::progress_bar$new(total = nrow(data))

  Brood_data <- data %>%
    #Convert columns to expected values
    dplyr::mutate(PopID = dplyr::case_when(.$PopID == "FR" ~ "BOS",
                                           .$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .$Species == "pc" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species),
                  ClutchType_observed = dplyr::case_when(.$ClutchType_observed %in% c(1, 9) ~ "first",
                                                         .$ClutchType_observed %in% c(2, 6, 7) ~ "second",
                                                         .$ClutchType_observed %in% c(3, 4, 5, 8) ~ "replacement"),
                  ClutchSizeError = dplyr::case_when(.$ClutchSizeError == "J" ~ 0,
                                                     .$ClutchSizeError == "N" ~ 2)) %>%
    #Remove only species chosen.
    dplyr::filter(Species %in% species_filter) %>%
    #Add NA columns and convert dates
    dplyr::mutate(LayDate = lubridate::ymd(LayDate),
                  LayDateError = NA_real_,
                  HatchDate = as.Date(NA), HatchDateError = NA_real_,
                  BroodSizeError = NA_real_,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_real_,
                  NumberFledgedError = NA_real_,
                  AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                  NumberChicksTarsus = NumberChicksMass) %>%
    #Calculate clutchtype, assuming NAs are true unknowns
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE)) %>%
    dplyr::left_join(Tarsus_method, by = "BroodID") %>%
    #Order columns
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot,
                  LocationID, FemaleID, MaleID, ClutchType_observed,
                  ClutchType_calculated,
                  LayDate, LayDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus,
                  OriginalTarsusMethod, ExperimentID) %>%
    #Coerce BroodID to be character
    dplyr::mutate(BroodID = as.character(BroodID))

  return(Brood_data)

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- NumberChicksMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL
  TarsusAlt <- TarsusStandard <- TarsusStd <- NULL


}

#' Create capture data table for data from University of Antwerp, Belgium.
#'
#' Create capture data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_capture_UAN <- function(data, species_filter){

  #Capture data includes all times an individual was captured (with measurements
  #like mass, tarsus etc.). This will include first capture as nestling (for
  #residents) This means there will be multiple records for a single individual.

  pb <- progress::progress_bar$new(total = nrow(data) * 2)

  Capture_data <- data %>%
    #Adjust species and PopID
    dplyr::mutate(CapturePopID = dplyr::case_when(.$CapturePopID == "FR" ~ "BOS",
                                                  .$CapturePopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .$Species == "pc" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species)) %>%
    #Filter by species
    dplyr::filter(Species %in% species_filter) %>%
    #Make tarsus length into standard method (Svensson Alt)
    #Firstly, convert the Svennson's standard measures to Svennson's Alt.
    #Then only use this converted measure when actual Svennson's Alt is unavailable.
    dplyr::mutate(TarsusStandard = convert_tarsus(TarsusStandard, method = "Standard")) %>%
    #Add tarsus and original tarsus method with bind_cols
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(SvenStd = .$TarsusStandard, SvenAlt = .$TarsusAlt),
                                     function(SvenStd, SvenAlt){

                                       pb$print()$tick()

                                       if(!is.na(SvenAlt)){

                                         return(tibble::tibble(Tarsus = SvenAlt, OriginalTarsusMethod = "Alternative"))

                                       } else if(!is.na(SvenStd)){

                                         return(tibble::tibble(Tarsus = SvenStd, OriginalTarsusMethod = "Standard"))

                                       } else {

                                         return(tibble::tibble(Tarsus = NA_real_, OriginalTarsusMethod = NA_character_))

                                       }})) %>%
    #Create NAs and convert date/time
    dplyr::mutate(CaptureDate = lubridate::ymd(CaptureDate),
                  BreedingSeason = as.integer(lubridate::year(CaptureDate)),
                  CaptureTime = na_if(paste(CaptureTime %/% 1,
                                            stringr::str_pad(string = round((CaptureTime %% 1)*60),
                                                             width = 2,
                                                             pad = "0"), sep = ":"), "NA:NA"),
                  ReleasePopID = CapturePopID, ReleasePlot = CapturePlot) %>%
    #Calculate age at capture and chick age based on the LT column
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(.$Age_observed, .$CaptureMethod),
                                     .f = ~{

                                       pb$print()$tick()

                                       # If Age (LT) was not recorded
                                       # instead estimate age from the capture type:
                                       if(is.na(..1) | ..1 == 0){

                                         #Capture type P and PP are chicks in the nest
                                         if(..2 %in% c("P", "PP")){

                                           return(tibble::tibble(Age_observed_new = 1, ChickAge = NA_integer_))

                                           #Captures in mist nets, observed rings, cage traps, roost checks
                                           #must be able to fly. But these can be anything from fledglings
                                           #in first calendar year +
                                           #Comment this out because it's really age calculated rather than observed
                                           # } else if (.y %in% c("FU", "GE", "MN", "ON", "NO", "SK", "SL", "LS")){
                                           #
                                           #   return(tibble::tibble(Age_observed = 2, ChickAge = NA))

                                         } else {

                                           #If no age and no chick capture type is given, then observed age is unknown.
                                           return(tibble::tibble(Age_observed_new = NA_integer_, ChickAge = NA_integer_))

                                         }

                                       #If age is > 5 this is the chick age in days
                                       } else if(..1 > 5){

                                         return(tibble::tibble(Age_observed_new = 1, ChickAge = ..1))

                                       } else {

                                         #If it's 1-5 then we translate into EURING codes for adults
                                         if(..1 %in% c(1, 2)){

                                           return(tibble::tibble(Age_observed_new = 1 + ..1*2, ChickAge = NA_integer_))

                                         } else if (..1 %in% c(3, 4)) {

                                           return(tibble::tibble(Age_observed_new = 4 + (..1 - 3)*2, ChickAge = NA_integer_))

                                         } else {

                                           return(tibble::tibble(Age_observed_new = NA, ChickAge = NA_integer_))

                                         }

                                       }

                                     })) %>%
    #Determine age at first capture for every individual
    dplyr::mutate(ischick = dplyr::case_when(.$Age_observed_new <= 3 ~ 1L)) %>%
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    #Arrange columns
    #Replace Age_observed with Age_observed_new which has been converted to EURING codes
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime,
                  ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed = Age_observed_new, Age_calculated, ChickAge, BroodID)

  return(Capture_data)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  TarsusStandard <- `.` <- ischick <- NULL

}

#' Create individual data table for data from University of Antwerp, Belgium.
#'
#' Create individual data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param CAPTURE_info Capture data table from the raw data
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_individual_UAN <- function(data, CAPTURE_info, species_filter){

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  #To determine which brood an individual is from, we subset the CAPTURE_info table
  #and include only those records where an individual was caught as a nestling (i.e. Age_observed > 5 where age is in days).
  #We then take the nest that this nestling was in when it was caught.
  Indv_broods <- CAPTURE_info %>%
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::filter(!is.na(BroodID) & (Age_observed > 5 | Age_observed == 1)) %>%
    dplyr::summarise(BroodIDLaid = as.character(first(BroodID)))

  #Do this for PopID, capture age and first year as well
  Indv_Pop <- CAPTURE_info %>%
    dplyr::filter(!is.na(CapturePopID)) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::arrange(CaptureDate, .by_group = TRUE) %>%
    dplyr::summarise(PopID = first(CapturePopID),
                     FirstAge = first(Age_observed),
                     FirstYear = as.integer(min(lubridate::year(CaptureDate))))

  #There were no translocations, so BroodIDLaid/Fledged are the same
  Indv_data <- data %>%
    dplyr::left_join(Indv_broods, by = "IndvID") %>%
    dplyr::left_join(Indv_Pop, by = "IndvID") %>%
    #Add and filter by species
    dplyr::mutate(PopID = dplyr::case_when(.$PopID == "FR" ~ "BOS",
                                           .$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .$Species == "pc" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species),
                  Sex = dplyr::case_when(.$Sex %in% c(1, 3) ~ "M",
                                         .$Sex %in% c(2, 4) ~ "F")) %>%
    dplyr::filter(!is.na(Species) & Species %in% species_filter) %>%
    dplyr::mutate(BroodIDFledged = BroodIDLaid,
                  RingSeason = FirstYear,
                  RingAge = dplyr::case_when(.$FirstAge > 5 | .$FirstAge == 1 ~ "chick",
                                             is.na(.$FirstAge) | .$FirstAge <= 5 ~ "adult")) %>%
    select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex)

  return(Indv_data)

}

#' Create location data table for data from University of Antwerp, Belgium.
#'
#' Create location data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#'
#' @return A data frame.

create_location_UAN <- function(data){

  Location_data <- data %>%
    dplyr::mutate(LocationID = GBPL, LocationType = dplyr::case_when(TYPE %in% c("pc", "pm", "cb") | is.na(TYPE) ~ "NB",
                                                                        TYPE == "FPT" ~ "FD",
                                                                        TYPE %in% c("PMO", "&") ~ "MN")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(NestboxID = ifelse(LocationType == "NB", LocationID, NA_character_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PopID = dplyr::case_when(.$SA == "FR" ~ "BOS",
                                           .$SA == "PB" ~ "PEE"),
                  Latitude = Y_deg, Longitude = X_deg,
                  Latitude_Lambert = Y, Longitude_Lambert = X,
                  StartSeason = as.integer(YEARFIRST), EndSeason = as.integer(YEARLAST),
                  Habitat = "deciduous",
                  HasCoords = as.factor(!is.na(Latitude_Lambert))) %>%
    #Split into two groups whether they have coordinates or not
    split(f = .$HasCoords)

  #For the group without coordinates in degrees but with Lambert, we use Lambert coordinates instead.
  #Turn it into an sf object and change the CRS to be WGS84
  true_coords <- sf::st_as_sf(Location_data$'TRUE',
                              coords = c("Longitude_Lambert", "Latitude_Lambert"),
                              crs = 31370) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates()

  Location_data$'TRUE'$Longitude <- true_coords[, 1]
  Location_data$'TRUE'$Latitude <- true_coords[, 2]

  Location_data <- dplyr::bind_rows(Location_data) %>%
    dplyr::select(-HasCoords, -Latitude_Lambert, -Longitude_Lambert)

  return(Location_data)

}
