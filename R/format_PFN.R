#' Construct standard format for PiedFlyNet data
#'
#'A pipeline to produce the standard format for the hole-nesting bird populations
#' monitored/administered by PiedFlyNet.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: By default, pipeline outputs will include great tit \emph{Parus
#'major}; blue tit \emph{Cyanistes caeruleus}; pied flycatcher \emph{Ficedula
#'hypoleuca}; Eurasian nuthatch \emph{Sitta europaea}; coal tit \emph{Periparus
#'ater}; marsh tit \emph{Poecile palustris}, and common redstart \emph{Phoenicurus phoenicurus}.
#'Other minority species are excluded.
#'
#'\strong{Primary data sources}: Brood data is extracted from data in a
#'brood table/nest monitoring format ("Nest data"). Capture data is extracted from both
#'Nest data and Malcolm Burgess' version of the BTO ringing database for birds marked within
#'the PiedFlyNet ("IPMR data"). The format of this ringing data is IPMR, the 'old' BTO ringing database format, and Malcolm
#'has his own pipeline to convert the new BTO ringing database format to the old one.
#'Nest data is available for all populations, IPMR data is not.
#'Some information on captures is present in Nest and IPMR data, while some information is
#'exclusive to either data source. See the documentation of `create_capture_PFN()` for
#'a description of how data from Nest and IPMR data is consolidated.
#'
#'\strong{Unidentified individuals}: Some individuals have not been given unique IDs. The pipleline
#'recognizes these as improper IDs as they do not follow the standard format of ring
#'numbers for this data (defined as a sequences of 1-9 letters followed by a sequence of 1-9 integer numbers).
#'In the Brood data, we assume unknown identity for individuals with improper IDs and treat ID as NA.
#'From Capture data and Individual data, these individuals are omitted entirely.
#'
#'\strong{Re-ringing events}: Some individuals carry multiple rings (up to 3 in this data) throughout
#'their lives, and re-ringing events are recorded in IPMR data. To correctly link data to
#'individuals (not just ring numbers), the pipeline assigns a unique individual identifier defined as
#'the number of the first ring ever given to this individual.
#'
#'\strong{Resightings}: The IPMR primary data contains some resightings of birds. At present, these
#' records are excluded from the outputs because the standard format for capture data cannot distinguish
#' between recaptures and resightings in its current state. Functionality do do this (i.e. via a "capture type" variable)
#' may be added to a future update of the standard format.
#'
#'\strong{Age calculation}: The age of each bird is estimated based on its history of ringing and recaptures.
#'Since some birds that breed in one of the focal populations may have been ringed in another study sites (= immigrants), the
#'pipeline calculates age for birds in any one population based on capture records in all monitored populations,
#'and not just ringing/captures in locations within the specific study site.
#'
#'\strong{LayDate_observed and HatchDate_observed}: Information is provided as date of first egg (DFE) and
#'date of hatching (DH). These are given as integer number, and represent days after a set
#'starting date (day 0). Day 0 is defined as 31. March.
#'
#'\strong{ChickAge}: For every capture, we estimate the age of a chick as the difference between the hatch date
#'taken from `BroodIDFledged` (in `Individual_data`) and the `CaptureDate`.
#'
#'#'\strong{Location information}: This is generated for all nestboxes (from Nest data)
#'and all additional capture locations (IPMR data). For East Dartmoor, detailed
#'information on location (`Latitude`, `Longitude`) and the period a nestbox has
#'been deployed (´StartSeason`, `EndSeason`) is available and incorporated. For
#'All other populations, this data is not available at present. Locations for other
#'populations therefore have no coordinate information, and `StartSeason` and
#'`EndSeason` are calculated as the first and last year in which birds were
#'observed to be breeding within a box.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#' @export
#'


format_PFN <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #-------#
  # SETUP #
  #-------#

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force user to select directory
  force(db)

  # Record start time to estimate processing time.
  start_time <- Sys.time()

  #----------------------------#
  # NEST PRIMARY DATA ASSEMBLY #
  #----------------------------#

  # Load Nest data files for all populations

  ## East Dartmoor (EDM)
  Nest_EDM <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_EDM.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'EDM')

  ## Nags Head (NAG)
  Nest_NAG <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_NAG.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'NAG')

  ## Loch Katrine (KAT)
  Nest_KAT <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_KAT.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'KAT')

  ## Dinas (DIN)
  Nest_DIN <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_DIN.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'DIN',
                  # Standardize naming convention
                  Box = stringr::str_replace(string = .data$Box, pattern = 'DV', replacement = 'DOV'),
                  Box = stringr::str_replace(string = .data$Box, pattern = 'TN', replacement = 'TYN'))

  ## Teign (TEI)
  Nest_TEI <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_TEI.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'TEI')

  ## Okehampton (OKE)
  Nest_OKE <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_OKE.csv"),
                              na.strings = c("", "?"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'OKE')

  ## North Wales (NWA)
  # Nest_NWA <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Nest_NWA.csv"), na.strings = c("", "?"), colClasses = "character") %>%
  #   dplyr::mutate(PopID = 'NWA')
  # NOTE: Data from North Wales (NWA) may be added to SPI-Birds in the future

  # Combine all Nest data files together
  Nest_data <- dplyr::bind_rows(Nest_EDM,
                                Nest_NAG,
                                Nest_KAT,
                                Nest_DIN,
                                Nest_TEI,
                                Nest_OKE#,
                                #Nest_NWA
  )

  # Determine species codes & pop codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  if(is.null(pop)){

    pop <- c("EDM", "NAG", "KAT", "DIN", "TEI", "OKE")

  }

  #----------------------------#
  # IPMR PRIMARY DATA ASSEMBLY #
  #----------------------------#

  # Load Nest data files for all populations

  ## PiedFlyNet collection (EDM, TEI, OKE)
  IPMR_PFN <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_IPMR_PFN.csv"),
                              na.strings = c("", "?", "UNK", "-"), colClasses = "character")

  ## Scotland (KAT)
  IPMR_KAT <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_IPMR_KAT.csv"),
                              na.strings = c("", "?", "UNK", "-"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'KAT',
                  PLACE = dplyr::case_when(.data$PLACE == "HRA" ~ "BR.W",
                                           .data$PLACE %in% c("HRB", "HRO") ~ "BR.E",
                                           .data$PLACE == "HRC" ~ "STR",
                                           .data$PLACE == "HRE" ~ "TOP",
                                           .data$PLACE == "HRF" ~ "MID",
                                           .data$PLACE == "HRG" ~ "TOP",
                                           .data$PLACE == "HRJ" ~ "Rh",
                                           .data$PLACE == "HRK" ~ "SF",
                                           .data$PLACE == "NN6002" ~ "CR",
                                           .data$PLACE == "NS5997" ~ "PoG",
                                           TRUE ~ .data$PLACE),
                  SITE = NA_character_) %>%
    dplyr::rename("SPEC" = "SPNAME",
                  "SCHEME" = "COUNTRY")
  # NOTE: The KAT IPMR data has some different column names and formats than the
  #      other IPMR input data file, so these need to be adjusted here.

  ## Dinas (DIN)
  IPMR_DIN <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_IPMR_DIN.csv"),
                              na.strings = c("", "?", "UNK", "-"), colClasses = "character") %>%
    dplyr::mutate(PopID = 'DIN',
                  # Drop everything after the first ","
                  SITE_DIN = stringr::str_split(string = .data$USERV1, pattern = c(','), simplify = T)[,1],
                  # Drop everything after the first ";"
                  SITE_DIN = stringr::str_split(string = .data$SITE_DIN, pattern = c(';'), simplify = T)[,1],
                  SITE_DIN = stringr::str_remove_all(string = .data$SITE_DIN, pattern = ' '), # Remove all spaces
                  SITE_DIN = stringr::str_remove(string = .data$SITE_DIN, pattern = 'Box'), # Remove "Box"
                  # Replace "DINAS" with "DIN"
                  SITE_DIN = stringr::str_replace(string = .data$SITE_DIN,
                                                  pattern = 'DINAS', replacement = 'DIN'),
                  # Set all to NA that contain lower case letters (mostly part of comments)
                  SITE_DIN = dplyr::case_when(stringr::str_detect(.data$SITE_DIN, "[[:lower:]]") ~ NA_character_,
                                              TRUE ~ .data$SITE_DIN),
                  # Standardize naming convention
                  SITE_DIN = stringr::str_replace(string = .data$SITE_DIN, pattern = 'DV', replacement = 'DOV'),
                  SITE_DIN = stringr::str_replace(string = .data$SITE_DIN, pattern = 'TN', replacement = 'TYN'),
                  RING2 = dplyr::case_when(grepl("rering", .data$USERV1) ~ stringr::str_extract(string = .data$USERV1, pattern = stringr::regex("[A-Z]{1,}[0-9]{1,}")),
                                                TRUE ~ .data$RING2)
                  )

  # NOTE 1: Unlike other IPMR data, DIN IPMR data contains information on
  # capture location (NestboxID) not in the SITE column, but as part of comments
  # in the USERV1 column. Therefore, I here extract as much of this information
  # as possible.

  # NOTE 2: The USERV1 column also contains auxiliary information on re-ringing
  # for a few individuals, so I extract this and move it into the correct column (RING2).


  # Combine all IPMR data files together
  IPMR_data <- dplyr::bind_rows(IPMR_PFN,
                                IPMR_KAT,
                                IPMR_DIN)


  #--------------------------------#
  # LOCATION PRIMARY DATA ASSEMBLY #
  #--------------------------------#

  Location_details <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_Locations_EDM.csv"),
                                      na.strings = c("", "?"), colClasses = "character",
                                      fileEncoding="UTF-8-BOM")

  # NOTE: For now, location details is only available for EDM.
  #       However, this may be added for other populations later.

  #--------------------------------#
  # ASSIGNING INDIVIDUAL IDENTITIY #
  #--------------------------------#
  # This is necessary because some birds have carried several rings during their lifetime (re-ringed)

  # Extract ring number (combinations) for all birds
  RingCombos <- IPMR_data[,c('RING', 'RING2', 'DATE')] %>%
    dplyr::mutate(DATE = as.Date(.data$DATE, format = "%d/%m/%Y")) %>%
    dplyr::distinct(.data$RING, .data$RING2, .keep_all = TRUE)

  # Collate all ring numbers used for each individual
  ReRingTable <- make_IndIdentifier(raw_data = RingCombos[,c('RING','RING2')])

  # Determine earliest occurrence of each ring
  RING1.Dates <- RingCombos[,c('RING', 'DATE')] %>%
    dplyr::distinct(.data$RING, .data$DATE, .keep_all = TRUE) %>%
    dplyr::rename("RingNr" = "RING",
                  "DATE1" = "DATE")

  RING2.Dates <- RingCombos[,c('RING2', 'DATE')] %>%
    dplyr::distinct(.data$RING2, .data$DATE, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(.data$RING2)) %>%
    dplyr::rename("RingNr" = "RING2",
                  "DATE2" = "DATE")

  RING.Dates <- dplyr::left_join(RING1.Dates, RING2.Dates,
                                 by = "RingNr") %>%
    dplyr::mutate(firstDATE = pmin(.data$DATE1, .data$DATE2, na.rm = TRUE)) %>%
    dplyr::select("RingNr", "firstDATE")

  # Merge identifiers and (re-)ringing dates, and set individual identifier (ReRingID) = first ring
  ReRingTable <- dplyr::left_join(ReRingTable, RING.Dates,
                                  by = 'RingNr') %>%
    ## TODO: check many-to-many matches
    ## RingNr E605260 is assigned to different identifiers (61565, 62479)
    ## Same is true for E605264, E606283, E606284
    ## There is a bunch of RingNr with multiple first dates:
    ## C437604, C437610, C437622, C437624, and more
    ## Check with data custodian
    dplyr::arrange(.data$Identifier, .data$firstDATE) %>%
    dplyr::group_by(.data$Identifier) %>%
    dplyr::mutate(ReRingID = dplyr::first(.data$RingNr)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$RingNr, .data$ReRingID) %>%
    dplyr::distinct(.data$RingNr, .keep_all = TRUE)
    # NOTE: At the moment, I am using this last line to remove duplicate pairings
    #       arising from two-way matches in RingCombos. Eventually, this is something
    #       that the make_IndIdentifier() function should fix.


  #--------------------------------------#
  # DETERMINING "BAD" (INCONCLUSIVE) IDs #
  #--------------------------------------#

  # Create a vector of all non-NA ID records in the Nest data (males, females, and chicks)
  allIDs <- stats::na.omit(c(Nest_data$MaleID, Nest_data$FemaleID, Nest_data$Young1,
                             Nest_data$Young2, Nest_data$Young3, Nest_data$Young4,
                             Nest_data$Young5, Nest_data$Young6, Nest_data$Young7,
                             Nest_data$Young8, Nest_data$Young9, Nest_data$Young10,
                             Nest_data$Young11))

  #Return only those that don't match the expected ringing format (i.e. XXX9999)
  #"^[A-Z]{1,}[0-9]{1,}$" is a regular expression that looks for IDs that follow a pattern:
  #- Starts with at least one capital letter: '^[A-Z]{1,}'
  #- Ends with at least one number: '[0-9]{1,}$'
  badIDs <- unique(allIDs[!stringr::str_detect(allIDs, pattern = "^[A-Z]{1,}[0-9]{1,}$")])


  #-------------------------------------#
  # CREATING STANDARD FORMAT BROOD DATA #
  #-------------------------------------#

  message("Compiling brood data...")

  Brood_data <- create_brood_PFN(Nest_data = Nest_data,
                                 ReRingTable = ReRingTable,
                                 species_filter = species,
                                 pop_filter = pop,
                                 badIDs = badIDs)

  #---------------------------------------#
  # CREATING STANDARD FORMAT CAPTURE DATA #
  #---------------------------------------#

  message("Compiling capture data....")

  Capture_data <- create_capture_PFN(Nest_data = Nest_data,
                                     IPMR_data = IPMR_data,
                                     ReRingTable = ReRingTable,
                                     species_filter = species,
                                     pop_filter = pop,
                                     badIDs = badIDs)

  #------------------------------------------#
  # CREATING STANDARD FORMAT INDIVIDUAL DATA #
  #------------------------------------------#

  message("Compiling individual data...")

  Individual_data <- create_individual_PFN(Capture_data = Capture_data,
                                           species_filter = species,
                                           pop_filter = pop)

  #----------------------------------------#
  # CREATING STANDARD FORMAT LOCATION DATA #
  #----------------------------------------#

  message("Compiling location data...")

  Location_data <- create_location_PFN(Brood_data = Brood_data,
                                       Capture_data = Capture_data,
                                       Location_details = Location_details,
                                       pop_filter = pop)

  #-------------------------------------------#
  # STANDARD FORMAT DATA WRANGLING FOR EXPORT #
  #-------------------------------------------#

  # Brood data: Add and consolidate brood size information from IPMR
  IPMRBroodSize_data <- Capture_data[,c('BroodID', 'PULALIV')] %>%
    dplyr::mutate(PULALIV = as.integer(.data$PULALIV)) %>%
    dplyr::filter(!is.na(.data$PULALIV) & !is.na(.data$BroodID)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(BroodSize_IPMR = max(.data$PULALIV),
                     .groups = "drop")

  Brood_data <- dplyr::left_join(Brood_data, IPMRBroodSize_data,
                                 by = 'BroodID') %>%
    dplyr::mutate(BroodSize_observed = dplyr::case_when(
      is.na(.data$BroodSize_observed) ~ .data$BroodSize_IPMR,
      is.na(.data$BroodSize_IPMR) ~ .data$BroodSize_observed,
      .data$BroodSize_observed == .data$BroodSize_IPMR ~ .data$BroodSize_observed,
      .data$BroodSize_observed > .data$BroodSize_IPMR ~ .data$BroodSize_observed,
      .data$BroodSize_observed < .data$BroodSize_IPMR ~ .data$BroodSize_IPMR
    ))


  # Capture data: Merge in information on ChickAge (from Brood_data) & remove unnecessary columns
  ChickAge_data <- Brood_data[,c('BroodID', 'ChickAge')]

  Capture_data <- dplyr::left_join(Capture_data, ChickAge_data,
                                   by = 'BroodID')
  Capture_data$ChickAge[which(Capture_data$Age_observed > 3)] <- NA_integer_

  # Remove unnecessary columns
  Brood_data <- Brood_data %>%
    dplyr::select(-"ChickAge", -"BroodSize_IPMR")

  Capture_data <- Capture_data %>%
    dplyr::select(-"BroodID", -"PULALIV") %>%
    dplyr::relocate("ExperimentID",
                    .after = "ChickAge")


  #-----------------------------#
  # STANDARD FORMAT DATA EXPORT #
  #-----------------------------#

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PFN.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PFN.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>%
                       dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_PFN.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PFN.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_PFN.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R object...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data,
                protocol_version = protocol_version))
  }

}

#' Create brood data table for PiedFlyNet populations.
#'
#' @param Nest_data Collated Nest primary data from PiedFlyNet populations
#' @param ReRingTable Table containing ring numbers and unique identifiers for all individuals
#' @param species_filter Species six-letter codes from the standard protocol. Used to filter the data.
#' @param pop_filter Population three-letter codes from the standard protocol. Used to filter the data.
#' @param badIDs Vector containing non-conclusive IDs (extracted from Nest data)
#' @return A data frame with Brood data


create_brood_PFN <- function(Nest_data, ReRingTable, species_filter, pop_filter, badIDs){

  # 1) Rename columns that are equivalent (content and format) to columns in the standard format
  Brood_data <- Nest_data %>%
    dplyr::rename("Plot" = "Popn",
                  "LocationID" = "Box") %>%

    # 2) Add unique BroodID and reformat columns with equivalent content but different format
    dplyr::mutate(BroodID = paste0(.data$PopID, "-", .data$BroodID),
                  BreedingSeason = as.integer(.data$Year),
                  Species = dplyr::case_when(.data$Species %in% c("BLUTI", "BLUTI ") ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$Species == "PIEFL" ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .data$Species %in% c("GRETI", "GRETI ") ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == "REDST" ~ species_codes[species_codes$SpeciesID == 11220, ]$Species,
                                             .data$Species == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                             .data$Species == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$Species == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                             .data$Species == "WREN" ~ NA_character_, # Not currently included, 1 observation only
                                             .data$Species == "TREEC" ~ NA_character_), # Not currently, 1 observation only
                  ClutchType_observed = dplyr::case_when(.data$CltCd == "1" ~ "first",
                                                         .data$CltCd == "2" ~ "replacement",
                                                         .data$CltCd == "3" ~ "second"),
                  LayDate_observed = as.Date(paste('31/03/', .data$BreedingSeason, sep = ''),
                                             format = "%d/%m/%Y") + as.numeric(.data$DFE),
                  ClutchSize = dplyr::case_when(as.integer(.data$CltSize) > 0 ~ as.integer(.data$CltSize),
                                                as.integer(.data$CltSize) == 0 | is.na(.data$CltSize) ~ NA_integer_),
                  ClutchSize_min = dplyr::case_when(is.na(.data$CltSize) | as.integer(.data$CltSize) == 0 ~ suppressWarnings(pmax(as.integer(.data$MinEggs), as.integer(.data$UnHatch)+as.integer(.data$Hatch), as.integer(.data$UnHatch)+as.integer(.data$Fledged), na.rm = TRUE))),
                  HatchDate = as.Date(paste('31/03/', .data$BreedingSeason, sep = ''),
                                      format = "%d/%m/%Y") + as.numeric(.data$DH),
                  BroodSize = as.integer(.data$Hatch),
                  NumberFledged_observed = as.integer(.data$Fledged),
                  ChickAge = as.integer(as.Date(.data$YoungDate, format = "%d/%m/%Y") - .data$HatchDate)) %>%

    # Note on ClutchSize_min: In cases in which clutch size was either not recorded, or recorded as 0 (e.g. when clutches were laid but not incubated).
    #                         other types of data may contain information on the minimum size of the clutch:
    #                         Minimum number of eggs laid (MinEggs), and number of unhatched eggs (Unhatch) plus number of hatched eggs (Hatch) or fledglings (Fledged)
    #                         Usually, only some of this information is available, and we therefore define the minimum possible clutch size as the largest number
    #                         in the above categories. We use the largest number to determine minimum clutch size because all counted eggs/chicks were definitely
    #                         part of the clutch, but the chances of eggs/chicks having disappeared before the count increases as we move further away from the
    #                         laying date (i.e. number of fledlings (+ number of unhatched eggs) <= number of hatched eggs (+ number of unhatched eggs) <= number of laid eggs)

    # 3) Add columns that are based on calculations
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed)
    # --> This sorting is required for the function "calc_clutchtype" to work

  ## 3.1) Calculate and add means and observation numbers for fledgling weight/tarsus length
  #	IF any chick ages fall into the relevant range
  if(any(Brood_data$ChickAge %in% c(14:16))){

    Brood_averages <- Brood_data %>%
      dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%  #Remove chicks outside the age range
      dplyr::select("BroodID",
                    tidyselect::contains("Weight."),
                    tidyselect::contains("Tarsus.")) %>% #Select only weight and tarsus cols
      tidyr::pivot_longer(cols = -"BroodID") %>%  #Rearrange so they're individual rows rather than individual columns
      dplyr::filter(!is.na(.data$value)) %>% #Remove Nas
      dplyr::mutate(name = gsub(pattern = "Weight",
                                replacement = "ChickMass",
                                gsub(pattern = ".Y\\d+", replacement = "", x = .data$name))) %>% #Change the names so it's not 'Weight.Y1' and 'Tarsus.Y1' but just 'ChickMass' and 'Tarsus'
      dplyr::group_by(.data$BroodID, .data$name) %>% #Group by brood, weight and tarsus
      dplyr::summarise(Avg = mean(as.numeric(.data$value), na.rm = TRUE),
                       Number = dplyr::n(),
                       .groups = "drop") %>% #Extract the mean and sample size
      tidyr::pivot_wider(names_from = "name", values_from = c("Avg", "Number"),
                         names_sep = "") #Move this back into cols so we have AvgChickMass, NumberChickMass etc.

    Brood_data <- Brood_data %>%
      dplyr::left_join(Brood_averages,
                       by = "BroodID")

  } else {

    Brood_data <- Brood_data %>%
      dplyr::mutate(AvgChickMass = NA_real_,
                    NumberChickMass = NA_integer_,
                    AvgTarsus = NA_real_,
                    NumberChicksTarsus = NA_integer_)

  }

  ## 3.2) Add other calculated columns
  Brood_data <- Brood_data %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = Brood_data, na.rm = FALSE,
                                                          protocol_version = "1.1"))

  # 4) Rename columns and add columns without data
  Brood_data <- Brood_data %>%
      dplyr::mutate(NumberChicksMass = .data$NumberChickMass,
                    NumberChicksTarsus = .data$NumberTarsus,
                    LayDate_min = as.Date(NA),
                    LayDate_max = as.Date(NA),
                    ClutchSize_observed = .data$ClutchSize,
                    ClutchSize_max = dplyr::case_when(!is.na(.data$ClutchSize_min) ~ Inf),
                    HatchDate_observed = .data$HatchDate,
                    HatchDate_min = as.Date(NA),
                    HatchDate_max = as.Date(NA),
                    BroodSize_observed = .data$BroodSize,
                    BroodSize_min = NA_integer_,
                    BroodSize_max = NA_integer_,
                    FledgeDate_observed = as.Date(NA),
                    FledgeDate_min = as.Date(NA),
                    FledgeDate_max = as.Date(NA),
                    NumberFledged_min = NA_integer_,
                    NumberFledged_max = NA_integer_,
                    AvgEggMass =  NA_real_,
                    NumberEggs = NA_integer_,
                    OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Alternative"),
                    ExperimentID = NA_character_) %>%

    # 5) Remove broods from species & pops not included in filter
    dplyr::filter(.data$Species %in% species_filter & .data$PopID %in% pop_filter) %>%

    # 6) Replace non-conclusive male and female IDs with NA
    dplyr::mutate(FemaleID = dplyr::case_when(!(.data$FemaleID %in% badIDs) ~ .data$FemaleID,
                                              .data$FemaleID %in% badIDs ~ NA_character_),
                  MaleID = dplyr::case_when(!(.data$MaleID %in% badIDs) ~ .data$MaleID,
                                            .data$MaleID %in% badIDs ~ NA_character_))

  # 7) Convert ring numbers for re-ringed females and males
  ReRingTableF <- ReRingTable %>%
    dplyr::rename("FemaleID" = "RingNr",
                  "FemaleReRingID" = "ReRingID")

  ReRingTableM <- ReRingTable %>%
    dplyr::rename("MaleID" = "RingNr",
                  "MaleReRingID" = "ReRingID")

  Brood_data <- Brood_data %>%
    dplyr::left_join(ReRingTableF,
                     by = 'FemaleID') %>%
    dplyr::left_join(ReRingTableM,
                     by = 'MaleID') %>%
    dplyr::mutate(FemaleID = ifelse(is.na(.data$FemaleReRingID),
                                    .data$FemaleID, .data$FemaleReRingID),
                  MaleID = ifelse(is.na(.data$MaleReRingID),
                                  .data$MaleID, .data$MaleReRingID)) %>%

    # 8) Select and arrange columns for output
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID", "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min", "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min", "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
                  "AvgEggMass", "NumberEggs",
                  "AvgChickMass", "NumberChicksMass",
                  "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod",
                  "ExperimentID", "ChickAge")

  # 9) Return data
  return(Brood_data)
}

#' Create capture data table for PiedFlyNet populations from
#' consolidated Nest and IPMR primary data sources.
#'
#' Consolidation of information is necessary when Nest and IPMR primary data
#' contain conflicting information about what is likely the same capture
#' Captures are matched via `PopID`, `IndvID` and `CaptureDate` when possible.
#' When `CaptureDate` is missing or incomplete, matching is attempted using
#' `BreedingSeason` and `LocationID`. If matching is impossible based on available
#' information, data is not consolidated.
#' When one of two matched capture records is missing information, equivalent
#' information from the other record is used. When two matched capture records
#' contain conflicting information, the entry is either noted as
#' conflicting (`Species`, `Sex_observed`) or information from the more reliable
#' data source is used. With the exception of `CaptureAlive` and `ReleaseAlive`,
#' Nest data is considered as the more reliable data source.
#'
#' @param Nest_data Collated Nest primary data from PiedFlyNet populations
#' @param IPMR_data Collated IPMR primary data from PiedFlyNet populations
#' @param ReRingTable Table containing ring numbers and unique identifiers for all individuals
#' @param species_filter Species six-letter codes from the standard protocol. Used to filter the data.
#' @param pop_filter Population three-letter codes from the standard protocol. Used to filter the data.
#' @param badIDs Vector containing non-conslusive IDs (extracted from Nest data)

#' @return A data frame with Capture data

create_capture_PFN <- function(Nest_data, IPMR_data, ReRingTable, species_filter, pop_filter, badIDs){

  # 1) Extract capture data from both nest and IPMR files
  Capture_data_Nest <- create_capture_Nest_PFN(Nest_data = Nest_data, ReRingTable = ReRingTable, badIDs = badIDs)
  Capture_data_IPMR <- create_capture_IPMR_PFN(IPMR_data = IPMR_data, ReRingTable = ReRingTable)

  # 2) Match and combine unique captures from both data sources

  ## 2.1) Merge information on identical captures (IndvID-Capture data combos) appearing in both datasets
  Captures_FullMatch <- dplyr::inner_join(Capture_data_Nest, Capture_data_IPMR,
                                          by = c("IndvID", "BreedingSeason", "CaptureDate"),
                                          relationship = "many-to-many")

  ## 2.2) Identify Nest and IPMR data captures that have been
  ##      duplicated/matched >1 time (Full Matches)

  Duplicates_Full_Nest <- Captures_FullMatch %>%
    dplyr::filter(!is.na(.data$rowNo_Nest)) %>%
    dplyr::mutate(Duplicate_NestCapture = ifelse(duplicated(.data$rowNo_Nest) | duplicated(.data$rowNo_Nest, fromLast = TRUE), 1,0)) %>%
    dplyr::filter(.data$Duplicate_NestCapture == 1) %>%

    # Determining true matches...
    dplyr::mutate(TrueMatch_Nest = dplyr::case_when(

      # ...based on location information (whenever available)
      !is.na(.data$LocationID_IPMR) & .data$LocationID_IPMR == .data$LocationID_Nest ~ 1,
      !is.na(.data$LocationID_IPMR) & .data$LocationID_IPMR != .data$LocationID_Nest ~ 0,

      # Based on presence of capture time for specific instances in OKE when IPMR entries were duplicated
      .data$BreedingSeason %in% c(2009, 2017, 2018) & .data$CapturePopID_Nest == "OKE" & !is.na(.data$CaptureTime) ~ 1,
      .data$BreedingSeason %in% c(2009, 2017, 2018) & .data$CapturePopID_Nest == "OKE" & is.na(.data$CaptureTime) ~ 0,

      # ...based on manual identification of actual multiple captures
      .data$BreedingSeason == 2011 & .data$CapturePopID_Nest == "OKE"  ~ 1

      ))

  # NOTE: With the currently included datasets, this resolves all occurences.
  #       If new datasets are added, this will have to be updated.

  Duplicates_Full_IPMR <- Captures_FullMatch %>%
    dplyr::filter(!is.na(.data$rowNo_IPMR)) %>%
    dplyr::mutate(Duplicate_IPMRCapture = ifelse(duplicated(.data$rowNo_IPMR) | duplicated(.data$rowNo_IPMR, fromLast = TRUE), 1,0)) %>%
    dplyr::filter(.data$Duplicate_IPMRCapture == 1) %>%

    # Determining true matches...
    # ...based on location information (whenever available)
    dplyr::mutate(TrueMatch_IPMR = dplyr::case_when(!is.na(.data$LocationID_IPMR) & .data$LocationID_IPMR == .data$LocationID_Nest ~ 1)) %>%

    #...based on whether another capture in the set was assigned as the true match
    dplyr::group_by(.data$rowNo_IPMR) %>%
    dplyr::mutate(MatchedSet = ifelse(any(.data$TrueMatch_IPMR == 1,
                                          na.rm = TRUE), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TrueMatch_IPMR = dplyr::case_when(!is.na(.data$TrueMatch_IPMR) ~ .data$TrueMatch_IPMR,
                                                    .data$MatchedSet ~ 0))

  ## 2.3) Resolve IPMR and Nest captures that were matched several times if possible, "flag" them otherwise (Full Matches)
  ##      --> We remove duplicated Nest captures that were determined to definitely NOT match with IPMR (TrueMatch_Nest = 0, NA not an option)
  ##      --> We flag duplicated IPMR captures whose Nest match could not be definitely confirmed (is.na(TrueMatch_IPMR), 0 not an option)

  Captures_FullMatch <- Captures_FullMatch %>%
    dplyr::left_join(Duplicates_Full_IPMR[,c('rowNo_Nest', 'rowNo_IPMR',
                                             'Duplicate_IPMRCapture', 'TrueMatch_IPMR')],
                     by = c("rowNo_Nest", "rowNo_IPMR")) %>%
    dplyr::left_join(Duplicates_Full_Nest[,c('rowNo_Nest', 'rowNo_IPMR',
                                             'Duplicate_NestCapture', 'TrueMatch_Nest')],
                     by = c("rowNo_Nest", "rowNo_IPMR")) %>%

    dplyr::mutate(DropRecord = dplyr::case_when(.data$TrueMatch_Nest == 0 | .data$TrueMatch_IPMR == 0  ~ TRUE,
                                                TRUE ~ FALSE),
                  UnconfirmedIPMRMatch = dplyr::case_when(is.na(.data$Duplicate_IPMRCapture) ~ FALSE,
                                                          .data$TrueMatch_Nest == 1 ~ FALSE,
                                                          .data$TrueMatch_IPMR == 1 ~ FALSE,
                                                         TRUE ~ TRUE)) %>%
    dplyr::filter(!.data$DropRecord)


  ## 2.4) Remove full matches from both datasets (using row identifiers) & rename CaptureDate columns
  Capture_data_Nest <- Capture_data_Nest %>%
    dplyr::filter(!(.data$rowNo_Nest %in% Captures_FullMatch$rowNo_Nest)) %>%
    dplyr::rename("CaptureDate_Nest" = "CaptureDate")

  Capture_data_IPMR <- Capture_data_IPMR %>%
    dplyr::filter(!(.data$rowNo_IPMR %in% Captures_FullMatch$rowNo_IPMR)) %>%
    dplyr::rename("CaptureDate_IPMR" = "CaptureDate")

  ## 2.5) Identify unique dated captures from Nest data, then remove them from dataset
  ##      (These are Nest captures that have a CaptureDate, but the date does not match with any date in IPMR)
  Captures_Nest_unique <- Capture_data_Nest %>%
    dplyr::filter(!is.na(.data$CaptureDate_Nest))

  Capture_data_Nest <- Capture_data_Nest %>%
    dplyr::filter(!(.data$rowNo_Nest %in% Captures_Nest_unique$rowNo_Nest))


  ## 2.6) Merge information on likely identical captures (IndvID-BreedingSeason combos) appearing
  ##      in both datasets
  Captures_LikelyMatch <- dplyr::full_join(Capture_data_Nest, Capture_data_IPMR,
                                           by = c("IndvID", "BreedingSeason"),
                                           relationship = "many-to-many")

  # ATTENTION:
  # If the same individual is noted several times within the same year in the nest files,
  #	but no capture dates are provided there, this approach will lead to duplicated captures
  #	and an inability to correctly "match" equivalent captures from IPMR.
  #	Important to check if that may be the case (i.e. through duplicate occurrences of row numbers in merged data),
  # for any given dataset. If such entries are present, it may be possible to "match" the correct data pairs
  #	by location (all individuals) and/or age/trait information (latter only for chick). Most likely, however,
  #	we may have to set CaptureDate to NA (most conservative). Most importantly, duplicates need to be removed!


  ## 2.7) Identify Nest and IPMR data captures that have been
  ##      duplicated/matched >1 time (Likely Matches)

  Duplicates_Likely_Nest <- Captures_LikelyMatch %>%
    dplyr::filter(!is.na(.data$rowNo_Nest) & !is.na(.data$rowNo_IPMR)) %>%
    dplyr::mutate(Duplicate_NestCapture = ifelse(duplicated(.data$rowNo_Nest) | duplicated(.data$rowNo_Nest, fromLast = TRUE), 1,0)) %>%
    dplyr::filter(.data$Duplicate_NestCapture == 1) %>%

    # Determining true matches...
    # ...based on location information (whenever available)
    dplyr::mutate(TrueMatch_Nest = dplyr::case_when(!is.na(.data$LocationID_IPMR) & .data$LocationID_IPMR == .data$LocationID_Nest ~ 1)) %>%

    #...based on whether another capture in the set was assigned as the true match
    dplyr::group_by(.data$rowNo_Nest) %>%
    dplyr::mutate(MatchedSet = ifelse(any(.data$TrueMatch_Nest == 1, na.rm = TRUE), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TrueMatch_Nest2 = dplyr::case_when(!is.na(.data$TrueMatch_Nest) ~ .data$TrueMatch_Nest,
                                                     .data$MatchedSet ~ 0))

  Duplicates_Likely_IPMR <- Captures_LikelyMatch %>%
    dplyr::filter(!is.na(.data$rowNo_Nest) & !is.na(.data$rowNo_IPMR)) %>%
    dplyr::mutate(Duplicate_IPMRCapture = ifelse(duplicated(.data$rowNo_IPMR) | duplicated(.data$rowNo_IPMR, fromLast = TRUE), 1,0)) %>%
    dplyr::filter(.data$Duplicate_IPMRCapture == 1) %>%

    # Determining true matches...
    # ...based on location information (whenever available)
    dplyr::mutate(TrueMatch_IPMR = dplyr::case_when(!is.na(.data$LocationID_IPMR) & .data$LocationID_IPMR == .data$LocationID_Nest ~ 1)) %>%

    #...based on whether another capture in the set was assigned as the true match
    dplyr::group_by(.data$rowNo_IPMR) %>%
    dplyr::mutate(MatchedSet = ifelse(any(.data$TrueMatch_IPMR == 1,
                                          na.rm = TRUE), TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TrueMatch_IPMR = dplyr::case_when(!is.na(.data$TrueMatch_IPMR) ~ .data$TrueMatch_IPMR,
                                                    .data$MatchedSet ~ 0))

  ## 2.8) Resolve IPMR and Nest captures that were matched several times if possible, "flag" them otherwise (Likely Matches)
  ##      --> We cannot remove any captures based on the assessment here (as none can be clearly proven to be "wrong")
  ##      --> We flag duplicated Nest and IPMR captures whose match could not be definitely confirmed (is.na(TrueMatch_Nest) or is.na(TrueMatch_IPMR), 0 not an option)
  Captures_LikelyMatch <- Captures_LikelyMatch %>%
    dplyr::left_join(Duplicates_Likely_IPMR[,c('rowNo_Nest', 'rowNo_IPMR',
                                               'Duplicate_IPMRCapture', 'TrueMatch_IPMR')],
                     by = c("rowNo_Nest", "rowNo_IPMR")) %>%
    dplyr::left_join(Duplicates_Likely_Nest[,c('rowNo_Nest', 'rowNo_IPMR',
                                               'Duplicate_NestCapture', 'TrueMatch_Nest')],
                     by = c("rowNo_Nest", "rowNo_IPMR")) %>%

    dplyr::mutate(DropRecord = dplyr::case_when(.data$TrueMatch_Nest == 0 | .data$TrueMatch_IPMR == 0  ~ TRUE,
                                                TRUE ~ FALSE),
                  UnconfirmedIPMRMatch = dplyr::case_when(is.na(.data$Duplicate_IPMRCapture) ~ FALSE,
                                                          .data$TrueMatch_IPMR == 1 ~ FALSE,
                                                          TRUE ~ TRUE),
                  UnconfirmedNestMatch = dplyr::case_when(is.na(.data$Duplicate_NestCapture) ~ FALSE,
                                                          .data$TrueMatch_Nest == 1 ~ FALSE,
                                                          TRUE ~ TRUE)) %>%
    dplyr::filter(!(.data$DropRecord))


  ## 2.9) Re-combine CaptureDate- and BreedingSeason-based matches (also including unique IPMR captures), and unique nest captures
  Capture_data <- dplyr::bind_rows(Captures_FullMatch, Captures_LikelyMatch, Captures_Nest_unique) %>%
    dplyr::mutate(UnconfirmedIPMRMatch = ifelse(is.na(.data$UnconfirmedIPMRMatch),
                                                FALSE, .data$UnconfirmedIPMRMatch),
                  UnconfirmedNestMatch = ifelse(is.na(.data$UnconfirmedNestMatch),
                                                FALSE, .data$UnconfirmedNestMatch))


  # 3) Consolidate information from Nest and IPMR captures
  Capture_data <- Capture_data %>%
    dplyr::mutate(

      ## 3.1) Capture dates, times & observers
      ##      (Use IPMR if unavailable from Nest and definitive match is possible)
      CaptureDate = dplyr::case_when(
        !is.na(.data$CaptureDate) ~ .data$CaptureDate, # This ensures date from full matches (by date) is used when available
        .data$UnconfirmedIPMRMatch ~ as.Date(NA),
        !is.na(.data$CaptureDate_Nest) ~ .data$CaptureDate_Nest,
        is.na(.data$CaptureDate_Nest) ~ .data$CaptureDate_IPMR
      ),

      CaptureTime = dplyr::case_when(
        is.na(CaptureDate) ~ NA_character_,
        .data$CaptureDate == .data$CaptureDate_IPMR ~ .data$CaptureTime,
        TRUE ~ NA_character_
      ),
      # NOTE: Think about having criteria for defining certain types of CaptureTimes as NA (e.g. 00:00)

      ObserverID = dplyr::case_when(
        is.na(CaptureDate) ~ NA_character_,
        .data$CaptureDate == .data$CaptureDate_IPMR ~ .data$ObserverID,
        TRUE ~ NA_character_
      ),

      # 3.2) Species
      #      (Set to 'CCCCCC' if conflicting)
      Species = dplyr::case_when(
        is.na(.data$Species_Nest) ~ .data$Species_IPMR,
        is.na(.data$Species_IPMR) ~ .data$Species_Nest,
        .data$Species_Nest == .data$Species_IPMR  ~ .data$Species_Nest,
        .data$Species_Nest != .data$Species_IPMR ~ 'CCCCCC'
      ),
      # MB: These should be flagged as conflicted and checked manually.

      # 3.3) LocationID
      #     (Use Nest if conflicting and there is no unconfirmed Nest match)
      #     (Use IPMR if conflicting and there is an unconfirmed Nest match)
      LocationID = dplyr::case_when(
        is.na(.data$LocationID_Nest) ~ .data$LocationID_IPMR,
        is.na(.data$LocationID_IPMR) ~ .data$LocationID_Nest,
        .data$LocationID_Nest == .data$LocationID_IPMR ~ .data$LocationID_Nest,
        .data$LocationID_Nest != .data$LocationID_IPMR & !.data$UnconfirmedNestMatch ~ .data$LocationID_Nest,
        .data$LocationID_Nest != .data$LocationID_IPMR & .data$UnconfirmedNestMatch ~ .data$LocationID_IPMR
      ),
      # MB: Generally Nest data priority here (spent a lot of time cleaning this in Nest files).
      #     Currently exception for conflicts in unconfirmed matches: IPMR priority
      #     (We may revisit the latter decision at a later point)


      # 3.4) CapturePopID & ReleasePopID
      CapturePopID = dplyr::case_when(
        is.na(.data$CapturePopID_Nest) ~ .data$CapturePopID_IPMR,
        is.na(.data$CapturePopID_IPMR) ~ .data$CapturePopID_Nest,
        .data$CapturePopID_Nest == .data$CapturePopID_IPMR ~ .data$CapturePopID_Nest,
        .data$CapturePopID_Nest != .data$CapturePopID_IPMR ~ .data$CapturePopID_Nest
      ),
      ReleasePopID = dplyr::case_when(
        is.na(.data$ReleasePopID_Nest) ~ .data$ReleasePopID_IPMR,
        is.na(.data$ReleasePopID_IPMR) ~ .data$ReleasePopID_Nest,
        .data$ReleasePopID_Nest == .data$ReleasePopID_IPMR ~ .data$ReleasePopID_Nest,
        .data$ReleasePopID_Nest != .data$ReleasePopID_IPMR ~ .data$ReleasePopID_Nest
      ),
      # NOTE: There are no occurrences of conflicts here in the data at the moment.

      # 3.5) CapturePlot & ReleasePlot
      CapturePlot = dplyr::case_when(
        is.na(.data$CapturePlot_Nest) ~ .data$CapturePlot_IPMR,
        is.na(.data$CapturePlot_IPMR) ~ .data$CapturePlot_Nest,
        .data$CapturePlot_Nest == .data$CapturePlot_IPMR ~ .data$CapturePlot_Nest,
        .data$CapturePlot_Nest != .data$CapturePlot_IPMR ~ .data$CapturePlot_Nest
      ),
      ReleasePlot = dplyr::case_when(
        is.na(.data$ReleasePlot_Nest) ~ .data$ReleasePlot_IPMR,
        is.na(.data$ReleasePlot_IPMR) ~ .data$ReleasePlot_Nest,
        .data$ReleasePlot_Nest == .data$ReleasePlot_IPMR ~ .data$ReleasePlot_Nest,
        .data$ReleasePlot_Nest != .data$ReleasePlot_IPMR ~ .data$ReleasePlot_Nest
      ),
      # MB: Nest data priority here too

      # 3.6) Trait measurements (Mass & Tarsus & OriginalTarusMethod)
      Mass = dplyr::case_when(
        is.na(.data$Mass_Nest) ~ .data$Mass_IPMR,
        is.na(.data$Mass_IPMR) ~ .data$Mass_Nest,
        .data$Mass_Nest == .data$Mass_IPMR ~ .data$Mass_Nest,
        .data$Mass_Nest != .data$Mass_IPMR ~ .data$Mass_Nest
      ),
      # MB: IPMR only lets you record one digit, so go with nest (more information)

      Tarsus = dplyr::case_when(
        is.na(.data$Tarsus_Nest) ~ .data$Tarsus_IPMR,
        is.na(.data$Tarsus_IPMR) ~ .data$Tarsus_Nest,
        .data$Tarsus_Nest == .data$Tarsus_IPMR ~ .data$Tarsus_Nest,
        .data$Tarsus_Nest != .data$Tarsus_IPMR ~ .data$Tarsus_Nest
      ),
      # MB: IPMR only lets you record one digit, so go with nest (more information)

      OriginalTarsusMethod = dplyr::case_when(
        is.na(.data$Tarsus_Nest) ~ .data$OriginalTarsusMethod_IPMR,
        is.na(.data$Tarsus_IPMR) ~ .data$OriginalTarsusMethod_Nest,
        .data$Tarsus_Nest == .data$Tarsus_IPMR ~ .data$OriginalTarsusMethod_Nest,
        .data$Tarsus_Nest != .data$Tarsus_IPMR ~ .data$OriginalTarsusMethod_Nest
      ),

      # NOTE: There are no occurrences of conflicts here in the data at the moment.

      # 3.7) Age_observed
      Age_observed = dplyr::case_when(
        is.na(.data$Age_observed_Nest) ~ .data$Age_observed_IPMR,
        is.na(.data$Age_observed_IPMR) ~ .data$Age_observed_Nest,
        .data$Age_observed_Nest == .data$Age_observed_IPMR ~ .data$Age_observed_Nest,
        .data$Age_observed_Nest < 4 & .data$Age_observed_IPMR < 4 ~ .data$Age_observed_Nest,
        .data$Age_observed_Nest >= 4 & .data$Age_observed_IPMR >= 4 ~ .data$Age_observed_Nest
        # In situations in which there is conflicting info on whether an individual was
        # a chick (>4) or an adult (>=4), Age_observed is currently set to NA.
        # These records are always mistakes in the raw data, and are best pointed out and resolved there.
      ),

      # MB: There should not be age classes 2 or 3 in a nest capture --> if we have nest data age 1, we should always stick with that (--> are there even any cases where this happens?)
      # MB: some ringers think they can age birds >4 in the field (-> IPMR data), but they are often not correct

      # 3.8) Sex_observed
      Sex_observed = dplyr::case_when(
        is.na(.data$Sex_observed_Nest) ~ .data$Sex_observed_IPMR,
        is.na(.data$Sex_observed_IPMR) ~ .data$Sex_observed_Nest,
        .data$Sex_observed_Nest == .data$Sex_observed_IPMR  ~ .data$Sex_observed_Nest,
        .data$Sex_observed_Nest != .data$Sex_observed_IPMR ~ 'C'
      ),

      # 3.9) CaptureAlive & ReleaseAlive
      CaptureAlive = dplyr::case_when(
        is.na(.data$CaptureAlive_Nest) ~ .data$CaptureAlive_IPMR,
        is.na(.data$CaptureAlive_IPMR) ~ .data$CaptureAlive_Nest,
        .data$CaptureAlive_Nest == .data$CaptureAlive_IPMR ~ .data$CaptureAlive_Nest,
        .data$CaptureAlive_Nest != .data$CaptureAlive_IPMR ~ .data$CaptureAlive_IPMR
      ),
      ReleaseAlive = dplyr::case_when(
        is.na(.data$ReleaseAlive_Nest) ~ .data$ReleaseAlive_IPMR,
        is.na(.data$ReleaseAlive_IPMR) ~ .data$ReleaseAlive_Nest,
        .data$ReleaseAlive_Nest == .data$ReleaseAlive_IPMR ~ .data$ReleaseAlive_Nest,
        .data$ReleaseAlive_Nest != .data$ReleaseAlive_IPMR ~ .data$ReleaseAlive_IPMR
      )
      # MB: Priority to IPMR because adults found dead in boxes will still be recorded as breeding males/females in nest file

    ) %>%


    ## 4) Calculate age at capture

    # Sort chronologically within individual
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%

    #Calculate age at each capture based on first capture
    # NOTE: Ideally, this will be done for data from all populations together (to correctly calculate age of known immigrants)
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%


    ## 5) Remove captures from individuals not part of any included populations and remove species
    dplyr::filter(!is.na(.data$CapturePopID), .data$CapturePopID %in% pop_filter, .data$Species %in% species_filter) %>%

    ## 6) Make CaptureID

    # Write unique capture identifier as IndvID-CaptureNumber
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste0(.data$IndvID, '-', dplyr::row_number())) %>%
    dplyr::ungroup() %>%

    ## 11) Select required columns
    dplyr::select("CaptureID", "IndvID", "Species", "Sex_observed",
                  "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID", "CaptureAlive", "ReleaseAlive",
                  "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot",
                  "Mass", "Tarsus", "OriginalTarsusMethod", "WingLength",
                  "Age_observed", "Age_calculated", "ExperimentID",
                  "BroodID", "PULALIV")

  # Return complete Capture data
  return(Capture_data)

}


#' Extract capture data table for PiedFlyNet populations from
#' Nest primary data.
#'
#' @param Nest_data Collated Nest primary data from PiedFlyNet populations
#' @param ReRingTable Table containing ring numbers and unique identifiers for all individuals
#' @param badIDs Vector containing non-conslusive IDs (extracted from Nest data)

create_capture_Nest_PFN <- function(Nest_data, ReRingTable, badIDs){

  Capture_data <- Nest_data

  # 1) Extract male capture data
  Male_Capture_data <- tibble::tibble(IndvID = Capture_data$MaleID,
                                      BroodID = NA_character_, # Will be removed later
                                      Species_Nest = dplyr::case_when(Capture_data$Species %in% c("BLUTI", "BLUTI ") ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                                                      Capture_data$Species == "PIEFL" ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                                                      Capture_data$Species %in% c("GRETI", "GRETI ") ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                                                      Capture_data$Species == "REDST" ~ species_codes[species_codes$SpeciesID == 11220, ]$Species,
                                                                      Capture_data$Species == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                                                      Capture_data$Species == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                                                      Capture_data$Species == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                                                      Capture_data$Species == "WREN" ~ NA_character_, # Not currently included, 1 observation only
                                                                      Capture_data$Species == "TREEC" ~ NA_character_), # Not currently, 1 observation only
                                      BreedingSeason = as.integer(Capture_data$Year),
                                      CaptureDate = as.Date(Capture_data$MaleDate, format = "%d/%m/%Y"),
                                      LocationID_Nest = Capture_data$Box,
                                      CapturePopID_Nest = Capture_data$PopID,
                                      CapturePlot_Nest = Capture_data$Popn,
                                      ReleasePopID_Nest = Capture_data$PopID,
                                      ReleasePlot_Nest = Capture_data$Popn,
                                      Mass_Nest = NA_real_,
                                      Tarsus_Nest = NA_real_,
                                      OriginalTarsusMethod_Nest = NA_character_,
                                      #WingLength = NA_real_,
                                      Age_observed_Nest = dplyr::case_when(is.na(Capture_data$MaleMinAge) ~ 4L,
                                                                           Capture_data$MaleStatus == 'R' ~ as.integer(Capture_data$MaleMinAge)*2L + 3L,
                                                                           TRUE ~ as.integer(Capture_data$MaleMinAge)*2L + 2L),
                                      Sex_observed_Nest = "M",
                                      CaptureAlive_Nest = TRUE,
                                      ReleaseAlive_Nest = TRUE)

  # 2) Extract female capture data
  Female_Capture_data <- tibble::tibble(IndvID = Capture_data$FemaleID,
                                        BroodID = NA_character_, # Will be removed later
                                        Species_Nest = dplyr::case_when(Capture_data$Species %in% c("BLUTI", "BLUTI ") ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                                                        Capture_data$Species == "PIEFL" ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                                                        Capture_data$Species %in% c("GRETI", "GRETI ") ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                                                        Capture_data$Species == "REDST" ~ species_codes[species_codes$SpeciesID == 11220, ]$Species,
                                                                        Capture_data$Species == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                                                        Capture_data$Species == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                                                        Capture_data$Species == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                                                        Capture_data$Species == "WREN" ~ NA_character_, # Not currently included, 1 observation only
                                                                        Capture_data$Species == "TREEC" ~ NA_character_), # Not currently, 1 observation only
                                        BreedingSeason = as.integer(Capture_data$Year),
                                        CaptureDate = as.Date(Capture_data$FemaleDate, format = "%d/%m/%Y"),
                                        LocationID_Nest = Capture_data$Box,
                                        CapturePopID_Nest = Capture_data$PopID,
                                        CapturePlot_Nest = Capture_data$Popn,
                                        ReleasePopID_Nest = Capture_data$PopID,
                                        ReleasePlot_Nest = Capture_data$Popn,
                                        Mass_Nest = NA_real_,
                                        Tarsus_Nest = NA_real_,
                                        OriginalTarsusMethod_Nest = NA_character_,
                                        #WingLength = NA_real_,
                                        Age_observed_Nest = dplyr::case_when(is.na(Capture_data$FemaleMinAge) ~ 4L,
                                                                             Capture_data$FemaleStatus == 'R' ~ as.integer(Capture_data$FemaleMinAge)*2L + 3L,
                                                                             TRUE ~ as.integer(Capture_data$FemaleMinAge)*2L + 2L),
                                        Sex_observed_Nest = "F",
                                        CaptureAlive_Nest = TRUE,
                                        ReleaseAlive_Nest = TRUE)

  # 3) Extract chick capture data
  Chick_Capture_data <- Capture_data %>%
    dplyr::select(-"Young12") %>%
    # Pivot information stored in columns to rows
    tidyr::pivot_longer(cols = tidyselect::matches("Young[[:digit:]]|Weight.Y[[:digit:]]|Tarsus.Y[[:digit:]]"),
                        names_to = c(".value", "id"),
                        names_pattern = "^([[:alpha:]]+).*([[:digit:]]{1,2})$") %>%
    dplyr::rename("IndvID" = "Young") %>%
    dplyr::mutate(BroodID = paste(.data$PopID, .data$BroodID, sep = "-"),
                  Species_Nest = dplyr::case_when(.data$Species %in% c("BLUTI", "BLUTI ") ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                                  .data$Species == "PIEFL" ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                                  .data$Species %in% c("GRETI", "GRETI ") ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                                  .data$Species == "REDST" ~ species_codes[species_codes$SpeciesID == 11220, ]$Species,
                                                  .data$Species == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                                  .data$Species == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                                  .data$Species == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species,
                                                  .data$Species == "WREN" ~ NA_character_, # Not currently included, 1 observation only
                                                  .data$Species == "TREEC" ~ NA_character_),
                  BreedingSeason = as.integer(.data$Year),
                  CaptureDate = as.Date(.data$YoungDate, format = "%d/%m/%Y"),
                  LocationID_Nest = .data$Box,
                  CapturePopID_Nest = .data$PopID,
                  CapturePlot_Nest = .data$Popn,
                  ReleasePopID_Nest = .data$PopID,
                  ReleasePlot_Nest = .data$Popn,
                  Mass_Nest = as.numeric(.data$Weight),
                  Tarsus_Nest = as.numeric(.data$Tarsus),
                  OriginalTarsusMethod_Nest = "Alternative",
                  Age_observed_Nest = 1L,
                  Sex_observed_Nest = NA_character_,
                  CaptureAlive_Nest = TRUE,
                  ReleaseAlive_Nest = TRUE) %>%
    dplyr::select("IndvID", "BroodID", "Species_Nest":"ReleaseAlive_Nest")

  # 4) Combine data
  Capture_data <- dplyr::bind_rows(Chick_Capture_data, Male_Capture_data, Female_Capture_data) %>%

    # 5) Split captures for cases in which two individuals were reported in the same field (IDs separates by "/")
    tidyr::separate_rows(.data$IndvID, sep = '/') %>%

    # 6) Remove all individuals with missing IDs and/or from species not included in Species_codes
    dplyr::filter(.data$Species_Nest %in% species_codes$Species & !is.na(.data$IndvID) & !(.data$IndvID %in% badIDs)) %>%

    # 7) Add a unique row identifier
    dplyr::mutate(rowNo_Nest = dplyr::row_number())

  # 8) Adjust IndvID for re-ringed individuals
  Capture_data <- dplyr::left_join(Capture_data, ReRingTable,
                                   by = c('IndvID' = 'RingNr')) %>%
    dplyr::mutate(IndvID = dplyr::case_when(is.na(.data$ReRingID) ~ .data$IndvID,
                                            .data$IndvID != .data$ReRingID ~ .data$ReRingID,
                                            .data$IndvID == .data$ReRingID ~ .data$IndvID))

  # 9) Return data
  return(Capture_data)

}


#' Extract capture data table for PiedFlyNet populations from
#' IPMR primary data.
#'
#' @param IPMR_data Collated IPMR primary data from PiedFlyNet populations
#' @param Nest_data Collated Nest primary data from PiedFlyNet populations
#' @param ReRingTable Table containing ring numbers and unique identifiers for all individuals

create_capture_IPMR_PFN <- function(IPMR_data, Nest_data, ReRingTable){


  ## 1) Rename columns that are equivalent (content and format) to columns in the standard format
  Capture_data <- IPMR_data %>%
    dplyr::rename("IndvID" = "RING",
                  "CapturePlot_IPMR" = "PLACE",
                  "Sex_observed_IPMR" = "SEX") %>%

    ## 2) Add population ID and reformat colums with equivalent content but different format
    dplyr::mutate(PopID = dplyr::case_when(!is.na(.data$PopID) ~ .data$PopID,
                                           .data$CapturePlot_IPMR %in% c("YAR", "BVW", "NEA") ~ "EDM",
                                           .data$CapturePlot_IPMR %in% c("BRWD", "STEPS", "HITCH") ~ "TEI",
                                           .data$CapturePlot_IPMR %in% c("OKECAS", "MELD", "MELDON", "OKEHAM", "FATOKE", "EASOKE", "BELSTO") ~ "OKE"),

                  Species_IPMR = dplyr::case_when(.data$SPEC == "BLUTI" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                                  .data$SPEC %in% c("PIEFL", "Pied Flycatcher") ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                                  .data$SPEC == "GRETI" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                                  .data$SPEC %in% c("REDST", "Redstart") ~ species_codes[species_codes$SpeciesID == 11220, ]$Species,
                                                  .data$SPEC == "MARTI" ~ species_codes[species_codes$SpeciesID == 14400, ]$Species,
                                                  .data$SPEC == "NUTHA" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                                  .data$SPEC == "COATI" ~ species_codes[species_codes$SpeciesID == 14610, ]$Species),

                  LocationID_IPMR = dplyr::case_when(.data$SITE == "MILL" ~ "Foxworthy Mill", # Specific EDM location
                                                     .data$SITE == "HIDE2" ~ "HIDE1D", # Specific EDM location
                                                     .data$PopID == "EDM" ~ .data$SITE, # Remaining EDM locations (perfect matches)
                                                     .data$PopID == "DIN" ~ .data$SITE_DIN,
                                                     #PopID %in% c("TEI", "OKE") & stringr::str_detect(SITE, "[[:digit:]]") ~ as.character(readr::parse_number(SITE)), # TEI and OKE locations (number match)
                                                     .data$PopID %in% c("TEI", "OKE") & stringr::str_detect(.data$SITE, "[[:digit:]]") ~ gsub(".*?([0-9]+).*", "\\1", .data$SITE),
                                                     TRUE ~ .data$SITE), # All other conditions

                  CaptureDate = as.Date(.data$DATE, format = "%d/%m/%Y"),
                  CaptureTime = ifelse(!grepl("00:00", .data$DATE), sub(".* ", "", .data$DATE), NA),
                  ObserverID = dplyr::case_when(!is.na(.data$INIT) ~ .data$INIT,
                                                is.na(.data$INIT) & !is.na(.data$RINGINIT) ~ .data$RINGINIT),
                  CapturePopID_IPMR = .data$PopID,
                  ReleasePopID_IPMR = .data$PopID,
                  ReleasePlot_IPMR = .data$CapturePlot_IPMR,
                  Mass_IPMR = as.numeric(.data$WT),
                  Tarsus_IPMR = dplyr::case_when(is.na(.data$TSMTD) ~ as.numeric(.data$TARSUS),
                                                 .data$TSMTD == "S" ~ as.numeric(.data$TARSUS),
                                                 .data$TSMTD == "M" ~ (x = convert_tarsus(as.numeric(.data$TARSUS), method = "Oxford"))),
                  OriginalTarsusMethod_IPMR = dplyr::case_when(is.na(.data$TSMTD) ~ "Alternative",
                                                               .data$TSMTD == "S" ~ "Alternative",
                                                               .data$TSMTD == "M" ~ "Oxford"),
                  WingLength = as.numeric(.data$WING),
                  Age_observed_IPMR = as.integer(stringr::str_remove(.data$AGE, "J")),
                  stringsAsFactors = FALSE) %>%

    ## 3) Add additional columns that need to be derived from original columns
    dplyr::mutate(BreedingSeason = as.integer(format(.data$CaptureDate, "%Y")),
                  CaptureAlive_IPMR = ifelse(.data$RTYPE == "X", FALSE, TRUE),
                  ReleaseAlive_IPMR = ifelse(.data$RTYPE == "X", FALSE, TRUE),
                  ExperimentID = dplyr::case_when(.data$COND == "M" ~ "SURVIVAL",
                                                  is.na(.data$COND) ~ NA_character_)
    ) %>%


    ## 4) Exclude entries not included in the standard format

    # Remove all individuals from species not included in Species_codes
    dplyr::filter(.data$Species_IPMR %in% species_codes$Species) %>%

    # Remove all observations not involving a capture/dead recovery (i.e. resightings)
    dplyr::filter(.data$RTYPE != 'O') %>%

    # NOTE: For now, we are excluding "resightings" from the output.
    #       However, we may change this following a future update of the standard protocol with the inclusion of a "CaptureType" variable (which will allow to distinguish recaptures and resightings)

    ## 5) Add a unique row identifier
    dplyr::mutate(rowNo_IPMR = dplyr::row_number()) %>%

    ## 6) Adjust IndvID for re-ringed individuals
    dplyr::left_join(ReRingTable,
                     by = c('IndvID' = 'RingNr')) %>%
    dplyr::mutate(IndvID = dplyr::case_when(is.na(.data$ReRingID) ~ .data$IndvID,
                                            .data$IndvID != .data$ReRingID ~ .data$ReRingID,
                                            .data$IndvID == .data$ReRingID ~ .data$IndvID)) %>%

    ## 7) Select required columns
    dplyr::select("IndvID", "Species_IPMR", "Sex_observed_IPMR",
                  "BreedingSeason", "CaptureDate", "CaptureTime", "ObserverID",
                  "LocationID_IPMR", "CaptureAlive_IPMR", "ReleaseAlive_IPMR",
                  "CapturePopID_IPMR", "CapturePlot_IPMR", "ReleasePopID_IPMR", "ReleasePlot_IPMR",
                  "Mass_IPMR", "Tarsus_IPMR", "OriginalTarsusMethod_IPMR", "WingLength",
                  "Age_observed_IPMR", "ExperimentID", "rowNo_IPMR",
                  "PULALIV")

  ## 7) Return data
  return(Capture_data)
}

#' Create individual data table for PiedFlyNet populations
#'
#' @param Capture_data Capture data table generated by `create_capture_PFN`
#' @param species_filter Species six-letter codes from the standard protocol. Used to filter the data.
#' @param pop_filter Population three-letter codes from the standard protocol. Used to filter the data.
#'
#' @return A data frame with Individual data

create_individual_PFN <- function(Capture_data, species_filter, pop_filter){

  # 1) Take capture data and determine basic summary data for each individual (within population)
  IndvPop_data <- Capture_data %>%
    dplyr::arrange(.data$CapturePopID, .data$IndvID, .data$BreedingSeason,
                   .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
    dplyr::summarise(PopID = dplyr::first(.data$CapturePopID),
                     .groups = "keep") %>%
    dplyr::ungroup()

  # 2) Take capture data and determine basic summary data for each individual (across populations)
  Indv_data <- Capture_data %>%
    dplyr::arrange(.data$IndvID, .data$CapturePopID, .data$BreedingSeason,
                   .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                              .f = ~{

                                                if(length(..1) == 0){

                                                  return(NA_character_)

                                                } else if(length(..1) == 1){

                                                  return(..1)

                                                } else {

                                                  return("CCCCCC")

                                                }

                                              }),
                     BroodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodID))),
                                                  .f = ~{

                                                    if(length(..1) == 0){

                                                      return(NA_character_)

                                                    } else if(length(..1) == 1){

                                                      return(..1)

                                                    } else {

                                                      return("CONFLICTED")

                                                    }

                                                  }), ## TODO: check with data custodian
                     BroodIDFledged = .data$BroodIDLaid, # Identical, as no cross-fostering experiments were made
                     Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                     .f = ~{

                                                       if(length(..1) == 0){

                                                         return(NA_character_)

                                                       } else if(length(..1) == 1){

                                                         return(..1)

                                                       } else {

                                                         return("C")

                                                       }

                                                     }),
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = ifelse(any(.data$Age_calculated %in% c(1, 3)), "chick",
                                      ifelse(min(.data$Age_calculated) == 2, NA_character_, "adult")),
                     .groups = "keep") %>%
    dplyr::ungroup()


  # 3) Merge information and format
  Individual_data <- dplyr::left_join(IndvPop_data, Indv_data,
                                      by = c('IndvID')) %>%
    # Filter species & pops
    dplyr::filter(.data$Species %in% species_filter, .data$PopID %in% pop_filter) %>%

    # Add a column for genetic sex (not available here)
    dplyr::mutate(Sex_genetic = NA_character_) %>%

    # Order columns and sort
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_calculated", "Sex_genetic") %>%
    dplyr::arrange(.data$IndvID, .data$PopID, .data$RingSeason)

  return(Individual_data)

}

#' Create location data table for PiedFlyNet populations.
#'
#' @param Brood_data Brood data table generated by `create_brood_PFN`
#' @param Capture_data Capture data table generated by `create_capture_PFN`
#' @param Location_details Table containing detailed information on nest boxes.
#' Currently only contains information for EDM. Information for other populations
#' will be added later.
#' @param pop_filter Population three-letter codes from the standard protocol. Used to filter the data.
#'
#' @return A data frame with Location data

create_location_PFN <- function(Brood_data, Capture_data, Location_details, pop_filter){

  # 1) Format additional data on nest boxes
  #    NOTE: At present, additional nest box data is only available for East Dartmoor (EDM)
  #          Such data will be added for other populations at a later stage.

  Location_details <- Location_details %>%

    dplyr::select(-"Popn") %>%

    dplyr::rename("NestboxID" = "Box",
                  "Latitude" = "lat",
                  "Longitude" = "long",
                  "StartSeason_ext" = "First",
                  "EndSeason_ext" = "Last") %>%

    dplyr::mutate(PopID = 'EDM',
                  Latitude = suppressWarnings(as.numeric(.data$Latitude)),
                  Longitude = suppressWarnings(as.numeric(.data$Longitude)),
                  StartSeason_ext = suppressWarnings(as.integer(.data$StartSeason_ext)),
                  EndSeason_ext = suppressWarnings(as.integer(.data$EndSeason_ext)))

  # 2) Start a dataframe with all unique LocationIDs / NestboxIDs from brood data
  # NOTE: In this case, LocationIDs and NestboxIDs are identical.

  Location_data <- calc_birdNBuse(Brood_data) %>%

    # 3) Add additional columns
    dplyr::mutate(NestboxID = .data$LocationID,
                  LocationType = 'NB') %>%

    # 4) Merge in detailed information
    dplyr::left_join(Location_details,
                     by = c('PopID', 'NestboxID')) %>%

    # 5) Prioritize detailed information when available
    dplyr::mutate(StartSeason = ifelse(!is.na(.data$StartSeason_ext), .data$StartSeason_ext, .data$StartSeason),
                  EndSeason = ifelse(!is.na(.data$StartSeason_ext), .data$EndSeason_ext, .data$EndSeason)) %>%
    dplyr::select(-"StartSeason_ext", -"EndSeason_ext")

  # 6) Collect information on additional capture locations (from capture data)
  CapLocations <- Capture_data[,c('CapturePopID', 'LocationID')] %>%
    dplyr::distinct() %>%
    dplyr::rename("PopID" = "CapturePopID")

  # 6) Combine data, add HabitatType, and select relevant columns
  Location_data <- Location_data %>%
    dplyr::full_join(CapLocations,
                     by = c('PopID', 'LocationID')) %>%
    dplyr::filter(.data$PopID %in% pop_filter) %>%
    dplyr::mutate(HabitatType = 'deciduous') %>%
    dplyr::select("LocationID", "NestboxID",
                  "LocationType", "PopID",
                  "Latitude", "Longitude",
                  "StartSeason", "EndSeason",
                  "HabitatType") %>%
    dplyr::arrange(.data$PopID, .data$StartSeason, .data$LocationID, .data$NestboxID)

  return(Location_data)

}

#' Make a summary table containing all ring numbers of re-ringed individual
#'
#' Assigns new unique identifiers to individuals that appear with two or more
#' different ring numbers in a PFN data set. The resulting table can then be used
#' to replace all alternative ring numbers of an individual with the new
#' identifier in all tables contained in the standard format.
#'
#' @param raw_data Data frame. Contains pairs of rings (one pair per
#' re-ringing event). Column 'RING' = first ring, column 'RING2' = second ring.
#'
#' @return A data frame with columns 'RingNr' (= original ring number) and
#' 'Identifier' (= new unique integer identifier for the individual).
#'
#' @export
#'
#' @examples
#' #Create fake dataset
#' dat <- data.frame(RING = c('A1', 'B2', 'C7', 'D7'), RING2 = c('B9', 'C7', 'E3', NA))
#' #Summarise re-ringing information by individual
#' make_IndIdentifier(raw_data = dat)

make_IndIdentifier = function(raw_data){

  #First, we need make sure any first ring ("RING") only appears once in the data.
  #(This involves removing pairings of re-curring rings with NA for RING2)
  duplicateRING <- c(raw_data[which(duplicated(raw_data$RING)),'RING'],
                     raw_data[which(raw_data$RING %in% raw_data$RING2), 'RING'])

  raw_data <- raw_data %>%
    dplyr::filter(!(.data$RING %in% duplicateRING & is.na(.data$RING2)))

  #We will be left joining one set of data to the other
  #so we need to make sure the column names match
  check_data  <- raw_data %>%
    dplyr::rename("RING2" = "RING",
                  "RING3" = "RING2")

  #Create an output_data object that will be updated
  output_data <- raw_data

  #As long as any new ring values (e.g. RING2) are also used as an old ring value (e.g. RING)
  #Then we need to keep going
  #We use ncol() so that the code is robust to any number of re-ringings
  N = 1
  while (any(check_data[, 2] %in% check_data[, 1])) {

    #Add a new column to the output
    #Instead of RING > RING2, we now have RING > RING2 > RING3
    output_data <- output_data %>%
      dplyr::left_join(check_data,
                       by = "RING2") %>%
      #Here we are removing the now redundant rows (where RING2 is now RING3 somewhere else)
      #!!as.symbol is just a way of referring to a column in dplyr using some code that returns a string rather than just a column name
      #e.g. !!as.symbol(names(.)[ncol(.) - 1]) refers to the column with the second last name
      dplyr::filter(!(!is.na(!!as.symbol(names(.)[ncol(.) - 1])) & !!as.symbol(names(.)[ncol(.) - 1]) %in% !!as.symbol(names(.)[ncol(.)]) & is.na(!!as.symbol(names(.)[ncol(.)]))))

    #Update the check_data to only consider those where a new column was added (i.e. they have a RING3 value)
    check_data <- output_data %>%
      dplyr::filter(!is.na(!!as.symbol(names(.)[ncol(.)]))) %>%
      #Only use the last 2 columns
      #We're only interested in whether there are any remaining cases where a left join could be appropriate
      dplyr::select((ncol(.)-1):ncol(.)) %>%
      dplyr::filter(.[1] != .[2])

    #Change the name of the columns to use in the next loop (i.e. next time we want to add RING4)
    colnames(check_data) <- paste0("RING", c(N, N + 1))

    #Update N so we can keep adding new columns
    N <- N + 1

  }

  #The while loop above will make it so that each individual has a single row with N columns
  #which is the number of rings they've been given over their life.
  #We can now convert this into long format where the IndvID is just the rownumber.
  final_rings <- output_data %>%
    #Create a unique IndvID from the row number that is independent of the ring
    tibble::rowid_to_column(var = "Identifier") %>%
    #Pivot so that each ring number has its own row
    tidyr::pivot_longer(cols = tidyselect::starts_with("RING"),
                        values_to = "RingNr") %>%
    dplyr::filter(!is.na(.data$RingNr)) %>%
    dplyr::select("RingNr", "Identifier")

  # Return final re-ringing table
  return(final_rings)

}
