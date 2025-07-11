#'Construct standard format for data from Glasgow, Scotland
#'
#'A pipeline to produce the standard format for the nest box population in Glasgow, Scotland, administered by Davide Dominoni.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Primarily great tits and blue tits.
#'
#'\strong{BroodID}: A concatentation of PopID and the row number of the brood record (e.g. SAL-1).
#'
#'\strong{IndvID}: Should be a 7 digit alphanumeric string. IndvIDs with a different number of characters are likely errors.
#'These are set to NA and removed.
#'
#'\strong{CaptureDate}: Some individuals were not recorded in the ringing records, but were observed breeding at a monitored nest.
#'For these individuals, the CaptureDate is set as May 15 of the breeding year.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_GLA <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force choose_directory() if used
  force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

  # Determine species and population codes for filtering
  if(is.null(species)){

    species_filter <- NULL

  } else {

    species_filter <- species

  }

  ## Only keeping 5 main populations, so these will be the default filter ("CAS", "GAR", "SAL","KEL", "SCE")
  ## Otherwise, use the specified pop filter
  if(is.null(pop)){

    pop_filter <- c("CAS", "GAR", "SAL","KEL", "SCE")

  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ##  Options
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)

  ## Read experiment classification table
  expID_tab <- utils::read.csv(file = paste0(db, "/GLA_experiment_groups.csv"))

  ## Read in primary data from brood records
  nest_data <- readxl::read_xlsx(path = paste0(db, "/GLA_PrimaryData_Nest.xlsx"),
                                 guess_max = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename important variables
    dplyr::mutate(BreedingSeason = as.integer(.data$Yr),
                  LocationID = as.character(.data$NestboxNumber),
                  Species = as.character(.data$Species),
                  PopID = as.character(.data$Site)) %>%

    ## Adjust dates
    dplyr::rowwise() %>%

    ## There are two formats for dates.
    ## The first can be handled with Lubridate, the second needs to be rearranged
    dplyr::mutate(dplyr::across(.cols = c(.data$FirstEggDate,
                                          .data$LayingComplete,
                                          .data$ObservedHatch),
                                .fns = ~suppressWarnings(dplyr::case_when(grepl("/", .) ~ lubridate::dmy(., quiet = TRUE),
                                                                          TRUE ~ lubridate::ymd(paste(unlist(stringr::str_split(as.character(janitor::excel_numeric_to_date(as.numeric(.))),
                                                                                                                                pattern = "-"))[c(1,3,2)],
                                                                                                      collapse = "-"),
                                                                                                quiet = TRUE))))) %>%
    dplyr::ungroup() %>%

    dplyr::mutate(Hatchlings = as.integer(.data$Hatchlings),
                  Fledglings = as.integer(.data$Fledglings),
                  MaleRing = as.character(.data$MaleRing),
                  FemaleRing = as.character(.data$FemaleRing),
                  ClutchSize = as.integer(.data$ClutchSize)) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$LocationID, .data$FirstEggDate) %>%

    ## Create additional variables that will be used in multiple data tables
    dplyr::mutate(ClutchSize_max = dplyr::case_when(.data$ClutchComplete == "TRUE" ~ as.numeric(.data$ClutchSize),
                                                    .data$ClutchComplete ==  "FALSE" ~ Inf),
                  Species = dplyr::case_when(.data$Species == "greti" ~ species_codes[species_codes$speciesEURINGCode == 14640,]$Species,
                                             .data$Species == "bluti" ~ species_codes[species_codes$speciesEURINGCode == 14620,]$Species,
                                             .data$Species == "piefl" ~ species_codes[species_codes$speciesEURINGCode == 13490,]$Species,
                                             .data$Species == "nutha" ~ species_codes[species_codes$speciesEURINGCode == 14790,]$Species,
                                             .data$Species == "tresp" ~ species_codes[species_codes$speciesEURINGCode == 15980,]$Species),
                  BroodSize_observed = .data$ClutchSize - as.integer(.data$UnhatchedEggs),
                  PopID = dplyr::case_when(.data$PopID == "cashel" ~ "CAS",
                                           .data$PopID == "garscube" ~ "GAR",
                                           .data$PopID == "kelvingrove_park" ~ "KEL",
                                           .data$PopID == "sallochy" ~ "SAL",
                                           .data$PopID == "SCENE" ~ "SCE",
                                           TRUE ~ as.character(.data$PopID)),
                  LayDate_observed = .data$FirstEggDate) %>%

    ## Rename
    dplyr::rename("FemaleID" = "FemaleRing",
                  "MaleID" = "MaleRing",
                  "ClutchSize_observed" = "ClutchSize",
                  "HatchDate_observed" = "ObservedHatch",
                  "NumberFledged_observed" = "Fledglings") %>%

    ## Create BroodID based on PopID and row number
    dplyr::mutate(BroodID = dplyr::case_when(!is.na(.data$Species) ~ paste(.data$PopID, dplyr::row_number(), sep ="-"))) %>%

    ## Join experiment labels
    dplyr::left_join(expID_tab, by = c("Experiment", "Treatment")) %>%

    ## Select variables of interest
    dplyr::select("BreedingSeason",
                  "PopID",
                  "LocationID",
                  "Species",
                  "ReplacementClutch",
                  "LayDate_observed",
                  "HatchDate_observed",
                  "ClutchSize_observed",
                  "UnhatchedEggs",
                  "BroodSize_observed",
                  "NumberFledged_observed",
                  "FemaleID",
                  "MaleID",
                  "BroodID",
                  "ExperimentID")

  ## Read in primary data from ringing records
  rr_data <- readxl::read_xlsx(path = paste0(db, "/GLA_PrimaryData_RingingRecords.xlsx"),
                               col_types = c(rep("text",7),
                                             "date","date",
                                             rep("text",31))) %>%

    ## Reformat
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Remove unnecessary variables
    dplyr::select(-c("Id",
                     "Fat",
                     "ColourLeftUp",
                     "ColourLeftDown",
                     "ColourRightUp",
                     "ColourRightDown")) %>%

    ## Rename variables
    dplyr::rename("BreedingSeason" = "Yr",
                  "PopID" = "Site",
                  "IndvID" = "RingNumber",
                  "LocationID" = "NestboxNumber",
                  "CaptureDate" = "Date",
                  "CaptureTime" = "Time",
                  "ObserverID" = "Initial",
                  "Mass" = "Weight",
                  "WingLength" = "Wing",
                  "Sex_observed" = "Sex") %>%

    ## Reformat variables
    dplyr::mutate(CaptureDate = lubridate::ymd(.data$CaptureDate),
                  CaptureTime = dplyr::case_when(format(.data$CaptureTime, format = "%H:%M:%S") == "00:00:00" ~ NA_character_,
                                                 TRUE ~ format(.data$CaptureTime, format = "%H:%M:%S")),
                  Mass = as.numeric(.data$Mass),
                  Tarsus = as.numeric(.data$Tarsus),
                  WingLength = as.numeric(.data$WingLength),
                  ChickAge = suppressWarnings(as.integer(.data$ChickAge)), # Chick ages that are 11+12 are treated as NA - these would not be used for calculating measurements anyways
                  Species = dplyr::case_when(.data$Species == "greti" ~ species_codes[species_codes$speciesEURINGCode == 14640,]$Species,
                                             .data$Species == "bluti" ~ species_codes[species_codes$speciesEURINGCode == 14620,]$Species,
                                             .data$Species == "piefl" ~ species_codes[species_codes$speciesEURINGCode == 13490,]$Species,
                                             .data$Species == "nutha" ~ species_codes[species_codes$speciesEURINGCode == 14790,]$Species,
                                             .data$Species == "tresp" ~ species_codes[species_codes$speciesEURINGCode == 15980,]$Species),

                  ## 3J is the same age as 3 - just means it likely hatched at that site. It is now a retired BTO age code.
                  Age_observed = dplyr::case_when(.data$Age == "X" ~ 1L,
                                                  .data$Age == "3J" ~ 3L,
                                                  TRUE ~ suppressWarnings(as.integer(.data$Age))),

                  ## Recode sex
                  Sex_observed = dplyr::case_when(grepl(pattern = "F", .data$Sex_observed) ~ "F",
                                                  grepl(pattern = "M", .data$Sex_observed) ~ "M"),

                  BreedingSeason = as.integer(.data$BreedingSeason))  %>%

    ## Adjust species names and population names
    dplyr::mutate(PopID = dplyr::case_when(.data$PopID == "cashel" ~ "CAS",
                                           .data$PopID == "garscube" ~ "GAR",
                                           .data$PopID == "kelvingrove_park" ~ "KEL",
                                           .data$PopID == "sallochy" ~ "SAL",
                                           .data$PopID == "SCENE" ~ "SCE",
                                           TRUE ~ .data$PopID))


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    rr_data <- rr_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    rr_data <- rr_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_GLA(nest_data, rr_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_GLA(nest_data, rr_data, Brood_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_GLA(Capture_data, Brood_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_GLA(nest_data, rr_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_GLA.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_GLA.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_GLA.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_GLA.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_GLA.txt"),
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

#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~


#' Create brood data table for great tits and blue tits in Glasgow, Scotland.
#'
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @return A data frame.

create_brood_GLA <- function(nest_data, rr_data) {

  ## Get brood data from ringing records
  rr_data_brood_sum <- rr_data %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Determine whether a chick or adult
    dplyr::mutate(RingAge = ifelse(.data$Age_observed == 1, "chick", "adult")) %>%

    ## Only keeping chicks to calculate brood information
    dplyr::filter(.data$RingAge == "chick") %>%

    ## Summarize brood information for each nest
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$Species, .data$LocationID) %>%

    ## If any chicks reach the age where they can be ringed at a nest box, that nest box will not be used again in the breeding season
    ## As such, for each location with chicks, there is only going to be one FemaleID and one MaleID
    ## In one case (2020 - SCE - CYACAE - 107), the mother ID is not assigned for all chicks, but is set to NA for one chick
    ## We will be conservative and assign any broods with multiple FemaleIDs or MaleIDs as NA
    dplyr::reframe(FemaleID = dplyr::case_when(dplyr::n_distinct(.data$MotherRing) > 1 ~ NA_character_,
                                               TRUE ~ unique(.data$MotherRing)),
                   MaleID = dplyr::case_when(dplyr::n_distinct(.data$FatherRing) > 1 ~ NA_character_,
                                             TRUE ~ unique(.data$FatherRing)),
                   AvgChickMass = round(mean(.data$Mass[.data$ChickAge <= 16L & .data$ChickAge >= 14L], na.rm = TRUE),1),
                   NumberChicksMass = sum(.data$ChickAge <= 16L & .data$ChickAge >= 14L & is.na(.data$Mass) == F),
                   AvgTarsus = round(mean(.data$Tarsus[.data$ChickAge <= 16L & .data$ChickAge >= 14L ], na.rm = TRUE),1),
                   NumberChicksTarsus = sum(.data$ChickAge <= 16L & .data$ChickAge >= 14L & is.na(.data$Tarsus) == FALSE)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$Species, .data$LocationID) %>%

    ## Replace NaNs and 0 with NA
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~dplyr::na_if(., NaN)),
                  dplyr::across(tidyselect::where(is.numeric), ~dplyr::na_if(., 0)))


  ## Get brood data from nest records
  nest_data_brood_sum <-  nest_data %>%
    dplyr::mutate(ClutchType_observed = dplyr::case_when(is.na(.data$ReplacementClutch) ~ NA_character_,
                                                         .data$ReplacementClutch == 0L ~ "first",
                                                         .data$ReplacementClutch == 1L ~ "replacement",
                                                         .data$ReplacementClutch  == 2L ~ NA_character_))

  ## Join brood data from ringing records to brood data from nest records
  Brood_data <- nest_data_brood_sum %>%
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$Species, .data$LocationID) %>%
    dplyr::mutate(last_rec = dplyr::case_when(.data$BroodID == max(.data$BroodID) ~ "yes",
                                              TRUE ~ "no")) %>%

    ## Only joining information from ringing records to last nest record from the nest in the year
    ## This will avoid any possible cases of joining information on ringed and measured chicks to cases where the nest attempt failed during chick rearing
    dplyr::left_join(rr_data_brood_sum %>%
                       dplyr::mutate(chicks_fledged = dplyr::case_when(!is.na(.data$NumberChicksMass) | !is.na(.data$NumberChicksTarsus) ~ "yes",
                                                                        TRUE ~ "no")),
                     by = c("BreedingSeason", "PopID", "LocationID")) %>%
    dplyr::select(-"chicks_fledged", -"last_rec") %>%

    ## Merge Male and Female ID columns to fill in any that are missing
    dplyr::mutate(MaleID_j = dplyr::case_when(.data$MaleID.x == .data$MaleID.y ~ .data$MaleID.x,
                                              is.na(.data$MaleID.x) & !is.na(.data$MaleID.y) ~ .data$MaleID.y,
                                              !is.na(.data$MaleID.x) & is.na(.data$MaleID.y) ~ .data$MaleID.x),
                  FemaleID_j = dplyr::case_when(.data$FemaleID.x == .data$FemaleID.y ~ .data$FemaleID.x,
                                                is.na(.data$FemaleID.x) & !is.na(.data$FemaleID.y) ~ .data$FemaleID.y,
                                                !is.na(.data$FemaleID.x) & is.na(.data$FemaleID.y) ~ .data$FemaleID.x)) %>%

    ## Merge Species information from nest and brood records, use species information from ringing data when in conflict with brood data
    dplyr::mutate(Species_j = dplyr::case_when(.data$Species.x == .data$Species.y ~ .data$Species.x,
                                               is.na(.data$Species.y) ~ .data$Species.x,
                                               .data$Species.x != .data$Species.y & !is.na(.data$Species.y) ~ .data$Species.y)) %>%

    ## Remove extra ID columns
    dplyr::select(-dplyr::contains(c(".x",".y"))) %>%
    dplyr::rename("MaleID" = "MaleID_j",
                  "FemaleID" = "FemaleID_j",
                  "Species" = "Species_j") %>%

  ## If FemaleID or MaleID differs from expected format, set to NA
  dplyr::mutate(dplyr::across(.cols = c(.data$FemaleID,
                                        .data$MaleID),
                              .fns = ~dplyr::case_when(stringr::str_detect(., "^[[:digit:][:alpha:]]{7}$") ~ .,
                                                       TRUE ~ NA_character_))) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0, !(names(brood_data_template) %in% names(.))]   %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%

    ## Remove any NAs from essential columns
    dplyr::filter(!is.na(.data$BroodID),
                  !is.na(.data$PopID),
                  !is.na(.data$BreedingSeason),
                  !is.na(.data$Species)) %>%

    ## Calculate clutch type
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE))

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Glasgow, Scotland.
#'
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @param Brood_data Data frame of Brood data in standard format from Glasgow, Scotland.
#'
#' @return A data frame.

create_capture_GLA <- function(nest_data, rr_data, Brood_data) {

  ## Capture data from ringing records
  ## TODO: Check on tarsus method
  Capture_data_rr <- rr_data %>%
    dplyr::filter(!(.data$IndvID %in% c("too small", "too_small", "no_rings_COVID","unknown"))) %>% # Keep only records of banded individuals

    ## Join Experiment info for chicks
    dplyr::filter(.data$Age == 1L) %>%

    ## Keep only records with manipulations since these are the ones we will want to join based on
    dplyr::left_join(Brood_data %>%
                       dplyr::filter(!is.na(.data$ExperimentID)) %>%
                       dplyr::select("BreedingSeason",
                                     "PopID",
                                     "LocationID",
                                     "ExperimentID"),
                     by = c("BreedingSeason", "PopID","LocationID")) %>%

    ## Add in adults. Create ExperimentID column separately for this group if radio tagged
    dplyr::bind_rows(rr_data %>%
                       dplyr::filter(!(.data$IndvID %in% c("too small", "too_small", "no_rings_COVID","unknown"))) %>% # Keep only records of banded individuals
                       dplyr::filter(.data$Age != 1L | is.na(.data$Age)) %>%
                       dplyr::mutate(ExperimentID = dplyr::case_when(.data$RadioTagFitted == TRUE | .data$RfidFitted == TRUE ~ "SURVIVAL"))) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$PopID, .data$IndvID, .data$CaptureDate) %>%

    ## Add additional data
    dplyr::mutate(CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  CaptureAlive = dplyr::case_when(.data$Retrap == "X" ~ FALSE,
                                                  .data$Retrap %in% c("N", "R", "C", "U", NA) ~ TRUE), ## Set CaptureAlive to FALSE if Retrap is X, otherwise TRUE
                  ReleaseAlive = dplyr::case_when(.data$Retrap == "X" | .data$Age == "X" ~ FALSE,
                                                  .data$Retrap %in% c("N", "R", "C", "U", NA) ~ TRUE), ## Set ReleaseAlive to FALSE if Retrap is X or if Age is X (chick found dead at nest)
                  ReleasePopID = dplyr::case_when(.data$ReleaseAlive == FALSE ~ NA_character_,
                                                  TRUE ~ as.character(.data$CapturePopID))) %>%  ## Set ReleasePopID to NA if ReleaseAlive is FALSE, otherwise same as CapturePopID

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))]   %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))


  ## Create capture data from nest data.
  Capture_data_nest <-
    nest_data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"),
                        names_to = "Sex_observed",
                        values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Recode sexes
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                                  grepl("Male", .data$Sex_observed) ~ "M"),
                  CapturePopID = .data$PopID,
                  CaptureDate = dplyr::case_when(is.na(.data$LayDate_observed) ~ as.Date(paste0(.data$BreedingSeason, "-05-15")),
                                                 !is.na(.data$LayDate_observed) ~ .data$LayDate_observed,
                                                 TRUE ~ as.Date(NA_character_)),
                  CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  ReleasePopID = .data$PopID, ## Set ReleasePopID
                  CaptureAlive = TRUE, ## Set CaptureAlive to T
                  ReleaseAlive = TRUE) %>% ## Set ReleaseAlive to T

    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))]   %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))


  ## Get records of individuals that were reported in the nest data, but not in the ringing records
  brood_recs_unique <- dplyr::anti_join(Capture_data_nest, Capture_data_rr,
                                        by = c("BreedingSeason", "CapturePopID", "IndvID","LocationID"))

  ## Combine captures and add additional information
  Capture_data <- Capture_data_rr %>%
    dplyr::bind_rows(brood_recs_unique) %>%

    ## Filter out incorrect IDs
    dplyr::mutate(IndvID = dplyr::case_when(stringr::str_detect(.data$IndvID, "^[[:digit:][:alpha:]]{7}$") ~ .data$IndvID,
                                   TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%

    ##  Change column class
    dplyr::mutate(BreedingSeason = as.integer(.data$BreedingSeason)) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%

    ## Calculate age
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$CapturePopID, .data$IndvID, .data$CaptureDate) %>%

    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"))  ## Create CaptureID based on IndvID and the record number

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Glasgow, Scotland.
#'
#' @param Capture_data Capture data output from Glasgow, Scotland
#'
#' @param Brood_data Brood data output from Glasgow, Scotland
#'
#' @return A data frame.

create_individual_GLA <- function(Capture_data, Brood_data){

  Individual_data_temp <- Capture_data %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(RingSeason = min(.data$BreedingSeason, na.rm = T),
                  PopID = .data$CapturePopID) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    ## Determine individual info
    dplyr::mutate(Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                  .f = ~{
                                                    if(length(..1) == 0){
                                                      return(NA_character_)
                                                    } else if(length(..1) == 1){
                                                      return(..1)
                                                    } else {
                                                      return("C")
                                                    }
                                                  }),
                  Sex_genetic = NA_character_,
                  Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                           .f = ~{
                                             if(length(..1) == 0){
                                               return(NA_character_)
                                             } else if(length(..1) == 1){
                                               return(..1)
                                             } else {
                                               return("CCCCCC")
                                             }
                                           }),

                  RingAge = purrr::pmap_chr(.l = list(dplyr::first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            }))

  ## Get chicks and join BroodID to these records
  ## A small number of duplicates (~10) are created when there are replacement clutches.
  ## To avoid this, only the last brood record for each LocationID is kept which will be the brood that may result in fledglings since there are no double broods in these populations.
  Individual_data <- Individual_data_temp %>%

    ## Filter to keep only records of individuals banded as chicks, the first record of that individual, and records where LocationID is known
    dplyr::filter(.data$RingAge == "chick" & .data$RingSeason == .data$BreedingSeason & !is.na(.data$LocationID)) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::filter(!is.na(.data$LocationID)) %>%
                       dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
                       dplyr::filter(.data$BroodID == dplyr::last(.data$BroodID)) %>%
                       dplyr::select("BreedingSeason", "PopID", "LocationID", "BroodID") %>%
                       dplyr::rename(BroodIDLaid = .data$BroodID),
                     by = c("BreedingSeason", "PopID", "LocationID")) %>%

    ## Add back in filtered records
    dplyr::bind_rows(Individual_data_temp %>%
                       dplyr::filter(.data$RingAge == "adult" | is.na(.data$LocationID) | (.data$RingSeason != .data$BreedingSeason))) %>%

    ## Add BroodID information
    ## BroodIDFledged is the same as BroodIDLaid unless ExperimentID is COHORT or PARENTAGE
    ## In these cases, the origin of eggs/nestlings is unknown and so the BroodIDLaid is unknown
    ## BroodIDFledged can still be determined based on the BroodID of the nest
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(BroodIDFledged = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodIDLaid))),
                                                  .f = ~{
                                                    if(length(..1) == 0){
                                                      return(NA_character_)
                                                    } else if(length(..1) == 1){
                                                      return(..1)
                                                    }
                                                  }),
                  BroodIDLaid = dplyr::case_when(grepl("COHORT|PARENTAGE", .data$ExperimentID) ~ NA_character_,
                                                 TRUE ~ .data$BroodIDLaid)) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0 ,!(names(individual_data_template) %in% names(.))] %>%
                                                dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Glasgow, Scotland.
#'
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @return A data frame.

create_location_GLA <- function(nest_data, rr_data) {

  ## Build location data based on ringing recovery data first
  ## Then join nest data
  ## No nest boxes have been removed
  Location_data <- rr_data %>%
    dplyr::select("BreedingSeason", "PopID", "LocationID") %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Add in locations from nest data
    dplyr::bind_rows(nest_data %>%
                       dplyr::select("BreedingSeason", "PopID", "LocationID") %>%
                       dplyr::filter(!is.na(.data$LocationID))) %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$BreedingSeason, .data$LocationID, .keep_all = TRUE) %>%

    ## All records shows be complete: remove any incomplete cases
    tidyr::drop_na() %>%

    ## Get additional information
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(NestboxID = .data$LocationID,
                  LocationType = "NB",
                  HabitatType = dplyr::case_when(.data$PopID == "GAR" ~ "urban",
                                                 .data$PopID == "CAS" ~ "deciduous",
                                                 .data$PopID == "KEL" ~ "urban",
                                                 .data$PopID == "SCE" ~ "deciduous",
                                                 .data$PopID == "SAL" ~ "deciduous"),
                  Latitude  = dplyr::case_when(.data$PopID == "GAR" ~ 55.9048,
                                               .data$PopID == "CAS" ~ 56.10888,
                                               .data$PopID == "KEL" ~ 55.8692216,
                                               .data$PopID == "SCE" ~ 56.1291,
                                               .data$PopID == "SAL" ~ 56.1232),
                  Longitude = dplyr::case_when(.data$PopID == "GAR" ~ -4.3199,
                                               .data$PopID == "CAS" ~ -4.57823,
                                               .data$PopID == "KEL" ~ -4.2818993,
                                               .data$PopID == "SCE" ~ -4.61478,
                                               .data$PopID == "SAL" ~ -4.5993)) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[0, !(names(location_data_template) %in% names(.))]  %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)

  return(Location_data)

}
