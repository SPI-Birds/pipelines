#'Construct standard format for data from Glasgow, Scotland
#'
#'A pipeline to produce the standard format for the nest box population in Glasgow, Scotland, administered by Davide Dominoni.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Primarily great tits and blue tits, but a small number of records of Eurasian tree sparrows, pied flycatchers, and Eurasian nuthatch.
#'In the Brood table, four records
#'
#'\strong{IndvID}: Should be a 7 alphanumeric character string.
#'Three records have ring numbers that are six characters and these are probably incorrect.
#'
#'
#'\strong{CaptureDate}: Some individuals were not recorded in the ringing records, but were observed breeding at a monitored nest.
#'For these individuals, the CaptureDate is set as June 1st of the breeding year.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_GLA <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

  #### Force user to select directory
  force(db)

  #### Determine species and population codes for filtering
  if(is.null(species)){

    species_filter <- NULL

  } else {

    species_filter <- species

  }

  ## Going to only keep 5 main populations, so these will be the default filter ("CAS", "GAR", "SAL","KEL", "SCE")
  ## Otherwise, use the specified pop filter
  if(is.null(pop)){

    pop_filter <- c("CAS", "GAR", "SAL","KEL", "SCE")


  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE)

  ## Read in primary data from brood records
  ## Some dates are inverted (these are the dates that are stored as Excel dates in the primary data)
  ## To fix these dates, the day and month needs to be changed for all Excel dates. Currently rather slow
  nest_data <- readxl::read_xlsx(path = paste0(db, "/GLA_PrimaryData_Nest.xlsx"), guess = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename important variables
    dplyr::mutate(BreedingSeason = as.integer(.data$Yr),
                  LocationID = as.character(.data$NestboxNumber),
                  Species = as.character(.data$Species),
                  PopID = as.character(.data$Site)) %>%

    ## Rowwise, adjust dates
    dplyr::rowwise() %>%

    ## There are two formats for dates
    ## First can be handled with Lubridate. Lubridate gives warning messages for some dates , but these do get parsed
    ## Some dates, however, are in the incorrect format (month - day - year) and these need to be rearranged
    dplyr::mutate(FirstEggDate = case_when(grepl("/", .data$FirstEggDate) ~ lubridate::dmy(.data$FirstEggDate, quiet = TRUE),
                                           TRUE ~ lubridate::ymd(paste(unlist(stringr::str_split(as.character(janitor::excel_numeric_to_date(as.numeric(.data$FirstEggDate))), pattern = "-"))[c(1,3,2)], collapse = "-"))),

                  LayingComplete = case_when(grepl("/", .data$LayingComplete) ~ lubridate::dmy(.data$LayingComplete, quiet = TRUE),
                                             TRUE ~ lubridate::ymd(paste(unlist(stringr::str_split(as.character(janitor::excel_numeric_to_date(as.numeric(.data$LayingComplete))), pattern = "-"))[c(1,3,2)], collapse = "-"))),

                  ObservedHatch = case_when(grepl("/", .data$ObservedHatch) ~ lubridate::dmy(.data$ObservedHatch, quiet = TRUE),
                                            TRUE ~ lubridate::ymd(paste(unlist(stringr::str_split(as.character(janitor::excel_numeric_to_date(as.numeric(.data$ObservedHatch))), pattern = "-"))[c(1,3,2)], collapse = "-"))),

                  Hatchlings = as.integer(.data$Hatchlings),
                  Fledglings = as.integer(.data$Fledglings),
                  MaleRing = as.character(.data$MaleRing),
                  FemaleRing = as.character(.data$FemaleRing),
                  ClutchSize = as.integer(.data$ClutchSize)) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, dplyr::desc(.data$BreedingSeason), .data$LocationID, .data$FirstEggDate) %>%

    ## Create additional variables that will be used in multiple data tables
    dplyr::mutate(ClutchSize_max = dplyr::case_when(.data$ClutchComplete == "TRUE" ~ as.numeric(.data$ClutchSize),
                                                    .data$ClutchComplete ==  "FALSE" ~ Inf),
                  Species = dplyr::case_when(.data$Species == "greti" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == "bluti" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$Species == "piefl" ~ species_codes[species_codes$SpeciesID == 13490, ]$Species,
                                             .data$Species == "nutha" ~ species_codes[species_codes$SpeciesID == 14790, ]$Species,
                                             .data$Species == "tresp" ~ species_codes[species_codes$SpeciesID == 15980, ]$Species),
                  BroodSize_observed = .data$ClutchSize - as.integer(.data$UnhatchedEggs),
                  PopID = dplyr::case_when(.data$PopID == "cashel" ~ "CAS",
                                           .data$PopID == "garscube" ~ "GAR",
                                           .data$PopID == "kelvingrove_park" ~ "KEL",
                                           .data$PopID == "sallochy" ~ "SAL",
                                           .data$PopID == "SCENE" ~ "SCE",
                                           TRUE ~ as.character(.data$PopID)),
                  LayDate_observed = .data$FirstEggDate) %>%

    ## Select variables of interest
    dplyr::select(.data$BreedingSeason, .data$LocationID, .data$PopID, .data$ReplacementClutch,
                  .data$Experiment, .data$Treatment, .data$Species,
                  .data$LayDate_observed, .data$FirstEggDate, .data$LayingComplete, .data$ObservedHatch,
                  .data$ClutchSize, .data$HatchlingsManip, .data$ClutchComplete, .data$UnhatchedEggs,
                  .data$Fledglings, .data$MaleRing, .data$FemaleRing) %>%

    ## Rename
    dplyr::rename(FemaleID = .data$FemaleRing,
                  MaleID = .data$MaleRing,
                  LayDate_min = .data$FirstEggDate,
                  LayDate_max = .data$LayingComplete,
                  ClutchSize_observed = .data$ClutchSize,
                  HatchDate_observed = .data$ObservedHatch,
                  NumberFledged_observed = .data$Fledglings) %>%


    ## Create BroodID based on PopID and row number
    dplyr::ungroup() %>%
    dplyr::mutate(BroodID = dplyr::case_when(!is.na(.data$Species) ~ paste(.data$PopID, dplyr::row_number(), sep ="-")))


  ## Read in primary data from ringing records
  rr_data <- readxl::read_xlsx(path = paste0(db, "/GLA_PrimaryData_RingingRecords.xlsx"),
                               col_types = c(rep("text",7),
                                             "date","date",
                                             rep("text",31))) %>%

    ## Reformat
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Remove unnecessary variables
    dplyr::select(-c(.data$Id,
                     .data$Fat,
                     .data$ColourLeftUp,
                     .data$ColourLeftDown,
                     .data$ColourRightUp,
                     .data$ColourRightDown)) %>%

    ## Rename variables
    dplyr::rename(BreedingSeason = .data$Yr,
                  PopID = .data$Site,
                  IndvID = .data$RingNumber,
                  LocationID = .data$NestboxNumber,
                  CaptureDate = .data$Date,
                  CaptureTime = .data$Time,
                  ObserverID = .data$Initial,
                  Mass = .data$Weight,
                  WingLength = .data$Wing,
                  Sex_observed = .data$Sex) %>%

    ## Reformat variables
    ## TODO: check about times
    dplyr::mutate(CaptureDate = lubridate::ymd(.data$CaptureDate),
                  CaptureTime = dplyr::case_when(format(.data$CaptureTime, format = "%H:%M:%S") == "00:00:00" ~ NA_character_,
                                                 TRUE ~ format(.data$CaptureTime, format = "%H:%M:%S")),
                  Mass = as.numeric(.data$Mass),
                  Tarsus = as.numeric(.data$Tarsus),
                  WingLength = as.numeric(.data$WingLength),
                  ChickAge = as.integer(.data$ChickAge), # Small number of records entered as 11+12. These becomes NA
                  Species = dplyr::case_when(.data$Species == "bluti" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$Species == "greti" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species),

                  ## TODO: check about meaning of 3J
                  Age_observed = dplyr::case_when(.data$Age == "X" ~ 1L,
                                                  .data$Age == "3J" ~ 3L,
                                                  TRUE ~ as.integer(.data$Age)),
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
  Capture_data <- create_capture_GLA(nest_data, rr_data)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_GLA.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_GLA.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_GLA.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_GLA.csv"), row.names = F)

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

#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~


#' Create brood data table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create brood data table in standard format for Glasgow, Scotland.
#'
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @return A data frame.

create_brood_GLA <- function(nest_data, rr_data) {

  ## Get brood data from ringing records
  rr_data_brood_sum <- rr_data %>%
    dplyr::filter(is.na(.data$LocationID) == FALSE) %>%

    ## Determine whether a chick or adult - done for each observation since there is no unique identifier for individuals
    dplyr::mutate(RingAge = ifelse(.data$Age_observed == 1, "chick", "adult")) %>%

    ## Keeping chicks
    dplyr::filter(RingAge == "chick") %>%

    ## Summarize brood information for each nest
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%

    dplyr::summarise(Species = names(which.max(table(.data$Species, useNA = "always"))),
                     FemaleID = names(which.max(table(.data$MotherRing, useNA = "always"))),
                     MaleID = names(which.max(table(.data$FatherRing, useNA = "always"))),
                     AvgChickMass = round(mean(Mass[ChickAge <= 16L & ChickAge >= 14L], na.rm = TRUE),1),
                     NumberChicksMass = sum(ChickAge <= 16L & ChickAge >= 14L & is.na(Mass) == F),
                     AvgTarsus = round(mean(Tarsus[ChickAge <= 16L & ChickAge >= 14L ], na.rm = TRUE),1),
                     NumberChicksTarsus = sum(ChickAge <= 16L & ChickAge >= 14L & is.na(Tarsus) == F)) %>%

    ## Replace NaNs and 0 with NA
    ## TODO: Apparently there is some weirdness with 'where' not being exported by any package and utils::globalVariables("where") needs to be called somewhere
    dplyr::mutate(dplyr::across(where(is.numeric), ~na_if(., "NaN")),
                  dplyr::across(where(is.numeric), ~na_if(., 0)))


  ## Get brood data from nest records
  nest_data_brood_sum <-
    nest_data %>%

    ## TODO: Check on experiments and meaning of replacement clutch
    dplyr::mutate(ExperimentID = dplyr::case_when(!is.na(.data$Experiment) ~
                                                    "COHORT; PARENTAGE"),
                  ClutchType_observed = dplyr::case_when(is.na(.data$ReplacementClutch) ~ NA_character_,
                                                         .data$ReplacementClutch == 0L ~ "first",
                                                         .data$ReplacementClutch == 1L ~ "replacement",
                                                         .data$ReplacementClutch  == 2L ~ NA_character_))


  ## Join brood data from ringing records to brood data from nest records
  ## TODO: Check about determining species - there are cases (4 as of 2021) where nest and ringing data suggest different social parents
  ## TODO: Add experiment information
  Brood_data <- nest_data_brood_sum %>%
    dplyr::left_join(rr_data_brood_sum %>%
                       select(-.data$Species),
                     by = c("BreedingSeason", "PopID", "LocationID")) %>%

    ## Join Male and Female ID columns to fill in any that are missing
    dplyr::mutate(MaleID_j = dplyr::case_when(.data$MaleID.x == .data$MaleID.y ~ .data$MaleID.x,
                                              is.na(.data$MaleID.x) & !is.na(.data$MaleID.y) ~ .data$MaleID.y,
                                              !is.na(.data$MaleID.x) & is.na(.data$MaleID.y) ~ .data$MaleID.x),
                  FemaleID_j = dplyr::case_when(.data$FemaleID.x == .data$FemaleID.y ~ .data$FemaleID.x,
                                                is.na(.data$FemaleID.x) & !is.na(.data$FemaleID.y) ~ .data$FemaleID.y,
                                                !is.na(.data$FemaleID.x) & is.na(.data$FemaleID.y) ~ .data$FemaleID.x)) %>%

    ## Remove extra ID columns
    dplyr::select(-(dplyr::contains(c(".x",".y")))) %>%

    dplyr::rename(MaleID = .data$MaleID_j,
                  FemaleID = .data$FemaleID_j) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%

    ## Remove any NAs from essential columns
    dplyr::filter(!is.na(.data$BroodID),
           !is.na(.data$PopID),
           !is.na(.data$BreedingSeason),
           !is.na(.data$Species)) %>%

    ## Calculate clutch type
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Adjust column classes as necessary
    dplyr::mutate(BroodID = as.character(.data$BroodID))

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create a capture data table in standard format for great tits and blue tits in Glasgow, Scotland.
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @return A data frame.

create_capture_GLA <- function(nest_data, rr_data) {

  ## Capture data from ringing records
  ## TODO: Check on tarsus method and age codes
  Capture_data_rr <- rr_data %>%
    dplyr::filter(!(.data$IndvID %in% c("too small", "too_small", "no_rings_COVID","unknown"))) %>% # Keep only records of banded individuals

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$PopID, .data$IndvID, .data$CaptureDate) %>%

    ## Add additional data
    dplyr::mutate(CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  CaptureAlive = dplyr::case_when(.data$Retrap == "X" ~ FALSE,
                                                  .data$Retrap %in% c("N", "R", "C", "U", NA) ~ TRUE), ## Set CaptureAlive to FALSE if Retrap is X, otherwise TRUE
                  ReleaseAlive = dplyr::case_when(.data$Retrap == "X" | .data$Age == "X" ~ FALSE,
                                                  .data$Retrap %in% c("N", "R", "C", "U", NA) ~ TRUE), ## Set ReleaseAlive to FALSE if Retrap is X and if Age is X (chick found dead at nest)
                  ReleasePopID = dplyr::case_when(.data$ReleaseAlive == FALSE ~ NA_character_,
                                                  TRUE ~ as.character(.data$CapturePopID)), ## Set ReleasePopID to NA if ReleaseAlive is FALSE, otherwise same as CapturePopID
                  ExperimentID = dplyr::case_when(.data$RadioTagFitted == TRUE | .data$RfidFitted == TRUE ~ "SURVIVAL")) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))


  ## Create capture data from primary data on brood. This will mostly be used to get information about experiments and perhaps some resightings.
  ## TODO: Look into experimental groups
  Capture_data_nest <-
    nest_data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols=c("FemaleID","MaleID"), names_to = "Sex_observed", values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Recode sexes
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                                  grepl("Male", .data$Sex_observed) ~ "M")) %>%

    dplyr::group_by(.data$PopID) %>%
    dplyr::mutate(CapturePopID = .data$PopID) %>%

    ## TODO: Check on Capture Date
    dplyr::mutate(CaptureDate = case_when(is.na(.data$LayDate_observed) ~ as.Date(paste0(.data$BreedingSeason, "-06-01")),
                                          !is.na(.data$LayDate_observed) ~ .data$LayDate_observed,
                                          TRUE ~ NA_Date_),
                  CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  ReleasePopID = .data$PopID, ## Set ReleasePopID
                  CaptureAlive = TRUE, ## Set CaptureAlive to T
                  ReleaseAlive = TRUE, ## Set ReleaseAlive to T
                  ExperimentID = dplyr::case_when(!is.na(.data$Experiment) ~
                                                    "COHORT; PARENTAGE")) %>%   ## TODO: Check about experimental types

    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))


  ## Get records of individuals that were reported in the nest data, but not in the ringing records
  brood_recs_unique <- dplyr::anti_join(Capture_data_nest, Capture_data_rr,
                                        by = c("BreedingSeason", "CapturePopID", "IndvID","LocationID"))

  ## Combine captures and add additional information
  Capture_data <- Capture_data_rr %>%
    dplyr::bind_rows(brood_recs_unique) %>%

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
#' Create full individual data table in standard format for great tits and blue tits in Glasgow, Scotland.
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

    dplyr::mutate(PopID = .data$CapturePopID,
                  RingSeason = min(.data$BreedingSeason)) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

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

                  RingAge = purrr::pmap_chr(.l = list(first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")  # TODO: If age observed is unknown, assuming adult. Check this assumption
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            }))


  ## Get chicks and join BroodID to these records
  ## TODO: Duplicates are created due to two nests having replacement clutches.
  ## Temporary solution to avoid this is to keep the newest brood record for each LocationID since apparently there are no true second clutches in the data.
  ## This means that any banded chicks must come from the last brood.
  Individual_data <- Individual_data_temp %>%

    ## Filter to keep only records of individuals banded as chicks and the first record of that individual and records where LocationID is known
    dplyr::filter(.data$RingAge == "chick" & .data$RingSeason == .data$BreedingSeason & !is.na(.data$LocationID)) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::filter(!is.na(.data$LocationID)) %>%
                       dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
                       dplyr::filter(.data$BroodID == first(.data$BroodID)) %>%
                       dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID, .data$BroodID) %>%
                       dplyr::rename(BroodIDLaid = .data$BroodID),
                     by = c("BreedingSeason", "PopID", "LocationID")) %>%

    ## Add back in filtered records
    dplyr::bind_rows(Individual_data_temp %>%
                       dplyr::filter(.data$RingAge == "adult" | is.na(.data$LocationID) | (.data$RingSeason != .data$BreedingSeason))) %>%

    ## For each individual, check if there is BroodID information from an earlier capture
    ## TODO: Check that there is no cross fostering
    group_by(.data$IndvID) %>%
    dplyr::mutate(BroodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodIDLaid))),
                                                     .f = ~{
                                                       if(length(..1) == 0){
                                                         return(NA_character_)
                                                       } else if(length(..1) == 1){
                                                         return(..1)
                                                       } else {
                                                         return("CCCCCC")
                                                       }
                                                     }),
                BroodIDFledged = .data$BroodIDLaid) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[,!(names(individual_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create a location data table in standard format for great tits and blue tits in Glasgow, Scotland.
#' @param nest_data Data frame of nest data from Glasgow, Scotland.
#'
#' @param rr_data Data frame of ringing records from Glasgow, Scotland.
#'
#' @return A data frame.

create_location_GLA <- function(nest_data, rr_data) {

  ## Build location data based on ringing recovery data first
  ## Then join nest data
  ## TODO: Check whether any boxes have been removed
  Location_data <- rr_data %>%
    dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Add in locations from nest data
    dplyr::bind_rows(nest_data %>%
                       dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
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
    dplyr::bind_cols(location_data_template[,!(names(location_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)

  return(Location_data)

}