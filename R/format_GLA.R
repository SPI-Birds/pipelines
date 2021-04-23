#'Construct standard format for data from Glasgow, Scotland
#'
#'A pipeline to produce the standard format for the nest box population in Glasgow, Scotland, administered by Davide Dominoni.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}:
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

  #Assign to database location
  db <- paste0(db, "\\AMM_PrimaryData.accdb")

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

  ## Going to only keep 5 main populations (), so these will be the default filter ("CAS", "GAR", "SAL","KEL", "SCE")
  ## Otherwise, use the specified pop filter
  if(is.null(pop)){

    pop_filter <- c("CAS", "GAR", "SAL","KEL", "SCE")


  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  #### Primary data

  message("Importing primary data...")

  ## Read in primary data from brood records
  nest_data <- readr::read_csv(file = paste0("/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/GLA/GLA_PrimaryData.csv")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename important variables
    dplyr::mutate(BreedingSeason = as.integer(.data$Yr),
                  LocationID = as.character(.data$NestboxNumber),
                  Species = as.character(.data$Species),
                  PopID = as.character(.data$Site),
                  FirstEggDate = lubridate::dmy(.data$FirstEggDate),
                  LayDate_observed = .data$FirstEggDate,
                  LayingComplete = lubridate::dmy(.data$LayingComplete),
                  ObservedHatch = lubridate::dmy(.data$ObservedHatch),
                  Hatchlings = as.integer(.data$Hatchlings),
                  Fledglings = as.integer(.data$Fledglings),
                  MaleRing = as.character(.data$MaleRing),
                  FemaleRing = as.character(.data$FemaleRing),) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, dplyr::desc(.data$BreedingSeason), .data$LocationID, dplyr::desc(.data$FirstEggDate)) %>%

    ## Create BroodID
    dplyr::mutate(BroodID = row_number()) %>%

    ## Select variables of interest
    dplyr::select(.data$BreedingSeason, .data$LocationID, .data$PopID, .data$ReplacementClutch,
                  .data$Experiment, .data$Treatment, .data$Species, .data$LayDate_observed, .data$FirstEggDate,  .data$LayingComplete, .data$ExpectedHatch, .data$ObservedHatch,
                  .data$ClutchSize, .data$HatchlingsManip, .data$ClutchComplete, .data$UnhatchedEggs, .data$Fledglings, .data$MaleRing, .data$FemaleRing) %>%

    ## Create additional variables that will be used in multiple data tables
    dplyr::mutate(BroodID = 1:length(.data$PopID),
                  ClutchSize_max = dplyr::case_when(.data$ClutchComplete == 1 ~ .data$ClutchSize,
                                                    .data$ClutchComplete ==  0 ~ Inf),
                  Species = dplyr::case_when(.data$Species == "bluti" ~ 14620,
                                             .data$Species == "greti" ~ 14640),
                  BroodSize_observed = .data$ClutchSize - .data$UnhatchedEggs,
                  PopID = dplyr::case_when(.data$PopID == "cashel" ~ "CAS",
                                           .data$PopID == "garscube" ~ "GAR",
                                           .data$PopID == "kelvingrove_park" ~ "KEL",
                                           .data$PopID == "sallochy" ~ "SAL",
                                           .data$PopID == "SCENE" ~ "SCE")) %>%

    ## Rename
    dplyr::rename(FemaleID = .data$FemaleRing,
                  MaleID = .data$MaleRing,
                  LayDate_min = .data$FirstEggDate,
                  LayDate_max = .data$LayingComplete,
                  ClutchSize_observed = .data$ClutchSize,
                  HatchDate_observed = .data$ObservedHatch,
                  NumberFledged_observed = .data$Fledglings) %>%

    ## Adjust species names
    dplyr::mutate(Species = dplyr::case_when(.data$Species == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species))

  ## Read in primary data from ringing records
  rr_data <- readr::read_csv(file = paste0("/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/GLA/GLA_RingingRecords.csv"),
                             col_types = readr::cols(.default = "?",
                                                     nestbox_number = "c",
                                                     chick_age = "n",
                                                     radio_tag_ID = "c",
                                                     sample_ID = "n",
                                                     colour_left_up = "c",
                                                     colour_left_down = "c",
                                                     colour_right_up = "c",
                                                     colour_right_down  = "c")) %>%

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
                  Sex_observed = .data$Sex,
                  Age_observed = .data$Age) %>%

    ## Reformat variables
    ## TODO: check about times - currently not formatted properly
    dplyr::mutate(CaptureDate = lubridate::mdy(.data$CaptureDate),
                  CaptureTime = NA_character_,
                  ChickAge = as.integer(ChickAge),
                  Species = dplyr::case_when(.data$Species == "bluti" ~ 14620,
                                             .data$Species == "greti" ~ 14640),
                  Age_observed = dplyr::case_when(.data$Age_observed == "X" ~ 1L,
                                                  .data$Age_observed == "3J" ~ 3L,
                                                  TRUE ~ as.integer(.data$Age_observed)))  %>%
    ## Adjust species names and population names
    dplyr::mutate(Species = dplyr::case_when(.data$Species == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species),
                  PopID = dplyr::case_when(.data$PopID == "cashel" ~ "CAS",
                                           .data$PopID == "garscube" ~ "GAR",
                                           .data$PopID == "kelvingrove_park" ~ "KEL",
                                           .data$PopID == "sallochy" ~ "SAL",
                                           .data$PopID == "SCENE" ~ "SCE",
                                           TRUE ~ .data$PopID))


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    nest_data <- nest_data %>%
      filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    rr_data <- rr_data %>%
      filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    nest_data <- nest_data %>%
      filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    rr_data <- rr_data %>%
      filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }


  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_GLA(data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_GLA(data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_GLA(Capture_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_GLA(data)

  #### FINAL ARRANGEMENT
  Capture_data <-
    Capture_data %>%
    dplyr::filter(!is.na(.data$CaptureDate))

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
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @return A data frame.

create_brood_GLA <- function(brood_data, rr_data) {

  ## Create brood data template
  load(file = "data/Brood_data_template.rda")

  ## Get brood data from ringing records
  rr_data_brood_sum <- rr_data %>%
    dplyr::filter(is.na(.data$LocationID) == F) %>%

    ## Determine whether a chick or adult - done for each observation since there is no unique identifier for individuals
    dplyr::mutate(RingAge = ifelse(.data$Age_observed == 1, "chick", "adult")) %>%

    ## Keeping chicks
    dplyr::filter(RingAge == "chick") %>%

    ## Summarize brood information. Will fail if multiple unique species, male IDs or female IDs within a nest
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
    dplyr::summarise(Species = names(which.max(table(.data$Species, useNA = "always"))),
                     FemaleID = names(which.max(table(.data$MotherRing, useNA = "always"))),
                     MaleID = names(which.max(table(.data$FatherRing, useNA = "always"))),
                     AvgChickMass = round(mean(Mass[ChickAge <= 16 & ChickAge >= 14], na.rm = T),1),
                     NumberChicksMass = n_distinct(IndvID[ChickAge <= 16 & ChickAge >= 14], na.rm = T),
                     AvgTarsus = round(mean(Tarsus[ChickAge <= 16 & ChickAge >= 14], na.rm = T),1),
                     NumberChicksTarsus = n_distinct(IndvID[ChickAge <= 16 & ChickAge >= 14], na.rm = T)) %>%

    ## Replace NaNs and 0 with NA
    dplyr::mutate(across(where(is.numeric), ~na_if(., "NaN"))) %>%
    dplyr::mutate(across(where(is.numeric), ~na_if(., 0)))


  ## Get brood data from nest records
  nest_data_brood_sum <-
    nest_data %>%

    ## TODO: Check on experiments and meaning of replacement clutch

    dplyr::mutate(ExperimentID = dplyr::case_when(!is.na(.data$Experiment) ~
                                                    "COHORT; PARENTAGE"),
                  ClutchType_observed = dplyr::case_when(.data$ReplacementClutch == 0 ~ "first",
                                                         is.na(.data$ReplacementClutch) ~ "first",
                                                         .data$ReplacementClutch > 0 ~ "second"))

  ## Join brood data from ringing records to brood data from nest records
  ## TODO: Add experiment information
  Brood_data <- nest_data_brood_sum %>%
    dplyr::left_join(rr_data_brood_sum, by = c("BreedingSeason", "Species", "PopID", "LocationID")) %>%

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
    dplyr::select(contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%

    ## Calculate clutch type
    dplyr::arrange(PopID, BreedingSeason, Species, FemaleID, LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE))

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create a capture data table in standard format for great tits and blue tits in Glasgow, Scotland.
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @param Brood_data Data frame. Brood_data from Glasgow, Scotland
#'
#' @return A data frame.

create_capture_GLA <- function(data) {

  ## Create capture data template
  load("data/Capture_data_template.rda")

  ## Capture data from ringing records
  ## TODO: Check on tarsus method and age codes
  Capture_data_rr <- rr_data %>%
    dplyr::filter(!(.data$IndvID %in% c("too small", "too_small", "no_rings_COVID"))) %>% # Keep only records of banded individuals

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
  Capture_data_brood <-
    data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols=c("FemaleID","MaleID"), names_to = "Sex_observed", values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Recode sexes
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                                  grepl("Male", .data$Sex_observed) ~ "M")) %>%

    dplyr::group_by(.data$PopID) %>%
    dplyr::mutate(CapturePopID = .data$PopID) %>%

    ## UPDATE CaptureDate
    dplyr::mutate(CaptureDate = .data$LayDate_min, ## TODO: Check on determining CaptureDate
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


  ## Get records of individuals that were reported in brood data, but not in ringing records
  brood_recs_unique <- dplyr::anti_join(Capture_data_brood, Capture_data_rr, by = c("BreedingSeason", "CapturePopID", "IndvID"))

  ## Combine captures and add additional information
  ## TODO: Calculated ages are wrong. Need to check
  Capture_data <- Capture_data_rr %>%
    bind_rows(brood_recs_unique) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$CapturePopID, .data$CaptureDate) %>%

    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"),  ## Create CaptureID based on IndvID and the record number
                  BreedingSeason = as.integer(BreedingSeason)) %>%

    ## Arrange
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%

    ## Calculate age
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason)

  # ## Check that column classes in final table match template
  # Capture_data[,which(purrr::map_df(capture_data_template, class) != purrr::map_df(Capture_data, class))]s

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create full individual data table in standard format for great tits and blue tits in Glasgow, Scotland.
#'
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @return A data frame.

create_individual_GLA <- function(Capture_data, Brood_data){

  ## Create individual data template
  load("data/Individual_data_template.rda")

  Individual_data <- Capture_data %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%

    dplyr::mutate(PopID = .data$CapturePopID[which.min(.data$CaptureDate)], ## Define PopID based on where individual was first encountered
                  RingSeason = min(.data$BreedingSeason)) %>%

    ## Arrange
    dplyr::arrange(IndvID, CaptureDate) %>%

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

                  RingAge = purrr::pmap_chr(.l = list(first(Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return(NA_character_)
                                              } else if(..1 <= 3L){ # TODO:Check that this is right
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            })) %>%

    # Keep only distinct records
    dplyr::distinct(.data$IndvID, .keep_all = TRUE)

  ## Add BroodID information for chicks
  ## For birds banded as chicks with a known location, the brood ID can be joined from the brood data



  Individual_data_m <- Individual_data %>%


    dplyr::left_join(brood_data[,c("BreedingSeason", "PopID", "LocationID","BroodID")], by = c("BreedingSeason", "PopID", "LocationID"))

  Individual_data_m.dups <- Individual_data_m %>%
    group_by(IndvID) %>%
    mutate(count = n_distinct(BroodID))



  dplyr::mutate(BroodIDLaid = ifelse(.data$RingAge == "chick",
                                     paste(.data$BreedingSeason, .data$LocationID, sep = "_"),
                                     NA_character_)) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[,!(names(individual_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create a location data table in standard format for great tits and blue tits in Glasgow, Scotland.
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @return A data frame.

create_location_GLA <- function(data) {

  location_data_template <- create_location_data_template_fn()

  Location_data <-
    data %>%
    dplyr::select(.data$BreedingSeason, .data$LocationID, .data$PopID) %>%
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::arrange(.data$BreedingSeason, .by_group = TRUE) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     PopID = unique(.data$PopID)) %>%
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
                  Longitude = case_when(.data$PopID == "GAR" ~ -4.3199,
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

  return(Location_data)

}
