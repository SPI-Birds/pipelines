## Housekeeping

## Source functions to read in templates
source(file = "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/templates/individual_data_template_fn.R")
source(file = "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/templates/brood_data_template_fn.R")
source(file = "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/templates/capture_data_template_fn.R")
source(file = "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/templates/location_data_template_fn.R")

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

  if(is.null(pop)){

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  #### Primary data

  message("Importing primary data...")

  db <- setwd("/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/GLA/")
  data <- readr::read_csv(file = paste0(db, "/GLA_PrimaryData.csv")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename important variables
    dplyr::mutate(BreedingSeason = as.integer(.data$Yr),
                  LocationID = as.character(.data$NestboxNumber),
                  Species = as.character(.data$Species),
                  PopID = as.character(.data$Site),
                  FirstEggDate = lubridate::dmy(.data$FirstEggDate),
                  LayingComplete = lubridate::dmy(.data$LayingComplete),
                  ObservedHatch = lubridate::dmy(.data$ObservedHatch),
                  Hatchlings = as.integer(.data$Hatchlings),
                  Fledglings = as.integer(.data$Fledglings),
                  MaleRing = as.character(.data$MaleRing),
                  FemaleRing = as.character(.data$FemaleRing)) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, dplyr::desc(.data$BreedingSeason), .data$LocationID, dplyr::desc(.data$FirstEggDate)) %>%

    ## Select variables of interest
    dplyr::select(.data$BreedingSeason, .data$LocationID, .data$PopID, .data$ReplacementClutch,
                  .data$Experiment, .data$Treatment, .data$Species, .data$FirstEggDate, .data$LayingComplete, .data$ExpectedHatch, .data$ObservedHatch,
                  .data$ClutchSize, .data$HatchlingsManip, .data$ClutchComplete, .data$UnhatchedEggs, .data$Fledglings, .data$MaleRing, .data$FemaleRing) %>%

    ## Create additional variables that will be used in multiple data tables
    dplyr::group_by(.data$PopID) %>%
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
                  LayDate_observed = .data$FirstEggDate,
                  LayDate_min = .data$FirstEggDate,
                  LayDate_max = .data$LayingComplete,
                  ClutchSize_observed = .data$ClutchSize,
                  HatchDate_observed = .data$ObservedHatch,
                  NumberFledged_observed = .data$Fledglings) %>%

    ## Adjust species names
    dplyr::mutate(Species = dplyr::case_when(.data$Species == 14640 ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                             .data$Species == 14620 ~ species_codes[species_codes$SpeciesID == 14620, ]$Species))


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    data <- data %>%
      filter(.data$Species %in% species_filter)

  }

  ## Filter to keep only desired Species if specified
  if(!is.null(pop_filter)){

    data <- data %>%
      filter(.data$PopID %in% pop_filter)

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

create_brood_GLA <- function(data) {

  ## Create brood data template
  brood_data_template <- create_brood_data_template_fn()

  Brood_data <-
    data %>%

    ## To be updated using meta-data
    dplyr::mutate(ExperimentID = dplyr::case_when(!is.na(.data$Experiment) ~
                                                    "COHORT; PARENTAGE")) %>%

    ## Keep only necessary columns
    dplyr::select(contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template))

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
  capture_data_template <- create_capture_data_template_fn()

  Capture_data <-
    data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols=c("FemaleID","MaleID"), names_to = "Sex_observed", values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Recode sexes (here I am just going with the reported sex unlike above where I checked for consistency, something we can discuss)
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                           grepl("Male", .data$Sex_observed) ~ "M")) %>%

    dplyr::group_by(.data$PopID) %>%

    ## UPDATE CaptureDate
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"), ## Create CaptureID based on IndvID and the record number
                  CaptureDate = .data$LayDate_min, ## TO FIX
                  CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  ReleasePopID = .data$PopID, ## Set ReleasePopID
                  CaptureAlive = TRUE, ## Set CaptureAlive to T
                  ReleaseAlive = TRUE, ## Set ReleaseAlive to T
                  ExperimentID = dplyr::case_when(!is.na(.data$Experiment) ~
                                                    "COHORT; PARENTAGE")) %>%   ## TO FIX

    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Glasgow, Scotland.
#'
#' Create full individual data table in standard format for great tits and blue tits in Glasgow, Scotland.
#'
#' @param data Data frame of modified primary data from Glasgow, Scotland.
#'
#' @return A data frame.

create_individual_GLA <- function(Capture_data){

  individual_data_template <- create_individual_data_template_fn()

  Individual_data <-
    Capture_data %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
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

                  ## Define PopID based on where individual was  first encountered
                  PopID = .data$CapturePopID[which.min(.data$CaptureDate)],
                  RingSeason = min(.data$BreedingSeason),
                  RingAge = "adult") %>%

    dplyr::ungroup() %>%

    # Keep only distinct records
    dplyr::distinct(.data$IndvID, .keep_all = TRUE) %>%

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
