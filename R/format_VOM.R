#'Construct standard format for data from Vomb Fure, Sweden
#'
#'A pipeline to produce the standard format for the nest box population in Vomb Fure, Sweden, administered by Juli Broggi and Jan-Ake Nilsson.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{IndvID}: Should have the form: "2AA11111" - The first character is always a 2, the second two characters are letters,
#'and the last five characters are numbers for a total of 8 characters.
#'
#'\strong{CaptureDate}: Set as the lay date for the nest. When lay date is not present, use the mean lay date (July 14th).
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_VOM <- function(db = choose_directory(),
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

  if(is.null(pop)){

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE,
          digits = 10)

  # db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/VOM/VOM_VombFure_Sweden/"

  ## Read in primary data
  nest_data <- readxl::read_xlsx(path = paste0(db, "/VOM_PrimaryData.xlsx"), guess = 5000, col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename columns
    ## TODO: Check about hatch estimate (HATCHe)
    ## TODO: Check about experiments
    ## TODO: Check species codes
    dplyr::mutate(PopID = "VOM",
                  BreedingSeason = as.integer(.data$Yea),
                  LocationID = as.character(.data$NBr),
                  LayDate_observed = suppressWarnings(as.Date(as.numeric(.data$Ld),
                                                              origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$HatcHe),
                                                                origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  ClutchSize_observed = suppressWarnings(as.integer(.data$Cs)),
                  BroodSize_observed = suppressWarnings(as.integer(.data$Bs)),
                  NumberFledged_observed = suppressWarnings(as.integer(.data$Fled)),
                  Species = .data$Sps,
                  FemaleID = .data$RnGf,
                  MaleID = .data$RnGm,
                  ClutchType_observed = .data$BroodOrderWithinYear,
                  Species = dplyr::case_when(.data$Species == "PF"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "NT"  ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "MT"  ~ species_codes[species_codes$SpeciesID == 14400,]$Species,
                                             .data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species)) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$LocationID) %>%

    ## Retain variables of interest
    dplyr::select(.data$BreedingSeason,
                  .data$PopID,
                  .data$LocationID,
                  .data$Species,
                  .data$LayDate_observed,
                  .data$HatchDate_observed,
                  .data$ClutchSize_observed,
                  .data$BroodSize_observed,
                  .data$NumberFledged_observed,
                  .data$FemaleID,
                  .data$MaleID)


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_VOM(nest_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_VOM(nest_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_VOM(Capture_data_temp)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_VOM(nest_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0,!(names(brood_data_template) %in% names(.))]  %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template))

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0,!(names(capture_data_template) %in% names(.))]  %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)


  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0,!(names(individual_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)


  ## Location data
  Location_data <- Location_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[0,!(names(location_data_template) %in% names(.))]  %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)


  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_VOM.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_VOM.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_VOM.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_VOM.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Vomb Fure, Sweden.
#'
#' @param nest_data Data frame of nest data from Vomb Fure, Sweden.
#'
#' @return A data frame.

create_brood_VOM <- function(nest_data) {

  ## Create brood data
  Brood_data_temp <- nest_data %>%

    dplyr::mutate(BroodID = paste(.data$LocationID, 1:n(), sep = "_")) %>%

    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE))

  return(Brood_data_temp)

}

#' Create capture data table for great tits and blue tits in Vomb Fure, Sweden.
#'
#' @param nest_data Data frame of nest data from Vomb Fure, Sweden.
#'
#' @return A data frame.

create_capture_VOM <- function(nest_data) {

  ## Create capture data
  ## TODO: Check on capture date. For now, set as lay date and when lay date is not known, set to July 14th (mean lay date)
  Capture_data_temp <- nest_data %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID), values_to = "IndvID", names_to = "Sex_observed") %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%

    ## Add additional information
    ## TODO: Check on age information
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$LocationID) %>%
    dplyr::mutate(Sex_observed = substring(.data$Sex_observed, 1,1),
           Age_observed = NA_integer_,
           CaptureDate = dplyr::case_when(!is.na(.data$LayDate_observed) ~.data$LayDate_observed,
                                          TRUE ~ as.Date(paste0(.data$BreedingSeason, "-07-14"))),
           CapturePopID = .data$PopID,
           ReleasePopID = .data$PopID,
           CaptureAlive = TRUE,
           ReleaseAlive = TRUE,
           CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"))

  return(Capture_data_temp)

}

#' Create individual table for great tits and blue tits in Vomb Fure, Sweden.
#'
#' @param Capture_data_temp Data frame of Capture data from Vomb Fure, Sweden.
#'
#' @return A data frame.

create_individual_VOM <- function(Capture_data_temp){

  ## Create individual data
  Individual_data_temp <- Capture_data_temp %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(PopID = .data$CapturePopID,
                  RingSeason = min(.data$BreedingSeason, na.rm = T),
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
                                            })) %>%

    dplyr::distinct(.data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup()

  return(Individual_data_temp)

}


#' Create location data table for great tits and blue tits in Vomb Fure, Sweden.
#'
#' @param nest_data Data frame of nest data from Vomb Fure, Sweden.
#'
#' @return A data frame.

create_location_VOM <- function(nest_data) {

  ## Build location data based on nest data
  Location_data_temp <- nest_data %>%

    ## Summarize information for each nest box
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::summarise(NestboxID = .data$LocationID,
                     LocationType = "NB",
                     StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     Latitude = 55.67,
                     Longitude = 13.55,
                     HabitatType = "mixed") %>%

    ## Keep distinct records
    dplyr::distinct(.data$LocationID, .keep_all = TRUE)

  return(Location_data_temp)

}
