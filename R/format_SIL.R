#'Construct standard format for data from Silwood, UK
#'
#'A pipeline to produce the standard format for the nest box population in Silwood, UK, administered by Julia Schroeder.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Blue tits.
#'
#'\strong{IndvID}: Should be a character string of length 7 where the first three characters are either a letter or number and the last four characters are all numbers.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_SIL <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

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
  options(dplyr.summarise.inform = FALSE)

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/SIL/data/SIL_Silwood_UK"

  ## Read in brood data
  ## TODO: There are 3 FemaleIDs (Z04792, f.A18BFS09, f.E07FS09) that are likely incorrect
  brood_data <- read.csv(file = paste0(db, "/SIL_PrimaryData_Breeding.csv"))  %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(PopID = "SIL",
                     BreedingSeason = as.integer(.data$year),
                     Species = species_codes[species_codes$SpeciesID == 14620,]$Species,
                     LocationID = as.character(.data$nest_box),
                     FemaleID = case_when(stringr::str_detect(.data$female_BTO_ring, "^[[:alpha:][:digit:]]{3}[:digit:]{4}$") ~ .data$female_BTO_ring,
                                            TRUE ~ NA_character_),
                     MaleID = .data$male_BTO_ring,
                     LayDate_observed = suppressWarnings(as.Date(as.numeric(.data$lay_date),
                                                                 origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$hatch_date),
                                                                   origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     ClutchSize_observed = suppressWarnings(as.integer(.data$clutch_size)),
                     BroodSize_observed = suppressWarnings(as.integer(.data$no_hatchlings)),
                     NumberFledged_observed = suppressWarnings(as.integer(.data$no_day14))) %>%

    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$LocationID)


  ## Read in adult data
  adult_data <- read.csv(file = paste0(db, "/SIL_PrimaryData_capture.csv"))  %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    ## TODO: Check age codes
    ## TODO: Check on Capture dates - NA for many, using mean for those cases currently
    ## TODO: Check on mass values that are >X or <X
    ## TODO: Currently setting the 3 incorrect IDs to NA, consider changing
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(PopID = "SIL",
                     BreedingSeason = as.integer(.data$year),
                     Species = species_codes[species_codes$SpeciesID == 14620,]$Species,
                     LocationID = as.character(.data$nest_box),
                     CaptureDate = dplyr::case_when(!is.na(.data$capture_date) ~ as.Date(as.numeric(.data$capture_date),
                                                                                         origin = as.Date(paste0(.data$BreedingSeason, "-03-31"))),
                                                    TRUE ~ as.Date(36, origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     IndvID = case_when(stringr::str_detect(.data$BTO_ring, "^[[:alpha:][:digit:]]{3}[:digit:]{4}$") ~ .data$BTO_ring,
                                        TRUE ~ NA_character_),
                     Sex_observed = .data$sex,
                     Age_observed = .data$age,
                     CaptureTime = format(strptime(.data$capture_time,format = "%H:%M"), "%H:%M"),
                     Mass = round(suppressWarnings(as.numeric(.data$mass)), 2),
                     WingLength = as.numeric(.data$wing),
                     Tarsus = as.numeric(.data$tarsus)) %>%

    dplyr::filter(!is.na(.data$IndvID))



  ## Read in nest data
  nest_data <- read.csv(file = paste0(db, "/SIL_PrimaryData_nestboxlocations.csv"))  %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::select(LocationID = .data$NestBox,
                  Latitude = .data$latitude,
                  Longitude = .data$longitude) %>%
    dplyr::mutate(PopID = "SIL")



  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_SIL(brood_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_SIL(adult_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_SIL(Capture_data_temp)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_SIL(nest_data, Brood_data_temp)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0, !(names(brood_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)


  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)


  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0, !(names(individual_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))  %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  ## Location data
  Location_data <- Location_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[0, !(names(location_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))  %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)



  ## Filter to keep only desired Species if specified for Brood, Capture, and Individual tables
  if(!is.null(species_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Pops if specified for Brood, Capture, Individual, and Location tables
  if(!is.null(pop_filter)){

    Brood_data <- Brood_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$CapturePopID %in% pop_filter & !(is.na(.data$CapturePopID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    Location_data <- Location_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_SIL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_SIL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_SIL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_SIL.csv"), row.names = F)

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

#' Create brood data table in Silwood, UK.
#'
#' @param brood_data Data frame of brood data from Silwood, UK.
#'
#' @return A data frame.

create_brood_SIL <- function(brood_data) {

  ## Combine primary data to create brood data
  Brood_data_temp <- brood_data %>%

    ## Create BroodID
    dplyr::group_by(.data$BreedingSeason) %>%
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, 1:dplyr::n(), sep = "-")) %>%
    dplyr::ungroup() %>%

    ## Calculate clutch type
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =., protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(brood_data_template)), dplyr::everything())

  return(Brood_data_temp)

}

#' Create capture data table for Silwood, UK.
#'
#' @param adult_data, Data frame of adult ringing records from Silwood, UK.
#'
#' @return A data frame.

create_capture_SIL <- function(adult_data) {


  ## Combine primary data to create capture data
  Capture_data_temp <- adult_data %>%

    ## Create new columns
    dplyr::mutate(CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    ## Calculate age
    dplyr::group_by(.data$IndvID) %>%
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason) %>%

    ## Create CaptureID
    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything())


  return(Capture_data_temp)

}

#' Create individual table for Silwood, UK.
#'
#' @param Capture_data_temp Capture data output from Silwood, UK
#'
#' @return A data frame.

create_individual_SIL <- function(Capture_data_temp){

  ## Create individual data from capture data
  Individual_data_temp <- Capture_data_temp %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
    dplyr::mutate(PopID = .data$CapturePopID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(RingSeason = min(.data$BreedingSeason, na.rm = T)) %>%

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
                                            }))  %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())

  return(Individual_data_temp)

}


#' Create location data table for Silwood, UK.
#'
#' @param nest_data Data frame of nest location data from Silwood, UK.
#'
#' @param brood_data_temp Data frame of brood data from Silwood, UK.
#'
#' @return A data frame.

create_location_SIL <- function(nest_data, Brood_data_temp) {

  ## Build location data based on nest data
  ## TODO: Check about the boxes that have been repositioned
  ## TODO: Check about habitat types
  Location_data_temp <- nest_data %>%

    ## Summarize information for each nest box
    ## TODO: Check that nest boxes have not been removed
    dplyr::group_by(.data$PopID, .data$LocationID) %>%

    dplyr::left_join(Brood_data_temp %>%
                       dplyr::select(.data$LocationID,
                                     .data$BreedingSeason),
                     by = "LocationID") %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::mutate(StartSeason = min(.data$BreedingSeason),
                  EndSeason = NA_integer_,
                  NestboxID = .data$LocationID,
                  LocationType = "NB",
                  HabitatType = "mixed") %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$LocationID, .keep_all = TRUE) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}
