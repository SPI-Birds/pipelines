#'Construct standard format for data from Malmo, Sweden
#'
#'A pipeline to produce the standard format for the nest box population in Malmo, Sweden, administered by Caroline Isaksson
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}:
#'
#'\strong{BroodID}:
#'
#'\strong{IndvID}:
#'
#'\strong{CaptureDate}:
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_MAL <- function(db = choose_directory(),
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

  ##  Options
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)

  # ## Read experiment classification table
  # expID_tab <- utils::read.csv(file = paste0(db, "/MAL_experiment_groups.csv"))

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/MAL/data/MAL_Malmo_Sweden/"

  ## Read in primary data from ringing records
  rr_data <- readxl::read_xlsx(path = paste0(db, "/MAL_PrimaryData_1.xlsx"),
                               guess_max = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(Age = toupper(.data$Age)) %>%

    ## Reformat and rename important variables
    ## TODO: Check rounding on Tarsus
    ## TODO: Check on age codes
    ## TODO: Check on IndvID format - some IDs look like they are not actually band numbers
    dplyr::transmute(PopID = as.character("MAL"),
                     BreedingSeason = as.integer(.data$Year),

                     ## TODO: Check Species codes
                     Species = dplyr::case_when(.data$Species == "TX" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "BM" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "RS" ~ "?",
                                                .data$Species == "NA" ~ "?"),
                     Plot = stringr::str_extract(.data$SiteNest, "[:alpha:]+"),
                     LocationID = .data$SiteNest,
                     CaptureDate = lubridate::ymd(.data$Date),
                     CaptureTime = format(lubridate::as_datetime(.data$Time), "%H:%M"),
                     Mass = suppressWarnings(as.numeric(.data$MassG)),
                     Tarsus = suppressWarnings(round(as.numeric(.data$TarsusMm),3)),
                     WingLength = suppressWarnings(as.numeric(.data$WingMm)),
                     IndvID = .data$Id,
                     Age_observed = dplyr::case_when(.data$Age == "P" ~ 1L,
                                                     .data$Age == "2" ~ 1L,
                                                     .data$Age == "2K" ~ 5L,
                                                     .data$Age == "2K+" ~ 5L,
                                                     .data$Age == "3" ~ 5L,
                                                     .data$Age == "3K" ~ 5L,
                                                     .data$Age == "3K+" ~ 5L),
                     Sex_observed = case_when(.data$Sex == "FEMALE" ~ "F",
                                              .data$Sex == "MALE" ~ "M",
                                              TRUE ~ as.character(.data$Sex)),
                     ObserverID = .data$Handler) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$CaptureDate)


  ## Read in primary data from nest records (new format)
  nest_data_17_19 <- readxl::read_xlsx(path = paste0(db, "/MAL_PrimaryData_2.xlsx"),
                                       sheet = 1,
                                       guess_max = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    dplyr::transmute(PopID = as.character("MAL"),
                     BreedingSeason = as.integer(.data$Year),

                     ## TODO: Check Species codes
                     ## TODO: Check on NumberAt1415
                     Species = dplyr::case_when(.data$Species == "TX" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "BM" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "RS" ~ "?",
                                                .data$Species == "NA" ~ "?"),
                     Plot = stringr::str_extract(.data$SiteNest, "[:alpha:]+"),
                     LocationID = .data$SiteNest,
                     LayDate_observed = lubridate::ymd(.data$FirstEgg),
                     HatchDate_observed = lubridate::ymd(.data$HatchDate),
                     ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                     BroodSize_observed = suppressWarnings(as.integer(.data$NumberHatched)),
                     FledgeSize_observed = suppressWarnings(as.integer(.data$NumberAtD14)),

                     ## Year is not entered in some cases, retrieve it from other date columns when possible
                     ## TODO: Inform about missing years
<<<<<<< HEAD
                     BreedingSeason = dplyr::case_when(is.na(.data$Year) ~ as.integer(lubridate::year(.data$LayDate_observed)),
                                                       TRUE ~ as.integer(.data$Year)))
||||||| :construction: Add general tests to MAL pipeline brancjh
                     BreedingSeason = dplyr::case_when(is.na(.data$Year) ~ lubridate::year(.data$LayDate_observed),
                                                       TRUE ~ .data$Year))

=======
                     BreedingSeason = dplyr::case_when(is.na(.data$Year) ~ as.integer(lubridate::year(.data$LayDate_observed)),
                                                       TRUE ~ as.integer(.data$Year)))

>>>>>>> MAL_pipeline

  ## Read in primary data from nest records (old format)
  nest_data_13_16 <- readxl::read_xlsx(path = paste0(db, "/MAL_PrimaryData_2.xlsx"),
                                       sheet = 2,
                                       guess_max = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## TODO: Check on ClutchSize2
    dplyr::transmute(PopID = as.character("MAL"),
                     BreedingSeason = as.integer(.data$Year),

                     ## TODO: Check Species codes
                     Species = dplyr::case_when(.data$Species == "TX" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "BM" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "RS" ~ "?",
                                                .data$Species == "NA" ~ "?"),
                     Plot = stringr::str_extract(.data$SiteNest, "[:alpha:]+"),
                     LocationID = .data$SiteNest,
                     LayDate_observed = lubridate::ymd(.data$FirstEgg),
                     HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$HatchDay),
                                                                   origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                     BroodSize_observed = suppressWarnings(as.integer(.data$NumberHatched)),
                     FledgeSize_observed = suppressWarnings(as.integer(.data$NumberAtD14D15)))

  ## Combine two primary data formats and process further
  ## TODO: Species missing in a surprising number of cases
  nest_data <- bind_rows(nest_data_13_16, nest_data_17_19) %>%

    dplyr::arrange(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$LayDate_observed) %>%

    ## Create BroodID based on Po pID and row number
    dplyr::mutate(BroodID = dplyr::case_when(!is.na(.data$Species) ~ paste(.data$PopID, dplyr::row_number(), sep ="-")))

  # TODO: Join experiment labels
  # left_join(expID_tab, by = c("Experiment", "Treatment"))


  ## Read in primary data from location data
  loc_data <- readxl::read_xlsx(path = paste0(db, "/MAL_PrimaryData_2.xlsx"),
                                guess_max = 5000,
                                sheet = 4) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::rename_with(~ tolower(gsub("X", "", .x, fixed = TRUE))) %>%
    tidyr::pivot_longer(cols = matches("20.*"),
                        values_to = "Present",
                        names_to = "Year") %>%
    dplyr::arrange(.data$sitenest, .data$Year) %>%
    dplyr::group_by(.data$sitenest) %>%
    dplyr::mutate(removed = case_when(.data$Present == "N" & .data$Year == max(.data$Year) ~ "Y" ,
                                      TRUE ~ "N")) %>%
    dplyr::mutate(Year = as.integer(.data$Year)) %>%
    as.data.frame


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    rr_data <- rr_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    nest_data <- nest_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    rr_data <- rr_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    nest_data <- nest_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_MAL(nest_data, rr_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_MAL(rr_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_MAL(Capture_data, Brood_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_MAL(loc_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MAL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MAL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MAL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MAL.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Malmo, Sweden.
#'
#' @param nest_data Data frame of nest data from Malmo, Sweden.
#'
#' @param rr_data Data frame of ringing records from Malmo, Sweden.
#'
#' @return A data frame.

create_brood_MAL <- function(nest_data, rr_data) {

  ## Get info on adults for each brood from ringing records
  rr_data_brood_ads <- rr_data %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Determine whether a chick or adult
    dplyr::mutate(RingAge = ifelse(.data$Age_observed == 1L, "chick", "adult")) %>%

    ## Keep only adults and where sex is known
    dplyr::filter(.data$RingAge == "adult" & !is.na(.data$Sex_observed)) %>%

    ## Get number of distinct M or F IDs associated with Location
    ## Remove records with multiple adult IDs of the same sex recorded since it will not be possible to infer the parent ID for that sex
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$Species, .data$Plot, .data$LocationID, .data$Sex_observed) %>%
    dplyr::mutate(IDs = n_distinct(.data$IndvID)) %>%
    dplyr::filter(IDs == 1) %>%

    ## Keep only distinct records for:
    dplyr::distinct(.data$BreedingSeason, .data$PopID, .data$Species, .data$Plot, .data$LocationID, .data$Sex_observed, .keep_all = T) %>%

    ## Create brood record to use as ID column
    dplyr::mutate(Brood_rec = paste(.data$BreedingSeason, .data$PopID, .data$Species, .data$Plot, .data$LocationID)) %>%

    ## Get adults for each brood
    tidyr::pivot_wider(values_from = .data$IndvID,
                       names_from = .data$Sex_observed,
                       id_cols = .data$Brood_rec) %>%

    ## Rename columns
    dplyr::rename(FemaleID = "F",
                  MaleID = "M")

  ## Get info on chicks for each brood from ringing records
  ## TODO: Check on age when chicks are measured
  rr_data_brood_cks <- rr_data %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Determine whether a chick or adult
    dplyr::mutate(RingAge = ifelse(.data$Age_observed == 1L, "chick", "adult")) %>%

    ## Keep only chicks
    dplyr::filter(.data$RingAge == "chick") %>%

    ## Create brood record to use as ID column
    dplyr::mutate(Brood_rec = paste(.data$BreedingSeason, .data$PopID, .data$Species, .data$Plot, .data$LocationID)) %>%

    ## Summarise brood info from chicks
    dplyr::group_by(.data$Brood_rec) %>%
    dplyr::summarise(AvgChickMass = round(mean(.data$Mass, na.rm = T),1),
                     NumberChicksMass = sum(is.na(.data$Mass) == F),
                     AvgTarsus = round(mean(.data$Tarsus, na.rm = T),1),
                     NumberChicksTarsus = sum(is.na(.data$Tarsus) == F)) %>%

    ## Replace NaNs and 0 with NA
    dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::na_if(., "NaN")),
                  dplyr::across(where(is.numeric), ~dplyr::na_if(., 0)))


  ## Get general brood info and join in info on parents and chicks
  Brood_data <- nest_data %>%

    ## Create brood record to use as ID column
    dplyr::mutate(Brood_rec = paste(.data$BreedingSeason, .data$PopID, .data$Species, .data$Plot, .data$LocationID)) %>%

    ## Join
    dplyr::left_join(rr_data_brood_ads, by = "Brood_rec") %>%
    dplyr::left_join(rr_data_brood_cks, by = "Brood_rec") %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    # Add missing columns
    dplyr::bind_cols(brood_data_template[0,!(names(brood_data_template) %in% names(.))]  %>%
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


  ## Check column classes
  purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Malmo, Sweden.
#'
#' @param rr_data Data frame of ringing records from Malmo, Sweden.
#'
#' @return A data frame.

create_capture_MAL <- function(rr_data) {

  ## Capture data from ringing records
  Capture_data <- rr_data %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$PopID, .data$IndvID, .data$CaptureDate) %>%

    ## Add additional data
    dplyr::mutate(CapturePopID = .data$PopID,
                  CapturePlot = .data$Plot,
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE,

                  ## Set ReleasePopID to NA if ReleaseAlive is FALSE, otherwise same as CapturePopID
                  ReleasePopID = dplyr::case_when(.data$ReleaseAlive == FALSE ~ NA_character_,
                                                  TRUE ~ as.character(.data$CapturePopID)),

                  ## Set ReleasePlotID to NA if ReleaseAlive is FALSE, otherwise same as CapturePlot
                  ReleasePlot = dplyr::case_when(.data$ReleaseAlive == FALSE ~ NA_character_,
                                                 TRUE ~ as.character(.data$CapturePlot))) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0,!(names(capture_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%

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

    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>% ## Create CaptureID based on IndvID and the record number
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Malmo, Sweden.
#'
#' @param Capture_data Capture data output from Malmo, Sweden.
#'
#' @param Brood_data Brood data output from Malmo, Sweden.
#'
#' @return A data frame.

create_individual_MAL <- function(Capture_data, Brood_data){

  Individual_data <- Capture_data %>%

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
                                            })) %>%

    ## Join Brood data for Individuals banded as chicks
    dplyr::mutate(brood_record = dplyr::case_when(.data$RingAge == "chick" &
                                                    .data$RingSeason == .data$BreedingSeason &
                                                    !is.na(.data$LocationID) ~ "yes",
                                                  TRUE ~ NA_character_)) %>%

    ## Only join BroodID to chick records
    dplyr::left_join(Brood_data %>%
                       dplyr::mutate(brood_record = "yes") %>%
                       dplyr::select(.data$brood_record,
                                     .data$BreedingSeason,
                                     .data$Species,
                                     .data$Plot,
                                     .data$LocationID,
                                     .data$BroodID),
                     by = c("brood_record", "BreedingSeason", "Species", "CapturePlot" = "Plot", "LocationID")) %>%

    dplyr::rename(BroodIDLaid = .data$BroodID) %>%

    ## Add BroodIDFledged
    dplyr::mutate(BroodIDFledged = .data$BroodIDLaid) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0,!(names(individual_data_template) %in% names(.))]  %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Malmo, Sweden.
#'
#' @param loc_data Data frame of location data from Malmo, Sweden.
#'
#' @return A data frame.

create_location_MAL <- function(loc_data) {

  ## Build location data based on locations from primary data
  ## TODO: Check whether nest boxes have been removed
  Location_data <- loc_data %>%

    dplyr::rename(HabitatType = habitat,
                  LocationID = sitenest,
                  Latitude = latitude,
                  Longitude = longitude) %>%

    dplyr::group_by(.data$LocationID) %>%
    dplyr::mutate(PopID = "MAL",
                  HabitatType = tolower(HabitatType),
                  NestboxID = .data$LocationID,
                  StartSeason = as.integer(min(.data$Year[.data$Present == "Y"], na.rm = TRUE)),
                  EndSeason = as.integer(dplyr::case_when(any(removed == "Y") ~ max(.data$Year),
                                               TRUE ~ NA_integer_)),
                  LocationType = "NB") %>%

    dplyr::distinct(.data$LocationID, .keep_all = T) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[0,!(names(location_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)

  return(Location_data)

}
