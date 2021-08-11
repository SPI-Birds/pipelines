#'Construct standard format for data from Bergen, Norway
#'
#'A pipeline to produce the standard format for the nest box population in Bergen, Norway, administered by Adele Mennerat.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}:
#'
#'\strong{IndvID}:
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_BRG <- function(db = choose_directory(),
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

  ## Read in primary data from nest sheet
  ## TODO: Check about nest box names. Will numbers ever be repeated?

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/BRG/BRG_Bergen_Norway/"

  ## Read in nest data
  nest_data <- readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                 guess_max = 5000,
                                 sheet = "Broods",
                                 col_types = "text") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(PopID = "BRG",
                     BreedingSeason = as.integer(.data$Year),
                     Species = dplyr::case_when(.data$Species == "Kjøttmeis"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "Blåmeis"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                                .data$Species == "Svartmeis"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species),
                     Plot = .data$Location,
                     LocationID = as.character(.data$Nestbox),
                     LayDate_observed = suppressWarnings(as.Date(as.numeric(.data$LayDate),
                                                                 origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$HatchDate),
                                                                   origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                     ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                     BroodSize_observed = suppressWarnings(as.integer(.data$BroodSize)),
                     NumberFledged_observed = suppressWarnings(as.integer(.data$Fledged)),
                     HabitatType = tolower(.data$Vegetation)) %>%

    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID)


  ## Read in chick data
  chick_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                                   sheet = "Chicks",
                                                   guess_max = 5000,
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(PopID = "BRG",
                     BreedingSeason = as.integer(.data$Year),
                     Species = dplyr::case_when(.data$Species == "Kjøttmeis"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "Blåmeis"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                                .data$Species == "Svartmeis"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species),
                     Plot = .data$Location,
                     LocationID = as.character(.data$Nestbox),
                     CaptureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))),
                     IndvID = .data$Ring,
                     RingAge = .data$Age,
                     ChickAge = as.integer(.data$ChickAge),
                     CaptureTime = gsub("--", ":00", .data$Time),
                     Mass = round(suppressWarnings(as.numeric(.data$Weight)), 1))


  ## Read in adult data
  adult_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/BRG_PrimaryData.xlsx"),
                                                   sheet = "Adults",
                                                   guess_max = 5000,
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    ## TODO: Check age codes
    dplyr::mutate(dplyr::across(where(is.character), ~dplyr::na_if(., "."))) %>%
    dplyr::transmute(PopID = "BRG",
                     BreedingSeason = as.integer(.data$Year),
                     Species = dplyr::case_when(.data$Species == "Kjøttmeis"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "Blåmeis"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "Svarthvitfluesnapper"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                                .data$Species == "Svartmeis"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species),
                     Plot = .data$Location,
                     LocationID = as.character(.data$Nestbox),
                     CaptureDate = suppressWarnings(as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))),
                     IndvID = .data$Ring,
                     Sex_observed = .data$Sex,
                     Age_observed = dplyr::case_when(.data$ObsAge == "juv" ~ 5L,
                                                     .data$ObsAge == "ad" ~ 6L),
                     CaptureTime = paste0(substr(.data$Time,1,2), ":", substr(.data$Time,3,4)),
                     Mass = round(suppressWarnings(as.numeric(.data$Weight)), 1),
                     WingLength = as.numeric(.data$Winglength))

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_BRG(nest_data, chick_data, adult_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_BRG(chick_data, adult_data, Brood_data_temp)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_BRG(Capture_data_temp)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_BRG(nest_data)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BRG.csv"), row.names = F)

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

#' Create brood data table in Bergen, Norway.
#'
#' @param nest_data Data frame of nest data from Bergen, Norway.
#'
#' @param chick_data Data frame of chick ringing records from Bergen, Norway.
#'
#' @param adult_data Data frame of adult ringing records from Bergen, Norway.
#'
#' @return A data frame.

create_brood_BRG <- function(nest_data, chick_data, adult_data) {

  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%

    ## Join summarized chick data
    dplyr::left_join(chick_data %>%
                       dplyr::filter(dplyr::between(.data$ChickAge, 14, 16)) %>%
                       dplyr::group_by(.data$BreedingSeason, .data$Plot, .data$LocationID) %>%
                       dplyr::summarise(AvgChickMass = mean(.data$Mass, na.rm = T),
                                        NumberChicksMass = sum(!is.na(.data$Mass))),
                     by = c("BreedingSeason", "Plot", "LocationID")) %>%

    ## Join adult data to get info on parents
    dplyr::left_join(adult_data %>%
                       dplyr::select(.data$BreedingSeason,
                                     .data$Plot,
                                     .data$LocationID,
                                     .data$IndvID,
                                     .data$Sex_observed) %>%
                       tidyr::pivot_wider(id_cols = c(.data$BreedingSeason,
                                                      .data$Plot,
                                                      .data$LocationID),
                                          values_from = .data$IndvID,
                                          names_from = .data$Sex_observed) %>%
                       dplyr::rename(FemaleID = "F",
                                     MaleID = "M"),
                     by = c("BreedingSeason", "Plot", "LocationID")) %>%

    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%

    ## Create BroodID
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, 1:dplyr::n(), sep = "-")) %>%

    ## Calculate clutch type
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(brood_data_template)), dplyr::everything())

  return(Brood_data_temp)

}

#' Create capture data table for Bergen, Norway.
#'
#' @param chick_data, Data frame of chick ringing records from Bergen, Norway.
#'
#' @param adult_data, Data frame of adult ringing records from Bergen, Norway.
#'
#' @param Brood_data_temp, Data frame of brood data created from primary data from Bergen, Norway.
#'
#' @return A data frame.

create_capture_BRG <- function(chick_data, adult_data, Brood_data_temp) {


  ## Combine primary data to create capture data
  Capture_data_temp <- adult_data %>%

    ## Bind chick data
    dplyr::bind_rows(chick_data %>%
                       dplyr::mutate(Age_observed = 1L) %>%
                       dplyr::left_join(Brood_data_temp %>%
                                          dplyr::select(.data$BreedingSeason,
                                                        .data$Plot,
                                                        .data$LocationID,
                                                        .data$BroodID),
                                        by = c("BreedingSeason", "Plot", "LocationID"))) %>%

    ## Create new columns
    dplyr::mutate(CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePlot  = .data$Plot) %>%

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
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything())


  return(Capture_data_temp)

}

#' Create individual table for Bergen, Norway.
#'
#' @param Capture_data_temp Capture data output from Bergen, Norway
#'
#' @return A data frame.

create_individual_BRG <- function(Capture_data_temp){

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


    ## Add BroodID information
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(BroodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodID))),
                                               .f = ~{
                                                 if(length(..1) != 1){
                                                   return(NA_character_)
                                                 } else if(length(..1) == 1){
                                                   return(..1)
                                                 }
                                               }),
                  BroodIDFledged = .data$BroodIDLaid) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())

  return(Individual_data_temp)

}


#' Create location data table for Bergen, Norway.
#'
#' @param nest_data Data frame of nest data from Bergen, Norway.
#'
#' @return A data frame.

create_location_BRG <- function(nest_data) {

  ## Build location data based on nest data
  Location_data_temp <- nest_data %>%

    ## Summarize information for each nest box
    ## TODO: Check that nest boxes have not been removeds
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::mutate(NestboxID = .data$LocationID,
                  LocationType = "NB",
                  StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                  EndSeason = NA_integer_,
                  Latitude = 60.25,
                  Longitude = 5.26) %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$LocationID, .keep_all = TRUE) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}
