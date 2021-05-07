#'Construct standard format for data from Grobla, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Grobla, Poland, administered by Mariusz Cichoń
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_GRO <- function(db = choose_directory(),
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

  ## Otherwise, use the specified pop filter
  if(is.null(pop)){

    pop_filter <- pop

  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE)

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/pipelines/GRO/data/"

  ## Read in the three separate primary data tables
  bt_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_BT_Phenology.xls"), guess = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(PopID = rep("BT", nrow(.)))
  cf_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_CF_Phenology.xls"), guess = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(PopID = rep("CF", nrow(.)))
  gt_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_GT_Phenology.xls"), guess = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(PopID = rep("GT", nrow(.)))

  ## Rbind data
  gro_data <- rbind(bt_data, cf_data, gt_data) %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename
    janitor::clean_names(case = "upper_camel") %>%
    dplyr::rename(BreedingSeason = .data$Year,
                  LocationID = .data$Box,
                  FemaleID = .data$FemaleRing,
                  MaleID = .data$MaleRing,
                  LayDate_observed = .data$LayingDate,
                  ClutchSize_observed = .data$ClutchSize,
                  HatchDate_observed = .data$HatchingDate,
                  BroodSize_observed = .data$HatchlingN,
                  NumberFledged_observed = .data$FledglingN,
                  ExperimentID = .data$Experiment,
                  ObserverID = .data$Observer,
                  PopID = .data$PopId) %>%

    ## Reformat
    ## TODO: Ask about the few cases about clutch, brood, and fledge counts that get coerced to NAs
    dplyr::mutate(BreedingSeason = as.integer(.data$BreedingSeason),
                  ClutchSize_observed = suppressWarnings(as.integer(ClutchSize_observed)),
                  BroodSize_observed = suppressWarnings(as.integer(BroodSize_observed)),
                  NumberFledged_observed = suppressWarnings(as.integer(NumberFledged_observed))) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$LocationID)


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    gro_data <- gro_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    gro_data <- gro_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_GRO(gro_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_GRO()

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_GRO()

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_GRO()

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_GRO.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_GRO.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_GRO.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_GRO.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Grobla, Poland.
#'
#' Create brood data table in standard format for Grobla, Poland.
#'
#' @param nest_data Data frame of nest data from Grobla, Poland.
#'
#' @param rr_data Data frame of ringing records from Grobla, Poland.
#'
#' @return A data frame.

create_brood_GRO <- function(gro_data) {

  ## Get brood data
  gro_data_brood_sum <- gro_data %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Summarize brood information for each nest
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID, .data$LayDate_observed) %>%

    dplyr::summarise(FemaleID = .data$FemaleID,
                     MaleID = .data$MaleID)

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Grobla, Poland.
#'
#' Create a capture data table in standard format for great tits and blue tits in Grobla, Poland.
#' @param data Data frame of modified primary data from Grobla, Poland.
#'
#' @param nest_data Data frame of nest data from Grobla, Poland.
#'
#' @param rr_data Data frame of ringing records from Grobla, Poland.
#'
#' @return A data frame.

create_capture_GRO <- function(gro_data) {

  ## Capture data from nest data
  Capture_data <- gro_data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols=c("FemaleID","MaleID"), names_to = "Sex_observed", values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Recode sexes
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                                  grepl("Male", .data$Sex_observed) ~ "M")) %>%

    dplyr::group_by(.data$PopID) %>%

    ## TODO: Check on Capture Date
    ## TODO: Check about experimental types
    dplyr::mutate(CaptureDate = case_when(!is.na(.data$LayDate_observed) ~ as.Date(.data$LayDate_observed),
                                          is.na(.data$LayDate_observed) ~ as.Date(paste0(.data$BreedingSeason, "-06-01"))),
                  CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%

    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%

    ## Calculate age
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$CapturePopID, .data$IndvID, .data$CaptureDate) %>%

    ## Add CaptureID
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"))

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Grobla, Poland.
#'
#' Create full individual data table in standard format for great tits and blue tits in Grobla, Poland.
#'
#' @param Capture_data Capture data output from Grobla, Poland
#'
#' @param Brood_data Brood data output from Grobla, Poland
#'
#' @return A data frame.

create_individual_GRO <- function(Capture_data){

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


  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Grobla, Poland.
#'
#' Create a location data table in standard format for great tits and blue tits in Grobla, Poland.
#' @param nest_data Data frame of nest data from Grobla, Poland.
#'
#' @param rr_data Data frame of ringing records from Grobla, Poland.
#'
#' @return A data frame.

create_location_GRO <- function(gro_data) {

  ## Build location data based on nest data
  ## TODO: Check whether any boxes have been removed
  Location_data <- gro_data %>%
    dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$BreedingSeason, .data$LocationID, .keep_all = TRUE) %>%

    ## All records should be complete: remove any incomplete cases
    tidyr::drop_na() %>%

    ## Get additional information
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(NestboxID = .data$LocationID,
                  LocationType = "NB",
                  HabitatType = "deciduous",
                  Latitude  = 50.06,
                  Longitude = 20.25) %>%

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
