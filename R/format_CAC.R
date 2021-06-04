#'Construct standard format for data from Can Cata, Spain
#'
#'A pipeline to produce the standard format for the nest box population in Can Cata, Spain, administered by Juan Carlos Senar
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{CaptureDate}:
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_CAC <- function(db = choose_directory(),
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

  ## Use the specified pop filter
  if(is.null(pop)){

    pop_filter <- NULL


  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE)

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/CAC/CAC_CanCatà_Spain/"

  ## Read in primary data
  cac_data <- readxl::read_xlsx(path = paste0(db, "/CAC_PrimaryData.xlsx"), guess = 5000) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat
    dplyr::mutate(BreedingSeason = as.integer(.data$Year),
                  PopID = as.character("CAC"),
                  LayDate_observed = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$LayingDate))),
                  HatchDate_observed = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$HatchingDate))),
                  FledgeDate_observed = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$FledglingDate))),

                  ## TODO: Check on meaning of parentheses around numbers
                  ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                  NumberFledged_observed = as.integer(.data$NumberFledglings),

                  ## TODO: Check on cross fostering
                  ExperimentID = dplyr::case_when(!is.na(.data$Crossfostering) ~ "COHORT; PARENTAGE"),

                  ## TODO: Check on one species that is labeled PM+PC
                  Sp = dplyr::case_when(stringr::str_squish(toupper(.data$Sp)) == "PM"  ~ species_codes[species_codes == 14640,]$Species,
                                 TRUE ~ .data$Sp),

                  ## TODO: Check on brood classifications
                  Brood = dplyr::case_when(.data$Brood == "1" ~ "first",
                                    .data$Brood == "2" ~ "second",
                                    .data$Brood == "R" ~ "replacement"),

                  ## TODO: Check if lowercase and uppercase are all the same nestboxes
                  LocationID = toupper(.data$NestBox)) %>%

    ## Rename
    dplyr::rename(Species = .data$Sp,
                  ClutchType_observed = .data$Brood,
                  FemaleID = .data$Female,
                  MaleID = .data$Male) %>%

    ## Reorder
    dplyr::select(.data$PopID,
                  .data$Species,
                  .data$BreedingSeason,
                  .data$LocationID,
                  .data$FemaleID,
                  .data$AgeF,
                  .data$MaleID,
                  .data$AgeM,
                  .data$LayDate_observed,
                  .data$HatchDate_observed,
                  .data$ClutchType_observed,
                  .data$ClutchSize_observed,
                  .data$FledgeDate_observed,
                  .data$NumberFledged_observed,
                  dplyr::everything()) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason,
                   .data$LocationID,
                   .data$LayingDate) %>%

    ## Remove columns
    dplyr::select(-.data$LayingDate,
                  -.data$HatchingDate,
                  -.data$FledglingDate,
                  -.data$PvcF,
                  -.data$PvcM,
                  -.data$Year,
                  -.data$ClutchSize,
                  -.data$NumberFledglings,
                  -.data$NestBox) %>%

    ## Create BroodID based on PopID and row number
    dplyr::ungroup() %>%
    dplyr::mutate(BroodID = dplyr::case_when(!is.na(.data$Species) ~ paste(.data$PopID, dplyr::row_number(), sep ="-")))


  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){


    cac_data <- cac_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    cac_data <- cac_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data <- create_brood_CAC(cac_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_CAC(cac_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_CAC(Capture_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_CAC(cac_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CAC.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primary data from Can Cata, Spain.
#'
#' @return A data frame.

create_brood_CAC <- function(cac_data) {

  ## Get brood data from nest data
  Brood_data <- cac_data %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%

    ## Calculate clutch type
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE))

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primary data from Can Cata, Spain.
#'
#' @return A data frame.

create_capture_CAC <- function(cac_data) {

  ## Create capture data from primary data
  ## TODO: Look into experimental groups
  ## TODO: Determine Capture times
  Capture_data <-
    cac_data %>%

    ## Pivot longer to make a row for each individual
    tidyr::pivot_longer(cols=c("FemaleID","MaleID"), names_to = "Sex_observed", values_to = "IndvID") %>%

    ## Only keep records with band numbers
    dplyr::filter(!(is.na(.data$IndvID))) %>%

    ## Remove non-band numbers
    dplyr::filter(!grepl("Anella|anella|No|no|NO",.data$IndvID)) %>%

    ## Recode sexes
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl("Female", .data$Sex_observed) ~ "F",
                                                  grepl("Male", .data$Sex_observed) ~ "M"),

                  ## TODO: Check about age codes
                  Age_observed = as.integer(dplyr::case_when(.data$Sex_observed == "F" ~ dplyr::case_when(.data$AgeF == "A" ~ "6",
                                                                                                   .data$AgeF == "Y" ~ "4"),
                                                             .data$Sex_observed == "M" ~ dplyr::case_when(.data$AgeM == "A" ~ "6",
                                                                                                   .data$AgeM == "Y" ~ "4"))),
                  CapturePopID = .data$PopID, ## Set CapturePopID based on PopID
                  ReleasePopID = .data$PopID, ## Set ReleasePopID
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%

    ## Calculate age
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason,
             showpb = F) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"))  ## Create CaptureID based on IndvID and the record number

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Can Cata, Spain.
#'
#' @param Capture_data Capture data output from Can Cata, Spain
#'
#' @return A data frame.

create_individual_CAC <- function(Capture_data){

  Individual_data <- Capture_data %>%

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

                  RingAge = purrr::pmap_chr(.l = list(dplyr::first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")  # TODO: If age observed is unknown, assuming adult. Check this assumption
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            })) %>%

    ## Distinct
    dplyr::distinct(.data$IndvID, .keep_all = T) %>%

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


#' Create location data table for great tits and blue tits in Can Cata, Spain.
#'
#' @param cac_data Data frame of primaru data from Can Cata, Spain.
#'
#' @return A data frame.

create_location_CAC <- function(cac_data) {

  ## TODO: Check on meaning of letters associated with nest boxes
  ## TODO: Check whether nest boxes have been removed
  Location_data <- cac_data %>%
    dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%
    dplyr::mutate(LocationID = factor(.data$LocationID, levels = unique(stringr::str_sort(.data$LocationID, numeric = T)))) %>%
    dplyr::arrange(.data$LocationID) %>%

    ## Keep distinct records
    dplyr::distinct(.data$BreedingSeason, .data$LocationID, .keep_all = TRUE) %>%

    ## All records should be complete: remove any incomplete cases
    tidyr::drop_na() %>%

    ## Get additional information
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationID = as.character(.data$LocationID),
                  NestboxID = .data$LocationID,
                  LocationType = "NB",
                  HabitatType = "mixed",
                  Latitude  = 45.27,
                  Longitude = 2.8) %>%

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
