#'Construct standard format for data from Grobla, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Grobla, Poland, administered by Mariusz Cichoń
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'#'\strong{CaptureDate}: Adults are typically captured two weeks into the nestling period.
#'If hatch date is not known for the nest, CaptureDate is set to two weeks after May 14th, which is the average hatch date.
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

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

  start_time <- Sys.time()

  ## Set options
  options(dplyr.summarise.inform = FALSE)

  ## Read in the three separate primary data tables
  bt_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_BT.xls"), guess_max = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(Species = rep(species_codes[species_codes$SpeciesID == 14620,]$Species, nrow(.)))
  cf_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_CF.xls"), guess_max = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(Species = rep(species_codes[species_codes$SpeciesID == 13480,]$Species, nrow(.)))
  gt_data <- nest_data <- readxl::read_excel(path = paste0(db, "/GRO_PrimaryData_GT.xls"), guess_max = 5000, range = readxl::cell_cols("A:L")) %>%
    dplyr::mutate(Species = rep(species_codes[species_codes$SpeciesID == 14640,]$Species, nrow(.)))

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
                  ObserverID = .data$Observer) %>%

    ## Reformat
    ## Few cases of clutch size observed have '?' - these should be considered the minimum clutch size, otherwise on information provided on minimum clutch size
    dplyr::mutate(PopID = "GRO",
                  LocationID = toupper(LocationID),
                  BreedingSeason = as.integer(.data$BreedingSeason),
                  LayDate_observed =  as.Date(.data$LayDate_observed),
                  HatchDate_observed =  as.Date(.data$HatchDate_observed),
                  ClutchSize_min = dplyr::case_when(grepl("?", .data$ClutchSize_observed, fixed = TRUE) ~ as.integer(sub("?","",.data$ClutchSize_observed, fixed = TRUE)),
                                                    TRUE ~ NA_integer_),
                  ClutchSize_observed = suppressWarnings(as.numeric(gsub("?", replacement = "", x = .data$ClutchSize_observed, fixed = TRUE))),
                  BroodSize_observed = suppressWarnings(as.numeric(gsub("?", replacement = "", x = .data$BroodSize_observed, fixed = TRUE))),
                  NumberFledged_observed = suppressWarnings(as.numeric(gsub("?", replacement = "", x = .data$NumberFledged_observed, fixed = TRUE))),
                  ## Information on clutch type recorded opportunistically in the Notes - when there is a note, label
                  ClutchType_observed = dplyr::case_when(grepl("First|first", .data$Notes, fixed = FALSE) ~ "first",
                                                         grepl("Repeated|second|secend|Second", .data$Notes, fixed = FALSE) ~ "second",
                                                         TRUE ~ NA_character_),

                  ## Remove whitespace from IDs
                  FemaleID = gsub(" ", "", .data$FemaleID),
                  MaleID = gsub(" ", "", .data$MaleID)) %>%

    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$LocationID) %>%

    ## Set non-conforming IDs to NA (as of 2020 there are 2 in FemaleID: KP6855, LE3665)
    dplyr::mutate(dplyr::across(c(.data$FemaleID, .data$MaleID),
                                ~dplyr::case_when(stringr::str_detect(., "^[[:digit:][:alpha:]]{7}$") ~ .,
                                                  TRUE ~ NA_character_)))


  ## Read in experiment table
  exp_table <- readr::read_csv(paste0(db, "/GRO_PrimaryData_ExperimentLabels.csv"))

  ## Join in experiment labels
  gro_data <- gro_data %>%
    dplyr::rename(Experiment_Treatment = .data$Experiment) %>%
    dplyr::left_join(exp_table, by = c("BreedingSeason","Species","Experiment_Treatment"))

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
  Capture_data <- create_capture_GRO(gro_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_GRO(Capture_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_GRO(gro_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_GRO.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_GRO.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_GRO.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_GRO.csv"), row.names = FALSE)

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
#' @param gro_data Data frame of modified primary data from Grobla, Poland.
#'
#' @return A data frame.

create_brood_GRO <- function(gro_data) {

  ## Get brood data
  Brood_data <- gro_data %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Summarize brood information for each nest
    dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID, .data$LayDate_observed) %>%

    dplyr::mutate(FemaleID = .data$FemaleID,
                  MaleID = .data$MaleID)  %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0, !(names(brood_data_template) %in% names(.))]  %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%

    ## Remove any NAs from essential columns
    dplyr::filter(!is.na(.data$PopID),
                  !is.na(.data$BreedingSeason),
                  !is.na(.data$Species)) %>%

    ## Calculate clutch type
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Calculating BroodID if Species is known
    dplyr::mutate(BroodID = dplyr::case_when(!is.na(Species) ~ paste(.data$PopID, dplyr::row_number(), sep ="-"))) %>%

    ## Remove any cases without a BroodID
    dplyr::filter(!is.na(.data$BroodID))



  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Grobla, Poland.
#'
#' @param gro_data Data frame of modified primary data from Grobla, Poland.
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

    ## Capture date is generally two weeks after hatching
    ## If hatch date is not known, then May 14th of that year is used
    dplyr::group_by(.data$IndvID, .data$BreedingSeason) %>%
    dplyr::mutate(CaptureDate = dplyr::case_when(!is.na(.data$HatchDate_observed) ~ as.Date(.data$HatchDate_observed) + 14,
                                                 is.na(.data$HatchDate_observed) ~ as.Date(paste0(.data$BreedingSeason, "-05-28"))),
                  CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%

    dplyr::ungroup() %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))]  %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%

    ## Calculate age
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason,
             showpb = FALSE) %>%

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
#' @param Capture_data Capture data output based on modified primary data from Grobla, Poland
#'
#' @return A data frame.

create_individual_GRO <- function(Capture_data){

  Individual_data <- Capture_data %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID) %>%

    dplyr::mutate(PopID = .data$CapturePopID,
                  RingSeason = min(.data$BreedingSeason),
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

                  RingAge = purrr::pmap_chr(.l = list(first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            })) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0, !(names(individual_data_template) %in% names(.))]  %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))


  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

  return(Individual_data)

}


#' Create location data table for great tits and blue tits in Grobla, Poland.
#'
#' @param gro_data Data frame of modified primary data from Grobla, Poland.
#'
#' @return A data frame.

create_location_GRO <- function(gro_data) {

  ## Build location data based on nest data
  ## Some boxes were removed in the early years of the study, but there is no information about which boxes or when
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
    dplyr::bind_cols(location_data_template[0, !(names(location_data_template) %in% names(.))] %>%
                       dplyr::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)

  return(Location_data)

}
