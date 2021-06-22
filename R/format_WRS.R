<<<<<<< HEAD
#'Construct standard format for data from Warsaw, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Warsaw, Poland, administered by Marta Szulkin.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}:
#'
#'\strong{IndvID}:
#'
#'\strong{CaptureDate}:
#'
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_WRS <- function(db = choose_directory(),
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
  options(dplyr.summarise.inform = FALSE)

  # db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/WRS/data/WAR_Warshav_Poland/"

  ## Read in primary data from nest sheet
  ## TODO: Change WAR to WRS
  nest_data <- readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"), guess = 5000, sheet = "Nests") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename columns
    dplyr::mutate(BreedingSeason = as.integer(.data$Year),
                  LocationID = as.character(.data$NestboxId),
                  Plot = as.character(.data$Site),
                  LayDate_observed = suppressWarnings(as.Date(as.numeric(.data$LayDate),
                                                              origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$Hd),
                                                                origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                  BroodSize_observed = suppressWarnings(as.integer(.data$NrHatched)),
                  NumberFledged_observed = suppressWarnings(as.integer(.data$NrFledged)),
                  Latitude = as.numeric(.data$Lat),
                  Longitude = as.numeric(.data$Long)) %>%

    ## Recode column information
    ## TODO: Check species - How to handle 'TIT'?
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "NA")),
                  PopID = "WRS",
                  dplyr::across(where(is.character), ~na_if(., "NA")),
                  Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                             .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                             .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species),
                  ExperimentID = dplyr::case_when(.data$DummyCam == 1 | .data$Camera == 1 ~ "OTHER",
                                                  TRUE ~ NA_character_),
                  NumberEggs = suppressWarnings(as.integer(.data$NrEggsWeighed)),
                  AvgEggMass = suppressWarnings(round(as.numeric(.data$EggMassTot)/.data$NumberEggs, 3))) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%

    ## Select variables of interest
    dplyr::select(.data$BreedingSeason,
                  .data$PopID,
                  .data$Plot,
                  .data$LocationID,
                  .data$Species,
                  .data$LayDate_observed,
                  .data$HatchDate_observed,
                  .data$ClutchSize_observed,
                  .data$BroodSize_observed,
                  .data$NumberFledged_observed,
                  .data$NumberEggs,
                  .data$AvgEggMass,
                  .data$ExperimentID,
                  .data$Latitude,
                  .data$Longitude,
                  .data$UniqueBreedingEvent)

  ## Read in primary data from chicks
  chick_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"),
                                                   guess = 5000,
                                                   sheet = "Chicks",
                                                   .name_repair = "minimal")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename variables
    ## TODO: Check about capture dates for dead chicks
    dplyr::rename(BreedingSeason = .data$Year,
                  Plot = .data$Site,
                  LocationID = .data$NestboxId,
                  IndvID = .data$RingId,
                  CaptureDate = .data$D15Date,
                  Tarsus = .data$TarsusD15) %>%

    ## Handling different date formats in Excel
    dplyr::mutate(CaptureDate = suppressWarnings(dplyr::case_when(grepl("-", .data$CaptureDate) ~ lubridate::dmy(.data$CaptureDate, quiet = TRUE),
                                                                  TRUE ~ janitor::excel_numeric_to_date(as.numeric(.data$CaptureDate))))) %>%

    ## Adjust variables
    ## TODO: Check about 'dead chick' ID codes
    dplyr::mutate(dplyr::across(where(is.character), ~na_if(., "NA")),
                  PopID = "WRS",
                  BreedingSeason = as.integer(BreedingSeason),
                  ReleaseAlive = dplyr::case_when(.data$ChickExp != 0 | .data$ChickPred != 0 | .data$Fledged == 0~ FALSE,
                                                  TRUE ~ TRUE),
                  Mass = suppressWarnings(round(as.numeric(dplyr::case_when(!is.na(.data$WeightD15) ~ WeightD15,
                                                                            !is.na(.data$WeightD10) ~ WeightD10,
                                                                            !is.na(.data$WeightD5)  ~ WeightD5,
                                                                            !is.na(.data$WeightD2)  ~ WeightD2,
                                                                            TRUE ~ NA_character_)),3)),
                  ChickAge = dplyr::case_when(!is.na(.data$WeightD15) ~ 15L,
                                              !is.na(.data$WeightD10) ~ 10L,
                                              !is.na(.data$WeightD5)  ~ 5L,
                                              !is.na(.data$WeightD2)  ~ 2L,
                                              TRUE ~ NA_integer_),
                  Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                             .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                             .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species)) %>%

    dplyr::select(.data$BreedingSeason,
                  .data$PopID,
                  .data$Plot,
                  .data$LocationID,
                  .data$Species,
                  .data$IndvID,
                  .data$CaptureDate,
                  .data$Mass,
                  .data$ChickAge,
                  .data$ReleaseAlive,
                  .data$UniqueBreedingEvent)

  ## Read in primary data from adults
  adult_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"),
                                                   sheet = "Adults",
                                                   col_types = "text")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename columns
    dplyr::rename(BreedingSeason = .data$Year,
                  Plot = .data$Site,
                  LocationID = .data$NestboxId,
                  IndvID = .data$RingId,
                  ObserverID = .data$Obs,
                  Sex_observed = .data$Sex,
                  Age_observed = .data$Age,
                  Mass = .data$Weight,
                  WingLength = .data$WingLenght,
                  Tarsus = .data$Tarsus) %>%

    ## Handling different date formats in Excel
    dplyr::mutate(CaptureDate = suppressWarnings(dplyr::case_when(grepl("/", .data$Date) ~ lubridate::dmy(.data$Date, quiet = TRUE),
                                                                  TRUE ~ janitor::excel_numeric_to_date(as.numeric(.data$Date))))) %>%

    ## Recode columns
    ## TODO: Add openxlsx package to dependencies
    ## TODO: Check about age codes
    ## TODO: Check about aggression scoring - Should this be an experiment? Currently listed as OTHER
    ## TODO: where() not namespaced
    mutate(dplyr::across(where(is.character), ~na_if(., "NA")),
           PopID = "WRS",
           BreedingSeason = as.integer(BreedingSeason),
           dplyr::across(c(Mass, WingLength, Tarsus), ~ suppressWarnings(as.numeric(.x))),
           CaptureTime = suppressWarnings(format(openxlsx::convertToDateTime(.data$Hour), "%H:%M")),
           Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                      .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                      .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                      .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                      .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                      .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                      .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species),
           ReleaseAlive = dplyr::case_when(.data$AdultExp == 1 ~ FALSE,
                                           TRUE ~ TRUE),
           Age_observed = dplyr::case_when(.data$Age_observed == 2 ~ 5L,
                                           toupper(.data$Age_observed) == "PO2" ~ 6L),
           ExperimentID = dplyr::case_when(!is.na(.data$Aggress) ~ "OTHER",
                                           TRUE ~ NA_character_),
           dplyr::across(where(is.character), ~na_if(., "NA"))) %>%

    select(.data$BreedingSeason,
           .data$PopID,
           .data$Plot,
           .data$LocationID,
           .data$Species,
           .data$IndvID,
           .data$CaptureDate,
           .data$CaptureTime,
           .data$Sex_observed,
           .data$Age_observed,
           .data$Mass,
           .data$WingLength,
           .data$Tarsus,
           .data$ReleaseAlive,
           .data$ObserverID,
           .data$ExperimentID,
           .data$UniqueBreedingEvent)

  ## Filter to keep only desired Species if specified
  if(!is.null(species_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    chick_data <- chick_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    adult_data <- adult_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

  }

  ## Filter to keep only desired Populations if specified
  if(!is.null(pop_filter)){

    nest_data <- nest_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    chick_data <- chick_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    adult_data <- adult_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

  }

  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_WRS(nest_data, chick_data, adult_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_WRS(chick_data, adult_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_WRS(Capture_data_temp, Brood_data_temp)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_WRS(nest_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template))

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)


  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)


  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[,!(names(individual_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)


  ## Location data
  Location_data <- Location_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[,!(names(location_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)


  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WRS.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param chick_data Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_brood_WRS <- function(nest_data, chick_data, adult_data) {

  ## Combine primary data
  ## TODO: Check on tarsus method
  ## TODO: Check on clutch type observed
  Brood_data_temp <- nest_data %>%

    ## Keep only records with sufficient information
    filter(!is.na(.data$UniqueBreedingEvent) & !is.na(.data$Species)) %>%

    dplyr::left_join(adult_data %>%
                       dplyr::select(.data$UniqueBreedingEvent,
                                     .data$Sex_observed,
                                     .data$IndvID) %>%
                       na.omit() %>%

                       ## A few cases where the same individuals were caught multiple times for a single breeding event
                       ## Keeping only distinct records by breeding event and sex
                       ## TODO: Check about whether this is robust
                       dplyr::distinct(.data$UniqueBreedingEvent, .data$Sex_observed, .keep_all = T) %>%
                       tidyr::pivot_wider(id_cols = .data$UniqueBreedingEvent,
                                          values_from = .data$IndvID,
                                          names_from = .data$Sex_observed) %>%
                       dplyr::rename(FemaleID = "F",
                                     MaleID = "M"),
                     by = "UniqueBreedingEvent") %>%

    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%

    ## Create BroodID
    dplyr::mutate(BroodID = paste(.data$Plot, 1:n(), sep = "-")) %>%

    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data =. , protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Reorder columns
    dplyr::select(any_of(names(brood_data_template)), everything())

  return(Brood_data_temp)

}

#' Create capture data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param chick_data, Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data, Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_capture_WRS <- function(chick_data, adult_data) {

  ## All chicks with IndvIDs containing 'XX' died before fledging
  ## TODO: Check on dropping these, they currently don't have CaptureDates
  Capture_data_temp <- adult_data %>%
    dplyr::mutate(RingAge_temp = "adult",
                  CaptureAlive = TRUE) %>%

    ## Bind chick data
    dplyr::bind_rows(chick_data %>%
                       dplyr::mutate(RingAge_temp = "chick",
                                     CaptureAlive = dplyr::case_when(grepl("XX",.data$IndvID) & is.na(.data$Mass) ~ FALSE,
                                                                     TRUE ~ TRUE),
                                     Age_observed = 1L)) %>%

    ## Create new columns
    dplyr::mutate(CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CapturePlot = .data$Plot,
                  ReleasePlot = .data$Plot) %>%

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
    dplyr::select(any_of(names(capture_data_template)), everything())


  return(Capture_data_temp)

}

#' Create individual table for great tits and blue tits in Warsaw, Poland.
#'
#' @param Capture_data_temp Capture data output from Warsaw, Poland
#'
#' @param Brood_data_temp Brood data output from Warsaw, Poland
#'
#' @return A data frame.

create_individual_WRS <- function(Capture_data_temp, Brood_data_temp){

  ## TODO: Add nest KPN46 in 2016 to test data
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

    ## Create dummy variable to use in joining Brood data for Individuals banded as chicks
    dplyr::mutate(brood_record = dplyr::case_when(.data$RingAge == "chick" &
                                                    .data$RingSeason == .data$BreedingSeason &
                                                    !is.na(.data$LocationID) ~ "yes",
                                                  TRUE ~ NA_character_)) %>%

    ## Only join BroodID to chick records
    dplyr::left_join(Brood_data_temp %>%
                       dplyr::mutate(brood_record = "yes") %>%
                       dplyr::select(.data$brood_record,
                                     .data$UniqueBreedingEvent,
                                     .data$BroodID),
                     by = c("brood_record", "UniqueBreedingEvent")) %>%

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
    dplyr::select(any_of(names(individual_data_template)), everything())


  return(Individual_data_temp)

}


#' Create location data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @return A data frame.

create_location_WRS <- function(nest_data) {

  ## Build location data based on nest data
  Location_data_temp <- nest_data %>%
    dplyr::select(.data$BreedingSeason, .data$PopID, .data$Plot, .data$LocationID, .data$Latitude, .data$Longitude) %>%
    dplyr::filter(!is.na(.data$LocationID)) %>%

    ## Keep distinct records
    dplyr::distinct(.data$PopID, .data$BreedingSeason, .data$LocationID, .keep_all = TRUE) %>%

    ## All records shows be complete: remove any incomplete cases
    tidyr::drop_na() %>%

    ## Get additional information
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::mutate(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                  EndSeason = NA_integer_,
                  NestboxID = .data$LocationID,
                  LocationType = "NB",

                  ## TODO: Match vegetation with plot name
                  HabitatType = dplyr::case_when(.data$Plot == "CMZ" ~ "urban",
                                                 .data$Plot == "KPN" ~ "urban",
                                                 .data$Plot == "POL" ~ "urban",
                                                 .data$Plot == "LOL" ~ "urban",
                                                 .data$Plot == "MUR" ~ "urban",
                                                 .data$Plot == "OLO" ~ "urban",
                                                 .data$Plot == "PAL" ~ "urban",
                                                 .data$Plot == "UNI" ~ "urban",
                                                 .data$Plot == "BIB" ~ "urban"))


  return(Location_data_temp)

}
|||||||
=======
#'Construct standard format for data from Warsaw, Poland
#'
#'A pipeline to produce the standard format for the nest box population in Warsaw, Poland, administered by Marta Szulkin.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}:
#'
#'\strong{IndvID}:
#'
#'\strong{CaptureDate}:
#'
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_WRS <- function(db = choose_directory(),
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
  options(dplyr.summarise.inform = FALSE)

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/WRS/data/WAR_Warshav_Poland/"

  ## Read in primary data from nest sheet
  ## TODO: Change WAR to WRS
  nest_data <- readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"), guess = 5000, sheet = "Nests") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Reformat and rename columns
    dplyr::mutate(BreedingSeason = as.integer(.data$Year),
                  LocationID = as.character(.data$NestboxId),
                  Plot = as.character(.data$Site),
                  LayDate_observed = suppressWarnings(as.Date(as.numeric(.data$LayDate),
                                                              origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$Hd),
                                                                origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  ClutchSize_observed = suppressWarnings(as.integer(.data$ClutchSize)),
                  BroodSize_observed = suppressWarnings(as.integer(.data$NrHatched)),
                  NumberFledged_observed = suppressWarnings(as.integer(.data$NrFledged)),
                  Latitude = as.numeric(.data$Lat),
                  Longitude = as.numeric(.data$Long)) %>%

    ## Recode column information
    ## TODO: Check species - How to handle 'TIT'?
    dplyr::mutate(PopID = "WRS",
                  Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                             .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                             .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species),
                  ExperimentID = dplyr::case_when(.data$DummyCam == 1 | .data$Camera == 1 ~ "OTHER",
                                                  TRUE ~ NA_character_),
                  NumberEggs = suppressWarnings(as.integer(.data$NrEggsWeighed)),
                  AvgEggMass = suppressWarnings(round(as.numeric(.data$EggMassTot)/.data$NumberEggs, 3))) %>%

    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%

    ## Select variables of interest
    dplyr::select(.data$BreedingSeason,
                  .data$PopID,
                  .data$Plot,
                  .data$LocationID,
                  .data$Species,
                  .data$LayDate_observed,
                  .data$HatchDate_observed,
                  .data$ClutchSize_observed,
                  .data$BroodSize_observed,
                  .data$NumberFledged_observed,
                  .data$NumberEggs,
                  .data$AvgEggMass,
                  .data$ExperimentID,
                  .data$Latitude,
                  .data$Longitude)

  ## Read in primary data from chicks
  chick_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"),
                                                   guess = 5000,
                                                   sheet = "Chicks",
                                                   .name_repair = "minimal")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename variables
    ## TODO: Check about capture dates for dead chicks
    dplyr::rename(BreedingSeason = .data$Year,
                  Plot = .data$Site,
                  LocationID = .data$NestboxId,
                  IndvID = .data$RingId,
                  CaptureDate = .data$D15Date,
                  Tarsus = .data$TarsusD15) %>%

    ## Adjust variables
    ## TODO: Check about 'dead chick' ID codes
    dplyr::mutate(PopID = "WRS",
                  ReleaseAlive = dplyr::case_when(.data$ChickExp != 0 | .data$ChickPred != 0 | .data$Fledged == 0~ FALSE,
                                                  TRUE ~ TRUE),
                  Mass = suppressWarnings(round(as.numeric(dplyr::case_when(!is.na(.data$WeightD15) ~ WeightD15,
                                                                            !is.na(.data$WeightD10) ~ WeightD10,
                                                                            !is.na(.data$WeightD5)  ~ WeightD5,
                                                                            !is.na(.data$WeightD2)  ~ WeightD2,
                                                                            TRUE ~ NA_character_)),3)),
                  ChickAge = dplyr::case_when(!is.na(.data$WeightD15) ~ 15L,
                                              !is.na(.data$WeightD10) ~ 10L,
                                              !is.na(.data$WeightD5)  ~ 5L,
                                              !is.na(.data$WeightD2)  ~ 2L,
                                              TRUE ~ NA_integer_),

                  Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                             .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                             .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species)) %>%

    dplyr::select(.data$BreedingSeason,
                  .data$PopID,
                  .data$Plot,
                  .data$LocationID,
                  .data$Species,
                  .data$IndvID,
                  .data$CaptureDate,
                  .data$Mass,
                  .data$ChickAge,
                  .data$ReleaseAlive)


  ## Read in primary data from adults
  adult_data <- suppressWarnings(readxl::read_xlsx(path = paste0(db, "/WAR_PrimaryData.xlsx"),
                                                   sheet = "Adults")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename columns
    dplyr::rename(BreedingSeason = .data$Year,
                  Plot = .data$Site,
                  LocationID = .data$NestboxId,
                  IndvID = .data$RingId,
                  ObserverID = .data$Obs,
                  CaptureDate = .data$Date,
                  Sex_observed = .data$Sex,
                  Age_observed = .data$Age,
                  Mass = .data$Weight,
                  WingLength = .data$WingLenght,
                  Tarsus = .data$Tarsus) %>%

    ## Recode columns
    ## TODO: Add openxlsx package to dependencies
    ## TODO: Check about age codes
    mutate(PopID = "WRS",
           CaptureTime = suppressWarnings(format(openxlsx::convertToDateTime(.data$Hour), "%H:%M")),
           Species = dplyr::case_when(.data$Species == "GT"  ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                      .data$Species == "BT"  ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                      .data$Species == "FC"  ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                      .data$Species == "NUT" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                      .data$Species == "CT"  ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                      .data$Species == "RS"  ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                      .data$Species == "SP"  ~ species_codes[species_codes$SpeciesID == 15980,]$Species),
           ReleaseAlive = dplyr::case_when(.data$AdultExp == 1 ~ FALSE,
                                           TRUE ~ TRUE),
           Age_observed = dplyr::case_when(.data$Age_observed == 2 ~ 5,
                                           toupper(.data$Age_observed) == "PO2" ~ 6)) %>%

    select(.data$BreedingSeason,
           .data$PopID,
           .data$Plot,
           .data$LocationID,
           .data$IndvID,
           .data$CaptureDate,
           .data$CaptureTime,
           .data$Sex_observed,
           .data$Age_observed,
           .data$Mass,
           .data$WingLength,
           .data$Tarsus,
           .data$ReleaseAlive,
           .data$ObserverID)


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
  Brood_data <- create_brood_WRS(nest_data, adult_data, nest_data)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data <- create_capture_WRS(nest_data, adult_data, nest_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data <- create_individual_WRS(nest_data, adult_data, nest_data)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data <- create_location_WRS(nest_data, adult_data, nest_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WRS.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WRS.csv"), row.names = F)

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


#' Create brood data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param rr_data Data frame of ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_brood_WRS <- function(nest_data, rr_data) {


    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[,!(names(brood_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%


  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)

  return(Brood_data)

}

#' Create capture data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param rr_data Data frame of ringing records from Warsaw, Poland.
#'
#' @param Brood_data Data frame of Brood data in standard format from Warsaw, Poland.
#'
#' @return A data frame.

create_capture_WRS <- function(nest_data, rr_data, Brood_data) {



    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[,!(names(capture_data_template) %in% names(.))]) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template))



  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)

  return(Capture_data)

}

#' Create individual table for great tits and blue tits in Warsaw, Poland.
#'
#' @param Capture_data Capture data output from Warsaw, Poland
#'
#' @param Brood_data Brood data output from Warsaw, Poland
#'
#' @return A data frame.

create_individual_WRS <- function(Capture_data, Brood_data){

  Individual_data_temp <- Capture_data %>%

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
                                            }))


  ## Get chicks and join BroodID to these records
  ## A couple of duplicates are created when there are replacement clutches.
  ## To avoid this, only the last brood record for each LocationID is kept (which will be replacement clutches that may actually result in fledglings) since apparently there are no double clutches in these populations.
  ## This means that any banded chicks will from the information associated with the last brood.
  Individual_data <- Individual_data_temp %>%

    ## Filter to keep only records of individuals banded as chicks, the first record of that individual, and records where LocationID is known
    dplyr::filter(.data$RingAge == "chick" & .data$RingSeason == .data$BreedingSeason & !is.na(.data$LocationID)) %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::filter(!is.na(.data$LocationID)) %>%
                       dplyr::group_by(.data$BreedingSeason, .data$PopID, .data$LocationID) %>%
                       dplyr::filter(.data$BroodID == dplyr::last(.data$BroodID)) %>%
                       dplyr::select(.data$BreedingSeason, .data$PopID, .data$LocationID, .data$BroodID) %>%
                       dplyr::rename(BroodIDLaid = .data$BroodID),
                     by = c("BreedingSeason", "PopID", "LocationID")) %>%


    ## Add back in filtered records
    dplyr::bind_rows(Individual_data_temp %>%
                       dplyr::filter(.data$RingAge == "adult" | is.na(.data$LocationID) | (.data$RingSeason != .data$BreedingSeason))) %>%

    ## Add BroodID information
    ## BroodIDFledged is the same as BroodIDLaid unless ExperimentID is COHORT or PARENTAGE
    ## In these cases, the origin of eggs/nestlings is unknown and so the BroodIDLaid is unknown
    ## BroodIDFledged however can still be determined based on the BroodID of the nest
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(BroodIDFledged = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodIDLaid))),
                                                  .f = ~{
                                                    if(length(..1) == 0){
                                                      return(NA_character_)
                                                    } else if(length(..1) == 1){
                                                      return(..1)
                                                    }
                                                  }),
                  BroodIDLaid = case_when(grepl("COHORT|PARENTAGE", .data$ExperimentID) ~ NA_character_,
                                          TRUE ~ .data$BroodIDLaid)) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%

    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%

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


#' Create location data table for great tits and blue tits in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param rr_data Data frame of ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_location_WRS <- function(nest_data, rr_data) {

  ## Build location data based on ringing recovery data first
  ## Then join nest data
  ## No nest boxes have been removed
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
>>>>>>> WRS_pipeline
