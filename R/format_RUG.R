#'Construct standard format for data from Groningen, Netherlands
#'
#'A pipeline to produce the standard format for the nest box population in Groningen, Netherlands, administered by Christiaan Both
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Pied flycatcher
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_RUG <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
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

  ## Start time
  start_time <- Sys.time()

  ## Options
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)

  message("Importing primary data...")

  ## Determine operating system
  OS <- tolower(utils::sessionInfo()$running)

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/RUG/data/RUG_RijksuniversiteitGroningen_Netherlands/"

  ### N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  ## Connect to the RUG database
  if(grepl(pattern = 'mac', x = OS)){
    db_file <- paste0(db, "/RUG_PrimaryData.accdb")
    connection <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = paste0("Driver={Actual Access};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  } else if(grepl(pattern = 'windows', x = OS)){
    db_file <- paste0(db, "\\RUG_PrimaryData.mdb")
    connection <- DBI::dbConnect(drv = odbc::odbc(),
                                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  }

  ## Read in experimentID table
  expID_table <- read.csv(paste0(db, "./RUG_experimentID_table.csv"))

  ## Read in primary data on brood information
  brood_data <- dplyr::tbl(connection, "SPI breeding data") %>%
    tibble::as_tibble() %>%

    ## TODO: Check on hatch dates (GBD)
    dplyr::select(BreedingSeason = .data$JA,
                  Species = .data$ST,
                  Plot = .data$Terreinnummer,
                  LocationID = .data$Nestkastnummer,
                  ClutchType_observed = .data$LE,
                  LayDate_observed = .data$LBG,
                  LayDate_max = .data$AL,
                  ClutchSize_observed = .data$LG,
                  HatchDate_observed = .data$GBD,
                  BroodSize_observed = .data$GB,
                  BroodSize_min = .data$AG,
                  NumberFledged_observed = .data$UIT,
                  FledgeDate_observed = .data$UVD,
                  FemaleID = .data$VROUW,
                  MaleID = .data$MAN,
                  ExperimentID = .data$Manipulation) %>%

    ## TODO: Check about handling uncertainty in dates in lay, hatchm and fledge
    dplyr::mutate(BreedingSeason = as.integer(.data$BreedingSeason),
                  PopID = "RUG",

                  ## TODO: Ask about LocationID (some have a suffix of .0 or .1)
                  ## Remove .X suffix from LocationID, but keep in a separate column.
                  ## Any location ID without a 'brood event' is assigned a 0.
                  Brood_event = tidyr::replace_na(
                    sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 2),
                    "0"),
                  LocationID = sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 1),

                  Species = dplyr::case_when(.data$Species == "0" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "1" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                             .data$Species == "2" ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                             .data$Species == "3" ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "4" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                             .data$Species == "5" ~ species_codes[species_codes$SpeciesID == 14400,]$Species,
                                             .data$Species == "6" ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                             .data$Species == "7" ~ species_codes[species_codes$SpeciesID == 15980,]$Species,
                                             .data$Species == "8" ~ species_codes[species_codes$SpeciesID == 10200,]$Species),

                  ## TODO: Check with Liam about clutch types - they have many categories
                  ClutchType_observed = dplyr::case_when(.data$ClutchType_observed == 0L ~ "first",
                                                         .data$ClutchType_observed == 1L ~ "replacement",
                                                         .data$ClutchType_observed == 2L ~ "second",
                                                         .data$ClutchType_observed == 3L ~ "second",
                                                         .data$ClutchType_observed == 4L ~ "replacement",
                                                         .data$ClutchType_observed == 5L ~ "third",
                                                         .data$ClutchType_observed == 6L ~ "replacement",
                                                         .data$ClutchType_observed == 7L ~ "second",
                                                         .data$ClutchType_observed == 8L ~ "replacement",
                                                         TRUE ~ NA_character_),

                  LayDate_observed = dplyr::case_when(.data$LayDate_observed == -99 ~ lubridate::NA_Date_,
                                                      .data$LayDate_observed > 500 ~ lubridate::NA_Date_,
                                                      TRUE ~ as.Date(.data$LayDate_observed,
                                                                     origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),

                  LayDate_max = .data$LayDate_observed + ifelse(.data$LayDate_max != -1, .data$LayDate_max, 0),
                  HatchDate_observed = dplyr::case_when(.data$HatchDate_observed == -1 ~ lubridate::NA_Date_,
                                                        .data$HatchDate_observed > 500 ~ lubridate::NA_Date_,
                                                        TRUE ~ as.Date(.data$HatchDate_observed,
                                                                       origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  FledgeDate_observed = dplyr::case_when(.data$FledgeDate_observed == 0 ~ lubridate::NA_Date_,
                                                         .data$FledgeDate_observed > 500 ~ lubridate::NA_Date_,
                                                         TRUE ~ as.Date(.data$FledgeDate_observed,
                                                                        origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),

                  ## Set unknown IDs to NA
                  dplyr::across(c(.data$FemaleID,
                                  .data$MaleID),
                                ~dplyr::case_when(stringr::str_detect(., "^[:alpha:]{2}\\.[:digit:]{5}$") ~ .,
                                                  TRUE ~ NA_character_)),

                  ## Set to integers
                  across(c(ClutchSize_observed,
                           BroodSize_observed,
                           NumberFledged_observed,
                           BroodSize_min), as.integer),

                  ## Set -1, 98, and 99 to NA in clutch/brood/fledge counts
                  across(c(ClutchSize_observed,
                           BroodSize_observed,
                           NumberFledged_observed,
                           BroodSize_min), ~dplyr::na_if(., c(-1))),
                  across(c(ClutchSize_observed,
                           BroodSize_observed,
                           NumberFledged_observed,
                           BroodSize_min), ~dplyr::na_if(., c(98))),
                  across(c(ClutchSize_observed,
                           BroodSize_observed,
                           NumberFledged_observed,
                           BroodSize_min), ~dplyr::na_if(., c(99)))) %>%

    ## Create BroodID
    dplyr::arrange(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$LayDate_observed) %>%

    dplyr::group_by(.data$BreedingSeason, .data$Plot, .data$LocationID) %>%
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$Plot, .data$LocationID, 1:dplyr::n(), sep = "-")) %>%

    dplyr::select(dplyr::contains(names(brood_data_template)), dplyr::everything()) %>%
    tibble::as_tibble()


  ## Read in primary data from ringing records
  rr_data <- dplyr::tbl(connection, "SPI catch data") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(BreedingSeason = as.integer(.data$JR),
                     PopID = "RUG",
                     Species = dplyr::case_when(.data$S == "0" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$S == "1" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$S == "2" ~ species_codes[species_codes$SpeciesID == 13490,]$Species,
                                                .data$S == "3" ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                                .data$S == "4" ~ species_codes[species_codes$SpeciesID == 14790,]$Species,
                                                .data$S == "5" ~ species_codes[species_codes$SpeciesID == 14400,]$Species,
                                                .data$S == "6" ~ species_codes[species_codes$SpeciesID == 11220,]$Species,
                                                .data$S == "7" ~ species_codes[species_codes$SpeciesID == 15980,]$Species,
                                                .data$S == "8" ~ species_codes[species_codes$SpeciesID == 10200,]$Species),
                     IndvID = dplyr::case_when(stringr::str_detect(.data$RNR,  "^[:alpha:]{2}\\.[:digit:]{5}$") ~ .data$RNR,
                                               TRUE ~ NA_character_),

                     ## TODO: Ask about age codes
                     Age_observed = dplyr::case_when(.data$LTW == 0L ~ NA_integer_,
                                                     .data$LTW == 1L ~ 1L,
                                                     .data$LTW == 2L ~ 4L,
                                                     .data$LTW == 3L ~ 3L,
                                                     .data$LTW == 4L ~ 4L,
                                                     .data$LTW == 5L ~ 5L,
                                                     .data$LTW == 6L ~ 6L,),

                     ## TODO: Ask about Terreinnummer
                     Plot = .data$Terreinnummer,

                     ## TODO: Ask about LocationID (some have a suffix of .0 or .1)
                     ## Remove .X suffix from LocationID, but keep in a separate column.
                     ## Any location ID without a 'brood event' is assigned a 0.
                     LocationID = sapply(stringr::str_split(.data$Plaatsnummer, pattern =  "\\.", n = 2), `[`, 1),
                     Brood_event = tidyr::replace_na(
                       sapply(stringr::str_split(.data$Plaatsnummer, pattern =  "\\.", n = 2), `[`, 2),
                       "0"),

                     ## TODO: Ask about sex codes, currently assuming 1 is M and 2 is F based on body size
                     Sex_observed = dplyr::case_when(.data$SW  == 0L ~ NA_character_,
                                                     .data$SW  == 1L ~ "M",
                                                     .data$SW  == 2L ~ "F"),

                     ## TODO: Check about molecular sexing, only two non-0 cases
                     Sex_genetic = dplyr::case_when(.data$SP  == 0L ~ NA_character_,
                                                    .data$SP  == 1L ~ "M",
                                                    .data$SP  == 2L ~ "F"),

                     CaptureDate = lubridate::dmy(paste(.data$DAG, .data$MND, .data$JR, sep = "-"),
                                                  quiet = T),
                     CaptureTime = format(strptime(.data$TYD, format = "%I%M"), "%H:%M"),

                     ## TODO: Check on units of mass
                     Mass = round(as.numeric(.data$GEW/100),1),

                     ## TODO: Check on which wing measurement to use and
                     WingLength = as.numeric(.data$LVL2)/10,

                     ## TODO: Check on tarsus measurements protocol. To give each individual a value, setting 0s to NA, and averaging two measurements
                     Tarsus1 = dplyr::na_if(.data$TARS1, 0)/10,
                     Tarsus2 = dplyr::na_if(.data$TARS2, 0)/10,

                     ## Replace 0 with NA in measurement columns
                     dplyr::across(c(.data$Mass,
                                     .data$WingLength,
                                     .data$Tarsus1,
                                     .data$Tarsus2), ~dplyr::na_if(., 0)),

                     ## TODO: Check on negative ages, currently setting to NA
                     ChickAge = dplyr::case_when(.data$pullusage < 0 ~ NA_integer_,
                                                 TRUE ~ as.integer(.data$pullusage)),
                     ObserverID = .data$SYS,
                     experiment_number = .data$MANIP,
                     Oringal_nest_box = .data$HER,
                     ReleasePlot = .data$'Plot release',

                     ## TODO: Check about capture and release alive assignments. It is surprising that none of the birds that died have ExperimentIDs attached.
                     CaptureAlive = dplyr::case_when(.data$Lijk == 1 & is.na(.data$MANIP) ~ FALSE,
                                                     .data$DL == 0 ~ TRUE,
                                                     .data$DL == 1 ~ FALSE,
                                                     .data$DL == 2 ~ FALSE,
                                                     TRUE ~ TRUE),
                     ReleaseAlive = dplyr::case_when(.data$CaptureAlive == FALSE ~ FALSE,
                                                     .data$Lijk == 1 ~ FALSE,
                                                     .data$DL == 4 ~ FALSE,
                                                     .data$DL == 5 ~ FALSE,
                                                     .data$DL == 7 ~ FALSE,
                                                     .data$DL == 8 ~ FALSE,
                                                     .data$DL == 10 ~ FALSE,
                                                     .data$DL == 11 ~ FALSE,
                                                     .data$DL == 12 ~ FALSE,
                                                     .data$DL == 14 ~ FALSE,
                                                     .data$DL == 20 ~ FALSE,
                                                     TRUE ~ TRUE)) %>%

    ## TODO: Check about classifying experiment groups and fill in the table. Will join based on ExperimentID numbers
    dplyr::left_join(expID_table,
                     by = "experiment_number") %>%
    dplyr::select(-.data$experiment_number) %>%

    ## Calculate average tarsus for each observation
    dplyr::rowwise() %>%
    dplyr::mutate(Tarsus = dplyr::na_if(round(mean(c(.data$Tarsus1, .data$Tarsus2), na.rm = T),1),"NaN")) %>%
    dplyr::select(-.data$Tarsus1, -.data$Tarsus2) %>%

    ## Create BroodID
    dplyr::group_by(.data$BreedingSeason, .data$Plot, .data$LocationID) %>%
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$Brood_event, sep = "-")) %>%

    dplyr::arrange(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$CaptureDate) %>%
    dplyr::select(dplyr::contains(names(capture_data_template)), dplyr::everything()) %>%
    tibble::as_tibble()


  # BROOD DATA

  message("Compiling brood information...")

  Brood_data_temp <- create_brood_RUG(brood_data, rr_data)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data_temp <- create_capture_RUG(Brood_data_temp, rr_data)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data_temp <- create_individual_RUG(Capture_data_temp)


  # LOCATION DATA

  message("Compiling location information...")

  Location_data_temp <- create_location_RUG(Brood_data_temp, nest_coord_data)

  #Disconnect from database
  DBI::dbDisconnect(connection)

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

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  if (output_type == 'csv') {

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_RUG.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_RUG.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_RUG.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_RUG.csv"), row.names = F)

    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}

#' Create brood data table for Groningen, The Netherlands.
#'
#' @param brood_data Brood data compiled from primary data from Groningen, The Netherlands.
#'
#' @param rr_data Ringing data compiled from primary data from Groningen, The Netherlands.
#'
#' @return A data frame.

create_brood_RUG   <- function(brood_data, rr_data) {

  ## Summarize chick data form ringing records
  rr_nest_sum <- rr_data %>%

    ## Only keeping chicks
    dplyr::filter(.data$Age_observed == 1) %>%

    ## For each brood, get summary stats
    dplyr::group_by(.data$BreedingSeason, .data$Plot, .data$LocationID, .data$Brood_event, .data$BroodID) %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 12, 16)) %>%
    dplyr::summarise(AvgChickMass = round(mean(Mass, na.rm = T), 1),
                     NumberChicksMass = sum(!is.na(.data$Mass)),
                     AvgTarsus = round(mean(Tarsus, na.rm = T), 2),
                     NumberChicksTarsus = sum(!is.na(.data$Tarsus))) %>%

    ## Replace NaNs and 0 with NA
    dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::na_if(., "NaN")),
                  dplyr::across(where(is.numeric), ~dplyr::na_if(., 0)))


  ## For brood records where the 'Brood event' code is not 0, these can be joined


  ## Join to primary brood data based on BroodID
  Brood_data_temp <- brood_data %>%
    dplyr::left_join(rr_nest_sum,
                     by = "BroodID")



  2019-8-700





  ## First, identify nests with multiple breeding attempts within a single season
  ## (These all have a brood ID with suffix other than -1)
  multi_clutch_nests <- brood_data %>%
    filter(endsWith(.data$BroodID, "-[2-9]$"))



  ## Filter nests from ringing record summary that are from the second clutch
  rr_nest_sum_multi <- rr_nest_sum %>%
    filter(paste(.data$BreedingSeason,
                 .data$Plot,
                 .data$LocationID) %in%
             paste(multi_clutch_nests$BreedingSeason,
                   multi_clutch_nests$Plot,
                   multi_clutch_nests$LocationID))


  ## Then, identify nests with multiple breeding attempts within a single season
  ## (These all have a brood ID with suffix equal to -1)
  single_clutch_nests <- brood_data %>%
    filter(endsWith(.data$BroodID, "-1")) %>%

    ## Join nest summary data
    left_join(rr_nest_sum,
              by = c("BreedingSeason", "Plot", "LocationID"))










  ## Combine brood data with information from ringing records
  Brood_data_temp <- brood_data %>%

    ,

  by = "BroodID") %>%

  ## Remove NAs
  ## TODO: Check on many Species that are NAs
  dplyr::filter_at(vars(.data$PopID,
                        .data$BreedingSeason,
                        .data$Species,
                        .data$BroodID), all_vars(!is.na(.)))

return(Brood_data_temp)


}

#' Create capture data table in standard format for data from Groningen, The Netherlands.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_RUG}}.
#'
#' @param rr_data Data frame. Primary data from ringing records.
#'
#' @return A data frame.

create_capture_RUG <- function(Brood_data_temp, rr_data) {

  ## Chick captures from ringing data. Join brood data to get BroodID

  ## Process captures from ringing data which has records of all individuals
  Capture_data_temp <- rr_data %>%

    ## Create new columns
    ## TODO: Check on translocations
    dplyr::mutate(CapturePopID = .data$PopID,
                  ReleasePopID = .data$PopID,
                  CapturePlot  = .data$Plot,
                  ReleasePlot  = .data$Plot) %>%

    ## Calculate age
    dplyr::group_by(.data$IndvID) %>%
    calc_age(ID = .data$IndvID,
             Age = .data$Age_observed,
             Date = .data$CaptureDate,
             Year = .data$BreedingSeason) %>%

    dplyr::group_by(.data$IndvID) %>%

    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%

    dplyr::filter_at(vars(.data$CaptureID,
                          .data$PopID,
                          .data$BreedingSeason,
                          .data$Species,
                          .data$CaptureDate), all_vars(!is.na(.))) %>%

    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%

    dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything())

  return(Capture_data_temp)

}

#' Create individual data table in standard format for data from Groningen, The Netherlands.
#'
#' @param Capture_data_temp Data frame. Output from \code{\link{create_capture_RUG}}.
#'
#' @return A data frame.

create_individual_RUG <- function(Capture_data_temp) {

  ## Create individual data from capture data
  Individual_data_temp <- Capture_data_temp %>%

    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

    #### Format and create new data columns
    dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
    dplyr::mutate(PopID = .data$CapturePopID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                           .f = ~{
                                             if(length(..1) == 0){
                                               return(NA_character_)
                                             } else if(length(..1) == 1){
                                               return(..1)
                                             } else {
                                               return("CCCCCC")
                                             }
                                           }),

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
                  RingSeason = min(.data$BreedingSeason, na.rm = T),
                  RingAge = purrr::pmap_chr(.l = list(dplyr::first(.data$Age_observed)),
                                            .f = ~{
                                              if(is.na(..1)){
                                                return("adult")
                                              } else if(..1 <= 3L){
                                                return("chick")
                                              } else if(..1 > 3L){
                                                return("adult")
                                              }
                                            }),

                  BroodIDLaid = purrr::map_chr(.x = list(unique(stats::na.omit(.data$BroodID))),
                                               .f = ~{
                                                 if(length(..1) != 1){
                                                   return(NA_character_)
                                                 } else if(length(..1) == 1){
                                                   return(..1)
                                                 }
                                               }),

                  ## No cross-fostering, so BroodIDFledge always is BroodIDLaid
                  BroodIDFledged = .data$BroodIDLaid) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())

  return(Individual_data_temp)

}

#' Create location data table in standard format for data from Groningen, The Netherlands.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_RUG}}.
#'
#' @param nest_coord_data Data frame. Primary data on nest coordinates.
#'
#' @return A data frame.

create_location_RUG <- function(Brood_data_temp, nest_coord_data) {

  ## Create location data from brood data and nest coordinates
  ## TODO: Check whether nest boxes are removed
  ## TODO: More boxes than are listed in the meta data
  ## TODO: Check on habitat type
  ## TODO: Check about funnel and captures outside of nest boxes
  Location_data_temp <- Brood_data_temp %>%
    dplyr::select(.data$Plot,
                  .data$LocationID,
                  .data$BreedingSeason) %>%
    dplyr::group_by(.data$Plot, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason)) %>%
    dplyr::mutate(PopID = "RUG",
                  NestboxID = .data$LocationID,
                  HabitatType = "mixed",
                  LocationType = "NB") %>%

    ## Join coordinates
    dplyr::left_join(nest_coord_data %>%
                       dplyr::select(-.data$Locality),
                     by = "LocationID") %>%
    na.omit() %>%

    ## Create temporary column for arranging
    dplyr::mutate(nest_num = as.integer(str_replace_all(.data$LocationID, "[:alpha:]", ""))) %>%
    dplyr::arrange(.data$Plot, .data$nest_num ) %>%
    dplyr::select(-.data$nest_num) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}
