#'Construct standard format for data from Mariola, Spain
#'
#'A pipeline to produce the standard format for the nest box population in Mariola, Spain, administered by Eduardo Belda.
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

format_MAR <- function(db = choose_directory(),
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

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/MAR/data/MAR_Mariola_Spain/"

  ### N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  ## Connect to the MAR database
  if(grepl(pattern = 'mac', x = OS)){
    db_file <- paste0(db, "/MAR_PrimaryData.mdb")
    connection <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = paste0("Driver={Actual Access};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  } else if(grepl(pattern = 'windows', x = OS)){
    db_file <- paste0(db, "\\MAR_PrimaryData.mdb")
    connection <- DBI::dbConnect(drv = odbc::odbc(),
                                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  }

  ## Read in primary data on brood information
  ## TODO: Ask about Locality
  ## TODO: Check about experiment IDs
  ## TODO: Ask about 36 and 36 vieja. Different nests?
  ## TODO: Check about 45 in brood size
  brood_data <- dplyr::tbl(connection, "Nidification data") %>%
    tibble::as_tibble() %>%
    janitor::clean_names(case = "upper_camel") %>%

    ## Select and rename relevant columns
    dplyr::select(BreedingSeason = .data$Year,
                  BroodID = .data$NestCode,
                  Plot = .data$Locality,
                  LocationID = .data$Nest,
                  Species = .data$Specie,
                  ClutchType_observed = .data$ClutchType,
                  LayDate_observed = .data$Ld,
                  ClutchSize_observed = .data$Cs,
                  HatchDate_observed = .data$Hd,
                  BroodSize_observed = .data$Hs,
                  FemaleID = .data$Female,
                  MaleID = .data$Male,
                  ExperimentID = .data$Experimental,
                  .data$Radiotracking,
                  contains("Chick")) %>%

    ## Recode and reformat
    ## TODO: Check on ClutchType observed codes
    dplyr::mutate(BreedingSeason = as.integer(.data$BreedingSeason),
                  PopID = "MAR",
                  Species = dplyr::case_when(.data$Species == "Parus ater" ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                             .data$Species == "Parus major" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                             .data$Species == "Parus cristatus" ~ species_codes[species_codes$SpeciesID == 14540,]$Species,
                                             .data$Species == "Troglodytes troglodytes" ~ species_codes[species_codes$SpeciesID == 10660,]$Species,
                                             .data$Species == "Jynx torquilla" ~ species_codes[species_codes$SpeciesID == 08480,]$Species,
                                             .data$Species == "Certhia brachydactyla|Certhia brachydactilia" ~ species_codes[species_codes$SpeciesID == 14870,]$Species,
                                             .data$Species == "Parus caeruleus" ~ species_codes[species_codes$SpeciesID == 14620,]$Species),
                  LayDate_observed = as.Date(.data$LayDate_observed,
                                             origin = as.Date(paste0(.data$BreedingSeason, "-03-31"))),
                  HatchDate_observed = as.Date(.data$HatchDate_observed,
                                               origin = as.Date(paste0(.data$BreedingSeason, "-03-31"))),
                  ## Clean up LocationID
                  LocationID = toupper(gsub(",| ","", .data$LocationID)),
                  ClutchSize_observed = as.integer(ClutchSize_observed),
                  BroodSize_observed = as.integer(BroodSize_observed),
                  ClutchType_observed = dplyr::case_when(.data$ClutchType_observed == "1" ~ "first",
                                                         .data$ClutchType_observed == "2" ~ "second",
                                                         .data$ClutchType_observed == "R" ~ "replacement"),

                  ## BroodID missing in some cases. Filling in missing BroodIDs using nest - last two digits of the year - nesting attempt.
                  BroodID = dplyr::case_when(!is.na(.data$BroodID) ~ .data$BroodID,
                                             is.na(sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 2)) ~
                                               paste(sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 1),
                                                     substr(.data$BreedingSeason, 3, 4),
                                                     "1",
                                                     sep = "-"),
                                             !is.na(sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 2)) ~
                                               paste(sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 1),
                                                     substr(.data$BreedingSeason, 3, 4),
                                                     sapply(stringr::str_split(.data$LocationID, pattern =  "\\.", n = 2), `[`, 2),
                                                     sep = "-"))) %>%

    dplyr::arrange(.data$BreedingSeason, .data$LocationID)


  ## Read in primary data from ringing records
  ## TODO: Ask about Locality
  rr_data <- dplyr::tbl(connection, "Captures2013") %>%
    tibble::as_tibble(.data) %>%
    janitor::clean_names(case = "upper_camel") %>%

    ## Process relevant columns
    dplyr::transmute(PopID = "MAR",
                     BreedingSeason = as.integer(.data$Year),
                     Plot = .data$Locality,
                     IndvID = .data$Ring,
                     BroodID = .data$NestCode,
                     Species = .data$Specie,
                     BroodID = .data$NestCode,
                     Age_observed = as.integer(.data$Age),
                     Sex_observed = dplyr::case_when(grepl("F|f|Female|female", .data$Sex,) ~ "F",
                                                     grepl("M|m|Male|male", .data$Sex,) ~ "M"),
                     CaptureDate = lubridate::dmy(paste(.data$Day, .data$Month, .data$Year, sep = "-"),
                                                  quiet = T),
                     CaptureTime = format(as.POSIXct(.data$Time, format = "%H:%M"), "%H:%M"),
                     Mass = round(as.numeric(.data$Weight),1),
                     WingLength = round(as.numeric(.data$Wing),1),
                     Tarsus = round(as.numeric(.data$Tarsus),2),
                     ObserverID = iconv(.data$Ringer, from = "UTF-8", to = "ASCII", sub = ""),
                     ExperimentID = .data$Experiment,
                     Species = dplyr::case_when(.data$Species == "Parus ater" ~ species_codes[species_codes$SpeciesID == 14610,]$Species,
                                                .data$Species == "Parus major" ~ species_codes[species_codes$SpeciesID == 14640,]$Species,
                                                .data$Species == "Parus cristatus" ~ species_codes[species_codes$SpeciesID == 14540,]$Species,
                                                .data$Species == "Troglodytes troglodytes" ~ species_codes[species_codes$SpeciesID == 10660,]$Species,
                                                .data$Species == "Jynx torquilla" ~ species_codes[species_codes$SpeciesID == 8480,]$Species,
                                                .data$Species == "Certhia brachydactyla|Certhia brachydactilia" ~ species_codes[species_codes$SpeciesID == 14870,]$Species,
                                                .data$Species == "Aegithalos caudatus" ~ species_codes[species_codes$SpeciesID == 14370,]$Species,
                                                .data$Species == "Carduelis cannabina" ~ species_codes[species_codes$SpeciesID == 16600,]$Species,
                                                .data$Species == "Erithacus rubecula" ~ species_codes[species_codes$SpeciesID == 10990,]$Species,
                                                .data$Species == "Fringilla coelebs" ~ species_codes[species_codes$SpeciesID == 16360,]$Species,
                                                .data$Species == "Parus caeruleus" ~ species_codes[species_codes$SpeciesID == 13153,]$Species,
                                                .data$Species == "Regulus ignicapillus" ~ species_codes[species_codes$SpeciesID == 14620,]$Species,
                                                .data$Species == "Serinus serinus" ~ species_codes[species_codes$SpeciesID == 16400,]$Species))

  ## Read in primary data from nest locations
  nest_coord_data <- dplyr::tbl(connection, "Nest coordinates") %>%
    tibble::as_tibble(.data) %>%
    janitor::clean_names(case = "upper_camel") %>%
    na.omit() %>%
    sf::st_as_sf(coords = c("CoordinatesX", "CoordinatesY"),
                 crs = ("+proj=utm +zone=30S +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(Longitude = sf::st_coordinates(.)[,1],
                  Latitude= sf::st_coordinates(.)[,2]) %>%
    tibble::as_tibble() %>%
    dplyr::select(-.data$geometry,
                  -.data$Id) %>%
    dplyr::rename(LocationID = Nestbox)


  # BROOD DATA

  message("Compiling brood information...")

  Brood_data_temp <- create_brood_MAR(brood_data, rr_data)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data_temp <- create_capture_MAR(Brood_data_temp, rr_data)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data_temp <- create_individual_MAR(Capture_data_temp)


  # LOCATION DATA

  message("Compiling location information...")

  Location_data_temp <- create_location_MAR(Brood_data_temp, nest_coord_data)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MAR.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MAR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MAR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MAR.csv"), row.names = F)

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

#' Create brood data table for Mariola, Spain.
#'
#' @param brood_data Brood data compiled from primary data from Mariola, Spain.
#'
#' @param rr_data Ringing data compiled from primary data from Mariola, Spain.
#'
#' @return A data frame.

create_brood_MAR   <- function(brood_data, rr_data) {

  ## Create brood data
  ## Combine brood data with information from ringing records
  ## Some chick records in the ringing data do not have a broodID, currently these are not used to calculate brood summary stats since they cannot be joined to the data frame
  Brood_data_temp <- brood_data %>%

    ## Summarize chick data form ringing records
    ## TODO: Check on age of chicks when measured
    dplyr::left_join(rr_data %>%

                       ## Only keeping chicks
                       dplyr::filter(.data$Age_observed == 1) %>%

                       ## For each brood, get summary stats
                       dplyr::group_by(.data$BroodID) %>%
                       dplyr::summarise(AvgChickMass = round(mean(Mass, na.rm = T), 1),
                                        NumberChicksMass = sum(!is.na(.data$Mass)),
                                        AvgTarsus = round(mean(Tarsus, na.rm = T), 2),
                                        NumberChicksTarsus = sum(!is.na(.data$Tarsus))) %>%
                       ## Replace NaNs and 0 with NA
                       dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::na_if(., "NaN")),
                                     dplyr::across(where(is.numeric), ~dplyr::na_if(., 0))),

                     by = "BroodID") %>%

    ## Remove NAs
    ## TODO: Check on many Species that are NAs
    dplyr::filter_at(vars(.data$PopID,
                          .data$BreedingSeason,
                          .data$Species,
                          .data$BroodID), all_vars(!is.na(.)))

  return(Brood_data_temp)


}

#' Create capture data table in standard format for data from Mariola, Spain.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_MAR}}.
#'
#' @param rr_data Data frame. Primary data from ringing records.
#'
#' @return A data frame.

create_capture_MAR <- function(Brood_data_temp, rr_data) {

  ## Chick captures from ringing data. Join brood data to get BroodID
  ## TODO: Many conflicting species codes, using species data from ringing records
  ## TODO: A few duplicated chick bands. These should be fixed
  chicks_cap <- rr_data %>%
    dplyr::filter(Age_observed == 1L) %>%
    dplyr::select(-.data$BroodID) %>%
    dplyr::left_join(Brood_data_temp %>%
                       dplyr::select(.data$BreedingSeason, .data$BroodID, dplyr::starts_with("Chick")) %>%
                       tidyr::pivot_longer(cols =  dplyr::starts_with("Chick"), values_to = "IndvID") %>%
                       dplyr::select(-name) %>%
                       dplyr::filter(stringr::str_detect(.data$IndvID, "^[[:alpha:][:digit:]]{6,8}$")) %>%
                       na.omit() %>%
                       dplyr::distinct(.data$IndvID, .keep_all = T),
                     by = c("BreedingSeason","IndvID")) %>%

    dplyr::mutate(RingAge = "chick") %>%
    dplyr::filter_at(vars( .data$PopID, .data$BreedingSeason, .data$Species, .data$CaptureDate), all_vars(!is.na(.)))

  #Adult captures from ringing data
  adults_cap <- rr_data %>%
    dplyr::filter(Age_observed != 1L) %>%
    dplyr::filter(stringr::str_detect(.data$IndvID, "^[[:alpha:][:digit:]]{6,8}$")) %>%

    ## Removing BroodID from adult data since BroodID here does not refer to the BroodID where the adult was laid
    dplyr::select(-.data$BroodID) %>%
    dplyr::filter_at(vars( .data$PopID, .data$BreedingSeason, .data$Species, .data$CaptureDate), all_vars(!is.na(.)))

  ## Bind and add additional columns
  Capture_data_temp <- chicks_cap %>%
    dplyr::bind_rows(adults_cap) %>%

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
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything()) %>%

    ## Remove NAs
    dplyr::filter_at(vars(.data$PopID,
                          .data$BreedingSeason,
                          .data$Species,
                          .data$CaptureDate), all_vars(!is.na(.)))

  return(Capture_data_temp)

}

#' Create individual data table in standard format for data from Mariola, Spain.
#'
#' @param Capture_data_temp Data frame. Output from \code{\link{create_capture_MAR}}.
#'
#' @return A data frame.

create_individual_MAR <- function(Capture_data_temp) {

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

#' Create location data table in standard format for data from Mariola, Spain.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_MAR}}.
#'
#' @param nest_coord_data Data frame. Primary data on nest coordinates.
#'
#' @return A data frame.

create_location_MAR <- function(Brood_data_temp, nest_coord_data) {

  ## Create location data from brood data and nest coordinates
  ## TODO: More boxes than are listed in the meta data
  ## TODO: Check on habitat type
  ## TODO: Check about funnel and captures outside of nest boxes
  Location_data_temp <- Brood_data_temp %>%
    dplyr::select(.data$Plot,
                  .data$LocationID,
                  .data$BreedingSeason) %>%
    dplyr::group_by(.data$Plot, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason)) %>%
    dplyr::mutate(PopID = "MAR",
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
