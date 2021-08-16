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

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/MAR/data/MAR_Mariola_Spain/MAR_PrimaryData.mdb"

  #Assign to database location
  ## TODO: Set up for MAC and Windows
  db <- paste0(db, "\\MAR_PrimaryData.mdb")

  #Assign species for filtering
  #If no species are specified, all species are included
  if(is.null(species)){

    species_filter <- species_codes$Species

  } else {

    species_filter <- species

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  ### N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the MAR database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                                                                               db,
                                                                               ";Uid=Admin;Pwd=;"))

  connection <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Actual Access};Dbq=",
                                                                         db,
                                                                         ";Uid=Admin;Pwd=;"),
                               timeout = 10)

  ## Read in primary data on brood information
  ## TODO: Ask about Locality
  ## TODO: Check about experiment IDs
  ## TODO: Ask about 36 and 36 vieja. Different nests?
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
    dplyr::mutate(PopID = "MAR",
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

    arrange(.data$BreedingSeason, .data$LocationID)




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


  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_MAR(brood_data, rr_data)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_MAR(Brood_data, connection)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_MAR(Capture_data, Brood_data, connection)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_MAR(Capture_data, connection)

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
#' Create brood data table in standard format for data from Mariola, Spain.
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

    ## Remove records where Species is unknown
    dplyr::filter(!is.na(.data$Species))

  return(Brood_data_temp)


}

#' Create capture data table for Mariola, Spain
#'
#' Create capture data table in standard format for data from Mariola, Spain.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_MAR}}.
#'
#' @param capture_data Connection the SQL database.
#'
#' @return A data frame.

create_capture_MAR <- function(Brood_data_temp, capture_data) {


  ## Chick captures from ringing data. Join brood data to get BroodID
  ## TODO: Many conflicting species codes, using species data from ringing records
  ## TODO: A few duplicated chick bands. These should be fixed
  chicks_cap <- rr_data %>%
    dplyr::filter(Age_observed == 1L) %>%
    dplyr::select(-.data$BroodID) %>%
    dplyr::left_join(brood_data %>%
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
  capture_data_temp <- chicks_cap %>%
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
    dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything())

  return(capture_data_temp)

}

#' Create individual data table for Mariola, Spain
#'
#' Create individual data table in standard format for data from Mariola, Spain.
#'
#' @param capture_data_temp Data frame. Output from \code{\link{create_capture_MAR}}.
#'
#' @return A data frame.

create_individual_MAR <- function(capture_data_temp) {


  ## Create individual data from capture data
  Individual_data_temp <- capture_data_temp %>%

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


  individual_data_temp


}

#' Create location data table for Mariola, Spain.
#'
#' Create location data table in standard format for data from Mariola, Spain.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_MAR}}.
#' @param connection Connection the SQL database.
#'
#' @return A data frame.

create_location_MAR <- function(Capture_data, connection) {

  Habitat_data <- dplyr::tbl(connection, "HabitatDescription") %>%
    dplyr::select(.data$NestBox, .data$Beech:.data$OtherTree) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$NestBox) %>%
    dplyr::summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
    dplyr::summarise(DEC = sum(c(.data$Beech, .data$Larch, .data$Maple, .data$Birch, .data$Oak, .data$Willow, .data$Poplar, .data$Alder, .data$AshTree)),
                     EVE = sum(c(.data$Spruce, .data$Pine))) %>%
    dplyr::mutate(perc_dec = .data$DEC/(.data$DEC + .data$EVE),
                  dominant_sp = dplyr::case_when(.data$perc_dec >= 0.66 ~ "DEC",
                                                 .data$perc_dec < 0.33 ~ "EVE",
                                                 TRUE ~ "MIX"))

  #The vast majority of nestboxes (90%) of nestboxes are surrounded by deciduous or mixed stands. Therefore,
  #we use the 'DEC' category to describe the population.
  #In the future, we could include a nest-box specific habitat type, but that would require us to decide
  #the range over which habitat is sampled (the nestbox vicinity, the plot?). To do later.
  table(Habitat_data$dominant_sp)

  start_year <- min(Capture_data$BreedingSeason)

  Location_data <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::collect() %>%
    dplyr::filter(.data$NestBox != -99L) %>%
    dplyr::mutate(LocationID = as.character(.data$NestBox),
                  NestboxID = as.character(.data$NestBox),
                  Latitude = as.numeric(.data$CoordinateLatitude2013), ## Needed because these variables are stored as
                  Longitude = as.numeric(.data$CoordinateLongitude2013), ## named vectors, not regular numeric vectors
                  LocationType = "NB",
                  PopID = "MAR",
                  StartSeason = start_year,
                  EndSeason = dplyr::case_when(nchar(.data$NestboxID) == 4 & stringr::str_sub(.data$NestboxID, 1, 2) == "16" ~ 2016L,
                                               TRUE ~ 2019L),
                  HabitatType = "DEC") %>%
    dplyr::select(.data$LocationID,
                  .data$NestboxID,
                  .data$LocationType,
                  .data$PopID,
                  .data$Latitude,
                  .data$Longitude,
                  .data$StartSeason,
                  .data$EndSeason,
                  .data$HabitatType)

  Location_data

}
