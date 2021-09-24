#'Construct standard format for data from Sagunto, Spain
#'
#'A pipeline to produce the standard format for the nest box population in Sagunto, Spain, administered by Emilio Barba.
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

format_SAG <- function(db = choose_directory(),
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

  db <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/SAG_Sagunto_Spain/"

  ### N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  ## Connect to the SAG database
  if(grepl(pattern = 'mac', x = OS)){
    db_file <- paste0(db, "/SAG_PrimaryData.accdb")
    connection <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = paste0("Driver={Actual Access};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  } else if(grepl(pattern = 'windows', x = OS)){
    db_file <- paste0(db, "\\SAG_PrimaryData.mdb")
    connection <- DBI::dbConnect(drv = odbc::odbc(),
                                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                                                             db_file,
                                                             ";Uid=Admin;Pwd=;"))
  }

  ## Get brood information from primary data
  brood_data <- dplyr::tbl(connection, "Datos de Nidificaci\u00f3n") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel") %>%

    ## TODO: Check on column names: Localizacion, Caja
    ## TODO: Fix one date (03)
    ## TODO: Ask about CodigoNido
    ## TODO: Ask about nests with: ' - Is this intentional?
    ## TODO: Ask about experiments
    ## TODO: Nests with "«"?
    dplyr::transmute(PopID = "SAG",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     BreedingSeason = as.numeric(dplyr::case_when(.data$Ano <= 99 & .data$Ano > 84 ~ paste0(19L,.data$Ano),
                                                                  .data$Ano <= 80 ~ paste0(20L,.data$Ano))),
                     Plot= toupper(.data$Localizacion),
                     LocationID = gsub("«","", toupper(.data$Caja)),
                     ClutchType_observed = dplyr::case_when(.data$TipoDePuesta == "Primera" ~ "first",
                                                            .data$TipoDePuesta == "Segunda" ~ "second",
                                                            .data$TipoDePuesta == "Reposici\x97n" ~ "replacement",
                                                            .data$TipoDePuesta == "Desconocida" ~ NA_character_,
                                                            TRUE ~ NA_character_),

                     ## TODO: Two years (2016 and 2017 do not currently have a nest code, which is helpful for identifying 2nd clutches).
                     BroodID = .data$CodigoNido,

                     ## TODO: Check on units
                     AvgEggMass = .data$VolumenMedioHuevos,

                     ## TODO: Check on number of eggs
                     NumberEggs = .data$TamanoDePuesta,

                     ## TODO: Ask about uncertainty in lay and hatch dates
                     LayDate_observed = as.Date(as.integer(.data$FechaDeInicioDePuesta),
                                                origin = as.Date(paste0(.data$BreedingSeason, "-03-31"))),
                     ClutchSize_observed = .data$TamanoDePuesta,
                     HatchDate_observed = as.Date(as.integer(.data$FechaDeEclosion),
                                                  origin = as.Date(paste0(.data$BreedingSeason, "-03-31"))),
                     BroodSize_observed = .data$NumeroDeEclosiones,
                     NumberFledged_observed = .data$NumeroDePollosQueVuelan,
                     ExperimentID = dplyr::case_when(grepl("S", .data$Experimentos) ~ "OTHER",
                                                     TRUE ~ NA_character_)) %>%

    dplyr::mutate(dplyr::across(c(.data$ClutchSize_observed,
                                  .data$BroodSize_observed,
                                  .data$NumberFledged_observed), as.integer)) %>%

    dplyr::arrange(.data$BreedingSeason, .data$LocationID)


  ## Get ringing records from primary data
  rr_data_ring <- dplyr::tbl(connection, "DATOS PRIMER ANILLAMIENTO") %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    tibble::as_tibble(.data) %>%
    janitor::clean_names(case = "upper_camel") %>%

    ## Process relevant columns
    ## TODO: Check on missing capture dates
    dplyr::transmute(table = "ringing",
                     PopID = "SAG",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$Anilla,  "^[:alnum:]{6,8}$") ~ .data$Anilla,
                                                       TRUE ~ NA_character_)),
                     Plot= toupper(.data$Localizacion),
                     LocationID = toupper(.data$CodigoCaja),
                     BroodID = .data$CodigoNido,
                     Sex_observed = dplyr::case_when(.data$Sexo == "H" ~ "M",
                                                     .data$Sexo == "M" ~ "F",
                                                     TRUE ~ NA_character_),
                     Age_observed = as.integer(.data$EdadEuring),
                     CaptureDate = format(.data$FechaCaptura, "%Y-%m-%d"),
                     BreedingSeason = as.numeric(format(.data$FechaCaptura, "%Y")),
                     Mass = round(as.numeric(Peso),1),
                     WingLength = round(as.numeric(.data$LongitudAla),1),
                     Tarsus = round(as.numeric(.data$LongitudTarso),2),
                     ExperimentID = dplyr::case_when(!is.na(.data$Transponder) ~ "SURVIVAL"))


  ## Get recapture records from primary data
  ## TODO: Check on missing capture dates
  rr_data_recap <- dplyr::tbl(connection, "RECUPERACIONES") %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    tibble::as_tibble(.data) %>%
    janitor::clean_names(case = "upper_camel") %>%

    ## Process relevant columns
    dplyr::transmute(table = "recap",
                     PopID = "SAG",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$Anilla,  "^[:alnum:]{6,8}$") ~ .data$Anilla,
                                                       TRUE ~ NA_character_)),
                     Plot= toupper(.data$Localizacion),
                     LocationID = toupper(.data$CodigoCaja),
                     BroodID = .data$CodigoNido,
                     Age_observed = as.integer(.data$EdadEuring),
                     CaptureDate = format(.data$FechaCaptura, "%Y-%m-%d"),
                     BreedingSeason = as.numeric(format(.data$FechaCaptura, "%Y")),
                     Mass = round(as.numeric(.data$Peso),1),
                     WingLength = round(as.numeric(.data$LongitudAla),1),
                     Tarsus = round(as.numeric(.data$LongitudTarso),2),
                     ExperimentID = dplyr::case_when(!is.na(.data$Transponder) ~ "SURVIVAL",
                                                     TRUE ~ NA_character_))

  ## Combine capture records
  ## TODO: Ask about multiple nests within brood id (e.g. 12015)
  rr_data <- dplyr::bind_rows(rr_data_ring,
                              rr_data_recap) %>%
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%

    ## Replace 0 with NA in mass, wing length, and tarsus
    dplyr::mutate(dplyr::across(c(.data$Mass,
                                  .data$WingLength,
                                  .data$Tarsus), ~dplyr::na_if(., 0))) %>%

    ## Sex is not always entered, but information is sometimes present from earlier years
    ## If it is not conflicted, fill in missing values
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                  .f = ~{
                                                    if(length(..1) == 0){
                                                      return(NA_character_)
                                                    } else if(length(..1) == 1){
                                                      return(..1)
                                                    } else {
                                                      return(NA_character_)
                                                    }
                                                  }),
                  CaptureDate = as.Date(.data$CaptureDate)) %>%
    tibble::as_tibble()



  ## Get nest locations from primary data
  nest_coord_data <- dplyr::tbl(connection, "Coordenadas") %>%
    tibble::as_tibble(.data) %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel") %>%
    na.omit() %>%
    sf::st_as_sf(coords = c("CoordenadaEsteOeste", "CoordenadaNorteSur"),
                 crs = ("+proj=utm +zone=30S +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(Longitude = as.numeric(sf::st_coordinates(.)[, 1]),
                  Latitude = as.numeric(sf::st_coordinates(.)[, 2])) %>%
    as.data.frame() %>%
    dplyr::select(LocationID = .data$Localizacion,
                  Longitude,
                  Latitude)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data_temp <- create_brood_SAG(brood_data, rr_data)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data_temp <- create_capture_SAG(Brood_data_temp, rr_data)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data_temp <- create_individual_SAG(Capture_data_temp)


  # LOCATION DATA

  message("Compiling location information...")

  Location_data_temp <- create_location_SAG(Brood_data_temp, nest_coord_data)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_SAG.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_SAG.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_SAG.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_SAG.csv"), row.names = F)

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

#' Create brood data table for Sagunto, Spain.
#'
#' @param brood_data Brood data compiled from primary data from Sagunto, Spain.
#'
#' @param rr_data Ringing data compiled from primary data from Sagunto, Spain.
#'
#' @return A data frame.

create_brood_SAG   <- function(brood_data, rr_data) {

  ## Create brood data
  Brood_data_temp <- brood_data %>%

    ## Summarize chick data form ringing records
    ## TODO: Some BroodIDs are clearly associated with multiple broods (e.g. 12015), these need to be fixed
    ## TODO: Check about how birds are sexed when age == 1
    dplyr::left_join(rr_data %>%

                       ## Get parents from each brood
                       dplyr::filter(.data$table == "ringing",
                                     (!is.na(.data$Sex_calculated) & .data$Age_observed != 1),
                                     !is.na(.data$BroodID)) %>%
                       dplyr::select(.data$PopID,
                                     .data$Species,
                                     .data$BreedingSeason,
                                     .data$Plot,
                                     .data$LocationID,
                                     .data$Sex_calculated,
                                     .data$IndvID,
                                     .data$BroodID) %>%
                       dplyr::group_by(.data$BroodID, .data$Sex_calculated) %>%

                       ## Remove broods where more than one parent has the same sex
                       dplyr::mutate(count = n()) %>%
                       dplyr::filter(count < 2) %>%
                       tidyr::pivot_wider(id_cols = .data$BroodID,
                                          names_from = .data$Sex_calculated,
                                          values_from = .data$IndvID,
                                          values_fill = NA) %>%
                       dplyr::rename(FemaleID = .data$F,
                                     MaleID = .data$M),
                     by = "BroodID") %>%

    ## Get chick summary stats
    dplyr::left_join(rr_data %>%

                       ## Keeping chicks from ringing data
                       dplyr::filter(.data$Age_observed == 1,
                                     .data$table == "ringing") %>%

                       dplyr::select(.data$BroodID,
                                     .data$IndvID,
                                     .data$Mass,
                                     .data$Tarsus) %>%

                       ## For each brood, get summary stats
                       dplyr::group_by(.data$BroodID) %>%
                       dplyr::summarise(AvgChickMass = round(mean(Mass, na.rm = T), 1),
                                        NumberChicksMass = sum(!is.na(.data$Mass)),
                                        AvgTarsus = round(mean(Tarsus, na.rm = T), 2),
                                        NumberChicksTarsus = sum(!is.na(.data$Tarsus))) %>%

                       ## Replace NaNs and 0 with NA
                       dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::na_if(., "NaN")),
                                     dplyr::across(where(is.numeric), ~dplyr::na_if(., 0))) %>%

                       ## Drop NAs in BroodID
                       dplyr::filter(!is.na(.data$BroodID)),
                     by = "BroodID") %>%

    ## Remove NAs
    dplyr::filter_at(dplyr::vars(.data$PopID,
                                 .data$BreedingSeason,
                                 .data$Species,
                                 .data$BroodID),
                     dplyr::all_vars(!is.na(.))) %>%

    tibble::as_tibble()

  return(Brood_data_temp)

}

#' Create capture data table in standard format for data from Sagunto, Spain.
#'
#' @param rr_data Data frame. Primary data from ringing records.
#'
#' @return A data frame.

create_capture_SAG <- function(rr_data) {

  ## Captures from ringing data
  Capture_data_temp <- rr_data %>%

    ## Rename variables
    dplyr::rename(CapturePopID = .data$PopID) %>%

    ## Remove NAs from key columns
    dplyr::filter_at(dplyr::vars( .data$CapturePopID,
                                  .data$IndvID,
                                  .data$BreedingSeason,
                                  .data$Species,
                                  .data$CaptureDate),
                     dplyr::all_vars(!is.na(.))) %>%

    ## Create capture  ID
    dplyr::arrange(.data$BreedingSeason,
                   .data$IndvID,
                   .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"))


  ## Get broods with multiple nests in capture data
  cap_broods_dups_dup <- Capture_data_temp %>%
    group_by(BroodID) %>%
    mutate(nest_combos = n_distinct(paste(Plot, LocationID)[Age_observed == 1  & !is.na(Age_observed)]),
           date_combos = n_distinct(CaptureDate[Age_observed == 1 & !is.na(Age_observed)]),
           date_diff = max(CaptureDate[Age_observed == 1], na.rm = T)-
             min(CaptureDate[Age_observed == 1], na.rm = T)) %>%
    filter(!is.na(BroodID)) %>%
    arrange(desc(date_diff), desc(date_combos), desc(nest_combos), ) %>%
    filter(date_combos > 1) %>%
    select(BroodID, Plot, LocationID, CaptureDate, Age_observed, Sex_observed, date_diff, date_combos, nest_combos)


  return(Capture_data_temp)

}

#' Create individual data table in standard format for data from Sagunto, Spain.
#'
#' @param Capture_data_temp Data frame. Output from \code{\link{create_capture_SAG}}.
#'
#' @return A data frame.

create_individual_SAG <- function(Capture_data_temp) {

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

                  ## No cross-fostering, so BroodIDFledged is always BroodIDLaid
                  BroodIDFledged = .data$BroodIDLaid) %>%

    ## BroodIDs should be NA for any individuals ringed as adults
    dplyr::mutate(across(c(.data$BroodIDLaid,
                           .data$BroodIDFledged),
                         ~dplyr::case_when(.data$RingAge == "adult" ~ NA_character_,
                                           .data$RingAge == "chick" ~ .))) %>%

    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())

  return(Individual_data_temp)

}

#' Create location data table in standard format for data from Sagunto, Spain.
#'
#' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_SAG}}.
#'
#' @param nest_coord_data Data frame. Primary data on nest coordinates.
#'
#' @return A data frame.

create_location_SAG <- function(Brood_data_temp, nest_coord_data) {

  ## Create location data from brood data and nest coordinates
  ## TODO: Check whether nest boxes are removed

  Location_data_temp <- Brood_data_temp %>%
    dplyr::select(.data$Plot,
                  .data$LocationID,
                  .data$BreedingSeason) %>%
    dplyr::group_by(.data$Plot, .data$LocationID) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason)) %>%
    dplyr::mutate(PopID = "SAG",
                  NestboxID = .data$LocationID,
                  HabitatType = "deciduous",
                  LocationType = "NB") %>%
    dplyr::distinct(.data$Plot, .data$LocationID, .keep_all = T)

    ## Join coordinates
    dplyr::left_join(nest_coord_data,
                     by = "LocationID") %>%
    na.omit() %>%

    ## Create temporary column for arranging
    dplyr::mutate(nest_num = as.integer(str_replace_all(.data$LocationID, "[:alpha:]", ""))) %>%
    dplyr::arrange(.data$Plot, .data$nest_num ) %>%
    dplyr::select(-.data$nest_num) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}
