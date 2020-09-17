#'Construct standard format for data from Valsein, Spain.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Valsein, Spain, administered by the National Museum of Natural Sciences.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Load data}: Data is in one excel spreadsheet with two sheets with brood info
#'and 7 sheets with chick info. We assume that the number/location of sheets will not change,
#'but we need to check this with data owner.
#'
#'\strong{BroodID}: BroodID is a concatenation of Year and BoxID (BoxID_Year).
#'
#'\strong{ClutchType_observed}: No clutch type recorded. This is left blank.
#'This may be 'PrecpLD' or 'TIPO' column in early broods.
#'Need to check with data owner.
#'
#'\strong{LayDate/HatchDate}: Assume that lay date information is April days (i.e. 0 = March 31st).
#'Need to check with data owner.
#'
#'\strong{FledgeDate}: No information provided.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_VAL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  #Load all data
  data_file    <- paste0(db, "/VAL_PrimaryData.xlsx")
  early_broods <- readxl::read_excel(data_file, sheet = 1, na = c("", "-")) %>%
    janitor::clean_names()
  late_broods  <- readxl::read_excel(data_file, sheet = 2, na = c("", "-")) %>%
    janitor::clean_names()
  chick_data   <- purrr::map_df(.x = 3:9,
                                 .f = ~{

                                   readxl::read_excel(data_file,
                                                      sheet = ..1,
                                                      col_type = "text")

                                 })

  # BROOD DATA
  #Extract Valsein brood data

  message("Compiling brood data....")

  Brood_data <- create_brood_VAL(early_broods, late_broods)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_VAL(early_broods, late_broods, chick_data)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_VAL(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_VAL(db = db)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_VAL.csv"), row.names = F)

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

#' Create brood data table for Valsein, Spain.
#'
#' Create brood data table in standard format for data from Valsein, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 -)
#'
#' @return A data frame.

create_brood_VAL <- function(early_broods, late_broods){

  early_broods_format <- early_broods %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  BroodID = paste(.data$nido, .data$year, sep = "_"),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Plot = NA_character_,
                  LocationID = .data$nido,
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$MarchDay + floor(.data$tmed_ld),
                  LayDate_min = .data$MarchDay + floor(.data$tmin_ld),
                  LayDate_max = .data$MarchDay + floor(.data$tmax_ld),
                  ClutchSize_observed = .data$incub,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + .data$hdate,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = .data$hatchl,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = .data$fledgl,
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(.)) %>%
    dplyr::select(.data$BroodID:.data$ClutchType_observed,
                  .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$NumberFledged_max)

  late_broods_format <- late_broods %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  BroodID = paste(.data$nest, .data$year, sep = "_"),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Plot = NA_character_,
                  LocationID = .data$nest,
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$MarchDay + floor(.data$ld),
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$cs,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + floor(.data$hd),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$cs * .data$hatching_suc),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = as.integer(.data$cs * .data$fled_suc),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(.)) %>%
    dplyr::select(.data$BroodID:.data$ClutchType_observed,
                  .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$NumberFledged_max)

  all_broods <- dplyr::bind_rows(early_broods_format, late_broods_format)

  return(all_broods)

}

#' Create capture table for Valsein, Spain.
#'
#' Create full capture data table in standard format for data from Valsein, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 -)
#' @param chick_data Data frame with data on chick captures
#'
#' @return A data frame.

create_capture_VAL    <- function(early_broods, late_broods, chick_data){

  #Extract info on adult captures
  early_captures <- early_broods %>%
    dplyr::select(female, ano_anilla_52, fage, surv_fem, fdia, ftarso, fala, fpeso,
                  male, ano_anilla_84, mage, surv_man, mdia, mtarso, mala, mpeso) %>%
    tidyr::pivot_longer(cols = c(female, male), names_to = "Sex_observed", values_to = "IndvID") %>%
    dplyr::mutate(Species = )

}

#' Create individual table for Valsein, Spain.
#'
#' Create full individual data table in standard format for data from Valsein, Spain.
#'
#' @param Capture_data Output of \code{\link{create_capture_VAL}}.
#'
#' @return A data frame.

create_individual_VAL <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }), PopID = "HAR",
    BroodIDLaid = first(BroodID),
    BroodIDFledged = BroodIDLaid,
    RingSeason = first(BreedingSeason),
    RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
    Sex = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("C")

      }

    })) %>%
    dplyr::rowwise() %>%
    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
                  BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

}

#' Create location table for Valsein, Spain.
#'
#' Create full location data table in standard format for data from Valsein, Spain.
#'
#' @param db Location of primary data from Harjavalta.
#'
#' @return A data frame.
#' @export

create_location_VAL <- function(db){

  message("Extracting location data from paradox database")

  #Extract table "Pullit.db" which contains brood data
  Location_data <- extract_paradox_db(path = db, file_name = "HAR_PrimaryData_Locations.DB") %>%
    #Remove last 2 cols that have no info
    dplyr::select(-Aukko, -Malli) %>%
    dplyr::rename(BreedingSeason = Vuos, LocationID = Nuro,
                  ForestType = Mety, PinusSylvestris = Manty,
                  PiceaAbies = Kuusi, Betulasp = Koivu,
                  PopulusTremula = Haapa,
                  SorbusAcuparia = Pihlaja,
                  Salixsp = Pajut, JuniperusCommunis = Kataja,
                  Alnussp = Leppa, PrunusPadas = Tuomi,
                  TreeHeight = Kork, BasalArea = Totrel,
                  PineHeight = Makor, SpruceHeight = Kukor,
                  BirchHeight = Kokor, PineBasalArea = Marel,
                  SpruceBasalArea = Kurel, BirchBasalArea = Korel,
                  Latitude = Leve, Longitude = Pitu, Municipality = Kunta,
                  LocationName = Paikka)

  #Separate locations with and without coordinates
  Location_nocoord <- Location_data %>%
    dplyr::filter(is.na(Longitude))

  Location_wcoord  <- Location_data %>%
    dplyr::filter(!is.na(Longitude))

  #Read location data with coordinates as sf object in Finnish Coordinate system
  #Coordinates are in Finland Uniform Coordinate System (EPSG 2393)

  Location_data_sf <- sf::st_as_sf(Location_wcoord,
                                   coords = c("Longitude", "Latitude"),
                                   crs = 2393) %>%
    sf::st_transform(crs = 4326)

  Location_full <- dplyr::bind_rows(dplyr::bind_cols(dplyr::select(Location_wcoord, -Longitude, -Latitude),
                                                     tibble(Longitude = sf::st_coordinates(Location_data_sf)[, 1]),
                                                     tibble(Latitude = sf::st_coordinates(Location_data_sf)[, 2])),
                                    Location_nocoord)

  #The records of each Location should show the start/end season
  Location_data <- Location_full %>%
    dplyr::group_by(LocationID) %>%
    dplyr::arrange(BreedingSeason, .by_group = TRUE) %>%
    dplyr::summarise(NestboxID = unique(LocationID), PopID = "HAR", Latitude = as.numeric(first(Latitude)), Longitude = as.numeric(first(Longitude)),
                     LocationType = "NB", StartSeason = min(BreedingSeason), EndSeason = max(BreedingSeason), Habitat = "Evergreen") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  return(Location_data)

}
