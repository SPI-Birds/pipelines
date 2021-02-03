#' Construct standard format data from MTA-PE Evolutionary Ecology Group, University of Pannonia, Hungary (MTA)
#'
#' A pipeline to produce the standard format for bird study population
#' at the MTA-PE Evolutionary Ecology Group, Hungary, administered by
#' The data include 5 sites: Veszprém, Balatonfüred, Szentgál, Vilma-puszta and Gulya-domb.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{xxx}:
#'
#' \strong{Latitude, Longitude} The exact coordinates of the nestboxes are not available.
#' Data owner provided an map of the location, which can be georeferenced in the case of
#' interest or necessity of the data user. Currently, only general coordinates
#' for the location are provided.
#'
#' @inheritParams pipeline_params
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export


format_MTA <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #### Force user to select directory
  force(db)

  #### Determine species codes for filtering

  if(is.null(species)){

    species <- species_codes$Species

  }
  start_time <- Sys.time()


  #### Primary data
  message("Importing primary data...")

  mta_data <- readxl::read_excel(path =  paste0(db, "/MTA_PrimaryData.xlsx"),
                                 na = c("NA", "nA")) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(PopID = "MTA",
                  BreedingSeason = as.integer(.data$Year),
                  NestboxID = toupper(NestboxNumber)) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-FemaleColorId,
                  -MaleColorId) %>%
    #### Reorder columns
    dplyr::select(BreedingSeason,
                  Species,
                  PopID,
                  everything()) %>%
    dplyr::distinct()


  #### BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_MTA(data = pew_data)


  #### CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_MTA(pew_data, Brood_data)


  #### INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_MTA(Capture_data)


  #### LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_MTA(pew_data)


  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")
    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MTA.csv"), row.names = F)
    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MTA.csv"), row.names = F)
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




#' Create location data table for MTA-PE, Hungary.
#'
#' Create location data table in standard format for data from MTA-PE, Hungary.
#'
#' @param data Data frame pew_data with primary data from MTA-PE, Hungary.
#'
#' @return A data frame.

create_location_MTA <- function(data) {

  Location_data <-
    data %>%
    dplyr::select(.data$BreedingSeason, .data$NestboxID, .data$PopID, .data$Site) %>%
    group_by(.data$NestboxID) %>%
    arrange(.data$BreedingSeason) %>%
    dplyr::summarise(StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
                     EndSeason = NA_integer_,
                     LocationID = unique(.data$NestboxID),
                     PopID = unique(.data$PopID),
                     Site = unique(.data$Site)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType = case_when(.data$Site %in% c("Veszprem", "Balatonfured") ~ "urban",
                                          .data$Site %in% c("Vilma.puszta", "Szentgal_erdo") ~ "deciduous",
                                          .data$Site == "Gulya.domb" ~ NA_character_),
                  #### Only general coordinates for the location
                  Latitude  = NA_real_,
                  Longitude = NA_real_) %>%
    #### Final arrangement
    dplyr::select(LocationID, NestboxID, LocationType, PopID,
                  Latitude, Longitude, StartSeason, EndSeason, HabitatType)

  return(Location_data)

}

#### There are 3 lines more thatn n_distinct(mta_data$NestboxID) ???
