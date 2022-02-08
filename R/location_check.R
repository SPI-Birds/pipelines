#' Perform quality checks on location data
#'
#' A wrapper that runs all single checks related to \code{Location_data}.
#'
#' The following location data checks are performed:
#' \itemize{
#' \item \strong{L1}: Check capture location coordinates using \code{\link{check_coordinates}}.
#' }
#'
#' @inheritParams checks_location_params
#' @param map Logical. Produce map of capture locations? See \code{\link{check_coordinates}}.
#'
#' @inherit checks_return return
#'
#' @export

location_check <- function(Location_data, approved_list, output, map){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("L", 1),
                               CheckDescription = c("Check capture location coordinates"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Checking location data...")

  # - Check format location data
  message("L1: Checking capture location coordinates...")

  check_coordinates_output <- check_coordinates(Location_data, approved_list, output, map)

  check_list[1,3:4] <- check_coordinates_output$CheckList

  # Warning list
  warning_list <- list(Check1 = check_coordinates_output$WarningOutput)

  # Error list
  error_list <- list(Check1 = check_coordinates_output$ErrorOutput)

  return(list(CheckList = check_list,
              WarningRows = unique(c(check_coordinates_output$WarningRows)),
              ErrorRows = unique(c(check_coordinates_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list,
              Maps = check_coordinates_output$Maps))
}

#' Check coordinates of capture locations
#'
#' Check that the coordinates of capture locations are close to the centre point of the study site. Capture locations that are farther than 15 km will result in an error. It's optional to print the remaining locations on a map and visualized in the quality check report.
#'
#' Check ID: L1.
#'
#' @inheritParams checks_location_params
#' @param map Logical. Produce map of capture locations?
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{WarningRows}{A vector of rows with warnings.}
#' \item{ErrorRows}{A vector of rows with errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#' \item{Maps}{A list of maps with capture locations.}
#'
#' @import ggmap
#'
#' @export

# TODO: Write tests for this new function
check_coordinates <- function(Location_data, approved_list, output, map){

  # Skip if coordinates were not recorded, or if only warnings are flagged
  if(!any(!is.na(Location_data$Longitude) & !is.na(Location_data$Latitude)) | !(output %in% c("both", "errors"))) {

    remote_locations <- tibble::tibble(Row = integer())

  } else {

    # Check for potential errors
    err <- FALSE
    error_records <- tibble::tibble(Row = NA_character_)
    error_output <- NULL

    # Determine centre point per PopID
    centre_points <- Location_data %>%
      # Filter out capture locations without coordinates
      # TODO: Maybe in future only for LocationType NB (nestbox) & MN (mist net),
      # not FD (dead recoveries)
      # TODO: For populations with a lot of subplots, or for migratory species,
      # determine centre points via k-clustering?
      tidyr::drop_na(dplyr::any_of(c("Longitude", "Latitude"))) %>%
      dplyr::group_by(.data$PopID) %>%
      # Centre points are determined by calculating the maximum kernel density for Longitude and Latitude
      dplyr::summarise(Centre_lon = density(Longitude)$x[which(density(Longitude)$y == max(density(Longitude)$y))],
                       Centre_lat = density(Latitude)$x[which(density(Latitude)$y == max(density(Latitude)$y))],
                       .groups = "drop")

    # Add centre points to original data frame
    locations <- Location_data %>%
      tidyr::drop_na(dplyr::any_of(c("Longitude", "Latitude"))) %>%
      dplyr::left_join(centre_points, by = "PopID") %>%
      # Calculate distance from each capture location to population-specific centre points
      dplyr::mutate(Coords = sf::st_as_sf(., coords = c("Longitude", "Latitude"),
                                          crs = "EPSG:4326")$geometry,
                    Centre = sf::st_as_sf(., coords = c("Centre_lon", "Centre_lat"),
                                          crs = "EPSG:4326")$geometry,
                    Distance = as.integer(sf::st_distance(.data$Coords, .data$Centre, by_element = TRUE)))

    # Filter very remote locations (15 km or farther)
    remote_locations <- locations %>%
      dplyr::filter(.data$Distance >= 15000) %>%
      dplyr::select(.data$Row, .data$LocationID, .data$PopID, .data$Distance)

    # If potential errors, add to report
    if(nrow(remote_locations) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- remote_locations %>%
        dplyr::mutate(CheckID = "L1") %>%
        dplyr::anti_join(approved_list$Location_approved_list, by = c("PopID", "CheckID", "LocationID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (LocationID: ", ..2, "; PopID: ", ..3, ")",
                                           " is ", round(..4 / 1000, 1) , " km away from the centre point of the study site.")

                                  })

    }

  }

  # No check for warnings
  war <- FALSE
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Produce map of capture locations
  if(map & any(!is.na(Location_data$Longitude) & !is.na(Location_data$Latitude)) & output %in% c("both", "errors")) {

    # Only map capture locations within 15 km from study site
    map_data <- locations %>%
      dplyr::filter(Distance < 15000)

    suppressMessages({

      # Create map per PopID
      maps <- purrr::map(.x = unique(map_data$PopID),
                         .f = ~{

                           ggmap::qmplot(data = map_data[map_data$PopID == .x,], x = Longitude,y = Latitude,
                                         source = "stamen", maptype = "terrain", extent = "panel",
                                         color = I("#881f70"), alpha = I(0.4), size = I(2)) +
                             #ggplot2::geom_point(data = map_data[map_data$PopID == .x, ][1, ], ggplot2::aes(x = Centre_lon, y = Centre_lat), color = "black", shape = 17) +
                             ggplot2::theme_classic() +
                             ggplot2::theme(axis.text = ggplot2::element_text(color = "black"),
                                            axis.title = ggplot2::element_text(size = 12),
                                            panel.border = ggplot2::element_rect(color = "black", fill = NA)) +
                             ggplot2::labs(title = pop_codes[pop_codes$PopID == .x, ]$PopName,
                                           subtitle = paste0("Capture locations within 15 km from centre point (",
                                                             round(map_data[map_data$PopID == .x,]$Centre_lon[1], 3), ", ",
                                                             round(map_data[map_data$PopID == .x,]$Centre_lat[1], 3), ")."),
                                           caption = "Source: Map tiles by Stamen Design, under CC BY 3.0. \nMap data by OpenStreetMap, under ODbL.")


                         }) #%>%
        #setNames(unique(map_data$PopID))

    })

  } else {

    maps <- NULL

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output),
              Maps = maps))

  # Satisfy RCMD checks
  approved_list <- NULL

}
