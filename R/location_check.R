#' Perform quality checks on location data
#'
#' A wrapper that runs all single checks related to \code{Location_data}.
#'
#' The following location data checks are performed:
#' \itemize{
#' \item \strong{L1}: Check location coordinates using \code{\link{check_coordinates}}.
#' }
#'
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#' @inheritParams checks_capture_params
#' @param map Logical. Produce map of locations? See \code{\link{check_coordinates}}.
#'
#' @inherit checks_return return
#'
#' @export

location_check <- function(Location_data, Brood_data, Capture_data, approved_list, output, skip, map){

  # Perform location checks
  message("Checking location data...")

  # Run checks and create list of check outputs
  check_outputs <- tibble::lst(L1 = check_coordinates(Location_data, Brood_data, Capture_data,
                                                      approved_list, output, skip, map) # L1: Check location coordinates

  )

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("L", 1),
                               CheckDescription = c("Check location coordinates"),
                               Warning = NA,
                               Error = NA,
                               Skipped = NA)

  check_list[,3:5] <- purrr::map_dfr(.x = check_outputs, .f = 1) # Combine check lists of single checks

  # Create list of 'warning' messages
  warning_list <- purrr::map(.x = check_outputs, .f = 4)

  # Create list of 'potential error' messages
  error_list <- purrr::map(.x = check_outputs, .f = 5)

  return(list(CheckList = check_list,
              WarningRows = purrr::map(.x = check_outputs, .f = 2) %>% unlist(use.names = FALSE) %>% unique(),
              ErrorRows = purrr::map(.x = check_outputs, .f = 3) %>% unlist(use.names = FALSE) %>% unique(),
              Warnings = warning_list,
              Errors = error_list,
              Maps = check_outputs$L1$Maps))

}

#' Check coordinates of locations
#'
#' Check that the coordinates of locations recorded in Brood_data and/or Capture_Data are close to the centre point of the study site. Locations that are farther than 20 km will result in an error. It's optional to print the locations on a map, which is then visualized in the quality check report.
#'
#' Check ID: L1.
#'
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#' @inheritParams checks_capture_params
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
#' @import leaflet
#'
#' @export

check_coordinates <- function(Location_data, Brood_data, Capture_data, approved_list, output, skip, map){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("L1" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("L1: Checking location coordinates...")

  } else {

    message("<< L1 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Skip if coordinates were not recorded, or if only warnings are flagged
  if(!any(!is.na(Location_data$Longitude) & !is.na(Location_data$Latitude)) | !(output %in% c("both", "errors")) | skip_check == TRUE) {

    remote_locations <- tibble::tibble(Row = integer())

  } else {

    # Keep records with known & unique longitude/latitudes
    records_w_longlat <- Location_data %>%
      tidyr::drop_na(dplyr::any_of(c("Longitude", "Latitude"))) %>%
      # Filter location records that appear in Brood_data or Capture_data only
      dplyr::filter(.data$LocationID %in% unique(Brood_data$LocationID) | .data$LocationID %in% unique(Capture_data$LocationID)) %>%
      dplyr::group_by(.data$PopID) %>%
      dplyr::summarise(n_unique_lon = length(unique(.data$Longitude)),
                       n_unique_lat = length(unique(.data$Latitude)),
                       .groups = "drop")

    # Print message for populations with too few known, unique coordinates
    if(any(records_w_longlat$n_unique_lon < 2 | records_w_longlat$n_unique_lat < 2)) {

      few_coords <- records_w_longlat %>%
        dplyr::filter(.data$n_unique_lon < 2 | .data$n_unique_lat < 2) %>%
        dplyr::select(.data$PopID)

      purrr::walk(.x = list(few_coords$PopID),
                  .f = ~{

                    message(paste0("Number of records for ", .x,
                                   " is too low to calculate centre point."))

                  })

    }

    # Select PopIDs with at least 2 locations with known and unique coordinates
    pops_w_longlat <- records_w_longlat %>%
      dplyr::filter(.data$n_unique_lon >= 2 | .data$n_unique_lat >= 2) %>%
      dplyr::pull(.data$PopID)

    # Determine centre point per PopID
    centre_points <- Location_data %>%
      # Filter out capture locations without coordinates
      # TODO: Maybe in future only for LocationType NB (nestbox) & MN (mist net),
      # not FD (dead recoveries)
      # TODO: For populations with a lot of subplots, or for migratory species,
      # determine centre points via k-clustering?
      tidyr::drop_na(dplyr::any_of(c("Longitude", "Latitude"))) %>%
      # Filter location records that appear in Brood_data and/or Capture_data only
      # And keep populations with at least 2 records with known coordinates
      dplyr::filter((.data$LocationID %in% unique(Brood_data$LocationID) | .data$LocationID %in% unique(Capture_data$LocationID)) &
                      .data$PopID %in% pops_w_longlat) %>%
      dplyr::group_by(.data$PopID) %>%
      # Centre points are determined by calculating the maximum kernel density for Longitude and Latitude
      dplyr::summarise(Centre_lon = mean(stats::density(.data$Longitude)$x[which(stats::density(.data$Longitude)$y == max(stats::density(.data$Longitude)$y))]),
                       Centre_lat = mean(stats::density(.data$Latitude)$x[which(stats::density(.data$Latitude)$y == max(stats::density(.data$Latitude)$y))]),
                       .groups = "drop")

    # Add centre points to original data frame
    locations <- Location_data %>%
      tidyr::drop_na(dplyr::any_of(c("Longitude", "Latitude"))) %>%
      dplyr::filter(.data$LocationID %in% unique(Brood_data$LocationID) | .data$LocationID %in% unique(Capture_data$LocationID)) %>%
      dplyr::left_join(centre_points, by = "PopID") %>%
      dplyr::filter(!is.na(.data$Centre_lon) & !is.na(.data$Centre_lat)) %>%
      # Calculate distance from each capture location to population-specific centre points
      dplyr::mutate(Coords = sf::st_as_sf(., coords = c("Longitude", "Latitude"),
                                          crs = "EPSG:4326")$geometry,
                    Centre = sf::st_as_sf(., coords = c("Centre_lon", "Centre_lat"),
                                          crs = "EPSG:4326")$geometry,
                    Distance = as.integer(sf::st_distance(.data$Coords, .data$Centre, by_element = TRUE)))

    # Filter very remote locations (20 km or farther)
    remote_locations <- locations %>%
      dplyr::filter(.data$Distance >= 20000) %>%
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
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Add messages about populations with too few records with known coordinates to warning output
  if(output %in% c("both", "warnings") & exists("few_coords") & skip_check == FALSE) {

    skipped_output <- purrr::map(.x = few_coords$PopID,
                                 .f = ~{

                                   paste0("Number of records for ", .x,
                                          " with known coordinates is too low to calculate the study site's centre point.")

                                 })

    warning_output <- skipped_output

  }

  # Produce map of locations
  if(map & any(!is.na(Location_data$Longitude) & !is.na(Location_data$Latitude)) & output %in% c("both", "errors") & skip_check == FALSE) {

    suppressMessages({

      # Create map per PopID
      maps <- purrr::map(.x = pops_w_longlat,
                         .f = ~{

                           leaflet::leaflet() %>%
                             leaflet::fitBounds(lng1 = min(locations[locations$PopID == .x, ]$Longitude),
                                                lng2 = max(locations[locations$PopID == .x, ]$Longitude),
                                                lat1 = min(locations[locations$PopID == .x, ]$Latitude),
                                                lat2 = max(locations[locations$PopID == .x, ]$Latitude)) %>%
                             leaflet::addProviderTiles(leaflet::providers$CartoDB.Voyager) %>%
                             leaflet::addCircleMarkers(lng = locations[locations$PopID == .x, ]$Longitude,
                                                       lat = locations[locations$PopID == .x, ]$Latitude,
                                                       fillColor = "#881f70", stroke = FALSE,
                                                       radius  = 5, fillOpacity = 0.5) %>%
                             leaflet::addLabelOnlyMarkers(lng = locations[locations$PopID == .x, ][1, ]$Centre_lon,
                                                          lat = locations[locations$PopID == .x, ][1, ]$Centre_lat,
                                                          label = "*",
                                                          labelOptions = leaflet::labelOptions(
                                                            noHide = TRUE,
                                                            textOnly = TRUE,
                                                            style = list("color" = "black",
                                                                         "font-size" = "32px"))) %>%
                             leaflet::addLegend(title = paste0(pop_codes[pop_codes$PopID == .x, ]$PopName, " (", .x, ")"),
                                                colors = "white", labels = "Centre point (*)")

                         }) %>%
      setNames(pops_w_longlat)

    })

  } else {

    maps <- NULL

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output),
              Maps = maps))

  # Satisfy RCMD checks
  approved_list <- NULL

}
