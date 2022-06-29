#' Construct standard format for data from Mayachino, Russia.
#'
#' A pipeline to produce the standard format for the study site at
#' Mayachino, Russia, administered by the Institute of Biology at the Karelian Research Centre.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#' \strong{plotID}: The "line of nest boxes" are interpreted as distinctive plots. Check with data owner.
#'
#' \strong{speciesID}: No species identification in primary data. Check with data owner.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format.
#' @export
#'

format_MAY <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- c(species_codes$speciesID)

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  message("Importing primary data...")

  # Read in pied flycatcher data
  pf_data <- suppressMessages(readxl::read_excel(paste0(db, "/MAY_PrimaryData_PF.xls"),
                                                 guess_max = 4000) %>%
                                # Convert all cols to snake_case
                                janitor::clean_names() %>%
                                # Create IDs
                                dplyr::mutate(siteID = "MAY",
                                              # Ensure unique plotIDs; add siteID prefix
                                              # TODO: Are the line of nest boxes indeed plot identifiers? Check with data owner
                                              plotID = paste0("DLO_", tolower(.data$the_line_of_nest_boxes)),
                                              # Ensure unique locationIDs; requires plot & nestbox
                                              locationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # Ensure unique broodIDs; requires laying date (1 = 1st of May), plot, nestbox
                                              # This accounts for multiple clutches in a single nestbox, in a single year
                                              broodID = paste(.data$year, .data$start_date_of_laying_1_may_1,
                                                              .data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # TODO: No species identification, so all individuals assumed to be pied flycatcher; check with data owner
                                              speciesID = species_codes$speciesID[species_codes$speciesCode == "10003"]) %>%
                                # Convert dates
                                dplyr::mutate(dplyr::across(.cols = c(.data$date_of_female_molt_survey,
                                                                      .data$date_of_male_molt_survey),
                                                            .fns = ~{

                                                              as.Date(.x)

                                                            }))
                              )

  # Read in great tit data
  gt_data <- suppressMessages(readxl::read_excel(paste0(db, "/MAY_PrimaryData_GT.xlsx"),
                                                 skip = 1,
                                                 guess_max = 500) %>%
                                # Remove trailing columns that contain no information
                                dplyr::select(1:22) %>%
                                # Convert all cols to snake_case
                                janitor::clean_names() %>%
                                # Create IDs
                                dplyr::mutate(siteID = "MAY",
                                              # Ensure unique plotIDs; add siteID prefix
                                              # TODO: Are the line of nest boxes indeed plot identifiers? Check with data owner
                                              plotID = paste0("DLO_", tolower(.data$the_line_of_nest_boxes)),
                                              # Ensure unique locationIDs; requires plot & nestbox
                                              locationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # Ensure unique broodIDs; requires laying date (1 = 1st of May), plot, nestbox
                                              # This accounts for multiple clutches in a single nestbox, in a single year
                                              broodID = paste(.data$year, .data$start_date_of_laying_1_may_1,
                                                              .data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # TODO: No species identification, so all individuals assumed to be pied flycatcher; check with data owner
                                              speciesID = species_codes$speciesID[species_codes$speciesCode == "10001"])
  )

}

#----------------------#
# TODO: Check whether "line of nest boxes" are indeed plot IDs
# TODO: Check whether all individuals are correctly identified as said species (pied flycatcher, or great tit) in their respective files
