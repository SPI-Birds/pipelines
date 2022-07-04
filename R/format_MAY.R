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
#' \strong{Lay dates & hatch dates}: Some lay dates and hatch dates contain special characters (e.g., "<", "?", "()"), which are ignored. Some lay dates and hatch dates in primary data are a range of values (e.g., "31-34"). These are converted into the minimum (minimumLayDate, minimumHatchDate) and maximum dates (maximumLayDate, maximumHatchDate); observed dates (observedLayDate, observedHatchDate) are given by the average of the range (rounded down; i.e. "32" in the example). Check with data owner.
#'
#' \strong{Clutch size, brood size, fledgling number}: Some clutch sizes, brood sizes, and fledgling numbers contain special characters (e.g., ?", "()"), which are ignored. Some clutch sizes, brood sizes, and fledgling numbers are written as an arithmetic expression (e.g., "7+4"), and interpreted as such (i.e., the observed value equals 11 in the example).
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
                                dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box,
                                                                                     pattern = "- ", replacement = ""),
                                              siteID = "MAY",
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
                                dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box,
                                                                                     pattern = "- ", replacement = ""),
                                              siteID = "MAY",
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

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_MAY(gt_data = gt_data,
                                 pf_data = pf_data,
                                 species_filter = species,
                                 optional_variables = optional_variables)

}


#' Create brood data table for Mayachino, Russia.
#'
#' Create brood data table in standard format for data from Mayachino, Russia.
#'
#' @param gt_data Data frame. Great tit data from Mayachino, Russia.
#' @param pf_data Data frame. Pied flycatcher data from Mayachino, Russia.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_MAY <- function(gt_data,
                             pf_data,
                             species_filter,
                             optional_variables = NULL) {

  # Pied flycatcher
  pf_broods <- pf_data %>%
    # Create female & male IDs
    tidyr::unite(femaleID, .data$females_ring_series, .data$females_ring, remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite(maleID, .data$males_ring_series, .data$males_ring, remove = FALSE, na.rm = TRUE, sep = "") %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$femaleID, .data$maleID),
                                .fns = ~{

                                  stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")

                                }),
                  # If femaleID & maleID differ from expected format, set to NA
                  # TODO: Check IDs with data owner: many missing "ring series" letters (e.g., XA, VS)
                  femaleID = dplyr::case_when(stringr::str_detect(.data$femaleID, "^[:upper:]{0,2}[:digit:]{5,6}$") ~ .data$femaleID,
                                              TRUE ~ NA_character_),
                  maleID = dplyr::case_when(stringr::str_detect(.data$maleID, "^[:upper:]{0,2}[:digit:]{5,6}$") ~ .data$maleID,
                                              TRUE ~ NA_character_)) %>%
    # Convert dates from May days (1 = 1st of May) to year, month, day
    # Days formatted as e.g., "<32", ">32", "32?", "(32)" or "?" are interpreted without the special characters
    # Days formatted as e.g., "32-35" are interpreted as a minimum and maximum; observed dates are taken as the rounded down average
    # TODO: check with data owner
    dplyr::mutate(dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    is.na(.x) ~ NA_integer_,
                                    stringr::str_detect(.x, "<") ~ as.integer(stringr::str_remove(.x, "<")),
                                    stringr::str_detect(.x, ">") ~ as.integer(stringr::str_remove(.x, ">")),
                                    stringr::str_detect(.x, "\\?") ~ as.integer(stringr::str_remove(.x, "\\?")),
                                    stringr::str_detect(.x, "\\(") ~ as.integer(stringr::str_extract(.x, "(?<=\\()[:digit:]{1,2}(?=\\))")),
                                    stringr::str_detect(.x, "-") ~ as.integer(floor((as.integer(stringr::str_extract(.x, "[:digit:]{1,2}(?=-)")) + as.integer(stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}"))) / 2)),
                                    TRUE ~ as.integer(.x)
                                  )

                                },
                                .names = "{.col}_observedMayDate"),
                  dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    stringr::str_detect(.x, "-") ~ as.integer(stringr::str_extract(.x, "[:digit:]{1,2}(?=-)")),
                                    TRUE ~ NA_integer_
                                  )

                                },
                                .names = "{.col}_minimumMayDate"),
                  dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    stringr::str_detect(.x, "-") ~ as.integer(stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}")),
                                    TRUE ~ NA_integer_
                                  )

                                },
                                .names = "{.col}_maximumMayDate"),
                  dplyr::across(.cols = dplyr::ends_with(c("observedMayDate", "minimumMayDate", "maximumMayDate")),
                                .fns = ~{

                                  lubridate::as_date(x = paste0(.data$year, "-04-30")) + .x

                                }),
                  observedLayYear = lubridate::year(.data$start_date_of_laying_1_may_1_observedMayDate),
                  observedLayMonth = lubridate::month(.data$start_date_of_laying_1_may_1_observedMayDate),
                  observedLayDay = lubridate::day(.data$start_date_of_laying_1_may_1_observedMayDate),
                  minimumLayYear = lubridate::year(.data$start_date_of_laying_1_may_1_minimumMayDate),
                  minimumLayMonth = lubridate::month(.data$start_date_of_laying_1_may_1_minimumMayDate),
                  minimumLayDay = lubridate::day(.data$start_date_of_laying_1_may_1_minimumMayDate),
                  maximumLayYear = lubridate::year(.data$start_date_of_laying_1_may_1_maximumMayDate),
                  maximumLayMonth = lubridate::month(.data$start_date_of_laying_1_may_1_maximumMayDate),
                  maximumLayDay = lubridate::day(.data$start_date_of_laying_1_may_1_maximumMayDate),
                  observedHatchYear = lubridate::year(.data$hatching_date_1_may_1_observedMayDate),
                  observedHatchMonth = lubridate::month(.data$hatching_date_1_may_1_observedMayDate),
                  observedHatchDay = lubridate::day(.data$hatching_date_1_may_1_observedMayDate),
                  minimumHatchYear = lubridate::year(.data$hatching_date_1_may_1_minimumMayDate),
                  minimumHatchMonth = lubridate::month(.data$hatching_date_1_may_1_minimumMayDate),
                  minimumHatchDay = lubridate::day(.data$hatching_date_1_may_1_minimumMayDate),
                  maximumHatchYear = lubridate::year(.data$hatching_date_1_may_1_maximumMayDate),
                  maximumHatchMonth = lubridate::month(.data$hatching_date_1_may_1_maximumMayDate),
                  maximumHatchDay = lubridate::day(.data$hatching_date_1_may_1_maximumMayDate)) %>%
    # Convert numbers (clutch size, brood size, fledgling number)
    # Values formatted as e.g. "7+4" or "7-4" are interpreted as arithmetic calculations, yielding values of 11 and 3, respectively
    # Values formatted as e.g. "(4)" or "5?" are interpreted without the special characters
    # TODO: Check with data owner
    dplyr::mutate(dplyr::across(.cols = c(.data$clutch_size, .data$number_of_hatched_nestlings, .data$number_of_fledlings),
                                .fns = ~{

                                  stringr::str_replace_all(.x, pattern = " ", replacement = "")

                                }),
                  dplyr::across(.cols = c(.data$clutch_size, .data$number_of_hatched_nestlings, .data$number_of_fledlings),
                                .fns = ~ {

                                  dplyr::case_when(
                                    stringr::str_detect(.x, "\\?") ~ dplyr::na_if(stringr::str_remove(.x, "\\?"), ""),
                                    stringr::str_detect(.x, "\\(") ~ stringr::str_extract(.x, "(?<=\\()[:digit:]{1,2}(?=\\))"),
                                    stringr::str_detect(.x, ".*[:alpha:]+.*") ~ NA_character_,
                                    TRUE ~ .x
                                  )

                                }),
                  dplyr::across(.cols = c(.data$clutch_size, .data$number_of_hatched_nestlings, .data$number_of_fledlings),
                                .fns = ~ {

                                  sapply(.x, function(x) eval(parse(text = x)))

                                }),
                  observedClutchSize = .data$clutch_size,
                  observedBroodSize = .data$number_of_hatched_nestlings,
                  observedNumberFledged = .data$number_of_fledlings)


    # dplyr::group_by(.data$year)
    # # TODO: Some nests are marked as "experiment". Check with data owner
    # dplyr::mutate(treatmentID = dplyr::case_when(.data$the_cause_of_the_nests_death == "experiment" ~ paste(.data$year, dplyr::row_number(), sep = "_"),
    #                                              TRUE ~ NA_character_))



}

#----------------------#
# TODO: Check whether "line of nest boxes" are indeed plot IDs
# TODO: Check whether all individuals are correctly identified as said species (pied flycatcher, or great tit) in their respective files
# TODO: Check individual IDs: many missing "ring series" letters
# TODO: Check dates notation (<32, >32, 32-35, etc.)
# TODO: Check brood numbers notation (7+4, (7), etc.)
# TODO: Is there info on clutch type?
# TODO: Check how to interpret "cause of nest's death", and in particular "experiment" - any info on experiments?
