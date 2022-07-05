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

    species <- species_codes$speciesID

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
                                dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box, c("-" = "", " " = "")),
                                              siteID = "MAY",
                                              # Ensure unique plotIDs; add siteID prefix
                                              # TODO: Are the line of nest boxes indeed plot identifiers? Check with data owner
                                              plotID = paste0("MAY_", toupper(.data$the_line_of_nest_boxes)),
                                              # Ensure unique locationIDs; requires plot & nestbox
                                              locationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # Ensure unique broodIDs; plot, nestbox ID, and "no string"
                                              # TODO: Check with data owner whether "no string" is consistent
                                              broodID = paste(.data$year, .data$the_line_of_nest_boxes,
                                                              .data$no_nest_box, .data$no_string_1, sep = "_"),
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
                                dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box, c("-" = "", " " = "")),
                                              siteID = "MAY",
                                              # Ensure unique plotIDs; add siteID prefix
                                              # TODO: Are the line of nest boxes indeed plot identifiers? Check with data owner
                                              plotID = paste0("MAY_", toupper(.data$the_line_of_nest_boxes)),
                                              # Ensure unique locationIDs; requires plot & nestbox
                                              locationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
                                              # Ensure unique broodIDs; plot, nestbox ID, and "no string"
                                              # TODO: Check with data owner whether "no string" is consistent
                                              broodID = paste(.data$year, .data$the_line_of_nest_boxes,
                                                              .data$no_nest_box, .data$no_string_1, sep = "_"),
                                              # TODO: No species identification, so all individuals assumed to be pied flycatcher; check with data owner
                                              speciesID = species_codes$speciesID[species_codes$speciesCode == "10001"])
  )

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_MAY(gt_data = gt_data,
                                 pf_data = pf_data,
                                 species_filter = species,
                                 optional_variables = optional_variables)

  # WRANGLE DATA FOR EXPORT

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  # TIME

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_MAY.csv"), row.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data,
                Experiment_data = Experiment_data))

  }

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

    # Pied flycatcher data
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
                                      is.na(.x) ~ NA_character_,
                                      stringr::str_detect(.x, "<") ~ stringr::str_remove(.x, "<"),
                                      stringr::str_detect(.x, ">") ~ stringr::str_remove(.x, ">"),
                                      stringr::str_detect(.x, "^\\?") ~ NA_character_,
                                      stringr::str_detect(.x, ".*\\?") ~ stringr::str_remove(.x, "\\?"),
                                      stringr::str_detect(.x, "\\(") ~ stringr::str_extract(.x, "(?<=\\()[:digit:]{1,2}(?=\\))"),
                                      stringr::str_detect(.x, ".-") ~ as.character(floor((as.integer(stringr::str_extract(.x, "[:digit:]{1,2}(?=-)")) + as.integer(stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}"))) / 2)),
                                      TRUE ~ .x
                                    )

                                  },
                                  .names = "{.col}_observedMayDate"),
                    dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                  .fns = ~{

                                    dplyr::case_when(
                                      stringr::str_detect(.x, ".-") ~ stringr::str_extract(.x, "[:digit:]{1,2}(?=-)"),
                                      TRUE ~ NA_character_
                                    )

                                  },
                                  .names = "{.col}_minimumMayDate"),
                    dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                  .fns = ~{

                                    dplyr::case_when(
                                      stringr::str_detect(.x, ".-") ~ stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}"),
                                      TRUE ~ NA_character_
                                    )

                                  },
                                  .names = "{.col}_maximumMayDate"),
                    dplyr::across(.cols = dplyr::ends_with(c("observedMayDate", "minimumMayDate", "maximumMayDate")),
                                  .fns = ~{

                                    lubridate::as_date(x = paste0(.data$year, "-04-30")) + as.integer(.x)

                                  }),
                    observedLayYear = dplyr::case_when(is.na(.data$start_date_of_laying_1_may_1_observedMayDate) ~ as.integer(.data$year),
                                                       TRUE ~ as.integer(lubridate::year(.data$start_date_of_laying_1_may_1_observedMayDate))),
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
                    observedNumberFledged = .data$number_of_fledlings) %>%
      # Identify experiments
      # TODO: Some nests are marked as "experiment". Check with data owner
      dplyr::group_by(.data$year) %>%
      # Create index for every experimental brood per year, starting at 1 each year, and ignoring other "causes of death" and NAs
      dplyr::mutate(expBroodIndex = cumsum(dplyr::coalesce(.data$the_cause_of_the_nests_death == "experiment", 0)) + as.integer(.data$the_cause_of_the_nests_death == "experiment") * 0,
                    # Concatenate year and index to create unique treatment ID
                    treatmentID = dplyr::case_when(.data$the_cause_of_the_nests_death == "experiment" ~ paste(.data$year, .data$expBroodIndex, sep = "_"),
                                                   TRUE ~ NA_character_)) %>%
      dplyr::ungroup()

  # Great tit data
  gt_broods <- gt_data %>%
    # Create female & male IDs
    tidyr::unite(femaleID, .data$females_ring_series, .data$females_ring, remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite(maleID, .data$males_ring_series, .data$males_ring, remove = FALSE, na.rm = TRUE, sep = "") %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$femaleID, .data$maleID),
                                .fns = ~{

                                  stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")

                                }),
                  # If femaleID & maleID differ from expected format, set to NA
                  femaleID = dplyr::case_when(stringr::str_detect(.data$femaleID, "^[:upper:]{2}[:digit:]{5,6}$") ~ .data$femaleID,
                                              TRUE ~ NA_character_),
                  maleID = dplyr::case_when(stringr::str_detect(.data$maleID, "^[:upper:]{2}[:digit:]{5,6}$") ~ .data$maleID,
                                            TRUE ~ NA_character_)) %>%
    # Convert dates from May days (1 = 1st of May) to year, month, day
    # Days formatted as e.g., "<32", "(32)" or "?" are interpreted without the special characters
    # Days formatted as e.g., "32-35" are interpreted as a minimum and maximum; observed dates are taken as the rounded down average
    # TODO: check with data owner
    dplyr::mutate(dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    is.na(.x) ~ NA_character_,
                                    stringr::str_detect(.x, "<") ~ stringr::str_remove(.x, "<"),
                                    stringr::str_detect(.x, "<-") ~ stringr::str_remove(.x, "<-"),
                                    stringr::str_detect(.x, "^\\?") ~ NA_character_,
                                    stringr::str_detect(.x, ".*\\?") ~ stringr::str_remove(.x, "\\?"),
                                    stringr::str_detect(.x, "\\(") ~ stringr::str_extract(.x, "(?<=\\()[:digit:]{1,2}(?=\\))"),
                                    stringr::str_detect(.x, ".-") ~ as.character(floor((as.integer(stringr::str_extract(.x, "[:digit:]{1,2}(?=-)")) + as.integer(stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}"))) / 2)),
                                    TRUE ~ .x
                                  )

                                },
                                .names = "{.col}_observedMayDate"),
                  dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    stringr::str_detect(.x, ".-") ~ stringr::str_extract(.x, "[:digit:]{1,2}(?=-)"),
                                    TRUE ~ NA_character_
                                  )

                                },
                                .names = "{.col}_minimumMayDate"),
                  dplyr::across(.cols = c(.data$start_date_of_laying_1_may_1, .data$hatching_date_1_may_1),
                                .fns = ~{

                                  dplyr::case_when(
                                    stringr::str_detect(.x, ".-") ~ stringr::str_extract(.x, "(?<=-)[:digit:]{1,2}"),
                                    TRUE ~ NA_character_
                                  )

                                },
                                .names = "{.col}_maximumMayDate"),
                  dplyr::across(.cols = dplyr::ends_with(c("observedMayDate", "minimumMayDate", "maximumMayDate")),
                                .fns = ~{

                                  lubridate::as_date(x = paste0(.data$year, "-04-30")) + as.integer(.x)

                                }),
                  observedLayYear = dplyr::case_when(is.na(.data$start_date_of_laying_1_may_1_observedMayDate) ~ as.integer(.data$year),
                                                     TRUE ~ as.integer(lubridate::year(.data$start_date_of_laying_1_may_1_observedMayDate))),
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
    # Values formatted as e.g. "7+4" are interpreted as arithmetic calculations (i.e., value is 11)
    # Values formatted as e.g. "(4)" or "5?" are interpreted without the special characters
    # TODO: Check with data owner
    dplyr::mutate(dplyr::across(.cols = c(.data$clutch_size_in_brackets_possibly_number_of_eggs,
                                          .data$number_of_hatched_nestlings_in_brackets_possibly_number_of_nestlings,
                                          .data$number_of_fledlings),
                                .fns = ~{

                                  dplyr::case_when(
                                    stringr::str_detect(.x, "\\?") ~ dplyr::na_if(stringr::str_remove(.x, "\\?"), ""),
                                    stringr::str_detect(.x, "\\(") ~ stringr::str_extract(.x, "(?<=\\()[:digit:]{1,2}(?=\\))"),
                                    stringr::str_detect(.x, ".*[:alpha:]+.*") ~ NA_character_,
                                    TRUE ~ .x
                                  )

                                }),
                  dplyr::across(.cols = c(.data$clutch_size_in_brackets_possibly_number_of_eggs,
                                          .data$number_of_hatched_nestlings_in_brackets_possibly_number_of_nestlings,
                                          .data$number_of_fledlings),
                                .fns = ~ {

                                  sapply(.x, function(x) eval(parse(text = x)))

                                }),
                  observedClutchSize = .data$clutch_size_in_brackets_possibly_number_of_eggs,
                  observedBroodSize = .data$number_of_hatched_nestlings_in_brackets_possibly_number_of_nestlings,
                  observedNumberFledged = .data$number_of_fledlings) %>%
    # Convert clutch type
    # Data owner writes: 1 - normal first; 2 - normal second; 1 or 2 repeat, after losing 1 or 2 brood; (1 or 2) in brackets possibly first, second, or  repeat brood
    # TODO: Check interpretation with data owner
    dplyr::rename(clutchType = .data$no_of_brood_1_normal_first_2_normal_second_1_or_2_repeat_after_losing_1_or_2_brood_1_or_2_in_brackets_possibly_first_second_or_repeat_brood) %>%
    dplyr::mutate(observedClutchType = dplyr::case_when(.data$clutchType %in% c("1", "(1)") ~ "first",
                                                        stringr::str_detect(.data$clutchType, "1.*repeat") ~ "replacement",
                                                        stringr::str_detect(.data$clutchType, "[2-3]") ~ "second",
                                                        TRUE ~ NA_character_))

  # Add optional variables
  pf_output <- pf_broods %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.2") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  gt_output <- gt_broods %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$year) else .} %>%
    {if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.2") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  # Combine
  pf_output <- pf_output %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  gt_output <- gt_output %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v1.2$Brood_data[1, !(names(data_templates$v1.2$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v1.2$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  output <- dplyr::bind_rows(pf_output, gt_output) %>%
    dplyr::arrange(.data$observedLayYear, .data$observedLayMonth, .data$observedLayDay, .data$locationID)

  return(output)

}




#----------------------#
# TODO: Check whether "line of nest boxes" are indeed plot IDs
# TODO: Check whether "no string" are consistent & unique IDs
# TODO: Check whether all individuals are correctly identified as said species (pied flycatcher, or great tit) in their respective files
# TODO: Check individual IDs: many missing "ring series" letters
# TODO: Check dates notation (<32, >32, 32-35, etc.)
# TODO: Check brood numbers notation (7+4, (7), etc.)
# TODO: PF: Is there info on clutch type for pied flycatchers?
# TODO: GT: Check clutch type interpretation
# TODO: PF: Check how to interpret "cause of nest's death", and in particular "experiment" - any info on experiments?
