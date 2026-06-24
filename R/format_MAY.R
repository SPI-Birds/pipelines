#' Construct standard format for data from Mayachino, Russia.
#'
#' A pipeline to produce the standard format for the study site at
#' Mayachino, Russia, administered by the Institute of Biology at the
#' Karelian Research Centre.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' \strong{Plot}: The "line of nest boxes" column in the Excel is used as Plot.
#'
#' \strong{Species}: Pied flycatcher and great tit are stored in separate
#' primary data files, with no species column. All individuals in a given
#' file are assumed to be of that file's species.
#'
#' \strong{LayDate_observed, HatchDate_observed}: Dates are recorded as days
#' since 1 May, sometimes with special characters (e.g. "<32", "32?", "(32)"),
#' which are ignored, or as a range (e.g. "31-34"). Ranges are converted into
#' LayDate_min/max and HatchDate_min/max; the observed date is the rounded-down
#' average of the range.
#'
#' \strong{ClutchSize_observed, BroodSize_observed, NumberFledged_observed}:
#' Some values contain special characters (e.g. "?", "()"), which are ignored,
#' or are written as an arithmetic expression (e.g. "7+4"), which is evaluated
#' (i.e. 11 in the example). No uncertainty range is recorded for these counts,
#' so only the observed value is reported.
#' TODO: 7 + 4 might be an uncertainty range. Ask.
#'
#' \strong{CaptureDate}: No exact capture dates are recorded. For adults,
#' the start of incubation (lay date + clutch size) as a proxy. Chick
#' captures use the same proxy.
#'
#' \strong{CaptureAlive, ReleaseAlive}: All individuals are assumed to be
#' captured and released alive.
#'
#' \strong{IndvID}: Individual IDs are only kept when they consist of 0-2
#' letters followed by 5 or 6 digits; many great tit and pied flycatcher
#' records are missing their letters and are set to NA.
#'
#' \strong{StartSeason}: Assume all nest boxes were placed in the first year
#' of the study.
#' TODO: Ask if this assumption is correct.
#'
#' \strong{HabitatType}: No habitat information is recorded in the primary
#' data, so this is left blank.
#'
#' @inheritParams pipeline_params
#'
#' @return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#' @export

format_MAY <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R") {
  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force user to select directory
  force(db)

  # Determine species codes for filtering
  if (is.null(species)) {
    species <- species_codes$Species
  }

  # Record start time to estimate processing time
  start_time <- Sys.time()

  message("Importing primary data...")

  # Read in pied flycatcher data
  pf_data <- suppressMessages(readxl::read_excel(paste0(db, "/MAY_PrimaryData_PF.xls"),
    guess_max = 4000
  )) %>%
    janitor::clean_names() %>%
    dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box, c("-" = "", " " = "")))

  # Read in great tit data
  gt_data <- suppressMessages(readxl::read_excel(paste0(db, "/MAY_PrimaryData_GT.xlsx"),
    skip = 1,
    guess_max = 4000
  )) %>%
    # Remove empty columns that contain no information
    dplyr::select(1:22) %>%
    janitor::clean_names() %>%
    dplyr::mutate(no_nest_box = stringr::str_replace_all(.data$no_nest_box, c("-" = "", " " = "")))

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_MAY(
    gt_data = gt_data,
    pf_data = pf_data,
    species_filter = species
  )

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_MAY(
    gt_data = gt_data,
    pf_data = pf_data,
    species_filter = species
  )

  # INDIVIDUAL DATA

  message("Compiling individual data....")

  Individual_data <- create_individual_MAY(
    capture_data = Capture_data,
    species_filter = species
  )

  # LOCATION DATA

  message("Compiling location data....")

  Location_data <- create_location_MAY(
    gt_data = gt_data,
    pf_data = pf_data
  )

  # WRANGLE DATA FOR EXPORT

  # BroodID on Capture_data is only needed to construct Individual_data
  Capture_data <- Capture_data %>%
    dplyr::select(-"BroodID")

  # Make sure data conforms to standard protocol

  ## Add missing columns for Brood_data
  Brood_data <- Brood_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Brood_data[1, !(
        names(data_templates[[paste0("v", protocol_version)]]$Brood_data) %in% names(.)
      )]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Brood_data))

  ## Add missing columns for Capture_data
  Capture_data <- Capture_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Capture_data[1, !(
        names(data_templates[[paste0("v", protocol_version)]]$Capture_data) %in% names(.)
      )]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Capture_data))

  ## Add missing columns for Individual_data
  Individual_data <- Individual_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Individual_data[1, !(
        names(data_templates[[paste0("v", protocol_version)]]$Individual_data) %in% names(.)
      )]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Individual_data))

  ## Add missing columns for Location_data
  Location_data <- Location_data %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Location_data[1, !(
        names(data_templates[[paste0("v", protocol_version)]]$Location_data) %in% names(.)
      )]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Location_data))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if (output_type == "csv") {
    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_MAY.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MAY.csv"), row.names = FALSE)

    utils::write.table(
      x = protocol_version, file = paste0(path, "\\protocol_version_MAY.txt"),
      quote = FALSE, row.names = FALSE, col.names = FALSE
    )

    invisible(NULL)
  }

  if (output_type == "R") {
    message("Returning R objects...")

    return(list(
      Brood_data = Brood_data,
      Capture_data = Capture_data,
      Individual_data = Individual_data,
      Location_data = Location_data,
      protocol_version = protocol_version
    ))
  }
}


# Dates are recorded as days since 1 May.
# Sometimes with special characters (probably translation errors?).
# These are are ignored. They can also be  a range
# which is converted into a minimum and maximum date.
# The observed date is the rounded average of the range.
parse_MAY_date <- function(x, year) {
  # A leading minus sign is not a range, but a data before May 1st.
  has_range <- stringr::str_detect(x, "[:digit:]-[:digit:]") & !is.na(x)

  cleaned <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(x, "<") ~ stringr::str_remove(x, "<"),
    stringr::str_detect(x, ">") ~ stringr::str_remove(x, ">"),
    stringr::str_detect(x, "^\\?") ~ NA_character_,
    stringr::str_detect(x, "\\?") ~ stringr::str_remove(x, "\\?"),
    stringr::str_detect(x, "\\(") ~ stringr::str_extract(x, "(?<=\\()[:digit:]{1,2}(?=\\))"),
    has_range ~ as.character(floor((as.integer(stringr::str_extract(x, "[:digit:]{1,2}(?=-)")) +
      as.integer(stringr::str_extract(x, "(?<=-)[:digit:]{1,2}"))) / 2)),
    TRUE ~ x
  )

  base <- lubridate::as_date(paste0(year, "-04-30"))
  observed <- base + as.integer(cleaned)
  min_date <- dplyr::if_else(has_range, base + as.integer(stringr::str_extract(x, "[:digit:]{1,2}(?=-)")), observed)
  max_date <- dplyr::if_else(has_range, base + as.integer(stringr::str_extract(x, "(?<=-)[:digit:]{1,2}")), observed)

  tibble::tibble(observed = observed, min = min_date, max = max_date)
}


# Counts are usually plain integers.
# Sometimes they contain special characters which are ignored.
# Some are written as an arithmetic expression  which is evaluated (i.e.
# Rows with letter are assumed unkown.
parse_MAY_count <- function(x) {
  cleaned <- stringr::str_replace_all(x, " ", "")

  cleaned <- dplyr::case_when(
    is.na(cleaned) ~ NA_character_,
    stringr::str_detect(cleaned, "\\?") ~ dplyr::na_if(stringr::str_remove(cleaned, "\\?"), ""),
    stringr::str_detect(cleaned, "\\(") ~ stringr::str_extract(cleaned, "(?<=\\()[:digit:]{1,2}(?=\\))"),
    stringr::str_detect(cleaned, "[:alpha:]") ~ NA_character_,
    TRUE ~ cleaned
  )

  vapply(cleaned, FUN.VALUE = integer(1), USE.NAMES = FALSE, FUN = function(value) {
    if (is.na(value) || value == "") {
      return(NA_integer_)
    }

    as.integer(eval(parse(text = value)))
  })
}


#' Create brood data table for Mayachino, Russia.
#'
#' Create brood data table in standard format for data from Mayachino, Russia.
#'
#' @param gt_data Data frame. Great tit data from Mayachino, Russia.
#' @param pf_data Data frame. Pied flycatcher data from Mayachino, Russia.
#' @param species_filter Species six-letter codes from the standard protocol.
#' Used to filter the data.
#'
#' @return A data frame.

create_brood_MAY <- function(gt_data,
                             pf_data,
                             species_filter) {
  brood_cols <- c(
    "BroodID", "PopID", "BreedingSeason", "Species", "Plot", "LocationID",
    "FemaleID", "MaleID", "ClutchType_observed",
    "LayDate_observed", "LayDate_min", "LayDate_max",
    "ClutchSize_observed",
    "HatchDate_observed", "HatchDate_min", "HatchDate_max",
    "BroodSize_observed", "NumberFledged_observed", "ExperimentID"
  )

  # Pied flycatcher data
  pf_broods <- pf_data %>%
    # Create female & male IDs
    tidyr::unite("FemaleID", "females_ring_series", "females_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite("MaleID", "males_ring_series", "males_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c("FemaleID", "MaleID"),
        .fns = ~ stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")
      ),
      # If FemaleID & MaleID differ from the expected format it is set to NA
      # TODO: Check IDs with data custodian: many missing letters in ringnumbers
      FemaleID = dplyr::if_else(stringr::str_detect(.data$FemaleID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
        .data$FemaleID, NA_character_
      ),
      MaleID = dplyr::if_else(stringr::str_detect(.data$MaleID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
        .data$MaleID, NA_character_
      ),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year),
      hatch = parse_MAY_date(.data$hatching_date_1_may_1, .data$year),
      ClutchSize_observed = parse_MAY_count(.data$clutch_size),
      BroodSize_observed = parse_MAY_count(.data$number_of_hatched_nestlings),
      NumberFledged_observed = parse_MAY_count(.data$number_of_fledlings),
      # TODO: Check with data custodian whether rows withouth letters and only
      # numbers are unique and valid.
      BroodID = paste(.data$year, .data$the_line_of_nest_boxes, .data$no_nest_box,
        .data$no_string_1,
        sep = "_"
      ),
      PopID = "MAY",
      BreedingSeason = as.integer(.data$year),
      Species = "FICHYP",
      # TODO: Does the column 'line of nest boxes' indicate plot?
      Plot = toupper(.data$the_line_of_nest_boxes),
      LocationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
      # TODO: No clutch type info for pied flycatchers, correct?
      ClutchType_observed = NA_character_,
      # TODO: Some nests are marked as "experiment".
      # Check what kind of experiment.
      ExperimentID = dplyr::if_else(stringr::str_detect(.data$the_cause_of_the_nests_death, "experiment"),
        "OTHER", NA_character_
      )
    ) %>%
    tidyr::unpack(c("lay", "hatch"), names_sep = "_") %>%
    dplyr::rename(
      LayDate_observed = "lay_observed", LayDate_min = "lay_min", LayDate_max = "lay_max",
      HatchDate_observed = "hatch_observed", HatchDate_min = "hatch_min", HatchDate_max = "hatch_max"
    ) %>%
    dplyr::select(dplyr::all_of(brood_cols))

  # Great tit data
  gt_broods <- gt_data %>%
    # Create female & male IDs
    tidyr::unite("FemaleID", "females_ring_series", "females_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite("MaleID", "males_ring_series", "males_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    # Data custodian writes: 1 - normal first; 2 - normal second; 1 or 2 repeat, after losing 1 or 2 brood; (1 or 2) in brackets possibly first, second, or repeat brood
    # TODO: Check interpretation with data custodian
    dplyr::rename(clutchType = "no_of_brood_1_normal_first_2_normal_second_1_or_2_repeat_after_losing_1_or_2_brood_1_or_2_in_brackets_possibly_first_second_or_repeat_brood") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c("FemaleID", "MaleID"),
        .fns = ~ stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")
      ),
      FemaleID = dplyr::if_else(stringr::str_detect(.data$FemaleID, "^[:upper:]{2}[:digit:]{5,6}$"),
        .data$FemaleID, NA_character_
      ),
      MaleID = dplyr::if_else(stringr::str_detect(.data$MaleID, "^[:upper:]{2}[:digit:]{5,6}$"),
        .data$MaleID, NA_character_
      ),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year),
      hatch = parse_MAY_date(.data$hatching_date_1_may_1, .data$year),
      ClutchSize_observed = parse_MAY_count(.data$clutch_size_in_brackets_possibly_number_of_eggs),
      BroodSize_observed = parse_MAY_count(.data$number_of_hatched_nestlings_in_brackets_possibly_number_of_nestlings),
      NumberFledged_observed = parse_MAY_count(.data$number_of_fledlings),
      BroodID = paste(.data$year, .data$the_line_of_nest_boxes, .data$no_nest_box,
        .data$no_string_1,
        sep = "_"
      ),
      PopID = "MAY",
      BreedingSeason = as.integer(.data$year),
      Species = "PARMAJ",
      Plot = toupper(.data$the_line_of_nest_boxes),
      LocationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
      # TODO: Ambiguous "x or repeat" entries are mapped to "second", correct?
      ClutchType_observed = dplyr::case_when(
        .data$clutchType %in% c("1", "(1)") ~ "first",
        stringr::str_detect(.data$clutchType, "1.*repeat") ~ "replacement",
        stringr::str_detect(.data$clutchType, "[2-3]") ~ "second",
        TRUE ~ NA_character_
      ),
      ExperimentID = NA_character_
    ) %>%
    tidyr::unpack(c("lay", "hatch"), names_sep = "_") %>%
    dplyr::rename(
      LayDate_observed = "lay_observed", LayDate_min = "lay_min", LayDate_max = "lay_max",
      HatchDate_observed = "hatch_observed", HatchDate_min = "hatch_min", HatchDate_max = "hatch_max"
    ) %>%
    dplyr::select(dplyr::all_of(brood_cols))

  Brood_data <- dplyr::bind_rows(pf_broods, gt_broods) %>%
    dplyr::filter(.data$Species %in% {{ species_filter }}) %>%
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE))

  return(Brood_data)
}


#' Create capture data table for Mayachino, Russia.
#'
#' Create capture data table in standard format for data from Mayachino, Russia.
#'
#' @param gt_data Data frame. Great tit data from Mayachino, Russia.
#' @param pf_data Data frame. Pied flycatcher data from Mayachino, Russia.
#' @param species_filter Species six-letter codes from the standard protocol.
#' Used to filter the data.
#'
#' @return A data frame.

create_capture_MAY <- function(gt_data,
                               pf_data,
                               species_filter) {
  # 1. Pied flycatcher parents
  pf_parents <- pf_data %>%
    tidyr::unite("FemaleID", "females_ring_series", "females_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite("MaleID", "males_ring_series", "males_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c("FemaleID", "MaleID"),
        .fns = ~ stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")
      ),
      FemaleID = dplyr::if_else(stringr::str_detect(.data$FemaleID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
        .data$FemaleID, NA_character_
      ),
      MaleID = dplyr::if_else(stringr::str_detect(.data$MaleID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
        .data$MaleID, NA_character_
      ),
      ClutchSize = parse_MAY_count(.data$clutch_size),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year)
    ) %>%
    tidyr::unpack("lay", names_sep = "_") %>%
    dplyr::mutate(CaptureDate = .data$lay_observed + .data$ClutchSize) %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"), names_to = "sex", values_to = "IndvID") %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::mutate(
      Sex_observed = dplyr::if_else(.data$sex == "FemaleID", "F", "M"),
      # TODO: Check with data custodian how to interpret ages.
      age = dplyr::if_else(.data$Sex_observed == "F", .data$females_age, .data$males_age),
      age = dplyr::na_if(.data$age, "registered earlier"),
      # TODO: Check units and tarsus measurement method with data custodian
      Tarsus = as.numeric(dplyr::na_if(
        dplyr::if_else(.data$Sex_observed == "F",
          as.character(.data$female_tarsus_length),
          .data$male_tarsus_length
        ),
        "registered earlier"
      )),
      WingLength = as.numeric(dplyr::na_if(
        dplyr::if_else(.data$Sex_observed == "F",
          as.character(.data$female_wing_length),
          as.character(.data$male_wing_length)
        ),
        "registered earlier"
      )),
      ChickAge = NA_integer_,
      Species = "FICHYP"
    )

  # 2. Pied flycatcher chicks
  message("Completing sequence of pied flycatcher chick IDs")
  pb_pf <- progress::progress_bar$new(total = nrow(pf_data))

  pf_chicks <- pf_data %>%
    dplyr::mutate(IndvID = purrr::map(
      .x = .data$nestling_rings,
      .f = ~ {
        pb_pf$tick()

        retrieve_chickIDs_MAY(.x)
      }
    )) %>%
    tidyr::unnest(cols = "IndvID") %>%
    dplyr::mutate(IndvID = dplyr::if_else(stringr::str_detect(.data$IndvID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
      .data$IndvID, NA_character_
    )) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::mutate(
      ClutchSize = parse_MAY_count(.data$clutch_size),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year)
    ) %>%
    tidyr::unpack("lay", names_sep = "_") %>%
    dplyr::mutate(
      CaptureDate = .data$lay_observed + .data$ClutchSize,
      Sex_observed = NA_character_,
      Tarsus = NA_real_,
      WingLength = NA_real_,
      ChickAge = NA_integer_,
      age = "chick",
      Species = "FICHYP"
    )

  # 3. Great tit parents
  gt_parents <- gt_data %>%
    tidyr::unite("FemaleID", "females_ring_series", "females_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    tidyr::unite("MaleID", "males_ring_series", "males_ring", remove = FALSE, na.rm = TRUE, sep = "") %>%
    dplyr::rename(
      FemaleAge = "females_age_1_one_year_old_bird_hatched_last_breeding_season_2_two_or_more_years_old_an_adult_hatched_before_the_last_calendar_year_3_or_4_age_3_4_or_more_years",
      MaleAge = "males_age_1_one_year_old_bird_hatched_last_breeding_season_2_two_or_more_years_old_an_adult_hatched_before_the_last_calendar_year_3_or_4_age_3_4_or_more_years"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c("FemaleID", "MaleID"),
        .fns = ~ stringr::str_replace_all(dplyr::na_if(.x, ""), pattern = " ", replacement = "")
      ),
      FemaleID = dplyr::if_else(stringr::str_detect(.data$FemaleID, "^[:upper:]{2}[:digit:]{5,6}$"),
        .data$FemaleID, NA_character_
      ),
      MaleID = dplyr::if_else(stringr::str_detect(.data$MaleID, "^[:upper:]{2}[:digit:]{5,6}$"),
        .data$MaleID, NA_character_
      ),
      ClutchSize = parse_MAY_count(.data$clutch_size_in_brackets_possibly_number_of_eggs),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year)
    ) %>%
    tidyr::unpack("lay", names_sep = "_") %>%
    dplyr::mutate(CaptureDate = .data$lay_observed + .data$ClutchSize) %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"), names_to = "sex", values_to = "IndvID") %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::mutate(
      Sex_observed = dplyr::if_else(.data$sex == "FemaleID", "F", "M"),
      age = dplyr::if_else(.data$Sex_observed == "F", .data$FemaleAge, .data$MaleAge),
      age = dplyr::na_if(.data$age, "registered earlier this season"),
      # No morphometric measurements were taken for great tits
      Tarsus = NA_real_,
      WingLength = NA_real_,
      ChickAge = NA_integer_,
      Species = "PARMAJ"
    )

  # 4. Great tit chicks
  message("Completing sequence of great tit chick IDs")
  pb_gt <- progress::progress_bar$new(total = nrow(gt_data))

  gt_chicks <- gt_data %>%
    dplyr::mutate(IndvID = purrr::map(
      .x = .data$nestling_rings,
      .f = ~ {
        pb_gt$tick()

        retrieve_chickIDs_MAY(.x)
      }
    )) %>%
    tidyr::unnest(cols = "IndvID") %>%
    dplyr::mutate(IndvID = dplyr::if_else(stringr::str_detect(.data$IndvID, "^[:upper:]{0,2}[:digit:]{5,6}$"),
      .data$IndvID, NA_character_
    )) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::mutate(
      ClutchSize = parse_MAY_count(.data$clutch_size_in_brackets_possibly_number_of_eggs),
      lay = parse_MAY_date(.data$start_date_of_laying_1_may_1, .data$year)
    ) %>%
    tidyr::unpack("lay", names_sep = "_") %>%
    # TODO: Chick capture date uses the same lay date + clutch size proxy as parents as the exact ringing date is unkown/unrecorded.
    dplyr::mutate(
      CaptureDate = .data$lay_observed + .data$ClutchSize,
      Sex_observed = NA_character_,
      Tarsus = NA_real_,
      WingLength = NA_real_,
      ChickAge = NA_integer_,
      age = "chick",
      Species = "PARMAJ"
    )

  # 5. Combine capture tables
  Capture_data <- dplyr::bind_rows(pf_parents, pf_chicks, gt_parents, gt_chicks) %>%
    dplyr::mutate(
      BreedingSeason = as.integer(.data$year),
      BroodID = paste(.data$year, .data$the_line_of_nest_boxes, .data$no_nest_box,
        .data$no_string_1,
        sep = "_"
      ),
      LocationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_"),
      CapturePopID = "MAY",
      ReleasePopID = "MAY",
      CapturePlot = toupper(.data$the_line_of_nest_boxes),
      ReleasePlot = .data$CapturePlot,
      CaptureTime = NA_character_,
      ObserverID = NA_character_,
      CaptureAlive = TRUE,
      ReleaseAlive = TRUE,
      OriginalTarsusMethod = dplyr::if_else(!is.na(.data$Tarsus), "Alternative", NA_character_),
      Age_observed = dplyr::case_when(
        .data$age == "chick" ~ 1L,
        .data$age == "1" ~ 5L,
        .data$age %in% as.character(2:8) ~ 6L,
        TRUE ~ NA_integer_
      ),
      ExperimentID = NA_character_
    ) %>%
    dplyr::filter(.data$Species %in% {{ species_filter }}) %>%
    # CaptureDate cannot be approximated without a lay date and clutch size; such captures are dropped
    # TODO: Correct?
    dplyr::filter(!is.na(.data$CaptureDate)) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    calc_age(
      data = ., ID = .data$IndvID, Age = .data$Age_observed,
      Date = .data$CaptureDate, Year = .data$BreedingSeason
    ) %>%
    dplyr::select(
      "CaptureID", "IndvID", "Species", "Sex_observed", "BreedingSeason",
      "CaptureDate", "CaptureTime", "ObserverID", "LocationID",
      "CaptureAlive", "ReleaseAlive", "CapturePopID", "CapturePlot",
      "ReleasePopID", "ReleasePlot", "Tarsus", "OriginalTarsusMethod",
      "WingLength", "Age_observed", "Age_calculated", "ChickAge",
      "ExperimentID", "BroodID"
    )

  return(Capture_data)
}


#' Create individual data table for Mayachino, Russia.
#'
#' Create individual data table in standard format for data from Mayachino, Russia.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_MAY}}.
#' @param species_filter Species six-letter codes from the standard protocol.
#' Used to filter the data.
#'
#' @return A data frame.

create_individual_MAY <- function(capture_data,
                                  species_filter) {
  Individual_data <- capture_data %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise( # TODO: Check individuals recorded as both great tit and pied flycatcher with data custodian
      Species = dplyr::if_else(dplyr::n_distinct(.data$Species) > 1, "CCCCCC", dplyr::first(.data$Species)),
      RingSeason = as.integer(dplyr::first(.data$BreedingSeason)),
      RingAge = dplyr::if_else(dplyr::first(.data$Age_observed) == 1L, "chick", "adult"),
      FirstBroodID = dplyr::first(.data$BroodID),
      Sex_calculated = {
        sexes <- stats::na.omit(unique(.data$Sex_observed))

        if (length(sexes) == 1) sexes else NA_character_
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      PopID = "MAY",
      # Only assign a brood ID if first caught as a chick. Assume no swap experiments.
      BroodIDLaid = dplyr::if_else(.data$RingAge == "chick", .data$FirstBroodID, NA_character_),
      BroodIDFledged = .data$BroodIDLaid,
      Sex_genetic = NA_character_
    ) %>%
    dplyr::filter(.data$Species %in% {{ species_filter }}) %>%
    dplyr::select(
      "IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged",
      "RingSeason", "RingAge", "Sex_calculated", "Sex_genetic"
    )

  return(Individual_data)
}


#' Create location data table for Mayachino, Russia.
#'
#' Create location data table in standard format for data from Mayachino, Russia.
#'
#' @param gt_data Data frame. Great tit data from Mayachino, Russia.
#' @param pf_data Data frame. Pied flycatcher data from Mayachino, Russia.
#'
#' @return A data frame.

create_location_MAY <- function(gt_data,
                                pf_data) {
  combined <- dplyr::bind_rows(
    pf_data %>% dplyr::select("the_line_of_nest_boxes", "no_nest_box", "year"),
    gt_data %>% dplyr::select("the_line_of_nest_boxes", "no_nest_box", "year")
  )

  # There are no coordinates known, or type of next boxes used.
  Location_data <- combined %>%
    dplyr::mutate(LocationID = paste(.data$the_line_of_nest_boxes, .data$no_nest_box, sep = "_")) %>%
    dplyr::select("LocationID") %>%
    tidyr::drop_na() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      NestboxID = .data$LocationID,
      LocationType = "NB",
      PopID = "MAY",
      Latitude = NA_real_,
      Longitude = NA_real_,
      StartSeason = as.integer(min(combined$year, na.rm = TRUE)),
      EndSeason = NA_integer_,
      HabitatType = NA_character_
    )

  return(Location_data)
}


#' Retrieve chick IDs in MAY pipeline
#'
#' In MAY primary data, the chick IDs in a brood are stored as series of partially incomplete character sequences (e.g., "856840,1,55-62", "099362-65"). This function extracts the full sequence of characters for each ID in the series. "-" are interpreted as a range; "," are interpreted as a regular separator. Values in other formats (e.g., "without a rings", "531094.95999999996") and sequences that lead to an excessive number of IDs (e.g. "54522-291") are set to NA.
#'
#' @param chickID Character. The series of partially incomplete chick IDs of a brood.
#'
#' @return A vector with the complete chick IDs of a brood, or NA.
#'
#' @export
#'
#' @examples
#'
#' retrieve_chickIDs_MAY("856840,1,55-62")
#'
retrieve_chickIDs_MAY <- function(chickID) {
  # Chicks from the same brood get ringed in one sitting, so their ring numbers
  # are sequential. Rather than writing out every full number, whoever filled
  # in the sheet wrote the first one in full and then just the digits that
  # changed for the rest, e.g. "856840,1,55-62" means ring 856840, then 856841,
  # then the run 856855 through 856862. Before we can make sense of any of
  # that we need to agree on what counts as a separator: a comma means "next,
  # unrelated entry", a dash means "everything in between, inclusive". Problem
  # is, not everyone filled in the sheet the same way, so the first thing we
  # do is force every other punctuation we've seen used for "next entry"
  # (a plain space between two numbers, a semicolon, a plus sign) into a
  # comma, so the rest of the function only has to deal with two delimiters.
  chickID <- stringr::str_replace_all(chickID, pattern = "(?<=[:digit:])[:space:](?=[:digit:])", replacement = ",")
  # Any other space is just sloppy typing and can go, and so can the
  # occasional explanatory note in brackets, it's not part of the ring
  # sequence and we have no good way to parse free text anyway.
  chickID <- stringr::str_remove_all(chickID, pattern = " ")
  chickID <- stringr::str_remove_all(chickID, pattern = "\\(.*\\)")
  chickID <- stringr::str_replace_all(chickID, pattern = ";", replacement = ",")
  chickID <- stringr::str_replace_all(chickID, pattern = "\\+", replacement = ",")

  # A handful of rows aren't a ring sequence at all and trying to force them
  # through the logic below would just produce garbage, so we bail out to NA
  # instead. That covers: a string with neither a comma nor a dash (i.e.
  # nothing here looks like our shorthand notation), a literal note like
  # "without a ring" rather than a ring number, a number that Excel has
  # mangled into a long decimal (e.g. "531094.95999999996", at which point the
  # real ring number is unrecoverable), or a note written in Cyrillic instead
  # of digits.
  na_strings <- c("without", "\\.", "[\\p{Cyrillic}]")

  if (stringr::str_detect(chickID, "[-,]", negate = TRUE) | stringr::str_detect(chickID, paste(na_strings, collapse = "|")) | is.na(chickID)) {
    output <- NA

    # Otherwise we've got something that looks like a real series, so try to
    # reconstruct the full ring numbers from it.
  } else {
    # A handful of ring series start with one or two letters,
    # which stay the same for every chick in the brood. Peel them off the
    # front now, work through the numbers on their own, and stick the letters
    # back on every resulting ID right at the end.
    id_letters <- stringr::str_extract(chickID, "[:alpha:]{1,2}")
    id_numbers <- stringr::str_remove(chickID, "[:alpha:]{1,2}")

    # Splitting on "-" and "," gives us the bare number fragments in order,
    # but it throws away which delimiter sat between each pair, so we grab
    # that separately and keep it in the same order, we'll need to know later
    # which gaps were ranges and which were just separate chicks.
    id_series <- stringr::str_split(id_numbers, pattern = "[-,]")[[1]]

    special_chars <- stringr::str_extract_all(chickID, "[-,]")[[1]]

    # Eventually every fragment is padded against the very first number in
    # the row (see new_series below). But "79699-700,157-60" means 79699,
    # 79700, and 79157 through 79160 - "157-60" is padded against "157", not
    # "79699" (which would wrongly turn "60" into "79660"). So first we walk
    # back from each fragment to the closest earlier one with more digits,
    # instead of always using the first. id_ref collects position 1 plus any
    # position that turns out to be such a closer reference - here, position
    # 3 ("157"), because "60" needed it. "157" itself still gets folded into
    # "79699" in the final pass, ending up as "79157".
    id_ref <- purrr::map_dbl(2:length(id_series), ~ {
      if (any(nchar(id_series) > nchar(id_series[.x]))) {
        ref <- which(nchar(id_series) > nchar(id_series[.x]))
        max(ref[.x > ref])
      } else {
        ref <- 1
      }
    })

    id_ref <- unique(c(1, id_ref))

    # Continuing the example: id_ref is c(1, 3). This step fixes "60"
    # (position 4, shorter than "157" at position 3) by padding it against
    # "157" instead of "79699": one leading digit, giving "160". "700"
    # (position 2) is untouched here - it comes before position 3, so it
    # waits for the next step, padded against "79699" with everything else.
    if (length(id_ref) > 1) {
      id_series <- purrr::map_chr(
        .x = seq_len(length(id_series)),
        .f = ~ {
          if (.x > id_ref[2] & nchar(id_series[.x]) < nchar(id_series[id_ref[2]])) {
            ref_length <- nchar(id_series[id_ref[2]])
            no_length <- nchar(id_series[.x])

            paste0(
              stringr::str_sub(id_series[id_ref[2]], start = 1, end = ref_length - no_length),
              id_series[.x]
            )

            # Fragments before the second reference are left as they are -
            # they'll get padded against the first reference in the next step.
          } else {
            id_series[.x]
          }
        }
      )
    }

    # Now pad every fragment against the leading digits of the very first
    # number in the row. This is harmless for fragments that are already
    # full numbers (the padding ends up zero-length), and it's what actually
    # turns the abbreviated fragments into full ring numbers.
    new_series <- purrr::map_chr(
      .x = seq_len(length(id_series)),
      .f = ~ {
        ref_length <- nchar(id_series[id_ref[1]])
        no_length <- nchar(id_series[.x])

        paste0(
          stringr::str_sub(id_series[1], start = 1, end = ref_length - no_length),
          id_series[.x]
        )
      }
    )


    # We now have full numbers but have lost track of which gaps were ranges,
    # so weave the separators back in between them in their original order,
    # and swap "-" for ":" so the result reads as a valid R range expression.
    # Rather than writing our own loop to expand "855:862" into 855, 856, ...,
    # 862, we just let R's own range operator do that for us.
    new_string <- paste0("c(", stringr::str_replace_all(stringr::str_flatten(c(new_series, special_chars)[order(c(seq_along(new_series), seq_along(special_chars)))]), pattern = "-", replacement = ":"), ")")

    new_ids <- eval(parse(text = new_string))

    # Expanding a range through R's integer arithmetic drops any leading
    # zero the original fragment had (e.g. "099362" becomes 99362), so pad
    # everything back out to the width of the first fragment to restore it.
    output <- stringr::str_pad(as.character(new_ids), width = nchar(id_series[1]), side = "left", pad = 0)

    # Glue the ring-series letters we peeled off at the start back onto every
    # ID we've produced, if there were any.
    if (!is.na(id_letters)) {
      output <- paste0(id_letters, output)
    }
  }

  # If something above has gone wrong - a dash meant something other than a
  # range, or two unrelated numbers got chained together into one giant run -
  # we can end up "expanding" a row into dozens of IDs, which isn't plausible
  # for a real brood of these species. Treat that as a sign the parsing
  # failed rather than as real data.
  # TODO: Check with data custodian
  if (length(output) > 15) {
    output <- NA
  }

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
# TODO: Check capture dates of parents (particularly when laying dates are NA)
# TODO: Check units of age columns
# TODO: Check chick rings with data custodian (missing letters?)
# TODO: Check capture dates of chicks (average incubation length, chick age at ringing)
# TODO: Check chick age
# TODO: Check whether individuals were only caught/released alive & physically
# TODO: Check individuals that are recorded as great tit and pied flycatcher
# TODO: Check location info: location type, start year of boxes, coordinates, habitat type
# TODO: Check units of measurements
# TODO: Check tarsus method
# TODO: Check experiment info
# TODO: Check whether dropping captures with no derivable CaptureDate is correct
# TODO: Check whether 15 IDs is the right cutoff for an implausible chick ring sequence
