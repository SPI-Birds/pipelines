#' Test column presence
#'
#' Test whether expected columns are present
#'
#' This function is intended to be used in the test for every pipeline in
#' order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param table Character string. Which data table should be tested? One of:
#' "Brood", "Individual", "Capture", "Location", "Measurement" or
#' "Experiment".
#' @param protocol_version The protocol version of the SPI-Birds
#' standard data being used to process the primary data. Either "1.0.0",
#' "1.1.0", or "2.0.0".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a vector of columns that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'

test_col_present <- function(pipeline_output,
                             table,
                             protocol_version,
                             debug = FALSE) {
  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # Select template
  template <- data_templates[[paste0("v", protocol_version)]][[paste0(
    table, "_data"
  )]]

  # Test
  if (debug == FALSE) {
    eval(bquote(testthat::expect_true(
      setequal(names(template), names(data_table))
    )))
  } else {
    names(data_table)[!(names(data_table) %in% names(template))]
  }
}

#' Test column classes
#'
#' Test whether column classes for pipeline outputs match the expected column
#' classes as specified in the standard format.
#'
#' This function is intended to be used in the test for every pipeline in
#' order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param table Character string. Which data table should be tested? One of:
#' "Brood", "Individual", "Capture", "Location", "Measurement" or
#' "Experiment".
#' @param protocol_version The protocol version of the SPI-Birds
#' standard data being used to process the primary data. Either "1.0.0",
#' "1.1.0", or "2.0.0".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a vector of columns that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'
test_col_classes <- function(pipeline_output,
                             table,
                             protocol_version,
                             debug = FALSE) {
  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # Select template
  template <- data_templates[[paste0("v", protocol_version)]][[paste0(
    table, "_data"
  )]]

  # Retrieve classes of columns in data
  data_col_classes <- purrr::imap_dfr(
    .x = data_table,
    .f = ~ {
      tibble::tibble(
        var = .y,
        data_class = class(.x)
      )
    }
  )

  # Retrieve classes of columns in template
  template_col_classes <- purrr::imap_dfr(
    .x = template,
    .f = ~ {
      tibble::tibble(
        var = .y,
        template_class = class(.x)
      )
    }
  )

  # Left join column classes in data with those in template
  mismatched_classes <- data_col_classes %>%
    dplyr::left_join(template_col_classes, by = "var") %>%
    dplyr::filter(.data$data_class != .data$template_class)

  # Test that there are 0 mismatched classes
  if (debug == FALSE) {
    eval(bquote(testthat::expect_equal(nrow(mismatched_classes), 0)))
  } else {
    mismatched_classes
  }
}

#' Test ID format
#'
#' Test that ID formats for each column are expected. There are 4 ID columns.
#' "IndvID"/"individualID" in the Capture and Individual data, and
#' "FemaleID"/"femaleID" and "MaleID"/"maleID" in the Brood data. This
#' function will take the regular expression that matches the expected ID
#' format for the pipeline and will make sure that the ID column does not
#' have entries that do not match the expected format.
#'
#' This function is intended to be used in the test for every pipeline in
#' order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param column Character string. Which ID column should be tested? For
#' v1.0.0 and v1.1.0, one of: "FemaleID", "MaleID", "IndvID". For v2.0.0,
#' one of: "femaleID", "maleID", "individualID".
#' @param table Character string. If testing "IndvID"/"individualID", the
#' table in which the column is tested. Either, "Individual" or "Capture".
#' NULL (default) for all other IDs in `ID_col`.
#' @param format Character string. Regular expression corresponding to the
#' expected ID format.
#'
#' @return Logical (TRUE/FALSE) for whether all of IDs are formatted as
#' expected.
#' @export
#'
# TODO: Add measurementID, locationID, treatmentID?
test_ID_format <- function(pipeline_output,
                           column,
                           table = NULL,
                           format) {
  # Ensure that a table is specified when testing IndvID/individualID
  if (column %in% c("IndvID", "individualID") & is.null(table)) {
    stop(paste0(
      "You must select the table in which you want to test ", column, "."
    ))
  }

  # Test
  eval(bquote(testthat::expect_true(

    # FemaleID/femaleID, MaleID/maleID
    if (column %in% c("FemaleID", "femaleID", "MaleID", "maleID")) {
      all(stringr::str_detect(
        pipeline_output[[1]][[column]],
        format
      ), na.rm = TRUE)
    }

    ## IndvID/individualID in Capture/Individual data
    else if (column %in% c("IndvID", "individualID")) {
      all(stringr::str_detect(
        pipeline_output[[paste0(table, "_data")]][[column]],
        format
      ), na.rm = TRUE)
    }
  )))
}

#' Test unique values in key columns
#'
#' Some columns are not allowed to have duplicated values. Namely,
#' BroodID/broodID (Brood data), CaptureID/captureID (Capture data),
#' IndvID/individualID (Individual data), measurementID (Measurement data),
#' locationID (Location data; NB: only in >v2.0.0), treatmentID (Experiment
#' data)
#' This function takes a column name and returns a logical value
#' corresponding to whether all values in the column are unique (TRUE) or
#' not (FALSE).
#'
#' This function is intended to be used in the test for every pipeline in
#' order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param column Character string. Which column should be checked for
#' duplicates? For v1.0.0 and v1.1.0, one of: "BroodID", "CaptureID", or
#' "PopID-IndvID". For v2.0.0, one of: "broodID", "captureID",
#' "individualID", "measurementID", "locationID", "treatmentID".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a vector of IDs that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'
test_unique_values <- function(pipeline_output,
                               column,
                               debug = FALSE) {
  # Select IDs
  ## BroodID/broodID
  if (column %in% c("BroodID", "broodID")) {
    IDs <- pipeline_output$Brood_data[[column]]
  }

  ## CaptureID/captureID
  else if (column %in% c("CaptureID", "captureID")) {
    IDs <- pipeline_output$Capture_data[[column]]
  }

  ## IndvID
  else if (column == "IndvID") {
    IDs <- paste(
      pipeline_output[[3]]$PopID, pipeline_output[[3]]$IndvID,
      sep = "-"
    )
  }

  ## individualID
  else if (column == "individualID") {
    IDs <- pipeline_output$Individual_data$individualID
  }

  ## measurementID
  else if (column == "measurementID") {
    IDs <- pipeline_output$Measurement_data[[column]]
  }

  ## locationID
  else if (column == "locationID") {
    IDs <- pipeline_output$Location_data[[column]]
  }

  ## treatmentID
  else if (column == "treatmentID") {
    IDs <- pipeline_output$Experiment_data[[column]]
  }

  # Select any duplicated IDs
  duplicated_IDs <- IDs[duplicated(IDs)]

  # Test that there are 0 duplicated IDs
  if (debug == FALSE) {
    eval(bquote(testthat::expect_true(
      !any(duplicated(IDs))
    )))
  } else {
    IDs[duplicated(IDs)]
  }
}

#' Test for NAs in key columns
#'
#' Some key columns are not allowed to have NAs. The data templates for each
#' table have dummy values entered when the column cannot contain any NAs.
#' This function takes a table name and returns a logical value corresponding
#' to whether there are any NAs in the key columns in each data table.
#' In the case of NAs, the test fails.
#'
#' This function is intended to be used in the test for every pipeline in
#' order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param table Which data table should be checked for NAs in the key
#' columns? One of: "Brood", "Capture", "Individual", "Measurement,
#' "Location", or "Experiment".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a vector of columns that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'

test_NA_columns <- function(pipeline_output,
                            table,
                            debug = FALSE) {
  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # List of key variables in the selected data table that should not have
  # missing values
  key_vars <- key_variables[[paste0(table, "_data")]]

  # Test for NAs
  if (debug == FALSE) {
    eval(bquote(
      testthat::expect_equal(
        data_table %>%
          dplyr::select(tidyselect::any_of(key_vars)) %>%
          dplyr::select(tidyselect::where(~ any(is.na(.)))) %>%
          ncol(),
        0
      )
    ))
  } else {
    data_table %>%
      dplyr::select(tidyselect::any_of(key_vars)) %>%
      dplyr::select(tidyselect::where(~ any(is.na(.)))) %>%
      names()
  }
}

#' Test category columns
#'
#' Some columns should only include particular categorical values.
#' For example, 'observedClutchType' should only include any of "first",
#' "second", "replacement" or NA.
#' This test makes sure that there are no unexpected values in these
#' different category columns.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param table Which table should be checked? One of: "Brood", "Capture",
#' "Individual", "Location", "Measurement", or "Experiment".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a list of columns (and the unexpected values in
#' them) that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'

test_category_columns <- function(pipeline_output,
                                  table,
                                  debug = FALSE) {
  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # List of categorical variables and their possible categories present in
  # the selected data table
  categories <- categorical_variables[[paste0(table, "_data")]]

  # Remove absent categorical variables from categories
  # NB: Categorical variable may be absent because categorical_variables
  # list has all categorical variables from all versions of the standard
  # format
  categories <- categories[names(categories) %in% names(data_table)]

  # Check for each variable whether unique values are possible categories
  unexpected_cat <- purrr::map2(
    .x = categories,
    .y = names(categories),
    .f = ~ {
      # Get unique variable values
      column_vals <- data_table %>%
        dplyr::select({{ .y }}) %>%
        dplyr::pull() %>%
        unique() %>%
        # Split if multiple categories are
        # concatenated (e.g., in case of
        # ExperimentID [v1.0.0/v1.1.0])
        strsplit(split = ";") %>%
        unlist() %>%
        as.character()

      column_vals[!(column_vals %in% .x)]
    }
  )

  # Test
  if (debug == FALSE) {
    eval(bquote(testthat::expect_true(
      all(purrr::map_lgl(
        .x = unexpected_cat,
        .f = ~ length(.x) == 0
      ))
    )))
  } else {
    unexpected_cat %>%
      purrr::keep(~ length(.x) != 0)
  }
}

#' Test date columns
#'
#' Date columns should only include certain ranges.
#' For example, 'maximumHatchYear' should have a value between 1000 and 9999".
#' This test makes sure that there are no unexpected or non-valide
#' values in these date  columns.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param protocol_version The protocol version of the SPI-Birds
#' standard data being used to process the primary data. Either "1.0.0",
#' "1.1.0", or "2.0.0".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a list of columns (and the unexpected values in
#' them) that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'

test_date_columns <- function(pipeline_output,
                              protocol_version,
                              debug = FALSE) {
  # Read in the expected date columns for this protocol version
  columns <- data_templates[[paste0("v", protocol_version)]]

  # Create vector to store table + column name
  date_column_names <- vector()

  # Extract date column names
  for (i in names(columns)) {
    filt_columns <- colnames(date_columns[[i]] %>%
      dplyr::select((dplyr::contains("Date") | dplyr::contains("Year") |
        dplyr::contains("Month") | dplyr::contains("Day")) &
        !dplyr::contains("Error")))

    if (length(filt_columns != 0)) {
      filt_columns <- paste0(i, "$", filt_columns)
    }

    date_column_names <- c(date_column_names, filt_columns)
  }

  # Create list to store invalid values for debug mode
  invalid_values <- list()

  # Now we can test if the date data complies with the protocol
  for (i in seq_along(date_column_names)) {
    str_parts <- strsplit(date_column_names[i], "\\$")[[1]]

    # Check if we have a "Date" column (protocol version 1.0 or 1.1)
    # or separate Year/Month/Day columns (protocol version 2.0)
    # We need to split the former, but not the latter
    # Might not be the prettiest code with all those if statements,
    # but it works
    if (grepl("Date", str_parts[2])) {
      date_col <- pipeline_output[[str_parts[1]]][[str_parts[2]]]
      Year <- lubridate::year(date_col)
      Month <- lubridate::month(date_col)
      Day <- lubridate::day(date_col)
    } else if (grepl("Year", str_parts[2])) {
      Year <- pipeline_output[[str_parts[1]]][[str_parts[2]]]
      Month <- NULL
      Day <- NULL
    } else if (grepl("Month", str_parts[2])) {
      Year <- NULL
      Month <- pipeline_output[[str_parts[1]]][[str_parts[2]]]
      Day <- NULL
    } else if (grepl("Day", str_parts[2])) {
      Year <- NULL
      Month <- NULL
      Day <- pipeline_output[[str_parts[1]]][[str_parts[2]]]
    }

    # Run testthat on Year
    if (!is.null(Year)) {
      invalid_year <- Year[!is.na(Year) & (Year < 1000 | Year > 9999)]
      if (length(invalid_year) > 0) {
        invalid_values[[paste0(date_column_names[i], "_Year")]] <- invalid_year
      }

      if (debug == FALSE) {
        testthat::expect_true(
          all(is.na(Year) | (Year >= 1000 & Year <= 9999)),
          info = paste0("Year values out of range in ", date_column_names[i])
        )
      }
    }

    # Run testthat on Month
    if (!is.null(Month)) {
      invalid_month <- Month[!is.na(Month) & (Month < 1 | Month > 12)]
      if (length(invalid_month) > 0) {
        invalid_values[[paste0(date_column_names[i], "_Month")]] <- invalid_month
      }

      if (debug == FALSE) {
        testthat::expect_true(
          all(is.na(Month) | (Month >= 1 & Month <= 12)),
          info = paste0("Month values out of range in ", date_column_names[i])
        )
      }
    }

    # Run testthat on Day
    if (!is.null(Day)) {
      invalid_day <- Day[!is.na(Day) & (Day < 1 | Day > 31)]
      if (length(invalid_day) > 0) {
        invalid_values[[paste0(date_column_names[i], "_Day")]] <- invalid_day
      }

      if (debug == FALSE) {
        testthat::expect_true(
          all(is.na(Day) | (Day >= 1 & Day <= 31)),
          info = paste0("Day values out of range in ", date_column_names[i])
        )
      }
    }
  }

  # Return a list with the invalid value if debug is enabled
  if (debug == FALSE) {
    message(paste0("All dates conform protocol ", protocol_version))
    invisible(TRUE)
  } else {
    if (length(invalid_values) == 0) {
      message("No invalid date values found")
      return(list())
    } else {
      return(invalid_values)
    }
  }
}

#' Test time columns
#'
#' Time columns should only include certain ranges.
#' For example, 'CaptureTime' should have hours between 0 and 23 and
#' minutes between 0 and 59.
#' This test makes sure that there are no unexpected or non-valid
#' values in these time columns.
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param protocol_version The protocol version of the SPI-Birds
#' standard data being used to process the primary data. Either "1.0.0",
#' "1.1.0", or "2.0.0".
#' @param debug Logical (TRUE/FALSE) for whether the function returns the
#' test output (FALSE) or a list of columns (and the unexpected values in
#' them) that failed this test (TRUE).
#'
#' @return See `debug`.
#' @export
#'

test_time_columns <- function(pipeline_output,
                              protocol_version,
                              debug = FALSE) {
  # Read in the expected time columns for this protocol version
  columns <- data_templates[[paste0("v", protocol_version)]]

  # Create vector to store table + column name
  time_column_names <- vector()

  # Extract time column names
  for (i in names(columns)) {
    filt_columns <- colnames(columns[[i]] %>%
      dplyr::select(dplyr::contains("Time") &
        !dplyr::contains("Error")))

    if (length(filt_columns) > 0) {
      filt_columns <- paste0(i, "$", filt_columns)
    }

    time_column_names <- c(time_column_names, filt_columns)
  }

  # Create list to store invalid values for debug mode
  invalid_values <- list()

  # Now we can test if the time data complies with the protocol
  for (i in seq_along(time_column_names)) {
    str_parts <- strsplit(time_column_names[i], "\\$")[[1]]

    # Get the time column
    time_col <- pipeline_output[[str_parts[1]]][[str_parts[2]]]

    # Extract hours and minutes from time
    # The protocol does not specify it, but it can have seconds
    # Parse time to HH:MM:SS or HH:MM
    time_parsed <- lubridate::parse_date_time(time_col, orders = c("HMS", "HM"))
    Hour <- lubridate::hour(time_parsed)
    Minute <- lubridate::minute(time_parsed)

    # Run testthat on Hour
    if (!is.null(Hour)) {
      invalid_hour <- Hour[!is.na(Hour) & (Hour < 0 | Hour > 23)]
      if (length(invalid_hour) > 0) {
        invalid_values[[paste0(time_column_names[i], "_Hour")]] <- invalid_hour
      }

      if (debug == FALSE) {
        testthat::expect_true(
          all(is.na(Hour) | (Hour >= 0 & Hour <= 23)),
          info = paste0("Hour values out of range in ", time_column_names[i])
        )
      }
    }

    # Run testthat on Minute
    if (!is.null(Minute)) {
      invalid_minute <- Minute[!is.na(Minute) & (Minute < 0 | Minute > 59)]
      if (length(invalid_minute) > 0) {
        invalid_values[[paste0(time_column_names[i], "_Minute")]] <- invalid_minute
      }

      if (debug == FALSE) {
        testthat::expect_true(
          all(is.na(Minute) | (Minute >= 0 & Minute <= 59)),
          info = paste0("Minute values out of range in ", time_column_names[i])
        )
      }
    }
  }

  # Return a list with the invalid value if debug is enabled
  if (debug == FALSE) {
    message(paste0("All times conform protocol ", protocol_version))
    invisible(TRUE)
  } else {
    if (length(invalid_values) == 0) {
      message("No invalid time values found")
      return(list())
    } else {
      return(invalid_values)
    }
  }
}
