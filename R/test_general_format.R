#' Test column presence
#'
#' Test whether expected columns are present
#'
#' This function is intended to be used in the test for every pipeline in order
#' to ensure consistency between pipelines.
#'
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param data_template Character string. Which data template should be tested? One of:
#' "Brood", "Individual", "Capture", "Location", "Measurement" or "Experiment".
#' @param protocol_version The protocol version of the SPI-Birds
#' standard data being used to process the primary data. Either "1.0", "1.1", or "2.0" (default).
#' @param verbose Logical (TRUE/FALSE) for whether the function returns the test output (FALSE) or a vector of columns that failed this test.
#'
#' @return See `verbose`.
#' @export
#'

test_col_present <- function(pipeline_output,
                             data_template,
                             protocol_version = "2.0",
                             verbose = FALSE) {

  # Select data table
  data_table <- pipeline_output[[paste0(data_template, "_data")]]

  # Select template
  template <- data_templates[[paste0("v", protocol_version)]][[paste0(data_template, "_data")]]

  # Select optional variables
  opt_vars <- utility_variables[[paste0(data_template, "_data")]]

  # Test
  if(verbose == FALSE) {

    eval(bquote(testthat::expect_true(

      setequal(c(names(template), names(opt_vars)), names(data_table))

    )))

  } else {

    names(data_table)[!(names(data_table) %in% c(names(template), names(opt_vars)))]

  }

}

#' Test column classes
#'
#' Test whether column classes for pipeline outputs match the expected column classes as specified in the standard format.
#'
#' This function is intended to be used
#' in the test for every pipeline in order to ensure consistency between pipelines.
#'
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param table Character string. Which data table should be verified with their template? One of:
#' "Brood", "Individual", "Capture", "Location", "Measurement", or "Experiment".
#' @param protocol_version The protocol version of the SPI Birds
#' standard data being used to process the primary data. Either "1.0", "1.1", or "2.0" (default).
#' @param verbose Logical (TRUE/FALSE) for whether the function returns the test output (FALSE) or a data frame of column names that failed this test, their incorrectly assigned classes, and the classes they should have been assigned.
#'
#' @return See `verbose`.
#' @export
#'

test_col_classes <- function(pipeline_output,
                             table,
                             protocol_version = "2.0",
                             verbose = FALSE) {

  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # Select template
  template <- data_templates[[paste0("v", protocol_version)]][[paste0(table, "_data")]]

  # Select optional variables
  opt_vars <- utility_variables[[paste0(table, "_data")]]

  # Retrieve classes of columns in data
  data_col_classes <- purrr::imap_dfr(.x = data_table,
                                      .f = ~{

                                        tibble::tibble(var = .y,
                                                       data_class = class(.x))

                                      })

  # Retrieve classes of columns in template
  template_col_classes <- purrr::imap_dfr(.x = template,
                                          .f = ~{

                                            tibble::tibble(var = .y,
                                                           template_class = class(.x))

                                          })

  # Retrieve classes of columns in optional variables
  optvars_col_classes <- purrr::imap_dfr(.x = opt_vars,
                                         .f = ~{

                                           tibble::tibble(var = .y,
                                                          template_class = class(.x))

                                         })

  # Left join column classes in data with those in template & opt vars
  mismatched_classes <- data_col_classes %>%
    dplyr::left_join(dplyr::bind_rows(template_col_classes, optvars_col_classes), by = "var") %>%
    dplyr::filter(.data$data_class != .data$template_class)

  # Test that there are 0 mismatched classes
  if(verbose == FALSE) {

    eval(bquote(testthat::expect_equal(nrow(mismatched_classes), 0)))

  } else {

    mismatched_classes

  }

}

#' Test ID format
#'
#' Test that ID formats for each column are expected. There are 4 ID columns. "individualID" in the Capture and
#' Individual data, and "femaleID" and "maleID" in the Brood data. This function will take the regular expression that matches
#' the expected ID format for the pipeline and will make sure that the ID column does not have entries that do not match the expected format.
#'
#' This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param ID_col Character string. Which ID column should be tested? ("femaleID", "maleID", "C-individualID", "I-individualID").
#' The C and I in the final two options stand for Capture and Individual data, respectively. So, to test individualID in the Capture
#' data, ID_col should be set to "C-individualID".
#' @param ID_format Character string. Regular expression corresponding to the proper ID format.
#'
#' @return Logical (TRUE/FALSE) for whether all of IDs are formatted as expected.
#' @export
#'
##FIXME: Add measurementID, locationID, treatmentID?
test_ID_format <- function(pipeline_output,
                           ID_col,
                           ID_format) {

  ## Test
  eval(bquote(testthat::expect_true(

    ## femaleID
    if (ID_col == "femaleID") {

      if("femaleID" %in% names(pipeline_output$Brood_data)) {

        all(stringr::str_detect(pipeline_output$Brood_data$femaleID,
                                ID_format), na.rm = TRUE)

      } else {

        all(stringr::str_detect(pipeline_output$Brood_data$FemaleID,
                                ID_format), na.rm = TRUE)

      }

    }

    ## maleID
    else if (ID_col == "maleID") {

      if("maleID" %in% names(pipeline_output$Brood_data)) {

        all(stringr::str_detect(pipeline_output$Brood_data$maleID,
                                ID_format), na.rm = TRUE)

      } else {

        all(stringr::str_detect(pipeline_output$Brood_data$MaleID,
                                ID_format), na.rm = TRUE)

      }

    }

    ## individualID in Capture data
    else if (ID_col == "C-individualID") {

      if("individualID" %in% names(pipeline_output$Capture_data)) {

        all(stringr::str_detect(pipeline_output$Capture_data$individualID,
                                ID_format), na.rm = TRUE)

      } else {

        all(stringr::str_detect(pipeline_output$Capture_data$IndvID,
                                ID_format), na.rm = TRUE)

      }

    }

    ## individualID in Individual data
    else if (ID_col == "I-individualID") {

      if("individualID" %in% names(pipeline_output$Individual_data)) {

        all(stringr::str_detect(pipeline_output$Individual_data$individualID,
                                ID_format), na.rm = TRUE)

      } else {

        all(stringr::str_detect(pipeline_output$Individual_data$IndvID,
                                ID_format), na.rm = TRUE)

      }

    }

  )))

}

#' Test unique values in key columns
#'
#' Some columns are not allowed to have duplicated values. Namely, broodID (Brood data), captureID (Capture data), individualID (Individual data), measurementID (Measurement data), locationID (Location data; NB: only in >v1.2), treatmentID (Experiment data)
#'
#' This function takes a column name and returns a logical value corresponding to whether all values in the column are unique (TRUE) or not (FALSE).
#'
#' This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param column Character string. Which column should be checked for duplicates? One of: "broodID", "captureID", "individualID", "measurementID", "locationID", "treatmentID". Note that for "individualID" we use <siteID>_<individualID to check for duplicates within sites.
#'
#' @return Logical (TRUE/FALSE) for whether all values in the column are unique
#' @export
#'
test_unique_values <- function(pipeline_output,
                               column) {

  eval(bquote(testthat::expect_true(

    ## broodID
    if (column == "broodID") {

      if("broodID" %in% names(pipeline_output$Brood_data)) {

        !any(duplicated(pipeline_output$Brood_data$broodID))

      } else {

        !any(duplicated(pipeline_output$Brood_data$BroodID))

      }

    }

    ## captureID
    else if (column == "captureID") {

      if("captureID" %in% names(pipeline_output$Capture_data)) {

        !any(duplicated(pipeline_output$Capture_data$captureID))

      } else {

        !any(duplicated(pipeline_output$Capture_data$CaptureID))

      }

    }

    ## individualID
    else if (column == "individualID") {

      if("individualID" %in% names(pipeline_output$Individual_data)) {

        !any(duplicated(pipeline_output$Individual_data$individualID))

      } else {

        !any(duplicated(paste(pipeline_output[[3]]$PopID, pipeline_output[[3]]$IndvID, sep = "-")))

      }

    }

    ## measurementID
    else if (column == "measurementID") {

      !any(duplicated(pipeline_output$Measurement_data$measurementID))

    }

    ## locationID
    else if (column == "locationID") {

      !any(duplicated(pipeline_output$Location_data$locationID))

    }

    ## treatmentID
    else if (column == "treatmentID") {

      !any(duplicated(pipeline_output$Experiment_data$treatmentID))

    }

  )))

}

#' Test for NAs in key columns
#'
#'Some key columns are not allowed to have NAs. This function takes a table name and returns a logical value corresponding to whether there are any NAs in the key columns in each data table. In the case of NAs, the test fails.
#'
#'This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param table Which data table should be checked for NAs in the key columns? One of: "Brood", "Capture", "Individual", "Measurement, "Location", or "Experiment".
#' @param verbose Logical (TRUE/FALSE) for whether the function returns the test output (FALSE) or a data frame of column names that failed this test.
#'
#' @return See `verbose`.
#' @export
#'
test_NA_columns <- function(pipeline_output,
                            table,
                            verbose = FALSE){

  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # List of key variables in the selected data table that should not have missing values
  key_vars <- key_variables[[paste0(table, "_data")]]

  # Test for NAs
  if(verbose == FALSE) {

    eval(bquote(
      testthat::expect_equal(

        data_table %>%
          dplyr::select(tidyselect::any_of(key_vars)) %>% # Select key columns
          dplyr::select(tidyselect::where(~ any(is.na(.)))) %>% # Select any key column that has NAs
          ncol(),
        0 # If number of columns is larger than 0, test fails

      )

    ))

  } else {

    data_table %>%
      dplyr::select(tidyselect::any_of(key_vars)) %>% # Select key columns
      dplyr::select(tidyselect::where(~ any(is.na(.))))

  }

}

#' Test category columns
#'
#' Some columns should only include particular categorical values.
#' For example, 'observedClutchType' should only include any of "first", "second", "replacement" or NA.
#' This test makes sure that there are no unexpected values in these different category columns.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X, where X is the three- or four-letter study site code).
#' @param table Which table should be checked? One of: "Brood", "Capture", "Individual", "Location", "Measurement", or "Experiment".
#' @param verbose Logical (TRUE/FALSE) for whether the function returns the test output (FALSE) or a data frame of column names that failed this test.
#'
#' @return See `verbose`.
#' @export
#'
test_category_columns <- function(pipeline_output,
                                  table,
                                  verbose = FALSE){

  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # List of categorical variables and their possible categories present in the selected data table
  categories <- categorical_variables[[paste0(table, "_data")]]

  # Remove absent categorical variables from categories
  # NB: Categorical variable may be absent because categorical_variables list has all categorical variables from all versions of the standard format
  categories <- categories[names(categories) %in% names(data_table)]

  # Check for each variable whether unique values are possible categories
  lgl <- purrr::imap_lgl(.x = categories,
                         .f = ~{

                           # Get unique variable values
                           column_vals <- data_table %>%
                             dplyr::select({{.y}}) %>%
                             dplyr::pull() %>%
                             unique %>%
                             strsplit(split = "; ") %>% # Split if multiple categories are concatenated
                             unlist() %>%
                             as.character()

                           all(column_vals %in% .x)

                         })

  # Test
  if(verbose == FALSE) {

    eval(bquote(testthat::expect_true(

      all(lgl)

    )))

  } else {

    names(lgl)[lgl == FALSE]

  }

}
