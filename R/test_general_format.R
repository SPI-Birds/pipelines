#' Test column presence
#'
#' Test whether expected columns are present
#'
#' This function is intended to be used in the test for every pipeline in order
#' to ensure consistency between pipelines.
#'
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param data_template Character string. Which data template should be tested? One of:
#' "Brood", "Individual", "Capture", or "Location"
#'
#' @return Logical (TRUE/FALSE) for whether all of the column classes are expected.
#' @export
#'
test_col_present <- function(pipeline_output,
                             data_template) {

  # Select data table
  data_table <- pipeline_output[[paste0(data_template, "_data")]]

  # Select template
  template <- data_templates$v1.1[[paste0(data_template, "_data")]]

  # Test
  eval(bquote(testthat::expect_true(

    setequal(names(template), names(data_table))

  )))

}

#' Test column classes
#'
#' Test whether column classes for pipeline outputs match the expected column classes as specified in the standard format (version 1.1.0).
#'
#' This function is intended to be used
#' in the test for every pipeline in order to ensure consistency between pipelines.
#'
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param data_template Character string. Which data template should be tested? One of:
#' "Brood", "Individual", "Capture", or "Location"
#'
#' @return Logical (TRUE/FALSE) for whether all of the column classes are expected.
#' @export
#'
test_col_classes <- function(pipeline_output,
                             data_template) {

  # Select data table
  data_table <- pipeline_output[[paste0(data_template, "_data")]]

  # Select template
  template <- data_templates$v1.1[[paste0(data_template, "_data")]]

  # Match columns in data table and template
  selected_cols_data <- data_table %>%
    dplyr::select(tidyselect::any_of(names(template)))

  selected_cols_template <- template %>%
    dplyr::select(tidyselect::any_of(names(data_table)))

  # Test
  eval(bquote(testthat::expect_true(

    all(purrr::map_df(selected_cols_template, class) == purrr::map_df(selected_cols_data, class))

  )))

}

#' Test ID format
#'
#' Test that ID formats for each column are expected. There are 4 ID columns. "IndvID" in the Capture and
#' Individual data, and "FemaleID" and "MaleID" in the Brood data. This function will take the regular expression that matches
#' the expected ID format for the pipeline and will make sure that the ID column does not have entries that do not match the expected format.
#'
#' This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param ID_col Character string. Which ID column should be tested? ("FemaleID", "MaleID", "C-IndvID", "I-IndvID").
#' The C and I in the final two options stand for Capture and Individual data, respectively. So, to test IndvID in the Capture
#' data, ID_col should be set to "C-IndvID".
#' @param ID_format Character string. Regular expression corresponding to the proper ID format.
#'
#' @return Logical (TRUE/FALSE) for whether all of IDs are formatted as expected.
#' @export
#'
test_ID_format <- function(pipeline_output,
                           ID_col,
                           ID_format) {

  ## Test
  eval(bquote(testthat::expect_true(

    ## FemaleID
    if (ID_col == "FemaleID") {

      all(stringr::str_detect(pipeline_output[[1]]$FemaleID,
                              ID_format), na.rm = T)

    }

    ## MaleID
    else if (ID_col == "MaleID") {

      all(stringr::str_detect(pipeline_output[[1]]$MaleID,
                              ID_format), na.rm = T)

    }

    ## IndvID in Capture data
    else if (ID_col == "C-IndvID") {

      all(stringr::str_detect(pipeline_output[[2]]$IndvID,
                              ID_format), na.rm = T)

    }

    ## IndvID in Individual data
    else if (ID_col == "I-IndvID") {

      all(stringr::str_detect(pipeline_output[[3]]$IndvID,
                              ID_format), na.rm = T)

    }

  )))

}

#' Test unique values in key columns
#'
#' Some columns are not allowed to have duplicated values. Namely, BroodID (Brood data), CaptureID (Capture data),
#' and the concatenation of PopID and IndvID (Individual data).
#' This function takes a column name and returns a logical value corresponding to whether all values in the column are unique (TRUE) or not (FALSE).
#'
#' This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param column Character string. Which column should be checked for duplicates? One of: "BroodID", "CaptureID", or "PopID-IndvID"
#'
#' @return Logical (TRUE/FALSE) for whether all values in the column are unique
#' @export
#'
test_unique_values <- function(pipeline_output,
                               column) {

  eval(bquote(testthat::expect_true(

    ## BroodID
    if (column == "BroodID") {

      !any(duplicated(pipeline_output[[1]]$BroodID))

    }

    ## CaptureID
    else if (column == "CaptureID") {

      !any(duplicated(pipeline_output[[2]]$CaptureID))

    }

    ## IndvID
    else if (column == "PopID-IndvID") {

      !any(duplicated(paste(pipeline_output[[3]]$PopID, pipeline_output[[3]]$IndvID, sep = "-")))

    }

  )))

}

#' Test for NAs in key columns
#'
#'Some key columns are not allowed to have NAs. The data templates for each table have dummy values entered when the column cannot contain any NAs.
#'This function takes a table name and returns a logical value corresponding to whether there are any NAs in the key columns in each data table.
#'In the case of NAs, the test fails.
#'
#'This function is intended to be used in the test for every pipeline in order to ensure consistency between pipelines.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param table Which data table should be checked for NAs in the key columns? One of: "Brood", "Capture", "Individual", or "Location"
#'
#' @return Logical (TRUE/FALSE) for whether there are any NAs in the key columns for the specified table.
#' @export
#'
test_NA_columns <- function(pipeline_output,
                            table){

  # Select data table
  data_table <- pipeline_output[[paste0(table, "_data")]]

  # List of key variables in the selected data table that should not have missing values
  key_vars <- key_variables[[paste0(table, "_data")]]

  # Remove absent key variables from categories
  # NB: Key variable may be absent because key_variables list has all key variables from all versions of the standard format
  key_vars <- key_vars[key_vars %in% names(data_table)]

  # Test for NAs
  eval(bquote(
    expect_equal(

      data_table %>%
        dplyr::select(tidyselect::all_of(key_vars)) %>% # Select key columns
        dplyr::select(where(~ any(is.na(.)))) %>% # Select any key column that has NAs
        ncol(),
      0 # If number of columns is larger than 0, test fails
    )
  ))

}

#' Test category columns
#'
#' Some columns should only include particular categorical values.
#' For example, 'ExperimentID' should only include some combination of "PHENOLOGY","COHORT", "PARENTAGE", "SURVIVAL", or "OTHER".
#' This test makes sure that there are no unexpected values in these different category columns.
#'
#' @param pipeline_output A list of the 4 data frames returned from format_X
#' (where X is the three letter population code).
#' @param table Which table should be checked? One of: "Brood", "Capture", "Individual", or "Location"
#'
#' @return Logical (TRUE/FALSE) for whether there are non-standard categories in the data frame.
#' @export
#'
test_category_columns <- function(pipeline_output,
                                  table){

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
                             as.character

                           all(column_vals %in% .x)

                         })

  # Test
  eval(bquote(testthat::expect_true(

    all(lgl)

  )))

}
