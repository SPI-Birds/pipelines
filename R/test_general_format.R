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

  ## Test
  eval(bquote(testthat::expect_true(

    if (data_template == "Brood"){

      setequal(names(brood_data_template), names(pipeline_output[[1]]))

    }

    else if (data_template == "Capture") {

      setequal(names(capture_data_template), names(pipeline_output[[2]]))

    }

    else if (data_template == "Individual") {

      setequal(names(individual_data_template), names(pipeline_output[[3]]))

    }

    else if (data_template == "Location") {

      setequal(names(location_data_template), names(pipeline_output[[4]]))

    }

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

  ## Test
  eval(bquote(testthat::expect_true(

    if (data_template == "Brood"){

      brood <- pipeline_output[[1]] %>%
        dplyr::select(tidyselect::any_of(names(brood_data_template)))

      brood_data_template_ab <- brood_data_template %>%
        dplyr::select(tidyselect::any_of(names(pipeline_output[[1]])))

      all(purrr::map_df(brood_data_template_ab, class) == purrr::map_df(brood, class))

    }


    else if (data_template == "Capture") {

      capture <- pipeline_output[[2]] %>%
        dplyr::select(tidyselect::any_of(names(capture_data_template)))

      capture_data_template_ab <- capture_data_template %>%
        dplyr::select(tidyselect::any_of(names(pipeline_output[[2]])))

      all(purrr::map_df(capture_data_template_ab, class) == purrr::map_df(capture, class))

    }

    else if (data_template == "Individual") {

      individual <- pipeline_output[[3]] %>%
        dplyr::select(tidyselect::any_of(names(individual_data_template)))

      individual_data_template_ab <- individual_data_template %>%
        dplyr::select(tidyselect::any_of(names(pipeline_output[[3]])))

      all(purrr::map_df(individual_data_template_ab, class) == purrr::map_df(individual, class))

    }

    else if (data_template == "Location") {

      location <- pipeline_output[[4]] %>%
        dplyr::select(tidyselect::any_of(names(location_data_template)))

      location_data_template_ab <- location_data_template %>%
        dplyr::select(tidyselect::any_of(names(pipeline_output[[4]])))

      all(purrr::map_df(location_data_template_ab, class) == purrr::map_df(location, class))
    }

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
                              glue::glue({ID_format})))

    }

    ## MaleID
    else if (ID_col == "MaleID") {

      all(stringr::str_detect(pipeline_output[[1]]$MaleID,
                              glue::glue({ID_format})))

    }

    ## IndvID in Capture data
    else if (ID_col == "C-IndvID") {

      all(stringr::str_detect(pipeline_output[[2]]$IndvID,
                              glue::glue({ID_format})))

    }

    ## IndvID in Individual data
    else if (ID_col == "I-IndvID") {

      all(stringr::str_detect(pipeline_output[[3]]$IndvID,
                              glue::glue({ID_format})))

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

  ## Test for NAs
  eval(bquote(expect_false(

    if (table == "Brood") {

      ## Check for NAs in key columns
      any(
        is.na(pipeline_output[[1]] %>%
                dplyr::select(names(brood_data_template %>%
                                      dplyr::select_if(~!any(is.na(.))))))
      )

    }

    else if (table == "Capture") {

      ## Check for NAs in key columns
      any(
        is.na(pipeline_output[[2]] %>%
                dplyr::select(names(capture_data_template %>%
                                      dplyr::select_if(~!any(is.na(.))))))
      )
    }

    else if (table == "Individual") {

      ## Check for NAs in key columns
      any(
        is.na(pipeline_output[[3]] %>%
                dplyr::select(names(individual_data_template %>%
                                      dplyr::select_if(~!any(is.na(.))))))
      )

    }


    else if (table == "Location") {

      ## Check for NAs in key columns
      any(
        is.na(pipeline_output[[4]] %>%
                dplyr::select(names(location_data_template %>%
                                      dplyr::select_if(~!any(is.na(.))))))
      )

    }

  )))
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

  if (table == "Brood") {

    category_columns <- brood_data_template %>%
      dplyr::select_if(~dplyr::n_distinct(.) > 1) %>%
      dplyr::select(-BroodID)

  }

  else if (table == "Capture") {

    category_columns <- capture_data_template %>%
      dplyr::select_if(~dplyr::n_distinct(.) > 1) %>%
      dplyr::select(-CaptureID)

  }

  else if (table == "Individual") {

    category_columns <- individual_data_template %>%
      dplyr::select_if(~dplyr::n_distinct(.) > 1) %>%
      dplyr::select(-IndvID)

  }

  else if (table == "Location") {

    category_columns <- location_data_template %>%
      dplyr::select_if(~dplyr::n_distinct(.) > 1)

  }

  lgl <- logical(length = 0)
  for (i in names(category_columns)) {

    ## Get column values
    column_vals <- pipeline_output[[paste0(table, "_data")]] %>%
      dplyr::select(tidyselect::all_of(i)) %>%
      unique %>%
      dplyr::pull(i) %>%
      strsplit(split = "; ") %>%
      unlist() %>%
      as.character

    ## Get categories
    cats <- dplyr::pull(category_columns, var = i)

    ## Add
    lgl <- c(lgl, all(column_vals %in% cats))

  }

  ## Test
  eval(bquote(testthat::expect_true(

    all(lgl)

  )))

}
