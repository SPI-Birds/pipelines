# This file is a wrapper for the functions in test_general_format.R
# It allows us to run all tests for a given pipeline in one go

#' Test protocol compliance
#'
#' Runs all protocol validation tests from test_general_format.R
#'
#' @param pipeline_output A list of data frames returned from format_X,
#' where X is the pipeline code.
#' @param protocol_version The protocol version of the SPI-Birds standard
#' data being used. Either "1.0.0", "1.1.0", or "2.0.0".
#' @param ID_format Regular expression pattern for validating ID formats.
#' Default is FALSE, which skips this test.
#'
#' @return Invisible TRUE if all tests pass, errors otherwise.
#' @export

test_protocol_compliance <- function(pipeline_output,
                                     ID_format = FALSE) {
    # Retrieve protocol version from the pipeline output
    protocol_version <- pipeline_output$protocol_version

    # Select tables to test based on protocol version
    if (protocol_version %in% c("1.0.0", "1.1.0")) {
        tables <- c(
            "Brood",
            "Capture",
            "Individual",
            "Location"
        )
    } else if (protocol_version == "2.0.0") {
        tables <- c(
            "Brood",
            "Capture",
            "Individual",
            "Location",
            "Measurement",
            "Experiment"
        )
    }

    testthat::test_that("Expected columns are present", {
        for (table in tables) {
            test_col_present(pipeline_output, table, protocol_version)
        }
    })

    testthat::test_that("Column classes are as expected", {
        for (table in tables) {
            test_col_classes(pipeline_output, table, protocol_version)
        }
    })

    testthat::test_that("Key columns in each table do not have NAs", {
        for (table in tables) {
            test_NA_columns(pipeline_output, table)
        }
    })

    testthat::test_that("Categorical columns do not have unexpected values", {
        for (table in tables) {
            test_category_columns(pipeline_output, table)
        }
    })

    testthat::test_that("Key columns only contain unique values", {
        if (protocol_version %in% c("1.0.0", "1.1.0")) {
            test_unique_values(pipeline_output, "BroodID")
            test_unique_values(pipeline_output, "CaptureID")
            test_unique_values(pipeline_output, "IndvID")
        } else if (protocol_version == "2.0.0") {
            test_unique_values(pipeline_output, "broodID")
            test_unique_values(pipeline_output, "captureID")
            test_unique_values(pipeline_output, "individualID")
            test_unique_values(pipeline_output, "locationID")
        }
    })

    if (!identical(ID_format, FALSE)) {
        testthat::test_that("ID columns match the expected format for the pipeline", {
            if (protocol_version %in% c("1.0.0", "1.1.0")) {
                test_ID_format(pipeline_output, "FemaleID",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "MaleID",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "IndvID",
                    table = "Individual",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "IndvID",
                    table = "Capture",
                    format = ID_format
                )
            } else if (protocol_version == "2.0.0") {
                test_ID_format(pipeline_output, "femaleID",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "maleID",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "individualID",
                    table = "Individual",
                    format = ID_format
                )
                test_ID_format(pipeline_output, "individualID",
                    table = "Capture",
                    format = ID_format
                )
            }
        })
    }

    testthat::test_that("Date columns are valid", {
        test_date_columns(pipeline_output, protocol_version)
    })

    testthat::test_that("Time columns are valid", {
        test_time_columns(pipeline_output, protocol_version)
    })

    testthat::test_that("Count columns are within expected ranges", {
        test_min_max_columns(pipeline_output, protocol_version)
    })

    message("All protocol compliance tests passed")
    invisible(TRUE)
}
