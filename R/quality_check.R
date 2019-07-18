#' A function to perform a quality check on pipeline outputs
#'
#' @param db Location of standard .csv outputs of the pipeline.
#' @param report Produce report (pdf) for user.
#'
#' @return A text file of line-by-line warnings,
#'   a text file of line-by-line errors,
#'   a summary dataframe of test warnings and errors for use in testthat,
#'   and a user-friendly report of the line-by-line list of warnings and errors.
#'
#' @export

quality_check <- function(db = choose.dir(),
                          report = NULL) {

  start_time <- Sys.time()

  message("Importing data...")

  ## Each pipeline produces 4 .csv outputs in standard format

  ## DECISION TO BE MADE:
  ## -- Read in .csv files (names are population-specific)
  ## -- or Load in R objects with generic names?

  Individual_data <- read.csv(paste0(db, "/Indv_data_VEL.csv"))
  Brood_data <- read.csv(paste0(db, "/Brood_data_VEL.csv"))
  Capture_data <- read.csv(paste0(db, "/Capture_data_VEL.csv"))
  Location_data <- read.csv(paste0(db, "/Location_data_VEL.csv"))

  ## Create check list with - a summary of warnings and errors per test.
  check_list <- tibble::tibble(Check = c("Individual data format", "Brood data format",
                                         "Capture data format", "Location data format"),
                               Warning = NA,
                               Error = NA)

  ## Create line-by-line warning and error dataframe
  ## -- HOW TO STRUCTURE THIS?

  ## Check formats
  ## -- Individual data
  message("Checking format of individual data...")

  check_format_individual_output <- check_format_individual(Individual_data, check_list)

  check_list <- check_format_individual_output$check_list

  ## -- Brood data
  message("Checking format of brood data...")

  check_format_brood_output <- check_format_brood(Brood_data, check_list)

  check_list <- check_format_brood_output$check_list

  ## -- Capture data
  message("Checking format of capture data...")

  check_format_capture_output <- check_format_capture(Capture_data, check_list)

  check_list <- check_format_capture_output$check_list

  ## -- Location data
  message("Checking format of location data...")

  check_format_location_output <- check_format_location(Location_data, check_list)

  check_list <- check_format_location_output$check_list


  ## Create text file of warnings
  sink('warnings.txt')

  cat("Warnings\n\n") ## MAKE POP-SPECIFIC

  cat("Check 1: Individual data format\n\n")
  cat(unlist(check_format_individual_output$warning_output), sep="\n", "\n")

  cat("Check 2: Brood data format\n\n")
  cat(unlist(check_format_brood_output$warning_output), sep="\n", "\n")

  cat("Check 3: Capture data format\n\n")
  cat(unlist(check_format_capture_output$warning_output), sep="\n", "\n")

  cat("Check 4: Location data format\n\n")
  cat(unlist(check_format_location_output$warning_output), sep="\n", "\n")

  sink()

  ## Create text file of errors
  sink('errors.txt')

  cat("Errors\n\n") ## MAKE POP-SPECIFIC

  cat("Check 1: Individual data format\n\n")
  cat(unlist(check_format_individual_output$error_output), sep="\n", "\n")

  cat("Check 2: Brood data format\n\n")
  cat(unlist(check_format_brood_output$error_output), sep="\n", "\n")

  cat("Check 3: Capture data format\n\n")
  cat(unlist(check_format_capture_output$error_output), sep="\n", "\n")

  cat("Check 4: Location data format\n\n")
  cat(unlist(check_format_location_output$error_output), sep="\n", "\n")

  sink()


  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("\nAll checks performed in ", round(time, 2), " seconds"))


  checks_warnings <- sum(check_list$Warning == TRUE, na.rm=TRUE)

  checks_errors <- sum(check_list$Error == TRUE, na.rm=TRUE)

  cat(yellow(paste0("\n", checks_warnings, " out of ", nrow(check_list), " tests have warnings.")))

  cat(red(paste0("\n", checks_errors, " out of ", nrow(check_list), " tests have errors.")))

}


#' Check format of individual data
#'
#' Check if the formats of each column in the individual data match with the standard format
#' @param Individual_data Data frame. Individual data output from pipeline.
#' @param check_list Data frame. A summary list of test warnings and errors.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_individual <- function(Individual_data, check_list){

  ## Data frame with column names and formats according to the standard protocol
  Individual_data_standard <- tibble::tibble(Variable = c("IndvID", "Species", "PopID",
                                                          "BroodIDLaid", "BroodIDRinged", "RingSeason",
                                                          "RingAge", "Sex"),
                                             Format_standard = c("character", "character", "character",
                                                                 "character", "character", "numeric",
                                                                 "numeric", "character"))

  ## Data frame with column names and formats from Individual data
  Individual_data_col <- tibble::tibble(Variable = names(Individual_data),
                                        Format = unlist(purrr::pmap(list(Individual_data), class)))

  ## Mismatches between Individual data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Individual_data_mismatch <- dplyr::left_join(Individual_data_standard, Individual_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Individual_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Individual_data_mismatch$Variable,
                                .y = Individual_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Individual_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Individual_data_missing <- dplyr::left_join(Individual_data_standard, Individual_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Individual_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Individual_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Individual_data is missing, unmeasured or undetermined.")
                                 })
  }

  check_list[1, "Warning"] <- war
  check_list[1, "Error"] <- err

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))
}


#' Check format of brood data
#'
#' Check if the formats of each column in the brood data match with the standard format
#' @param Brood_data Data frame. Brood data output from pipeline.
#' @param check_list Data frame. A summary list of test warnings and errors.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_brood <- function(Brood_data, check_list){

  ## Data frame with column names and formats according to the standard protocol
  Brood_data_standard <- tibble::tibble(Variable = c("PopID", "BreedingSeason", "Species", "Plot",
                                                     "LocationID", "BroodID", "FemaleID", "MaleID",
                                                     "ClutchType_observed", "ClutchType_calc",
                                                     "LayingDate", "LayingDateError", "ClutchSize",
                                                     "ClutchSizeError", "HatchDate", "HatchDateError",
                                                     "BroodSize", "BroodSizeError", "FledgeDate",
                                                     "FledgeDateError", "NumberFledged",
                                                     "NumberFledgedError", "AvgEggMass", "NumberEggs",
                                                     "AvgChickMass", "NumberChicksMass", "AvgTarsus",
                                                     "NumberChicksTarsus", "ExperimentID"),
                                        Format_standard = c("character", "numeric", "character", "character",
                                                            "character", "character", "character", "character",
                                                            "character", "character",
                                                            "Date", "numeric", "numeric",
                                                            "numeric", "Date", "numeric",
                                                            "numeric", "numeric", "Date",
                                                            "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric",
                                                            "numeric", "character"))

  ## Data frame with column names and formats from Brood data
  Brood_data_col <- tibble::tibble(Variable = names(Brood_data),
                                   Format = unlist(purrr::pmap(list(Brood_data), class)))

  ## Mismatches between Brood_data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Brood_data_mismatch <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Brood_data_mismatch$Variable,
                                .y = Brood_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Brood_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Brood_data_missing <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Brood_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Brood_data is missing, unmeasured or undetermined.")
                                 })
  }

  check_list[2, "Warning"] <- war
  check_list[2, "Error"] <- err

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))
}


#' Check format of capture data
#'
#' Check if the formats of each column in the capture data match with the standard format
#' @param Capture_data Data frame. Capture data output from pipeline.
#' @param check_list Data frame. A summary list of test warnings and errors.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_capture <- function(Capture_data, check_list){

  ## Data frame with column names and formats according to the standard protocol
  Capture_data_standard <- tibble::tibble(Variable = c("IndvID", "Species", "BreedingSeason",
                                                       "LocationID", "CaptureDate", "CaptureTime",
                                                       "CapturePopID", "CapturePlot", "ReleasePopID",
                                                       "ReleasePlot", "Mass", "Tarsus", "WingLength",
                                                       "Age_obsv", "Age_calc", "ChickAge"),
                                          Format_standard = c("character", "character", "numeric",
                                                              "character", "Date", "Date",
                                                              "character", "character", "character",
                                                              "character", "numeric", "numeric", "numeric",
                                                              "numeric", "numeric", "numeric"))

  ## Data frame with column names and formats from Capture data
  Capture_data_col <- tibble::tibble(Variable = names(Capture_data),
                                     Format = unlist(purrr::pmap(list(Capture_data), class)))

  ## Mismatches between Capture data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Capture_data_mismatch <- dplyr::left_join(Capture_data_standard, Capture_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Capture_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Capture_data_mismatch$Variable,
                                .y = Capture_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Capture_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Capture_data_missing <- dplyr::left_join(Capture_data_standard, Capture_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Capture_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Capture_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Capture_data is missing, unmeasured or undetermined.")
                                 })
  }

  check_list[3, "Warning"] <- war
  check_list[3, "Error"] <- err

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))
}

#' Check format of location data
#'
#' Check if the formats of each column in the location data match with the standard format
#' @param Location_data Data frame. Location data output from pipeline.
#' @param check_list Data frame. A summary list of test warnings and errors.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_location <- function(Location_data, check_list){

  ## Data frame with column names and formats according to the standard protocol
  Location_data_standard <- tibble::tibble(Variable = c("LocationID", "NestboxID", "LocationType",
                                                        "PopID", "Latitude", "Longitude",
                                                        "StartSeason", "EndSeason", "Habitat"),
                                           Format_standard = c("character", "character", "character",
                                                               "character", "numeric", "numeric",
                                                               "numeric", "numeric", "character"))

  ## Data frame with column names and formats from Location data
  Location_data_col <- tibble::tibble(Variable = names(Location_data),
                                      Format = unlist(purrr::pmap(list(Location_data), class)))

  ## Mismatches between Location data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Location_data_mismatch <- dplyr::left_join(Location_data_standard, Location_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Location_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Location_data_mismatch$Variable,
                                .y = Location_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Location_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Location_data_missing <- dplyr::left_join(Location_data_standard, Location_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Location_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Location_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Location_data is missing, unmeasured or undetermined.")
                                 })
  }

  check_list[3, "Warning"] <- war
  check_list[3, "Error"] <- err

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))
}

