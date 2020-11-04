#' Perform quality checks on brood data
#'
#' A wrapper that runs all single checks related to \code{Brood_data}.
#'
#' The following brood data checks are performed:
#' \itemize{
#' \item \strong{B1}: Check if the formats of each column in \code{Brood_data} match with the standard format using \code{\link{check_format_brood}}.
#' \item \strong{B2}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \item \strong{B3}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#' \item \strong{B4}: Compare laying date and hatching date per brood using \code{\link{compare_laying_hatching}}.
#' \item \strong{B5}: Compare hatching date and fledging date per brood using \code{\link{compare_hatching_fledging}}.
#' \item \strong{B6a-c}: Check brood variable values against reference values using \code{\link{check_values_brood}}. Brood variables checked: ClutchSize, BroodSize, NumberFledged.
#' \item \strong{B7}: Check if parents of a brood are the same species using \code{\link{check_parent_species}}.
#' \item \strong{B8}: Compare brood size with number of chicks captured using \code{\link{compare_broodsize_chicknumber}}.
#' \item \strong{B9}: Check if the IDs of broods are unique using \code{\link{check_unique_BroodID}}.
#' }
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_brood}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @inherit checks_return return
#'
#' @export

brood_check <- function(Brood_data, Individual_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("B", c(1:5, paste0(6, letters[1:3]), 7:9)),
                               CheckDescription = c("Check format of brood data",
                                                    "Compare clutch and brood sizes",
                                                    "Compare brood sizes and fledgling numbers",
                                                    "Compare laying and hatching dates",
                                                    "Compare hatching and fledging dates",
                                                    "Check clutch size values against reference values",
                                                    "Check brood size values against reference values",
                                                    "Check fledgling number values against reference values",
                                                    "Check that parents are the same species",
                                                    "Compare brood size with number of chicks captured",
                                                    "Check that brood IDs are unique"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Brood checks")

  # - Check format brood data
  if(check_format) {
    message("B1: Checking format of brood data...")

    check_format_brood_output <- check_format_brood(Brood_data)

    check_list[1,3:4] <- check_format_brood_output$CheckList

  }

  # - Compare clutch and brood sizes
  message("B2: Comparing clutch and brood sizes...")

  compare_clutch_brood_output <- compare_clutch_brood(Brood_data)

  check_list[2,3:4] <- compare_clutch_brood_output$CheckList

  # - Compare brood sizes and fledgling numbers
  message("B3: Comparing brood sizes and fledgling numbers...")

  compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data)

  check_list[3,3:4] <- compare_brood_fledglings_output$CheckList

  # - Compare laying and hatching dates
  message("B4: Comparing laying and hatching dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data)

  check_list[4,3:4] <- compare_laying_hatching_output$CheckList

  # - Compare hatching and fledging dates
  message("B5: Comparing hatching and fledging dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)

  check_list[5,3:4] <- compare_hatching_fledging_output$CheckList

  # - Check clutch size values against reference values
  message("B6a: Checking clutch size values against reference values...")

  check_values_clutch_size_output <- check_values_brood(Brood_data, "ClutchSize")

  check_list[6,3:4] <- check_values_clutch_size_output$CheckList

  # - Check brood size values against reference values
  message("B6b: Checking brood size values against reference values...")

  check_values_brood_size_output <- check_values_brood(Brood_data, "BroodSize")

  check_list[7,3:4] <- check_values_brood_size_output$CheckList

  # - Check fledgling number values against reference values
  message("B6c: Checking fledgling number values against reference values...")

  check_values_fledgling_number_output <- check_values_brood(Brood_data, "NumberFledged")

  check_list[8,3:4] <- check_values_fledgling_number_output$CheckList

  # - Check parents of broods are the same species
  message("B7: Checking that parents are the same species...")

  check_parent_species_output <- check_parent_species(Brood_data, Individual_data)

  check_list[9,3:4] <- check_parent_species_output$CheckList

  # - Compare brood size and number of chicks captured
  message("B8: Comparing brood size and number of chicks captured...")

  compare_broodsize_chicknumber_output <- compare_broodsize_chicknumber(Brood_data, Individual_data)

  check_list[10,3:4] <- compare_broodsize_chicknumber_output$CheckList

  # - Check that BroodIDs are unique
  message("B9: Checking that brood IDs are unique...")

  check_unique_BroodID_output <- check_unique_BroodID(Brood_data)

  check_list[11,3:4] <- check_unique_BroodID_output$CheckList


  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_brood_output$WarningOutput,
                         Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6a = check_values_clutch_size_output$WarningOutput,
                         Check6b = check_values_brood_size_output$WarningOutput,
                         Check6c = check_values_fledgling_number_output$WarningOutput,
                         Check7 = check_parent_species_output$WarningOutput,
                         Check8 = compare_broodsize_chicknumber_output$WarningOutput,
                         Check9 = check_unique_BroodID_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_brood_output$ErrorOutput,
                       Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6a = check_values_clutch_size_output$ErrorOutput,
                       Check6b = check_values_brood_size_output$ErrorOutput,
                       Check6c = check_values_fledgling_number_output$ErrorOutput,
                       Check7 = check_parent_species_output$ErrorOutput,
                       Check8 = compare_broodsize_chicknumber_output$ErrorOutput,
                       Check9 = check_unique_BroodID_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6a = check_values_clutch_size_output$WarningOutput,
                         Check6b = check_values_brood_size_output$WarningOutput,
                         Check6c = check_values_fledgling_number_output$WarningOutput,
                         Check7 = check_parent_species_output$WarningOutput,
                         Check8 = compare_broodsize_chicknumber_output$WarningOutput,
                         Check9 = check_unique_BroodID_output$WarningOutput)

    # Error list
    error_list <- list(Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6a = check_values_clutch_size_output$ErrorOutput,
                       Check6b = check_values_brood_size_output$ErrorOutput,
                       Check6c = check_values_fledgling_number_output$ErrorOutput,
                       Check7 = check_parent_species_output$ErrorOutput,
                       Check8 = compare_broodsize_chicknumber_output$ErrorOutput,
                       Check9 = check_unique_BroodID_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              WarningRows = unique(c(compare_clutch_brood_output$WarningRows,
                                     compare_brood_fledglings_output$WarningRows,
                                     compare_laying_hatching_output$WarningRows,
                                     compare_hatching_fledging_output$WarningRows,
                                     check_values_clutch_size_output$WarningRows,
                                     check_values_brood_size_output$WarningRows,
                                     check_values_fledgling_number_output$WarningRows,
                                     check_parent_species_output$WarningRows,
                                     compare_broodsize_chicknumber_output$WarningRows,
                                     check_unique_BroodID_output$WarningRows)),
              ErrorRows = unique(c(compare_clutch_brood_output$ErrorRows,
                                   compare_brood_fledglings_output$ErrorRows,
                                   compare_laying_hatching_output$ErrorRows,
                                   compare_hatching_fledging_output$ErrorRows,
                                   check_values_clutch_size_output$ErrorRows,
                                   check_values_brood_size_output$ErrorRows,
                                   check_values_fledgling_number_output$ErrorRows,
                                   check_parent_species_output$ErrorRows,
                                   compare_broodsize_chicknumber_output$ErrorRows,
                                   check_unique_BroodID_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Check format of brood data
#'
#' Check that the format of each column in the brood data match with the standard format.
#'
#' Check ID: B1.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

check_format_brood <- function(Brood_data){

  ## Data frame with column names and formats according to the standard protocol
  Brood_data_standard <- tibble::tibble(Variable = c("Row", "BroodID", "PopID", "BreedingSeason", "Species", "Plot",
                                                     "LocationID", "FemaleID", "MaleID",
                                                     "ClutchType_observed", "ClutchType_calculated",
                                                     "LayDate", "LayDateError", "ClutchSize",
                                                     "ClutchSizeError", "HatchDate", "HatchDateError",
                                                     "BroodSize", "BroodSizeError", "FledgeDate",
                                                     "FledgeDateError", "NumberFledged",
                                                     "NumberFledgedError", "AvgEggMass", "NumberEggs",
                                                     "AvgChickMass", "NumberChicksMass", "AvgTarsus",
                                                     "NumberChicksTarsus", "OriginalTarsusMethod", "ExperimentID"),
                                        Format_standard = c("integer", "character", "character", "integer", "character",
                                                            "character", "character", "character", "character",
                                                            "character", "character",
                                                            "Date", "numeric", "integer",
                                                            "numeric", "Date", "numeric",
                                                            "integer", "numeric", "Date",
                                                            "numeric", "integer",
                                                            "numeric", "numeric", "integer",
                                                            "numeric", "integer", "numeric",
                                                            "integer", "character", "character"))

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
  # Brood_data_missing <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
  #   filter(Format == "logical")
  #
  # war <- FALSE
  # warning_output <- NULL
  #
  # if(nrow(Brood_data_missing) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::map(.x = Brood_data_missing$Variable,
  #                                .f = ~{
  #                                  paste0(.x, " in Brood_data is missing, unmeasured or undetermined (NA).")
  #                                })
  # }

  #Test for empty columns by looking at uniques, rather than using data type
  warning_output <- purrr::pmap(.l = list(as.list(Brood_data), colnames(Brood_data)),
                                .f = ~{

                                  if(all(is.na(unique(..1)))){

                                    return(paste0(..2, " in Brood_data is missing, unmeasured or undetermined (NA)."))

                                  } else {

                                    return()

                                  }

                                })



  #Remove all cases that return NULL
  #Assigning NULL (rather than returning NULL in function) removes the list item
  warning_output[sapply(warning_output, is.null)] <- NULL

  if(length(warning_output) > 0){

    war <- TRUE

  } else {

    war <- FALSE

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will result in an error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will result in a warning.
#'
#' Check ID: B2.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_clutch_brood <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & ClutchSize < BroodSize)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(!is.na(ExperimentID) & ClutchSize < BroodSize)

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_non %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = list(error_records$Row,
                                          error_records$BroodID,
                                          error_records$ClutchSize,
                                          error_records$BroodSize),
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has a larger brood size (", ..4,
                                         ") than clutch size (", ..3,
                                         "), but was not experimentally manipulated.")
                                })
  }

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_data_man %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = list(warning_records$Row,
                                            warning_records$BroodID,
                                            warning_records$ClutchSize,
                                            warning_records$BroodSize),
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                           " has a larger brood size (", ..4,
                                           ") than clutch size (", ..3,
                                           "), but was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)


  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  ExperimentID <- ClutchSize <- BroodSize <- NULL
  approved_list <- NULL

}


#' Compare brood sizes and fledgling numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' Check ID: B3.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_brood_fledglings <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & BroodSize < NumberFledged)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(!is.na(ExperimentID) & BroodSize < NumberFledged)

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_non %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = list(error_records$Row,
                                          error_records$BroodID,
                                          error_records$BroodSize,
                                          error_records$NumberFledged),
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has a larger fledgling number (", ..4,
                                         ") than brood size (", ..3,
                                         "), but was not experimentally manipulated.")
                                })
  }

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_data_man %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = list(warning_records$Row,
                                            warning_records$BroodID,
                                            warning_records$BroodSize,
                                            warning_records$NumberFledged),
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                           " has a larger fledgling number (", ..4,
                                           ") than brood size (", ..3,
                                           "), and was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  ExperimentID <- BroodSize <- NumberFledged <- NULL
  approved_list <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' Check ID: B4.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_laying_hatching <- function(Brood_data){

  # Broods with laying date later than hatching date
  Brood_data_late <- Brood_data %>%
    filter(LayDate >= HatchDate)

  # Broods with laying date earlier than hatching date but the difference
  # in number of days is smaller than incubation time
  ## INCUBATION TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(LayDate < HatchDate & (HatchDate-LayDate) >= )

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_late %>%
      dplyr::mutate(CheckID = "B4") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = list(error_records$Row,
                                          error_records$BroodID,
                                          error_records$LayDate,
                                          error_records$HatchDate),
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has a later laying date (", ..3,
                                         ") than hatching date (", ..4, ").")
                                })
  }

  # Warnings
  war <- FALSE
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL


  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  LayDate <- HatchDate <- NULL
  approved_list <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' Check ID: B5.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_hatching_fledging <- function(Brood_data){

  # Broods with laying date later than hatching date
  Brood_data_late <- Brood_data %>%
    filter(HatchDate >= FledgeDate)

  # Broods with hatching date earlier than fledging date but the difference
  # in number of days is smaller than breeding time
  ## BREEDING TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(HatchDate < FledgeDate & (FledgeDate-HatchDate) >= )

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_late %>%
      dplyr::mutate(CheckID = "B5") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = list(error_records$Row,
                                          error_records$BroodID,
                                          error_records$HatchDate,
                                          error_records$FledgeDate),
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has a later hatching date (", ..3,
                                         ") than fledging date (", ..4, ").")
                                })
  }

  # Warnings
  war <- FALSE
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  HatchDate <- FledgeDate <- NULL
  approved_list <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against population-species-specific reference values in brood data. Reference values are based on the data if the number of observations is sufficiently large (n >= 50). Records for population-species combinations that are low in number (n < 50) are not evaluated by this check.
#'
#' \strong{ClutchSize, BroodSize, NumberFledged}: Records are considered unusual if they are larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error. Check IDs: B6a-c
#'
#' \strong{LayDate}: Date columns are transformed to Julian days to calculate quantiles. Records are considered unusual if they are smaller than the 1th percentile or larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are smaller than 1 or larger than 366, and will be flagged as an error. Check IDs: B6d.
#'
#'
#' @inheritParams checks_brood_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_brood <- function(Brood_data, var) {

  # Stop if "var" is missing
  if(missing(var)) {

    stop("Please select a variable in Brood_data to check against reference values.")

  }

  # Create reference values from data
  # Numeric & integer columns
  if(var %in% c("ClutchSize", "BroodSize", "NumberFledged")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var))) %>%
      dplyr::group_by(Species, PopID) %>%
      dplyr::summarise(Warning_min = NA,
                       Warning_max = ceiling(quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE)),
                       Error_min = 0,
                       Error_max = 4 * Warning_max,
                       n = n())

    # Date columns
  } else if(var %in% c("LayDate")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var))) %>%
      dplyr::group_by(BreedingSeason) %>%
      # Transform dates to julian days to calculate quantiles
      dplyr::mutate(!!paste0(var, "_julian") := lubridate::yday(!!rlang::sym(var))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Species, PopID) %>%
      dplyr::summarise(Warning_min = floor(quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.01, na.rm = TRUE)),
                       Warning_max = ceiling(quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.99, na.rm = TRUE)),
                       Error_min = 1,
                       Error_max = 366,
                       n = n())
  }


  # Print message for population-species combinations with too low number of observations
  if(any(ref$n < 50)) {

    low_obs <- ref %>%
      dplyr::filter(n < 50) %>%
      dplyr::select(Species, PopID)

      purrr::pwalk(.l = list(low_obs$Species,
                             low_obs$PopID,
                             rep(var, length(low_obs))),
                   .f = ~{

                     message(paste0("Number of ", ..3, " records for ", ..2, ": ", ..1,
                                    " is too low (< 50) to create reliable reference values.",
                                    " Check will be skipped for this population-species combination."))

                   })

    ### Alternative solution if number of observations is low. ###

    # If number of records is too low, replace reference values by data from Hoge Veluwe (HOG)
    #
    # # Select species-population combination whose reference values will be replaced by NIOO data
    # replaced_species_pops <- ref %>%
    #   dplyr::filter(n < 50) %>%
    #   dplyr::select(Species, PopID)
    #
    # present_values <- NULL
    #
    # # For species present in HOG data, select associated reference values
    # if(any(replaced_species_pops$Species %in% unique(brood_ref_values$Species))) {
    #
    #   species_present <- replaced_species_pops[replaced_species_pops$Species %in% unique(brood_ref_values$Species),]
    #
    #   # Select new reference values
    #   replaced_values <- purrr::pmap_dfr(.l = list(species_present$Species,
    #                                                species_present$PopID,
    #                                                rep(var, nrow(species_present))),
    #                                      .f = ~{
    #
    #                                        # Print message
    #                                        message(paste0("Number of ", ..3, " records for ", ..2, ": ", ..1,
    #                                                       " is too low (< 50) to create reliable reference values.",
    #                                                       " Reference values are based on NIOO data instead."))
    #
    #                                        # Select new reference values
    #                                        brood_ref_values %>%
    #                                          dplyr::filter(Species == ..1 & Variable == var) %>%
    #                                          dplyr::mutate(PopID = ..2) %>%
    #                                          dplyr::select(Species, PopID, Warning_min,
    #                                                        Warning_max, Error_min, Error_max, n)
    #
    #                                      })
    #
    # }
    #
    # # For species not present in HOG data, skip check.
    # if(any(!(replaced_species_pops$Species %in% unique(brood_ref_values$Species)))) {
    #
    #   species_absent <- replaced_species[!(replaced_species_pops$Species %in% unique(brood_ref_values$Species))]
    #
    #   # Print message
    #   purrr::pwalk(.l = list(species_absent$Species,
    #                          species_absent$PopID,
    #                          rep(var, length(species_absent))),
    #                .f = ~{
    #
    #                  message(paste0("Number of ", ..3, " records for ", ..2, ": ", ..1,
    #                                 " is too low (< 50) to create reliable reference values.",
    #                                 " There are no reference values available from Hoge Veluwe data.",
    #                                 " Check will be skipped for this species."))
    #
    #                })
    #
    # }
    #
    # ref <- ref %>%
    #   dplyr::filter(n >= 50) %>%
    #   dplyr::bind_rows(replaced_values) %>%
    #   dplyr::arrange(PopID, Species)

  }

  # Filter references for population-species combinations based on at least 50 observations
  ref <- ref %>%
    dplyr::filter(n >= 50) %>%
    dplyr::arrange(PopID, Species)

  # Progress bar
  pb <- progress::progress_bar$new(total = 2*nrow(ref),
                                   format = "[:bar] :percent ~:eta remaining",
                                   clear = FALSE)

  # Brood-specific errors
  Brood_err <- purrr::pmap(.l = ref,
                           .f = ~{

                             pb$tick()

                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged")) {

                               Brood_data %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 (!!rlang::sym(var) < ..5 | !!rlang::sym(var) > ..6)) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             } else if(var %in% c("LayDate")) {

                               Brood_data %>%
                                 dplyr::group_by(BreedingSeason) %>%
                                 # Transform dates to julian days to compare to julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := lubridate::yday(!!rlang::sym(var))) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 (!!rlang::sym(paste0(var, "_julian")) < ..5 | !!rlang::sym(paste0(var, "_julian")) > ..6)) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             }

                           }) %>%
    dplyr::bind_rows()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_err %>%
      dplyr::mutate(CheckID = checkID_var[checkID_var$Var == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::arrange(Row)

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (BroodID: ", ..3, "; ",
                                         Species_codes[Species_codes$Code == ..5, "CommonName"], ")",
                                         " has an impossible value in ", ..6, " (", ..4, ").")
                                })
  }

  # Brood-specific warnings
  Brood_war <- purrr::pmap(.l = ref,
                           .f = ~{

                             pb$tick()

                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged")) {

                               Brood_data %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 (!!rlang::sym(var) > ..4 & !!rlang::sym(var) <= ..6)) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             } else if(var %in% c("LayDate")) {

                               Brood_data %>%
                                 dplyr::group_by(BreedingSeason) %>%
                                 # Transform dates to julian days to compare to julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := lubridate::yday(!!rlang::sym(var))) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 ((!!rlang::sym(paste0(var, "_julian")) > ..4 & !!rlang::sym(paste0(var, "_julian")) <= ..6) | (!!rlang::sym(paste0(var, "_julian")) < ..3 & !!rlang::sym(paste0(var, "_julian")) >= ..5))) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             }

                           }) %>%
    dplyr::bind_rows()

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_war %>%
      dplyr::mutate(CheckID = checkID_var[checkID_var$Var == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::arrange(Row)

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " (BroodID: ", ..3, "; ",
                                           Species_codes[Species_codes$Code == ..5, "CommonName"], ")",
                                           " has an unusual value in ", ..6, " (", ..4, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  brood_ref_values <- selected_ref_values <- ref_names <- NULL
  Species <- Variable <- NULL
  approved_list <- checkID_var <- NULL

}


#' Check parent species
#'
#' Check that the parents of broods are the same species.
#'
#' Check ID: B7.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_parent_species <- function(Brood_data, Individual_data) {

  # Find species information of mothers
  Females <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, FemaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename(FemaleSpecies = Species)

  # Find species information of fathers
  Males <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, MaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("MaleID" = "IndvID")) %>%
    dplyr::rename(MaleSpecies = Species)

  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Warnings
  # Select records where parents are different species
  Interspecific_broods <- dplyr::left_join(Females, Males, by=c("Row", "PopID", "BroodID")) %>%
    dplyr::filter(FemaleSpecies != MaleSpecies)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Interspecific_broods) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Interspecific_broods %>%
      dplyr::mutate(CheckID = "B7") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..3, ")",
                                         " has parents of different species",
                                         " (Mother: ", Species_codes[Species_codes$Code == ..5, "CommonName"],
                                         ", father: ", Species_codes[Species_codes$Code == ..7, "CommonName"], ").")
                                })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  FemaleSpecies <- MaleSpecies <- NULL
  approved_list <- NULL

}


#' Compare brood size with number of chicks captured
#'
#' Compare BroodSize in Brood_data with the number of chicks captured in Capture_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks captured results in a warning, because chicks might have died before ringing and measuring. Records where BroodSize is smaller than the number of chicks captured results in an error, because this should not be possible.
#'
#' Check ID: B8.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_broodsize_chicknumber <- function(Brood_data, Individual_data) {

  # Link BroodID from Individual_data to each capture in Capture_data
  Chicks_captured <- Individual_data %>%
    dplyr::select(IndvID, BroodIDLaid) %>%
    dplyr::group_by(BroodIDLaid) %>%
    dplyr::summarise(Chicks = n_distinct(IndvID)) %>%
    dplyr::ungroup()

  # Errors
  # Select records where number of chicks in Capture_data > brood size in Brood_data
  # (this should not be possible)
  Brood_err <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    dplyr::filter(BroodSize < Chicks) %>%
    dplyr::select(Row, PopID, BroodID, BroodSize, Chicks)

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_err %>%
      dplyr::mutate(CheckID = "B8") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..3, ")",
                                         " has a smaller BroodSize (", ..4, ")",
                                         " than the number of chicks in Individual_data (",
                                         ..5, ").")
                                })
  }

  # Warnings
  # Select records where number of chicks in Capture_data < brood size in Brood_data
  # (chicks might have died before measuring/ringing)
  Brood_war <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    dplyr::filter(BroodSize > Chicks) %>%
    dplyr::select(Row, PopID, BroodID, BroodSize, Chicks)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_war %>%
      dplyr::mutate(CheckID = "B8") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (BroodID: ", ..3, ")",
                                           " has a larger BroodSize (", ..4, ")",
                                           " than the number of chicks in Individual_data (",
                                           ..5, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check unique brood identifiers
#'
#' Check that the brood identifiers (BroodID) are unique within populations. Records with brood identifiers that are not unique within populations will result in an error.
#'
#' Check ID: B9.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_unique_BroodID <- function(Brood_data){

  # Errors
  # Select records that are duplicated within populations
  Duplicated <- Brood_data %>%
    dplyr::group_by(PopID, BroodID) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Duplicated) > 0) {
    err <- TRUE

    # Compare to whitelist
    error_records <- Duplicated %>%
      dplyr::mutate(CheckID = "B9") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::map(.x = unique(error_records$BroodID),
                               .f = ~{
                                 paste0("Record on row ",
                                        # Duplicated rows
                                        error_records[error_records$BroodID == .x, "Row"][1,],
                                        " has the same BroodID (", .x, ") as row(s) ",
                                        # Duplicates (if 1, else more)
                                        ifelse(nrow(error_records[error_records$BroodID == .x, "Row"][-1,]) == 1,
                                               error_records[error_records$BroodID == .x, "Row"][-1,],
                                               gsub("^c\\(|\\)$", "",
                                                    error_records[error_records$BroodID == .x, "Row"][-1,])),
                                        ".")
                               })
  }

  # No warnings
  war <- FALSE
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL


  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}
