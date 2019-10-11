#' Perform quality checks on individual data
#'
#' A wrapper that runs all single checks related to \code{Individual_data}.
#'
#' The following individual data checks are performed:
#' \itemize{
#' \item \strong{I1}: Check if the formats of each column in \code{Individual_data} match with the standard format using \code{\link{check_format_individual}}.
#' }
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_individual}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#'
#' @export

individual_check <- function(Individual_data, Capture_data, Location_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = purrr::map_chr(1:3, ~paste0("I", .)),
                               CheckDescription = c("Check format of individual data",
                                                    "Check unique individual IDs",
                                                    "Check that chicks have BroodIDs"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Individual checks")

  # - Check format individual data
  if(check_format) {
    message("I1: Checking format of individual data...")

    check_format_individual_output <- check_format_individual(Individual_data)

    check_list[1, 3:4] <- check_format_individual_output$CheckList
  }

  # - Check unique individual IDs
  message("I2: Checking unique individual IDs...")

  check_unique_IndvID_output <- check_unique_IndvID(Individual_data)

  check_list[2, 3:4] <- check_unique_IndvID_output$CheckList

  # - Check that chicks have BroodIDs
  message("I3: Checking that chicks have BroodIDs...")

  check_BroodID_chicks_output <- check_BroodID_chicks(Individual_data, Capture_data, Location_data)

  check_list[3, 3:4] <- check_BroodID_chicks_output$CheckList


  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_individual_output$WarningOutput,
                         Check2 = check_unique_IndvID_output$WarningOutput,
                         Check3 = check_BroodID_chicks_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_individual_output$ErrorOutput,
                       Check2 = check_unique_IndvID_output$ErrorOutput,
                       Check3 = check_BroodID_chicks_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2 = check_unique_IndvID_output$WarningOutput,
                         Check3 = check_BroodID_chicks_output$WarningOutput)

    # Error list
    error_list <- list(Check2 = check_unique_IndvID_output$ErrorOutput,
                       Check3 = check_BroodID_chicks_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              Warnings = warning_list,
              Errors = error_list))
}


#' Check format of individual data
#'
#' Check that the format of each column in the individual data match with the standard format
#' @inheritParams checks_individual_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

check_format_individual <- function(Individual_data){

  # Data frame with column names and formats according to the standard protocol
  Individual_data_standard <- tibble::tibble(Variable = c("Row", "IndvID", "Species", "PopID",
                                                          "BroodIDLaid", "BroodIDFledged", "RingSeason",
                                                          "RingAge", "Sex"),
                                             Format_standard = c("integer", "character", "character", "character",
                                                                 "character", "character", "integer",
                                                                 "character", "character"))

  # Data frame with column names and formats from Individual data
  Individual_data_col <- tibble::tibble(Variable = names(Individual_data),
                                        Format = purrr::pmap_chr(.l = list(Individual_data), .f = class))

  # Mismatches between Individual data and standard protocol
  # Column format "logical" refers to unmeasured/undetermined variables (NA)
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
  # Individual_data_missing <- dplyr::left_join(Individual_data_standard, Individual_data_col, by="Variable") %>%
  #   filter(Format == "logical")
  #
  # war <- FALSE
  # warning_output <- NULL
  #
  # if(nrow(Individual_data_missing) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::map(.x = Individual_data_missing$Variable,
  #                                .f = ~{
  #                                  paste0(.x, " in Individual_data is missing, unmeasured or undetermined (NA).")
  #                                })
  # }

  # Test for empty columns by looking at uniques, rather than using data type
  warning_output <- purrr::pmap(.l = list(as.list(Individual_data), colnames(Individual_data)),
                                .f = ~{

                                  if(all(is.na(unique(..1)))){

                                    return(paste0(..2, " in Individual_data is missing, unmeasured or undetermined (NA)."))

                                  } else {

                                    return()

                                  }

                                })



  # Remove all cases that return NULL
  # Assigning NULL (rather than returning NULL in function) removes the list item
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

  # Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Check unique individual identifiers
#'
#' Check that the individual identifiers (IndvID) are unique within populations.
#' @inheritParams checks_individual_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#'
#' @export

check_unique_IndvID <- function(Individual_data){

  Duplicated_individuals <- Individual_data %>%
    dplyr::group_by(PopID, IndvID) %>%
    filter(n() > 1)

  err <- FALSE
  error_output <- NULL

  if(nrow(Duplicated_individuals) > 0) {
    err <- TRUE

    error_output <- purrr::map(.x = unique(Duplicated_individuals$IndvID),
                                .f = ~{
                                  paste0("Record on row ",
                                         Duplicated_individuals[Duplicated_individuals$IndvID == .x, "Row"][1,],
                                         " (IndvID: ", .x, ")",
                                         " is duplicated in row(s) ",
                                         ifelse(nrow(Duplicated_individuals[Duplicated_individuals$IndvID == .x, "Row"][-1,]) == 1,
                                                Duplicated_individuals[Duplicated_individuals$IndvID == .x, "Row"][-1,],
                                                gsub("^c\\(|\\)$", "",
                                                     Duplicated_individuals[Duplicated_individuals$IndvID == .x, "Row"][-1,])),
                                         ".")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))
}


#' Check that chicks have BroodID
#'
#' Check that all chicks in Individual_data that are caught and ringed in a nest box have a BroodID. Individuals just ringed after fledging are regarded as chicks but are not associated with a BroodID.
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

check_BroodID_chicks <- function(Individual_data, Capture_data, Location_data) {

  First_captures <- Capture_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::filter(CaptureDate == dplyr::first(CaptureDate)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(Location_data, by=c("CapturePopID" = "PopID", "LocationID"))

  Ind_cap_loc_data <- Individual_data %>%
    dplyr::left_join(First_captures, by="IndvID")

  No_BroodID_nest <- Ind_cap_loc_data %>%
    dplyr::filter(RingAge == "chick" & (is.na(BroodIDLaid) | is.na(BroodIDFledged)) & LocationType == "NB")

  err <- FALSE
  error_output <- NULL

  if(nrow(No_BroodID_nest) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = No_BroodID_nest,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " has no BroodID.")
                                })
  }


  war <- FALSE
  warning_output <- NULL

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

}


