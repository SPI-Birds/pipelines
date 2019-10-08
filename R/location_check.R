#' Perform quality checks on location data
#'
#' A wrapper that runs all single checks related to \code{Location_data}.
#'
#' The following location data checks are performed:
#' \itemize{
#' \item \strong{L1}: Check if the formats of each column in \code{Location_data} match with the standard format using \code{\link{check_format_location}}.
#' }
#'
#' @inheritParams checks_location_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#'
#' @export

location_check <- function(Location_data){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = purrr::map_chr(1, ~paste0("L", .)),
                               CheckDescription = c("Location data format"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Location checks")

  # - Check format location data
  message("L1: Checking format of location data...")

  check_format_location_output <- check_format_location(Location_data)

  check_list[1,3:4] <- check_format_location_output$CheckList


  return(list(CheckList = check_list,
              Warnings = list(
                Check1 = check_format_location_output$WarningOutput),
              Errors = list(
                Check1 = check_format_location_output$ErrorOutput)
  ))
}


#' Check format of location data
#'
#' Check if the formats of each column in the location data match with the standard format
#' @inheritParams checks_location_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

check_format_location <- function(Location_data){

  ## Data frame with column names and formats according to the standard protocol
  Location_data_standard <- tibble::tibble(Variable = c("Row", "LocationID", "NestboxID", "LocationType",
                                                        "PopID", "Latitude", "Longitude",
                                                        "StartSeason", "EndSeason", "Habitat"),
                                           Format_standard = c("integer", "character", "character", "character",
                                                               "character", "numeric", "numeric",
                                                               "integer", "integer", "character"))

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
  # Location_data_missing <- dplyr::left_join(Location_data_standard, Location_data_col, by="Variable") %>%
  #   filter(Format == "logical")
  #
  # war <- FALSE
  # warning_output <- NULL
  #
  # if(nrow(Location_data_missing) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::map(.x = Location_data_missing$Variable,
  #                                .f = ~{
  #                                  paste0(.x, " in Location_data is missing, unmeasured or undetermined (NA).")
  #                                })
  # }

  #Test for empty columns by looking at uniques, rather than using data type
  warning_output <- purrr::pmap(.l = list(as.list(Location_data), colnames(Location_data)),
                                .f = ~{

                                  if(all(is.na(unique(..1)))){

                                    return(paste0(..2, " in Location_data is missing, unmeasured or undetermined (NA)."))

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