#' Perform quality checks on location data
#'
#' A wrapper that runs all single checks related to \code{Location_data}.
#'
#' The following location data checks are performed:
#' \itemize{
#' \item \strong{L1}:
#' }
#'
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#'
#' @export

location_check <- function(Location_data, approved_list){

  # Create check list with a summary of warnings and errors per check
  # check_list <- tibble::tibble(CheckID = paste0("L", 1),
  #                              CheckDescription = c(""),
  #                              Warning = NA,
  #                              Error = NA)

  # Checks
  message("Location checks")

  # # - Check format location data
  # if(check_format) {
  #   message("L1: Checking format of location data...")
  #
  #   check_format_location_output <- check_format_location(Location_data, approved_list)
  #
  #   check_list[1,3:4] <- check_format_location_output$CheckList
  # }

  # Warning list
  warning_list <- NULL

  # Error list
  error_list <- NULL

  check_list <- NULL

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = NULL,
              Warnings = warning_list,
              Errors = error_list))
}
