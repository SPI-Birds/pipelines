#' Create approved_list
#'
#' A function to create a list of values that are verified as correct by data owners but previously marked as warning/error.
#'
#' When a data owner is sent a quality check report, they can determine whether the flagged records are a) unverifiable, b) verifiable and incorrect, or c) verifiable and correct. If a), it cannot be known for certain whether the information should be kept or removed. They are continued to be flagged as improbable/impossible, and it is up to the user to decide how they treat them in analysis. If b), it will continue to be flagged until the data owner changes the value in the raw data. If c), the records will be disregarded for future quality checks.
#'
#' To prevent verified records from flagged in future quality checks, there are two pieces of information we use: the unique identifier of the record (e.g., BroodID in Brood_data) and the unique identifier of the quality check that resulted in the original warning/error (e.g., B2).
#'
#' @param new_approved_list A list of the same format as the approved_list with new verified records.
#' @param dummy \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, the approved_list is temporarily replaced by the dummy approved_list for running tests.
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_approved_list}{Approved records in Brood_data.}
#' \item{Capture_approved_list}{Approved records in Capture_data.}
#' \item{Individual_approved_list}{Approved records in Individual_data.}
#' \item{Location_approved_list}{Approved records in Location_data.}
#'
#' @export

create_approved_list <- function(new_approved_list, dummy = FALSE){

  if (dummy == FALSE) {

    if (file.exists(here::here("./data/approved_list.rda"))) {

      # Stop if new_approved_list is missing
      if(missing(new_approved_list)) {
        stop("Please provide a new list with verified records.")
      }

      # Merge existing approved_lists with new approved_lists
      Brood_approved_list <- dplyr::bind_rows(approved_list$Brood_approved_list,
                                              new_approved_list$Brood_approved_list)

      Capture_approved_list <- dplyr::bind_rows(approved_list$Capture_approved_list,
                                                new_approved_list$Capture_approved_list)

      Individual_approved_list <- dplyr::bind_rows(approved_list$Individual_approved_list,
                                                   new_approved_list$Individual_approved_list)

      Location_approved_list <- dplyr::bind_rows(approved_list$Location_approved_list,
                                                 new_approved_list$Location_approved_list)

      overwrite <- TRUE

    } else {

      # Empty approved_list for Brood_data
      Brood_approved_list <- tibble::tibble(PopID = NA_character_,
                                            BroodID = NA_character_,
                                            CheckID = NA_character_)

      # Empty approved_list for Capture_data
      Capture_approved_list <- tibble::tibble(PopID = NA_character_,
                                              CaptureID = NA_character_,
                                              CheckID = NA_character_)

      # Empty approved_list for Individual_data
      Individual_approved_list <- tibble::tibble(PopID = NA_character_,
                                                 IndvID = NA_character_,
                                                 CheckID = NA_character_)

      # Empty approved_list for Location_data
      Location_approved_list <- tibble::tibble(PopID = NA_character_,
                                               LocationID = NA_character_,
                                               CheckID = NA_character_)

      overwrite <- FALSE
    }

    # Combine data-specific approved_lists into one list object
    approved_list <- list(Brood_approved_list = Brood_approved_list,
                          Capture_approved_list = Capture_approved_list,
                          Individual_approved_list = Individual_approved_list,
                          Location_approved_list = Location_approved_list)

  } else {

    overwrite <- TRUE

    approved_list <- new_approved_list

  }

  usethis::use_data(approved_list, overwrite=overwrite)

}
