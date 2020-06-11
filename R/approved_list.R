#' Create approved_list
#'
#' A function to create a list of values that are verified as correct by data owners but previously marked as warning/error.
#'
#' When a data owner is sent a quality check report, they can determine whether the flagged records are a) unverifiable, b) verifiable and inccorect, or c) verifiable and correct. If a), it cannot be known for certain whether the information should be kept or removed. They are continued to be flagged as improbable/impossible, and it is up to the user to decide how they treat them in analysis. If b), it will continue to be flagged until the data owner changes the value in the raw data. If c), the records will be disregarded for future quality checks.
#'
#' To prevent verified records from flagged in future quality checks, there are two pieces of information we use: the unique identifier of the record (e.g., BroodID in Brood_data) and the unique identifier of the quality check that resulted in the original warning/error (e.g., B2).
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_approved_list}{Approved records in Brood_data.}
#' \item{Capture_approved_list}{Approved records in Capture_data.}
#' \item{Individual_approved_list}{Approved records in Individual_data.}
#' \item{Location_approved_list}{Approved records in Location_data.}
#'
#' @export

create_approved_list <- function(){

  # Approved_list for Brood_data
  Brood_approved_list <- tibble::tibble(PopID = NA_character_,
                                        BroodID = NA_character_,
                                        CheckID = NA_character_)

  # First entry
  # Brood_approved_list <- Brood_approved_list %>%
  #   dplyr::mutate(PopID = "", BroodID = "", CheckID = "")

  # Second and higher entries
  # Brood_approved_list <- Brood_approved_list %>%
  #   tibble::add_row(PopID = "", BroodID = "", CheckID = "")

  # Approved_list for Capture_data
  Capture_approved_list <- tibble::tibble(PopID = NA_character_,
                                          CaptureID = NA_character_,
                                          CheckID = NA_character_)

  # Approved_list for Individual_data
  Individual_approved_list <- tibble::tibble(PopID = NA_character_,
                                             IndvID = NA_character_,
                                             CheckID = NA_character_)

  # Approved_list for Location_data
  Location_approved_list <- tibble::tibble(PopID = NA_character_,
                                           LocationID = NA_character_,
                                           CheckID = NA_character_)

  # Combine data-specific approved_lists into one list object
  approved_list <- list(Brood_wapproved_list = Brood_approved_list,
                        Capture_approved_list = Capture_approved_list,
                        Individual_approved_list = Individual_approved_list,
                        Location_approved_list = Location_approved_list)

  return(approved_list)
}
