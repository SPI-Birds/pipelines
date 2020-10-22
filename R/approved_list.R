#' Create approved_list
#'
#' A function to combine data-specific approved lists (stored in .csv files) and create a dummy approved list for package testing.
#'
#' When a data owner is sent a quality check report, they can determine whether the flagged records are a) unverifiable, b) verifiable and incorrect, or c) verifiable and correct. If a), it cannot be known for certain whether the information should be kept or removed. They are continued to be flagged as improbable/impossible, and it is up to the user to decide how they treat them in analysis. If b), it will continue to be flagged until the data owner changes the value in the raw data. If c), the records will be disregarded for future quality checks.
#'
#' To prevent verified records from flagged in future quality checks, there are two pieces of information we use: the unique identifier of the record (e.g., BroodID in Brood_data) and the unique identifier of the quality check that resulted in the original warning/error (e.g., B2).
#'
#' @param dummy \code{TRUE} or \code{FALSE} (default). If \code{TRUE}, a dummy approved_list is created for running tests.
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_approved_list}{Approved records in Brood_data.}
#' \item{Capture_approved_list}{Approved records in Capture_data.}
#' \item{Individual_approved_list}{Approved records in Individual_data.}
#' \item{Location_approved_list}{Approved records in Location_data.}
#'
#' @export

create_approved_list <- function(dummy = FALSE){

  if (dummy == FALSE) {

    # Read data-specific approved lists that are edited in .csv files
    Brood_approved_list <- utils::read.csv(system.file("extdata",
                                                       "brood_approved_list.csv",
                                                       package = "pipelines",
                                                       mustWork = TRUE))
    Capture_approved_list <- utils::read.csv(system.file("extdata",
                                                         "capture_approved_list.csv",
                                                         package = "pipelines",
                                                         mustWork = TRUE))
    Individual_approved_list <- utils::read.csv(system.file("extdata",
                                                            "individual_approved_list.csv",
                                                            package = "pipelines",
                                                            mustWork = TRUE))
    Location_approved_list <- utils::read.csv(system.file("extdata",
                                                          "location_approved_list.csv",
                                                          package = "pipelines",
                                                          mustWork = TRUE))

  } else {

    # Dummy approved_list for Brood_data
    Brood_approved_list = tibble::tibble(PopID = "AAA",
                                         BroodID = "AAA-2020-0",
                                         CheckID = "B4")

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

  }

  # Combine data-specific approved lists into one list object
  approved_list <- list(Brood_approved_list = Brood_approved_list,
                        Capture_approved_list = Capture_approved_list,
                        Individual_approved_list = Individual_approved_list,
                        Location_approved_list = Location_approved_list)


  usethis::use_data(approved_list, overwrite=TRUE)

}
