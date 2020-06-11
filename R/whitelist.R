#' Create whitelist
#'
#' A function to create a whitelist of values that are verified as correct by data owners but previously marked as warning/error.
#'
#' When a data owner is sent a quality check report, they can determine whether the flagged records are a) unverifiable, b) verifiable and inccorect, or c) verifiable and correct. If a), it cannot be known for certain whether the information should be kept or removed. They are continued to be flagged as improbable/impossible, and it is up to the user to decide how they treat them in analysis. If b), it will continue to be flagged until the data owner changes the value in the raw data. If c), the records will be whitelisted for future quality checks.
#'
#' To whitelist records, there are two pieces of information we use: the unique identifier of the record (e.g., BroodID in Brood_data) and the unique identifier of the quality check that resulted in the original warning/error (e.g., B2).
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_whitelist}{Whitelisted records in Brood_data.}
#' \item{Capture_whitelist}{Whitelisted records in Capture_data.}
#' \item{Individual_whitelist}{Whitelisted records in Individual_data.}
#' \item{Location_whitelist}{Whitelisted records in Location_data.}
#'
#' @export

create_whitelist <- function(){

  # Whitelist for Brood_data
  Brood_whitelist <- tibble::tibble(PopID = NA_character_,
                                    BroodID = NA_character_,
                                    CheckID = NA_character_)

  # First entry
  # Brood_whitelist <- Brood_whitelist %>%
  #   dplyr::mutate(PopID = "", BroodID = "", CheckID = "")

  # Second and higher entries
  # Brood_whitelist <- Brood_whitelist %>%
  #   tibble::add_row(PopID = "", BroodID = "", CheckID = "")

  # Whitelist for Capture_data
  Capture_whitelist <- tibble::tibble(PopID = NA_character_,
                                      CaptureID = NA_character_,
                                      CheckID = NA_character_)

  # Whitelist for Individual_data
  Individual_whitelist <- tibble::tibble(PopID = NA_character_,
                                         IndvID = NA_character_,
                                         CheckID = NA_character_)

  # Whitelist for Location_data
  Location_whitelist <- tibble::tibble(PopID = NA_character_,
                                       LocationID = NA_character_,
                                       CheckID = NA_character_)

  # Combine data-specific whitelists into one list object
  whitelist <- list(Brood_whitelist = Brood_whitelist,
                    Capture_whitelist = Capture_whitelist,
                    Individual_whitelist = Individual_whitelist,
                    Location_whitelist = Location_whitelist)

  return(whitelist)
}
