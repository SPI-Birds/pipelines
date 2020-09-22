#' Perform quality checks on individual data
#'
#' A wrapper that runs all single checks related to \code{Individual_data}.
#'
#' The following individual data checks are performed:
#' \itemize{
#' \item \strong{I1}: Check if the formats of each column in \code{Individual_data} match with the standard format using \code{\link{check_format_individual}}.
#' \item \strong{I2}: Check if the IDs of individuals are unique using \code{\link{check_unique_IndvID}}.
#' \item \strong{I3}: Check if all chicks have BroodID using \code{\link{check_BroodID_chicks}}.
#' \item \strong{I4}: Check if individuals have no conflicting sex using \code{\link{check_conflicting_sex}}.
#' \item \strong{I5}: Check if individuals have no conflicting species using \code{\link{check_conflicting_species}}.
#' }
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_individual}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @inherit checks_return return
#'
#' @export

individual_check <- function(Individual_data, Capture_data, Location_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("I", 1:5),
                               CheckDescription = c("Check format of individual data",
                                                    "Check that individual IDs are unique",
                                                    "Check that chicks have BroodIDs",
                                                    "Check that individuals have no conflicting sex",
                                                    "Check that individuals have no conflicting species"),
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
  message("I2: Checking that individual IDs are unique...")

  check_unique_IndvID_output <- check_unique_IndvID(Individual_data)

  check_list[2, 3:4] <- check_unique_IndvID_output$CheckList

  # - Check that chicks have BroodIDs
  message("I3: Checking that chicks have BroodIDs...")

  check_BroodID_chicks_output <- check_BroodID_chicks(Individual_data, Capture_data, Location_data)

  check_list[3, 3:4] <- check_BroodID_chicks_output$CheckList

  # - Check that individuals have no conflicting sex
  message("I4: Checking that individuals have no conflicting sex...")

  check_conflicting_sex_output <- check_conflicting_sex(Individual_data)

  check_list[4, 3:4] <- check_conflicting_sex_output$CheckList

  # - Check that individuals have no conflicting species
  message("I5: Checking that individuals have no conflicting species...")

  check_conflicting_species_output <- check_conflicting_species(Individual_data)

  check_list[5, 3:4] <- check_conflicting_species_output$CheckList


  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_individual_output$WarningOutput,
                         Check2 = check_unique_IndvID_output$WarningOutput,
                         Check3 = check_BroodID_chicks_output$WarningOutput,
                         Check4 = check_conflicting_sex_output$WarningOutput,
                         Check5 = check_conflicting_species_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_individual_output$ErrorOutput,
                       Check2 = check_unique_IndvID_output$ErrorOutput,
                       Check3 = check_BroodID_chicks_output$ErrorOutput,
                       Check4 = check_conflicting_sex_output$ErrorOutput,
                       Check5 = check_conflicting_species_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2 = check_unique_IndvID_output$WarningOutput,
                         Check3 = check_BroodID_chicks_output$WarningOutput,
                         Check4 = check_conflicting_sex_output$WarningOutput,
                         Check5 = check_conflicting_species_output$WarningOutput)

    # Error list
    error_list <- list(Check2 = check_unique_IndvID_output$ErrorOutput,
                       Check3 = check_BroodID_chicks_output$ErrorOutput,
                       Check4 = check_conflicting_sex_output$ErrorOutput,
                       Check5 = check_conflicting_species_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              WarningRows = unique(c(check_unique_IndvID_output$WarningRows,
                                     check_BroodID_chicks_output$WarningRows,
                                     check_conflicting_sex_output$WarningRows,
                                     check_conflicting_species_output$WarningRows)),
              ErrorRows = unique(c(check_unique_IndvID_output$ErrorRows,
                                   check_BroodID_chicks_output$ErrorRows,
                                   check_conflicting_sex_output$ErrorRows,
                                   check_conflicting_species_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}


#' Check format of individual data
#'
#' Check that the format of each column in the individual data match with the standard format.
#'
#' Check ID: I1.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
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
#' Check that the individual identifiers (IndvID) are unique. Records with individual identifiers that are not unique among populations will result in a warning. Records with individual identifiers that are not unique within populations will result in an error.
#'
#' Check ID: I2.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_unique_IndvID <- function(Individual_data){

  # Errors
  # Select records with IndvIDs that are duplicated within populations
  Duplicated_within <- Individual_data %>%
    dplyr::group_by(PopID, IndvID) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Duplicated_within) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Duplicated_within %>%
      dplyr::mutate(CheckID = "I2") %>%
      dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

    # Create quality check report statements
    error_output <- purrr::map(.x = unique(error_records$IndvID),
                               .f = ~{
                                 paste0("Record on row ",
                                        # Duplicated rows
                                        error_records[error_records$IndvID == .x, "Row"][1,],
                                        " has the same IndvID (", .x, ") as row(s) ",
                                        # Duplicates (if 1, else more)
                                        ifelse(nrow(error_records[error_records$IndvID == .x, "Row"][-1,]) == 1,
                                               error_records[error_records$IndvID == .x, "Row"][-1,],
                                               gsub("^c\\(|\\)$", "",
                                                    error_records[error_records$IndvID == .x, "Row"][-1,])),
                                        ".")
                               })
  }

  # Warnings
  # Select records with IndvIDs that are duplicated among populations
  Duplicated_among <- Individual_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::filter(!duplicated(PopID) & n() > 1) %>%
    dplyr::filter(n() > 1)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Duplicated_among) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Duplicated_among %>%
      dplyr::mutate(CheckID = "I2") %>%
      dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

    # Create quality check report statements
    warning_output <- purrr::map(.x = unique(warning_records$IndvID),
                                 .f = ~{
                                   paste0("Record on row ",
                                          # Duplicated rows
                                          warning_records[warning_records$IndvID == .x, "Row"][1,],
                                          " (IndvID: ", .x, ", ", "PopID: ",
                                          warning_records[warning_records$IndvID == .x, "PopID"][1,],")",
                                          " has the same IndvID as row(s) ",
                                          # Duplicates (if 1, else more)
                                          ifelse(nrow(warning_records[warning_records$IndvID == .x, "Row"][-1,]) == 1,
                                                 warning_records[warning_records$IndvID == .x, "Row"][-1,],
                                                 gsub("^c\\(|\\)$", "",
                                                      warning_records[warning_records$IndvID == .x, "Row"][-1,])),
                                          " (PopID: ", warning_records[warning_records$IndvID == .x, "PopID"][-1,],").")
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


#' Check that chicks have BroodID
#'
#' Check that all chicks in Individual_data that are caught and ringed in a nest box have a BroodID. Individuals just ringed after fledging are regarded as chicks but are not associated with a BroodID.
#'
#' Check ID: I3.
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#'
#' @export

check_BroodID_chicks <- function(Individual_data, Capture_data, Location_data) {

  # Select first captures and link to the information of their locations
  First_captures <- Capture_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::filter(CaptureDate == dplyr::first(CaptureDate)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(Location_data, by=c("CapturePopID" = "PopID", "LocationID"))

  # Join with individual data
  Ind_cap_loc_data <- Individual_data %>%
    dplyr::left_join(First_captures, by="IndvID")

  # Errors
  # Select records with chicks caught in a nest box but not associated with a BroodID
  No_BroodID_nest <- Ind_cap_loc_data %>%
    dplyr::filter(RingAge == "chick" & (is.na(BroodIDLaid) | is.na(BroodIDFledged)) & LocationType == "NB")

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(No_BroodID_nest) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- No_BroodID_nest %>%
      dplyr::mutate(CheckID = "I3") %>%
      dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1, " (IndvID: ", ..2, ")",
                                         " has no BroodID.")
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


#' Check conflicting sex
#'
#' Check that the sex of individuals in Individual_data is recorded consistently. Individuals who have been recorded as both male ('M') and female ('F') will have conflicting sex ('C') in Individual_data.
#'
#' Check ID: I4.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_conflicting_sex <- function(Individual_data) {

  # Select records with conflicting sex
  Conflicting_sex <- Individual_data %>%
    dplyr::filter(Sex == "C")

  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Conflicting_sex) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Conflicting_sex %>%
      dplyr::mutate(CheckID = "I4") %>%
      dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (IndvID: ", ..2, ")",
                                           " has conflicting sex.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  Conflicting_sex <- NULL
  approved_list <- NULL
}



#' Check conflicting species
#'
#' Check that the species of individuals in Individual_data is recorded consistently. Individuals who have been recorded as two different species will have conflicting species ('CONFLICTED') in Individual_data.
#'
#' Check ID: I5.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_conflicting_species <- function(Individual_data) {

  # Select individuals with conflicting species
  Conflicting_species <- Individual_data %>%
    dplyr::filter(Species == "CONFLICTED")

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Conflicting_species) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Conflicting_species %>%
      dplyr::mutate(CheckID = "I5") %>%
      dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1," (IndvID: ", ..2, ")",
                                         " has conflicting species.")
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
  Conflicting_species <- NULL
  approved_list <- NULL

}
