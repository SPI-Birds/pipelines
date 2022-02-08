#' Perform quality checks on individual data
#'
#' A wrapper that runs all single checks related to \code{Individual_data}.
#'
#' The following individual data checks are performed:
#' \itemize{
#' \item \strong{I1}: Check if the IDs of individuals are unique using \code{\link{check_unique_IndvID}}.
#' \item \strong{I2}: Check if all chicks have BroodID using \code{\link{check_BroodID_chicks}}.
#' \item \strong{I3}: Check if individuals have no conflicting sex using \code{\link{check_conflicting_sex}}.
#' \item \strong{I4}: Check if individuals have no conflicting species using \code{\link{check_conflicting_species}}.
#' \item \strong{I5}: Check if individuals in Individual_data appear in Capture_data  using \code{\link{check_individuals_captures}}.
#' }
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

individual_check <- function(Individual_data, Capture_data, Location_data, approved_list, output){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("I", 1:5),
                               CheckDescription = c("Check that individual IDs are unique",
                                                    "Check that chicks have BroodIDs",
                                                    "Check that individuals have no conflicting sex",
                                                    "Check that individuals have no conflicting species",
                                                    "Check that individuals in Individual_data also appear in Capture_data"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Checking individual data...")

  # - Check unique individual IDs
  message("I1: Checking that individual IDs are unique...")

  check_unique_IndvID_output <- check_unique_IndvID(Individual_data, approved_list, output)

  check_list[1, 3:4] <- check_unique_IndvID_output$CheckList

  # - Check that chicks have BroodIDs
  message("I2: Checking that chicks have BroodIDs...")

  check_BroodID_chicks_output <- check_BroodID_chicks(Individual_data, Capture_data, Location_data, approved_list, output)

  check_list[2, 3:4] <- check_BroodID_chicks_output$CheckList

  # - Check that individuals have no conflicting sex
  message("I3: Checking that individuals have no conflicting sex...")

  check_conflicting_sex_output <- check_conflicting_sex(Individual_data, approved_list, output)

  check_list[3, 3:4] <- check_conflicting_sex_output$CheckList

  # - Check that individuals have no conflicting species
  message("I4: Checking that individuals have no conflicting species...")

  check_conflicting_species_output <- check_conflicting_species(Individual_data, approved_list, output)

  check_list[4, 3:4] <- check_conflicting_species_output$CheckList

  # - Check that individuals in Individual_data also appear in Capture_data
  message("I5: Checking that individuals in Individual_data also appear in Capture_data...")

  check_individuals_captures_output <- check_individuals_captures(Individual_data, Capture_data, approved_list, output)

  check_list[5, 3:4] <- check_individuals_captures_output$CheckList


  # Warning list
  warning_list <- list(Check1 = check_unique_IndvID_output$WarningOutput,
                       Check2 = check_BroodID_chicks_output$WarningOutput,
                       Check3 = check_conflicting_sex_output$WarningOutput,
                       Check4 = check_conflicting_species_output$WarningOutput,
                       Check5 = check_individuals_captures_output$WarningOutput)

  # Error list
  error_list <- list(Check1 = check_unique_IndvID_output$ErrorOutput,
                     Check2 = check_BroodID_chicks_output$ErrorOutput,
                     Check3 = check_conflicting_sex_output$ErrorOutput,
                     Check4 = check_conflicting_species_output$ErrorOutput,
                     Check5 = check_individuals_captures_output$ErrorOutput)

  return(list(CheckList = check_list,
              WarningRows = unique(c(check_unique_IndvID_output$WarningRows,
                                     check_BroodID_chicks_output$WarningRows,
                                     check_conflicting_sex_output$WarningRows,
                                     check_conflicting_species_output$WarningRows,
                                     check_individuals_captures_output$WarningRows)),
              ErrorRows = unique(c(check_unique_IndvID_output$ErrorRows,
                                   check_BroodID_chicks_output$ErrorRows,
                                   check_conflicting_sex_output$ErrorRows,
                                   check_conflicting_species_output$ErrorRows,
                                   check_individuals_captures_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Check unique individual identifiers
#'
#' Check that the individual identifiers (IndvID) are unique. Records with individual identifiers that are not unique within populations will result in an error.
#'
#' Check ID: I1.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_unique_IndvID <- function(Individual_data, approved_list, output){

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors")) {

    # Select records with IndvIDs that are duplicated within populations
    duplicated_within <- Individual_data %>%
      dplyr::group_by(.data$PopID, .data$IndvID) %>%
      dplyr::filter(n() > 1) %>%
      dplyr::ungroup()

    # If potential errors, add to report
    if(nrow(duplicated_within) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- duplicated_within %>%
        dplyr::mutate(CheckID = "I1") %>%
        dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

      # Create quality check report statements
      error_output <- purrr::map(.x = unique(error_records$IndvID),
                                 .f = ~{

                                   paste0("Record on row ",
                                          # Duplicated rows
                                          error_records[error_records$IndvID == .x, "Row"][1,],
                                          " (PopID: ", error_records[error_records$IndvID == .x, "PopID"][1,], ")",
                                          " has the same IndvID (", .x, ") as row(s) ",
                                          # Duplicates (if 1, else more)
                                          ifelse(nrow(error_records[error_records$IndvID == .x, "Row"][-1,]) == 1,
                                                 error_records[error_records$IndvID == .x, "Row"][-1,],
                                                 gsub("^c\\(|\\)$", "",
                                                      error_records[error_records$IndvID == .x, "Row"][-1,])),
                                          ".")

                                 })

    }

  }

  # No check for warnings
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


#' Check that chicks have BroodID
#'
#' Check that all chicks in Individual_data that are caught and ringed in a nest box have a BroodID. Individuals just ringed after fledging are regarded as chicks but are not associated with a BroodID.
#'
#' Check ID: I2.
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#'
#' @export

check_BroodID_chicks <- function(Individual_data, Capture_data, Location_data, approved_list, output) {

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors")) {

    # Select first captures and link to the information of their locations
    first_captures <- Capture_data %>%
      dplyr::group_by(.data$CapturePopID, .data$IndvID) %>%
      dplyr::filter(.data$CaptureDate == dplyr::first(.data$CaptureDate)) %>%
      dplyr::ungroup() %>%
      #dplyr::select(IndvID, Species, CapturePopID, LocationID) %>%
      dplyr::left_join(Location_data, by = c("CapturePopID" = "PopID", "LocationID"))

    # Join with individual data
    ind_cap_loc_data <- Individual_data %>%
      dplyr::left_join(first_captures, by = c("IndvID", "Species", "PopID" = "CapturePopID"))

    # Select records of individuals caught as a nestling which are not associated with a BroodID
    no_BroodID_nest <- ind_cap_loc_data %>%
      dplyr::filter(.data$Age_observed == 1 & (is.na(.data$BroodIDLaid) | is.na(.data$BroodIDFledged)) & .data$LocationType == "NB")

    # If potential errors, add to report
    if(nrow(no_BroodID_nest) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- no_BroodID_nest %>%
        dplyr::mutate(CheckID = "I2") %>%
        dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..4, "; IndvID: ", ..2, ")",
                                           " is a chick without a BroodID.")

                                  })

    }

  }

  # No check for warnings
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
#' Check ID: I3.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_conflicting_sex <- function(Individual_data, approved_list, output) {

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors")) {

    # Select records with conflicting sex
    # NB: allows v1.0 & v1.1 variable names of the standard format
    conflicting_sex <- Individual_data %>%
      {if("Sex" %in% colnames(.)) dplyr::filter(., .data$Sex == "C")
        else dplyr::filter(., .data$Sex_calculated == "C" | .data$Sex_genetic == "C")}

    # If potential errors, add to report
    if(nrow(conflicting_sex) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- conflicting_sex %>%
        dplyr::mutate(CheckID = "I3") %>%
        dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..4, "; IndvID: ", ..2, ")",
                                           " has conflicting sex.")

                                  })

    }

  }

  # No check for warnings
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



#' Check conflicting species
#'
#' Check that the species of individuals in Individual_data is recorded consistently. Individuals who have been recorded as two different species will have conflicting species ('CONFLICTED' or 'CCCCCC') in Individual_data.
#'
#' Check ID: I4.
#'
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_conflicting_species <- function(Individual_data, approved_list, output) {

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors")) {

    # Select individuals with conflicting species
    conflicting_species <- Individual_data %>%
      dplyr::filter(.data$Species %in% c("CONFLICTED", "CCCCCC"))

    # If potential errors, add to report
    if(nrow(conflicting_species) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- conflicting_species %>%
        dplyr::mutate(CheckID = "I4") %>%
        dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..4, "; IndvID: ", ..2, ")",
                                           " has conflicting species.")

                                  })

    }

  }

  # No check for warnings
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


#' Check that all individuals in Individual_data appear in Capture_data
#'
#' Check that all individuals recorded in Individual_data appear at least once in Capture_data. This should never occur, because Individual_data is usually a direct product of Capture_data (i.e., all unique individuals from Capture_data). If we find missing individuals, we need to check the pipeline code.
#'
#' Check ID: I5.
#'
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_individuals_captures <- function(Individual_data, Capture_data, approved_list, output){

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors")) {

    # Select individuals that are missing from Capture_data
    missing_individuals <- Individual_data %>%
      dplyr::group_by(.data$PopID) %>%
      dplyr::filter(!(.data$IndvID %in% Capture_data$IndvID))

    # If potential errors, add to report
    if(nrow(missing_individuals) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- missing_individuals %>%
        dplyr::mutate(CheckID = "I5") %>%
        dplyr::anti_join(approved_list$Individual_approved_list, by=c("PopID", "CheckID", "IndvID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..4, "; IndvID: ", ..2, ")",
                                           " does not appear in Capture_data.")

                                  })

    }

  }

  # No check for warnings
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
