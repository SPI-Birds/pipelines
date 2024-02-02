#' Perform quality checks on brood data - TEST
#'
#' A wrapper that runs all single checks related to \code{Brood_data}.
#'
#' The following brood data checks are performed:
#' \itemize{
#' \item \strong{B1}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \item \strong{B2}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#' \item \strong{B3}: Compare lay date and hatch date per brood using \code{\link{compare_laying_hatching}}.
#' \item \strong{B4}: Compare hatch date and fledge date per brood using \code{\link{compare_hatching_fledging}}.
#' \item \strong{B5a-f}: Check brood variable values against reference values using \code{\link{check_values_brood}}. Brood variables checked: ClutchSize_observed, BroodSize_observed, NumberFledged_observed, LayDate_observed, HatchDate_observed, FledgeDate_observed.
#' \item \strong{B6}: Compare brood size with number of chicks in Individual_data using \code{\link{compare_broodsize_chicknumber}}.
#' \item \strong{B7}: Check if the IDs of broods are unique using \code{\link{check_unique_BroodID}}.
#' \item \strong{B8}: Check if the order of clutch types for multiple breeding attempts per female per season is correct using \code{\link{check_clutch_type_order}}.
#' \item \strong{B9}: Check if parents of a brood are the same species using \code{\link{compare_species_parents}}.
#' \item \strong{B10}: Check if the brood and the parents of that brood are recorded as the same species using \code{\link{compare_species_brood_parents}}.
#' \item \strong{B11}: Check if the brood and the chicks in that brood are recorded as the same species using \code{\link{compare_species_brood_chicks}}.
#' \item \strong{B12}: Check if the sex of mothers listed under FemaleID are female using \code{\link{check_sex_mothers}}.
#' \item \strong{B13}: Check if the sex of fathers listed under MaleID are male using \code{\link{check_sex_fathers}}.
#' \item \strong{B14}: Check that both parents appear in Capture_data using \code{\link{check_parents_captures}}.
#' \item \strong{B15}: Check that nest locations appear in Location_data using \code{\link{check_brood_locations}}.
#' }
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#' @export

brood_check <- function(Brood_data, Individual_data, Capture_data, Location_data, approved_list, output, skip){

  # Perform brood checks
  message("Checking brood data...")

  # Select correct variable names, dependent on version of the standard format
  var_ext <- ifelse("ClutchSize" %in% colnames(Brood_data),
                    ifelse(all(is.na(Brood_data$ClutchSize)), "_observed", ""), "_observed")

  # Run checks and create list of check outputs
  check_outputs <- list(B1 = compare_clutch_brood(Brood_data, approved_list, output, skip), # B1: Compare clutch and brood sizes
                        B2 = compare_brood_fledglings(Brood_data, approved_list, output, skip), # B2: Compare brood sizes and fledgling numbers
                        B3 = compare_laying_hatching(Brood_data, approved_list, output, skip), # B3: Compare lay and hatch dates
                        B4 = compare_hatching_fledging(Brood_data, approved_list, output, skip), # B4: Compare hatch and fledge dates
                        B5a = check_values_brood(Brood_data, paste0("ClutchSize", var_ext),
                                                 approved_list, output, skip), # B5a: Check clutch size values against reference values
                        B5b = check_values_brood(Brood_data, paste0("BroodSize", var_ext),
                                                 approved_list, output, skip), # B5b: Check brood size values against reference values
                        B5c = check_values_brood(Brood_data, paste0("NumberFledged", var_ext),
                                                 approved_list, output, skip), # B5c: Check fledgling number values against reference values
                        B5d = check_values_brood(Brood_data, paste0("LayDate", var_ext),
                                                 approved_list, output, skip), # B5d: Check lay date values against reference values
                        B5e = check_values_brood(Brood_data, paste0("HatchDate", var_ext),
                                                 approved_list, output, skip), # B5e: Check hatch date values against reference values
                        B5f = check_values_brood(Brood_data, paste0("FledgeDate", var_ext),
                                                 approved_list, output, skip), # B5f: Check fledge date values against reference values
                        B6 = compare_broodsize_chicknumber(Brood_data, Individual_data,
                                                           approved_list, output, skip), # B6: Compare brood size and number of chicks in Individual_data
                        B7 = check_unique_BroodID(Brood_data, approved_list, output, skip), # B7: Check that BroodIDs are unique
                        B8 = check_clutch_type_order(Brood_data, approved_list, output, skip), # B8: Check clutch type order
                        B9 = compare_species_parents(Brood_data, Individual_data,
                                                     approved_list, output, skip), # B9: Compare species of mother and father
                        B10 = compare_species_brood_parents(Brood_data, Individual_data,
                                                            approved_list, output, skip), # B10: Compare species of brood and parents
                        B11 = compare_species_brood_chicks(Brood_data, Individual_data,
                                                           approved_list, output, skip), # B11: Compare species of brood and chicks
                        B12 = check_sex_mothers(Brood_data, Individual_data, approved_list, output, skip), # B12: Check sex of mothers
                        B13 = check_sex_fathers(Brood_data, Individual_data, approved_list, output, skip), # B13: Check sex of fathers
                        B14 = check_parents_captures(Brood_data, Capture_data, approved_list, output, skip), # B14: Check that parents appear in Capture_data
                        B15 = check_brood_locations(Brood_data, Location_data, approved_list, output, skip) # B15: Check that nest locations appear in Location_data
  )

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("B", c(1:4, paste0(5, letters[1:6]), 6:15)),
                               CheckDescription = c("Compare clutch and brood sizes",
                                                    "Compare brood sizes and fledgling numbers",
                                                    "Compare lay and hatch dates",
                                                    "Compare hatch and fledge dates",
                                                    "Check clutch size values against reference values",
                                                    "Check brood size values against reference values",
                                                    "Check fledgling number values against reference values",
                                                    "Check lay date values against reference values",
                                                    "Check hatch date values against reference values",
                                                    "Check fledge date values against reference values",
                                                    "Compare brood size with number of chicks in Individual_data",
                                                    "Check that brood IDs are unique",
                                                    "Check clutch type order",
                                                    "Check species of mother and father",
                                                    "Check species of brood and parents",
                                                    "Check species of brood and chicks",
                                                    "Check sex of mothers",
                                                    "Check sex of fathers",
                                                    "Check that parents appear in Capture_data",
                                                    "Check that nest locations appear in Location_data"),
                               Warning = NA,
                               Error = NA,
                               WarningRecords = NA_integer_,
                               ErrorRecords = NA_integer_,
                               Skipped = NA)

  check_list[,3:7] <- purrr::map_dfr(.x = check_outputs, .f = 1) # Combine check lists of single checks

  # Create list of 'warning' messages
  warning_list <- purrr::map(.x = check_outputs, .f = 4)

  # Create list of 'potential error' messages
  error_list <- purrr::map(.x = check_outputs, .f = 5)

  return(list(CheckList = check_list,
              WarningRows = purrr::map(.x = check_outputs, .f = 2) %>% unlist(use.names = FALSE) %>% unique(),
              ErrorRows = purrr::map(.x = check_outputs, .f = 3) %>% unlist(use.names = FALSE) %>% unique(),
              Warnings = warning_list,
              Errors = error_list))
}

#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will be flagged as a potential error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will be flagged as a warning.
#'
#' Check ID: B1.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_clutch_brood <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B1" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B1: Comparing clutch and brood sizes...")

  } else {

    message("<< B1 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Non-manipulated broods
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_non <- Brood_data %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., (is.na(.data$ExperimentID) | .data$ExperimentID == "") & .data$ClutchSize < .data$BroodSize) else dplyr::filter(., (is.na(.data$ExperimentID) | .data$ExperimentID == "") & .data$ClutchSize_observed < .data$BroodSize_observed)}

    # If potential errors, add to report
    if(nrow(brood_data_non) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_data_non %>%
        dplyr::mutate(CheckID = "B1") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "ClutchSize", "BroodSize")
          else dplyr::select(., "Row", "PopID", "BroodID", "ClutchSize_observed", "BroodSize_observed")}

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a larger brood size (", ..5,
                                           ") than clutch size (", ..4,
                                           ") but was not experimentally manipulated.")

                                  })

    }

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Manipulated broods
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_man <- Brood_data %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., (!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$ClutchSize < .data$BroodSize) else dplyr::filter(., (!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$ClutchSize_observed < .data$BroodSize_observed)}

    # If warnings, add to report
    if(nrow(brood_data_man) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- brood_data_man %>%
        dplyr::mutate(CheckID = "B1") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "ClutchSize", "BroodSize")
          else dplyr::select(., "Row", "PopID", "BroodID", "ClutchSize_observed", "BroodSize_observed")}

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " has a larger brood size (", ..5,
                                             ") than clutch size (", ..4,
                                             ") but was experimentally manipulated.")

                                    })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)


  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  approved_list <- NULL

}


#' Compare brood sizes and fledgling numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will be flagged as a potential error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will be flagged as a warning.
#'
#' Check ID: B2.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_brood_fledglings <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B2" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B2: Comparing brood sizes and fledgling numbers...")

  } else {

    message("<< B2 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Non-manipulated broods
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_non <- Brood_data %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., (is.na(.data$ExperimentID) | .data$ExperimentID == "") & .data$BroodSize < .data$NumberFledged) else dplyr::filter(., (is.na(.data$ExperimentID) | .data$ExperimentID == "") & .data$BroodSize_observed < .data$NumberFledged_observed)}

    # If potential errors, add to report
    if(nrow(brood_data_non) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_data_non %>%
        dplyr::mutate(CheckID = "B2") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "BroodSize", "NumberFledged")
          else dplyr::select(., "Row", "PopID", "BroodID", "BroodSize_observed", "NumberFledged_observed")}

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a larger fledgling number (", ..5,
                                           ") than brood size (", ..4,
                                           "), but was not experimentally manipulated.")

                                  })

    }

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Manipulated broods
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_man <- Brood_data %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., (!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$BroodSize < .data$NumberFledged) else dplyr::filter(., (!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$BroodSize_observed < .data$NumberFledged_observed)}

    # If warnings, add to report
    if(nrow(brood_data_man) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- brood_data_man %>%
        dplyr::mutate(CheckID = "B2") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "BroodSize", "NumberFledged")
          else dplyr::select(., "Row", "PopID", "BroodID", "BroodSize_observed", "NumberFledged_observed")}

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " has a larger fledgling number (", ..5,
                                             ") than brood size (", ..4,
                                             "), and was experimentally manipulated.")

                                    })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will be flagged as a potential error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will be flagged as a warning.
#'
#' Check ID: B3.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_laying_hatching <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B3" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B3: Comparing lay and hatch dates...")

  } else {

    message("<< B3 is skipped >>")

  }

  # TODO
  # Broods with laying date earlier than hatching date but the difference
  # in number of days is smaller than incubation time
  ## INCUBATION TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(LayDate < HatchDate & (HatchDate-LayDate) >= )

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Broods with laying date later than hatching date
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_late <- Brood_data %>%
      {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::filter(., .data$LayDate >= .data$HatchDate)
        else dplyr::filter(., .data$LayDate_observed >= .data$HatchDate_observed)}

    # If potential errors, add to report
    if(nrow(brood_data_late) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_data_late %>%
        dplyr::mutate(CheckID = "B3") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "LayDate", "HatchDate")
          else dplyr::select(., "Row", "PopID", "BroodID", "LayDate_observed", "HatchDate_observed")}

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a laying date (", ..4,
                                           ") equal to or later than a hatching date (", ..5, ").")

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' Check ID: B4.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_hatching_fledging <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B4" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B4: Comparing hatch and fledge dates...")

  } else {

    message("<< B4 is skipped >>")

  }

  # TODO
  # Broods with hatching date earlier than fledging date but the difference
  # in number of days is smaller than breeding time
  ## BREEDING TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(HatchDate < FledgeDate & (FledgeDate-HatchDate) >= )

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Broods with laying date later than hatching date
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_data_late <- Brood_data %>%
      {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::filter(., .data$HatchDate >= .data$FledgeDate)
        else dplyr::filter(., .data$HatchDate_observed >= .data$FledgeDate_observed)}

    # If potential errors, add to report
    if(nrow(brood_data_late) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_data_late %>%
      dplyr::mutate(CheckID = "B4") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::select(., "Row", "PopID", "BroodID", "HatchDate", "FledgeDate")
        else dplyr::select(., "Row", "PopID", "BroodID", "HatchDate_observed", "FledgeDate_observed")}

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a hatching date (", ..4,
                                         ") equal to or later than a fledging date (", ..5, ").")

                                })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against population-species-specific reference values in brood data. Reference values are based on the data if the number of observations is sufficiently large. Records for population-species combinations that are too low in number are only compared to reference values that are not data generated (see Details below).
#'
#' \strong{ClutchSize_observed, BroodSize_observed, NumberFledged_observed} \cr
#' Check IDs: B5a-c \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered impossible if they are negative or larger than 2 times the 99th percentile, and will be flagged as a potential error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as a potential error.}
#' }
#'
#' \strong{LayDate_observed, HatchDate_observed, FledgeDate_observed} \cr
#' Check ID: B5d-f \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered impossible if they are earlier than January 1st or later than December 31st of the current breeding season, and will be flagged as a potential error.}
#' \item{\emph{n < 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered impossible if they are earlier than January 1st or later than December 31st of the current breeding season, and will be flagged as a potential error.}
#' }
#'
#' Note: when the number of observations is too low to generate reference values, a message is added to the list of warnings.
#'
#' @inheritParams checks_brood_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_brood <- function(Brood_data, var, approved_list, output, skip) {

  # Stop if {var} is missing
  if(missing(var)) {

    stop("Please select a variable in Brood_data to check against reference values.")

  }

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when(var %in% c("ClutchSize", "ClutchSize_observed") & "B5a" %in% skip ~ TRUE,
                                 var %in% c("BroodSize", "BroodSize_observed") & "B5b" %in% skip ~ TRUE,
                                 var %in% c("NumberFledged", "NumberFledged_observed") & "B5c" %in% skip ~ TRUE,
                                 var %in% c("LayDate", "LayDate_observed") & "B5d" %in% skip ~ TRUE,
                                 var %in% c("HatchDate", "HatchDate_observed") & "B5e" %in% skip ~ TRUE,
                                 var %in% c("FledgeDate", "FledgeDate_observed") & "B5f" %in% skip ~ TRUE,
                                 "B5" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check messages
  if(skip_check == FALSE) {

    check_message <- dplyr::case_when(var %in% c("ClutchSize", "ClutchSize_observed") ~ c("B5a", "clutch size"),
                                      var %in% c("BroodSize", "BroodSize_observed") ~ c("B5b", "brood size"),
                                      var %in% c("NumberFledged", "NumberFledged_observed") ~ c("B5c", "fledgling number"),
                                      var %in% c("LayDate", "LayDate_observed") ~ c("B5d", "lay date"),
                                      var %in% c("HatchDate", "HatchDate_observed") ~ c("B5e", "hatch date"),
                                      var %in% c("FledgeDate", "FledgeDate_observed") ~ c("B5f", "fledge date"))

    message(paste0(check_message[1], ": Checking ", check_message[2], " values against reference values..."))

  } else {

    id_message <- dplyr::case_when(var %in% c("ClutchSize", "ClutchSize_observed") ~ "B5a",
                                   var %in% c("BroodSize", "BroodSize_observed") ~ "B5b",
                                   var %in% c("NumberFledged", "NumberFledged_observed") ~ "B5c",
                                   var %in% c("LayDate", "LayDate_observed") ~ "B5d",
                                   var %in% c("HatchDate", "HatchDate_observed") ~ "B5e",
                                   var %in% c("FledgeDate", "FledgeDate_observed") ~ "B5f")

    message(paste0("<< ", id_message, " is skipped >>"))

  }

  # Check for each population & species if {var} was recorded
  var_recorded <- Brood_data %>%
    dplyr::filter(!is.na(.data$Species)) %>%
    dplyr::group_by(.data$PopID, .data$Species) %>%
    dplyr::summarise(recorded = ifelse(!all(is.na(!!rlang::sym(var))), TRUE, FALSE),
                     .groups = "drop")

  var_not_recorded <- var_recorded %>%
    dplyr::filter(.data$recorded == FALSE)


  # Create reference values from data
  # Numeric & integer {vars}
  if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed") & skip_check == FALSE & any(var_recorded$recorded == TRUE)) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(.data$Species)) %>%
      dplyr::anti_join(var_not_recorded, by = c("PopID", "Species")) %>%
      dplyr::group_by(.data$Species, .data$PopID) %>%
      dplyr::summarise(Error_min = 0,
                       Error_max = 2 * ceiling(stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE)),
                       n = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::arrange(.data$PopID, .data$Species)

    # Date {vars}
  } else if(var %in% c("LayDate", "HatchDate", "FledgeDate",
                       "LayDate_observed", "HatchDate_observed", "FledgeDate_observed") & skip_check == FALSE & any(var_recorded$recorded == TRUE)) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(.data$Species)) %>%
      dplyr::anti_join(var_not_recorded, by = c("PopID", "Species")) %>%
      # Transform dates to Julian days (while accounting for year) to calculate quantiles
      dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
      dplyr::group_by(.data$Species, .data$PopID) %>%
      dplyr::summarise(Error_min = 1,
                       Error_max = 366, #TODO: Update for birds breeding in winter, in the tropics, or the Southern Hemisphere
                       n = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::arrange(.data$PopID, .data$Species)

  }

  # Print message for population-species combinations with too low number of observations
  if(skip_check == FALSE & exists("ref")) {

    if(any(ref$n < 100)) {

      low_obs <- ref %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select("Species", "PopID")

      purrr::pwalk(.l = list(low_obs$Species,
                             low_obs$PopID,
                             rep(var, nrow(low_obs))),
                   .f = ~{

                     message(paste0("Number of ", ..3, " records for ", ..2, ": ", ..1,
                                    " is too low (< 100) to create reliable reference values."))

                   })

    }

  }

  # Print message for population-species combinations for which {var} was not recorded
  if(skip_check == FALSE & nrow(var_not_recorded) > 0) {

    purrr::pwalk(.l = list(var_not_recorded$Species,
                           var_not_recorded$PopID),
                 .f = ~{

                   message(paste0(var, " was not recorded for ", ..2, ": ", ..1, "."))

                 })

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE & exists("ref")) {

    # Progress bar
    pb <- progress::progress_bar$new(total = 2 * nrow(ref),
                                     format = "[:bar] :percent ~:eta remaining")

    # Brood-specific errors
    brood_err <- purrr::pmap(.l = ref,
                             .f = ~{

                               pb$tick()


                               if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                             "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                                 # If number of observations is large enough, compare brood values
                                 # to all reference values
                                 if(..5 >= 100) {

                                   # Brood records below lower error threshold
                                   lower_err <- Brood_data %>%
                                     dplyr::mutate(Variable = var,
                                                   Threshold = "L",
                                                   Ref = ..3) %>%
                                     dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) < ..3) %>%
                                     dplyr::select("Row", "PopID", "BroodID", !!rlang::sym(var), "Species", "Variable", "Threshold", "Ref")


                                   # Brood records above upper error threshold
                                   upper_err <- Brood_data %>%
                                     dplyr::mutate(Variable = var,
                                                   Threshold = "U",
                                                   Ref = ..4) %>%
                                     dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) > ..4) %>%
                                     dplyr::select("Row", "PopID", "BroodID", !!rlang::sym(var), "Species", "Variable", "Threshold", "Ref")


                                   dplyr::bind_rows(lower_err, upper_err)

                                   # If number of observations is too low, only compare brood values
                                   # to reference values not based on quantiles
                                 } else {

                                   # Brood records below lower error threshold
                                   Brood_data %>%
                                     dplyr::mutate(Variable = var,
                                                   Threshold = "L",
                                                   Ref = ..3) %>%
                                     dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) < ..3) %>%
                                     dplyr::select("Row", "PopID", "BroodID", !!rlang::sym(var), "Species", "Variable", "Threshold", "Ref")

                                 }

                               } else if(var %in% c("LayDate", "HatchDate", "FledgeDate",
                                                    "LayDate_observed", "HatchDate_observed", "FledgeDate_observed")) {

                                 # Brood records below lower error threshold
                                 lower_err <- Brood_data %>%
                                   # Transform dates to Julian days (while accounting for year)
                                   # to compare to Julian day reference values
                                   dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                   dplyr::mutate(Variable = var,
                                                 Threshold = "L",
                                                 Ref = paste(.data$BreedingSeason, "01", "01", sep = "-")) %>%
                                   dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) < ..3) %>%
                                   dplyr::select("Row", "PopID", "BroodID", !!rlang::sym(var),
                                                 "Species", "Variable", "Threshold", "Ref")

                                 # Brood records above upper error threshold
                                 upper_err <- Brood_data %>%
                                   # Transform dates to Julian days (while accounting for year)
                                   # to compare to Julian day reference values
                                   dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                   dplyr::mutate(Variable = var,
                                                 Threshold = "U",
                                                 Ref = paste(.data$BreedingSeason, "12", "31", sep = "-")) %>%
                                   dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) > ..4) %>%
                                   dplyr::select("Row", "PopID", "BroodID", !!rlang::sym(var),
                                                 "Species", "Variable", "Threshold", "Ref")

                                 dplyr::bind_rows(lower_err, upper_err)

                               }

                             }) %>%
      dplyr::bind_rows()

    # If potential errors, add to report
    if(nrow(brood_err) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_err %>%
        dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        dplyr::arrange(.data$Row)

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    if(..6 %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                                  "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                                      paste0("Record on row ", ..1,
                                             " (PopID: ", ..2, "; ",
                                             "BroodID: ", ..3, "; ",
                                             species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                             " has a ", ifelse(..7 == "U", "larger", "smaller"), " value in ",
                                             ..6, " (", ..4, ") than the ", ifelse(..7 == "U", "upper", "lower"),
                                             " reference value (", ..8, "), which is considered impossible.")

                                    } else if(..6 %in% c("LayDate", "HatchDate", "FledgeDate",
                                                         "LayDate_observed", "HatchDate_observed", "FledgeDate_observed")) {

                                      paste0("Record on row ", ..1,
                                             " (PopID: ", ..2, "; ",
                                             "BroodID: ", ..3, "; ",
                                             species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                             " has ", ifelse(..7 == "U", "a later", "an earlier"), " value in ",
                                             ..6, " (", ..4, ") than the ", ifelse(..7 == "U", "upper", "lower"),
                                             " reference value (", ..8, "), which is considered impossible.")

                                    }

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Add messages about population-species combinations with low n to warning outputs & with no records for {var}
  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    if(exists("low_obs")) {

      skipped_output <- purrr::pmap(.l = list(low_obs$Species,
                                              low_obs$PopID),
                                    .f = ~{

                                      paste0("Number of records for ", ..2, ", ",
                                             species_codes[species_codes$Species == ..1, "CommonName"],
                                             ", is too low to create reliable reference values, so records are only checked for impossible/negative values.")

                                    })

      warning_output <- c(skipped_output, warning_output)

    }

    if(nrow(var_not_recorded) > 0) {

      not_recorded_output <- purrr::pmap(.l = list(var_not_recorded$Species,
                                                   var_not_recorded$PopID),
                                         .f = ~{

                                           paste0("This check was skipped for ", species_codes[species_codes$Species == ..1, "CommonName"], " in ", ..2,
                                                  " because ", var, " was not recorded.")

                                         })

      warning_output <- c(not_recorded_output, warning_output)

    }

  }

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)
  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- checkID_var <- NULL

}


#' Compare brood size with number of chicks in Individual_data
#'
#' Compare BroodSize in Brood_data with the number of chicks recorded in Individual_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks recorded will be flagged as a warning, because chicks might have died before ringing and measuring. In experimentally manipulated broods, BroodSize may be smaller than the number of chicks in Individual_data. If so, the record will be flagged as a warning. In non-manipulated broods, BroodSize should never be smaller than the number of chicks in Individual_data. If so, the records will be flagged as a potential error.
#'
#' Check ID: B6.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_broodsize_chicknumber <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B6" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B6: Comparing brood size and number of chicks in Individual_data...")

  } else {

    message("<< B6 is skipped >>")

  }

  if(skip_check == FALSE) {

    # Link BroodID from Individual_data to each capture in Capture_data
    chicks_captured <- Individual_data %>%
      dplyr::select("IndvID", "BroodIDLaid") %>%
      dplyr::group_by(.data$BroodIDLaid) %>%
      dplyr::summarise(Chicks = dplyr::n_distinct(.data$IndvID)) %>%
      dplyr::ungroup()

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select non-experimental records where number of chicks in Capture_data > brood size in Brood_data
    # (this should not be possible when no experimental manipulations have been done)
    # NB: allows v1.0 & v1.1 variable names of the standard format
    brood_err <- Brood_data %>%
      dplyr::left_join(chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
      {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  (is.na(.data$ExperimentID) | .data$ExperimentID == "") &.data$BroodSize < .data$Chicks)
        else dplyr::filter(., (is.na(.data$ExperimentID) | .data$ExperimentID == "") & .data$BroodSize_observed < .data$Chicks)} %>%
      {if("BroodSize" %in% colnames(.)) dplyr::select(., "Row", "PopID", "BroodID", "BroodSize", "Chicks")
        else dplyr::select(., "Row", "PopID", "BroodID", "BroodSize_observed", "Chicks")}

    # If potential errors, add to report
    if(nrow(brood_err) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_err %>%
        dplyr::mutate(CheckID = "B6") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a smaller BroodSize (", ..4, ")",
                                           " than the number of chicks in Individual_data (",
                                           ..5, ") but was not experimentally manipulated.")

                                  })

    }

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Select records where number of chicks in Capture_data < brood size in Brood_data
    # Yet, chicks might have died before measuring/ringing, or experimentally manipulated
    # AND select records where number of chicks in Capture_data > brood size in Brood_data
    # if broods are experimentally manipulated
    brood_war <- Brood_data %>%
      dplyr::left_join(chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
      {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  .data$BroodSize > .data$Chicks | ((!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$BroodSize < .data$Chicks))
        else dplyr::filter(., .data$BroodSize_observed > .data$Chicks | ((!is.na(.data$ExperimentID) | .data$ExperimentID != "") & .data$BroodSize_observed < .data$Chicks))} %>%
      {if("BroodSize" %in% colnames(.)) dplyr::select(., "Row", "PopID", "BroodID", "BroodSize", "Chicks")
        else dplyr::select(., "Row", "PopID", "BroodID", "BroodSize_observed", "Chicks")}

    # If warnings, add to report
    if(nrow(brood_war) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- brood_war %>%
        dplyr::mutate(CheckID = "B6") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      if(..4 < ..5) {

                                        paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                               " has a smaller BroodSize (", ..4, ")",
                                               " than the number of chicks in Individual_data (",
                                               ..5, "), and was experimentally manipulated.")

                                      } else if(..4 > ..5) {

                                        paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                               " has a larger BroodSize (", ..4, ")",
                                               " than the number of chicks in Individual_data (",
                                               ..5, ").")

                                      }

                                    })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

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
#' Check that the brood identifiers (BroodID) are unique within populations. Records with brood identifiers that are not unique within populations will be flaggea as a potential error.
#'
#' Check ID: B7.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_unique_BroodID <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B7" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B7: Checking that brood IDs are unique...")

  } else {

    message("<< B7 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select records that are duplicated within populations
    duplicated <- Brood_data %>%
      dplyr::group_by(.data$PopID, .data$BroodID) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()

    # If potential errors, add to report
    if(nrow(duplicated) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- duplicated %>%
        dplyr::mutate(CheckID = "B7") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
        dplyr::select("Row", "BroodID", "PopID")

      # Create quality check report statements
      error_output <- purrr::map(.x = unique(error_records$BroodID),
                                 .f = ~{

                                   paste0("Record on row ",
                                          # Duplicated rows
                                          error_records[error_records$BroodID == .x, "Row"][1,],
                                          " (PopID: ", error_records[error_records$BroodID == .x, "PopID"][1,], ")",
                                          " has the same BroodID (", .x, ") as row(s) ",
                                          # Duplicates (if 1, else more)
                                          ifelse(nrow(error_records[error_records$BroodID == .x, "Row"][-1,]) == 1,
                                                 error_records[error_records$BroodID == .x, "Row"][-1,],
                                                 gsub("^c\\(|\\)$", "",
                                                      error_records[error_records$BroodID == .x, "Row"][-1,])),
                                          ".")

                                 })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check clutch type order
#'
#' Check that the order of calculated clutch types per breeding female per season is correct; "first" should always come before "replacement" and "second".
#'
#' Check ID: B8.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_clutch_type_order <- function(Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B8" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B8: Checking that clutch type order is correct..")

  } else {

    message("<< B8 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select breeding females with ClutchType_calculated == "first" not as first clutch in a particular breeding season
    brood_err <- Brood_data %>%
      {if("LayDate" %in% colnames(.)) dplyr::arrange(., .data$LayDate)
        else dplyr::arrange(., .data$LayDate_observed)} %>%
      dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$ClutchType_calculated)) %>%
      dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$FemaleID) %>%
      dplyr::summarise(CTcal = ifelse(any(.data$ClutchType_calculated == "first"),
                                      which(.data$ClutchType_calculated == "first"), NA),
                       BroodID = .data$BroodID[.data$CTcal],
                       Row = .data$Row[.data$CTcal]) %>%
      dplyr::filter(!is.na(.data$CTcal) & .data$CTcal > 1) %>%
      dplyr::ungroup()

    # If potential errors, add to report
    if(nrow(brood_err) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- brood_err %>%
        dplyr::mutate(CheckID = "B8") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..6, " (PopID: ", ..1, "; BroodID: ", ..5, ")",
                                           " has ClutchType_calculated == 'first' but is not the first brood ",
                                           "recorded for that female (FemaleID: ", ..3, ") in that season (", ..2, ").")

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Compare species of parents
#'
#' Check that the parents of broods are of the same species. Common, biologically possible hybrid broods (e.g. FICHYP and FICALB) are flagged as a warning. Other combinations of species are flagged as a potential error.
#'
#' Check ID: B9.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_parents <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B9" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B9: Comparing species of mother and father...")

  } else {

    message("<< B9 is skipped >>")

  }

  if(skip_check == FALSE) {

    # Find species information of mothers
    females <- Brood_data %>%
      dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
      dplyr::select("Row", "PopID", "BroodID", "FemaleID") %>%
      dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("FemaleID" = "IndvID")) %>%
      dplyr::rename(FemaleSpecies = "Species")

    # Find species information of fathers
    males <- Brood_data %>%
      dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
      dplyr::select("Row", "PopID", "BroodID", "MaleID") %>%
      dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                       by=c("MaleID" = "IndvID")) %>%
      dplyr::rename(MaleSpecies = "Species")

    # Select records where parents are different species
    hybrid_broods <- dplyr::left_join(females, males,
                                      by=c("Row", "PopID", "BroodID")) %>%
      dplyr::filter(.data$FemaleSpecies != .data$MaleSpecies)

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Common cross-fostering and hybrids are considered "warnings"
    common_hybrid_broods <- hybrid_broods %>%
      dplyr::semi_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "MaleSpecies" = "Species2"))

    # If warnings, add to report
    if(nrow(common_hybrid_broods) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- common_hybrid_broods %>%
        dplyr::mutate(CheckID = "B9") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " has parents of different species",
                                             " (Mother: ", species_codes[species_codes$Species == ..5, "CommonName"],
                                             ", father: ", species_codes[species_codes$Species == ..7, "CommonName"],
                                             "), which are known to cross-foster/hybridize.")

                                    })

    }

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Uncommon hybrid broods (other than above) are considered "potential errors"
    uncommon_hybrid_broods <- hybrid_broods %>%
      dplyr::anti_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "MaleSpecies" = "Species2"))

    # If potential errors, add to report
    if(nrow(uncommon_hybrid_broods) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- uncommon_hybrid_broods %>%
        dplyr::mutate(CheckID = "B9") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has parents of different species",
                                           " (Mother: ", species_codes[species_codes$Species == ..5, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..7, "CommonName"],
                                           "), which do not commonly cross-foster/hybridize.")

                                  })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

}

#' Compare species of brood and species of parents
#'
#' Check that the species of broods is the same as the species of the parents of that brood. Common, biologically possible brood hybrids (e.g. FICHYP and FICALB) are flagged as a warning. Other combinations of species are flagged as a potential error.
#'
#' Check ID: B10.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_parents <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B10" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B10: Comparing species of brood and parents...")

  } else {

    message("<< B10 is skipped >>")

  }

  if(skip_check == FALSE) {

    # Find species information of mothers
    females <- Brood_data %>%
      dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
      dplyr::select("Row", "PopID", "BroodID",
                    "FemaleID", "BroodSpecies" = "Species") %>%
      dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                       by=c("FemaleID" = "IndvID")) %>%
      dplyr::rename("FemaleSpecies" = "Species")

    # Find species information of fathers
    males <- Brood_data %>%
      dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
      dplyr::select("Row", "PopID", "BroodID",
                    "MaleID", "BroodSpecies" = "Species") %>%
      dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                       by=c("MaleID" = "IndvID")) %>%
      dplyr::rename("MaleSpecies" = "Species")

    # Select records where parents are not of the same species as the brood
    females_males <- dplyr::left_join(females, males,
                                      by=c("Row", "PopID", "BroodID", "BroodSpecies"))

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Common cross-fostering and hybrids are considered "warnings"
    common_females <- females_males %>%
      dplyr::filter(.data$FemaleSpecies != .data$BroodSpecies) %>%
      dplyr::semi_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

    common_males <- females_males %>%
      dplyr::filter(.data$MaleSpecies != .data$BroodSpecies) %>%
      dplyr::semi_join(common_hybrids, by = c("MaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

    common_different_species_brood_parents <- dplyr::bind_rows(common_females, common_males) %>%
      dplyr::distinct()

    # If warnings, add to report
    if(nrow(common_different_species_brood_parents) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- common_different_species_brood_parents %>%
        dplyr::mutate(CheckID = "B10") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " is recorded as a different species than any of the parents",
                                             " (Mother: ", species_codes[species_codes$Species == ..6, "CommonName"],
                                             ", father: ", species_codes[species_codes$Species == ..8, "CommonName"],
                                             "). Species are known to cross-foster/hybridize.")

                                    })

    }

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Uncommon cross-fostering and hybrids are considered "potential errors"
    uncommon_females <-  females_males %>%
      dplyr::filter(.data$FemaleSpecies != .data$BroodSpecies) %>%
      dplyr::anti_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

    uncommon_males <-  females_males %>%
      dplyr::filter(.data$MaleSpecies != .data$BroodSpecies) %>%
      dplyr::anti_join(common_hybrids, by = c("MaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

    uncommon_different_species_brood_parents <- dplyr::bind_rows(uncommon_females, uncommon_males) %>%
      dplyr::distinct()

    # If potential errors, add to report
    if(nrow(uncommon_different_species_brood_parents) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- uncommon_different_species_brood_parents %>%
        dplyr::mutate(CheckID = "B10") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " is recorded as a different species than any of the parents",
                                           " (Mother: ", species_codes[species_codes$Species == ..6, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..8, "CommonName"],
                                           "). Species do not commonly cross-foster/hybridize.")

                                  })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- NULL

}

#' Compare species of brood and species of chicks
#'
#' Check that the species of broods is the same as species of the chicks in that brood. Common, biologically possible brood hybrids (e.g. FICHYP and FICALB) are flagged as a warning. Other combinations of species are flagged as a potential error.
#'
#' Check ID: B11.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_chicks <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B11" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B11: Comparing species of brood and chicks...")

  } else {

    message("<< B11 is skipped >>")

  }

  if(skip_check == FALSE) {

    individuals <- Individual_data %>%
      # Do not select individuals without BroodID and conflicted species
      # The latter is evaluated in check I5.
      dplyr::filter(!is.na(.data$BroodIDLaid) & (.data$Species != "CONFLICTED" | .data$Species != "CCCCCC")) %>%
      dplyr::select("IndvID", "IndvSpecies" = "Species", "BroodIDLaid", "PopID")

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Common cross-fostering and hybrids are considered "warnings"
    common_different_species_brood_chicks <- Brood_data %>%
      dplyr::left_join(individuals, by=c("BroodID" = "BroodIDLaid", "PopID")) %>%
      dplyr::mutate(SpeciesComp = .data$Species != .data$IndvSpecies) %>%
      dplyr::semi_join(common_hybrids, by = c("Species" = "Species1", "IndvSpecies" = "Species2")) %>%
      dplyr::group_by(.data$PopID, .data$BroodID, .data$Row) %>%
      dplyr::summarise(OtherSpeciesChicks = sum(.data$SpeciesComp),
                       Chicks = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::filter(.data$OtherSpeciesChicks > 0)

    # If warnings, add to report
    if(nrow(common_different_species_brood_chicks) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- common_different_species_brood_chicks %>%
        dplyr::mutate(CheckID = "B11") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..3, " (PopID: ", ..1, "; BroodID: ", ..2, ")",
                                             " is recorded as a different species than ", ..4, " out of ", ..5,
                                             " chicks, but species are known to cross-foster/hybridize.")

                                    })

    }

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Uncommon cross-fostering and hybrids are considered "potential errors"
    uncommon_different_species_brood_chicks <- Brood_data %>%
      dplyr::left_join(individuals, by=c("BroodID" = "BroodIDLaid", "PopID")) %>%
      dplyr::mutate(SpeciesComp = .data$Species != .data$IndvSpecies) %>%
      dplyr::anti_join(common_hybrids, by = c("Species" = "Species1", "IndvSpecies" = "Species2")) %>%
      dplyr::group_by(.data$PopID, .data$BroodID, .data$Row) %>%
      dplyr::summarise(OtherSpeciesChicks = sum(.data$SpeciesComp),
                       Chicks = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::filter(.data$OtherSpeciesChicks > 0)

    # If potential errors, add to report
    if(nrow(uncommon_different_species_brood_chicks) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- uncommon_different_species_brood_chicks %>%
        dplyr::mutate(CheckID = "B11") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..3, " (PopID: ", ..1, "; BroodID: ", ..2, ")",
                                           " is recorded as a different species than ", ..4, " out of ", ..5,
                                           " chicks, but species do not commonly cross-foster/hybridize.")

                                  })

    }

  }

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- NULL

}


#' Check sex of mothers
#'
#' Check that the individuals listed under FemaleID are female. Individuals with conflicted sex ("C") are ignored and checked in check I3 \code{\link{check_conflicting_sex}}.
#'
#' Check ID: B12.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_sex_mothers <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B12" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B12: Checking sex of mothers...")

  } else {

    message("<< B12 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select parents from Individual_data
    if("Sex" %in% colnames(Individual_data)) {

      parents <- Individual_data %>%
        dplyr::filter(.data$RingAge == "adult") %>%
        dplyr::select("IndvID", "PopID", "Sex")

    } else {# Use Sex_genetic if non_NA, otherwise Sex_calculated

      parents <- Individual_data %>%
        dplyr::filter(.data$RingAge == "adult") %>%
        dplyr::mutate(Sex = dplyr::case_when(is.na(.data$Sex_genetic) ~ .data$Sex_calculated,
                                             !is.na(.data$Sex_genetic) ~ .data$Sex_genetic)) %>%
        dplyr::select("IndvID", "PopID", "Sex")

    }

    # Males listed under FemaleID
    non_female_mothers <- Brood_data %>%
      dplyr::left_join(parents, by = c("PopID", "FemaleID" = "IndvID")) %>%
      dplyr::filter(.data$Sex == "M") %>%
      dplyr::select("Row", "BroodID", "PopID",
                    "FemaleID", "Sex") %>%
      dplyr::arrange(.data$Row)

    # If potential errors, add to report
    if(nrow(non_female_mothers) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- non_female_mothers %>%
        dplyr::mutate(CheckID = "B12") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..3,
                                           "; BroodID: ", ..2, ")",
                                           " lists a male individual (", ..4, ") under FemaleID.")

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check sex of fathers
#'
#' Check that the individuals listed under MaleID are male. Individuals with conflicted sex ("C") are ignored and checked in check I3 \code{\link{check_conflicting_sex}}.
#'
#' Check ID: B13.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_sex_fathers <- function(Brood_data, Individual_data, approved_list, output, skip) {

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B13" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B13: Checking sex of fathers...")

  } else {

    message("<< B13 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select parents from Individual_data
    if("Sex" %in% colnames(Individual_data)) {

      parents <- Individual_data %>%
        dplyr::filter(.data$RingAge == "adult") %>%
        dplyr::select("IndvID", "PopID", "Sex")

    } else {# Use Sex_genetic if non_NA, otherwise Sex_calculated

      parents <- Individual_data %>%
        dplyr::filter(.data$RingAge == "adult") %>%
        dplyr::mutate(Sex = dplyr::case_when(is.na(.data$Sex_genetic) ~ .data$Sex_calculated,
                                             !is.na(.data$Sex_genetic) ~ .data$Sex_genetic)) %>%
        dplyr::select("IndvID", "PopID", "Sex")

    }

    # Females listed under MaleID
    non_male_fathers <- Brood_data %>%
      dplyr::left_join(parents, by = c("PopID", "MaleID" = "IndvID")) %>%
      dplyr::filter(.data$Sex == "F") %>%
      dplyr::select("Row", "BroodID", "PopID",
                    "MaleID", "Sex") %>%
      dplyr::arrange(.data$Row)

    # If potential errors, add to report
    if(nrow(non_male_fathers) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- non_male_fathers %>%
        dplyr::mutate(CheckID = "B13") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..3,
                                           "; BroodID: ", ..2, ")",
                                           " lists a female individual (", ..4, ") under MaleID.")

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check that parents appear in Capture_data
#'
#' Check that all individuals recorded as parents in Brood_data appear at least once in Capture_data. Missing individuals will be flagged as a potential error.
#'
#' Check ID: B14.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_parents_captures <- function(Brood_data, Capture_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B14" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B14: Checking that parents appear in Capture_data...")

  } else {

    message("<< B14 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select parents that are missing from Capture_data
    missing_parents <- purrr::map(.x = unique(Brood_data$PopID),
                                  .f = ~{

                                    # Missing mothers
                                    missing_mothers <- dplyr::anti_join({Brood_data %>% dplyr::filter(!is.na(FemaleID) &.data$PopID == .x)},
                                                                        {Capture_data %>% dplyr::filter(.data$CapturePopID == .x)},
                                                                        by = c("FemaleID" = "IndvID", "PopID" = "CapturePopID")) %>%
                                      dplyr::select("Row", "PopID", "BroodID", "IndvID" = "FemaleID") %>%
                                      dplyr::mutate(Parent = "mother")

                                    # Missing fathers
                                    missing_fathers <- dplyr::anti_join({Brood_data %>% dplyr::filter(!is.na(MaleID) & .data$PopID == .x)},
                                                                        {Capture_data %>% dplyr::filter(.data$CapturePopID == .x)},
                                                                        by = c("MaleID" = "IndvID", "PopID" = "CapturePopID")) %>%
                                      dplyr::select("Row", "PopID", "BroodID", "IndvID" = "MaleID") %>%
                                      dplyr::mutate(Parent = "father")

                                    # Bind data frames, and combine info if both mother & father of the same brood are missing
                                    dplyr::bind_rows(missing_mothers, missing_fathers) %>%
                                      dplyr::arrange(.data$BroodID) %>%
                                      dplyr::mutate(Parent = dplyr::case_when(.data$BroodID %in% missing_mothers$BroodID & .data$BroodID %in% missing_fathers$BroodID ~ "both",
                                                                              TRUE ~ .data$Parent)) %>%
                                      dplyr::group_by(.data$Row, .data$PopID, .data$BroodID, .data$Parent) %>%
                                      dplyr::summarise(IndvID = paste(IndvID, collapse = ", "),
                                                       .groups = "drop")

                                  }) %>%
      dplyr::bind_rows()

    # If potential errors, add to report
    if(nrow(missing_parents) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- missing_parents %>%
        dplyr::mutate(CheckID = "B14") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    if(..4 != "both") {

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " has a ", ..4, " (IndvID: ", ..5, ") that does not appear in Capture_data.")

                                    } else {

                                      paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                             " has parents (IndvIDs: ", ..5, "), neither of which appears in Capture_data.")

                                    }

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check that all nest locations in Brood_data appear in Location_data
#'
#' Check that all nest locations recorded in Brood_data appear in Location_data. Missing locations will be flagged as a potential error.
#'
#' Check ID: B15.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#'
#' @export

check_brood_locations <- function(Brood_data, Location_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("B15" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("B15: Checking that nest locations appear in Location_data...")

  } else {

    message("<< B15 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select locations that are missing from Locations_data
    missing_locations <- purrr::map(.x = unique(Brood_data$PopID),
                                    .f = ~{

                                      dplyr::anti_join({Brood_data %>% dplyr::filter(!is.na(LocationID) & .data$PopID == .x)},
                                                       {Location_data %>% dplyr::filter(.data$PopID == .x)},
                                                       by = "LocationID")

                                    }) %>%
      dplyr::bind_rows() %>%
      dplyr::select("Row", "PopID", "BroodID", "LocationID")

    # If potential errors, add to report
    if(nrow(missing_locations) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- missing_locations %>%
        dplyr::mutate(CheckID = "B15") %>%
        dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a location (LocationID: ", ..4, ") that does not appear in Location_data.")

                                  })

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  # Count number of records flagged
  error_count <- sum(!is.na(error_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = NA_integer_,
                               ErrorRecords = error_count,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}
