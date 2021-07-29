#' Perform quality checks on brood data
#'
#' A wrapper that runs all single checks related to \code{Brood_data}.
#'
#' The following brood data checks are performed:
#' \itemize{
#' \item \strong{B1}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \item \strong{B2}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#' \item \strong{B3}: Compare lay date and hatching date per brood using \code{\link{compare_laying_hatching}}.
#' \item \strong{B4}: Compare hatching date and fledging date per brood using \code{\link{compare_hatching_fledging}}.
#' \item \strong{B5a-d}: Check brood variable values against reference values using \code{\link{check_values_brood}}. Brood variables checked: ClutchSize_observed, BroodSize_observed, NumberFledged_observed, LayDate_observed.
#' \item \strong{B6}: Compare brood size with number of chicks captured using \code{\link{compare_broodsize_chicknumber}}.
#' \item \strong{B7}: Check if the IDs of broods are unique using \code{\link{check_unique_BroodID}}.
#' \item \strong{B8}: Check if the order of clutch types for multiple breeding attempts per female per season is correct using \code{\link{check_clutch_type_order}}.
#' \item \strong{B9}: Check if parents of a brood are the same species using \code{\link{compare_species_parents}}.
#' \item \strong{B10}: Check if the brood and the parents of that brood are recorded as the same species using \code{\link{compare_species_brood_parents}}.
#' \item \strong{B11}: Check if the brood and the chicks in that brood are recorded as the same species using \code{\link{compare_species_brood_chicks}}.
#' \item \strong{B12}: Check if the sex of mothers listed under FemaleID are female using \code{\link{check_sex_mothers}}.
#' \item \strong{B13}: Check if the sex of fathers listed under MaleID are male using \code{\link{check_sex_fathers}}.
#' }
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#' @importFrom rlang sym `:=`
#' @importFrom progress progress_bar
#' @export

brood_check <- function(Brood_data, Individual_data, approved_list){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("B", c(1:5, paste0(6, letters[1:4]), 7:13)),
                               CheckDescription = c("Compare clutch and brood sizes",
                                                    "Compare brood sizes and fledgling numbers",
                                                    "Compare lay and hatch dates",
                                                    "Compare hatch and fledge dates",
                                                    "Check clutch size values against reference values",
                                                    "Check brood size values against reference values",
                                                    "Check fledgling number values against reference values",
                                                    "Check lay date values against reference values",
                                                    "Compare brood size with number of chicks captured",
                                                    "Check that brood IDs are unique",
                                                    "Check clutch type order",
                                                    "Check species of mother and father",
                                                    "Check species of brood and parents",
                                                    "Check species of brood and chicks",
                                                    "Check sex of mothers",
                                                    "Check sex of fathers"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Brood checks")

  # - Compare clutch and brood sizes
  message("B1: Comparing clutch and brood sizes...")

  compare_clutch_brood_output <- compare_clutch_brood(Brood_data, approved_list)

  check_list[1, 3:4] <- compare_clutch_brood_output$CheckList

  # - Compare brood sizes and fledgling numbers
  message("B2: Comparing brood sizes and fledgling numbers...")

  compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data, approved_list)

  check_list[2, 3:4] <- compare_brood_fledglings_output$CheckList

  # - Compare lay and hatch dates
  message("B3: Comparing lay and hatch dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data, approved_list)

  check_list[3, 3:4] <- compare_laying_hatching_output$CheckList

  # - Compare hatch and fledge dates
  message("B4: Comparing hatch and fledge dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data, approved_list)

  check_list[4, 3:4] <- compare_hatching_fledging_output$CheckList

  # - Check clutch size values against reference values
  message("B5a: Checking clutch size values against reference values...")

  var_ext <- ifelse("ClutchSize" %in% colnames(Brood_data),
                    ifelse(all(is.na(Brood_data$ClutchSize)), "_observed", ""), "_observed")

  check_values_clutch_size_output <- check_values_brood(Brood_data, paste0("ClutchSize", var_ext), approved_list)

  check_list[5, 3:4] <- check_values_clutch_size_output$CheckList

  # - Check brood size values against reference values
  message("B5b: Checking brood size values against reference values...")

  check_values_brood_size_output <- check_values_brood(Brood_data, paste0("BroodSize", var_ext), approved_list)

  check_list[6, 3:4] <- check_values_brood_size_output$CheckList

  # - Check fledgling number values against reference values
  message("B5c: Checking fledgling number values against reference values...")

  check_values_fledgling_number_output <- check_values_brood(Brood_data, paste0("NumberFledged", var_ext), approved_list)

  check_list[7, 3:4] <- check_values_fledgling_number_output$CheckList

  # - Check lay date values against reference values
  message("B5d: Checking lay date values against reference values...")

  check_values_lay_date_output <- check_values_brood(Brood_data, paste0("LayDate", var_ext), approved_list)

  check_list[8, 3:4] <- check_values_lay_date_output$CheckList

  # - Compare brood size and number of chicks captured
  message("B6: Comparing brood size and number of chicks captured...")

  compare_broodsize_chicknumber_output <- compare_broodsize_chicknumber(Brood_data, Individual_data, approved_list)

  check_list[9, 3:4] <- compare_broodsize_chicknumber_output$CheckList

  # - Check that BroodIDs are unique
  message("B7: Checking that brood IDs are unique...")

  check_unique_BroodID_output <- check_unique_BroodID(Brood_data, approved_list)

  check_list[10, 3:4] <- check_unique_BroodID_output$CheckList

  # - Check clutch type order
  message("B8: Checking that clutch type order is correct..")

  check_clutch_type_order_output <- check_clutch_type_order(Brood_data, approved_list)

  check_list[11, 3:4] <- check_clutch_type_order_output$CheckList

  # - Compare species of mother and father
  message("B9: Comparing species of mother and father...")

  compare_species_parents_output <- compare_species_parents(Brood_data, Individual_data, approved_list)

  check_list[12, 3:4] <- compare_species_parents_output$CheckList

  # - Compare species of brood and parents
  message("B10: Comparing species of brood and parents...")

  compare_species_brood_parents_output <- compare_species_brood_parents(Brood_data, Individual_data, approved_list)

  check_list[13, 3:4] <- compare_species_brood_parents_output$CheckList

  # - Compare species of brood and chicks
  message("B11: Comparing species of brood and chicks...")

  compare_species_brood_chicks_output <- compare_species_brood_chicks(Brood_data, Individual_data, approved_list)

  check_list[14, 3:4] <- compare_species_brood_chicks_output$CheckList

  # - Check sex of mothers
  message("B12: Checking sex of mothers...")

  check_sex_mothers_output <- check_sex_mothers(Brood_data, Individual_data, approved_list)

  check_list[15, 3:4] <- check_sex_mothers_output$CheckList

  # - Check sex of fathers
  message("B13: Checking sex of fathers...")

  check_sex_fathers_output <- check_sex_fathers(Brood_data, Individual_data, approved_list)

  check_list[16, 3:4] <- check_sex_fathers_output$CheckList


  # Warning list
  warning_list <- list(Check1 = compare_clutch_brood_output$WarningOutput,
                       Check2 = compare_brood_fledglings_output$WarningOutput,
                       Check3 = compare_laying_hatching_output$WarningOutput,
                       Check4 = compare_hatching_fledging_output$WarningOutput,
                       Check5a = check_values_clutch_size_output$WarningOutput,
                       Check5b = check_values_brood_size_output$WarningOutput,
                       Check5c = check_values_fledgling_number_output$WarningOutput,
                       Check5d = check_values_lay_date_output$WarningOutput,
                       Check6 = compare_broodsize_chicknumber_output$WarningOutput,
                       Check7 = check_unique_BroodID_output$WarningOutput,
                       Check8 = check_clutch_type_order_output$WarningOutput,
                       Check9 = compare_species_parents_output$WarningOutput,
                       Check10 = compare_species_brood_parents_output$WarningOutput,
                       Check11 = compare_species_brood_chicks_output$WarningOutput,
                       Check12 = check_sex_mothers_output$WarningOutput,
                       Check13 = check_sex_fathers_output$WarningOutput)

  # Error list
  error_list <- list(Check1 = compare_clutch_brood_output$ErrorOutput,
                     Check2 = compare_brood_fledglings_output$ErrorOutput,
                     Check3 = compare_laying_hatching_output$ErrorOutput,
                     Check4 = compare_hatching_fledging_output$ErrorOutput,
                     Check5a = check_values_clutch_size_output$ErrorOutput,
                     Check5b = check_values_brood_size_output$ErrorOutput,
                     Check5c = check_values_fledgling_number_output$ErrorOutput,
                     Check5d = check_values_lay_date_output$ErrorOutput,
                     Check6 = compare_broodsize_chicknumber_output$ErrorOutput,
                     Check7 = check_unique_BroodID_output$ErrorOutput,
                     Check8 = check_clutch_type_order_output$ErrorOutput,
                     Check9 = compare_species_parents_output$ErrorOutput,
                     Check10 = compare_species_brood_parents_output$ErrorOutput,
                     Check11 = compare_species_brood_chicks_output$ErrorOutput,
                     Check12 = check_sex_mothers_output$ErrorOutput,
                     Check13 = check_sex_fathers_output$ErrorOutput)

  return(list(CheckList = check_list,
              WarningRows = unique(c(compare_clutch_brood_output$WarningRows,
                                     compare_brood_fledglings_output$WarningRows,
                                     compare_laying_hatching_output$WarningRows,
                                     compare_hatching_fledging_output$WarningRows,
                                     check_values_clutch_size_output$WarningRows,
                                     check_values_brood_size_output$WarningRows,
                                     check_values_fledgling_number_output$WarningRows,
                                     check_values_lay_date_output$WarningRows,
                                     compare_broodsize_chicknumber_output$WarningRows,
                                     check_unique_BroodID_output$WarningRows,
                                     check_clutch_type_order_output$WarningRows,
                                     compare_species_parents_output$WarningRows,
                                     compare_species_brood_parents_output$WarningRows,
                                     compare_species_brood_chicks_output$WarningRows,
                                     check_sex_mothers_output$WarningRows,
                                     check_sex_fathers_output$WarningRows)),
              ErrorRows = unique(c(compare_clutch_brood_output$ErrorRows,
                                   compare_brood_fledglings_output$ErrorRows,
                                   compare_laying_hatching_output$ErrorRows,
                                   compare_hatching_fledging_output$ErrorRows,
                                   check_values_clutch_size_output$ErrorRows,
                                   check_values_brood_size_output$ErrorRows,
                                   check_values_fledgling_number_output$ErrorRows,
                                   check_values_lay_date_output$ErrorRows,
                                   compare_broodsize_chicknumber_output$ErrorRows,
                                   check_unique_BroodID_output$ErrorRows,
                                   check_clutch_type_order_output$ErrorRows,
                                   compare_species_parents_output$ErrorRows,
                                   compare_species_brood_parents_output$ErrorRows,
                                   compare_species_brood_chicks_output$ErrorRows,
                                   check_sex_mothers_output$ErrorRows,
                                   check_sex_fathers_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will result in an error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will result in a warning.
#'
#' Check ID: B1.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_clutch_brood <- function(Brood_data, approved_list){

  # Non-manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_non <- Brood_data %>%
    {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., is.na(.data$ExperimentID) & .data$ClutchSize < .data$BroodSize) else dplyr::filter(., is.na(.data$ExperimentID) & .data$ClutchSize_observed < .data$BroodSize_observed)}

  # Manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_man <- Brood_data %>%
    {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., !is.na(.data$ExperimentID) & .data$ClutchSize < .data$BroodSize) else dplyr::filter(., !is.na(.data$ExperimentID) & .data$ClutchSize_observed < .data$BroodSize_observed)}

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_data_non) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_data_non %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$ClutchSize, .data$BroodSize)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$ClutchSize_observed, .data$BroodSize_observed)}

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a larger brood size (", ..5,
                                         ") than clutch size (", ..4,
                                         "), but was not experimentally manipulated.")

                                })

  }

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(brood_data_man) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- brood_data_man %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$ClutchSize, .data$BroodSize)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$ClutchSize_observed, .data$BroodSize_observed)}

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a larger brood size (", ..5,
                                           ") than clutch size (", ..4,
                                           "), but was experimentally manipulated.")

                                  })

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)


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
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' Check ID: B2.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_brood_fledglings <- function(Brood_data, approved_list){

  # Non-manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_non <- Brood_data %>%
    {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., is.na(.data$ExperimentID) & .data$BroodSize < .data$NumberFledged) else dplyr::filter(., is.na(.data$ExperimentID) & .data$BroodSize_observed < .data$NumberFledged_observed)}

  # Manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_man <- Brood_data %>%
    {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., !is.na(.data$ExperimentID) & .data$BroodSize < .data$NumberFledged) else dplyr::filter(., !is.na(.data$ExperimentID) & .data$BroodSize_observed < .data$NumberFledged_observed)}

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_data_non) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_data_non %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize, .data$NumberFledged)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize_observed, .data$NumberFledged_observed)}

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a larger fledgling number (", ..5,
                                         ") than brood size (", ..4,
                                         "), but was not experimentally manipulated.")

                                })

  }

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(brood_data_man) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- brood_data_man %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize, .data$NumberFledged)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize_observed, .data$NumberFledged_observed)}

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a larger fledgling number (", ..5,
                                           ") than brood size (", ..4,
                                           "), and was experimentally manipulated.")

                                  })

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

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
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' Check ID: B3.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_laying_hatching <- function(Brood_data, approved_list){

  # Broods with laying date later than hatching date
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_late <- Brood_data %>%
      {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::filter(., .data$LayDate >= .data$HatchDate)
        else dplyr::filter(., .data$LayDate_observed >= .data$HatchDate_observed)}

  # TODO
  # Broods with laying date earlier than hatching date but the difference
  # in number of days is smaller than incubation time
  ## INCUBATION TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(LayDate < HatchDate & (HatchDate-LayDate) >= )

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_data_late) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_data_late %>%
      dplyr::mutate(CheckID = "B4") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$LayDate, .data$HatchDate)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$LayDate_observed, .data$HatchDate_observed)}

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a later laying date (", ..4,
                                         ") than hatching date (", ..5, ").")

                                })

  }

  # Warnings
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

compare_hatching_fledging <- function(Brood_data, approved_list){

  # Broods with laying date later than hatching date
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_data_late <- Brood_data %>%
    {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::filter(., .data$HatchDate >= .data$FledgeDate)
      else dplyr::filter(., .data$HatchDate_observed >= .data$FledgeDate_observed)}

  # TODO
  # Broods with hatching date earlier than fledging date but the difference
  # in number of days is smaller than breeding time
  ## BREEDING TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(HatchDate < FledgeDate & (FledgeDate-HatchDate) >= )

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_data_late) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_data_late %>%
      dplyr::mutate(CheckID = "B5") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$HatchDate, .data$FledgeDate)
        else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$HatchDate_observed, .data$FledgeDate_observed)}

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a later hatching date (", ..4,
                                         ") than fledging date (", ..5, ").")

                                })

  }

  # Warnings
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
#' \item{\emph{n >= 100}\cr}{Records are considered unusual if they are larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as an error.}
#' }
#'
#' \strong{LayDate_observed} \cr
#' Check ID: B5d \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered unusual if they are earlier than the 1st percentile or later than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are earlier than January 1st or later than December 31st of the current breeding season, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered impossible if they are earlier than January 1st or later than December 31st of the current breeding season, and will be flagged as an error.}
#' }
#'
#' @inheritParams checks_brood_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_brood <- function(Brood_data, var, approved_list) {

  # Stop if "var" is missing
  if(missing(var)) {

    stop("Please select a variable in Brood_data to check against reference values.")

  }

  # Create reference values from data
  # Numeric & integer columns
  if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(.data$Species)) %>%
      dplyr::group_by(.data$Species, .data$PopID) %>%
      dplyr::summarise(Warning_min = NA,
                       Warning_max = ceiling(stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE)),
                       Error_min = 0,
                       Error_max = 4 * .data$Warning_max,
                       n = n()) %>%
      dplyr::arrange(.data$PopID, .data$Species)

    # Date columns
  } else if(var %in% c("LayDate", "LayDate_observed")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(.data$Species)) %>%
      dplyr::group_by(.data$BreedingSeason) %>%
      # Transform dates to Julian days (while accounting for year) to calculate quantiles
      dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$Species, .data$PopID) %>%
      dplyr::summarise(Warning_min = floor(stats::quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.01, na.rm = TRUE)),
                       Warning_max = ceiling(stats::quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.99, na.rm = TRUE)),
                       Error_min = 1,
                       Error_max = 366,
                       n = n()) %>%
      dplyr::arrange(.data$PopID, .data$Species)

  }

  # Print message for population-species combinations with too low number of observations
  if(any(ref$n < 100)) {

    low_obs <- ref %>%
      dplyr::filter(.data$n < 100) %>%
      dplyr::select(.data$Species, .data$PopID)

      purrr::pwalk(.l = list(low_obs$Species,
                             low_obs$PopID,
                             rep(var, nrow(low_obs))),
                   .f = ~{

                     message(paste0("Number of ", ..3, " records for ", ..2, ": ", ..1,
                                    " is too low (< 100) to create reliable reference values."))

                   })

  }

  # Progress bar
  pb <- progress::progress_bar$new(total = 2*nrow(ref),
                                   format = "[:bar] :percent ~:eta remaining",
                                   clear = FALSE)

  # Brood-specific errors
  brood_err <- purrr::pmap(.l = ref,
                           .f = ~{

                             pb$tick()


                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                           "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                               # If number of observations is large enough, compare brood values
                               # to all reference values
                               if(..7 >= 100) {

                                 # Brood records below lower error threshold
                                 lower_err <- Brood_data %>%
                                   dplyr::mutate(Variable = var,
                                                 Threshold = "L",
                                                 Ref = ..5) %>%
                                   dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) < ..5) %>%
                                   dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var), .data$Species, .data$Variable, .data$Threshold, .data$Ref)


                                 # Brood records above upper error threshold
                                 upper_err <- Brood_data %>%
                                   dplyr::mutate(Variable = var,
                                                 Threshold = "U",
                                                 Ref = ..6) %>%
                                   dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) > ..6) %>%
                                   dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var), .data$Species, .data$Variable, .data$Threshold, .data$Ref)


                                 dplyr::bind_rows(lower_err, upper_err)

                                 # If number of observations is too low, only compare brood values
                                 # to reference values not based on quantiles
                               } else {

                                 # Brood records below lower error threshold
                                 Brood_data %>%
                                   dplyr::mutate(Variable = var,
                                                 Threshold = "L",
                                                 Ref = ..5) %>%
                                   dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(var) < ..5) %>%
                                   dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var), .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                               }

                             } else if(var %in% c("LayDate", "LayDate_observed")) {

                               # Brood records below lower error threshold
                               lower_err <- Brood_data %>%
                                 dplyr::group_by(.data$BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(Variable = var,
                                               Threshold = "L",
                                               Ref = paste(.data$BreedingSeason, "01", "01", sep = "-")) %>%
                                 dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) < ..5) %>%
                                 dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var),
                                               .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                               # Brood records above upper error threshold
                               upper_err <- Brood_data %>%
                                 dplyr::group_by(.data$BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(Variable = var,
                                               Threshold = "U",
                                               Ref = paste(.data$BreedingSeason, "12", "31", sep = "-")) %>%
                                 dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) > ..6) %>%
                                 dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var),
                                               .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                               dplyr::bind_rows(lower_err, upper_err)

                             }

                           }) %>%
    dplyr::bind_rows()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

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

                                  } else if(..6 %in% c("LayDate", "LayDate_observed")) {

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


  # Brood-specific warnings
  # Warnings are only checked for population-species combinations with at least 100 observations
  warning_ref <- ref %>%
    dplyr::filter(.data$n >= 100)

  brood_war <- purrr::pmap(.l = warning_ref,
                           .f = ~{

                             #pb$tick()

                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                           "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                               # Brood records above upper warning threshold
                               Brood_data %>%
                                 dplyr::mutate(Variable = var,
                                               Threshold = "U",
                                               Ref = ..4) %>%
                                 dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 &
                                                 (!!rlang::sym(var) > ..4 & !!rlang::sym(var) <= ..6)) %>%
                                 dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var), .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                             } else if(var %in% c("LayDate", "LayDate_observed")) {

                               # Brood records below lower warning threshold
                               lower_war <- Brood_data %>%
                                 dplyr::group_by(.data$BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(Variable = var,
                                               Threshold = "L",
                                               Ref = lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + ..3 - 1) %>%
                                 dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) < ..3 & !!rlang::sym(paste0(var, "_julian")) >= ..5) %>%
                                 dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var),
                                               .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                               # Brood records above upper warning threshold
                               upper_war <- Brood_data %>%
                                 dplyr::group_by(.data$BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::mutate(Variable = var,
                                               Threshold = "U",
                                               Ref = lubridate::ymd(paste(.data$BreedingSeason, "1", "1", sep = "-")) + ..4 - 1) %>%
                                 dplyr::filter(.data$Species == ..1 & .data$PopID == ..2 & !!rlang::sym(paste0(var, "_julian")) > ..4 & !!rlang::sym(paste0(var, "_julian")) <= ..6) %>%
                                 dplyr::select(.data$Row, .data$PopID, .data$BroodID, !!rlang::sym(var),
                                               .data$Species, .data$Variable, .data$Threshold, .data$Ref)

                               dplyr::bind_rows(lower_war, upper_war)

                             }

                           }) %>%
    dplyr::bind_rows()

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(brood_war) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- brood_war %>%
      dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::arrange(.data$Row)

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    if(..6 %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                                  "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                                      paste0("Record on row ", ..1,
                                             " (PopID: ", ..2, "; ",
                                             "BroodID: ", ..3, "; ",
                                             species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                             " has a ", ifelse(..7 == "U", "larger", "smaller"), " value in ",
                                             ..6, " (", ..4, ") than the ", ifelse(..7 == "U", "upper", "lower"),
                                             " reference value (", ..8, "), which is considered unusual.")

                                    } else if(..6 %in% c("LayDate", "LayDate_observed")) {

                                      paste0("Record on row ", ..1,
                                             " (PopID: ", ..2, "; ",
                                             "BroodID: ", ..3, "; ",
                                             species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                             " has ", ifelse(..7 == "U", "a later", "an earlier"), " value in ",
                                             ..6, " (", ..4, ") than the ", ifelse(..7 == "U", "upper", "lower"),
                                             " reference value (", ..8, "), which is considered unusual.")

                                    }

                                  })

  }

  # Add messages about population-species combinations with low n to warning outputs
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

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  approved_list <- checkID_var <- NULL

}


#' Compare brood size with number of chicks captured
#'
#' Compare BroodSize in Brood_data with the number of chicks captured in Capture_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks captured results in a warning, because chicks might have died before ringing and measuring. Records where BroodSize is smaller than the number of chicks captured results in an error, because this should not be possible.
#'
#' Check ID: B6.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_broodsize_chicknumber <- function(Brood_data, Individual_data, approved_list) {

  # Link BroodID from Individual_data to each capture in Capture_data
  chicks_captured <- Individual_data %>%
    dplyr::select(.data$IndvID, .data$BroodIDLaid) %>%
    dplyr::group_by(.data$BroodIDLaid) %>%
    dplyr::summarise(Chicks = n_distinct(.data$IndvID)) %>%
    dplyr::ungroup()

  # Errors
  # Select records where number of chicks in Capture_data > brood size in Brood_data
  # (this should not be possible)
  # NB: allows v1.0 & v1.1 variable names of the standard format
  brood_err <- Brood_data %>%
    dplyr::left_join(chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  .data$BroodSize < .data$Chicks)
      else dplyr::filter(., .data$BroodSize_observed < .data$Chicks)} %>%
    {if("BroodSize" %in% colnames(.)) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize, .data$Chicks)
      else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize_observed, .data$Chicks)}

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_err) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_err %>%
      dplyr::mutate(CheckID = "B7") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has a smaller BroodSize (", ..4, ")",
                                         " than the number of chicks in Individual_data (",
                                         ..5, ").")

                                })

  }

  # Warnings
  # Select records where number of chicks in Capture_data < brood size in Brood_data
  # (chicks might have died before measuring/ringing)
  brood_war <- Brood_data %>%
    dplyr::left_join(chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  .data$BroodSize > .data$Chicks)
      else dplyr::filter(., .data$BroodSize_observed > .data$Chicks)} %>%
    {if("BroodSize" %in% colnames(.)) dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize, .data$Chicks)
      else dplyr::select(., .data$Row, .data$PopID, .data$BroodID, .data$BroodSize_observed, .data$Chicks)}

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(brood_war) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- brood_war %>%
      dplyr::mutate(CheckID = "B8") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has a larger BroodSize (", ..4, ")",
                                           " than the number of chicks in Individual_data (",
                                           ..5, ").")

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


#' Check unique brood identifiers
#'
#' Check that the brood identifiers (BroodID) are unique within populations. Records with brood identifiers that are not unique within populations will result in an error.
#'
#' Check ID: B7.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_unique_BroodID <- function(Brood_data, approved_list){

  # Errors
  # Select records that are duplicated within populations
  duplicated <- Brood_data %>%
    dplyr::group_by(.data$PopID, .data$BroodID) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(duplicated) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- duplicated %>%
      dplyr::mutate(CheckID = "B8") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::select(.data$Row, .data$BroodID, .data$PopID)

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

check_clutch_type_order <- function(Brood_data, approved_list){

  # Select breeding females with ClutchType_calculated == "first" not as first clutch in a particular breeding season
  brood_err <- Brood_data %>%
    dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$ClutchType_calculated)) %>%
    dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$FemaleID) %>%
    dplyr::summarise(CTcal = ifelse(any(.data$ClutchType_calculated == "first"),
                                    which(.data$ClutchType_calculated == "first"), NA),
                     BroodID = .data$BroodID[.data$CTcal],
                     Row = .data$Row[.data$CTcal]) %>%
    dplyr::filter(!is.na(.data$CTcal) & .data$CTcal > 1) %>%
    dplyr::ungroup()

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(brood_err) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- brood_err %>%
      dplyr::mutate(CheckID = "B9") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..6, " (PopID: ", ..1, "; BroodID: ", ..5, ")",
                                         " has ClutchType_calculated == 'first', but is not the first brood recorded for that female (FemaleID: ", ..3, ") in that season (", ..2, ").")

                                }

    )

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


#' Compare species of parents
#'
#' Check that the parents of broods are of the same species. Common, biologically possible hybrid broods (e.g. FICHYP and FICALB) are flagged as ‘warning’. Other combinations of species are flagged as ‘potential error’.
#'
#' Check ID: B9.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_parents <- function(Brood_data, Individual_data, approved_list) {

  # Find species information of mothers
  females <- Brood_data %>%
    dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
    dplyr::select(.data$Row, .data$PopID, .data$BroodID, .data$FemaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename(FemaleSpecies = .data$Species)

  # Find species information of fathers
  males <- Brood_data %>%
    dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
    dplyr::select(.data$Row, .data$PopID, .data$BroodID, .data$MaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("MaleID" = "IndvID")) %>%
    dplyr::rename(MaleSpecies = .data$Species)

  # Select records where parents are different species
  hybrid_broods <- dplyr::left_join(females, males,
                                           by=c("Row", "PopID", "BroodID")) %>%
    dplyr::filter(.data$FemaleSpecies != .data$MaleSpecies)

  # Warnings
  # Common cross-fostering and hybrids are considered "warnings"
  common_hybrid_broods <- hybrid_broods %>%
    dplyr::semi_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "MaleSpecies" = "Species2"))

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(common_hybrid_broods) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- common_hybrid_broods %>%
      dplyr::mutate(CheckID = "B10") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has parents of different species",
                                           " (Mother: ", species_codes[species_codes$Species == ..5, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..7, "CommonName"], "), which are known to cross-foster/hybridize.")

                                  })

  }


  # Errors
  # Uncommon hybrid broods (other than above) are considered "errors"
  uncommon_hybrid_broods <- hybrid_broods %>%
    dplyr::anti_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "MaleSpecies" = "Species2"))

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(uncommon_hybrid_broods) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- uncommon_hybrid_broods %>%
      dplyr::mutate(CheckID = "B10") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " has parents of different species",
                                         " (Mother: ", species_codes[species_codes$Species == ..5, "CommonName"],
                                         ", father: ", species_codes[species_codes$Species == ..7, "CommonName"], "), which do not commonly cross-foster/hybridize.")

                                })

  }


  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = error_records$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

}

#' Compare species of brood and species of parents
#'
#' Check that the species of broods is the same as the species of the parents of that brood. Common, biologically possible brood hybrids (e.g. FICHYP and FICALB) are flagged as ‘warning’. Other combinations of species are flagged as ‘potential error’.
#'
#' Check ID: B10.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_parents <- function(Brood_data, Individual_data, approved_list) {

  # Find species information of mothers
  females <- Brood_data %>%
    dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
    dplyr::select(.data$Row, .data$PopID, .data$BroodID,
                  .data$FemaleID, "BroodSpecies" = .data$Species) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename("FemaleSpecies" = .data$Species)

  # Find species information of fathers
  males <- Brood_data %>%
    dplyr::filter(!is.na(.data$FemaleID) & !is.na(.data$MaleID)) %>%
    dplyr::select(.data$Row, .data$PopID, .data$BroodID,
                  .data$MaleID, "BroodSpecies" = .data$Species) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("MaleID" = "IndvID")) %>%
    dplyr::rename("MaleSpecies" = .data$Species)

  # Select records where parents are not of the same species as the brood
  females_males <- dplyr::left_join(females, males,
                                    by=c("Row", "PopID", "BroodID", "BroodSpecies"))

  # Warnings
  # Common cross-fostering and hybrids are considered "warnings"
  common_females <- females_males %>%
    dplyr::filter(.data$FemaleSpecies != .data$BroodSpecies) %>%
    dplyr::semi_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

  common_males <- females_males %>%
    dplyr::filter(.data$MaleSpecies != .data$BroodSpecies) %>%
    dplyr::semi_join(common_hybrids, by = c("MaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

  common_different_species_brood_parents <- dplyr::bind_rows(common_females, common_males) %>%
    dplyr::distinct()

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(common_different_species_brood_parents) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- common_different_species_brood_parents %>%
      dplyr::mutate(CheckID = "B11") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " is recorded as a different species than any of the parents",
                                           " (Mother: ", species_codes[species_codes$Species == ..6, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..8, "CommonName"], "). Species are known to cross-foster/hybridize.")

                                  })

  }

  # Errors
  # Uncommon cross-fostering and hybrids are considered "errors"
  uncommon_females <-  females_males %>%
    dplyr::filter(.data$FemaleSpecies != .data$BroodSpecies) %>%
    dplyr::anti_join(common_hybrids, by = c("FemaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

  uncommon_males <-  females_males %>%
    dplyr::filter(.data$MaleSpecies != .data$BroodSpecies) %>%
    dplyr::anti_join(common_hybrids, by = c("MaleSpecies" = "Species1", "BroodSpecies" = "Species2"))

  uncommon_different_species_brood_parents <- dplyr::bind_rows(uncommon_females, uncommon_males) %>%
    dplyr::distinct()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(uncommon_different_species_brood_parents) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- uncommon_different_species_brood_parents %>%
      dplyr::mutate(CheckID = "B11") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                         " is recorded as a different species than any of the parents",
                                         " (Mother: ", species_codes[species_codes$Species == ..6, "CommonName"],
                                         ", father: ", species_codes[species_codes$Species == ..8, "CommonName"], "). Species do not commonly cross-foster/hybridize.")

                                })

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

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
#' Check that the species of broods is the same as species of the chicks in that brood. Common, biologically possible brood hybrids (e.g. FICHYP and FICALB) are flagged as ‘warning’. Other combinations of species are flagged as ‘potential error’.
#'
#' Check ID: B11.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_chicks <- function(Brood_data, Individual_data, approved_list) {

  individuals <- Individual_data %>%
    # Do not select individuals without BroodID and conflicted species
    # The latter is evaluated in check I5.
    dplyr::filter(!is.na(.data$BroodIDLaid) & (.data$Species != "CONFLICTED" | .data$Species != "CCCCCC")) %>%
    dplyr::select(.data$IndvID, "IndvSpecies" = .data$Species, .data$BroodIDLaid, .data$PopID)

  # Warnings
  # Common cross-fostering and hybrids are considered "warnings"
  common_different_species_brood_chicks <- Brood_data %>%
    dplyr::left_join(individuals, by=c("BroodID" = "BroodIDLaid", "PopID")) %>%
    dplyr::mutate(SpeciesComp = .data$Species != .data$IndvSpecies) %>%
    dplyr::semi_join(common_hybrids, by = c("Species" = "Species1", "IndvSpecies" = "Species2")) %>%
    dplyr::group_by(.data$PopID, .data$BroodID, .data$Row) %>%
    dplyr::summarise(OtherSpeciesChicks = sum(.data$SpeciesComp),
                     Chicks = n(),
                     .groups = "drop") %>%
    dplyr::filter(.data$OtherSpeciesChicks > 0)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(common_different_species_brood_chicks) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- common_different_species_brood_chicks %>%
      dplyr::mutate(CheckID = "B12") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..3, " (PopID: ", ..1, "; BroodID: ", ..2, ")",
                                           " is recorded as a different species than ", ..4, " out of ", ..5, " chicks, but species are known to cross-foster/hybridize.")

                                  })

  }

  # Errors
  # Uncommon cross-fostering and hybrids are considered "warnings"
  uncommon_different_species_brood_chicks <- Brood_data %>%
    dplyr::left_join(individuals, by=c("BroodID" = "BroodIDLaid", "PopID")) %>%
    dplyr::mutate(SpeciesComp = .data$Species != .data$IndvSpecies) %>%
    dplyr::anti_join(common_hybrids, by = c("Species" = "Species1", "IndvSpecies" = "Species2")) %>%
    dplyr::group_by(.data$PopID, .data$BroodID, .data$Row) %>%
    dplyr::summarise(OtherSpeciesChicks = sum(.data$SpeciesComp),
                     Chicks = n(),
                     .groups = "drop") %>%
    dplyr::filter(.data$OtherSpeciesChicks > 0)

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(uncommon_different_species_brood_chicks) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- uncommon_different_species_brood_chicks %>%
      dplyr::mutate(CheckID = "B12") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..3, " (PopID: ", ..1, "; BroodID: ", ..2, ")",
                                         " is recorded as a different species than ", ..4, " out of ", ..5, " chicks, but species do not commonly cross-foster/hybridize.")

                                })

  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

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

check_sex_mothers <- function(Brood_data, Individual_data, approved_list) {

  # Select parents from Individual_data

  if("Sex" %in% colnames(Individual_data)) {

    parents <- Individual_data %>%
      dplyr::filter(.data$RingAge == "adult") %>%
      dplyr::select(.data$IndvID, .data$PopID, .data$Sex)

  } else {# Use Sex_genetic if non_NA, otherwise Sex_calculated

    parents <- Individual_data %>%
      dplyr::filter(.data$RingAge == "adult") %>%
      dplyr::mutate(Sex = dplyr::case_when(is.na(.data$Sex_genetic) ~ .data$Sex_calculated,
                                           !is.na(.data$Sex_genetic) ~ .data$Sex_genetic)) %>%
      dplyr::select(.data$IndvID, .data$PopID, .data$Sex)

  }

  # Males listed under FemaleID
  non_female_mothers <- Brood_data %>%
    dplyr::left_join(parents, by = c("PopID", "FemaleID" = "IndvID")) %>%
    dplyr::filter(.data$Sex == "M") %>%
    dplyr::select(.data$Row, .data$BroodID, .data$PopID,
                  .data$FemaleID, .data$Sex) %>%
    dplyr::arrange(.data$Row)

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(non_female_mothers) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- non_female_mothers %>%
      dplyr::mutate(CheckID = "B13") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..3,
                                         "; BroodID: ", ..2, ")",
                                         " lists a male individual (", ..4, ") under FemaleID.")

                                }
    )

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

check_sex_fathers <- function(Brood_data, Individual_data, approved_list) {

  # Select parents from Individual_data

  if("Sex" %in% colnames(Individual_data)) {

    parents <- Individual_data %>%
      dplyr::filter(.data$RingAge == "adult") %>%
      dplyr::select(.data$IndvID, .data$PopID, .data$Sex)

  } else {# Use Sex_genetic if non_NA, otherwise Sex_calculated

    parents <- Individual_data %>%
      dplyr::filter(.data$RingAge == "adult") %>%
      dplyr::mutate(Sex = dplyr::case_when(is.na(.data$Sex_genetic) ~ .data$Sex_calculated,
                                           !is.na(.data$Sex_genetic) ~ .data$Sex_genetic)) %>%
      dplyr::select(.data$IndvID, .data$PopID, .data$Sex)

  }

  # Females listed under MaleID
  non_male_fathers <- Brood_data %>%
    dplyr::left_join(parents, by = c("PopID", "MaleID" = "IndvID")) %>%
    dplyr::filter(.data$Sex == "F") %>%
    dplyr::select(.data$Row, .data$BroodID, .data$PopID,
                  .data$MaleID, .data$Sex) %>%
    dplyr::arrange(.data$Row)

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(non_male_fathers) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- non_male_fathers %>%
      dplyr::mutate(CheckID = "B14") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1, " (PopID: ", ..3,
                                         "; BroodID: ", ..2, ")",
                                         " lists a female individual (", ..4, ") under MaleID.")

                                }
    )

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
