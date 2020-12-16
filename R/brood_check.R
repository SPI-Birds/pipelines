#' Perform quality checks on brood data
#'
#' A wrapper that runs all single checks related to \code{Brood_data}.
#'
#' The following brood data checks are performed:
#' \itemize{
#' \item \strong{B1}: Check if the formats of each column in \code{Brood_data} match with the standard format using \code{\link{check_format_brood}}.
#' \item \strong{B2}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \item \strong{B3}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#' \item \strong{B4}: Compare lay date and hatching date per brood using \code{\link{compare_laying_hatching}}.
#' \item \strong{B5}: Compare hatching date and fledging date per brood using \code{\link{compare_hatching_fledging}}.
#' \item \strong{B6a-d}: Check brood variable values against reference values using \code{\link{check_values_brood}}. Brood variables checked: ClutchSize_observed, BroodSize_observed, NumberFledged_observed, LayDate_observed.
#' \item \strong{B7}: Compare brood size with number of chicks captured using \code{\link{compare_broodsize_chicknumber}}.
#' \item \strong{B8}: Check if the IDs of broods are unique using \code{\link{check_unique_BroodID}}.
#' \item \strong{B9}: Check if the order of clutch types for multiple breeding attempts per female per season is correct using \code{\link{check_clutch_type_order}}.
#' \item \strong{B10}: Check if parents of a brood are the same species using \code{\link{compare_species_parents}}.
#' \item \strong{B11}: Check if the brood and the parents of that brood are recorded as the same species using \code{\link{compare_brood_parents_species}}.
#' \item \strong{B12}: Check if the brood and the chicks in that brood are recorded as the same species using \code{\link{compare_brood_chicks_species}}.
#' }
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_brood}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @inherit checks_return return
#'
#' @export

brood_check <- function(Brood_data, Individual_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("B", c(1:5, paste0(6, letters[1:4]), 7:12)),
                               CheckDescription = c("Check format of brood data",
                                                    "Compare clutch and brood sizes",
                                                    "Compare brood sizes and fledgling numbers",
                                                    "Compare lay and hatche dates",
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
                                                    "Check species of brood and chicks"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Brood checks")

  # - Check format brood data
  if(check_format) {
    message("B1: Checking format of brood data...")

    check_format_brood_output <- check_format_brood(Brood_data)

    check_list[1,3:4] <- check_format_brood_output$CheckList

  }

  # - Compare clutch and brood sizes
  message("B2: Comparing clutch and brood sizes...")

  compare_clutch_brood_output <- compare_clutch_brood(Brood_data)

  check_list[2,3:4] <- compare_clutch_brood_output$CheckList

  # - Compare brood sizes and fledgling numbers
  message("B3: Comparing brood sizes and fledgling numbers...")

  compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data)

  check_list[3,3:4] <- compare_brood_fledglings_output$CheckList

  # - Compare lay and hatch dates
  message("B4: Comparing lay and hatch dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data)

  check_list[4,3:4] <- compare_laying_hatching_output$CheckList

  # - Compare hatch and fledge dates
  message("B5: Comparing hatch and fledge dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)

  check_list[5,3:4] <- compare_hatching_fledging_output$CheckList

  # - Check clutch size values against reference values
  message("B6a: Checking clutch size values against reference values...")

  var_ext <- ifelse("ClutchSize" %in% colnames(Brood_data), "", "_observed")

  check_values_clutch_size_output <- check_values_brood(Brood_data, paste0("ClutchSize", var_ext))

  check_list[6,3:4] <- check_values_clutch_size_output$CheckList

  # - Check brood size values against reference values
  message("B6b: Checking brood size values against reference values...")

  check_values_brood_size_output <- check_values_brood(Brood_data, paste0("BroodSize", var_ext))

  check_list[7,3:4] <- check_values_brood_size_output$CheckList

  # - Check fledgling number values against reference values
  message("B6c: Checking fledgling number values against reference values...")

  check_values_fledgling_number_output <- check_values_brood(Brood_data, paste0("NumberFledged", var_ext))

  check_list[8,3:4] <- check_values_fledgling_number_output$CheckList

  # - Check lay date values against reference values
  message("B6d: Checking lay date values against reference values...")

  check_values_lay_date_output <- check_values_brood(Brood_data, paste0("LayDate", var_ext))

  check_list[9,3:4] <- check_values_lay_date_output$CheckList

  # - Compare brood size and number of chicks captured
  message("B7: Comparing brood size and number of chicks captured...")

  compare_broodsize_chicknumber_output <- compare_broodsize_chicknumber(Brood_data, Individual_data)

  check_list[10,3:4] <- compare_broodsize_chicknumber_output$CheckList

  # - Check that BroodIDs are unique
  message("B8: Checking that brood IDs are unique...")

  check_unique_BroodID_output <- check_unique_BroodID(Brood_data)

  check_list[11,3:4] <- check_unique_BroodID_output$CheckList

  # - Check clutch type order
  message("B9: Checking that clutch type order is correct..")

  check_clutch_type_order_output <- check_clutch_type_order(Brood_data)

  check_list[12,3:4] <- check_clutch_type_order_output$CheckList

  # - Compare species of mother and father
  message("B10: Comparing species of mother and father...")

  compare_species_parents_output <- compare_species_parents(Brood_data, Individual_data)

  check_list[13,3:4] <- compare_species_parents_output$CheckList

  # - Compare species of brood and parents
  message("B11: Comparing species of brood and parents...")

  compare_species_brood_parents_output <- compare_species_brood_parents(Brood_data, Individual_data)

  check_list[14,3:4] <- compare_species_brood_parents_output$CheckList

  # - Compare species of brood and chicks
  message("B12: Comparing species of brood and chicks...")

  compare_species_brood_chicks_output <- compare_species_brood_chicks(Brood_data, Individual_data)

  check_list[15,3:4] <- compare_species_brood_chicks_output$CheckList


  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_brood_output$WarningOutput,
                         Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6a = check_values_clutch_size_output$WarningOutput,
                         Check6b = check_values_brood_size_output$WarningOutput,
                         Check6c = check_values_fledgling_number_output$WarningOutput,
                         Check6d = check_values_lay_date_output$WarningOutput,
                         Check7 = compare_broodsize_chicknumber_output$WarningOutput,
                         Check8 = check_unique_BroodID_output$WarningOutput,
                         Check9 = check_clutch_type_order_output$WarningOutput,
                         Check10 = compare_species_parents_output$WarningOutput,
                         Check11 = compare_species_brood_parents_output$WarningOutput,
                         Check12 = compare_species_brood_chicks_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_brood_output$ErrorOutput,
                       Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6a = check_values_clutch_size_output$ErrorOutput,
                       Check6b = check_values_brood_size_output$ErrorOutput,
                       Check6c = check_values_fledgling_number_output$ErrorOutput,
                       Check6d = check_values_lay_date_output$ErrorOutput,
                       Check7 = compare_broodsize_chicknumber_output$ErrorOutput,
                       Check8 = check_unique_BroodID_output$ErrorOutput,
                       Check9 = check_clutch_type_order_output$ErrorOutput,
                       Check10 = compare_species_parents_output$ErrorOutput,
                       Check11 = compare_species_brood_parents_output$ErrorOutput,
                       Check12 = compare_species_brood_chicks_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6a = check_values_clutch_size_output$WarningOutput,
                         Check6b = check_values_brood_size_output$WarningOutput,
                         Check6c = check_values_fledgling_number_output$WarningOutput,
                         Check6d = check_values_lay_date_output$WarningOutput,
                         Check7 = compare_broodsize_chicknumber_output$WarningOutput,
                         Check8 = check_unique_BroodID_output$WarningOutput,
                         Check9 = check_clutch_type_order_output$WarningOutput,
                         Check10 = compare_species_parents_output$WarningOutput,
                         Check11 = compare_species_brood_parents_output$WarningOutput,
                         Check12 = compare_species_brood_chicks_output$WarningOutput)

    # Error list
    error_list <- list(Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6a = check_values_clutch_size_output$ErrorOutput,
                       Check6b = check_values_brood_size_output$ErrorOutput,
                       Check6c = check_values_fledgling_number_output$ErrorOutput,
                       Check6d = check_values_lay_date_output$ErrorOutput,
                       Check7 = compare_broodsize_chicknumber_output$ErrorOutput,
                       Check8 = check_unique_BroodID_output$ErrorOutput,
                       Check9 = check_clutch_type_order_output$ErrorOutput,
                       Check10 = compare_species_parents_output$ErrorOutput,
                       Check11 = compare_species_brood_parents_output$ErrorOutput,
                       Check12 = compare_species_brood_chicks_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

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
                                     compare_species_brood_chicks_output$WarningRows)),
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
                                   compare_species_brood_chicks_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Check format of brood data
#'
#' Check that the format of each column in the brood data match with the standard format.
#'
#' Check ID: B1.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

check_format_brood <- function(Brood_data){

  ## Data frame with column names and formats according to the standard protocol
  Brood_data_standard <- tibble::tibble(Variable = c("Row", "BroodID", "PopID", "BreedingSeason", "Species", "Plot",
                                                     "LocationID", "FemaleID", "MaleID",
                                                     "ClutchType_observed", "ClutchType_calculated",
                                                     "LayDate", "LayDateError", "ClutchSize",
                                                     "ClutchSizeError", "HatchDate", "HatchDateError",
                                                     "BroodSize", "BroodSizeError", "FledgeDate",
                                                     "FledgeDateError", "NumberFledged",
                                                     "NumberFledgedError", "AvgEggMass", "NumberEggs",
                                                     "AvgChickMass", "NumberChicksMass", "AvgTarsus",
                                                     "NumberChicksTarsus", "OriginalTarsusMethod", "ExperimentID"),
                                        Format_standard = c("integer", "character", "character", "integer", "character",
                                                            "character", "character", "character", "character",
                                                            "character", "character",
                                                            "Date", "numeric", "integer",
                                                            "numeric", "Date", "numeric",
                                                            "integer", "numeric", "Date",
                                                            "numeric", "integer",
                                                            "numeric", "numeric", "integer",
                                                            "numeric", "integer", "numeric",
                                                            "integer", "character", "character"))

  ## Data frame with column names and formats from Brood data
  Brood_data_col <- tibble::tibble(Variable = names(Brood_data),
                                   Format = unlist(purrr::pmap(list(Brood_data), class)))

  ## Mismatches between Brood_data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Brood_data_mismatch <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Brood_data_mismatch$Variable,
                                .y = Brood_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Brood_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  # Brood_data_missing <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
  #   filter(Format == "logical")
  #
  # war <- FALSE
  # warning_output <- NULL
  #
  # if(nrow(Brood_data_missing) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::map(.x = Brood_data_missing$Variable,
  #                                .f = ~{
  #                                  paste0(.x, " in Brood_data is missing, unmeasured or undetermined (NA).")
  #                                })
  # }

  #Test for empty columns by looking at uniques, rather than using data type
  warning_output <- purrr::pmap(.l = list(as.list(Brood_data), colnames(Brood_data)),
                                .f = ~{

                                  if(all(is.na(unique(..1)))){

                                    return(paste0(..2, " in Brood_data is missing, unmeasured or undetermined (NA)."))

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


#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will result in an error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will result in a warning.
#'
#' Check ID: B2.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_clutch_brood <- function(Brood_data){

  # Non-manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_non <- Brood_data %>%
    {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., is.na(ExperimentID) & ClutchSize < BroodSize) else dplyr::filter(., is.na(ExperimentID) & ClutchSize_observed < BroodSize_observed)}

  # Manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_man <- Brood_data %>%
    {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::filter(., !is.na(ExperimentID) & ClutchSize < BroodSize) else dplyr::filter(., !is.na(ExperimentID) & ClutchSize_observed < BroodSize_observed)}

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_non %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, ClutchSize, BroodSize)
        else dplyr::select(., Row, PopID, BroodID, ClutchSize_observed, BroodSize_observed)}

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

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_data_man %>%
      dplyr::mutate(CheckID = "B2") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("ClutchSize", "BroodSize") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, ClutchSize, BroodSize)
        else dplyr::select(., Row, PopID, BroodID, ClutchSize_observed, BroodSize_observed)}

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
  ExperimentID <- ClutchSize <- BroodSize <- NULL
  approved_list <- NULL

}


#' Compare brood sizes and fledgling numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' Check ID: B3.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_brood_fledglings <- function(Brood_data){

  # Non-manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_non <- Brood_data %>%
    {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., is.na(ExperimentID) & BroodSize < NumberFledged) else dplyr::filter(., is.na(ExperimentID) & BroodSize_observed < NumberFledged_observed)}

  # Manipulated broods
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_man <- Brood_data %>%
    {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::filter(., !is.na(ExperimentID) & BroodSize < NumberFledged) else dplyr::filter(., !is.na(ExperimentID) & BroodSize_observed < NumberFledged_observed)}

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_non %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, BroodSize, NumberFledged)
        else dplyr::select(., Row, PopID, BroodID, BroodSize_observed, NumberFledged_observed)}

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

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_data_man %>%
      dplyr::mutate(CheckID = "B3") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("BroodSize", "NumberFledged") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, BroodSize, NumberFledged)
        else dplyr::select(., Row, PopID, BroodID, BroodSize_observed, NumberFledged_observed)}

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
  ExperimentID <- BroodSize <- NumberFledged <- NULL
  approved_list <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' Check ID: B4.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_laying_hatching <- function(Brood_data){

  # Broods with laying date later than hatching date
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_late <- Brood_data %>%
      {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::filter(., LayDate >= HatchDate)
        else dplyr::filter(., LayDate_observed >= HatchDate_observed)}

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

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_late %>%
      dplyr::mutate(CheckID = "B4") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("LayDate", "HatchDate") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, LayDate, HatchDate)
        else dplyr::select(., Row, PopID, BroodID, LayDate_observed, HatchDate_observed)}

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
  LayDate <- HatchDate <- NULL
  approved_list <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' Check ID: B5.
#'
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

compare_hatching_fledging <- function(Brood_data){

  # Broods with laying date later than hatching date
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_data_late <- Brood_data %>%
    {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::filter(., HatchDate >= FledgeDate)
      else dplyr::filter(., HatchDate_observed >= FledgeDate_observed)}

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

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_data_late %>%
      dplyr::mutate(CheckID = "B5") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      {if(all(c("HatchDate", "FledgeDate") %in% colnames(.))) dplyr::select(., Row, PopID, BroodID, HatchDate, FledgeDate)
        else dplyr::select(., Row, PopID, BroodID, HatchDate_observed, FledgeDate_observed)}

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
  HatchDate <- FledgeDate <- NULL
  approved_list <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against population-species-specific reference values in brood data. Reference values are based on the data if the number of observations is sufficiently large (n >= 100). Records for population-species combinations that are low in number (n < 100) are only compared to reference values that are not data generated (see Details below).
#'
#' \strong{ClutchSize_observed, BroodSize_observed, NumberFledged_observed} \cr
#' Check IDs: B6a-c \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered unusual if they are larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as an error.}
#' }
#'
#' \strong{LayDate_observed} \cr
#' Check ID: B6d \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered unusual if they are earlier than the 1st percentile or later than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are earlier than January 1st or later than December 31st, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Date columns are transformed to Julian days to calculate percentiles. Records are considered impossible if they are earlier than January 1st or later than December 31st, and will be flagged as an error.}
#' }
#'
#' @inheritParams checks_brood_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @importFrom progress progress_bar
#' @export

check_values_brood <- function(Brood_data, var) {

  # Stop if "var" is missing
  if(missing(var)) {

    stop("Please select a variable in Brood_data to check against reference values.")

  }

  # Create reference values from data
  # Numeric & integer columns
  if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(Species)) %>%
      dplyr::group_by(Species, PopID) %>%
      dplyr::summarise(Warning_min = NA,
                       Warning_max = ceiling(quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE)),
                       Error_min = 0,
                       Error_max = 4 * Warning_max,
                       n = n()) %>%
      dplyr::arrange(PopID, Species)

    # Date columns
  } else if(var %in% c("LayDate", "LayDate_observed")) {

    ref <- Brood_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)) & !is.na(Species)) %>%
      dplyr::group_by(BreedingSeason) %>%
      # Transform dates to Julian days (while accounting for year) to calculate quantiles
      dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Species, PopID) %>%
      dplyr::summarise(Warning_min = floor(quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.01, na.rm = TRUE)),
                       Warning_max = ceiling(quantile(!!rlang::sym(paste0(var, "_julian")), probs = 0.99, na.rm = TRUE)),
                       ##TODO: this error is specific to spring/summer breeders, make more general when species with other types of breeding system enter the pipelines.
                       Error_min = 1,
                       Error_max = 366,
                       n = n()) %>%
      dplyr::arrange(PopID, Species)
  }


  # Print message for population-species combinations with too low number of observations
  if(any(ref$n < 100)) {

    low_obs <- ref %>%
      dplyr::filter(n < 100) %>%
      dplyr::select(Species, PopID)

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
  Brood_err <- purrr::pmap(.l = ref,
                           .f = ~{

                             pb$tick()


                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                           "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                               # If number of observations is large enough, compare brood values
                               # to all reference values
                               if(..7 >= 100) {

                                 Brood_data %>%
                                   dplyr::filter(Species == ..1 & PopID == ..2 &
                                                   (!!rlang::sym(var) < ..5 | !!rlang::sym(var) > ..6)) %>%
                                   dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                   dplyr::mutate(Variable = var)

                                 # If number of observations is too low, only compare brood values
                                 # to reference values not based on quantiles
                               } else {

                                 Brood_data %>%
                                   dplyr::filter(Species == ..1 & PopID == ..2 & !!rlang::sym(var) < ..5) %>%
                                   dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                   dplyr::mutate(Variable = var)

                               }

                             } else if(var %in% c("LayDate", "LayDate_observed")) {

                               Brood_data %>%
                                 dplyr::group_by(BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 (!!rlang::sym(paste0(var, "_julian")) < ..5 | !!rlang::sym(paste0(var, "_julian")) > ..6)) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             }

                           }) %>%
    dplyr::bind_rows()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_err) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_err %>%
      dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::arrange(Row)

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (PopID: ", ..2, "; ",
                                         "BroodID: ", ..3, "; ",
                                         species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                         " has an impossible value in ", ..6, " (", ..4, ").")
                                })
  }


  # Brood-specific warnings
  # Warnings are only checked for population-species combinations with at least 100 observations
  warning_ref <- ref %>%
    dplyr::filter(n >= 100)

  Brood_war <- purrr::pmap(.l = warning_ref,
                           .f = ~{

                             pb$tick()

                             if(var %in% c("ClutchSize", "BroodSize", "NumberFledged",
                                           "ClutchSize_observed", "BroodSize_observed", "NumberFledged_observed")) {

                               Brood_data %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 (!!rlang::sym(var) > ..4 & !!rlang::sym(var) <= ..6)) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             } else if(var %in% c("LayDate", "LayDate_observed")) {

                               Brood_data %>%
                                 dplyr::group_by(BreedingSeason) %>%
                                 # Transform dates to Julian days (while accounting for year)
                                 # to compare to Julian day reference values
                                 dplyr::mutate(!!paste0(var, "_julian") := as.numeric(!!rlang::sym(var) - lubridate::ymd(paste(BreedingSeason, "1", "1", sep = "-")) + 1)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::filter(Species == ..1 & PopID == ..2 &
                                                 ((!!rlang::sym(paste0(var, "_julian")) > ..4 & !!rlang::sym(paste0(var, "_julian")) <= ..6) | (!!rlang::sym(paste0(var, "_julian")) < ..3 & !!rlang::sym(paste0(var, "_julian")) >= ..5))) %>%
                                 dplyr::select(Row, PopID, BroodID, !!rlang::sym(var), Species) %>%
                                 dplyr::mutate(Variable = var)

                             }

                           }) %>%
    dplyr::bind_rows()

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_war %>%
      dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::arrange(Row)

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..2, "; ",
                                           "BroodID: ", ..3, "; ",
                                           species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                           " has an unusual value in ", ..6, " (", ..4, ").")
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
  Species <- Variable <- NULL
  approved_list <- checkID_var <- NULL

}


#' Compare brood size with number of chicks captured
#'
#' Compare BroodSize in Brood_data with the number of chicks captured in Capture_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks captured results in a warning, because chicks might have died before ringing and measuring. Records where BroodSize is smaller than the number of chicks captured results in an error, because this should not be possible.
#'
#' Check ID: B7.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_broodsize_chicknumber <- function(Brood_data, Individual_data) {

  # Link BroodID from Individual_data to each capture in Capture_data
  Chicks_captured <- Individual_data %>%
    dplyr::select(IndvID, BroodIDLaid) %>%
    dplyr::group_by(BroodIDLaid) %>%
    dplyr::summarise(Chicks = n_distinct(IndvID)) %>%
    dplyr::ungroup()

  # Errors
  # Select records where number of chicks in Capture_data > brood size in Brood_data
  # (this should not be possible)
  # NB: allows v1.0 & v1.1 variable names of the standard format
  Brood_err <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  BroodSize < Chicks)
      else dplyr::filter(., BroodSize_observed < Chicks)} %>%
    {if("BroodSize" %in% colnames(.)) dplyr::select(., Row, PopID, BroodID, BroodSize, Chicks)
      else dplyr::select(., Row, PopID, BroodID, BroodSize_observed, Chicks)}

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Brood_err %>%
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
  Brood_war <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    {if("BroodSize" %in% colnames(.)) dplyr::filter(.,  BroodSize > Chicks)
      else dplyr::filter(., BroodSize_observed > Chicks)} %>%
    {if("BroodSize" %in% colnames(.)) dplyr::select(., Row, PopID, BroodID, BroodSize, Chicks)
      else dplyr::select(., Row, PopID, BroodID, BroodSize_observed, Chicks)}

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Brood_war %>%
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
#' Check ID: B8.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_unique_BroodID <- function(Brood_data){

  # Errors
  # Select records that are duplicated within populations
  Duplicated <- Brood_data %>%
    dplyr::group_by(PopID, BroodID) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup()

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Duplicated) > 0) {
    err <- TRUE

    # Compare to whitelist
    error_records <- Duplicated %>%
      dplyr::mutate(CheckID = "B8") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID")) %>%
      dplyr::select(Row, BroodID, PopID)

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
#' Check ID: B9.
#'
#' @inheritParams checks_brood_params
#' @inherit checks_return return
#'
#' @export

check_clutch_type_order <- function(Brood_data){

  # Select breeding females with ClutchType_calculated == "first" not as first clutch in a particular breeding season
  Brood_err <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(ClutchType_calculated)) %>%
    dplyr::group_by(PopID, BreedingSeason, FemaleID) %>%
    dplyr::summarise(CTcal = ifelse(any(ClutchType_calculated == "first"),
                                    which(ClutchType_calculated == "first"), NA),
                     BroodID = BroodID[CTcal],
                     Row = Row[CTcal]) %>%
    dplyr::filter(!is.na(CTcal) & CTcal > 1) %>%
    dplyr::ungroup()

  # Errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Brood_err) > 0) {

    err <- TRUE

    # Compare to whitelist
    error_records <- Brood_err %>%
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
  Brood_err <- NULL
  approved_list <- NULL

}


#' Compare species of parents
#'
#' Check that the parents of broods are of the same species. Broods with parents of different species are flagged as 'warning' because multi-species broods are known to exist.
#'
#' Check ID: B10.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_parents <- function(Brood_data, Individual_data) {

  # Find species information of mothers
  Females <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, FemaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename(FemaleSpecies = Species)

  # Find species information of fathers
  Males <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, MaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("MaleID" = "IndvID")) %>%
    dplyr::rename(MaleSpecies = Species)

  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Warnings
  # Select records where parents are different species
  Multi_species_broods <- dplyr::left_join(Females, Males,
                                           by=c("Row", "PopID", "BroodID")) %>%
    dplyr::filter(FemaleSpecies != MaleSpecies)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Multi_species_broods) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Multi_species_broods %>%
      dplyr::mutate(CheckID = "B10") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " has parents of different species",
                                           " (Mother: ", species_codes[species_codes$Species == ..5, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..7, "CommonName"], ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  FemaleSpecies <- MaleSpecies <- NULL
  approved_list <- NULL

}

#' Compare species of brood and species of parents
#'
#' Check that the species of broods is the same as the species of the parents of that brood. Records that violate this assumption are flagged as 'warning' because brood fostering is known to exist.
#'
#' Check ID: B11.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_parents <- function(Brood_data, Individual_data) {

  # Find species information of mothers
  Females <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, FemaleID, BroodSpecies = Species) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename(FemaleSpecies = Species)

  # Find species information of fathers
  Males <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, PopID, BroodID, MaleID, BroodSpecies = Species) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")],
                     by=c("MaleID" = "IndvID")) %>%
    dplyr::rename(MaleSpecies = Species)

  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Warnings
  # Select records where parents are not of the same species as the brood
  Multi_species_broods <- dplyr::left_join(Females, Males,
                                           by=c("Row", "PopID", "BroodID", "BroodSpecies")) %>%
    dplyr::filter(FemaleSpecies != BroodSpecies | MaleSpecies != BroodSpecies)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Multi_species_broods) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Multi_species_broods %>%
      dplyr::mutate(CheckID = "B11") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; BroodID: ", ..3, ")",
                                           " is recorded as a different species than any of the parents",
                                           " (Mother: ", species_codes[species_codes$Species == ..6, "CommonName"],
                                           ", father: ", species_codes[species_codes$Species == ..8, "CommonName"], ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  FemaleSpecies <- MaleSpecies <- NULL
  approved_list <- NULL

}

#' Compare species of brood and species of chicks
#'
#' Check that the species of broods is the same as species of the chicks in that brood. Records that violate this assumption are flagged as 'warning' because brood fostering is known to exist.
#'
#' Check ID: B12.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

compare_species_brood_chicks <- function(Brood_data, Individual_data) {

  Individuals <- Individual_data %>%
    # Do not select individuals without BroodID and conflicted species
    # The latter is evaluated in check I5.
    dplyr::filter(!is.na(BroodIDLaid) & (Species != "CONFLICTED" | Species != "CCCCCC")) %>%
    dplyr::select(IndvID, IndvSpecies = Species, BroodIDLaid, PopID)

  # Select records where chicks are not of the same species as the brood
  Multi_species_broods <- Brood_data %>%
    dplyr::left_join(Individuals, by=c("BroodID" = "BroodIDLaid", "PopID")) %>%
    dplyr::mutate(SpeciesComp = Species != IndvSpecies) %>%
    dplyr::group_by(PopID, BroodID, Row) %>%
    dplyr::summarise(OtherSpeciesChicks = sum(SpeciesComp),
                     Chicks = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(OtherSpeciesChicks > 0)

  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Multi_species_broods) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Multi_species_broods %>%
      dplyr::mutate(CheckID = "B12") %>%
      dplyr::anti_join(approved_list$Brood_approved_list, by=c("PopID", "CheckID", "BroodID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{
                                    paste0("Record on row ", ..3, " (PopID: ", ..1, "; BroodID: ", ..2, ")",
                                           " is recorded as a different species than ", ..4, " out of ", ..5, " chicks.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  FemaleSpecies <- MaleSpecies <- NULL
  approved_list <- NULL

}
