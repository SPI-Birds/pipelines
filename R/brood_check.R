#' Perform quality checks on brood data
#'
#' A wrapper that runs all single checks related to \code{Brood_data}.
#'
#' The following brood data checks are performed:
#' \itemize{
#' \item \strong{B1}: Check if the formats of each column in \code{Brood_data} match with the standard format using \code{\link{check_format_brood}}.
#' \item \strong{B2}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \item \strong{B3}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#' \item \strong{B4}: Compare laying date and hatching date per brood using \code{\link{compare_laying_hatching}}.
#' \item \strong{B5}: Compare hatching date and fledging date per brood using \code{\link{compare_hatching_fledging}}.
#' \item \strong{B6}: Check brood variable values against reference values using \code{\link{check_values_brood}}.
#' \item \strong{B7}: Check if parents of a brood are the same species using \code{\link{check_parent_species}}.
#' \item \strong{B8}: Compare brood size with number of chicks captured using \code{\link{compare_broodsize_chicknumber}}.
#' }
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_brood}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#'
#' @export

brood_check <- function(Brood_data, Individual_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = purrr::map_chr(1:8, ~paste0("B", .)),
                               CheckDescription = c("Check format of brood data",
                                                    "Compare clutch and brood sizes",
                                                    "Compare brood sizes and fledgling numbers",
                                                    "Compare laying and hatching dates",
                                                    "Compare hatching and fledging dates",
                                                    "Check brood variable values against reference values",
                                                    "Check that parents are the same species",
                                                    "Compare brood size with number of chicks captured"),
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

  # - Compare laying and hatching dates
  message("B4: Comparing laying and hatching dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data)

  check_list[4,3:4] <- compare_laying_hatching_output$CheckList

  # - Compare hatching and fledging dates
  message("B5: Comparing hatching and fledging dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)

  check_list[5,3:4] <- compare_hatching_fledging_output$CheckList

  # - Check brood variable values against reference values
  message("B6: Checking brood variable values against reference values...")

  check_values_brood_output <- check_values_brood(Brood_data)

  check_list[6,3:4] <- check_values_brood_output$CheckList

  # - Check parents of broods are the same species
  message("B7: Checking that parents are the same species...")

  check_parent_species_output <- check_parent_species(Brood_data, Individual_data)

  check_list[7,3:4] <- check_parent_species_output$CheckList

  # - Compare brood size and number of chicks captured
  message("B8: Comparing brood size and number of chicks captured...")

  compare_broodsize_chicknumber_output <- compare_broodsize_chicknumber(Brood_data, Individual_data)

  check_list[8,3:4] <- compare_broodsize_chicknumber_output$CheckList


  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_brood_output$WarningOutput,
                         Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6 = check_values_brood_output$WarningOutput,
                         Check7 = check_parent_species_output$WarningOutput,
                         Check8 = compare_broodsize_chicknumber_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_brood_output$ErrorOutput,
                       Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6 = check_values_brood_output$ErrorOutput,
                       Check7 = check_parent_species_output$ErrorOutput,
                       Check8 = compare_broodsize_chicknumber_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2 = compare_clutch_brood_output$WarningOutput,
                         Check3 = compare_brood_fledglings_output$WarningOutput,
                         Check4 = compare_laying_hatching_output$WarningOutput,
                         Check5 = compare_hatching_fledging_output$WarningOutput,
                         Check6 = check_values_brood_output$WarningOutput,
                         Check7 = check_parent_species_output$WarningOutput,
                         Check8 = compare_broodsize_chicknumber_output$WarningOutput)

    # Error list
    error_list <- list(Check2 = compare_clutch_brood_output$ErrorOutput,
                       Check3 = compare_brood_fledglings_output$ErrorOutput,
                       Check4 = compare_laying_hatching_output$ErrorOutput,
                       Check5 = compare_hatching_fledging_output$ErrorOutput,
                       Check6 = check_values_brood_output$ErrorOutput,
                       Check7 = check_parent_species_output$ErrorOutput,
                       Check8 = compare_broodsize_chicknumber_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              Warnings = warning_list,
              Errors = error_list,
              Warning_Rows = unique(c(compare_clutch_brood_output$WarningRows,
                                    compare_brood_fledglings_output$WarningRows,
                                    compare_laying_hatching_output$WarningRows,
                                    compare_hatching_fledging_output$WarningRows,
                                    check_values_brood_output$WarningRows,
                                    check_parent_species_output$WarningRows,
                                    compare_broodsize_chicknumber_output$WarningRows)),
              Error_Rows = unique(c(compare_clutch_brood_output$ErrorRows,
                                    compare_brood_fledglings_output$ErrorRows,
                                    compare_laying_hatching_output$ErrorRows,
                                    compare_hatching_fledging_output$ErrorRows,
                                    check_values_brood_output$ErrorRows,
                                    check_parent_species_output$ErrorRows,
                                    compare_broodsize_chicknumber_output$ErrorRows))))
}

#' Check format of brood data
#'
#' Check that the format of each column in the brood data match with the standard format.
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
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
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

compare_clutch_brood <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & ClutchSize < BroodSize)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(!is.na(ExperimentID) & ClutchSize < BroodSize)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_non$Row,
                                          Brood_data_non$ClutchSize,
                                          Brood_data_non$BroodSize),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " has a larger brood size (", ..3,
                                         ") than clutch size (", ..2,
                                         "), but was not experimentally manipulated.")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_data_man$Row,
                                            Brood_data_man$ClutchSize,
                                            Brood_data_man$BroodSize),
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " has a larger brood size (", ..3,
                                           ") than clutch size (", ..2,
                                           "), but was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = Brood_data_man$Row,
              ErrorRows = Brood_data_non$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  ExperimentID <- ClutchSize <- BroodSize <- NULL

}


#' Compare brood sizes and fledgling numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

compare_brood_fledglings <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & BroodSize < NumberFledged)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(!is.na(ExperimentID) & BroodSize < NumberFledged)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_non$Row,
                                          Brood_data_non$BroodSize,
                                          Brood_data_non$NumberFledged),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " has a larger fledgling number (", ..3,
                                         ") than brood size (", ..2,
                                         "), but was not experimentally manipulated.")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_data_man$Row,
                                            Brood_data_man$BroodSize,
                                            Brood_data_man$NumberFledged),
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " has a larger fledgling number (", ..3,
                                           ") than brood size (", ..2,
                                           "), and was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = Brood_data_man$Row,
              ErrorRows = Brood_data_non$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  ExperimentID <- BroodSize <- NumberFledged <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

compare_laying_hatching <- function(Brood_data){

  # Broods with laying date later than hatching date
  Brood_data_late <- Brood_data %>%
    filter(LayDate >= HatchDate)

  # Broods with laying date earlier than hatching date but the difference
  # in number of days is smaller than incubation time
  ## INCUBATION TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(LayDate < HatchDate & (HatchDate-LayDate) >= )

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_late$Row,
                                          Brood_data_late$LayDate,
                                          Brood_data_late$HatchDate),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " has a later laying date (", ..2,
                                         ") than hatching date (", ..3, ").")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  # if(nrow(Brood_data_man) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::pmap(.l = list(Brood_data_man$BroodID,
  #                                           Brood_data_man$BroodSize,
  #                                           Brood_data_man$NumberFledged),
  #                                 .f = ~{
  #                                   paste0("Record with BroodID ", ..1,
  #                                          " has a larger fledgling number (", ..3,
  #                                          ") than brood size (", ..2, ").")
  #                                 })
  # }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = Brood_data_late$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  LayDate <- HatchDate <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

compare_hatching_fledging <- function(Brood_data){

  # Broods with laying date later than hatching date
  Brood_data_late <- Brood_data %>%
    filter(HatchDate >= FledgeDate)

  # Broods with hatching date earlier than fledging date but the difference
  # in number of days is smaller than breeding time
  ## BREEDING TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(HatchDate < FledgeDate & (FledgeDate-HatchDate) >= )

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_late$Row,
                                          Brood_data_late$HatchDate,
                                          Brood_data_late$FledgeDate),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " has a later hatching date (", ..2,
                                         ") than fledging date (", ..3, ").")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  # if(nrow(Brood_data_man) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::pmap(.l = list(Brood_data_man$BroodID,
  #                                           Brood_data_man$BroodSize,
  #                                           Brood_data_man$NumberFledged),
  #                                 .f = ~{
  #                                   paste0("Record with BroodID ", ..1,
  #                                          " has a larger fledgling number (", ..3,
  #                                          ") than brood size (", ..2, ").")
  #                                 })
  # }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = Brood_data_late$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  HatchDate <- FledgeDate <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against species-specific reference values in brood data. Unusual values will result in a warning. Impossible values will result in an error. Variables that are checked: LayDate, ClutchSize, HatchDate, BroodSize, FledgeDate, NumberFledged, AvgEggMass, AvgChickMass, AvgTarsus.
#'
#' @inheritParams checks_brood_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

check_values_brood <- function(Brood_data) {

  # Reference values
  ref_names <- stringr::str_split(names(brood_ref_values), pattern="_")

  # Progress bar
  pb <- dplyr::progress_estimated(2*length(brood_ref_values))

  # Brood-specific errors
  Brood_err <- purrr::map2(.x = brood_ref_values,
                           .y = ref_names,
                           .f = ~{
                             pb$tick()$print()
                             sel <- which(Brood_data$Species == .y[1]
                                          & (Brood_data[,which(colnames(Brood_data) == .y[2])] < .x$Value[3]
                                             | Brood_data[,which(colnames(Brood_data) == .y[2])] > .x$Value[4]))

                             Brood_data[sel,] %>%
                               dplyr::select(Row, Value = !!.y[2]) %>%
                               dplyr::mutate(Species = .y[1],
                                             Variable = .y[2])
                           }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Species, Variable)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = Brood_err,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (", Species_codes[Species_codes$Code == ..3, "CommonName"], ")",
                                         " has an impossible value in ", ..4, " (", ..2, ").")
                                })
  }

  # Brood-specific warnings
  Brood_war <- purrr::map2(.x = brood_ref_values,
                           .y = ref_names,
                           .f = ~{
                             pb$tick()$print()
                             sel <- which(Brood_data$Species == .y[1]
                                          & Brood_data[,which(colnames(Brood_data) == .y[2])] > .x$Value[2]
                                          & Brood_data[,which(colnames(Brood_data) == .y[2])] <= .x$Value[4])

                             Brood_data[sel,] %>%
                               dplyr::select(Row, Value = !!.y[2]) %>%
                               dplyr::mutate(Species = .y[1],
                                             Variable = .y[2])
                           }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Species, Variable)

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = Brood_war,
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " (", Species_codes[Species_codes$Code == ..3, "CommonName"], ")",
                                           " has an unusually high value in ", ..4, " (", ..2, " > ",
                                           brood_ref_values[[paste(..3, ..4, sep="_")]]$Value[2],")")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = Brood_war$Row,
              ErrorRows = Brood_err$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  brood_ref_values <- Species <- NULL
  Variable <- NULL
}


#' Check parent species
#'
#' Check that the parents of broods are the same species.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

check_parent_species <- function(Brood_data, Individual_data) {

  # Find species information of mothers
  Females <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(Row, BroodID, FemaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("FemaleID" = "IndvID")) %>%
    dplyr::rename(FemaleSpecies = Species)

  # Find species information of fathers
  Males <- Brood_data %>%
    dplyr::filter(!is.na(FemaleID) & !is.na(MaleID)) %>%
    dplyr::select(MaleID) %>%
    dplyr::left_join(Individual_data[,c("IndvID", "Species")], by=c("MaleID" = "IndvID")) %>%
    dplyr::rename(MaleSpecies = Species)

  # Join and select broods where parents are different species
  Interspecific_broods <- dplyr::bind_cols(Females, Males) %>%
    dplyr::filter(FemaleSpecies != MaleSpecies)

  err <- FALSE
  error_output <- NULL

  if(nrow(Interspecific_broods) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = Interspecific_broods,
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has parents of different species",
                                         " (Mother: ", Species_codes[Species_codes$Code == ..4, "CommonName"],
                                         ", father: ", Species_codes[Species_codes$Code == ..6, "CommonName"], ").")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = NULL,
              ErrorRows = Interspecific_broods$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD Checks
  FemaleSpecies <- MaleSpecies <- NULL
}


#' Compare brood size with number of chicks captured
#'
#' Compare BroodSize in Brood_data with the number of chicks captured in Capture_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks captured results in a warning, because chicks might have died before ringing and measuring. Records where BroodSize is smaller than the number of chicks captured results in an error, because this should not be possible.
#'
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of whether the check resulted in any warnings or errors.}
#' \item{WarningOutput}{A list of row-by-row warnings.}
#' \item{ErrorOutput}{A list of row-by-row errors.}
#'
#' @export

compare_broodsize_chicknumber <- function(Brood_data, Individual_data) {

  # Link BroodID from Individual_data to each capture in Capture_data
  Chicks_captured <- Individual_data %>%
    dplyr::select(IndvID, BroodIDLaid) %>%
    dplyr::group_by(BroodIDLaid) %>%
    dplyr::summarise(Chicks = n_distinct(IndvID))

  # Error if number of chicks in Capture_data > brood size in Brood_data
  # (this should not be possible)
  Brood_err <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    dplyr::filter(BroodSize < Chicks) %>%
    dplyr::select(Row, BroodID, BroodSize, Chicks)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = Brood_err,
                                .f = ~{
                                  paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                         " has a smaller BroodSize (", ..3, ")",
                                         " than the number of chicks in Individual_data (",
                                         ..4, ").")
                                })
  }

  # Warning if number of chicks in Capture_data < brood size in Brood_data
  # (chicks might have died before measuring/ringing)
  Brood_war <- Brood_data %>%
    dplyr::left_join(Chicks_captured, by=c("BroodID" = "BroodIDLaid")) %>%
    dplyr::filter(BroodSize > Chicks) %>%
    dplyr::select(Row, BroodID, BroodSize, Chicks)


  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = Brood_war,
                                  .f = ~{
                                    paste0("Record on row ", ..1, " (BroodID: ", ..2, ")",
                                           " has a larger BroodSize (", ..3, ")",
                                           " than the number of chicks in Individual_data (",
                                           ..4, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = Brood_war$Row,
              ErrorRows = Brood_err$Row,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))
}
