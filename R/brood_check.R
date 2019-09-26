#' A wrapper function to perform quality checks on brood data
#'
#' A wrapper that runs all single checks related to `Brood_data`.
#'
#' \strong{Brood check 1}: Check if the formats of each column in `Brood_data` match with the standard format using \code{\link{check_format_brood}}.
#' \strong{Brood check 2}: Compare clutch size and brood size per brood using \code{\link{compare_clutch_brood}}.
#' \strong{Brood check 3}: Compare brood size and fledgling number per brood using \code{\link{compare_brood_fledglings}}.
#'
#' @inheritParams checks_brood_params
#'
#' @return A list of 3 items: a summary data frame of all checks, a record-by-record list of warnings, and a record-by-record list of errors.
#'
#' @export

brood_check <- function(Brood_data){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = purrr::map_chr(1:6, ~paste0("B", .)),
                               CheckDescription = c("Brood data format",
                                                    "Clutch and brood sizes",
                                                    "Brood sizes and fledgling numbers",
                                                    "Laying and hatching dates",
                                                    "Hatching and fledging dates",
                                                    "Improbable and impossible values brood data"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Brood checks")

  # - Check format brood data
  message("B1: Checking format of brood data...")

  check_format_brood_output <- check_format_brood(Brood_data)

  check_list[1,3:4] <- check_format_brood_output$check_list

  # - Compare clutch and brood sizes
  message("B2: Comparing clutch and brood sizes...")

  compare_clutch_brood_output <- compare_clutch_brood(Brood_data)

  check_list[2,3:4] <- compare_clutch_brood_output$check_list

  # - Compare brood sizes and fledgling numbers
  message("B3: Comparing brood sizes and fledgling numbers...")

  compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data)

  check_list[3,3:4] <- compare_brood_fledglings_output$check_list

  # - Compare laying and hatching dates
  message("B4: Comparing laying and hatching dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data)

  check_list[4,3:4] <- compare_laying_hatching_output$check_list

  # - Compare hatching and fledging dates
  message("B5: Comparing hatching and fledging dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)

  check_list[5,3:4] <- compare_hatching_fledging_output$check_list

  # - Check brood variable values against reference values
  message("B6: Checking brood variable values against reference values...")

  check_values_brood_output <- check_values_brood(Brood_data)

  check_list[6,3:4] <- check_values_brood_output$check_list


  return(list(CheckList = check_list,
              CheckIDs = check_list$CheckID,
              CheckDescriptions = check_list$CheckDescription,
              Warnings = list(
                Check1 = check_format_brood_output$warning_output,
                Check2 = compare_clutch_brood_output$warning_output,
                Check3 = compare_brood_fledglings_output$warning_output,
                Check4 = compare_laying_hatching_output$warning_output,
                Check5 = compare_hatching_fledging_output$warning_output,
                Check6 = check_values_brood_output$warning_output),
              Errors = list(
                Check1 = check_format_brood_output$error_output,
                Check2 = compare_clutch_brood_output$error_output,
                Check3 = compare_brood_fledglings_output$error_output,
                Check4 = compare_laying_hatching_output$error_output,
                Check5 = compare_hatching_fledging_output$error_output,
                Check6 = check_values_brood_output$error_output)
              ))
}

#' Check format of brood data
#'
#' Check if the formats of each column in the brood data match with the standard format
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
#' @export

check_format_brood <- function(Brood_data){

  ## Data frame with column names and formats according to the standard protocol
  Brood_data_standard <- tibble::tibble(Variable = c("Row", "BroodID", "PopID", "BreedingSeason", "Species", "Plot",
                                                     "LocationID", "FemaleID", "MaleID",
                                                     "ClutchType_observed", "ClutchType_calculated",
                                                     "LayingDate", "LayingDateError", "ClutchSize",
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

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will result in an error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
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
                                           "), and was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  ExperimentID <- ClutchSize <- BroodSize <- NULL

}


#' Compare brood sizes and fledgling numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
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

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  ExperimentID <- BroodSize <- NumberFledged <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
#' @export

compare_laying_hatching <- function(Brood_data){

  # Broods with laying date later than hatching date
  Brood_data_late <- Brood_data %>%
    filter(LayingDate >= HatchDate)

  # Broods with laying date earlier than hatching date but the difference
  # in number of days is smaller than incubation time
  ## INCUBATION TIME IS SPECIES-SPECIFIC (& POPULATION-SPECIFIC?)
  ## PERHAPS THIS WILL BE DETERMINED AND CHECKED IN ANOTHER CHECK (NOT NOW)

  # Brood_data_late <- Brood_data %>%
  #   filter(LayingDate < HatchDate & (HatchDate-LayingDate) >= )

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_late) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_late$Row,
                                          Brood_data_late$LayingDate,
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

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  LayingDate <- HatchDate <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
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

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  HatchDate <- FledgeDate <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against species-specific reference values in brood data. Implausible values will result in a warning. Impossible values will result in an error. Variables that are checked: LayingDate, ClutchSize, HatchDate, BroodSize, FledgeDate, NumberFledged, AvgEggMass, AvgChickMass, AvgTarsus.
#'
#' @inheritParams checks_brood_params
#'
#' @return Check list, warning output, error output.
#' @export

check_values_brood <- function(Brood_data) {

  # Select reference values for species present in Brood data
  species <- unique(Brood_data$Species)
  ref <- brood_ref_values_list[which(names(brood_ref_values_list) %in% species)]

  # Structure Brood data in the same way as reference values: a list of species;  each species is a list of brood variables
  variables <- c("ClutchSize", "BroodSize", "NumberFledged")

  Brood_data2 <- split(Brood_data[,c("Row", variables)], Brood_data$Species) # Split per species

  Brood_data2 <- purrr::map2(.x = Brood_data2, # Split per variable
                             .y = names(Brood_data2),
                             .f = ~{
                               data <- .x
                               purrr::map(variables,
                                          .f = ~{
                                            data %>%
                                              dplyr::select(Row, .x)
                                          }) -> data2
                               names(data2) <- variables
                               return(data2)
                             })

  # Brood-specific warning and error checks
  Brood_list <-  purrr::map2(.x = Brood_data2,
                             .y = ref,
                             .f = ~purrr::map2(.x, .y,
                                               .f = ~{
                                                 .x %>%
                                                   dplyr::mutate(
                                                     Value = dplyr::pull(.x[,2]),
                                                     Variable = names(.x)[2],
                                                     Warning = ifelse(.x[,2] > dplyr::pull(.y[2,3]) & .x[,2] <= dplyr::pull(.y[4,3]),
                                                                      TRUE, FALSE),
                                                     Error = ifelse(.x[,2] < dplyr::pull(.y[3,3]) | .x[,2] > dplyr::pull(.y[4,3]),
                                                                    TRUE, FALSE)
                                                   )
                                               })
  )

  # Brood_list <- purrr::map2(.x = Brood_data[, c("ClutchSize", "BroodSize", "NumberFledged")],
  #                           .y = ref,
  #                           .f = ~{
  #                             tibble::tibble(BroodID = Brood_data$BroodID,
  #                                            Variable = names(.y)[3],
  #                                            .x) %>%
  #                               dplyr::filter(!is.na(.x)) %>% # Only non-NA's
  #                               dplyr::mutate(Warning = ifelse(.x > as.numeric(.y[2,3]),
  #                                                              TRUE, FALSE),
  #                                             Error = ifelse(.x < as.numeric(.y[3,3]) | .x > as.numeric(.y[4,3]),
  #                                                            TRUE, FALSE))
  #                           })

  # Select records with errors (impossible values) from list
  Brood_err <- purrr::map(.x = Brood_list,
                          .f = ~purrr::map(.x,
                                           .f = ~{
                                             .x %>%
                                               dplyr::select(Row, Variable, Value, Error) %>%
                                               dplyr::filter(Error == TRUE)
                                           }) %>%
                            dplyr::bind_rows()
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Row)

  # Brood_err <- purrr::map(.x = Brood_list,
  #                         .f = ~{
  #                           .x %>%
  #                             dplyr::filter(Error == TRUE)
  #                         }) %>%
  #   dplyr::bind_rows()

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_err$Row,
                                          Brood_err$Variable,
                                          Brood_err$Value),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (species: ", dplyr::pull(Brood_data[Brood_data$Row == ..1, "Species"]), ")",
                                         " has an impossible value in ", ..2,
                                         " (", ..3, ").")
                                })
  }

  # Select records with warnings (improbable values) from list
  Brood_war <- purrr::map(.x = Brood_list,
                          .f = ~purrr::map(.x,
                                           .f = ~{
                                             .x %>%
                                               dplyr::select(Row, Variable, Value, Warning) %>%
                                               dplyr::filter(Warning == TRUE)
                                           }) %>%
                            dplyr::bind_rows()
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Row)


  # Brood_war <- purrr::map(.x = Brood_list,
  #                         .f = ~{
  #                           .x %>%
  #                             dplyr::filter(Warning == TRUE)
  #                         }) %>%
  #   dplyr::bind_rows()

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_war$Row,
                                            Brood_war$Variable,
                                            Brood_war$Value),
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (species: ", dplyr::pull(Brood_data[Brood_data$Row == ..1, "Species"]), ")",
                                           " has an improbable value in ", ..2,
                                           " (", ..3, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  #Satisfy RCMD Checks
  brood_ref_values_list <- Species <- NULL

}
