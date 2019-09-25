#' A wrapper function to perform quality checks on capture data
#'
#' A wrapper that runs all single checks related to `Capture_data`.
#'
#' \strong{Capture check 1}: Check if the formats of each column in `Capture_data` match with the standard format using \code{\link{check_format_capture}}.
#' \strong{Capture check 2}: Check capture variable values against reference values using \code{\link{check_values_capture}}.
#'
#' @inheritParams checks_capture_params
#'
#' @return A list of 3 items: a summary data frame of all checks, a record-by-record list of warnings, and a record-by-record list of errors.
#'
#' @export

capture_check <- function(Capture_data){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(Check = c("Capture data format",
                                         "Improbable and impossible values capture data"),
                               Warning = NA,
                               Error = NA)

  # Checks
  # - Check format capture data
  message("Capture check 1: Checking format of capture data...")

  check_format_capture_output <- check_format_capture(Capture_data)

  check_list[1,2:3] <- check_format_capture_output$check_list

  # - Check capture variable values against reference values
  message("Capture check 2: Checking capture variable values against reference values...")

  check_values_capture_output <- check_values_capture(Capture_data)

  check_list[2,2:3] <- check_values_capture_output$check_list


  return(list(CheckList = check_list,
              CheckNames = check_list$Check,
              Warnings = list(
                Check1 = check_format_capture_output$warning_output,
                Check2 = check_values_capture_output$warning_output),
              Errors = list(
                Check1 = check_format_capture_output$error_output,
                Check2 = check_values_capture_output$error_output)
  ))
}

#' Check format of capture data
#'
#' Check if the formats of each column in the capture data match with the standard format
#' @inheritParams checks_capture_params
#'
#' @return Check list, warning output, error output.
#' @export

check_format_capture <- function(Capture_data){

  ## Data frame with column names and formats according to the standard protocol
  Capture_data_standard <- tibble::tibble(Variable = c("IndvID", "Species", "BreedingSeason",
                                                       "CaptureDate", "CaptureTime", "ObserverID",
                                                       "LocationID", "CapturePopID", "CapturePlot",
                                                       "ReleasePopID", "ReleasePlot",
                                                       "Mass", "Tarsus", "OriginalTarsusMethod",
                                                       "WingLength", "Age_obsverved", "Age_calculated", "ChickAge"),
                                          Format_standard = c("character", "character", "integer",
                                                              "Date", "character", "character",
                                                              "character", "character", "character",
                                                              "character", "character",
                                                              "numeric", "numeric", "character", "numeric",
                                                              "integer", "integer", "integer"))

  ## Data frame with column names and formats from Capture data
  Capture_data_col <- tibble::tibble(Variable = names(Capture_data),
                                     Format = unlist(purrr::pmap(list(Capture_data), class)))

  ## Mismatches between Capture data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Capture_data_mismatch <- dplyr::left_join(Capture_data_standard, Capture_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Capture_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Capture_data_mismatch$Variable,
                                .y = Capture_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Capture_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  # Capture_data_missing <- dplyr::left_join(Capture_data_standard, Capture_data_col, by="Variable") %>%
  #   filter(Format == "logical")
  #
  # war <- FALSE
  # warning_output <- NULL
  #
  # if(nrow(Capture_data_missing) > 0) {
  #   war <- TRUE
  #
  #   warning_output <- purrr::map(.x = Capture_data_missing$Variable,
  #                                .f = ~{
  #                                  paste0(.x, " in Capture_data is missing, unmeasured or undetermined (NA).")
  #                                })
  # }

  #Test for empty columns by looking at uniques, rather than using data type
  warning_output <- purrr::pmap(.l = list(as.list(Capture_data), colnames(Capture_data)),
                                .f = ~{

                                  if(all(is.na(unique(..1)))){

                                    return(paste0(..2, " in Capture_data is missing, unmeasured or undetermined (NA)."))

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


#' Check capture variable values against reference values
#'
#' Check variable values against species-specific reference values in capture data. Implausible values will result in a warning. Impossible values will result in an error. Variables that are checked: Mass, Tarsus, WingLength, Age_obsv, Age_calc, Chick_age.
#'
#' @inheritParams checks_capture_params
#'
#' @return Check list, warning output, error output.
#' @export

check_values_capture <- function(Capture_data) {

  # Separate adults and chicks, because they have different reference values
  Adult_data <- Capture_data %>%
    dplyr::filter(Age_calculated > 3)

  Chick_data <- Capture_data %>%
    dplyr::filter(Age_calculated <= 3)

  # Select reference values for species present in Adult_data and Chick_data
  species_adult <- unique(Adult_data$Species)
  ref_adult <- cap_adult_ref_values_list[which(names(cap_adult_ref_values_list) %in% species_adult)]

  species_chick <- unique(Chick_data$Species)
  ref_chick <- cap_chick_ref_values_list[which(names(cap_chick_ref_values_list) %in% species_chick)]

  # Structure Adult & Chick data in the same way as reference values: a list of species;  each species is a list of capture variables
  variables <- c("Mass", "Tarsus")

  ## Split per species
  Adult_data2 <- split(Adult_data[,c("Row", variables)], Adult_data$Species)
  Chick_data2 <- split(Chick_data[,c("Row", variables)], Chick_data$Species)

  ## Split per variable
  Adult_data2 <- purrr::map2(.x = Adult_data2,
                             .y = names(Adult_data2),
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

  Chick_data2 <- purrr::map2(.x = Chick_data2,
                             .y = names(Chick_data2),
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

  # # Add unique row identifier to capture data
  # Capture_data <- Capture_data %>%
  #   tibble::rownames_to_column(var = "RowID")
  #
  # # Select species in capture data
  # Capture_data <- Capture_data %>%
  #   dplyr::filter(Species == species)
  #
  # # Separate adults from chicks, as they have different reference values
  # Adult_data <- Capture_data %>%
  #   dplyr::filter(Age_calculated > 3)
  #
  # Chick_data <- Capture_data %>%
  #   dplyr::filter(Age_calculated <= 3)

  # Create warning & error list of variable-specific dataframes for
  # - adults

  # Capture-specific warning and error checks
  Adult_list <-  purrr::map2(.x = Adult_data2,
                             .y = ref_adult,
                             .f = ~purrr::map2(.x, .y,
                                               .f = ~{
                                                 .x %>%
                                                   dplyr::mutate(
                                                     Value = dplyr::pull(.x[,2]),
                                                     Variable = names(.x)[2],
                                                     Warning = ifelse((.x[,2] < dplyr::pull(.y[1,3])
                                                                        & .x[,2] >= dplyr::pull(.y[3,3])) # Lower bound
                                                                      | (.x[,2] > dplyr::pull(.y[2,3])
                                                                         & .x[,2] <= dplyr::pull(.y[4,3])), # Upper bound
                                                                      TRUE, FALSE),
                                                     Error = ifelse(.x[,2] < dplyr::pull(.y[3,3]) | .x[,2] > dplyr::pull(.y[4,3]),
                                                                    TRUE, FALSE)
                                                   )
                                               })
  )

  Chick_list <-  purrr::map2(.x = Chick_data2,
                             .y = ref_chick,
                             .f = ~purrr::map2(.x, .y,
                                               .f = ~{
                                                 .x %>%
                                                   dplyr::mutate(
                                                     Value = dplyr::pull(.x[,2]),
                                                     Variable = names(.x)[2],
                                                     Warning = ifelse((.x[,2] < dplyr::pull(.y[1,3])
                                                                       & .x[,2] >= dplyr::pull(.y[3,3])) # Lower bound
                                                                      | (.x[,2] > dplyr::pull(.y[2,3])
                                                                         & .x[,2] <= dplyr::pull(.y[4,3])), # Upper bound
                                                                      TRUE, FALSE),
                                                     Error = ifelse(.x[,2] < dplyr::pull(.y[3,3]) | .x[,2] > dplyr::pull(.y[4,3]),
                                                                    TRUE, FALSE)
                                                   )
                                               })
  )

  # Capture_list <- purrr::map2(.x = Adult_data[, c("Mass", "Tarsus")],
  #                             .y = ref_adult,
  #                             .f = ~{
  #                               tibble::tibble(IndvID = Adult_data$IndvID,
  #                                              RowID = Adult_data$RowID,
  #                                              Age = "Adult",
  #                                              Variable = names(.y)[3],
  #                                              .x) %>%
  #                                 dplyr::filter(!is.na(.x)) %>% # Only non-NA's
  #                                 dplyr::mutate(Warning = ifelse(.x > as.numeric(.y[2,3]),
  #                                                                TRUE, FALSE),
  #                                               Error = ifelse(.x < as.numeric(.y[3,3]) | .x > as.numeric(.y[4,3]),
  #                                                              TRUE, FALSE))
  #                             })
  # - and chicks
  # Chick_list <- purrr::map2(.x = Chick_data[, c("Mass", "Tarsus")],
  #                           .y = ref_chick,
  #                           .f = ~{
  #                             tibble::tibble(IndvID = Chick_data$IndvID,
  #                                            RowID = Chick_data$RowID,
  #                                            Age = "Chick",
  #                                            Variable = names(.y)[3],
  #                                            .x) %>%
  #                               dplyr::filter(!is.na(.x)) %>% # Only non-NA's
  #                               dplyr::mutate(Warning = ifelse(.x > as.numeric(.y[2,3]),
  #                                                              TRUE, FALSE),
  #                                             Error = ifelse(.x < as.numeric(.y[3,3]) | .x > as.numeric(.y[4,3]),
  #                                                            TRUE, FALSE))
  #                           })

  #Combine adults and chicks
  Capture_list <- purrr::map2(.x = Adult_list,
                              .y = Chick_list,
                              .f = ~purrr::map2(
                                .x,
                                .y,
                                .f = ~{
                                  dplyr::bind_rows(.x, .y)
                                }
                              ))

  # Capture_list <- purrr::map2(.x = Adult_list,
  #                             .y = Chick_list,
  #                             .f = ~{
  #                               dplyr::bind_rows(.x, .y)
  #                             })

  # Select records with errors (impossible values) from list
  Capture_err <- purrr::map(.x = Capture_list,
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

  # Capture_err <- purrr::map(.x = Capture_list,
  #                           .f = ~{
  #                             .x %>%
  #                               dplyr::filter(Error == TRUE)
  #                           }) %>%
  #   dplyr::bind_rows()

  err <- FALSE
  error_output <- NULL

  if(nrow(Capture_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Capture_err$Row,
                                          Capture_err$Variable,
                                          Capture_err$Value),
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (species: ", dplyr::pull(Capture_data[Capture_data$Row == ..1, "Species"]), ", ",
                                         "age: ", dplyr::pull(Capture_data[Capture_data$Row == ..1, "Age_calculated"]) ,")",
                                         " has an impossible value in ", ..2,
                                         " (", ..3, ").")
                                })
  }

  # Select records with warnings (improbable values) from list
  Capture_war <- purrr::map(.x = Capture_list,
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

  war <- FALSE
  warning_output <- NULL

  if(nrow(Capture_war) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Capture_war$Row,
                                            Capture_war$Variable,
                                            Capture_war$Value),
                                  .f = ~{
                                    paste0("Record on row ", ..1,
                                           " (species: ", dplyr::pull(Capture_data[Capture_data$Row == ..1, "Species"]), ", ",
                                           "age: ", dplyr::pull(Capture_data[Capture_data$Row == ..1, "Age_calculated"]) ,")",
                                           " has an improbable value in ", ..2,
                                           " (", ..3, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = unlist(warning_output),
              error_output = unlist(error_output)))

  # Satisfy RCMD Checks
  cap_adult_ref_values_list <- cap_chick_ref_values_list <- NULL
  Species <- Age_calc <- NULL

}
