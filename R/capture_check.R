#' Perform quality checks on capture data
#'
#' A wrapper that runs all single checks related to \code{Capture_data}.
#'
#' The following capture data checks are performed:
#' \itemize{
#' \item \strong{C1}: Check if the formats of each column in \code{Capture_data} match with the standard format using \code{\link{check_format_capture}}.
#' \item \strong{C2a-b}: Check capture variable values against reference values using \code{\link{check_values_capture}}. Capture variables checked for adults and chicks: Mass and Tarsus.
#' #' \item \strong{C3}: Check chick age (in numbers of days since hatching) using \code{\link{check_chick_age}}.
#' }
#'
#' @inheritParams checks_capture_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_capture}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @inherit checks_return return
#'
#' @export

capture_check <- function(Capture_data, check_format=TRUE){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("C", c(1, paste0(2, letters[1:2]), 3)),
                               CheckDescription = c("Check format of capture data",
                                                    "Check mass values against reference values",
                                                    "Check tarsus values against reference values",
                                                    "Check chick age"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Capture checks")

  # - Check format capture data
  if(check_format) {
    message("C1: Checking format of capture variables...")

    check_format_capture_output <- check_format_capture(Capture_data)

    check_list[1,3:4] <- check_format_capture_output$CheckList
  }

  # - Check mass values against reference values
  message("C2a: Checking mass values against reference values...")

  check_values_mass_output <- check_values_capture(Capture_data, "Mass")

  check_list[2,3:4] <- check_values_mass_output$CheckList

  # - Check tarsus values against reference values
  message("C2b: Checking tarsus values against reference values...")

  check_values_tarsus_output <- check_values_capture(Capture_data, "Tarsus")

  check_list[3,3:4] <- check_values_tarsus_output$CheckList

  # - Check chick age values
  message("C3: Checking chick age values...")

  check_chick_age_output <- check_chick_age(Capture_data)

  check_list[4,3:4] <- check_chick_age_output$CheckList

  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_capture_output$WarningOutput,
                         Check2a = check_values_mass_output$WarningOutput,
                         Check2b = check_values_tarsus_output$WarningOutput,
                         Check3 = check_chick_age_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_capture_output$ErrorOutput,
                       Check2a = check_values_mass_output$ErrorOutput,
                       Check2b = check_values_tarsus_output$ErrorOutput,
                       Check3 = check_chick_age_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2a = check_values_mass_output$WarningOutput,
                         Check2b = check_values_tarsus_output$WarningOutput,
                         Check3 = check_chick_age_output$WarningOutput)

    # Error list
    error_list <- list(Check2a = check_values_mass_output$ErrorOutput,
                       Check2b = check_values_tarsus_output$ErrorOutput,
                       Check3 = check_chick_age_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              WarningRows = unique(c(check_values_mass_output$WarningRows,
                                     check_values_tarsus_output$WarningRows,
                                     check_chick_age_output$WarningRows)),
              ErrorRows = unique(c(check_values_mass_output$ErrorRows,
                                   check_values_tarsus_output$ErrorRows,
                                   check_chick_age_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Check format of capture data
#'
#' Check that the format of each column in the capture data match with the standard format.
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_format_capture <- function(Capture_data){

  ## Data frame with column names and formats according to the standard protocol
  Capture_data_standard <- tibble::tibble(Variable = c("Row", "IndvID", "Species", "BreedingSeason",
                                                       "CaptureDate", "CaptureTime", "ObserverID",
                                                       "LocationID", "CapturePopID", "CapturePlot",
                                                       "ReleasePopID", "ReleasePlot",
                                                       "Mass", "Tarsus", "OriginalTarsusMethod",
                                                       "WingLength", "Age_obsverved", "Age_calculated", "ChickAge"),
                                          Format_standard = c("integer", "character", "character", "integer",
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

  return(list(CheckList = check_list,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Check capture variable values against reference values
#'
#' Check variable values against species-specific reference values in capture data. Unusual values will result in a warning. Impossible values will result in an error. Variables that are checked for adults and chicks: Mass and Tarsus.
#'
#' @inheritParams checks_capture_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_capture <- function(Capture_data, var) {

  # Stop if var is missing
  if(missing(var)) {
    stop("Please select a variable in Capture_data to check against reference values.")
  }

  # Stop if var is given, but not a variable in Capture_data
  if(sum(stringr::str_detect(names(capture_ref_values), var)) == 0) {
    stop("The selected variable name is not in Capture_data. Perhaps you made a typo?")
  }

  # Select variable
  selected_ref_values <- capture_ref_values[stringr::str_detect(names(capture_ref_values), var)]

  # Reference values
  ref_names <- stringr::str_split(names(selected_ref_values), pattern="_")

  # Progress bar
  pb <- progress::progress_bar$new(total = 2*length(selected_ref_values),
                                   format = "[:bar] :percent ~:eta remaining",
                                   clear = FALSE)

  # Capture-specific errors
  Capture_err <- purrr::map2(.x = selected_ref_values,
                             .y = ref_names,
                             .f = ~{
                               pb$tick()

                               if(.y[2] == "Adult") {

                                 Capture_data %>%
                                   dplyr::filter(Age_calculated > 3) ->
                                   Capture_data2

                                 sel <- which(Capture_data2$Species == .y[1]
                                              & (Capture_data2[,which(colnames(Capture_data2) == .y[3])] < .x$Value[3]
                                                 | Capture_data2[,which(colnames(Capture_data2) == .y[3])] > .x$Value[4]))

                                 Capture_data2[sel,] %>%
                                   dplyr::select(Row, Value = !!.y[3], ChickAge) %>%
                                   dplyr::mutate(Species = .y[1],
                                                 Age = .y[2],
                                                 Variable = .y[3])

                                 } else if(.y[2] == "Chick") {

                                  Capture_data %>%
                                   dplyr::filter(Age_calculated <= 3) ->
                                   Capture_data2

                                   if(.y[3] == "Mass" & .y[1] %in% c("PARMAJ", "CYACAE")){

                                     Capture_data3 <- Capture_data2 %>%
                                       dplyr::filter(Species == .y[1])

                                     sel <- Capture_data3 %>%
                                       dplyr::filter(Species == .y[1]) %>%
                                       dplyr::mutate(error = purrr::pmap_lgl(.l = list(Mass, ChickAge, Age_calculated),
                                                                             .f = function(Mass, ChickAge, Age_calculated, Ref_values){

                                         if(Age_calculated == 3){

                                           current_chick_age <- 30

                                         } else {

                                           if(is.na(ChickAge) | ChickAge < 0 | ChickAge > 30){

                                             current_chick_age <- 14

                                           } else {

                                             current_chick_age <- ChickAge

                                           }

                                         }

                                         return(Mass < Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Error_min")] |
                                                  Mass > Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Error_max")])

                                       }, .x)) %>%
                                       pull(error) %>%
                                       which()

                                     Capture_data3[sel,] %>%
                                       dplyr::select(Row, Value = !!.y[3], ChickAge) %>%
                                       dplyr::mutate(Species = .y[1],
                                                     Age = .y[2],
                                                     Variable = .y[3])

                                   } else {

                                     sel <- which(Capture_data2$Species == .y[1]
                                                  & (Capture_data2[,which(colnames(Capture_data2) == .y[3])] < .x$Value[3]
                                                     | Capture_data2[,which(colnames(Capture_data2) == .y[3])] > .x$Value[4]))

                                     Capture_data2[sel,] %>%
                                       dplyr::select(Row, PopID = CapturePopID, CaptureID, Value = !!.y[3], ChickAge) %>%
                                       dplyr::mutate(Species = .y[1],
                                                     Age = .y[2],
                                                     Variable = .y[3])


                                   }

                               }
                             }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Species, Age, ChickAge, Variable)

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Capture_err) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Capture_err %>%
      dplyr::mutate(CheckID = checkID_var[checkID_var$Var == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (CaptureID: ", ..3, "; ",
                                         Species_codes[Species_codes$Code == ..6, "CommonName"], " ", ..7, ")",
                                         " has an impossible value in ", ..8, " (", ..4, ").")
                                })

  }

  # Capture-specific warnings
  Capture_war <- purrr::map2(.x = selected_ref_values,
                             .y = ref_names,
                             .f = ~{
                               pb$tick()

                               if(.y[2] == "Adult") {

                                 Capture_data %>%
                                   dplyr::filter(Age_calculated > 3) ->
                                   Capture_data2

                                 sel <- which(Capture_data2$Species == .y[1]
                                              & ((Capture_data2[,which(colnames(Capture_data2) == .y[3])] < .x$Value[1]
                                                  & Capture_data2[,which(colnames(Capture_data2) == .y[3])] >= .x$Value[3])
                                                 | (Capture_data2[,which(colnames(Capture_data2) == .y[3])] > .x$Value[2]
                                                    & Capture_data2[,which(colnames(Capture_data2) == .y[3])] <= .x$Value[4])))

                                 Capture_data2[sel,] %>%
                                   dplyr::select(Row, CaptureID, Value = !!.y[3], ChickAge) %>%
                                   dplyr::mutate(Species = .y[1],
                                                 Age = .y[2],
                                                 Variable = .y[3])

                               } else if(.y[2] == "Chick") {

                                 Capture_data %>%
                                   dplyr::filter(Age_calculated <= 3) ->
                                   Capture_data2

                                 if(.y[3] == "Mass" & .y[1] %in% c("PARMAJ", "CYACAE")){

                                   Capture_data3 <- Capture_data2 %>%
                                     dplyr::filter(Species == .y[1])

                                   sel <- Capture_data3 %>%
                                     dplyr::mutate(warning = purrr::pmap_lgl(.l = list(Mass, ChickAge, Age_calculated),
                                                                             .f = function(Mass, ChickAge, Age_calculated, Ref_values){

                                                                               if(Age_calculated == 3){

                                                                                 current_chick_age <- 30

                                                                               } else {

                                                                                 if(is.na(ChickAge) | ChickAge < 0 | ChickAge > 30){

                                                                                   current_chick_age <- 14

                                                                                 } else {

                                                                                   current_chick_age <- ChickAge

                                                                                 }

                                                                               }

                                                                               return((Mass < Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Warning_min")] & Mass >= Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Error_min")]) |
                                                                                        (Mass > Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Warning_max")] & Mass <= Ref_values$Value[which(Ref_values$ChickAge == current_chick_age & Ref_values$Reference == "Error_max")]))

                                                                             }, .x)) %>%
                                     pull(warning) %>%
                                     which()

                                   Capture_data3[sel,] %>%
                                     dplyr::select(Row, PopID = CapturePopID, CaptureID, Value = !!.y[3], ChickAge) %>%
                                     dplyr::mutate(Species = .y[1],
                                                   Age = .y[2],
                                                   Variable = .y[3])

                                 } else {

                                   sel <- which(Capture_data2$Species == .y[1]
                                                & ((Capture_data2[,which(colnames(Capture_data2) == .y[3])] < .x$Value[1]
                                                    & Capture_data2[,which(colnames(Capture_data2) == .y[3])] >= .x$Value[3])
                                                   | (Capture_data2[,which(colnames(Capture_data2) == .y[3])] > .x$Value[2]
                                                      & Capture_data2[,which(colnames(Capture_data2) == .y[3])] <= .x$Value[4])))

                                   Capture_data2[sel,] %>%
                                     dplyr::select(Row, PopID = CapturePopID, CaptureID, Value = !!.y[3], ChickAge) %>%
                                     dplyr::mutate(Species = .y[1],
                                                   Age = .y[2],
                                                   Variable = .y[3])

                                 }

                               }

                             }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Species, Age, ChickAge, Variable)

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(Capture_war) > 0) {
    war <- TRUE

    # Compare to approved_list
    warning_records <- Capture_war %>%
      dplyr::mutate(CheckID = checkID_var[checkID_var$Var == var,]$CheckID) %>%
      dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    if(..6 %in% c("PARMAJ", "CYACAE") & ..7 == "Chick" & ..8 == "Mass"){

                                      wmin <- capture_ref_values[[paste(..6, ..7, ..8, sep="_")]] %>%
                                        dplyr::filter(ChickAge == ..5 & Reference == "Warning_min") %>%
                                        dplyr::pull(Value)

                                      wmax <- capture_ref_values[[paste(..6, ..7, ..8, sep="_")]] %>%
                                        dplyr::filter(ChickAge == ..5 & Reference == "Warning_max") %>%
                                        dplyr::pull(Value)

                                      paste0("Record on row ", ..1,
                                             " (CaptureID: ", ..3, "; ",
                                             Species_codes[Species_codes$Code == ..6, "CommonName"], " ", tolower(..7), ")",
                                             " has an unusually ",
                                             ifelse(..4 < wmin,
                                                    paste0("low value in ", ..8, " (", ..4, " < ", round(wmin, 2)),
                                                    paste0("high value in ", ..8, " (", ..4, " > ", round(wmax, 2))),
                                             ") for its age (", ..5, ").")

                                    } else {

                                      paste0("Record on row ", ..1,
                                             " (CaptureID: ", ..3, "; ",
                                             Species_codes[Species_codes$Code == ..6, "CommonName"], " ", tolower(..7), ")",
                                             " has an unusually ",
                                             ifelse(..4 < capture_ref_values[[paste(..6, ..7, ..8, sep="_")]]$Value[1],
                                                    paste0("low value in ", ..8, " (", ..4, " < ",
                                                           capture_ref_values[[paste(..6, ..7, ..8, sep="_")]]$Value[1]),
                                                    paste0("high value in ", ..8, " (", ..4, " > ",
                                                           capture_ref_values[[paste(..6, ..7, ..8, sep="_")]]$Value[2])),
                                             ").")

                                    }

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
  capture_ref_values <- NULL
  Species <- Age_calc <- NULL
  Variable <- NULL
  approved_list <- checkID_var <- NULL
}


#' Check chick age
#'
#' Check whether chick ages (in number of days since hatching) are within the range of 0 and 30 days since hatching. Values outside this range will result in an error.
#'
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_chick_age <- function(Capture_data){

  # Errors
  # Select records with chick age < 0 OR > 30
  Chick_age_err <- Capture_data %>%
    dplyr::filter(ChickAge < 0 | ChickAge > 30) %>%
    dplyr::select(Row, PopID = CapturePopID, CaptureID, Species, ChickAge)

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(Chick_age_err) > 0) {
    err <- TRUE

    # Compare to approved_list
    error_records <- Chick_age_err %>%
      dplyr::mutate(CheckID = "C3") %>%
      dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{
                                  paste0("Record on row ", ..1,
                                         " (CaptureID: ", ..3, ", ", Species_codes[Species_codes$Code == ..4, "CommonName"], ")",
                                         " has an impossible value in ChickAge (", ..5, ").")
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
