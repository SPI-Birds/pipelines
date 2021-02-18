#' Perform quality checks on capture data
#'
#' A wrapper that runs all single checks related to \code{Capture_data}.
#'
#' The following capture data checks are performed:
#' \itemize{
#' \item \strong{C1}: Check if the formats of each column in \code{Capture_data} match with the standard format using \code{\link{check_format_capture}}.
#' \item \strong{C2a-b}: Check capture variable values against reference values using \code{\link{check_values_capture}}. Capture variables checked for adults and chicks: Mass and Tarsus.
#' \item \strong{C3}: Check chick age (in numbers of days since hatching) using \code{\link{check_chick_age}}.
#' \item \strong{C4}: Check that adults caught on nest are listed as parents using \code{\link{check_adult_parent_nest}}.
#' }
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_capture}}) is included in the quality check. Default: \code{TRUE}.
#'
#' @inherit checks_return return
#'
#' @export

capture_check <- function(Capture_data, Location_data, Brood_data, check_format=TRUE, approved_list){

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("C", c(1, paste0(2, letters[1:2]), 3:4)),
                               CheckDescription = c("Check format of capture data",
                                                    "Check mass values against reference values",
                                                    "Check tarsus values against reference values",
                                                    "Check chick age",
                                                    "Check that adults caught on nest are listed as the parents"),
                               Warning = NA,
                               Error = NA)

  # Checks
  message("Capture checks")

  # - Check format capture data
  if(check_format) {
    message("C1: Checking format of capture variables...")

    check_format_capture_output <- check_format_capture(Capture_data, approved_list)

    check_list[1,3:4] <- check_format_capture_output$CheckList
  }

  # - Check mass values against reference values
  message("C2a: Checking mass values against reference values...")

  check_values_mass_output <- check_values_capture(Capture_data, "Mass", approved_list)

  check_list[2,3:4] <- check_values_mass_output$CheckList

  # - Check tarsus values against reference values
  message("C2b: Checking tarsus values against reference values...")

  check_values_tarsus_output <- check_values_capture(Capture_data, "Tarsus", approved_list)

  check_list[3,3:4] <- check_values_tarsus_output$CheckList

  # - Check chick age values
  message("C3: Checking chick age values...")

  check_chick_age_output <- check_chick_age(Capture_data, approved_list)

  check_list[4,3:4] <- check_chick_age_output$CheckList

  # - Check that adults caught on nest are listed are the parents
  message("C4: Checking that adults caught on nest are listed are the parents...")

  check_adult_parent_nest_output <- check_adult_parent_nest(Capture_data, Location_data, Brood_data, approved_list)

  check_list[5,3:4] <- check_adult_parent_nest_output$CheckList

  if(check_format) {
    # Warning list
    warning_list <- list(Check1 = check_format_capture_output$WarningOutput,
                         Check2a = check_values_mass_output$WarningOutput,
                         Check2b = check_values_tarsus_output$WarningOutput,
                         Check3 = check_chick_age_output$WarningOutput,
                         Check4 = check_adult_parent_nest_output$WarningOutput)

    # Error list
    error_list <- list(Check1 = check_format_capture_output$ErrorOutput,
                       Check2a = check_values_mass_output$ErrorOutput,
                       Check2b = check_values_tarsus_output$ErrorOutput,
                       Check3 = check_chick_age_output$ErrorOutput,
                       CHeck4 = check_adult_parent_nest_output$ErrorOutput)
  } else {
    # Warning list
    warning_list <- list(Check2a = check_values_mass_output$WarningOutput,
                         Check2b = check_values_tarsus_output$WarningOutput,
                         Check3 = check_chick_age_output$WarningOutput,
                         Check4 = check_adult_parent_nest_output$WarningOutput)

    # Error list
    error_list <- list(Check2a = check_values_mass_output$ErrorOutput,
                       Check2b = check_values_tarsus_output$ErrorOutput,
                       Check3 = check_chick_age_output$ErrorOutput,
                       Check4 = check_adult_parent_nest_output$ErrorOutput)

    check_list <- check_list[-1,]
  }

  return(list(CheckList = check_list,
              WarningRows = unique(c(check_values_mass_output$WarningRows,
                                     check_values_tarsus_output$WarningRows,
                                     check_chick_age_output$WarningRows,
                                     check_adult_parent_nest_output$WarningRows)),
              ErrorRows = unique(c(check_values_mass_output$ErrorRows,
                                   check_values_tarsus_output$ErrorRows,
                                   check_chick_age_output$ErrorRows,
                                   check_adult_parent_nest_output$ErrorRows)),
              Warnings = warning_list,
              Errors = error_list))
}

#' Check format of capture data
#'
#' Check that the format of each column in the capture data match with the standard format.
#'
#' Check ID: C1.
#'
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_format_capture <- function(Capture_data, approved_list){

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
  Capture_data_mismatch <- dplyr::left_join(Capture_data_standard, Capture_data_col, by = "Variable") %>%
    filter(.data$Format != "logical" & .data$Format_standard != .data$Format)

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

}


#' Check capture variable values against reference values
#'
#' Check variable values against population-species-specific reference values in capture data. Reference values are based on the data if the number of observations is sufficiently large. Records for population-species combinations that are too low in number are only compared to reference values that are not data generated (see Details below).
#'
#' \strong{Mass} \cr
#' Check ID: C2a \cr
#' \itemize{
#' \item{Adults}
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered unusual if they are smaller than the 1st percentile or larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as an error.}
#' }
#' \item{Chicks}
#' \itemize{
#' \item{Reference values for chicks are calculated for each age (in days). This function tries to fit a logistic growth model to determine reference values to each day. If this model fails, reference values are determined per age if the number of observations is sufficiently large (n >= 100). Records are considered unusual if they are smaller than the 1st percentile or larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error.}
#' \item{In case the logistic growth model fails and the number of observations for an age are too low (n < 100), records are considered impossible if they are negative, and will be flagged as an error.}
#' }
#' }
#'
#'
#' \strong{Tarsus} \cr
#' Check ID: C2b \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered unusual if they are smaller than the 1st percentile or larger than the 99th percentile, and will be flagged as a warning. Records are considered impossible if they are negative or larger than 4 times the 99th percentile, and will be flagged as an error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as an error.}
#' }
#'
#' @inheritParams checks_capture_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_capture <- function(Capture_data, var, approved_list) {

  # Stop if "var" is missing
  if(missing(var)) {

    stop("Please select a variable in Capture_data to check against reference values.")

  }

  # Tarsus reference values
  if(var == "Tarsus" & !all(is.na(Capture_data[,var]))) {

    # Create reference values for adults & chicks from data
    ref <- Capture_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)), !is.na(.data$Species)) %>%
      dplyr::mutate(Stage = dplyr::case_when(
        .data$Age_calculated > 3 ~ "Adult",
        TRUE ~ "Chick"
      )) %>%
      dplyr::group_by(.data$Species, .data$CapturePopID, .data$Stage) %>%
      dplyr::summarise(Warning_min = stats::quantile(!!rlang::sym(var), probs = 0.01, na.rm = TRUE),
                       Warning_max = stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE),
                       Error_min = 0,
                       Error_max = 4 * .data$Warning_max,
                       n = n(),
                       Logis = FALSE) %>%
      dplyr::rename(PopID = .data$CapturePopID)

    # Print message for population-species combinations with too low number of observations
    if(any(ref$n < 100)) {

      low_obs <- ref %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select(.data$Species, .data$PopID, .data$Stage)

      purrr::pwalk(.l = list(low_obs$Species,
                             low_obs$PopID,
                             low_obs$Stage,
                             rep(var, nrow(low_obs))),
                   .f = ~{

                     message(paste0("Number of ", tolower(..4), " records for ", tolower(..3), " in ", ..2, ": ", ..1,
                                    " is too low (< 100) to create reliable reference values."))

                   })

    }

  }

  # Mass reference values
  if(var == "Mass" & !all(is.na(Capture_data[,var]))) {

    # Create reference values for adults from data
    ref_adults <- Capture_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)), !is.na(.data$Species), .data$Age_calculated > 3) %>%
      dplyr::group_by(.data$Species, .data$CapturePopID) %>%
      dplyr::summarise(Stage = "Adult",
                       Warning_min = stats::quantile(!!rlang::sym(var), probs = 0.01, na.rm = TRUE),
                       Warning_max = stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE),
                       Error_min = 0,
                       Error_max = 4 * .data$Warning_max,
                       n = n(),
                       Logis = FALSE) %>%
      dplyr::rename(PopID = .data$CapturePopID)

    # Print message for population-species combinations with too low number of observations for adults
    if(any(ref_adults$n < 100)) {

      low_obs_adults <- ref_adults %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select(.data$Species, .data$PopID)

      purrr::pwalk(.l = list(low_obs_adults$Species,
                             low_obs_adults$PopID,
                             rep(var, nrow(low_obs_adults))),
                   .f = ~{

                     message(paste0("Number of adult ", ..3, " records for ", ..2, ": ", ..1,
                                    " is too low (< 100) to create reliable reference values."))

                   })

    }

    # Create reference values for chicks from data
    # Calculate reference values using calculate_chick_mass_cutoffs
    # or calculate reference values per age if logistic model cannot be fitted.
    ref_chicks <- Capture_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)), !is.na(.data$Species), .data$Age_calculated <= 3) %>%
      dplyr::group_split(.data$Species, .data$CapturePopID) %>%
      purrr::pmap_dfr(.l = list(.), .f = ~{

        # Try to fit logistic growth model
        out <- tryCatch(
          expr = {

            message(paste0("Trying to fit a logistic growth model to calculate reference values for ",
                           unique(..1$CapturePopID), ": ", unique(..1$Species), "..."))

            calculate_chick_mass_cutoffs(..1)

          },
          # If fails, calculate age-specific reference values in the same way as adult reference values are created
          error = function(e){

            message("FAILED. Could not fit logistic growth model, because of error:")

            message(paste0(e)) # Print error

            message(paste0("Chick mass reference values for ", unique(..1$CapturePopID), ": ", unique(..1$Species),
                           " are created per age instead."))

            ..1 %>%
              dplyr::mutate(Stage = as.character(.data$ChickAge)) %>%
              dplyr::group_by(.data$Species, .data$CapturePopID, .data$Stage) %>%
              dplyr::summarise(Warning_min = stats::quantile(!!rlang::sym(var), probs = 0.01, na.rm = TRUE),
                               Warning_max = stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE),
                               Error_min = 0,
                               Error_max = 4 * .data$Warning_max,
                               n = n(),
                               Logis = FALSE) %>%
              dplyr::rename(PopID = .data$CapturePopID)

          })

        return(out)

      })

    # Print message for population-species combinations with too low number of observations for chicks
    if(any(ref_chicks$Logis == FALSE & ref_chicks$n < 100)) {

      low_obs_chicks <- ref_chicks %>%
        dplyr::filter(.data$Logis == FALSE & .data$n < 100) %>%
        dplyr::select(.data$Species, .data$PopID, .data$Stage)

      purrr::pwalk(.l = list(low_obs_chicks$Species,
                             low_obs_chicks$PopID,
                             low_obs_chicks$Stage),
                   .f = ~{

                     message(paste0("Number of mass records for ", ..3, "-day-old chicks in ", ..2, ": ", ..1,
                                    " is too low (< 100) to create reliable reference values."))

                   })

    }

    # Filter and arrange for errors
    if(nrow(ref_adults) > 0) {

      ref_adults_f <- ref_adults %>%
        dplyr::arrange(.data$PopID, .data$Species)

    }

    if(nrow(ref_chicks) > 0) {

      ref_chicks_f <- ref_chicks %>%
        dplyr::filter(!is.na(.data$Stage) | !(.data$Logis == FALSE)) %>%
        dplyr::arrange(.data$PopID, .data$Species)

    }

    # Combine reference values for adults and chicks, or only select the reference values that have been created
    if(nrow(ref_adults) > 0 & nrow(ref_chicks) > 0) {

      ref <- dplyr::bind_rows(ref_adults_f, ref_chicks_f)

    } else if(nrow(ref_adults) > 0 & nrow(ref_chicks) == 0) {

      ref <- ref_adults_f

    } else if(nrow(ref_adults) == 0 & nrow(ref_chicks) > 0) {

      ref <- ref_chicks_f

    }

  }

  # Set error and warning trackers and objects
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(ref) > 0 & !all(is.na(Capture_data[,var]))) {

    # Create progress bar
    pb <- progress::progress_bar$new(total = 2*nrow(ref),
                                     format = "[:bar] :percent ~:eta remaining",
                                     clear = FALSE)

    # Not all chicks have a recorded ChickAge. To still verify their values against reference values,
    # we create a new Column with CurrentChickAge, which follows the following rules:
    Capture_data <- Capture_data %>%
      dplyr::mutate(CurrentChickAge = dplyr::case_when(
        .data$Age_calculated > 3 ~ NA_real_, # Adults have no chick age
        .data$Age_calculated == 3 ~ as.numeric(max(Capture_data$ChickAge, na.rm = TRUE)), # Chicks age 3 have the maximum chick age
        .data$Age_calculated == 1 & is.na(.data$ChickAge) ~ 14, # Chicks age 1 and unknown chick age: 14
        .data$Age_calculated == 1 & .data$ChickAge < 0 ~ 14, # Chicks age 1 and impossible chick age: 14
        TRUE ~ as.numeric(.data$ChickAge) # Remaining chicks (chicks with known chick age) get recorded chick age
      )) %>%
      dplyr::mutate(CaptureID = as.character(1:nrow(Capture_data))) ##FIXME: remove after merge with master

    # Errors
    Capture_err <- purrr::pmap(.l = ref,
                               .f = ~{

                                 pb$tick()

                                 if(..3 == "Adult") {

                                   # If number of observations is large enough, compare brood values
                                   # to all reference values
                                   if(..8 >= "100") {

                                     Capture_data %>%
                                       dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                     (!!rlang::sym(var) < ..6 | !!rlang::sym(var) > ..7)) %>%
                                       dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                       dplyr::mutate(Variable = var)

                                     # If number of observations is too low, only compare brood values
                                     # to reference values not based on quantiles
                                   } else {

                                     Capture_data %>%
                                       dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                     !!rlang::sym(var) < ..6) %>%
                                       dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                       dplyr::mutate(Variable = var)

                                   }

                                 } else if(..3 != "Adult") {

                                   # If logistic model failed and number of observations is too low, only compare brood values
                                   # to reference values not based on quantiles
                                   if(..9 == FALSE & ..8 < 100) {

                                     Capture_data %>%
                                       dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$CurrentChickAge == ..3,
                                                     !!rlang::sym(var) < ..6) %>%
                                       dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                       dplyr::mutate(Variable = var)

                                     # Else, compare to all reference values
                                   } else {

                                     Capture_data %>%
                                       dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$CurrentChickAge == ..3,
                                                     (!!rlang::sym(var) < ..6 | !!rlang::sym(var) > ..7)) %>%
                                       dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                       dplyr::mutate(Variable = var)

                                   }


                                 }

                               }) %>%
      dplyr::bind_rows()

    if(nrow(Capture_err) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- Capture_err %>%
        dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID")) %>%
        dplyr::arrange(.data$Row)

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..2, "; ",
                                           "CaptureID: ", ..3, "; ",
                                           species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                           " has an impossible value in ", ..6, " (", ..4, ").")

                                  })

    }

    # Warnings
    # Warnings are only checked for population-species combinations with at least 100 observations
    warning_ref <- ref %>%
      dplyr::filter((.data$Stage == "Adult" & n >= 100) | (.data$Logis == TRUE) | (.data$Logis == FALSE & .data$n >= 100))

    Capture_war <- purrr::pmap(.l = warning_ref,
                               .f = ~{

                                 pb$tick()

                                 if(..3 == "Adult") {

                                   Capture_data %>%
                                     dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                   ((!!rlang::sym(var) < ..4 & !!rlang::sym(var) >= ..6) | (!!rlang::sym(var) > ..5 & !!rlang::sym(var) <= ..7))) %>%
                                     dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                     dplyr::mutate(Variable = var)

                                 } else if(..3 != "Adult") {

                                   Capture_data %>%
                                     dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$CurrentChickAge == ..3,
                                                   ((!!rlang::sym(var) < ..4 & !!rlang::sym(var) >= ..6) | (!!rlang::sym(var) > ..5 & !!rlang::sym(var) <= ..7))) %>%
                                     dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, !!rlang::sym(var), .data$Species) %>%
                                     dplyr::mutate(Variable = var)

                                 }

                               }) %>%
      dplyr::bind_rows()

    if(nrow(Capture_war) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- Capture_war %>%
        dplyr::mutate(CheckID = checkID_variable_combos[checkID_variable_combos$Variable == var,]$CheckID) %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID")) %>%
        dplyr::arrange(.data$Row)

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

    # Add messages about skipped population-species combinations to warning outputs
    if(var == "Tarsus") {

      low_obs <- ref %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select(.data$Species, .data$PopID, .data$Stage)

      skipped_output <- purrr::pmap(.l = list(low_obs$Species,
                                              low_obs$PopID,
                                              low_obs$Stage),
                                    .f = ~{

                                      paste0("Number of records for ", species_codes[species_codes$Species == ..1, "CommonName"],
                                             " ", paste0(tolower(..3), "s"), " in ", ..2,
                                             ", is too low to create reliable reference values, so records are only checked for impossible/negative values.")

                                    })

      warning_output <- c(skipped_output, warning_output)

    }

    if(var == "Mass") {

      if(any(ref_adults$n < 100, is.na(ref_chicks$Stage))) {

        low_obs_adults <- ref_adults %>%
          dplyr::filter(.data$n < 100) %>%
          dplyr::select(.data$Species, .data$PopID) %>%
          dplyr::mutate(Stage = "adults")

        skipped_adults_output <- purrr::pmap(.l = list(low_obs_adults$Species,
                                                       low_obs_adults$PopID,
                                                       low_obs_adults$Stage),
                                             .f = ~{

                                               paste0("Number of records for ", species_codes[species_codes$Species == ..1, "CommonName"],
                                                      " ", ..3, " in ", ..2,
                                                      ", is too low to create reliable reference values, so records are only checked for impossible/negative values.")

                                             })

        low_obs_chicks <- ref_chicks %>%
          dplyr::filter(is.na(.data$Stage) | (.data$Logis == FALSE & .data$n < 100)) %>%
          dplyr::select(.data$Species, .data$PopID)

        skipped_chicks_output <- purrr::pmap(.l = list(low_obs_chicks$Species,
                                                       low_obs_chicks$PopID,
                                                       low_obs_chicks$Stage),
                                             .f = ~{

                                               paste0("Number of records for ", species_codes[species_codes$Species == ..1, "CommonName"],
                                                      " ", ..3, "-day-old chicks in ", ..2,
                                                      ", is too low to create reliable reference values, so records are only checked for impossible/negative values.")

                                             })

        warning_output <- c(skipped_adults_output, skipped_chicks_output, warning_output)

      }

    }

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

#' Check chick age
#'
#' Check whether chick ages (in number of days since hatching) are within the range of 0 and 30 days since hatching. Values outside this range will result in an error.
#'
#' Check ID: C3.
#'
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_chick_age <- function(Capture_data, approved_list){

  # Errors
  # Select records with chick age < 0 OR > 30
  chick_age_err <- Capture_data %>%
    dplyr::filter(.data$ChickAge < 0 | .data$ChickAge > 30) %>% #TODO: make species-specific
    dplyr::select(.data$Row, PopID = .data$CapturePopID, .data$CaptureID, .data$Species, .data$ChickAge)

  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(nrow(chick_age_err) > 0) {

    err <- TRUE

    # Compare to approved_list
    error_records <- chick_age_err %>%
      dplyr::mutate(CheckID = "C3") %>%
      dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

    # Create quality check report statements
    error_output <- purrr::pmap(.l = error_records,
                                .f = ~{

                                  paste0("Record on row ", ..1,
                                         " (PopID: ", ..2,
                                         "; CaptureID: ", ..3, ", ", species_codes[species_codes$Species == ..4, "CommonName"], ")",
                                         " has an impossible value in ChickAge (", ..5, "). Impossible chick age may be caused by problems with hatch date.")

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


#' Check that adults captured on a nest are listed as parents of that nest
#'
#' Check that adults captured on a nest are are listed as the parents of that nest in Brood_data. If not, records will be flagged as a warning. Adults can be caught near a nest box.
#'
#' Check ID: C4.
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

check_adult_parent_nest <- function(Capture_data, Location_data, Brood_data, approved_list){

  # Select adults
  adults <- Capture_data %>%
    dplyr::filter(.data$Age_calculated > 3)

  # Determine location type of their capture locations
  pb1 <- progress::progress_bar$new(total = nrow(adults),
                                    format = "[:bar] :percent ~:eta remaining",
                                    clear = FALSE)

  location_type <- purrr::pmap(.l = list(adults$LocationID,
                                         adults$BreedingSeason,
                                         adults$CapturePopID),
                               .f = ~{

                                 pb1$tick()

                                 Location_type <- Location_data %>%
                                   dplyr::filter(.data$LocationID == ..1
                                                 & .data$StartSeason <= ..2
                                                 & (.data$EndSeason >= ..2 | is.na(.data$EndSeason))
                                                 & .data$PopID == ..3) %>%
                                   dplyr::pull(.data$LocationType)

                                 ifelse(is.null(Location_type), NA, Location_type)

                               })

  # Add location type to adults data frame and filter captures on nest box
  adults_nest_box <- adults %>%
    dplyr::mutate(LocationType = unlist(location_type)) %>%
    dplyr::filter(LocationType == "NB")

  # Check whether adults caught in nest box are associated with that nest in Brood data
  pb2 <- progress::progress_bar$new(total = nrow(adults_nest_box),
                                    format = "[:bar] :percent ~:eta remaining",
                                    clear = FALSE)

  parents_nests <- purrr::pmap_lgl(.l = list(adults_nest_box$CapturePopID,
                                             adults_nest_box$BreedingSeason,
                                             adults_nest_box$LocationID,
                                             adults_nest_box$IndvID),
                                   .f = ~{

                                     pb2$tick()

                                     Brood_parents <- Brood_data %>%
                                       dplyr::filter(.data$PopID == ..1
                                                     & .data$BreedingSeason == ..2
                                                     &  .data$LocationID == ..3) %>%
                                       dplyr::select(.data$FemaleID, .data$MaleID)

                                     ..4 %in% Brood_parents$FemaleID | ..4 %in% Brood_parents$MaleID
                                   })

  # Select adults caught in a nest box that are NOT associated with that nest
  unassociated_adults <- adults_nest_box[!parents_nests,] %>%
    dplyr::select(.data$Row, .data$CaptureID, .data$IndvID, PopID = .data$CapturePopID)

  # Warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(nrow(unassociated_adults) > 0) {

    war <- TRUE

    # Compare to approved_list
    warning_records <- unassociated_adults %>%
      dplyr::mutate(CheckID = "C4") %>%
      dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

    # Create quality check report statements
    warning_output <- purrr::pmap(.l = warning_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..4,
                                           "; CaptureID: ", ..2,
                                           "; IndvID: ", ..3, ")",
                                           " is caught on a location marked as a nest box but is not listed as the parent of that nest in Brood_data.")

                                  })

  }


  # No errors
  err <- FALSE
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}
