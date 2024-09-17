#' Perform quality checks on capture data
#'
#' A wrapper that runs all single checks related to \code{Capture_data}.
#'
#' The following capture data checks are performed:
#' \itemize{
#' \item \strong{C1a-b}: Check capture variable values against reference values using \code{\link{check_values_capture}}. Capture variables checked for adults and chicks: Mass and Tarsus.
#' \item \strong{C2}: Check chick age (in numbers of days since hatching) using \code{\link{check_chick_age}}.
#' \item \strong{C3}: Check that adults caught on nest are listed as parents using \code{\link{check_adult_parent_nest}}.
#' \item \strong{C4}: Check that the age of subsequent captures of the same individual is correct using \code{\link{check_age_captures}}.
#' \item \strong{C5}: Check if individuals in Capture_data appear in Individual_data using \code{\link{check_captures_individuals}}.
#' \item \strong{C6}: Check that capture locations appear in Location_data using \code{\link{check_capture_locations}}.
#' }
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

capture_check <- function(Capture_data, Location_data, Brood_data, Individual_data, approved_list, output, skip){

  # Perform capture checks
  message("Checking capture data...")

  # Run checks and create list of check outputs
  check_outputs <- tibble::lst(C1a = check_values_capture(Capture_data, "Mass", approved_list, output, skip), # C1a: Check mass values against reference values
                               C1b = check_values_capture(Capture_data, "Tarsus", approved_list, output, skip), # C1b: Check tarsus values against reference values
                               C2 = check_chick_age(Capture_data, approved_list, output, skip), # C2: Check chick age values
                               C3 = check_adult_parent_nest(Capture_data, Location_data,
                                                            Brood_data, approved_list, output, skip), # C3: Check that adults caught on nest are listed are the parents
                               C4 = check_age_captures(Capture_data, approved_list, output, skip), # C4: Check the order in age of subsequent captures
                               C5 = check_captures_individuals(Capture_data, Individual_data,
                                                               approved_list, output, skip), # C5: Check that individuals in Capture_data also appear in Individual_data
                               C6 = check_capture_locations(Capture_data, Location_data,
                                                            approved_list, output, skip) # C6: Check that capture locations appear in Location_data
  )

  # Create check list with a summary of warnings and errors per check
  check_list <- tibble::tibble(CheckID = paste0("C", c(paste0(1, letters[1:2]), 2:6)),
                               CheckDescription = c("Check mass values against reference values",
                                                    "Check tarsus values against reference values",
                                                    "Check chick age",
                                                    "Check that adults caught on nest during the breeding season are listed as the parents",
                                                    "Check the order in age of subsequent captures",
                                                    "Check that individuals in Capture_data also appear in Individual_data",
                                                    "Check that capture locations appear in Location_data"),
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

#' Check capture variable values against reference values
#'
#' Check variable values against population-species-specific reference values in capture data. Reference values are based on the data if the number of observations is sufficiently large. Records for population-species combinations that are too low in number are only compared to reference values that are not data generated (see Details below).
#'
#' \strong{Mass} \cr
#' Check ID: C1a \cr
#' \itemize{
#' \item{Adults}
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered impossible if they are negative or larger than 2 times the 99th percentile, and will be flagged as a potential error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as a potential error.}
#' }
#' \item{Chicks}
#' \itemize{
#' \item{Reference values for chicks are calculated for each age (in days). This function tries to fit a logistic growth model to determine reference values to each day. If this model fails, reference values are determined per age if the number of observations is sufficiently large (n >= 100). Records are considered impossible if they are negative or larger than 2 times the 99th percentile, and will be flagged as a potential error.}
#' \item{In case the logistic growth model fails and the number of observations for an age are too low (n < 100), records are considered impossible if they are negative, and will be flagged as a potential error.}
#' }
#' }
#'
#'
#' \strong{Tarsus} \cr
#' Check ID: C1b \cr
#' \itemize{
#' \item{\emph{n >= 100}\cr}{Records are considered impossible if they are negative or larger than 2 times the 99th percentile, and will be flagged as a potential error.}
#' \item{\emph{n < 100}\cr}{Records are considered impossible if they are negative, and will be flagged as a potential error.}
#' }
#'
#' Note: when the number of observations is too low to generate reference values, a message is added to the list of warnings.
#'
#' @inheritParams checks_capture_params
#' @param var Character. Variable to check against reference values.
#'
#' @inherit checks_return return
#'
#' @export

check_values_capture <- function(Capture_data, var, approved_list, output, skip) {

  # Stop if {var} is missing
  if(missing(var)) {

    stop("Please select a variable in Capture_data to check against reference values.")

  }

  # Check whether this check needs to be skipped
  skip_check <- dplyr::case_when(var %in% c("Mass") & "C1a" %in% skip ~ TRUE,
                                 var %in% c("Tarsus") & "C1b" %in% skip ~ TRUE,
                                 "C1" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check messages
  if(skip_check == FALSE) {

    check_message <- dplyr::case_when(var %in% c("Mass") ~ c("C1a", "mass"),
                                      var %in% c("Tarsus") ~ c("C1b", "tarsus"))

    message(paste0(check_message[1], ": Checking ", check_message[2], " values against reference values..."))

  } else {

    id_message <- dplyr::case_when(var %in% c("Mass") ~ "C1a",
                                   var %in% c("Tarsus") ~ "C1b")

    message(paste0("<< ", id_message, " is skipped >>"))

  }

  # Check for each population & species if {var} was recorded
  var_recorded <- Capture_data %>%
    dplyr::filter(!is.na(.data$Species)) %>%
    dplyr::group_by(.data$CapturePopID, .data$Species) %>%
    dplyr::summarise(recorded = ifelse(!all(is.na(!!rlang::sym(var))), TRUE, FALSE),
                     .groups = "drop")

  var_not_recorded <- var_recorded %>%
    dplyr::filter(.data$recorded == FALSE)

  # Create reference values from data
  # Tarsus
  if(var == "Tarsus" & skip_check == FALSE & output %in% c("both", "errors") & any(var_recorded$recorded == TRUE)) {

    # Create reference values for adults & chicks
    ref <- Capture_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)), !is.na(.data$Species)) %>%
      dplyr::anti_join(var_not_recorded, by = c("CapturePopID", "Species")) %>%
      dplyr::mutate(Stage = dplyr::case_when(
        .data$Age_calculated > 3 ~ "Adult",
        TRUE ~ "Chick"
      )) %>%
      dplyr::group_by(.data$Species, .data$CapturePopID, .data$Stage) %>%
      dplyr::summarise(Error_min = 0,
                       Error_max = 2 * round(stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE), 1),
                       n = dplyr::n(),
                       Logis = FALSE) %>%
      dplyr::rename("PopID" = "CapturePopID") %>%
      dplyr::mutate_at(c("Error_min", "Error_max"), ~round(., 2))

    # Print message for population-species combinations with too low number of observations
    if(exists("ref") & any(ref$n < 100)) {

      low_obs <- ref %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select("Species", "PopID", "Stage")

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
  if(var == "Mass" & skip_check == FALSE & output %in% c("both", "errors") & any(var_recorded$recorded == TRUE)) {

    # Create reference values for adults from data
    ref_adults <- Capture_data %>%
      dplyr::filter(!is.na(!!rlang::sym(var)), !is.na(.data$Species), .data$Age_calculated > 3) %>%
      dplyr::anti_join(var_not_recorded, by = c("CapturePopID", "Species")) %>%
      dplyr::group_by(.data$Species, .data$CapturePopID) %>%
      dplyr::summarise(Stage = "Adult",
                       Error_min = 0,
                       Error_max = 2 * stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE),
                       n = dplyr::n(),
                       Logis = FALSE) %>%
      dplyr::rename("PopID" = "CapturePopID") %>%
      dplyr::mutate_at(c("Error_min", "Error_max"), ~round(., 2))

    # Print message for population-species combinations with too low number of observations for adults
    if(exists("ref_adults") & any(ref_adults$n < 100)) {

      low_obs_adults <- ref_adults %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select("Species", "PopID")

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
      dplyr::anti_join(var_not_recorded, by = c("CapturePopID", "Species")) %>%
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
              dplyr::summarise(Error_min = 0,
                               Error_max = 2 * stats::quantile(!!rlang::sym(var), probs = 0.99, na.rm = TRUE),
                               n = dplyr::n(),
                               Logis = FALSE) %>%
              dplyr::rename("PopID" = "CapturePopID")

          })

        return(out)

      })  %>%
      dplyr::mutate_at(c("Error_min", "Error_max"), ~round(., 2))

    # Print message for population-species combinations with too low number of observations for chicks
    if(exists("ref_chicks") & any(ref_chicks$Logis == FALSE & ref_chicks$n < 100)) {

      low_obs_chicks <- ref_chicks %>%
        dplyr::filter(.data$Logis == FALSE & .data$n < 100 & !is.na(.data$Stage)) %>%
        dplyr::select("Species", "PopID", "Stage")

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

  # Print message for population-species combinations for which {var} was not recorded
  if(skip_check == FALSE & nrow(var_not_recorded) > 0) {

    purrr::pwalk(.l = list(var_not_recorded$Species,
                           var_not_recorded$CapturePopID),
                 .f = ~{

                   message(paste0(var, " was not recorded for ", ..2, ": ", ..1, "."))

                 })

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE & exists("ref") & any(var_recorded$recorded == TRUE)) {

    if(nrow(ref) > 0 && !all(is.na(Capture_data[,var]))) {

      # Create progress bar
      pb <- progress::progress_bar$new(total = 2*nrow(ref),
                                       format = "[:bar] :percent ~:eta remaining")

      if(var == "Mass") {

        # Not all chicks have a recorded ChickAge. To still verify their values against age-specific reference values,
        # we create a new Column with CurrentChickAge, which follows the following rules:
        Capture_data <- Capture_data %>%
          dplyr::mutate(CurrentChickAge = dplyr::case_when(
            .data$Age_calculated > 3 ~ NA_real_, # Adults have no chick age
            .data$Age_calculated == 3 ~ as.numeric(max(Capture_data$ChickAge, na.rm = TRUE)), # Chicks age 3 have the maximum chick age
            .data$Age_calculated == 1 & is.na(.data$ChickAge) ~ 14, # Chicks age 1 and unknown chick age: 14
            .data$Age_calculated == 1 & .data$ChickAge < 0 ~ 14, # Chicks age 1 and impossible chick age: 14
            TRUE ~ as.numeric(.data$ChickAge) # Remaining chicks (chicks with known chick age) get recorded chick age
          ))

      } else if(var == "Tarsus") {

        Capture_data <- Capture_data %>%
          dplyr::mutate(CurrentChickAge = dplyr::case_when(.data$Age_calculated > 3 ~ "Adult",
                                                           TRUE ~ "Chick"))

      }

      Capture_err <- purrr::pmap(.l = ref,
                                 .f = ~{

                                   pb$tick()

                                   if(..3 == "Adult") {

                                     # If number of observations is large enough, compare brood values
                                     # to all reference values
                                     if(..6 >= "100") {

                                       # Capture records below lower error threshold
                                       lower_err <- Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "L",
                                                       Ref = ..4) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                       !!rlang::sym(var) < ..4) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                       # Capture records above upper error threshold
                                       upper_err <- Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "U",
                                                       Ref = ..5) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                       !!rlang::sym(var) > ..5) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                       dplyr::bind_rows(lower_err, upper_err)

                                       # If number of observations is too low, only compare brood values
                                       # to reference values not based on quantiles
                                     } else {

                                       # Capture records below lower error threshold
                                       Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "L",
                                                       Ref = ..4) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$Age_calculated > 3,
                                                       !!rlang::sym(var) < ..4) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                     }

                                   } else if(..3 != "Adult") {

                                     # If logistic model failed and number of observations is too low, only compare brood values
                                     # to reference values not based on quantiles
                                     if(..7 == FALSE & ..6 < 100) {

                                       # Capture records below lower error threshold
                                       Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "L",
                                                       Ref = ..4) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2, .data$CurrentChickAge == ..3,
                                                       !!rlang::sym(var) < ..4) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                       # Else, compare to all reference values
                                     } else {

                                       # Capture records below lower error threshold
                                       lower_err <- Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "L",
                                                       Ref = ..4) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2,
                                                       .data$CurrentChickAge == ..3, !!rlang::sym(var) < ..4) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                       # Capture records above upper error threshold
                                       upper_err <- Capture_data %>%
                                         dplyr::mutate(Variable = var,
                                                       Threshold = "U",
                                                       Ref = ..5) %>%
                                         dplyr::filter(.data$Species == ..1, .data$CapturePopID == ..2,
                                                       .data$CurrentChickAge == ..3, !!rlang::sym(var) > ..5) %>%
                                         dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", !!rlang::sym(var),
                                                       "Species", "Variable", "Threshold", "Ref")

                                       dplyr::bind_rows(lower_err, upper_err)

                                     }


                                   }

                                 }) %>%
        dplyr::bind_rows()

      # If potential errors, add to report
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
                                             " has a ", ifelse(..7 == "U", "larger", "smaller"), " value in ",
                                             ..6, " (", ..4, ") than the ", ifelse(..7 == "U", "upper", "lower"),
                                             " reference value (", ..8, "), which is considered impossible.")

                                    })

      }

    }

  }

  # No check for warnings
  war <- NA
  #warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE & any(var_recorded$recorded == TRUE)) {

    # Add messages about skipped population-species combinations to warning outputs & with no records for {var}
    if(var == "Tarsus") {

      low_obs <- ref %>%
        dplyr::filter(.data$n < 100) %>%
        dplyr::select("Species", "PopID", "Stage")

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
          dplyr::select("Species", "PopID") %>%
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
          dplyr::filter(.data$Logis == FALSE & .data$n < 100 & !is.na(.data$Stage)) %>%
          dplyr::select("Species", "PopID", "Stage")

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

    if(nrow(var_not_recorded) > 0) {

      not_recorded_output <- purrr::pmap(.l = list(var_not_recorded$Species,
                                                   var_not_recorded$CapturePopID),
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

#' Check chick age
#'
#' Check whether chick ages (in number of days since hatching) are within the range of 0 and 30 days since hatching. Values outside this range will be flagged as a potential error.
#'
#' Check ID: C2.
#'
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_chick_age <- function(Capture_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("C2" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("C2: Checking chick age values...")

  } else {

    message("<< C2 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select records with chick age < 0 OR > 30
    chick_age_err <- Capture_data %>%
      dplyr::filter(.data$ChickAge < 0 | .data$ChickAge > 30) %>% #TODO: make species-specific
      dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", "Species", "ChickAge")

    # If potential errors, add to report
    if(nrow(chick_age_err) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- chick_age_err %>%
        dplyr::mutate(CheckID = "C2") %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..2,
                                           "; CaptureID: ", ..3, ", ", species_codes[species_codes$Species == ..4, "CommonName"], ")",
                                           " has an impossible value in ChickAge (", ..5, ").")

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


#' Check that adults captured on a nest during the breeding season are listed as parents of that nest
#'
#' Check that adults captured on a nest during the breeding season are are listed as the parents of that nest in Brood_data. If not, records will be flagged as a warning. Adults can be caught near a nest box. Records outside the breeding season but on a nesting location are ignored. The breeding season is determined annually and lasts from the minimum lay date in Brood_data that year to the maximum fledge date in Brood_data that year. If lay dates are not recorded in a population, the start of the breeding season is set to 1st of March; if fledge dates are not recorded, the end of the breeding season is set to 31st of August.
#'
#' Check ID: C3.
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#' @inheritParams checks_brood_params
#'
#' @inherit checks_return return
#'
#' @export

check_adult_parent_nest <- function(Capture_data, Location_data, Brood_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("C3" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("C3: Checking that adults caught on nest during the breeding season are listed as the parents...")

  } else {

    message("<< C3 is skipped >>")

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Select adults captured in the "breeding season"
    # Here we define a population's breeding season annually, which, for now, runs from the minimum lay date in Brood_data,
    # to the maximum fledge date in Brood_data (both in Julian day/day of the year)
    # TODO: Note that errors in either lay date or fledge date permeate here.
    # FIXME: Work-around when *ALL* FledgeDate or LayDate are NA
    # TODO: Update breeding season definition for birds breeding in winter, in the tropics, or the Southern Hemisphere
    if(all(c("LayDate", "FledgeDate") %in% colnames(Brood_data))) {

      breeding_season <- Brood_data %>%
        dplyr::group_by(.data$PopID, .data$BreedingSeason) %>%
        # If all Lay dates are NA, the start of the breeding season is set to March 1st
        # If all Fledge dates are NA, the end of the breeding season is set to August 31st
        dplyr::summarise(StartBreeding = ifelse(all(is.na(.data$LayDate)), lubridate::yday(paste0(unique(.data$BreedingSeason), "-03-01")),
                                                lubridate::yday(min(.data$LayDate, na.rm = TRUE))),
                         EndBreeding = ifelse(all(is.na(.data$FledgeDate)), lubridate::yday(paste0(unique(.data$BreedingSeason), "-08-31")),
                                              lubridate::yday(max(.data$FledgeDate, na.rm = TRUE))),
                         .groups = "drop") %>%
        dplyr::group_by(.data$PopID) %>%
        # Years with missing start/end of breeding season, take the earliest or latest value across years, respectively
        dplyr::mutate(StartBreeding = dplyr::case_when(is.na(.data$StartBreeding) ~ min(.data$StartBreeding, na.rm = TRUE),
                                                       TRUE ~ .data$StartBreeding),
                      EndBreeding = dplyr::case_when(is.na(.data$EndBreeding) ~ max(.data$EndBreeding, na.rm = TRUE),
                                                     TRUE ~ .data$EndBreeding)) %>%
        dplyr::ungroup()

    } else {

      breeding_season <- Brood_data %>%
        dplyr::group_by(.data$PopID, .data$BreedingSeason) %>%
        # If all Lay dates are NA, the start of the breeding season is set to March 1st
        # If all Fledge dates are NA, the end of the breeding season is set to August 31st
        dplyr::summarise(StartBreeding = ifelse(all(is.na(.data$LayDate_observed)), lubridate::yday(paste0(unique(.data$BreedingSeason), "-03-01")),
                                                lubridate::yday(min(.data$LayDate_observed, na.rm = TRUE))),
                         EndBreeding = ifelse(all(is.na(.data$FledgeDate_observed)), lubridate::yday(paste0(unique(.data$BreedingSeason), "-08-31")),
                                              lubridate::yday(max(.data$FledgeDate_observed, na.rm = TRUE))),
                         .groups = "drop") %>%
        dplyr::group_by(.data$PopID) %>%
        # Years with missing start/end of breeding season, take the earliest or latest value across years, respectively
        dplyr::mutate(StartBreeding = dplyr::case_when(is.na(.data$StartBreeding) ~ min(.data$StartBreeding, na.rm = TRUE),
                                                       TRUE ~ .data$StartBreeding),
                      EndBreeding = dplyr::case_when(is.na(.data$EndBreeding) ~ max(.data$EndBreeding, na.rm = TRUE),
                                                     TRUE ~ .data$EndBreeding)) %>%
        dplyr::ungroup()

    }

    adults <- Capture_data %>%
      dplyr::left_join(breeding_season, by = c("BreedingSeason", "CapturePopID" = "PopID")) %>%
      dplyr::filter(.data$Age_calculated > 3,
                    lubridate::yday(.data$CaptureDate) >= .data$StartBreeding,
                    lubridate::yday(.data$CaptureDate) <= .data$EndBreeding)

    # Determine location type of their capture locations
    # Duplicate location rows according to the number of years they were used for easy joining with adult data
    annual_locations <- Location_data %>%
      dplyr::mutate(EndSeason = dplyr::case_when(is.na(.data$EndSeason) ~ as.integer(lubridate::year(Sys.Date())),
                                                 !is.na(.data$EndSeason) ~ .data$EndSeason),
                    StartSeason = dplyr::case_when(is.na(.data$StartSeason) ~ min(.data$StartSeason, na.rm = TRUE),
                                                   !is.na(.data$StartSeason) ~ .data$StartSeason)) %>%
      tidyr::uncount(weights = .data$EndSeason - .data$StartSeason + 1) %>%
      dplyr::group_by(.data$Row) %>%
      dplyr::mutate(BreedingSeason = .data$StartSeason + dplyr::row_number() - 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"Row")

    # Add location type to adults data frame and filter captures on nest box
    adults_nest_box <- adults %>%
      dplyr::left_join(annual_locations, by = c("CapturePopID" = "PopID", "LocationID", "BreedingSeason")) %>%
      dplyr::filter(.data$LocationType == "NB")

    # Check whether adults caught in nest box are associated with that nest in Brood data
    females_w_nests <- adults_nest_box %>%
      dplyr::semi_join(Brood_data,
                       by = c("CapturePopID" = "PopID", "BreedingSeason", "LocationID", "IndvID" = "FemaleID"))

    males_w_nests <- adults_nest_box %>%
      dplyr::semi_join(Brood_data,
                       by = c("CapturePopID" = "PopID", "BreedingSeason", "LocationID", "IndvID" = "MaleID"))

    # Select adults caught in a nest box that are NOT associated with that nest
    unassociated_adults <- adults_nest_box %>%
      dplyr::anti_join(females_w_nests, by = c("CapturePopID", "BreedingSeason", "LocationID", "IndvID")) %>%
      dplyr::anti_join(males_w_nests, by = c("CapturePopID", "BreedingSeason", "LocationID", "IndvID")) %>%
      dplyr::select("Row", "CaptureID", "IndvID", "PopID" = "CapturePopID")

    # If warnings, add to report
    if(nrow(unassociated_adults) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- unassociated_adults %>%
        dplyr::mutate(CheckID = "C3") %>%
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

  }

  # No check for errors
  err <- NA
  #error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  # Count number of records flagged
  warning_count <- sum(!is.na(warning_records$Row))

  check_list <- tibble::tibble(Warning = war,
                               Error = err,
                               WarningRecords = warning_count,
                               ErrorRecords = NA_integer_,
                               Skipped = skip_check)

  return(list(CheckList = check_list,
              WarningRows = warning_records$Row,
              ErrorRows = NULL,
              WarningOutput = unlist(warning_output),
              ErrorOutput = unlist(error_output)))

  # Satisfy RCMD checks
  approved_list <- NULL

}


#' Check age of subsequent captures
#'
#' Check that the observed age of chronologically ordered captures is correct. Individuals who have a record as a chick in the same or a later year than a record of an adult will be flagged as a potential error. Other cases where the age of the subsequent capture is not equal or greater than the previous will be flagged as a warning.
#'
#' Check ID: C4.
#'
#' @inheritParams checks_capture_params
#'
#' @inherit checks_return return
#'
#' @export

check_age_captures <- function(Capture_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("C4" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("C4: Checking that the age of subsequent captures is ordered correctly...")

  } else {

    message("<< C4 is skipped >>")

  }

  # Check for warnings
  war <- FALSE
  warning_records <- tibble::tibble(Row = NA_character_)
  warning_output <- NULL

  if(output %in% c("both", "warnings") & skip_check == FALSE) {

    # Select records of captures with an age larger than the subsequent capture.
    # This may happen when age determination and uncertainty vary over time, but should be flagged as a warning.
    wrong_age_order <- Capture_data %>%
      dplyr::group_by(.data$CapturePopID, .data$IndvID) %>%
      dplyr::arrange(.data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
      dplyr::mutate(Age_observed_next = dplyr::lead(.data$Age_observed)) %>%
      dplyr::filter(
        .data$Age_observed > .data$Age_observed_next &
          ((.data$Age_observed %in% c(1, 3) & .data$Age_observed_next %in% c(1, 3)) | # chicks
             ((.data$Age_observed == 2 | .data$Age_observed >= 4) & # adults
                (.data$Age_observed_next == 2 | .data$Age_observed_next >= 4))
          )
      ) %>%
      dplyr::select("Row", "PopID" = "CapturePopID", "IndvID", "CaptureID",
                    "Species", "Age_observed", "Age_observed_next")

    # If warnings, add to report
    if(nrow(wrong_age_order) > 0) {

      war <- TRUE

      # Compare to approved_list
      warning_records <- wrong_age_order %>%
        dplyr::mutate(CheckID = "C4") %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

      # Create quality check report statements
      warning_output <- purrr::pmap(.l = warning_records,
                                    .f = ~{

                                      paste0("Record on row ", ..1,
                                             " (PopID: ", ..2,
                                             "; IndvID: ", ..3,
                                             "; CaptureID: ", ..4, "; ", species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                             " has an older observed age (Age: ", ..6, ") than the next capture of this individual (Age: ", ..7, ").")

                                    })

    }

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select records of chick captures that happened after adult captures of the same individual
    # This is impossible and should be flagged as an error.
    chicks_caught_after_adults <- Capture_data %>%
      dplyr::group_by(.data$CapturePopID, .data$IndvID) %>%
      dplyr::arrange(.data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
      dplyr::mutate(Age_observed_next = dplyr::lead(.data$Age_observed)) %>%
      dplyr::filter(.data$Age_observed > .data$Age_observed_next &
                      (.data$Age_observed == 2 | .data$Age_observed >= 4) &
                      .data$Age_observed_next %in% c(1, 3)) %>%
      dplyr::select("Row", "PopID" = "CapturePopID", "IndvID", "CaptureID",
                    "Species", "Age_observed", "Age_observed_next")

    # If potential errors, add to report
    if(nrow(chicks_caught_after_adults) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- chicks_caught_after_adults %>%
        dplyr::mutate(CheckID = "C4") %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..2,
                                           "; IndvID: ", ..3,
                                           "; CaptureID: ", ..4, "; ", species_codes[species_codes$Species == ..5, "CommonName"], ")",
                                           " has been caught as an adult (Age: ", ..6, ") before it was caught as a chick (Age: ", ..7, ").")

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

#' Check that all individuals captured in Capture_data appear in Individual_data
#'
#' Check that all individuals recorded in Capture_data have a record in Individual_data. Missing individuals should never occur, because Individual_data is usually a direct product of Capture_data (i.e., all unique individuals from Capture_data). Missing individuals will be flagged as a potential error. If there are any missing individuals, the SPI-Birds team needs to check the pipeline code. This check is the opposite of check I5 (\code{\link{check_individuals_captures}}).
#'
#' Check ID: C5.
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_individual_params
#'
#' @inherit checks_return return
#'
#' @export

check_captures_individuals <- function(Capture_data, Individual_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("C5" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("C5: Checking that individuals in Capture_data also appear in Individual_data...")

  } else {

    message("<< C5 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select individuals that are missing from Individual_data
    missing_individuals <- purrr::map(.x = unique(Capture_data$CapturePopID),
                                      .f = ~{

                                        dplyr::anti_join({Capture_data %>% dplyr::filter(!is.na(.data$IndvID) & .data$CapturePopID == .x)},
                                                         {Individual_data %>% dplyr::filter(.data$PopID == .x)},
                                                         by = "IndvID")

                                      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", "IndvID")

    # If potential errors, add to report
    if(nrow(missing_individuals) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- missing_individuals %>%
        dplyr::mutate(CheckID = "C5") %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1,
                                           " (PopID: ", ..2,
                                           "; CaptureID: ", ..3,
                                           "; IndvID: ", ..4, ")",
                                           " does not appear in Individual_data.")

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


#' Check that all capture locations in Capture_data appear in Location_data
#'
#' Check that all capture locations recorded in Capture_data appear in Location_data. Missing locations will be flagged as a potential error.
#'
#' Check ID: C6.
#'
#' @inheritParams checks_capture_params
#' @inheritParams checks_location_params
#'
#' @inherit checks_return return
#'
#' @export

check_capture_locations <- function(Capture_data, Location_data, approved_list, output, skip){

  # Check whether this check should be skipped
  skip_check <- dplyr::case_when("C6" %in% skip ~ TRUE,
                                 TRUE ~ FALSE)

  # Print check message
  if(skip_check == FALSE) {

    message("C6: Checking that capture locations appear in Location_data...")

  } else {

    message("<< C6 is skipped >>")

  }

  # Check for potential errors
  err <- FALSE
  error_records <- tibble::tibble(Row = NA_character_)
  error_output <- NULL

  if(output %in% c("both", "errors") & skip_check == FALSE) {

    # Select locations that are missing from Locations_data
    missing_locations <- purrr::map(.x = unique(Capture_data$CapturePopID),
                                    .f = ~{

                                      dplyr::anti_join({Capture_data %>% dplyr::filter(!is.na(LocationID) & .data$CapturePopID == .x)},
                                                       {Location_data %>% dplyr::filter(.data$PopID == .x)},
                                                       by = "LocationID")

                                    }) %>%
      dplyr::bind_rows() %>%
      dplyr::select("Row", "PopID" = "CapturePopID", "CaptureID", "LocationID")

    # If potential errors, add to report
    if(nrow(missing_locations) > 0) {

      err <- TRUE

      # Compare to approved_list
      error_records <- missing_locations %>%
        dplyr::mutate(CheckID = "C6") %>%
        dplyr::anti_join(approved_list$Capture_approved_list, by=c("PopID", "CheckID", "CaptureID"))

      # Create quality check report statements
      error_output <- purrr::pmap(.l = error_records,
                                  .f = ~{

                                    paste0("Record on row ", ..1, " (PopID: ", ..2, "; CaptureID: ", ..3, ")",
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
