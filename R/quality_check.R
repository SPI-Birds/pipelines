#' A function to perform a quality check on pipeline outputs
#'
#' @param R_data Output of pipeline as an R object. Generated using
#' 'output_type = R' in run_pipelines.
#' @param species Species on which to test (6 letter species code)
#' @param output_format Format of report: "html", "pdf", or "both" (default).
#'
#' @return A summary dataframe of test warnings and errors for use in testthat,
#'   and a report of the line-by-line list of warnings and errors.
#'
#' @example
#' \dontrun{
#'
#' CHO <- run_pipelines(PopID = "CHO", output_type = "R")
#' quality_check(CHO, species = "PARMAJ")
#'
#' }
#' @export

quality_check <- function(R_data,
                          species,
                          output_format = "both"){

  #Name of the pop (3 letter code) is also the name of the R data object
  pop <- names(R_data)[1]

  start_time <- Sys.time()

  Individual_data <- R_data[[1]]$Individual_data
  Brood_data <- R_data[[1]]$Brood_data
  Capture_data <- R_data[[1]]$Capture_data
  Location_data <- R_data[[1]]$Location_data

  ## Create check list with - a summary of warnings and errors per test
  check_list <- tibble::tibble(Check = c("Individual data format", "Brood data format",
                                         "Capture data format", "Location data format",
                                         "Clutch and brood sizes", "Brood sizes and fledgling numbers",
                                         "Laying and hatching dates", "Hatching and fledging dates",
                                         "Improbable and impossible values brood data",
                                         "Improbable and impossible values capture data"),
                               Warning = NA,
                               Error = NA)

  ## Checks
  ## - Check formats
  ## -- Individual data
  message("Check 1: Checking format of individual data...")

  check_format_individual_output <- check_format_individual(Individual_data)

  check_list[1,2:3] <- check_format_individual_output$check_list

  ## -- Brood data
  message("Check 2: Checking format of brood data...")

  check_format_brood_output <- check_format_brood(Brood_data)

  check_list[2,2:3] <- check_format_brood_output$check_list

  ## -- Capture data
  message("Check 3: Checking format of capture data...")

  check_format_capture_output <- check_format_capture(Capture_data)

  check_list[3,2:3] <- check_format_capture_output$check_list

  ## -- Location data
  message("Check 4: Checking format of location data...")

  check_format_location_output <- check_format_location(Location_data)

  check_list[4,2:3] <- check_format_location_output$check_list


  ## - Compare clutch and brood sizes
  message("Check 5: Comparing clutch and brood sizes...")

  compare_clutch_brood_output <- compare_clutch_brood(Brood_data)

  check_list[5,2:3] <- compare_clutch_brood_output$check_list

  ## - Compare brood sizes and fledglings numbers
  message("Check 6: Comparing brood sizes and fledgling numbers...")

  compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data)

  check_list[6,2:3] <- compare_brood_fledglings_output$check_list

  ## - Compare laying and hatching dates
  message("Check 7: Comparing laying and hatching dates...")

  compare_laying_hatching_output <- compare_laying_hatching(Brood_data)

  check_list[7,2:3] <- compare_laying_hatching_output$check_list

  ## - Compare hatching and fledging dates
  message("Check 8: Comparing hatching and fledging dates...")

  compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)

  check_list[8,2:3] <- compare_hatching_fledging_output$check_list

  ## - Check brood variable values against reference values
  message("Check 9: Checking brood variable values against reference values...")

  check_values_brood_output <- check_values_brood(Brood_data, species)

  check_list[9,2:3] <- check_values_brood_output$check_list

  ## - Check capture variable values against reference values
  message("Check 10: Checking capture variable values against reference values...")

  check_values_capture_output <- check_values_capture(Capture_data, species)

  check_list[10,2:3] <- check_values_capture_output$check_list




  # cat("Check 1: Individual data format\n\n")
  # cat(unlist(check_format_individual_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 2: Brood data format\n\n")
  # cat(unlist(check_format_brood_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 3: Capture data format\n\n")
  # cat(unlist(check_format_capture_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 4: Location data format\n\n")
  # cat(unlist(check_format_location_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 5: Clutch and brood sizes\n\n")
  # cat(unlist(compare_clutch_brood_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 6: Brood sizes and fledgling numbers\n\n")
  # cat(unlist(compare_brood_fledglings_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 7: Laying and hatching dates\n\n")
  # cat(unlist(compare_laying_hatching_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 8: Hatching and fledging dates\n\n")
  # cat(unlist(compare_hatching_fledging_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 9: Improbable and impossible values in brood data\n\n")
  # cat(unlist(check_values_brood_output$warning_output), sep="\n", "\n")
  #
  # cat("Check 10: Improbable and impossible values in capture data\n\n")
  # cat(unlist(check_values_capture_output$warning_output), sep="\n", "\n")
  #
  # sink()

  time <- difftime(Sys.time(), start_time, units = "sec")

  cat(paste0("\nAll checks performed in ", round(time, 2), " seconds"))

  checks_warnings <- sum(check_list$Warning == TRUE, na.rm=TRUE)

  checks_errors <- sum(check_list$Error == TRUE, na.rm=TRUE)

  cat(crayon::yellow(paste0("\n", checks_warnings, " out of ", nrow(check_list), " checks resulted in warnings.")),
      crayon::red(paste0("\n", checks_errors, " out of ", nrow(check_list), " checks resulted in errors.\n\n")))

  ## Create output file
  title <- paste0("Quality check report for ", Species_codes[Species_codes$Code == species, "CommonName"],
                  " in ", pop_names[pop_names$code == pop, "name"])

  mark_output <- c('---',
                   'title: "`r title`"',
                   'date: "`r Sys.Date()`"',
                   'geometry: margin=0.5in',
                   'output:
                      pdf_document:
                        toc: true
                      html_document:
                        toc: true',
                   '---',
                   '',
                   '\\newpage',
                   '# Summary',
                   '',
                   'All checks performed in `r round(time, 2)` seconds.',
                   '',
                   '`r checks_warnings` out of `r nrow(check_list)` checks resulted in warnings.',
                   '',
                   '`r checks_errors` out of `r nrow(check_list)` checks resulted in errors.',
                   '',
                   '# Warnings',
                   '',
                   'Check 1: Individual data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_individual_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 2: Brood data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_brood_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 3: Capture data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_capture_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 4: Location data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_location_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 5: Clutch and brood sizes',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_clutch_brood_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 6: Brood sizes and fledgling numbers',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_brood_fledglings_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 7: Laying and hatching dates',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_laying_hatching_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 8: Hatching and fledging dates',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_hatching_fledging_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 9: Improbable and impossible values in brood data',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_values_brood_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 10: Improbable and impossible values in capture data',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_values_capture_output$warning_output), sep="\n", "\n")',
                   '```',
                   '',
                   '\\newpage',
                   '# Errors',
                   '',
                   'Check 1: Individual data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_individual_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 2: Brood data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_brood_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 3: Capture data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_capture_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 4: Location data format',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_format_location_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 5: Clutch and brood sizes',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_clutch_brood_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 6: Brood sizes and fledgling numbers',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_brood_fledglings_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 7: Laying and hatching dates',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_laying_hatching_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 8: Hatching and fledging dates',
                   '```{r echo=FALSE}',
                   'cat(unlist(compare_hatching_fledging_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 9: Improbable and impossible values in brood data',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_values_brood_output$error_output), sep="\n", "\n")',
                   '```',
                   '',
                   'Check 10: Improbable and impossible values in capture data',
                   '```{r echo=FALSE}',
                   'cat(unlist(check_values_capture_output$error_output), sep="\n", "\n")',
                   '```')

  knitr::knit(text = mark_output, output = "output-report.md")
  #txt <- markdown::renderMarkdown(text = knitr::knit(text = mark_output))
  #markdown::markdownToHTML(text = knitr::knit(text = mark_output), output = "report.html")
  if(output_format == "html") rmarkdown::render("output-report.md", output_format = "html_document")
  if(output_format == "pdf") rmarkdown::render("output-report.md", output_format = "pdf_document")
  if(output_format == "both") rmarkdown::render("output-report.md", output_format = "all")

}


#' Check format of individual data
#'
#' Check if the formats of each column in the individual data match with the standard format
#' @param Individual_data Data frame. Individual data output from pipeline.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_individual <- function(Individual_data){

  ## Data frame with column names and formats according to the standard protocol
  Individual_data_standard <- tibble::tibble(Variable = c("IndvID", "Species", "PopID",
                                                          "BroodIDLaid", "BroodIDFledged", "RingSeason",
                                                          "RingAge", "Sex"),
                                             Format_standard = c("character", "character", "character",
                                                                 "character", "character", "integer",
                                                                 "character", "character"))

  ## Data frame with column names and formats from Individual data
  Individual_data_col <- tibble::tibble(Variable = names(Individual_data),
                                        Format = purrr::pmap_chr(.l = list(Individual_data), .f = class))

  ## Mismatches between Individual data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Individual_data_mismatch <- dplyr::left_join(Individual_data_standard, Individual_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Individual_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Individual_data_mismatch$Variable,
                                .y = Individual_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Individual_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Individual_data_missing <- dplyr::left_join(Individual_data_standard, Individual_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Individual_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Individual_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Individual_data is missing, unmeasured or undetermined (NA).")
                                 })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Check format of brood data
#'
#' Check if the formats of each column in the brood data match with the standard format
#' @param Brood_data Data frame. Brood data output from pipeline.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_brood <- function(Brood_data){

  ## Data frame with column names and formats according to the standard protocol
  Brood_data_standard <- tibble::tibble(Variable = c("BroodID", "PopID", "BreedingSeason", "Species", "Plot",
                                                     "LocationID", "FemaleID", "MaleID",
                                                     "ClutchType_observed", "ClutchType_calculated",
                                                     "LayingDate", "LayingDateError", "ClutchSize",
                                                     "ClutchSizeError", "HatchDate", "HatchDateError",
                                                     "BroodSize", "BroodSizeError", "FledgeDate",
                                                     "FledgeDateError", "NumberFledged",
                                                     "NumberFledgedError", "AvgEggMass", "NumberEggs",
                                                     "AvgChickMass", "NumberChicksMass", "AvgTarsus",
                                                     "NumberChicksTarsus", "OriginalTarsusMethod", "ExperimentID"),
                                        Format_standard = c("character", "character", "integer", "character",
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
  Brood_data_missing <- dplyr::left_join(Brood_data_standard, Brood_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Brood_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Brood_data is missing, unmeasured or undetermined (NA).")
                                 })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}


#' Check format of capture data
#'
#' Check if the formats of each column in the capture data match with the standard format
#' @param Capture_data Data frame. Capture data output from pipeline.
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
  Capture_data_missing <- dplyr::left_join(Capture_data_standard, Capture_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Capture_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Capture_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Capture_data is missing, unmeasured or undetermined (NA).")
                                 })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}

#' Check format of location data
#'
#' Check if the formats of each column in the location data match with the standard format
#' @param Location_data Data frame. Location data output from pipeline.
#'
#' @return Check list, warning output, error output.
#' @export

check_format_location <- function(Location_data){

  ## Data frame with column names and formats according to the standard protocol
  Location_data_standard <- tibble::tibble(Variable = c("LocationID", "NestboxID", "LocationType",
                                                        "PopID", "Latitude", "Longitude",
                                                        "StartSeason", "EndSeason", "Habitat"),
                                           Format_standard = c("character", "character", "character",
                                                               "character", "numeric", "numeric",
                                                               "integer", "integer", "character"))

  ## Data frame with column names and formats from Location data
  Location_data_col <- tibble::tibble(Variable = names(Location_data),
                                      Format = unlist(purrr::pmap(list(Location_data), class)))

  ## Mismatches between Location data and standard protocol
  ## Column format "logical" refers to unmeasured/undetermined variables (NA)
  Location_data_mismatch <- dplyr::left_join(Location_data_standard, Location_data_col, by="Variable") %>%
    filter(Format != "logical" & Format_standard != Format)

  err <- FALSE
  error_output <- NULL

  if(nrow(Location_data_mismatch) > 0) {
    err <- TRUE

    error_output <- purrr::map2(.x = Location_data_mismatch$Variable,
                                .y = Location_data_mismatch$Format_standard,
                                .f = ~{
                                  paste0("The format of ", .x, " in Location_data is not ", .y, ".")
                                })
  }

  ## Missing columns
  Location_data_missing <- dplyr::left_join(Location_data_standard, Location_data_col, by="Variable") %>%
    filter(Format == "logical")

  war <- FALSE
  warning_output <- NULL

  if(nrow(Location_data_missing) > 0) {
    war <- TRUE

    warning_output <- purrr::map(.x = Location_data_missing$Variable,
                                 .f = ~{
                                   paste0(.x, " in Location_data is missing, unmeasured or undetermined (NA).")
                                 })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  Format <- Format_standard <- NULL

}

#' Compare clutch and brood sizes
#'
#' Compare clutch size and brood size per brood. In non-manipulated broods, clutch size should be larger or equal to brood size. If not, the record will result in an error. In broods with clutch manipulation, clutch size might be smaller than brood size. If so, the record will result in a warning.
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#'
#' @return Check list, warning output, error output.
#' @export

compare_clutch_brood <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & ClutchSize < BroodSize)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(ClutchSize < BroodSize)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_non$BroodID,
                                          Brood_data_non$ClutchSize,
                                          Brood_data_non$BroodSize),
                                .f = ~{
                                  paste0("Record with BroodID ", ..1,
                                         " has a larger brood size (", ..3,
                                         ") than clutch size (", ..2,
                                         "), but was not experimentally manipulated.")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_data_man$BroodID,
                                            Brood_data_man$ClutchSize,
                                            Brood_data_man$BroodSize),
                                  .f = ~{
                                    paste0("Record with BroodID ", ..1,
                                           " has a larger brood size (", ..3,
                                           ") than clutch size (", ..2,
                                           "), and was experimentally manipulated.")
                                 })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  ExperimentID <- ClutchSize <- BroodSize <- NULL

}


#' Compare brood sizes and fledglings numbers
#'
#' Compare brood size and fledgling number per brood. In non-manipulated broods, brood size should be larger or equal to fledgling number. If not, the record will result in an error. In broods with clutch manipulation, brood size might be smaller than fledgling number. If so, the record will result in a warning.
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#'
#' @return Check list, warning output, error output.
#' @export

compare_brood_fledglings <- function(Brood_data){

  # Non-manipulated broods
  Brood_data_non <- Brood_data %>%
    filter(is.na(ExperimentID) & BroodSize < NumberFledged)

  # Manipulated broods
  Brood_data_man <- Brood_data %>%
    filter(BroodSize < NumberFledged)

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_data_non) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_data_non$BroodID,
                                          Brood_data_non$BroodSize,
                                          Brood_data_non$NumberFledged),
                                .f = ~{
                                  paste0("Record with BroodID ", ..1,
                                         " has a larger fledgling number (", ..3,
                                         ") than brood size (", ..2,
                                         "), but was not experimentally manipulated.")
                                })
  }

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_data_man) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_data_man$BroodID,
                                            Brood_data_man$BroodSize,
                                            Brood_data_man$NumberFledged),
                                  .f = ~{
                                    paste0("Record with BroodID ", ..1,
                                           " has a larger fledgling number (", ..3,
                                           ") than brood size (", ..2,
                                           "), and was experimentally manipulated.")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  ExperimentID <- BroodSize <- NumberFledged <- NULL

}



#' Compare laying and hatching dates
#'
#' Compare laying and hatching date per brood. Broods with laying date later than hatching date will result in an error. Broods with laying date earlier than hatching date but the difference in number of days is smaller than incubation time will result in a warning.
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
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

    error_output <- purrr::pmap(.l = list(Brood_data_late$BroodID,
                                          Brood_data_late$LayingDate,
                                          Brood_data_late$HatchDate),
                                .f = ~{
                                  paste0("Record with BroodID ", ..1,
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
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  LayingDate <- HatchDate <- NULL

}


#' Compare hatching and fledging dates
#'
#' Compare hatching and fledging date per brood. Broods with hatching date later than fledging date will result in an error. Broods with hatching date earlier than fledging date but the difference in number of days is smaller than breeding time will result in a warning.
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
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

    error_output <- purrr::pmap(.l = list(Brood_data_late$BroodID,
                                          Brood_data_late$HatchDate,
                                          Brood_data_late$FledgeDate),
                                .f = ~{
                                  paste0("Brood_data record with BroodID ", ..1,
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
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  HatchDate <- FledgeDate <- NULL

}


#' Check brood variable values against reference values
#'
#' Check variable values against species-specific reference values in brood data. Implausible values will result in a warning. Impossible values will result in an error. Variables that are checked: LayingDate, ClutchSize, HatchDate, BroodSize, FledgeDate, NumberFledged, AvgEggMass, AvgChickMass, AvgTarsus.
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#' @param species Six-letter species ID to select species-specific reference values.
#'
#' @return Check list, warning output, error output.
#' @export

check_values_brood <- function(Brood_data, species) {

  # Select species-specific reference values
  ref <- brood_ref_values_list[[species]]

  # Select species in data
  Brood_data <- Brood_data %>%
    dplyr::filter(Species == species)

  # Create list of variable-specific dataframes
  Brood_list <- purrr::map2(.x = Brood_data[, c("ClutchSize", "BroodSize", "NumberFledged")],
                               .y = ref,
                               .f = ~{
                                 tibble::tibble(BroodID = Brood_data$BroodID,
                                                Variable = names(.y)[3],
                                                .x) %>%
                                   dplyr::filter(!is.na(.x)) %>% # Only non-NA's
                                   dplyr::mutate(Warning = ifelse(.x > as.numeric(.y[2,3]),
                                                                  TRUE, FALSE),
                                                 Error = ifelse(.x < as.numeric(.y[3,3]) | .x > as.numeric(.y[4,3]),
                                                                TRUE, FALSE))
                               })

  # Select records with errors (impossible values) from list
  Brood_err <- purrr::map(.x = Brood_list,
                          .f = ~{
                            .x %>%
                              dplyr::filter(Error == TRUE)
                          }) %>%
    dplyr::bind_rows()

  err <- FALSE
  error_output <- NULL

  if(nrow(Brood_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Brood_err$BroodID,
                                          Brood_err$Variable,
                                          Brood_err$.x),
                                .f = ~{
                                  paste0("Brood_data record with BroodID ", ..1,
                                         " has an impossible value in ", ..2,
                                         " (", ..3, ").")
                                })
  }

  # Select records with warnings (improbable values) from list
  Brood_war <- purrr::map(.x = Brood_list,
                          .f = ~{
                            .x %>%
                              dplyr::filter(Warning == TRUE)
                          }) %>%
    dplyr::bind_rows()

  war <- FALSE
  warning_output <- NULL

  if(nrow(Brood_war) > 0) {
    war <- TRUE

    warning_output <- purrr::pmap(.l = list(Brood_war$BroodID,
                                            Brood_war$Variable,
                                            Brood_war$.x),
                                  .f = ~{
                                    paste0("Brood_data record with BroodID ", ..1,
                                           " has an improbable value in ", ..2,
                                           " (", ..3, ").")
                                  })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  brood_ref_values_list <- Species <- NULL

}


#' Check capture variable values against reference values
#'
#' Check variable values against species-specific reference values in capture data. Implausible values will result in a warning. Impossible values will result in an error. Variables that are checked: Mass, Tarsus, WingLength, Age_obsv, Age_calc, Chick_age.
#'
#' @param Capture_data Data frame. Capture data output from pipeline.
#' @param species Six-letter species ID to select species-specific reference values.
#'
#' @return Check list, warning output, error output.
#' @export

check_values_capture <- function(Capture_data, species) {

  # Select species-specific reference values
  ref_adult <- capture_ref_values_list[[species]]
  # ref_chick <- cap_chick_ref_values_list[[species]]

  # Add unique row identifier to capture data
  Capture_data <- Capture_data %>%
    tibble::rownames_to_column(var = "RowID")

  # Select species in capture data
  Capture_data <- Capture_data %>%
    dplyr::filter(Species == species)

  # Separate adults from chicks, as they have different reference values
  Adult_data <- Capture_data %>%
    dplyr::filter(Age_calculated > 3)

  Chick_data <- Capture_data %>%
    dplyr::filter(Age_calculated <= 3)

  # Create warning & error list of variable-specific dataframes for
  # - adults
  Capture_list <- purrr::map2(.x = Adult_data[, c("Mass", "Tarsus")],
                            .y = ref_adult,
                            .f = ~{
                              tibble::tibble(IndvID = Adult_data$IndvID,
                                             RowID = Adult_data$RowID,
                                             Age = "Adult",
                                             Variable = names(.y)[3],
                                             .x) %>%
                                dplyr::filter(!is.na(.x)) %>% # Only non-NA's
                                dplyr::mutate(Warning = ifelse(.x > as.numeric(.y[2,3]),
                                                               TRUE, FALSE),
                                              Error = ifelse(.x < as.numeric(.y[3,3]) | .x > as.numeric(.y[4,3]),
                                                             TRUE, FALSE))
                            })
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

  # Combine adults and chicks
  # Capture_list <- purrr::map2(.x = Adult_list,
  #                             .y = Chick_list,
  #                             .f = ~{
  #                               dplyr::bind_rows(.x, .y)
  #                             })


  # Select records with errors (impossible values) from list
  Capture_err <- purrr::map(.x = Capture_list,
                            .f = ~{
                              .x %>%
                                dplyr::filter(Error == TRUE)
                            }) %>%
    dplyr::bind_rows()

  err <- FALSE
  error_output <- NULL

  if(nrow(Capture_err) > 0) {
    err <- TRUE

    error_output <- purrr::pmap(.l = list(Capture_err$RowID,
                                          Capture_err$Variable,
                                          Capture_err$.x),
                                .f = ~{
                                  paste0("Capture_data record with RowID ", ..1,
                                         " has an impossible value in ", ..2,
                                         " (", ..3, ").")
                                })
  }

  # Select records with warnings (improbable values) from list
  Capture_war <- purrr::map(.x = Capture_list,
                            .f = ~{
                              .x %>%
                                dplyr::filter(Warning == TRUE)
                            }) %>%
    dplyr::bind_rows()

  war <- FALSE
  warning_output <- NULL

  if(nrow(Capture_war) > 0) {
    war <- TRUE

    error_output <- purrr::pmap(.l = list(Capture_war$RowID,
                                          Capture_war$Variable,
                                          Capture_war$.x),
                                .f = ~{
                                  paste0("Capture_data record with RowID ", ..1,
                                         " has an improbable value in ", ..2,
                                         " (", ..3, ").")
                                })
  }

  check_list <- tibble::tibble(Warning = war,
                               Error = err)

  return(list(check_list = check_list,
              warning_output = warning_output,
              error_output = error_output))

  #Satisfy RCMD Checks
  cap_adult_ref_values_list <- cap_chick_ref_values_list <- NULL
  Species <- Age_calc <- NULL

}
