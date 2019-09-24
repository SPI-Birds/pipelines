#' A wrapper function to perform a quality check on pipeline outputs
#'
#' A wrapper function that performs a quality check and produces a report.
#'
#' Combines the four data frame-specific wrappers: \code{\link{brood_check}}, \code{\link{capture_check}}, \code{\link{individual_check}} and \code{\link{location_check}}
#'
#' @param R_data Output of pipeline as an R object. Generated using
#' 'output_type = R' in \code{\link{run_pipelines}}.
#' @param output_format Format of report: "html", "pdf", or "both" (default).
#'
#' @return A summary dataframe of test warnings and errors for use in testthat,
#'   and a report of the line-by-line list of warnings and errors.
#'
#' @examples
#' \dontrun{
#'
#' CHO <- run_pipelines(PopID = "CHO", output_type = "R")
#' quality_check(CHO, species = "PARMAJ")
#'
#' }
#' @export

quality_check <- function(R_data,
                          output_format = "both"){

  start_time <- Sys.time()

  # Subset each item
  Brood_data <- R_data$Brood_data
  Capture_data <- R_data$Capture_data
  Individual_data <- R_data$Individual_data
  Location_data <- R_data$Location_data

  # Unique PopIDs and Species
  pop <- unique(R_data$Brood_data$PopID)
  species <- unique(R_data$Brood_data$Species)

  # Run checks
  Brood_checks <- brood_check(Brood_data)
  Capture_checks <- capture_check(Capture_data)
  Individual_checks <- individual_check(Individual_data)
  Location_checks <- location_check(Location_data)

  # Combine check lists
  check_list <- dplyr::bind_rows(Brood_checks$CheckList,
                                 Capture_checks$CheckList,
                                 Individual_checks$CheckList,
                                 Location_checks$CheckList)


  # Create check list with - a summary of warnings and errors per test
  # check_list <- tibble::tibble(Check = c("Individual data format", "Brood data format",
  #                                        "Capture data format", "Location data format",
  #                                        "Clutch and brood sizes", "Brood sizes and fledgling numbers",
  #                                        "Laying and hatching dates", "Hatching and fledging dates",
  #                                        "Improbable and impossible values brood data",
  #                                        "Improbable and impossible values capture data"),
  #                              Warning = NA,
  #                              Error = NA)
  #
  # # Checks
  # # - Check format individual data
  # message("Check 1: Checking format of individual data...")
  #
  # check_format_individual_output <- check_format_individual(Individual_data)
  #
  # check_list[1,2:3] <- check_format_individual_output$check_list
  #
  # # - Check format brood data
  # message("Check 2: Checking format of brood data...")
  #
  # check_format_brood_output <- check_format_brood(Brood_data)
  #
  # check_list[2,2:3] <- check_format_brood_output$check_list
  #
  # # - Check format capture data
  # message("Check 3: Checking format of capture data...")
  #
  # check_format_capture_output <- check_format_capture(Capture_data)
  #
  # check_list[3,2:3] <- check_format_capture_output$check_list
  #
  # # - Check format location data
  # message("Check 4: Checking format of location data...")
  #
  # check_format_location_output <- check_format_location(Location_data)
  #
  # check_list[4,2:3] <- check_format_location_output$check_list
  #
  # # - Compare clutch and brood sizes
  # message("Check 5: Comparing clutch and brood sizes...")
  #
  # compare_clutch_brood_output <- compare_clutch_brood(Brood_data)
  #
  # check_list[5,2:3] <- compare_clutch_brood_output$check_list
  #
  # # - Compare brood sizes and fledglings numbers
  # message("Check 6: Comparing brood sizes and fledgling numbers...")
  #
  # compare_brood_fledglings_output <- compare_brood_fledglings(Brood_data)
  #
  # check_list[6,2:3] <- compare_brood_fledglings_output$check_list
  #
  # # - Compare laying and hatching dates
  # message("Check 7: Comparing laying and hatching dates...")
  #
  # compare_laying_hatching_output <- compare_laying_hatching(Brood_data)
  #
  # check_list[7,2:3] <- compare_laying_hatching_output$check_list
  #
  # # - Compare hatching and fledging dates
  # message("Check 8: Comparing hatching and fledging dates...")
  #
  # compare_hatching_fledging_output <- compare_hatching_fledging(Brood_data)
  #
  # check_list[8,2:3] <- compare_hatching_fledging_output$check_list
  #
  # # - Check brood variable values against reference values
  # message("Check 9: Checking brood variable values against reference values...")
  #
  # check_values_brood_output <- check_values_brood(Brood_data, species)
  #
  # check_list[9,2:3] <- check_values_brood_output$check_list
  #
  # # - Check capture variable values against reference values
  # message("Check 10: Checking capture variable values against reference values...")
  #
  # check_values_capture_output <- check_values_capture(Capture_data, species)
  #
  # check_list[10,2:3] <- check_values_capture_output$check_list


  # Check messages
  time <- difftime(Sys.time(), start_time, units = "sec")

  cat(paste0("\nAll checks performed in ", round(time, 2), " seconds"))

  checks_warnings <- sum(check_list$Warning == TRUE, na.rm=TRUE)

  checks_errors <- sum(check_list$Error == TRUE, na.rm=TRUE)

  cat(crayon::yellow(paste0("\n", checks_warnings, " out of ", nrow(check_list), " checks resulted in warnings.")),
      crayon::red(paste0("\n", checks_errors, " out of ", nrow(check_list), " checks resulted in errors.\n\n")))

  # Create output file
  title <- paste0("Quality check report for ", Species_codes[Species_codes$Code == species, "CommonName"],
                  " in ", pop_names[pop_names$code == pop, "name"])

  c('---',
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
    '## Brood data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Brood_checks$Warnings,
                            1:length(Brood_checks$Warnings),
                            Brood_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Capture data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Capture_checks$Warnings,
                            1:length(Capture_checks$Warnings),
                            Capture_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Individual data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Individual_checks$Warnings,
                            1:length(Individual_checks$Warnings),
                            Individual_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Location data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Location_checks$Warnings,
                            1:length(Location_checks$Warnings),
                            Location_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '\\newpage',
    '# Errors',
    '',
    '## Brood data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Brood_checks$Errors,
                            1:length(Brood_checks$Errors),
                            Brood_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Capture data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Capture_checks$Errors,
                            1:length(Capture_checks$Errors),
                            Capture_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Individual data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Individual_checks$Errors,
                            1:length(Individual_checks$Errors),
                            Individual_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Location data',
    '',
    '```{r echo=FALSE}',
    'purrr::pwalk(.l = list(Location_checks$Errors,
                            1:length(Location_checks$Errors),
                            Location_checks$CheckNames),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```') -> mark_output

  knitr::knit(text = mark_output, output = "output-report.md")

  if(output_format == "html") rmarkdown::render("output-report.md", output_format = "html_document")
  if(output_format == "pdf") rmarkdown::render("output-report.md", output_format = "pdf_document")
  if(output_format == "both") rmarkdown::render("output-report.md", output_format = "all")

}
