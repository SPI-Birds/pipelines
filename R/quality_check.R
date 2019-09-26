#' Perform a quality check on pipeline outputs
#'
#' A wrapper function that performs a quality check and produces a report.
#'
#' Combines the four data frame-specific wrappers: \code{\link{brood_check}}, \code{\link{capture_check}}, \code{\link{individual_check}} and \code{\link{location_check}}.
#'
#' @param R_data Output of pipeline as an R object. Generated using
#' \code{output_type = R} in \code{\link{run_pipelines}}.
#' @param output \code{TRUE} or \code{FALSE}. If \code{TRUE}, a report is produced. Default: \code{TRUE}.
#' @param output_format A character. Format of output report. Options: \code{"html"}, \code{"pdf"}, or \code{"both"}. Default: \code{"both"}.
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{NumberChecks}{Number of checks performed.}
#' \item{NumberWarnings}{Number of checks resulted in warnings.}
#' \item{NumberErrors}{Number of checks resulted in errors.}
#' \item{ElapsedTime}{Elapsed time in seconds.}
#'
#' and a report (pdf, html or both) of row-by-row list of warnings and errors if \code{output_format = TRUE}.
#'
#' @examples
#' \dontrun{
#'
#' CHO <- run_pipelines(PopID = "CHO", output_type = "R")
#' quality_check(CHO)
#'
#' }
#' @export

quality_check <- function(R_data,
                          output = TRUE,
                          output_format = "both"){

  start_time <- Sys.time()

  # Subset each item
  Brood_data <- R_data$Brood_data
  Capture_data <- R_data$Capture_data
  Individual_data <- R_data$Individual_data
  Location_data <- R_data$Location_data

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

  # Check messages
  time <- difftime(Sys.time(), start_time, units = "sec")

  cat(paste0("\nAll checks performed in ", round(time, 2), " seconds"))

  checks_warnings <- sum(check_list$Warning == TRUE, na.rm=TRUE)

  checks_errors <- sum(check_list$Error == TRUE, na.rm=TRUE)

  cat(crayon::yellow(paste0("\n", checks_warnings, " out of ", nrow(check_list), " checks resulted in warnings.")),
      crayon::red(paste0("\n", checks_errors, " out of ", nrow(check_list), " checks resulted in errors.\n\n")))


  # Create output file
  # Unique PopIDs and Species for report title
  pop <- dplyr::pull(unique(Brood_data[!is.na(Brood_data$PopID), "PopID"]))
  if(length(pop) == 0) pop <- NA
  species <- dplyr::pull(unique(Brood_data[!is.na(Brood_data$Species), "Species"]))

  # Title
  title <- paste0("Quality check report")
  # title <- paste0("Quality check report for ", Species_codes[Species_codes$Code == species, "CommonName"],
  #                 " in ", pop_names[pop_names$code == pop, "name"])


  # Produce report
  if(output == TRUE) {
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
      '```{r wrap-hook, include=FALSE}',
      'library(knitr)
      hook_output = knit_hooks$get("output")
      knit_hooks$set(output = function(x, options) {
      if (!is.null(n <- options$linewidth)) {
        x = knitr:::split_lines(x)
        # any lines wider than n should be wrapped
        if (any(nchar(x) > n)) x = strwrap(x, width = n)
        x = paste(x, collapse = "\n")
      }
      hook_output(x, options)
      })',
      '```',
      '',
      '```{r, include=FALSE}',
      'knitr::opts_chunk$set(linewidth=100)',
      '```',
      '',
      '\\newpage',
      '# Summary',
      '',
      'Species: `r dplyr::pull(Species_codes[Species_codes$Code == species, "CommonName"])`',
      '',
      'Populations: `r dplyr::pull(pop_names[pop_names$code == pop, "name"])`',
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
                            Brood_checks$CheckList$CheckDescription),
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
                            Capture_checks$CheckList$CheckDescription),
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
                            Individual_checks$CheckList$CheckDescription),
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
                            Location_checks$CheckList$CheckDescription),
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
      '```{r echo=FALSE, linewidth=100}',
      'purrr::pwalk(.l = list(Brood_checks$Errors,
                            1:length(Brood_checks$Errors),
                            Brood_checks$CheckList$CheckDescription),
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
                            Capture_checks$CheckList$CheckDescription),
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
                            Individual_checks$CheckList$CheckDescription),
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
                            Location_checks$CheckList$CheckDescription),
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

  return(list(CheckList = check_list,
              NumberChecks = nrow(check_list),
              NumberWarnings = checks_warnings,
              NumberErrors = checks_errors,
              ElapsedTime = round(time, 2)))
}
