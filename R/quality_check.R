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
#' @param check_format \code{TRUE} or \code{FALSE}. If \code{TRUE}, the checks on variable format (i.e. \code{\link{check_format_brood}}, \code{\link{check_format_capture}}, \code{\link{check_format_individual}} and \code{\link{check_format_location}}) are included in the quality check. Default: \code{TRUE}.
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{NumberChecks}{Number of checks performed.}
#' \item{NumberWarnings}{Number of checks resulted in warnings.}
#' \item{NumberErrors}{Number of checks resulted in errors.}
#' \item{ElapsedTime}{Elapsed time in seconds.}
#' \item{R_data}{Pipeline output (a list of 4 dataframes) with Warning & Error columns marking the rows with warnings and errors.}
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
                          output_format = "both",
                          check_format = TRUE){

  start_time <- Sys.time()

  if(output_format == "both"){

    output_format <- c("html", "pdf")

  }

  # Subset each item
  Brood_data <- R_data$Brood_data
  Capture_data <- R_data$Capture_data
  Individual_data <- R_data$Individual_data
  Location_data <- R_data$Location_data

  # Add temporary empty CaptureID column to allow procedure to approve previously flagged records
  Capture_data$CaptureID <- NA_character_ ##FIXME remove after CaptureID column has been added in pipelines

  # Run checks
  Brood_checks <- brood_check(Brood_data, Individual_data, check_format)
  Capture_checks <- capture_check(Capture_data, Location_data, Brood_data, check_format)
  Individual_checks <- individual_check(Individual_data, Capture_data, Location_data, check_format)
  Location_checks <- location_check(Location_data, check_format)

  #Add warning and error columns to each data frame
  #We don't do this for location because there are currently now rowwise checks
  Brood_data$Warning <- NA
  Brood_data$Error <- NA
  Brood_data[Brood_data$Row %in% Brood_checks$WarningRows, "Warning"] <- TRUE
  Brood_data[Brood_data$Row %in% Brood_checks$ErrorRows, "Error"] <- TRUE

  Capture_data$Warning <- NA
  Capture_data$Error <- NA
  Capture_data[Capture_data$Row %in% Capture_checks$WarningRows, "Warning"] <- TRUE
  Capture_data[Capture_data$Row %in% Capture_checks$ErrorRows, "Error"] <- TRUE

  Individual_data$Warning <- NA
  Individual_data$Error <- NA
  Individual_data[Individual_data$Row %in% Individual_checks$WarningRows, "Warning"] <- TRUE
  Individual_data[Individual_data$Row %in% Individual_checks$ErrorRows, "Error"] <- TRUE

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

    #Create body of the Rmd file
    #This is the same for both html and pdf
    body <- c('```{r wrap-hook, include=FALSE}',
    'library(knitr)
      knitr::opts_chunk$set(comment = NA, linewidth=100)
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
    '# Summary',
    '',
    'Species: `r Species_codes[Species_codes$Code %in% species,]$CommonName`',
    '',
    'Populations: `r pop_names[pop_names$code %in% pop,]$name`',
    '',
    'All checks performed in `r round(time, 2)` seconds.',
    '',
    '`r checks_warnings` out of `r nrow(check_list)` checks resulted in warnings.',
    '',
    '`r checks_errors` out of `r nrow(check_list)` checks resulted in errors.',
    '',
    '# Potential Errors',
    '',
    '## Brood data',
    '',
    '```{r, echo=FALSE, linewidth=100}',
    'purrr::pwalk(.l = list(Brood_checks$Errors,
                            Brood_checks$CheckList$CheckID,
                            Brood_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Capture data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Capture_checks$Errors,
                            Capture_checks$CheckList$CheckID,
                            Capture_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Individual data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Individual_checks$Errors,
                            Individual_checks$CheckList$CheckID,
                            Individual_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Location data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Location_checks$Errors,
                            Location_checks$CheckList$CheckID,
                            Location_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '\\newpage',
    '# Warnings',
    '',
    '## Brood data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Brood_checks$Warnings,
                            Brood_checks$CheckList$CheckID,
                            Brood_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Capture data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Capture_checks$Warnings,
                            Capture_checks$CheckList$CheckID,
                            Capture_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Individual data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Individual_checks$Warnings,
                            Individual_checks$CheckList$CheckID,
                            Individual_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '',
    '## Location data',
    '',
    '```{r, echo=FALSE}',
    'purrr::pwalk(.l = list(Location_checks$Warnings,
                            Location_checks$CheckList$CheckID,
                            Location_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("Check ", ..2, ": ", ..3), "\n")
                    cat(..1, sep="\n", "\n")
                  })',
    '```',
    '')

    #For the different output formats, create specific yaml and
    #output the file
    purrr::pwalk(.l = list(output_format),
                 .f = ~{

                   if("html" %in% output_format){

                      mark_output <- c('---',
                                    'title: "`r title`"',
                                    'date: "`r Sys.Date()`"',
                                    'geometry: margin=0.5in',
                                    'output:
                                      html_document:
                                        css: ./inst/css/quality_check.css
                                        keep_md: false
                                        number_sections: true
                                        theme: cosmo
                                        toc: true
                                        toc_depth: 3
                                        toc_float:
                                          collapsed: false
                                          smooth_scroll: true
                                      mainfont: Arial',
                                    '---', body)

                     knitr::knit(text = mark_output, output = "output-report.md")

                     rmarkdown::render("output-report.md", output_format = "html_document")

                   }

                   if("pdf" %in% output_format){

                     mark_output <- c('---',
                                      'output:
                                        header-includes:
                                          - \\linespread{1.2}
                                          - \\newgeometry{left=29mm, right=29mm, top=20mm, bottom=15mm}
                                        pdf_document:
                                          toc: false',
                                      '---',
                                      quality_check_titlepage_pdf,
                                      '\\tableofcontents
                                      \\newpage',
                                      quality_check_description_pdf, body)

                     knitr::knit(text = mark_output, output = "output-report.md")

                     rmarkdown::render("output-report.md", output_format = "pdf_document")

                   }

                   })
  }

  return(list(CheckList = check_list,
              NumberChecks = nrow(check_list),
              NumberWarnings = checks_warnings,
              NumberErrors = checks_errors,
              ElapsedTime = round(time, 2),
              R_data = list(Brood_data = Brood_data,
                            Capture_data = Capture_data,
                            Individual_data = Individual_data,
                            Location_data = Location_data)))
}
