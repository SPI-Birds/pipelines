#' Perform a quality check on pipeline outputs
#'
#' A wrapper function that performs a quality check and produces a report.
#'
#' Combines the four data frame-specific wrappers: \code{\link{brood_check}}, \code{\link{capture_check}}, \code{\link{individual_check}} and \code{\link{location_check}}.
#'
#' @param R_data Output of pipeline as an R object. Generated using
#' \code{output_type = R} in \code{\link{run_pipelines}}.
#' @param output Character. Run checks on and produce report of: potential errors ("errors"), warnings and verified records ("warnings"), or both in separate files ("both"; default).
#' @param output_format Character. Format of output report: "html", "pdf", or "both" (default).
#' @param output_file Character. Name of the output reports. Default: "quality-check-report". Note that to the file name of the report on potential errors the suffix "_errors" is added (i.e. "quality-check-report_errors"), and to the report on warnings and verified records "_warnings" (i.e., "quality-check-report_warnings").
#' @param latex_engine Character. LaTeX engine for producing PDF output. Options are "pdflatex", "xelatex", and "lualatex" (default). NB: pdfLaTeX and XeLaTeX have memory limit restrictions, which can be problematic when generating large pdfs. LuaLaTeX has dynamic memory management which may help for generating large pdfs.
#' @param test Logical. Is `quality_check` being used inside package tests? If TRUE, `R_data` is ignored and
#' dummy data will be used instead.
#' @param map Logical. If TRUE, a map of capture locations is added in the report. See \code{\link{check_coordinates}}.
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
#' CHO_checked <- quality_check(R_data = CHO)
#'
#' }
#' @export

quality_check <- function(R_data,
                          output = "both",
                          output_format = "both",
                          output_file = "quality-check-report",
                          latex_engine = "lualatex",
                          test = FALSE,
                          map = TRUE){

  start_time <- Sys.time()

  message("Running quality check")

  if(output_format == "both"){

    output_format <- c("html", "pdf")

  }

  if(test) {

    R_data <- create_dummy_data()

    approved_list <- list(Brood_approved_list = tibble::tibble(PopID = "AAA",
                                                               BroodID = "AAA-2020-0",
                                                               CheckID = "B3"),
                          Capture_approved_list = tibble::tibble(PopID = NA_character_,
                                                                 CaptureID = NA_character_,
                                                                 CheckID = NA_character_),
                          Individual_approved_list = tibble::tibble(PopID = NA_character_,
                                                                    IndvID = NA_character_,
                                                                    CheckID = NA_character_),
                          Location_approved_list = tibble::tibble(PopID = NA_character_,
                                                                  LocationID = NA_character_,
                                                                  CheckID = NA_character_))

  } else {

    approved_list <- list(Brood_approved_list = utils::read.csv(system.file("extdata",
                                                                            "brood_approved_list.csv",
                                                                            package = "pipelines",
                                                                            mustWork = TRUE)),
                          Capture_approved_list = utils::read.csv(system.file("extdata",
                                                                              "capture_approved_list.csv",
                                                                              package = "pipelines",
                                                                              mustWork = TRUE)),
                          Individual_approved_list = utils::read.csv(system.file("extdata",
                                                                                 "individual_approved_list.csv",
                                                                                 package = "pipelines",
                                                                                 mustWork = TRUE)),
                          Location_approved_list = utils::read.csv(system.file("extdata",
                                                                               "location_approved_list.csv",
                                                                               package = "pipelines",
                                                                               mustWork = TRUE)))

  }

  # Subset each item
  Brood_data <- R_data$Brood_data
  Capture_data <- R_data$Capture_data
  Individual_data <- R_data$Individual_data
  Location_data <- R_data$Location_data

  # Add temporary empty CaptureID column to allow procedure to approve previously flagged records
  if(!("CaptureID" %in% colnames(Capture_data))) Capture_data$CaptureID <- NA_character_
  # FIXME remove after CaptureID column has been added in ALL pipelines

  # Run checks
  Brood_checks <- brood_check(Brood_data, Individual_data, approved_list, output)
  Capture_checks <- capture_check(Capture_data, Location_data, Brood_data, approved_list, output)
  Individual_checks <- individual_check(Individual_data, Capture_data, Location_data, approved_list, output)
  Location_checks <- location_check(Location_data, approved_list, output, map)

  # Add warning and error columns to each data frame
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

  Location_data$Warning <- NA
  Location_data$Error <- NA
  Location_data[Location_data$Row %in% Location_checks$WarningRows, "Warning"] <- TRUE
  Location_data[Location_data$Row %in% Location_checks$ErrorRows, "Error"] <- TRUE

  # Combine check lists
  check_list <- dplyr::bind_rows(Brood_checks$CheckList,
                                 Capture_checks$CheckList,
                                 Individual_checks$CheckList,
                                 Location_checks$CheckList)

  # Subset approved list items
  Brood_approved_list <- approved_list$Brood_approved_list %>%
    dplyr::filter(PopID %in% unique(R_data$Brood_data$PopID)) %>%
    dplyr::arrange(.data$PopID, .data$BroodID, .data$CheckID)

  Capture_approved_list <- approved_list$Capture_approved_list %>%
    dplyr::filter(PopID %in% unique(R_data$Brood_data$PopID)) %>%
    dplyr::arrange(.data$PopID, .data$CaptureID, .data$CheckID)

  Individual_approved_list <- approved_list$Individual_approved_list %>%
    dplyr::filter(PopID %in% unique(R_data$Brood_data$PopID)) %>%
    dplyr::arrange(.data$PopID, .data$IndvID, .data$CheckID)

  Location_approved_list <- approved_list$Location_approved_list %>%
    dplyr::filter(PopID %in% unique(R_data$Brood_data$PopID)) %>%
    dplyr::arrange(.data$PopID, .data$LocationID, .data$CheckID)


  # Check messages
  time <- difftime(Sys.time(), start_time, units = "sec")

  cat(paste0("\nAll checks performed in ", round(time, 2), " seconds"))

  checks_warnings <- sum(check_list$Warning == TRUE, na.rm=TRUE)

  checks_errors <- sum(check_list$Error == TRUE, na.rm=TRUE)

  cat(crayon::yellow(paste0("\n", checks_warnings, " out of ", nrow(check_list), " checks resulted in warnings.")),
      crayon::red(paste0("\n", checks_errors, " out of ", nrow(check_list), " checks resulted in errors.\n\n")))


  # Create output file
  # Unique PopIDs and Species for report title
  pop <- unique(Brood_data[!is.na(Brood_data$PopID), ]$PopID)
  if(length(pop) == 0) pop <- NA
  species <- unique(Brood_data[!is.na(Brood_data$Species), ]$Species)

  # Produce report with potential errors
  if(output %in% c("errors", "both")) {

    # Title
    title <- paste0("Quality check report: potential errors")

    # Create body of the Rmd file
    # This is the same for both html and pdf
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
    'Species: `r species_codes[species_codes$Species %in% species,]$CommonName`',
    '',
    'Populations: `r pop_codes[pop_codes$PopID %in% pop,]$PopName`',
    '',
    'All checks performed in `r round(time, 2)` seconds.',
    '',
    '`r checks_errors` out of `r nrow(check_list)` checks resulted in errors.',
    '',
    '# Potential Errors',
    '',
    '## Brood data',
    '',
    '```{r, echo=FALSE, results="asis", linewidth=100}',
    'purrr::pwalk(.l = list(Brood_checks$Errors,
                            Brood_checks$CheckList$CheckID,
                            Brood_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
    '```',
    '',
    '## Capture data',
    '',
    '```{r, echo=FALSE, results="asis"}',
    'purrr::pwalk(.l = list(Capture_checks$Errors,
                            Capture_checks$CheckList$CheckID,
                            Capture_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
    '```',
    '',
    '## Individual data',
    '',
    '```{r, echo=FALSE, results="asis"}',
    'purrr::pwalk(.l = list(Individual_checks$Errors,
                            Individual_checks$CheckList$CheckID,
                            Individual_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
    '```',
    '',
    '## Location data',
    '',
    '```{r, echo=FALSE, results="asis"}',
    'purrr::pwalk(.l = list(Location_checks$Errors,
                            Location_checks$CheckList$CheckID,
                            Location_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
    '```',
    '')


    if(!is.null(Location_checks$Maps)) { # Print the Maps section only if a map could be made (e.g., when no coordinates were provided, no map can be made)

        body <- c(body,
                  '\\newpage',
                  '**Maps**',
                  '',
                  '```{r, echo=FALSE, fig.cap = "", dpi = 300, results="asis"}',
                  'invisible(lapply(Location_checks$Maps, print))', # To suppress the printing of indices
                  '```',
                  '')

    }

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

                     knitr::knit(text = mark_output, output = "quality-check-report_errors.md")

                     rmarkdown::render("quality-check-report_errors.md",
                                       output_format = "html_document",
                                       output_file = paste0(output_file, "_errors"))

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
                                      titlepage_errors_pdf,
                                      '\\tableofcontents
                                      \\newpage',
                                      description_errors_pdf,
                                      check_descriptions_pdf,
                                      body)

                     knitr::knit(text = mark_output, output = "quality-check-report_errors.md")

                     rmarkdown::render("quality-check-report_errors.md",
                                       output_format = rmarkdown::pdf_document(latex_engine = latex_engine),
                                       output_file = paste0(output_file, "_errors"))

                   }

                   })
  }

  # Produce warnings and verified records
  if(output %in% c("warnings", "both")) {

    # Title
    title <- paste0("Quality check report: warnings and verified records")

    # Create body of the Rmd file
    # This is the same for both html and pdf
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
              'Species: `r species_codes[species_codes$Species %in% species,]$CommonName`',
              '',
              'Populations: `r pop_codes[pop_codes$PopID %in% pop,]$PopName`',
              '',
              'All checks performed in `r round(time, 2)` seconds.',
              '',
              '`r checks_warnings` out of `r nrow(check_list)` checks resulted in warnings.',
              '',
              '# Warnings',
              '',
              '## Brood data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              'purrr::pwalk(.l = list(Brood_checks$Warnings,
                            Brood_checks$CheckList$CheckID,
                            Brood_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
              '```',
              '',
              '## Capture data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              'purrr::pwalk(.l = list(Capture_checks$Warnings,
                            Capture_checks$CheckList$CheckID,
                            Capture_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
              '```',
              '',
              '## Individual data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              'purrr::pwalk(.l = list(Individual_checks$Warnings,
                            Individual_checks$CheckList$CheckID,
                            Individual_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
              '```',
              '',
              '## Location data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              'purrr::pwalk(.l = list(Location_checks$Warnings,
                            Location_checks$CheckList$CheckID,
                            Location_checks$CheckList$CheckDescription),
                  .f = ~{
                    cat(paste0("**Check ", ..2, ": ", ..3, "**", "  \n"))
                    cat(paste(" - ", ..1), sep = "  \n")
                    cat("  \n")
                  })',
              '```',
              '\\newpage',
              '',
              '# Verified records',
              '',
              '## Brood data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              '    purrr::pwalk(.l = Brood_approved_list,
                 .f = ~{ cat(paste0("Record with BroodID ", ..2,
                                    " and PopID ", ..1,
                                    " violates check ", ..3,
                                    " but been verified by the data owner and is trustworthy."),
                                    sep="  \n")
                  })',
              '```',
              '',
              '## Capture data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              '    purrr::pwalk(.l = Capture_approved_list,
                 .f = ~{ cat(paste0("Record with CaptureID ", ..2,
                                    " and PopID ", ..1,
                                    " violates check ", ..3,
                                    " but been verified by the data owner and is trustworthy."),
                                    sep="  \n")
                  })',
              '```',
              '',
              '## Individual data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              '    purrr::pwalk(.l = Individual_approved_list,
                 .f = ~{ cat(paste0("Record with IndvID ", ..2,
                                    " and PopID ", ..1,
                                    " violates check ", ..3,
                                    " but been verified by the data owner and is trustworthy."),
                                    sep="  \n")
                  })',
              '```',
              '',
              '## Location data',
              '',
              '```{r, echo=FALSE, results="asis"}',
              '    purrr::pwalk(.l = Location_approved_list,
                 .f = ~{ cat(paste0("Record with LocationID ", ..2,
                                    " and PopID ", ..1,
                                    " violates check ", ..3,
                                    " but been verified by the data owner and is trustworthy."),
                                    sep="  \n")
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

                     knitr::knit(text = mark_output, output = "quality-check-report_warnings.md")

                     rmarkdown::render("quality-check-report_warnings.md",
                                       output_format = "html_document",
                                       output_file = paste0(output_file, "_warnings"))

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
                                      titlepage_warnings_pdf,
                                      '\\tableofcontents
                                      \\newpage',
                                      description_warnings_pdf,
                                      check_descriptions_pdf,
                                      body)

                     knitr::knit(text = mark_output, output = "quality-check-report_warnings.md")

                     rmarkdown::render("quality-check-report_warnings.md",
                                       output_format = rmarkdown::pdf_document(latex_engine = latex_engine),
                                       output_file = paste0(output_file, "_warnings"))

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

  # Satisfy RCMD checks
  approved_list <- CheckID <- NULL
}
