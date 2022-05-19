#' Perform a quality check on pipeline outputs
#'
#' A wrapper function that performs a quality check and produces a report.
#'
#' Combines the four data frame-specific wrappers: \code{\link{brood_check}}, \code{\link{capture_check}}, \code{\link{individual_check}} and \code{\link{location_check}}.
#'
#' @param R_data Output of pipeline as an R object. Generated using
#' \code{output_type = R} in \code{\link{run_pipelines}}.
#' @param output Character. Run checks on and produce report of: potential errors ("errors"), warnings and verified records ("warnings"), or both in separate files ("both"; default).
#' @param report Logical. If \code{TRUE}, a report is produced. Default: \code{TRUE}.
#' @param report_format Character. Format of output report: "html", "pdf", or "both" (default).
#' @param report_file Character. Name of the output reports. Default: "quality-check-report". Note that to the file name of the report on potential errors the suffix "_errors" is added (i.e. "quality-check-report_errors"), and to the report on warnings and verified records "_warnings" (i.e., "quality-check-report_warnings").
#' @param latex_engine Character. LaTeX engine for producing PDF output. Options are "pdflatex", "xelatex", and "lualatex" (default). NB: pdfLaTeX and XeLaTeX have memory limit restrictions, which can be problematic when generating large pdfs. LuaLaTeX has dynamic memory management which may help for generating large pdfs.
#' @param test Logical. Is \code{quality_check} being used inside package tests? If  \code{TRUE}, \code{R_data} is ignored and
#' dummy data will be used instead.
#' @param map Logical. If  \code{TRUE}, a map of locations is added in the report. See \code{\link{check_coordinates}}.
#' @param skip Character. Identifiers of the individual quality checks (CheckID) that should be skipped.
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and potential errors.}
#' \item{NumberChecks}{Number of checks performed.}
#' \item{SkippedChecks}{Number of checks manually skipped.}
#' \item{WarningChecks}{Number of checks resulted in warnings.}
#' \item{ErrorChecks}{Number of checks resulted in potential errors.}
#' \item{ElapsedTime}{Elapsed time in seconds.}
#' \item{R_data}{Pipeline output (a list of 4 dataframes) with Warning & Error columns marking the rows with warnings and errors.}
#'
#' and reports (pdf, html or both) of potential errors and/or warnings if \code{report = TRUE}.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' CHO <- run_pipelines(PopID = "CHO", output_type = "R")
#' CHO_checked <- quality_check(R_data = CHO)
#'
#' }


quality_check <- function(R_data,
                          output = "both",
                          report = TRUE,
                          report_format = "both",
                          report_file = "quality-check-report",
                          latex_engine = "lualatex",
                          test = FALSE,
                          map = TRUE,
                          skip = NULL){

  start_time <- Sys.time()

  message("Running quality check")

  if(report_format == "both"){

    report_format <- c("html", "pdf")

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
  # FIXME remove after CaptureID column has been added to ALL pipelines

  # Run checks
  Brood_checks <- brood_check(Brood_data, Individual_data, Capture_data, Location_data, approved_list, output, skip)
  Capture_checks <- capture_check(Capture_data, Location_data, Brood_data, Individual_data, approved_list, output, skip)
  Individual_checks <- individual_check(Individual_data, Capture_data, Location_data, approved_list, output, skip)
  Location_checks <- location_check(Location_data, Brood_data, Capture_data, approved_list, output, skip, map)

  # Add warning and error columns to each data frame
  # FIXME remove after Warning & Error columns have been added to ALL pipelines
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

  cat(paste0("\nData quality check procedure took ", round(time, 2), " seconds"))

  checks_warnings <- sum(check_list$Warning == TRUE, na.rm = TRUE) # Number of checks resulting in warnings

  checks_errors <- sum(check_list$Error == TRUE, na.rm = TRUE) # Number of checks resulting in errors

  checks_skipped <- sum(check_list$Skipped == TRUE, na.rm = TRUE) # Total number of checks skipped

  skipped_errors <- check_list %>% dplyr::filter(!is.na(.data$Error), .data$Skipped == TRUE) %>% nrow() # Number of potential error checks skipped

  skipped_warnings <- check_list %>% dplyr::filter(!is.na(.data$Warning), .data$Skipped == TRUE) %>% nrow() # Number of warning checks skipped

  if(output %in% c("warnings", "both")) {

    cat(crayon::yellow(paste0("\nOut of ", sum(!is.na(check_list$Warning)), " checks that check for warnings, ", skipped_warnings,
                              " were manually skipped, and ", checks_warnings, " flagged one or more warnings.")))

  }

  if(output %in% c("errors", "both")) {

    cat(crayon::red(paste0("\nOut of ", sum(!is.na(check_list$Error)), " checks that check for potential errors, ", skipped_errors,
                           " were manually skipped, and ", checks_errors, " flagged one or more potential errors.")))

  }

  # Number of checks performed
  number_checks <- dplyr::case_when(output == "both" ~ check_list %>% dplyr::filter(.data$Skipped == FALSE) %>% nrow(),
                                    output == "errors" ~ check_list %>% dplyr::filter(.data$Skipped == FALSE & !is.na(.data$Error)) %>% nrow(),
                                    output == "warnings" ~ check_list %>% dplyr::filter(.data$Skipped == FALSE & !is.na(.data$Warning)) %>% nrow())

  # Create output file
  # Unique PopIDs and Species for report title
  pop <- unique(Brood_data[!is.na(Brood_data$PopID), ]$PopID)
  if(length(pop) == 0) pop <- NA
  species <- unique(Brood_data[!is.na(Brood_data$Species), ]$Species)

  # Produce report with potential errors
  if(output %in% c("errors", "both") & report == TRUE) {

    # Title
    title <- paste0("SPI-Birds Standard Data Quality Checks")

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
    '- Species: `r species_codes[species_codes$Species %in% species,]$CommonName`',
    '',
    '- Populations: `r pop_codes[pop_codes$PopID %in% pop,]$PopName`',
    '',
    '- Data quality check for potential errors performed in `r round(time, 2)` seconds.',
    '',
    '- Out of `r sum(!is.na(check_list$Error))` individual checks that check for potential errors, `r skipped_errors` were manually skipped, and `r checks_errors` flagged one or more potential errors.',
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

      pdf_map <- c('\\newpage',
                   '# Maps',
                   '',
                   '```{r, echo = FALSE, fig.cap = "", results = "asis"}',
                   'invisible(
                      iwalk(.x = Location_checks$Maps,
                            .f = ~{
                              htmlwidgets::saveWidget(widget = .x, file = paste0("figure/", .y, ".html"))
                            })
                    )

                    invisible(
                      map(.x = names(Location_checks$Maps),
                          .f = ~{
                            webshot::webshot(url = paste0("figure/", .x, ".html"),
                                             file = paste0("figure/", .x, ".png"))
                          })
                    )

                    knitr::include_graphics(list.files(path = paste0("figure/"), pattern = "^[A-Z]{3}.png$", full.names = TRUE))',
                   '```',
                   '')

      html_map <- c('\\newpage',
                    '# Maps',
                    '',
                    '```{r, echo = FALSE}',
                    'htmltools::tagList(Location_checks$Maps)',
                    '```',
                    '')

    }

    # For the different report formats, create specific yaml and
    # output the file
    purrr::pwalk(.l = list(report_format),
                 .f = ~{

                   if("html" %in% report_format){

                      title <- paste0(title, ": Potential Errors")
                      authors <- "SPI-Birds Team (Stefan J.G. Vriend, Liam D. Bailey, Chris Tyson, Antica Culina, & Marcel E. Visser)"
                      date_format <- "%B %d, %Y"

                      mark_output <- c('---',
                                    'title: "`r title`"',
                                    'author: "`r authors`"',
                                    'date: "`r format(Sys.time(), date_format)`"',
                                    'geometry: margin=0.5in',
                                    'always_allow_html: yes',
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
                                    '---',
                                    descriptions_errors_html,
                                    check_descriptions_html,
                                    body)

                     if(!is.null(Location_checks$Maps)) mark_output <- c(mark_output, html_map)

                     writeLines(mark_output, "quality-check-report_errors.rmd")

                     rmarkdown::render("quality-check-report_errors.rmd",
                                       output_format = "html_document",
                                       output_file = paste0(report_file, "_errors"))

                   }

                   if("pdf" %in% report_format){

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

                     if(!is.null(Location_checks$Maps)) mark_output <- c(mark_output, pdf_map)

                     writeLines(mark_output, "quality-check-report_errors.rmd")

                     rmarkdown::render("quality-check-report_errors.rmd",
                                       output_format = rmarkdown::pdf_document(latex_engine = latex_engine),
                                       output_file = paste0(report_file, "_errors"))

                   }

                   })
  }

  # Produce warnings and verified records
  if(output %in% c("warnings", "both") & report == TRUE) {

    # Title
    title <- paste0("SPI-Birds Standard Data Quality Checks")

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
              '- Species: `r species_codes[species_codes$Species %in% species,]$CommonName`',
              '',
              '- Populations: `r pop_codes[pop_codes$PopID %in% pop,]$PopName`',
              '',
              '- Data quality check for warnings performed in `r round(time, 2)` seconds.',
              '',
              '- Out of `r sum(!is.na(check_list$Warning))` individual checks that check for warnings, `r skipped_warnings` were manually skipped, and `r checks_warnings` flagged one or more warnings.',
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
    purrr::pwalk(.l = list(report_format),
                 .f = ~{

                   if("html" %in% report_format){

                     title <- paste0(title, ": Warnings & Verified Records")
                     authors <- "SPI-Birds Team (Stefan J.G. Vriend, Liam D. Bailey, Chris Tyson, Antica Culina, & Marcel E. Visser)"
                     date_format <- "%B %d, %Y"

                     mark_output <- c('---',
                                      'title: "`r title`"',
                                      'author: "`r authors`"',
                                      'date: "`r format(Sys.time(), date_format)`"',
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
                                      '---',
                                      descriptions_warnings_html,
                                      check_descriptions_html,
                                      body)

                     knitr::knit(text = mark_output, output = "quality-check-report_warnings.md")

                     rmarkdown::render("quality-check-report_warnings.md",
                                       output_format = "html_document",
                                       output_file = paste0(report_file, "_warnings"))

                   }

                   if("pdf" %in% report_format){

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
                                       output_file = paste0(report_file, "_warnings"))

                   }

                 })
  }

  return(list(CheckList = check_list,
              NumberChecks = number_checks,
              SkippedChecks = checks_skipped,
              WarningChecks = checks_warnings,
              ErrorChecks = checks_errors,
              ElapsedTime = round(time, 2),
              R_data = list(Brood_data = Brood_data,
                            Capture_data = Capture_data,
                            Individual_data = Individual_data,
                            Location_data = Location_data)))

  # Satisfy RCMD checks
  approved_list <- CheckID <- NULL

}
