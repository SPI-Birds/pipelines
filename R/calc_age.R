#' Calculate age based on when an individual was first captured
#'
#' Arrange data by individual and year and then determine potential age in each
#' capture.
#'
#' We only consider whether an individual was captured as a chick or not. We
#' don't consider the adult age when an individual was first captured (e.g.
#' EURING 4 or 5) This prevents any cases where an individual might be wrongly
#' aged at first capture.
#'
#' When there is no observed age at first capture we assume it couldn't be a
#' chick or this would've been recorded.
#'
#' @param data Data frame. Data frame with capture information.
#' @param ID Unquoted expression (i.e. character without quotation marks). Name
#'   of column with individual identity.
#' @param Age Unquoted expression (i.e. character without quotation marks). Name
#'   of column with observed age of individual in each capture. Must be in
#'   EURING codes.
#' @param Date Unquoted expression (i.e. character without quotation marks).
#'   Name of column with CaptureDate information. Must be in format dd/mm/yyyy.
#' @param Year Unquoted expression (i.e. character without quotation marks).
#'   Name of column with year information. N.B. This could be different to
#'   CaptureDate if we are dealing with species that breed over two year (e.g.
#'   Southern Hemisphere species).
#' @param showpb Logical. Should a progress bar be shown?
#' @return A data frame with calculated age.
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(666)
#' bird_data <- tibble::tibble(IndvID = LETTERS[sample(1:26, 100, replace = TRUE)],
#' SampleYear = sample(2012:2016, 100, replace = TRUE),
#' CaptureDate = as.Date(paste(SampleYear, 3, 31, sep = "-"), format = "%Y-%m-%d")
#' + sample(1:30, 100, replace = TRUE),
#' Age_obsv = sample(c(1L, 4L), 100, replace = TRUE)) %>%
#' calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate, Year = SampleYear)
calc_age <- function(data, ID, Age, Date, Year, showpb = TRUE){

  message("Calculating age. This can take some time for larger datasets.")

  output <- data %>%
    dplyr::arrange({{ID}}, {{Year}}, {{Date}}) %>%
    #For each individual, calculate their first age and year of capture
    dplyr::group_by({{ID}}) %>%
    dplyr::mutate(FirstAge  = as.integer(dplyr::first({{Age}})),
                  FirstYear = as.integer(dplyr::first({{Year}})),
                  yr_diff   = as.integer({{Year}}) - .data$FirstYear) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age_calculated = dplyr::case_when(is.na(.data$FirstAge) | .data$FirstAge > 3 ~ 4L + 2L*.data$yr_diff,
                                                    #Rule 2: If first age is specifically 2 (can fly freely but age not recorded)
                                                    #Then use first capture as age 2 (free flying but otherwise unknown)
                                                    .data$FirstAge == 2 ~ 2L + 2L*.data$yr_diff,
                                                    #Rule 3: Captures in first year use recorded age (either pullus = 0 or known first year i.e. fledgling = 3)
                                                    .data$FirstAge %in% c(1, 3) & .data$yr_diff == 0 ~ {{Age}},
                                                    #Rule 4: After birth year, first capture age is 3 (full grown born in current year)
                                                    .data$FirstAge %in% c(1, 3) & .data$yr_diff > 0 ~ 3L + 2L*.data$yr_diff,
                                                    #Rule 5: All other cases (e.g. ID unknown, year of capture unknown) have no Age_calculated
                                                    TRUE ~ NA_integer_)) %>%
    dplyr::select(-"FirstAge", -"FirstYear", -"yr_diff")

  return(output)

}
