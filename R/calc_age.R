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
#'
#' @return A data frame with calculated age.
#' @export
#'
#' @examples
#' set.seed(666)
#' bird_data <- tibble::tibble(IndvID = LETTERS[sample(1:26, 100, replace = TRUE)],
#' SampleYear = sample(2012:2016, 100, replace = TRUE),
#' CaptureDate = as.Date(paste(SampleYear, 3, 31, sep = "-"), format = "%Y-%m-%d") + sample(1:30, 100, replace = TRUE),
#' Age_obsv = sample(c(1, 4), 100, replace = TRUE)) %>%
#' calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate, Year = SampleYear)
calc_age <- function(data, ID, Age, Date, Year){

  data %>%
    dplyr::arrange({{ID}}, {{Year}}, {{Date}}) %>%
    dplyr::group_by({{ID}}) %>%
    dplyr::mutate(FirstAge  = first({{Age}}),
                  FirstYear = first({{Year}})) %>%
    dplyr::mutate(was_chick = FirstAge == 1,
                  yr_diff   = {{Year}} - FirstYear,
                  Age_calc = purrr::pmap_dbl(.l = list(was_chick, yr_diff),
                                             .f = ~{

                                               if(is.na(..1) | ..1 == FALSE) {

                                                 return(4 + 2*..2)

                                               } else {

                                                 if(..2 == 0){

                                                   return(1)

                                                 } else {

                                                   return(3 + 2*..2)

                                                 }

                                               }

                                             })) %>%
    dplyr::select(-FirstAge:-yr_diff)

}
