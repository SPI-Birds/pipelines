#' Calculate age based on when an individual was first captured
#'
#' Arrange data by individual and year and then determine potential age in each capture.
#'
#' We only consider whether an individual was captured as a chick or not.
#' We don't consider the adult age when an individual was first captured (e.g. EURING 4 or 5)
#' This prevents any cases where an individual might be wrongly aged at first capture.
#' @param FirstYr Numeric. When an individual was first captured.
#' @param CurrentYr Numeric. Current year of capture.
#' @param FirstAge Numeric. Age of individual when first caught, using EURING codes.
#'
#' @return A numeric vector of EURING codes.
#' @export
#'
#' @examples
#' set.seed(666)
#' bird_data <- tibble::tibble(IndvID = LETTERS[sample(1:26, 100, replace = TRUE)],
#' SampleYear = sample(2012:2016, 100, replace = TRUE),
#' Age_obsv = sample(c(1, 4), 100, replace = TRUE)) %>%
#' calc_age(ID = IndvID, Age = Age_obsv, Year = SampleYear)
calc_age <- function(data, ID, Age, Year){

  data %>%
    dplyr::arrange({{ID}}, {{Year}}) %>%
    dplyr::group_by({{ID}}) %>%
    dplyr::mutate(FirstAge  = first({{Age}}),
                  FirstYear = first({{Year}})) %>%
    dplyr::mutate(was_chick = FirstAge == 1,
                  yr_diff   = {{Year}} - FirstYear,
                  Age_calc = purrr::pmap_dbl(.l = list(was_chick, yr_diff),
                                             .f = ~{

                                               if(..1){

                                                 if(..2 == 0){

                                                   return(1)

                                                 } else {

                                                   return(3 + 2*..2)

                                                 }

                                               } else {

                                                 return(4 + 2*..2)

                                               }

                                             })) %>%
    dplyr::select(-FirstAge:-yr_diff)

}
