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
#'   Name of column with CaptureDate information. Must be in format YYYY-MM-DD.
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
#' mutate(Age_calculated = calc_age(data = ., ID = IndvID, Age = Age_obsv,
#' Date = CaptureDate, Year = SampleYear)


calc_age <- function(data, ID, Age, Date, Year, showpb = TRUE){

  pb <- progress::progress_bar$new(total = nrow(data))

  output <- data %>%
    dplyr::arrange({{ID}}, {{Year}}, {{Date}}) %>%
    dplyr::group_by({{ID}}) %>%
    dplyr::mutate(FirstAge  = as.integer(first({{Age}})),
                  FirstYear = as.integer(first({{Year}}))) %>%
    dplyr::mutate(yr_diff   = as.integer({{Year}}) - FirstYear,
                  Age_calculated = purrr::pmap_int(.l = list(FirstAge, yr_diff, {{Age}}, {{ID}}),
                                                   .f = ~{

                                                     if(showpb){

                                                       pb$tick()

                                                     }

                                                     if(is.na(..4)){

                                                       return(NA_integer_)

                                                     }

                                                     #If the capture has no age or the age is greater than 3
                                                     #where 3 is within first year...
                                                     if(is.na(..1) | ..1 > 3) {

                                                       #Give an age which is at least >1yo
                                                       return(4L + 2L*..2)

                                                       #If the age is 2 (i.e. can't even tell if it's a chick/adult)...
                                                     } else if(..1 == 2){

                                                       #Start at 2 instead of 4
                                                       return(2L + 2L*..2)

                                                     } else {

                                                       #Otherwise, if it was first caught as a chick
                                                       #and it's the same year of chick capture.
                                                       if(..2 == 0L){

                                                         #Return the recorded age
                                                         #This is important because it could also have been
                                                         #captured as a fledgling that year, in which
                                                         #case we don't want to give it a value of 1 (pullus)
                                                         return(..3)

                                                       } else {

                                                         #If it's later than the year of birth,
                                                         #give it a known age.
                                                         return(3L + 2L*..2)

                                                       }

                                                     }

                                                   })) %>%
    dplyr::ungroup() %>%
    pull(Age_calculated)


  return(output)

  #Satisfy RCMD Checks
  FirstYear <- FirstAge <- yr_diff <- NULL

}
