#' Calculate first and last breeding season when a nestbox was used by birds
#'
#' Currently, the variables FirstSeason and LastSeason in Location data are
#' defined as first and last breeding seasons data collectors put out a nestbox.
#' However, this data may often not be available, in which case it may be useful
#' to know instead in which breeding season a nestbox was first and last used by
#' breeding birds.
#'
#' This function uses information from the standardized Brood data table to
#' calculate the first and last breeding season when each nestbox in each
#' population was used by birds
#'
#' For the moment, if a nestbox was used in the latest year of data available, this
#' year is returned in LastSeason (which differs from the definition in the protocol,
#' which states that LastSeason for still actively used nestboxes should be NA).
#'
#' The function assumes that NestBoxID = LocationID.
#'
#' @param Brood_data Data frame. Data frame with standardized brood data.
#' @return A data frame with all combinations of `PopID` and `NestBoxID`
#'  and corresponding `StartSeason` and `EndSeason`.
#' @export
#'
#' @examples
#' library(dplyr)
#' Brood_data <- tibble::tibble(BroodID = rep(c(1:10), 2), PopID = rep('XXX', 'YYY', each = 2),
#'                              LocationID = rep(LETTERS[c(1,3,1,1,2,3,1,2,1,2)], 2),
#'                              BreedingSeason = sort(sample(2012:2016, 10, replace = TRUE)))
#' calc_birdNBuse(Brood_data)

calc_birdNBuse <- function(Brood_data){

  output <- Brood_data %>%
    dplyr::group_by(.data$PopID, .data$LocationID) %>%
    dplyr::summarise(StartSeason  = as.integer(min(.data$BreedingSeason)),
                  EndSeason  = as.integer(max(.data$BreedingSeason)),
                  .groups = 'keep') %>%
    dplyr::ungroup()

  return(output)

}
