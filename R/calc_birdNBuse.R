#' Calculate first and last breeding season when a nestbox was used by birds
#'
#' Currently, the variables FirstSeason and LastSeason in Location data are
#' defined as first and last breeding seasons data collectors put out a nestbox.
#' However, this data may often not be available, in which case it may be useful
#' to know instead in which breeding season a nestbox was first and last used by
#' breeding birds.
#'
#' This function uses information from the standardized Brood data table to
#' calculate the first and last breeding season when each nestbox was used by birds
#'
#' For the moment, if a nestbox was used in the latest year of data available, this
#' year is returned in LastSeason (which differs from the definition in the protocol,
#' which states that LastSeason for still actively used nestboxes should be NA).
#'
#' The function assumes that NestBoxID = LocationID.
#'
#' @param Brood_data Data frame. Data frame with standardized brood data.
#' @return A data frame with the NestBoxID and corresponding FirstSeason and LastSeason.
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(666)
#' Brood_data <- tibble::tibble(BroodID = c(1:10), LocationID = LETTERS[c(1,3,1,1,2,3,1,2,1,2)],
#'                              BreedingSeason = sort(sample(2012:2016, 10, replace = TRUE)))
calc_birdNBuse <- function(Brood_data){

  output <- Brood_data %>%
    dplyr::arrange(LocationID, BreedingSeason) %>%
    dplyr::group_by(LocationID) %>%
    dplyr::mutate(StartSeason  = as.integer(first(BreedingSeason)),
                  EndSeason  = as.integer(last(BreedingSeason))) %>%
    dplyr::ungroup() %>%
    dplyr::select(LocationID, StartSeason, EndSeason) %>%
    base::unique()

  return(output)

  #Satisfy RCMD Checks
  LocationID <- BreedingSeason <- BroodID <- StartSeason <- EndSeason <- NULL

}
