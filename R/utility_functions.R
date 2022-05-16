#--------------------------------------------------------------#
# Standard utility functions
#
# Standard utility functions are functions that generate additional variables that are not part of standard format but can be added to pipeline outputs when requested by the user. Often these variables are not obtained from direct observations in the field but derived from other variables (e.g., an individual's age is derived from its year of ringing/birth and the current year).
#
# Optional variables:
# - breedingSeason, calc_season()
# - calculatedClutchType, calc_clutchtype()
# - nestAttemptNumber, calc_nestattempt()
# - calculatedSex, calc_sex()
# - exactAge, calc_age()
# - minimumAge, calc_age()
#--------------------------------------------------------------#

#' Determine the breeding season in which a brood was laid
#'
#' Determine the breeding season based on the breeding biology of a species at a study site.
#'
#' The breeding season is determined as follows:
#' \itemize{
#'   \item{Species with 1 breeding season per calendar year} \itemize{
#'     \item{Breeding season falls completely within a single calendar year: \code{breedingSeason} = calendar year. For example, the breeding season from April 2022 to June 2022 is marked "2022".}
#'     \item{Breeding season is spread over two calendar years: \code{breedingSeason} = calendar year in which the season \emph{starts}. For example, the breeding season from December 2021 to February 2022 is marked "2021".}
#'   }
#'   \item{Species with >1 season per calendar year} \itemize{
#'     \item{All breeding seasons fall completely within a single calendar year: \code{breedingSeason} = <calendar year>_<season number>. For example, the first breeding season (from April 2022 to May 2022) is marked "2022_1", and the second breeding season (from August 2022 to September 2022) is marked "2022_2".}
#'     \item{Not all breeding seasons fall completely within a single calendar year: \code{breedingSeason} = <calendar year>_<season number>, where calendar year is the year in which the \emph{first} season \emph{starts}. For example, the first breeding season (from October 2021 to November 2021) is marked "2021_1", and the second breeding season (from December 2021 to January 2022) is marked "2021_2".}
#'   }
#' }
#'
#' @param data Data frame with brood information.
#'
#' @return A character vector with breeding season.
#'
#' @export
#'

calc_season <- function(data) {

  ## FIXME: Use info from data owners?
  breedingSeason <- data %>%
    dplyr::mutate(breedingSeason = dplyr::case_when(.data$speciesID %in% {{}} ~ ,
                                                    TRUE ~ as.character(.data$observedLayYear))) %>%
    dplyr::select(.data$breedingSeason)

  return(breedingSeason)

}


