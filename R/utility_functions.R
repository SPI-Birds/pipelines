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
#' Determine the breeding season based on the data owner's expertise on the breeding biology of the species at the study site.
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
#' @param season Name of the variable in primary data that marks the breeding seasons according to the data owner. This information is necessary if a single breeding season spreads over two calendar years, or if multiple seasons occur in a single calendar year.
#'
#' @return A character vector with breeding season.
#'
#' @export
#'

calc_season <- function(data,
                        season = NULL) {

  # In most cases, there is one breeding season that falls completely within a single calendar year.
  # Then we use the values stored in observedLayYear to determine the breeding season.
  if(is.null(season)) {

    breedingSeason <- data %>%
      dplyr::mutate(breedingSeason = as.character(.data$observedLayYear)) %>%
      dplyr::pull(.data$breedingSeason)

    # In cases that deviate from the default (1 breeding season in 1 year),
    # we will use the information from the data owner to mark the different seasons
  } else {

    breedingSeason <- data %>%
      # Determine year of breeding per season (as identified by data owner)
      dplyr::group_by(!!rlang::sym(season)) %>%
      dplyr::mutate(breedingYear = paste0(dplyr::first(.data$observedLayYear))) %>%
      # Determine number of seasons per year
      dplyr::group_by(.data$breedingYear) %>%
      dplyr::mutate(seasonNumber = as.numeric(as.factor(!!rlang::sym(season)))) %>%
      dplyr::ungroup() %>%
      # Determine breeding season. If more than 1 season, use seasonNumber as suffix; otherwise, just use breedingYear
      dplyr::mutate(breedingSeason = dplyr::case_when(any(.data$seasonNumber > 1) ~ paste(.data$breedingYear, .data$seasonNumber, sep = "_"),
                                                      TRUE ~ as.character(.data$breedingYear))) %>%
      dplyr::pull(.data$breedingSeason)

  }

  return(breedingSeason)

}


#' Calculate clutch type (when laying date is in date format)
#'
#' Use info on laying date and fledge data to determine the clutch type of a
#' given brood
#'
#' @param data Data frame with brood information.
#' @param na.rm Logical. Should NAs be removed and treated as 0s? If TRUE, NAs are treated as 0s. If FALSE, NAs
#'   are treated as true unknowns.
#' @param protocol_version Character string. The protocol version of the SPI Birds
#' standard data being used to process the primary data. Either "1.0" (default) or "1.1".
#'
#' @return A character vector with either 'first', 'replacement', 'second', or
#'   NA.
#' @export
#'
#' @examples
#' #Create fake dataset
#' set.seed(666)
#' dat <- tibble::tibble(PopID = "TEST", Species = "PARMAJ",
#' FemaleID = sample(LETTERS[1:7], size = 100, replace = TRUE),
#' NumberFledged = rpois(n = 100, lambda = 1),
#' #Create 100 fake broods
#' BreedingSeason = sample(c(seq(2000, 2012, 1)), size = 100, replace = TRUE),
#' LayDate = as.Date(paste(BreedingSeason,
#'                            sample(c(4, 5, 6), size = 100, replace = TRUE),
#'                            sample(seq(1, 31, 1), size = 100, replace = TRUE), sep = "-"),
#'                            format = "%Y-%m-%d"),
#' LayDate_observed = LayDate)
#' calc_clutchtype(data = dat, na.rm = FALSE)
calc_clutchtype <- function(data,
                            protocol_version = "1.0",
                            na.rm = FALSE) {

  ## Version 1.0
  if (protocol_version == "1.0"){
    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason, .data$Species, .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .$row,
                                                                        femID = .$FemaleID,
                                                                        cutoff_date = .$cutoff,
                                                                        nr_fledge_before = .$total_fledge,
                                                                        LD = .$LayDate),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before,
                                                                            LD){

                                                                # clutchtype$tick()$print()

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    return("replacement")

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull(.data$ClutchType_calculated)

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .$row,
                                                                        femID = .$FemaleID,
                                                                        cutoff_date = .$cutoff,
                                                                        nr_fledge_before = .$total_fledge,
                                                                        na_fledge_before = .$total_fledge_na,
                                                                        LD = .$LayDate),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before, na_fledge_before,
                                                                            LD){

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    #If there was atleast one NA record before this one
                                                                    #then we don't know if number of fledged before is
                                                                    #0 or >0. Therefore, we have to say NA.
                                                                    if(na_fledge_before > 0){

                                                                      return(NA)

                                                                    } else {

                                                                      #Otherwise, we can be confident that
                                                                      #number of fledge before is 0
                                                                      #and it must be a replacement
                                                                      return("replacement")

                                                                    }

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull(.data$ClutchType_calculated)

    }

  }

  ## Version 1.1
  if (protocol_version == "1.1"){

    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID, .data$BreedingSeason, .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate_observed, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason, .data$Species, .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .$row,
                                                                        femID = .$FemaleID,
                                                                        cutoff_date = .$cutoff,
                                                                        nr_fledge_before = .$total_fledge,
                                                                        LD = .$LayDate_observed),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before,
                                                                            LD){

                                                                # clutchtype$tick()$print()

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    return("replacement")

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull(.data$ClutchType_calculated)

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .$row,
                                                                        femID = .$FemaleID,
                                                                        cutoff_date = .$cutoff,
                                                                        nr_fledge_before = .$total_fledge,
                                                                        na_fledge_before = .$total_fledge_na,
                                                                        LD = .$LayDate_observed),
                                                              .f = function(rows, femID, cutoff_date,
                                                                            nr_fledge_before, na_fledge_before,
                                                                            LD){

                                                                #Firstly, check if the nest has a LD
                                                                #If not, we cannot calculate BroodType

                                                                if(is.na(LD)){

                                                                  return(NA)

                                                                }

                                                                #Next, check if the female is banded
                                                                #If a female is unbanded we assume the nest can NEVER be secondary
                                                                #If she had had a successful clutch before she would have been caught and banded
                                                                #Therefore it can only be first or replacement (based on 30d rule)
                                                                if(is.na(femID)){

                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    return("first")

                                                                  }

                                                                }

                                                                #If she is banded, then we need to apply all rules
                                                                #If it's the first nest recorded for this female in this year...
                                                                if(rows == 1){

                                                                  #If it doesn't meet the 30 day rule, then name it as replacement
                                                                  if(LD > cutoff_date){

                                                                    return("replacement")

                                                                  } else {

                                                                    #Otherwise, we assume it was the first clutch
                                                                    return("first")

                                                                  }

                                                                  #If it's NOT the first nest of the season for this female
                                                                } else {

                                                                  #If there have been no fledglings before this point..
                                                                  if(nr_fledge_before == 0){

                                                                    #If there was atleast one NA record before this one
                                                                    #then we don't know if number of fledged before is
                                                                    #0 or >0. Therefore, we have to say NA.
                                                                    if(na_fledge_before > 0){

                                                                      return(NA)

                                                                    } else {

                                                                      #Otherwise, we can be confident that
                                                                      #number of fledge before is 0
                                                                      #and it must be a replacement
                                                                      return("replacement")

                                                                    }

                                                                  } else {

                                                                    #If there has been atleast one clutch
                                                                    #that previously produced fledgligns
                                                                    #then this nest is 'second'
                                                                    #N.B. This is the case even if one of the previous nests
                                                                    #was an NA. We just need to know if it's >0, not the exact number
                                                                    return("second")

                                                                  }

                                                                }

                                                              })) %>%
        dplyr::pull(.data$ClutchType_calculated)

    }

  }

  ## Version 1.2
  if (protocol_version == "1.2") {



  }

  return(clutchtype_calculated)

  #Satisfy RCMD Checks
  PopID <- BreedingSeason <- Species <- LayDate <- FemaleID <- NumberFledged_observed <- NULL
  `.` <- ClutchType_calculated <- NULL

}


#' Calculate cumulative number of fledgings
#'
#' For a given nest, determine the cumulative number of fledglings in all nests
#' before this. This is used to calculate ClutchType_calc. The function also
#' includes functionality to return whether number of fledglings in previous
#' nests was measured or not (i.e. were there NAs in any nests before the
#' current one). This is neccesary in populations where the number of fledglings
#' is not measured consistently
#' @param x Column NumberFledged in the Brood_data table
#' @param na.rm Logical. If TRUE, returns cumulative number of fledglings
#' where NA is assumed to be 0. If FALSE, returns a vector of logicals
#' showing whether any nests before the current nest were unmeasured (NA).
#'
#' @return A vector of numerics (if na.rm = TRUE) or logicals (na.rm = FALSE)
#' @export
#'
#' @examples
#' #Assuming NA is 0
#' #Return vector of numerics.
#' calc_cumfledge(x = c(1, 3, NA, 4), na.rm = TRUE)
#'
#' #Do not assume NA is 0.
#' #Return a vector of logicals showing whether an NA occurred before
#' #the current record.
#' calc_cumfledge(x = c(1, 3, NA, 4), na.rm = FALSE)
calc_cumfledge <- function(x, na.rm = TRUE){

  if(na.rm){

    #This func assumes that all NAs are just 0s.
    #This is needed because otherwise cumsum returns all NAs
    #However, all we need to know is if there was atleast 1 successful nest before the current nest
    x[!stats::complete.cases(x)] <- 0

    nrs <- cumsum(x)

  } else {

    x <- is.na(x)

    nrs <- cumsum(x)

  }

  if(length(x) == 1){

    return(0)

  } else {

    return(c(0, nrs[1:(length(nrs) - 1)]))

  }

}
