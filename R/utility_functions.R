#--------------------------------------------------------------#
# Standard utility functions
#
# Standard utility functions are functions that generate additional variables that are not part of standard format but can be added to pipeline outputs when requested by the user. Often these variables are not obtained from direct observations in the field but derived from other variables (e.g., an individual's age is derived from its date of ringing and the current date).
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
#' @param season Unquoted expression (i.e. character without quotation marks). Name of the variable in primary data that marks the
#'   breeding seasons according to the data owner. This information is necessary if a single breeding season spreads over
#'   two calendar years, or if multiple seasons occur in a single calendar year.
#'
#' @return A data frame with breedingSeason.
#'
#' @export
#'
#' @examples
#' # 1 breeding season per calendar year
#' dat <- tibble::tibble(siteID = "AAA",
#'                       speciesID = "PARMAJ",
#'                       femaleID = LETTERS[1:10],
#'                       observedLayYear = c(rep(2020, 5), rep(2021, 5)))
#'
#' calc_season(data = dat)
#'
#' # 1 breeding season over 2 calendar years
#' dat <- tibble::tibble(siteID = "AAA",
#'                       speciesID = "PARMAJ",
#'                       femaleID = LETTERS[1:10],
#'                       observedLayYear = c(rep(2020, 3), rep(2021, 5), rep(2022, 2)),
#'                       season = rep(1:2, each = 5))
#'
#' calc_season(data = dat, season = "season")
#'
#' # 2 breeding seasons per calendar year
#' dat <- tibble::tibble(siteID = "AAA",
#'                       speciesID = "PARMAJ",
#'                       femaleID = LETTERS[1:12],
#'                       observedLayYear = c(rep(2020, 6), rep(2021, 6)),
#'                       season = rep(1:4, each = 3))
#'
#' calc_season(data = dat, season = "season")

calc_season <- function(data,
                        season) {

  # In most cases, there is one breeding season that falls completely within a single calendar year.
  # Then we use the values stored in observedLayYear to determine the breeding season.
  if(missing(season)) {

    breedingSeason <- data %>%
      dplyr::mutate(breedingSeason = as.character(.data$observedLayYear))

    # In cases that deviate from the default (1 breeding season in 1 year),
    # we will use the information from the data owner to mark the different seasons
  } else {

    breedingSeason <- data %>%
      # Determine year of breeding per season (as identified by data owner)
      dplyr::group_by({{season}}) %>%
      dplyr::mutate(breedingYear = paste0(dplyr::first(.data$observedLayYear))) %>%
      # Determine number of seasons per year
      dplyr::group_by(.data$breedingYear) %>%
      dplyr::mutate(seasonNumber = as.numeric(as.factor({{season}}))) %>%
      dplyr::ungroup() %>%
      # Group broods to breeding season. If more than 1 season per year,
      # use seasonNumber as suffix; otherwise, just use breedingYear
      dplyr::mutate(breedingSeason = dplyr::case_when(any(.data$seasonNumber > 1) ~ paste(.data$breedingYear,
                                                                                          .data$seasonNumber,
                                                                                          sep = "_"),
                                                      TRUE ~ as.character(.data$breedingYear))) %>%
      dplyr::select(-"breedingYear",
                    -"seasonNumber")

  }

  return(breedingSeason)

}


#' Calculate clutch type (when laying date is in date format)
#'
#' Use info on laying date and fledge data to determine the clutch type of a
#' given brood.
#'
#' @param data Data frame with brood information.
#' @param na.rm Logical. Should NAs be removed and treated as 0s? If TRUE, NAs are treated as 0s. If FALSE, NAs
#'   are treated as true unknowns.
#' @param protocol_version Character string. The protocol version of the SPI Birds
#' standard data being used to process the primary data. Either "1.0" (default), "1.1", or "2.0".
#'
#' @return A character vector with either 'first', 'replacement', 'second', or NA (v1.0 or v1.1), or a data frame with calculatedClutchType (v1.2), which takes either 'first', 'replacement', 'second', or NA.
#'
#' @export
#'
#' @examples
#' #Create fake dataset
#' set.seed(666)
#'
#' dat <- tibble::tibble(PopID = "TEST", Species = "PARMAJ",
#'                       FemaleID = sample(LETTERS[1:7], size = 100, replace = TRUE),
#'                       NumberFledged = rpois(n = 100, lambda = 1),
#'                       #Create 100 fake broods
#'                       BreedingSeason = sample(c(seq(2000, 2012, 1)), size = 100, replace = TRUE),
#'                       LayDate = as.Date(paste(BreedingSeason,
#'                                               sample(c(4, 5, 6), size = 100, replace = TRUE),
#'                                               sample(seq(1, 31, 1), size = 100, replace = TRUE), sep = "-"),
#'                                         format = "%Y-%m-%d"),
#'                       LayDate_observed = LayDate)
#'
#' calc_clutchtype(data = dat, na.rm = FALSE)

calc_clutchtype <- function(data,
                            protocol_version = "1.0",
                            na.rm = FALSE) {

  ## Version 1.0
  if (protocol_version == "1.0"){
    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID,
                      .data$BreedingSeason,
                      .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason,
                      .data$Species,
                      .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        LD = .data$LayDate),
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
        dplyr::pull("ClutchType_calculated")

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        na_fledge_before = .data$total_fledge_na,
                                                                        LD = .data$LayDate),
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
        dplyr::pull("ClutchType_calculated")

    }

  }

  ## Version 1.1
  if (protocol_version == "1.1"){

    cutoff_dat <- data %>%
      dplyr::group_by(.data$PopID,
                      .data$BreedingSeason,
                      .data$Species) %>%
      dplyr::mutate(cutoff = tryCatch(expr = min(.data$LayDate_observed, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$BreedingSeason,
                      .data$Species,
                      .data$FemaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        LD = .data$LayDate_observed),
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
        dplyr::pull("ClutchType_calculated")

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$NumberFledged_observed, na.rm = FALSE),
                      row = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ClutchType_calculated = purrr::pmap_chr(.l = list(rows = .data$row,
                                                                        femID = .data$FemaleID,
                                                                        cutoff_date = .data$cutoff,
                                                                        nr_fledge_before = .data$total_fledge,
                                                                        na_fledge_before = .data$total_fledge_na,
                                                                        LD = .data$LayDate_observed),
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
        dplyr::pull("ClutchType_calculated")

    }

  }

  ## Version 2.0
  if (protocol_version == "2.0") {

    cutoff_dat <- data %>%
      dplyr::group_by(.data$siteID,
                      .data$breedingSeason,
                      .data$speciesID) %>%
      dplyr::mutate(observedLayDate = lubridate::make_date(.data$observedLayYear,
                                                           .data$observedLayMonth,
                                                           .data$observedLayDay),
                    cutoff = tryCatch(expr = min(.data$observedLayDate, na.rm = TRUE) + lubridate::days(30),
                                      warning = function(...) return(NA))) %>%
      # Determine brood type for each nest based on female ID
      dplyr::group_by(.data$breedingSeason,
                      .data$speciesID,
                      .data$femaleID)

    #Depending on whether NAs should be treated as 0s or NAs we have different paths
    if(na.rm == TRUE){

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::arrange(.data$breedingSeason,
                       .data$femaleID,
                       .data$observedLayDate) %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$observedNumberFledged, na.rm = TRUE),
                      rowNumber = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(calculatedClutchType = purrr::pmap_chr(.l = list(rows = .data$rowNumber,
                                                                       femID = .data$femaleID,
                                                                       cutoff_date = .data$cutoff,
                                                                       nr_fledge_before = .data$total_fledge,
                                                                       LD = .data$observedLayDate),
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
        dplyr::select(-"observedLayDate",
                      -"cutoff",
                      -"total_fledge",
                      -"rowNumber")

    } else {

      clutchtype_calculated <- cutoff_dat %>%
        dplyr::arrange(.data$breedingSeason,
                       .data$femaleID,
                       .data$observedLayDate) %>%
        dplyr::mutate(total_fledge = calc_cumfledge(x = .data$observedNumberFledged, na.rm = TRUE),
                      total_fledge_na = calc_cumfledge(x = .data$observedNumberFledged, na.rm = FALSE),
                      rowNumber = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(calculatedClutchType = purrr::pmap_chr(.l = list(rows = .data$rowNumber,
                                                                       femID = .data$femaleID,
                                                                       cutoff_date = .data$cutoff,
                                                                       nr_fledge_before = .data$total_fledge,
                                                                       na_fledge_before = .data$total_fledge_na,
                                                                       LD = .data$observedLayDate),
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
        dplyr::select(-"observedLayDate",
                      -"cutoff",
                      -"total_fledge",
                      -"total_fledge_na",
                      -"rowNumber")

    }

  }

  return(clutchtype_calculated)

  #Satisfy RCMD Checks
  PopID <- BreedingSeason <- Species <- LayDate <- FemaleID <- NumberFledged_observed <- NULL
  `.` <- ClutchType_calculated <- NULL

}


#' Calculate cumulative number of fledgings
#'
#' For a given nest, determine the cumulative number of fledglings in all nests
#' before this. This is used to calculate calculatedClutchType using \code{\link{calc_clutchtype}}.
#' The function also includes functionality to return whether number of fledglings in previous
#' nests was measured or not (i.e., were there NAs in any nests before the
#' current one). This is necessary in populations where the number of fledglings
#' is not measured consistently.
#'
#' @param x Column observedNumberFledged in the Brood_data table
#' @param na.rm Logical. If TRUE, returns cumulative number of fledglings
#' where NA is assumed to be 0. If FALSE, returns a vector of logicals
#' showing whether any nests before the current nest were unmeasured (NA).
#'
#' @return A vector of numeric values (if na.rm = TRUE) or logical values (na.rm = FALSE).
#'
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

calc_cumfledge <- function(x,
                           na.rm = TRUE){

  if(na.rm){

    #This func assumes that all NAs are just 0s.
    #This is needed because otherwise cumsum returns all NAs
    #However, all we need to know is if there was at least 1 successful nest before the current nest
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


#' Calculate sequence of nest attempts by a breeding pair in a season
#'
#' Arrange data by breeding pairs (femaleID & maleID) and laying date to
#' calculate the sequence of nest attempts in a single season
#'
#' If either female or male was not identified, the nest is assumed to be the pair's first attempt.
#'
#' @param data Data frame with brood information.
#' @param season Unquoted expression (i.e. character without quotation marks). Name of the variable in primary data that marks the
#'   breeding seasons according to the data owner. This information is necessary if a single breeding season spreads over
#'   two calendar years, or if multiple seasons occur in a single calendar year.
#'
#' @return A data frame with nestAttemptNumber.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Single season in a single calendar year (no season information needed)
#' bird_data <- tibble::tibble(broodID = 1:7,
#'                             femaleID = c("F1", "F1", "F2", "F2", "F3", "F3", NA),
#'                             maleID = c("M1", "M1", "M2", "M2", "M3", NA, "M3"),
#'                             observedLayYear = c(2020, 2020, 2020, 2021, 2022, 2022, 2022),
#'                             observedLayMonth = rep(5, 7),
#'                             observedLayDay = rep(1, 7)) %>%
#'   calc_nestattempt()
#'
#' # Single season over two calendar years (season information needed)
#' bird_data <- tibble::tibble(broodID = 1:7,
#'                             femaleID = c("F1", "F1", "F2", "F2", "F3", "F3", NA),
#'                             maleID = c("M1", "M1", "M2", "M2", "M3", NA, "M3"),
#'                             season = c(1, 1, 1, 2, 3, 3, 3),
#'                             observedLayYear = c(2020, 2021, 2020, 2021, 2022, 2022, 2022),
#'                             observedLayMonth = c(12, 1, rep(12, 5)),
#'                             observedLayDay = rep(1, 7)) %>%
#'   calc_nestattempt(season = season)

calc_nestattempt <- function(data,
                             season) {

  # In most cases, there is one breeding season that falls completely within a single calendar year.
  # Then we use the values stored in observedLayYear as an indication of breeding season.
  if(missing(season)) {

    output <- data %>%
      # Determine breeding pair: concatenate femaleID & maleID
      dplyr::mutate(breedingPair = paste(.data$femaleID,
                                         .data$maleID,
                                         sep = "-")) %>%
      dplyr::group_by(.data$observedLayYear,
                      .data$breedingPair) %>%
      # Arrange by laying date
      dplyr::arrange(.data$observedLayYear,
                     .data$observedLayMonth,
                     .data$observedLayDay) %>%
      # If either female or male could not be identified, the nest is assumed to be the first attempt
      dplyr::mutate(nestAttemptNumber = dplyr::case_when(stringr::str_detect(.data$breedingPair, "NA") ~ 1L,
                                                         TRUE ~ as.integer(dplyr::row_number()))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"breedingPair")

    # In cases that deviate from the default (1 breeding season in 1 year),
    # we need to use information from the data owner to mark the different seasons
  } else {

    output <- data %>%
      # Determine breeding pair: concatenate femaleID & maleID
      dplyr::mutate(breedingPair = paste(.data$femaleID,
                                         .data$maleID,
                                         sep = "-")) %>%
      dplyr::group_by({{season}},
                      .data$breedingPair) %>%
      # Arrange by season & laying date
      dplyr::arrange({{season}},
                     .data$observedLayYear,
                     .data$observedLayMonth,
                     .data$observedLayDay) %>%
      # If either female or male could not be identified, the nest is assumed to be the first attempt
      dplyr::mutate(nestAttemptNumber = dplyr::case_when(stringr::str_detect(.data$breedingPair, "NA") ~ 1L,
                                                         TRUE ~ as.integer(dplyr::row_number()))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"breedingPair")

  }

  return(output)

}


#' Determine sex of an individual
#'
#' Conclude the sex of an individual based on sex scores during captures (observedSex).
#'
#' An individual is recorded as having a conflicting sex ('C') when it has been sexed differently during
#' different captures.
#'
#' @param individual_data Data frame with individual information.
#' @param capture_data Data frame with capture information.
#'
#' @return A data frame (individual information) with calculatedSex, which takes either 'F', 'M', 'C', or NA.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' capture_data <- tibble::tibble(individualID = rep(paste0("I", 1:4), each = 2),
#'                                observedSex = c("F", "F", "M", NA, "F", "M", NA, NA))
#'
#' individual_data <- tibble::tibble(individualID = paste0("I", 1:4)) %>%
#'   calc_sex(., capture_data)

calc_sex <- function(individual_data,
                     capture_data) {

  # Retrieve an individual's sex information its captures
  sex_calculated <- capture_data %>%
    dplyr::filter(!is.na(.data$observedSex)) %>%
    dplyr::group_by(.data$individualID) %>%
    # Determine the number of different sex scores per individualID
    dplyr::summarise(unique_sex = paste(unique(.data$observedSex),
                                        collapse = "-")) %>%
    dplyr::ungroup() %>%
    # Keep one record per individual
    dplyr::distinct() %>%
    # If the individualID has not been consistently assigned the same sex,
    # calculatedSex is set to 'C'
    dplyr::mutate(calculatedSex = dplyr::case_when(nchar(.data$unique_sex) > 1 ~ "C",
                                                   TRUE ~ .data$unique_sex)) %>%
    dplyr::select("individualID",
                  "calculatedSex")

  # Merge calculatedSex information into individual data
  output <- individual_data %>%
    dplyr::left_join(sex_calculated,
                     by = "individualID")

  return(output)

}

#' Calculate age based on when an individual was first captured
#'
#' We only consider whether an individual was captured as a chick or not. We don't consider the numeric age when an individual was first captured. This prevents any cases where an individual might be wrongly aged at first capture.
#'
#' When there is no observed age at first capture we assume it couldn't be a chick or this would've been recorded.
#'
#' \strong{Version 2.0}
#'
#' From version 2.0 onwards, age is no longer part of the standard format, but optionally available through this standard utility function. In addition, age is now provided as two variables, exactAge and minimumAge. \emph{exactAge} is the exact age of individuals, which can only be determined for individuals first captured as chicks/fledglings. \emph{minimumAge} is the minimum age of individuals, which can be determined for all individuals. In case of chicks/fledglings, \emph{exactAge} and \emph{minimumAge} are identical.
#'
#' The default behaviour of this function is that age is determined as number of years since birth (exactAge) or ringing (minimumAge). Age increases each time the ringing date is passed. For example, an individual born on 01/07/2022 will become 1 at 01/07/2023. Note: for individuals for which the date of first capture is incomplete or unknown (e.g., only the year is known), this function will return NA.
#'
#' @param data Data frame. Data frame with capture information.
#' @param ID Unquoted expression (i.e. character without quotation marks). Name
#'   of column with individual identity. NB: Not used for v2.0.
#' @param Age Unquoted expression (i.e. character without quotation marks). Name
#'   of column with observed age of individual in each capture. For v1.0 & v1.1, these must be in EURING codes.
#'   For v2.0: these indicate the life stage of individuals, e.g., chick, subadult, adult.
#'   If NA, individuals are assumed to be adults.
#' @param Date Unquoted expression (i.e. character without quotation marks).
#'   Name of column with captureDate information. Must be in format dd/mm/yyyy. NB: Not used for v2.0.
#' @param Year Unquoted expression (i.e. character without quotation marks).
#'   Name of column with year information. NB: This could be different to
#'   captureDate if we are dealing with species that breed over two year (e.g.
#'   Southern Hemisphere species).
#' @param protocol_version Character string. The protocol version of the SPI Birds
#'   standard data being used to process the primary data. Either "1.0" (default), "1.1", or "2.0".
#' @param showpb Logical. Should a progress bar be shown?
#'
#' @return A data frame with calculatedAge (v1.0, v1.1) or exactAge and minimumAge (v2.0).
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(74)
#'
#' # Version 1.0 or 1.1
#' bird_data <- tibble::tibble(IndvID = LETTERS[sample(1:26, 100, replace = TRUE)],
#'                             SampleYear = sample(2012:2016, 100, replace = TRUE),
#'                             CaptureDate = as.Date(paste(SampleYear, 3, 31, sep = "-"), format = "%Y-%m-%d")
#'                             + sample(1:30, 100, replace = TRUE),
#'                             Age_obsv = sample(c(1L, 4L), 100, replace = TRUE)) %>%
#'   calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate, Year = SampleYear)
#'
#' # Version 2.0
#' # Ringed as chick
#' bird_data <- tibble::tibble(captureYear = c(2001, 2002, 2003, 2004),
#'                             captureMonth = c(5, 5, 4, 1),
#'                             captureDay = rep(1, 4),
#'                             individualID = rep("A1", 4),
#'                             stage = c("chick", "adult", "adult", "adult"),
#'                             chickAge = c(15, NA, NA, NA)) %>%
#'   calc_age(Age = .data$stage, protocol_version = "2.0")
#'
#' # Ringed as adult
#' bird_data <- tibble::tibble(captureYear = c(2001, 2002, 2003, 2004),
#'                             captureMonth = c(5, 5, 4, 1),
#'                             captureDay = rep(1, 4),
#'                             individualID = rep("A1", 4),
#'                             stage = rep("adult", 4),
#'                             chickAge = rep(NA, 4)) %>%
#'   calc_age(Age = .data$stage, protocol_version = "2.0")

calc_age <- function(data, ID, Age, Date, Year, protocol_version = "1.0", showpb = TRUE){

  message("Calculating age. This can take some time for larger datasets.")

  # Version 1.0-1.1
  if(protocol_version %in% c("1.0", "1.1")) {

    output <- data %>%
      dplyr::arrange({{ID}}, {{Year}}, {{Date}}) %>%
      #For each individual, calculate their first age and year of capture
      dplyr::group_by({{ID}}) %>%
      dplyr::mutate(FirstAge  = as.integer(dplyr::first({{Age}})),
                    FirstYear = as.integer(dplyr::first({{Year}})),
                    yr_diff   = as.integer({{Year}}) - .data$FirstYear) %>%
      dplyr::ungroup() %>%
      ## TODO: Look at using apply functions.
      #Then calculate age from these data
      #Rule 1: If individual was not caught as a chick (i.e. age is NA or >3) then first capture is age 4 (after first year, birth year unknown)
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
      dplyr::select(-.data$FirstAge, -.data$FirstYear, -.data$yr_diff)

  }

  # Version 2.0
  if(protocol_version == "2.0") {

    # Age is calculated based on the number of years since ringing/birth
    output <- data %>%
      dplyr::mutate(captureDate = lubridate::make_date(.data$captureYear, .data$captureMonth, .data$captureDay)) %>%
      dplyr::arrange(.data$individualID, .data$captureYear, .data$captureDate) %>%
      # For each individual, determine whether their first catch was as a chick or not
      dplyr::group_by(.data$individualID) %>%
      dplyr::mutate(ringedAs = dplyr::case_when(!is.na(dplyr::first(.data$chickAge)) ~ "chick",
                                                dplyr::first({{Age}}) == "chick" ~ "chick",
                                                TRUE ~ "adult"),
                    firstDate = dplyr::first(.data$captureDate)) %>%
      dplyr::ungroup() %>%
      # Determine exact age and minimum age for adults & chicks
      dplyr::mutate(exactAge = dplyr::case_when(.data$ringedAs == "chick" ~ as.integer(floor(lubridate::interval(.data$firstDate, .data$captureDate) / lubridate::years (1))),
                                                .data$ringedAs == "adult" ~ NA_integer_),
                    minimumAge = dplyr::case_when(.data$ringedAs == "adult" ~ as.integer(floor(lubridate::interval(.data$firstDate, .data$captureDate) / lubridate::years (1)) + 1),
                                                  .data$ringedAs == "chick" ~ .data$exactAge))

  }

  return(output)

}
