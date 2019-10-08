#' Create biological reference values
#'
#' Species-specific biological reference values to check whether the data in the pipeline outputs have biologically reasonable values.
#'
#' For each species-specific variable a distinction is made between
#' improbable values and impossible values. For each, a minimum
#' and a maximum is provided. Thus, in total, each species-specific variable
#' corresponds to 4 reference values.
#'
#' NB: Manually add & edit reference values in the code and run function to
#' create the necessary files to be used in \code{\link{quality_check}}.
#'
#' \strong{PARMAJ/CYACAE values:} For great tit and blue tit the improbable and impossible values
#' are calculated as the 1%/99% percentiles and min/max for each of the variables taken from
#' data at Hoge Veluwe between the years 2015-2018. We treat chicks as only those listed as Age 1 (i.e. in nest).
#' Adults are anything >1 (i.e. also first year fledglings). This is not ideal and needs to be updated.
#'
#' @param db File path. Location to save reference values.
#'
#' @return
#' An R data object containing three lists of reference values:
#' \item{brood_ref_values_list}{Reference values related to brood data.}
#' \item{cap_adult_ref_values_list}{Reference values related to capture data of adults.}
#' \item{cap_chick_ref_values_list}{Reference values related to capture data of chicks.}
#'
#' @export

create_reference_values <- function(db = utils::choose.dir()) {

  ## - Brood data
  ## -- Parus major (Great tit)
  brood_ref_values_PARMAJ <- tibble::tibble(Species = "PARMAJ",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 12, 0, 14),
                                            BroodSize = c(NA, 10, 0, 14),
                                            NumberFledged = c(NA, 9, 0, 14))

  ## -- Cyanistes caeruleus (Blue tit)
  brood_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 13, 0, 14),
                                            BroodSize = c(NA, 13, 0, 14),
                                            NumberFledged = c(NA, 13, 0, 14))

  ## -- Ficedula hypoleuca (Pied flycatcher)
  brood_ref_values_FICHYP <- tibble::tibble(Species = "FICHYP",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Sitta europaea (Eurasian nuthatch)
  brood_ref_values_SITEUR <- tibble::tibble(Species = "SITEUR",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Periparus ater (Coal tit)
  brood_ref_values_PERATE <- tibble::tibble(Species = "PERATE",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Passer montanus (Eurasian tree sparrow)
  brood_ref_values_PASMON <- tibble::tibble(Species = "PASMON",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Ficedula albicollis (Collared flycatcher)
  brood_ref_values_FICALB <- tibble::tibble(Species = "FICALB",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Poecile palustris (Marsh tit)
  brood_ref_values_POEPAL <- tibble::tibble(Species = "POEPAL",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 10, 0, 15),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## - Capture data
  ## -- Adult
  ## --- Parus major (Great tit)
  cap_adult_ref_values_PARMAJ <- tibble::tibble(Species = "PARMAJ",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(14, 21, 10.95, 25),
                                                Tarsus = c(16, 21, 13.3, 22))

  ## --- Cyanistes caeruleus (Blue tit)
  cap_adult_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(9, 17, 9.5, 20.1),
                                                Tarsus = c(11, 19, 10.1, 10.8))

  ## --- Ficedula hypoleuca (Pied flycatcher)
  cap_adult_ref_values_FICHYP <- tibble::tibble(Species = "FICHYP",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Sitta europaea (Eurasian nutnatch)
  cap_adult_ref_values_SITEUR <- tibble::tibble(Species = "SITEUR",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Periparus ater (Coal tit)
  cap_adult_ref_values_PERATE <- tibble::tibble(Species = "PERATE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Passer montanus (Eurasian tree sparrow)
  cap_adult_ref_values_PASMON <- tibble::tibble(Species = "PASMON",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))


  ## --- Ficedula albicollis (Collared flycatcher)
  cap_adult_ref_values_FICALB <- tibble::tibble(Species = "FICALB",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Poecile palustris (Marsh tit)
  cap_adult_ref_values_POEPAL <- tibble::tibble(Species = "POEPAL",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## -- Chick
  ## --- Parus major (Great tit)
  cap_chick_ref_values_PARMAJ <- tibble::tibble(Species = "PARMAJ",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(7, 20, 3.5, 21.6),
                                                Tarsus = c(16, 21, 11.9, 21.3))

  ## --- Cyanistes caeruleus (Blue tit)
  cap_chick_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(6, 13, 5, 16.9),
                                                Tarsus = c(14.6, 20, 14.6, 26))

  ## --- Ficedula hypoleuca (Pied flycatcher)
  cap_chick_ref_values_FICHYP <- tibble::tibble(Species = "FICHYP",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Sitta europaea (Eurasian nutnatch)
  cap_chick_ref_values_SITEUR <- tibble::tibble(Species = "SITEUR",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Periparus ater (Coal tit)
  cap_chick_ref_values_PERATE <- tibble::tibble(Species = "PERATE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Passer montanus (Eurasian tree sparrow)
  cap_chick_ref_values_PASMON <- tibble::tibble(Species = "PASMON",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))


  ## --- Ficedula albicollis (Collared flycatcher)
  cap_chick_ref_values_FICALB <- tibble::tibble(Species = "FICALB",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Poecile palustris (Marsh tit)
  cap_chick_ref_values_POEPAL <- tibble::tibble(Species = "POEPAL",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## Create list of each variable within species
  listV <- function(x) {
    l <- list()
    for(i in 3:ncol(x)) {
      l[[i]] <- x[, c(1, 2, i)]
    }
    l <- purrr::compact(l)
    names(l) <- names(x)[3:ncol(x)]
    return(l)
  }

  ## - Brood data
  brood_ref_values_list <- purrr::map(mget(setdiff(ls(pattern="^brood_ref"), ls(pattern="list$"))),
                                      function(x) listV(x))
  names(brood_ref_values_list) <- stringr::str_extract(names(brood_ref_values_list), "[^\\_]+$")

  ## - Capture data
  ## -- Adult
  cap_adult_ref_values_list <- purrr::map(mget(setdiff(ls(pattern="^cap_adult_ref"), ls(pattern="list$"))),
                                          function(x) listV(x))
  names(cap_adult_ref_values_list) <- stringr::str_extract(names(cap_adult_ref_values_list), "[^\\_]+$")

  ## -- Chick
  cap_chick_ref_values_list <- purrr::map(mget(setdiff(ls(pattern="^cap_chick_ref"), ls(pattern="list$"))),
                                          function(x) listV(x))
  names(cap_chick_ref_values_list) <- stringr::str_extract(names(cap_chick_ref_values_list), "[^\\_]+$")



  ## Save as .rda
  save(brood_ref_values_list, cap_adult_ref_values_list, cap_chick_ref_values_list,
       file = paste0(db, "\\reference_values.rda"))
}
