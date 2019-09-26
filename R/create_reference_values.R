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
                                            ClutchSize = c(NA, 15, 0, 20),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

  ## -- Cyanistes caeruleus (Blue tit)
  brood_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                            Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                            ClutchSize = c(NA, 15, 0, 20),
                                            BroodSize = c(NA, 15, 0, 20),
                                            NumberFledged = c(NA, 15, 0, 20))

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
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Cyanistes caeruleus (Blue tit)
  cap_adult_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

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
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

  ## --- Cyanistes caeruleus (Blue tit)
  cap_chick_ref_values_CYACAE <- tibble::tibble(Species = "CYACAE",
                                                Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                                Mass = c(10, 20, 5, 25),
                                                Tarsus = c(15, 22, 12, 25))

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
