#' Create biological reference values
#'
#' Manually add & edit reference values in the code and run function to
#' create the necessary files to be used in \code{\link{quality_check}}.
#'
#' For each species-specific variable a distinction is made between
#' improbable values and impossible values. For each, provide a minimum
#' and a maximum. Thus, in total, each species-specific variable
#' corresponds to 4 reference values.
#'
#' @param db Directory to save reference values.
#'
#' @return An R data object (reference_values.rda)
#'   with three lists of reference values:
#'   brood data, adult capture data and chick capture data.
#'
#' @export

create_reference_values <- function(db = choose.dir()) {

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

  # --- Poecile palustris (Marsh tit)
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

  # --- Poecile palustris (Marsh tit)
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

  list_brood_ref_values_PARMAJ <- listV(brood_ref_values_PARMAJ)
  list_brood_ref_values_CYACAE <- listV(brood_ref_values_CYACAE)
  list_brood_ref_values_FICHYP <- listV(brood_ref_values_FICHYP)
  list_brood_ref_values_SITEUR <- listV(brood_ref_values_SITEUR)
  list_brood_ref_values_PERATE <- listV(brood_ref_values_PERATE)
  list_brood_ref_values_PASMON <- listV(brood_ref_values_PASMON)
  list_brood_ref_values_FICALB <- listV(brood_ref_values_FICALB)
  list_brood_ref_values_POEPAL <- listV(brood_ref_values_POEPAL)

  list_cap_adult_ref_values_PARMAJ <- listV(cap_adult_ref_values_PARMAJ)
  list_cap_adult_ref_values_CYACAE <- listV(cap_adult_ref_values_CYACAE)
  list_cap_adult_ref_values_FICHYP <- listV(cap_adult_ref_values_FICHYP)
  list_cap_adult_ref_values_SITEUR <- listV(cap_adult_ref_values_SITEUR)
  list_cap_adult_ref_values_PERATE <- listV(cap_adult_ref_values_PERATE)
  list_cap_adult_ref_values_PASMON <- listV(cap_adult_ref_values_PASMON)
  list_cap_adult_ref_values_FICALB <- listV(cap_adult_ref_values_FICALB)
  list_cap_adult_ref_values_POEPAL <- listV(cap_adult_ref_values_POEPAL)

  list_cap_chick_ref_values_PARMAJ <- listV(cap_chick_ref_values_PARMAJ)
  list_cap_chick_ref_values_CYACAE <- listV(cap_chick_ref_values_CYACAE)
  list_cap_chick_ref_values_FICHYP <- listV(cap_chick_ref_values_FICHYP)
  list_cap_chick_ref_values_SITEUR <- listV(cap_chick_ref_values_SITEUR)
  list_cap_chick_ref_values_PERATE <- listV(cap_chick_ref_values_PERATE)
  list_cap_chick_ref_values_PASMON <- listV(cap_chick_ref_values_PASMON)
  list_cap_chick_ref_values_FICALB <- listV(cap_chick_ref_values_FICALB)
  list_cap_chick_ref_values_POEPAL <- listV(cap_chick_ref_values_POEPAL)


  ## Create named list
  listN <- function(...) {
    dots <- list(...)
    names(dots) <- gsub(".*\\_", "", substitute(list(...))[-1])
    return(dots)
  }

  brood_ref_values_list <- listN(list_brood_ref_values_PARMAJ,
                                 list_brood_ref_values_CYACAE,
                                 list_brood_ref_values_FICHYP,
                                 list_brood_ref_values_SITEUR,
                                 list_brood_ref_values_PERATE,
                                 list_brood_ref_values_PASMON,
                                 list_brood_ref_values_FICALB,
                                 list_brood_ref_values_POEPAL)

  cap_adult_ref_values_list <- listN(list_cap_adult_ref_values_PARMAJ,
                                     list_cap_adult_ref_values_CYACAE,
                                     list_cap_adult_ref_values_FICHYP,
                                     list_cap_adult_ref_values_SITEUR,
                                     list_cap_adult_ref_values_PERATE,
                                     list_cap_adult_ref_values_PASMON,
                                     list_cap_adult_ref_values_FICALB,
                                     list_cap_adult_ref_values_POEPAL)

  cap_chick_ref_values_list <- listN(list_cap_chick_ref_values_PARMAJ,
                                     list_cap_chick_ref_values_CYACAE,
                                     list_cap_chick_ref_values_FICHYP,
                                     list_cap_chick_ref_values_SITEUR,
                                     list_cap_chick_ref_values_PERATE,
                                     list_cap_chick_ref_values_PASMON,
                                     list_cap_chick_ref_values_FICALB,
                                     list_cap_chick_ref_values_POEPAL)

  ## Save as .rda in /data
  save(brood_ref_values_list, cap_adult_ref_values_list, cap_chick_ref_values_list,
       file = paste0(db, "\\reference_values.rda"))
}
