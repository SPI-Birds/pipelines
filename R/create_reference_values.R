#' Create biological reference values
#'
#' Species-specific biological reference values to check whether the data in the pipeline outputs have biologically reasonable values.
#'
#' For each species-specific variable a distinction is made between uncommon or unusual values (which result in a "warning") and impossible values (which result in a "potential error"). For each, a minimum and a maximum is provided. Thus, in total, each species-specific variable corresponds to 4 reference values.
#'
#' NB: Manually add & edit reference values in the code and run function to
#' create the necessary files to be used in \code{\link{quality_check}}.
#'
#' @param db File path. Location to save reference values.
#'
#' @return
#' An R data object containing two lists of reference values:
#' \item{brood_ref_values}{Reference values related to brood data.}
#' \item{capture_ref_values}{Reference values related to capture data.}
#'
#' @export

create_reference_values <- function(db = utils::choose.dir()) {

  ## - Brood data
  brood_ref_values <- list(
    ## -- Parus major (Great tit)
    PARMAJ_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 14, 0, 28)),
    PARMAJ_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 14, 0, 28)),
    PARMAJ_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 14, 0, 28)),
    ## -- Cyanistes caeruleus (Blue tit)
    CYACAE_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 14, 0, 28)),
    CYACAE_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 14, 0, 28)),
    CYACAE_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 14, 0, 28)),
    ## -- Ficedula hypoleuca (Pied flycatcher)
    FICHYP_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    FICHYP_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    FICHYP_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Sitta europaea (Eurasian nuthatch)
    SITEUR_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    SITEUR_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    SITEUR_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Periparus ater (Coal tit)
    PERATE_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    PERATE_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    PERATE_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Passer montanus (Eurasian tree sparrow)
    PASMON_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    PASMON_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    PASMON_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Ficedula albicollis (Collared flycatcher)
    FICALB_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    FICALB_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    FICALB_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Poecile palustris (Marsh tit)
    POEPAL_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    POEPAL_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    POEPAL_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20))
  )

  ## - Capture data
  capture_ref_values <- list(
    ## --- Parus major (Great tit)
    PARMAJ_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10.95, 25, 5.5, 50)),
    PARMAJ_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(13.3, 22, 6.5, 44)),
    PARMAJ_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 21.6, 1.5, 40)),
    PARMAJ_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(11.9, 21.3, 5, 40)),
    ## --- Cyanistes caeruleus (Blue tit)
    CYACAE_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(9.5, 20.1, 5, 40)),
    CYACAE_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(10.1, 20.8, 5, 40)),
    CYACAE_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(5, 16.9, 2.5, 30)),
    CYACAE_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(14.6, 26, 7, 52)),
    ## --- Ficedula hypoleuca (Pied flycatcher)
    FICHYP_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    FICHYP_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    FICHYP_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    FICHYP_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    ## --- Sitta europaea (Eurasian nutnatch)
    SITEUR_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    SITEUR_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    SITEUR_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    SITEUR_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    ## --- Periparus ater (Coal tit)
    PERATE_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    PERATE_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    PERATE_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    PERATE_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    ## --- Passer montanus (Eurasian tree sparrow)
    PASMON_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    PASMON_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    PASMON_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    PASMON_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    ## --- Ficedula albicollis (Collared flycatcher)
    FICALB_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    FICALB_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    FICALB_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    FICALB_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    ## --- Poecile palustris (Marsh tit)
    POEPAL_Adult_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    POEPAL_Adult_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25)),
    POEPAL_Chick_Mass <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(10, 20, 5, 25)),
    POEPAL_Chick_Tarsus <- tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(15, 22, 12, 25))
  )

  ## Save as .rda
  save(brood_ref_values, capture_ref_values,
       file = paste0(db, "\\reference_values.rda"))
}
