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
                                          Value = c(NA, 15, 0, 20)),
    ## -- Poecile cinctus (Siberian tit)
    POECIN_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    POECIN_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    POECIN_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20)),
    ## -- Phoenicurus phoenicurus (Common redstart)
    PHOPHO_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, 10, 0, 15)),
    PHOPHO_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, 15, 0, 20)),
    PHOPHO_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, 15, 0, 20))
  )

  ##For PARMAJ and CYACAE chicks, we include age specific capture data for chicks
  #based on chick growth curves calculated from Hoge Veluwe
  #See calculate_chick_mass_cutoffs.R for details.

  ## PARMAJ, CYACAE and FICHYP cutoffs for Adult_Mass, Adult_Tarsus and Chick_Tarsus are based on cutoffs from NIOO.

  ## - Capture data
  capture_ref_values <- list(
    ## --- Parus major (Great tit)
    PARMAJ_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2.5, 25, 0, 50)),
    PARMAJ_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(4, 30, 0, 60)),
    PARMAJ_Chick_Mass  = chick_mass_cutoffs %>%
      dplyr::filter(Species == "PARMAJ") %>%
      dplyr::select(Reference, Value, ChickAge),
    PARMAJ_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3.5, 30, 0, 60)),
    ## --- Cyanistes caeruleus (Blue tit)
    CYACAE_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2.5, 25, 0, 50)),
    CYACAE_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(4, 27, 0, 54)),
    CYACAE_Chick_Mass = chick_mass_cutoffs %>%
      dplyr::filter(Species == "CYACAE") %>%
      dplyr::select(Reference, Value, ChickAge),
    CYACAE_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 27, 0, 54)),
    ## --- Ficedula hypoleuca (Pied flycatcher)
    FICHYP_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    FICHYP_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    FICHYP_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    FICHYP_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 23, 0, 46)),
    ## --- Sitta europaea (Eurasian nutnatch)
    SITEUR_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    SITEUR_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    SITEUR_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    SITEUR_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 23, 0, 46)),
    ## --- Periparus ater (Coal tit)
    PERATE_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    PERATE_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    PERATE_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    PERATE_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = cc(3, 23, 0, 46)),
    ## --- Passer montanus (Eurasian tree sparrow)
    PASMON_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    PASMON_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    PASMON_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    PASMON_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 23, 0, 46)),
    ## --- Ficedula albicollis (Collared flycatcher)
    FICALB_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    FICALB_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    FICALB_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    FICALB_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 23, 0, 46)),
    ## --- Poecile palustris (Marsh tit)
    POEPAL_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(3.5, 25, 0, 50)),
    POEPAL_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(5, 23, 0, 46)),
    POEPAL_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                        Value = c(2, 25, 0, 50)),
    POEPAL_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(3, 23, 0, 46)),
    ## --- Poecile cinctus (Siberian tit)
    POECIN_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(3.5, 25, 0, 50)),
    POECIN_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                         Value = c(5, 23, 0, 46)),
    POECIN_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(2, 25, 0, 50)),
    POECIN_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                         Value = c(3, 23, 0, 46)),
    ## --- Phoenicurus phoenicurus (Common redstart)
    PHOPHO_Adult_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(3.5, 25, 0, 50)),
    PHOPHO_Adult_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                         Value = c(5, 23, 0, 46)),
    PHOPHO_Chick_Mass = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(2, 25, 0, 50)),
    PHOPHO_Chick_Tarsus = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                         Value = c(3, 23, 0, 46)),
  )

  ## Save as .rda
  save(brood_ref_values, capture_ref_values,
       file = paste0(db, "\\reference_values.rda"))
}
