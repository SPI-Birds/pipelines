#' Create biological reference values
#'
#' Species-specific biological reference values to check whether the data in the pipeline outputs have biologically reasonable values.
#'
#' For each species-specific variable a distinction is made between uncommon or unusual values (which result in a "warning") and impossible values (which result in a "potential error"). For each, a minimum and a maximum is provided. Thus, in total, each species-specific variable corresponds to 4 reference values.
#'
#' If many records of data are available for a species, reference values are based on the data. Otherwise, reference values need to be manually added & edited.
#'
#' @param NIOO_data Data in standard format for NIOO populations
#'
#' @return
#' An R data object containing two lists of reference values:
#' \item{brood_ref_values}{Reference values related to brood data.}
#' \item{capture_ref_values}{Reference values related to capture data.}
#'
#' @export

##FIXME: remove this function if check_values_capture is updated.

create_reference_values <- function(NIOO_data) {

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
                                          Value = c(3, 23, 0, 46)),
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
                                         Value = c(3, 23, 0, 46))
  )

  ## Save as lists in /data folder
  usethis::use_data(capture_ref_values, overwrite=TRUE)
}
