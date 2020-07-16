#' Create biological reference values
#'
#' Species-specific biological reference values to check whether the data in the pipeline outputs have biologically reasonable values.
#'
#' For each species-specific variable a distinction is made between uncommon or unusual values (which result in a "warning") and impossible values (which result in a "potential error"). For each, a minimum and a maximum is provided. Thus, in total, each species-specific variable corresponds to 4 reference values.
#'
#' If many records of data are available for a species, reference values are based on the data. Otherwise, reference values need to be manually added & edited.
#'
#' Currently ClutchSize, BroodSize and NumberFledged for great tit, blue tit and pied flycatcher are based on data from NIOO populations. Warning_max is set at the 99.9% quantile. Error_max is set at 2 * Warning_max. Example: when 99.9% quantile for great tit ClutchSize is 15, Warning_max = 15 and Error_max = 30. See
#'
#' @param NIOO_Brood_data Data frame with brood data from NIOO populations for great tit, blue tit and pied flycatcher
#'
#' @return
#' An R data object containing two lists of reference values:
#' \item{brood_ref_values}{Reference values related to brood data.}
#' \item{capture_ref_values}{Reference values related to capture data.}
#'
#' @export

create_reference_values <- function(NIOO_Brood_data) {

  Warning_max <- NIOO_Brood_data %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(ClutchSize = round(quantile(ClutchSize, probs=0.999, na.rm=TRUE)),
                     BroodSize = round(quantile(BroodSize, probs=0.999, na.rm=TRUE)),
                     NumberFledged = round(quantile(NumberFledged, probs=0.999, na.rm=TRUE)))

  ## - Brood data
  brood_ref_values <- list(
    ## -- Parus major (Great tit)
    PARMAJ_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, Warning_max[Warning_max$Species == "PARMAJ",]$ClutchSize, 0, 2 * Warning_max[Warning_max$Species == "PARMAJ",]$ClutchSize)),
    PARMAJ_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, Warning_max[Warning_max$Species == "PARMAJ",]$BroodSize, 0, 2 * Warning_max[Warning_max$Species == "PARMAJ",]$BroodSize)),
    PARMAJ_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, Warning_max[Warning_max$Species == "PARMAJ",]$NumberFledged, 0, 2 * Warning_max[Warning_max$Species == "PARMAJ",]$NumberFledged)),
    ## -- Cyanistes caeruleus (Blue tit)
    CYACAE_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, Warning_max[Warning_max$Species == "CYACAE",]$ClutchSize, 0, 2 * Warning_max[Warning_max$Species == "CYACAE",]$ClutchSize)),
    CYACAE_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, Warning_max[Warning_max$Species == "CYACAE",]$BroodSize, 0, 2 * Warning_max[Warning_max$Species == "CYACAE",]$BroodSize)),
    CYACAE_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, Warning_max[Warning_max$Species == "CYACAE",]$NumberFledged, 0, 2* Warning_max[Warning_max$Species == "CYACAE",]$NumberFledged)),
    ## -- Ficedula hypoleuca (Pied flycatcher)
    FICHYP_ClutchSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                       Value = c(NA, Warning_max[Warning_max$Species == "FICHYP",]$ClutchSize, 0, 2 * Warning_max[Warning_max$Species == "FICHYP",]$ClutchSize)),
    FICHYP_BroodSize = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                      Value = c(NA, Warning_max[Warning_max$Species == "FICHYP",]$BroodSize, 0, 2 * Warning_max[Warning_max$Species == "FICHYP",]$BroodSize)),
    FICHYP_NumberFledged = tibble::tibble(Reference = c("Warning_min", "Warning_max", "Error_min", "Error_max"),
                                          Value = c(NA, Warning_max[Warning_max$Species == "FICHYP",]$NumberFledged, 0, 2 * Warning_max[Warning_max$Species == "FICHYP",]$NumberFledged)),
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
  usethis::use_data(brood_ref_values, overwrite=TRUE)
  usethis::use_data(capture_ref_values, overwrite=TRUE)
}


#' Visualize the brood refence values that are based on data
#'
#' Part of the reference values in \code{\link{create_refence_values}} are based on data. This function helps in making the creation of those reference values visible and transparent.
#'
#' Currently ClutchSize, BroodSize and NumberFledged for great tit, blue tit and pied flycatcher are based on data from NIOO populations. Warning_max is set at the 99.9% quantile.
#'
#' @param NIOO_Brood_data Data frame with brood data from NIOO populations for great tit, blue tit and pied flycatcher
#' @param var Brood_data variable to plot. Options: ClutchSize, BroodSize, NumberFledged.
#'
#' @return A plot with species-specific histograms of the selected variable, including a mark of the 99.9% quantile.
#'
#' @export

plot_ref_values <- function(NIOO_Brood_data, var){

  p <- purrr::map(.x = unique(NIOO_Brood_data$Species),
                  .f =  ~{

                    wm <- NIOO_Brood_data %>%
                      dplyr::filter(!is.na(!!dplyr::sym(var)), Species == .x) %>%
                      dplyr::summarise(round(stats::quantile(!!dplyr::sym(var),
                                                probs=0.999))) %>%
                                  dplyr::pull()

                    p <- NIOO_Brood_data %>%
                      dplyr::filter(!is.na(!!dplyr::sym(var)), Species == .x) %>%
                      ggplot2::ggplot(ggplot2::aes(x=!!dplyr::sym(var))) +
                      ggplot2::geom_histogram(ggplot2::aes(y=..density..), binwidth = 1,
                                              color="black", fill="#656565") +
                      ggplot2::geom_vline(ggplot2::aes(xintercept = wm),
                                          color="#920309") +
                      ggplot2::scale_x_continuous(breaks=seq(0, 30, 2)) +
                      ggplot2::labs(title = paste0(.x, ": ",
                                                  Species_codes[Species_codes$Code == .x,
                                                                "CommonName"]),
                                    subtitle = paste0("Warning_max (99.9% quantile) = ",
                                                      wm),
                                    caption = "Data source: NIOO",
                                    y = "Density") +
                      ggplot2::theme_classic() +
                      ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 13, face = "bold"),
                        plot.subtitle = ggplot2::element_text(color = "#920309", size=10)
                      )

                    if("Dubai" %in% extrafont::fonts()){
                      p + ggplot2::theme(text = ggplot2::element_text(family = "Dubai"))
                    } else {
                      p + ggplot2::theme(text = ggplot2::element_text(family = "serif"))
                    }

                  })

  cowplot::plot_grid(plotlist = p, align="v")

}
