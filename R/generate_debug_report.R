#' Generate a debug report for a pipeline
#'
#' Generates a debug report which include histograms of all continuous variables
#' and unique values for (some) categorical variable.
#' @param path File path where debug report will be created.
#' @param Pop Name of the population being debugged.
#' @param Brood_data A table containing the brood data.
#' @param Capture_data A table containing the capture data.
#' @param Indv_data A table containing the individual data.
#'
#' @return Creates a directory 'debug_report' which contains .csv and .pdf files
#'   with debug information.
#' @export

generate_debug_report <- function(path, Pop, Brood_data, Capture_data, Indv_data){

  #Generate the debug folder
  if (!dir.exists(paste0(path, "\\debug_report"))) {
    dir.create(paste0(path, "\\debug_report"), recursive = TRUE)
  }

  ### DEBUG BROOD DATA ###

  Brood_data_summary <- Brood_data %>%
    select(Species, ClutchType_observed, ClutchType_calculated) %>%
    summarise_all(~paste(stats::na.omit(unique(.x)), collapse = "/"))

  #Turn all dates into April days
  Brood_data <- Brood_data %>%
    mutate_if(.predicate = ~class(.x) == "Date", .funs = function(.x){

      tryCatch(expr = as.numeric(.x - lubridate::ymd(paste(lubridate::year(.x), "01", "04"))),
               warning = function(...) return(NA))

    })

  #Create a list of all histograms for continuous variables
  Brood_plots <- purrr::map(.x = c("LayingDate", "ClutchSize", "HatchDate", "BroodSize", "FledgeDate", "NumberFledged", "AvgEggMass", "AvgChickMass", "AvgTarsus"),
                             .f = ~plot_debug_hist(table = Brood_data, variable = .x))

  utils::write.csv(Brood_data_summary, file = paste0(path, "\\debug_report\\Brood_summary", Pop, ".csv"))

  grDevices::pdf(file = paste0(path, "\\debug_report\\Brood_summary.pdf"), width = 7, height = 6)
  purrr::map(.x = Brood_plots, .f = ~{if(!is.null(.x)) print(.x)})
  grDevices::dev.off()

  ### DEBUG CAPTURE DATA ###

  Capture_data_summary <- Capture_data %>%
    select(Species, CaptureTime) %>%
    summarise_all(~paste(unique(.x), collapse = "/"))

  #Turn all dates into April days
  Capture_data <- Capture_data %>%
    mutate_if(.predicate = ~class(.x) == "Date", .funs = function(.x){

      as.numeric(.x - lubridate::ymd(paste(lubridate::year(.x), "01", "04")))

    }) %>%
    #Column PopID needed for histograms to work
    dplyr::rename(PopID = CapturePopID))

  #Create a list of all histograms for continuous variables
  Capture_plots <- purrr::map(.x = c("Mass", "Tarsus", "WingLength", "Age_obsv", "Age_calc", "ChickAge"),
                            .f = ~plot_debug_hist(table = Capture_data, variable = .x))

  utils::write.csv(Capture_data_summary, file = paste0(path, "\\debug_report\\Capture_summary", Pop, ".csv"))

  grDevices::pdf(file = paste0(path, "\\debug_report\\Capture_summary.pdf"), width = 7, height = 6)
  purrr::map(.x = Capture_plots, .f = ~{if(!is.null(.x)) print(.x)})
  grDevices::dev.off()

  ### DEBUG INDIVIDUAL DATA ###

  Indv_data_summary <- Indv_data %>%
    select(Species, Sex) %>%
    summarise_all(~paste(unique(.x), collapse = "/"))

  #Turn all dates into April days
  Indv_data <- Indv_data %>%
    ungroup() %>%
    mutate_if(.predicate = ~class(.x) == "Date", .funs = function(.x){

      as.numeric(.x - lubridate::ymd(paste(lubridate::year(.x), "01", "04")))

    })

  #Create a list of all histograms for continuous variables
  Indv_plots <- purrr::map(.x = c("RingSeason", "RingAge"),
                            .f = ~plot_debug_hist(table = Indv_data, variable = .x))

  utils::write.csv(Indv_data_summary, file = paste0(path, "\\debug_report\\Indv_summary", Pop, ".csv"))

  grDevices::pdf(file = paste0(path, "\\debug_report\\Indv_summary.pdf"), width = 7, height = 6)
  purrr::map(.x = Indv_plots, .f = ~{if(!is.null(.x)) print(.x)})
  grDevices::dev.off()

}
