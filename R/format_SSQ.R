#' Construct standard summary for data from Santo Stefano Quisquina, Italy.
#'
#' A pipeline to produce a standard output for the great and blue tit population
#' in Santo Stefano Quisquina, Sicly, Italy, administered by Camillo Cusimano
#' and Daniel Campobello.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#' @param debug For internal use when editing pipelines. If TRUE, pipeline
#'   generates a summary of pipeline data. This
#'   includes: a) Histogram of continuous variables with mean/SD b) unique
#'   values of all categorical variables.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2

format_SSQ <- function(db = NULL,
                          Species = NULL,
                          path = ".",
                          debug = FALSE){

  #Find database path
  if(is.null(db)){

    message("Please select a database location...")

    db <- file.choose()

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- read_excel(db) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel")

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- all_data %>%
    left_join(filter(Species_codes, SpeciesID %in% c("14640", "14620")) %>%
                mutate(Species = c("Parus major", "Cyanistes caeruleus")) %>%
                select(Species, Code), by = "Species") %>%
    mutate(PopID = "SIC",
           BroodID = paste(Year, NestId, sep = "_")) %>%
    left_join(tibble::tibble(ClutchType_observed = c("first", "second", "replacement"),
                             Class = c(1, 3, 2)), by = "Class")


}
