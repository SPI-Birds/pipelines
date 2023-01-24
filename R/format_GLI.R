#' Construct standard format for data from Glimmen, the Netherlands
#'
#' A pipeline to produce the standard format for the study site at
#' Glimmen, the Netherlands, administred by Simon Verhulst (University of Groningen).
#'
#' This section provides details on data management choices that are unique to these data.
#'
#' This pipeline is built using SPI-Birds' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard format v2.0.0}.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format (v2.0.0).
#'
#' @export
#'

format_GLI <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)) {

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the developer & user
  start_time <- Sys.time()

}
