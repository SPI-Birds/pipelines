#' Run multiple pipelines
#'
#' Run multiple pipelines as specified by the user. If requested, pipeline
#' outputs are combined.
#'
#' @param combined Logical. If TRUE, all pipeline outputs are appended to create
#'   four .csv files. If FALSE, individual .csv files are made for each
#'   population.
#' @param torun_path File path. Data frame with two columns: ID, identifier for each
#'   pipeline to run. N.B. These are NOT the same as PopID because in some cases
#'   (e.g. NIOO), one pipeline is used for many PopIDs. Instead, this should be
#'   the letter code at the end of the format function (e.g. NIOO, VEL). path,
#'   directory where raw data is stored. If left blank, `choose.dir()` will be
#'   called to identify the directory. If the data frame is stored as a csv,
#'   `torun_path` must be a file path. If the data frame is stored in the global
#'   environment, no value is needed for `torun_path`.
#' @return Generate 4 .csv files.
#' @export
create_multiple <- function(combined = FALSE, torun_path = NULL){

  if(!exists("torun")){

    if(is.null(torun_path)){

      stop("Please provide a file location for the torun file...")

    } else {

      torun <- read.csv(torun_path, header = T, sep = ",")

    }

  }

  message("Running all formatting code...")

  purrr::walk(.x = list(torun),
              .f = ~{

                PopID <- ..1$PopID
                path  <- ..1$path

                ## If not path is provided, force the user to choose
                if(is.na(path)){

                  message(glue::glue('Choose a location for {PopID} files...',
                                     PopID = PopID))

                  path <- eval(choose.dir())

                ## If they have provided a path, make sure it meets Unix syntax
                ## (i.e. no \)
                } else {

                  path <- gsub(pattern = "\\\\",
                               replacement = "/",
                               x = path)

                }

                eval(parse(text = glue::glue("format_{PopID}(db = \"{path}\")",
                                             PopID = PopID,
                                             path = path)))

                })

}
