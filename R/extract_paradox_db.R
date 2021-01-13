#' Convert paradox data file to data frame.
#'
#' Converts .db paradox data file into R data frame.
#' @param path Directory where paradox files are located.
#' @param file_name File name of paradox file (.db) that will be converted.
#'
#' @return A data frame
#' @export
#' @import reticulate

extract_paradox_db <- function(path, file_name){

  #Determine operating system
  OS <- tolower(utils::sessionInfo()$running)

  #Set python version if operating system is windows
  #(this should not be necessary to set here when running on MacOS, assuming setup is otherwise fine)

  reticulate::py_run_file(system.file("extdata", "paradox_extract.py", package = "pipelines", mustWork = TRUE))

  #Write full pathname
  # When running on Mac OS, the path in extract_paradox_db may not contain "~"
  path_full <- normalizePath(path)

  output_file <- py$extract_paradox(path = path_full, file_name = file_name)

  pb <- progress::progress_bar$new(total = length(output_file))

  data_frame_output <- purrr::map_dfr(.x = output_file, .f = function(row){

    pb$tick()

    row <- purrr::map(.x = row, function(.x){

      ifelse(is.null(.x), NA, .x)

    })

    return(row)

  })

  return(data_frame_output)

}
