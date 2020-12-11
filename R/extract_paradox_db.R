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

  reticulate::use_python(python = "C:\\Users\\Liam\\AppData\\Local\\Programs\\Python\\Python37")

  reticulate::py_run_file(system.file("extdata", "paradox_extract.py", package = "pipelines", mustWork = TRUE))

  output_file <- py$extract_paradox(path = path, file_name = file_name)

  pb <- progress::progress_bar$new(total = length(output_file))

  data_frame_output <- purrr::map_dfr(.x = output_file, .f = function(row){

    pb$print()$tick()

    row <- purrr::map(.x = row, function(.x){

      ifelse(is.null(.x), NA, .x)

    })

    return(row)

  })

  return(data_frame_output)

}
