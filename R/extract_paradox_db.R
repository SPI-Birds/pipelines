#' Title
#'
#' @param db
#'
#' @return
#' @export
#' @import reticulate
#'
#' @examples
extract_paradox_db <- function(db){

  reticulate::use_python(python = "C:\\Users\\Liam\\AppData\\Local\\Programs\\Python\\Python37")

  py_run_file(system.file("extdata", "paradox_extract.py", package = "HNBStandFormat", mustWork = TRUE))

  data_frame_output <- purrr::map_dfr(.x = py$output_list, .f = function(row){

    row <- purrr::map(.x = row, function(.x){

      ifelse(is.null(.x), NA, .x)

    })

    return(row)

  })

  return(data_frame_output)

}
