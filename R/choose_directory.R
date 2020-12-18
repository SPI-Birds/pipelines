#' Choose folder interactively.
#'
#' On Windows, will use 'choose.dir' on Mac will use 'tk_choose.dir'.
#'
#' @return A pathname.
#' @export
#'

choose_directory <- function(caption = 'Select data directory') {
  if (exists('utils::choose.dir')) {
    choose.dir(caption = caption)
  } else {
    tk_choose.dir(caption = caption)
  }
}
