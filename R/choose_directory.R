#' Choose folder interactively.
#'
#' On Windows, will use 'choose.dir' on Mac will use 'tk_choose.dir'.
#'
#' @return A pathname.
#' @export

choose_directory <- function() {
  if (exists('choose.dir')) {
    utils::choose.dir()
  } else {
    tcltk::tk_choose.dir()
  }
}
