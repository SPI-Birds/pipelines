#' Choose folder interactively.
#'
#' On Windows, will use 'choose.dir' on Mac will use 'tk_choose.dir'.
#'
#' @return A pathname.
#' @export

choose_directory <- function() {

  # choose.dir() results in NA in Rstudio, since R 4.3.0
  # use tcltk::tk_choose.dir() for all OS for the time being

  tcltk::tk_choose.dir()

  # if (exists('choose.dir')) {
  #
  #   utils::choose.dir()
  #
  # } else {
  #
  #   tcltk::tk_choose.dir()
  #
  # }

}
