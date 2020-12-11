#' Choose folder interactively.
#'
#' On Windows, will use 'choose.dir' on Mac will use 'tk_choose.dir'.
#'
#' @return A pathname.
#' @export
#'

choose_directory <- function() {
  #As long as users have RstudioAPI available, use the selectDirectory function
  if(rstudioapi::hasFun("selectDirectory")){

    return(rstudioapi::selectDirectory())

  #Otherwise, check if choose.dir is available (i.e. Windows) and use this instead
  } else if(rstudioapi::hasFun("choose.dir")) {

    utils::choose.dir(caption = 'Select data directory')

  } else {

    stop("Please run this pipeline in RStudio or manually specify the directory.")

  }

}
