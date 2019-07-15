#' A function to perform a quality check on the pipeline outputs
#'
#' @param db Location of standard .csv outputs of pipeline.
#' @param report Produce report (pdf) for user.
#'
#' @return dataframe with row-by-row warnings and errors,
#'   dataframe with successful/failed checks/tests for use in testthat, user report.
#'
#' @export

quality_check <- function(db = choose.dir(),
                          report = NULL) {

  #start_time <- Sys.time()
  message("Importing data...")

  ## Each pipeline produces 4 .csv outputs in standard format
  ##

}
