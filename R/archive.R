#' Archive SPI-Birds raw data
#'
#' @param data_folder Path. Folder containing all SPI-Birds data (N drive)
#' @param PopID Character string. 3-4 letter code for population.
#' Should be the same as the folder name where data are saved.
#' @param new_data File path. Updated data to use as current data in the system.
#' Where a population uses multiple files provide a list of file paths.
#'
#' @return Returns nothing.
#' @export
#'
#' @examples
archive <- function(data_folder = choose_directory(), PopID, new_data) {

  force(db)

  #

  if (missing(PopID)) {

    stop("Provide the PopID that you wish to archive")

  }

  if (missing(new_data)) {

    stop("Provide the new data file(s)")

  }

  #Determine the name for the newest
  date_folder_name <- format(Sys.Date(), format = "%d_%m_%Y")



}
