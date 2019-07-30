#' Run multiple data pipelines
#'
#' Run multiple data pipelines. Currently, this produces separate .csv files but
#' will eventually produce combined .csv files for all populations.
#' @param path File path. Location of all folders for data owners. Note, the
#'   folders for each data owner must include the unique code of the data owner
#'   as seen in pop_names.
#' @param PopID The three-letter code of populations to format as listed in the
#'   \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'    protocol}.
#' @param Species The six-letter code of species to include as listed in the
#'   \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'    protocol}. Note, this argument takes precedence over argument PopID (i.e.
#'   if a population doesn't have the requested species it will not be
#'   formatted.)
#'
#' @return Generate .csv files
#' @export

run_pipelines <- function(path = utils::choose.dir(),
                          PopID = NULL,
                          Species = NULL){

  #Force choose.dir()
  force(path)

  #If PopID is NULL use all populations
  if(is.null(PopID)){

    PopID <- pop_names$code

  }

  #Filter pop_names by the specified PopIDs
  #This tells us the data owner of each population
  pop_subset <- dplyr::filter(pop_names, code %in% PopID) %>%
    #For each data owner, create a summary of all the populations
    #that the user wants to extract.
    dplyr::group_by(owner) %>%
    dplyr::summarise(pop = list(c(code)))

  #Find the file path for each of the data owners of interest
  all_dirs <- list.dirs(path = path, full.names = TRUE)
  all_dirs <- all_dirs[grepl(pattern = paste(pop_subset$owner, collapse = "|"), all_dirs)]
  all_dirs <- gsub(pattern = "\\", replacement = "/", x = all_dirs, fixed = TRUE)

  #For each data owner, run the pipeline using the populations requested
  purrr::pwalk(.l = list(dirs = all_dirs, owner = pop_subset$owner,
                         pops = pop_subset$pop),
               .f = function(dirs, owner, pops){

                 eval(parse(text = glue::glue('format_{owner}(db = dirs, pop = pops)')))

               })

  invisible(NULL)

}
