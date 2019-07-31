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

  #Assign species for filtering
  if(is.null(Species)){

    Species <- Species_codes$Code

  }

  #Firstly, check if there are any cases where a requested population does not have any info on a given species
  missing_species <- pop_species_combos %>%
    dplyr::filter(pop %in% PopID) %>%
    dplyr::group_by(pop) %>%
    dplyr::summarise(total_sp = sum(species %in% Species))

  #If there are any with missing data, give a warning message
  if(any(missing_species$total_sp == 0)){

    missing_species %>%
      dplyr::filter(total_sp == 0) %>%
      purrr::pwalk(.l = list(.$pop),
                   .f = ~{

                     message(glue::glue('Population {..1} has no information on the focal species and has been excluded.'))

                   })

  }

  #Now just work with those populations where the population and species are present
  pop_species_subset <- pop_species_combos %>%
    dplyr::filter(pop %in% PopID & species %in% Species) %>%
    dplyr::left_join(dplyr::select(pop_names, code, owner), by = c("pop" = "code")) %>%
    dplyr::group_by(owner, pop) %>%
    dplyr::summarise(species = list(c(species))) %>%
    dplyr::summarise(species = first(species),
                     pops = list(c(pop)))

  #Find the file path for each of the data owners of interest
  all_dirs <- list.dirs(path = path, full.names = TRUE)
  all_dirs <- all_dirs[grepl(pattern = paste(pop_species_subset$owner, collapse = "|"), all_dirs)]
  all_dirs <- gsub(pattern = "\\", replacement = "/", x = all_dirs, fixed = TRUE)

  #For each data owner, run the pipeline using the populations requested
  purrr::pwalk(.l = list(dirs = all_dirs, owner = pop_species_subset$owner,
                         pops = pop_species_subset$pops,
                         species = pop_species_subset$species),
               .f = function(dirs, owner, pops, species){

                 eval(parse(text = glue::glue('format_{owner}(db = dirs, pop = pops, species = c(\"{species}\"))')))

               })

  invisible(NULL)

}
