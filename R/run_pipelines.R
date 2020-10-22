#' Run multiple data pipelines
#'
#' Run multiple data pipelines. Currently, this produces separate .csv files but
#' will eventually produce combined .csv files for all populations.
#'
#' @param path File path. Location of all folders for data owners. Note, the
#'   folders for each data owner must include the unique code of the data owner
#'   as seen in pop_codes.
#' @param PopID The three-letter code of populations to format as listed in the
#'   \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'    protocol}.
#' @param Species The six-letter code of species to include as listed in the
#'   \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'    protocol}. Note, this argument takes precedence over argument PopID (i.e.
#'   if a population doesn't have the requested species it will not be
#'   formatted.)
#' @param output_type Should the pipeline generate .csv files ('csv') or R objects ('R'). Default: R.
#' @param save TRUE/FALSE. Should the output be saved locally? This is only relevant where
#' `output_type` is 'R'. If output_type is 'csv'
#' 4 .csv files will be created in the save path (specified by `save_path` argument). If output_type is 'R'
#' and `save` is TRUE, an .RDS file will be created in the save path.
#' @param save_path Path where files will be saved if `save` is TRUE. By default, the save
#' path will be path/standard_format.
#' @param filename The filename of the saved file. No file extension is
#' needed, as this will differ depending on `output_type`. By default, filename is
#' "standard_format".
#'
#' @return Generate .csv files or return an R list object with 4 items
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Create an R list of Harjavalta data
#' HAR_data <- run_pipelines(PopID = "HAR", Species = "PARMAJ", output_type = "R")
#'
#' }

run_pipelines <- function(path = choose_directory(),
                          PopID = NULL,
                          Species = NULL,
                          output_type = "R",
                          save = TRUE, save_path = NULL,
                          filename = "standard_format"){

  #Force choose_directory()
  force(path)

  if((output_type == "csv" | (output_type == "R" & save)) & is.null(save_path)){

    save_path <- paste(path, "standard_format", sep = "/")

    if(!any(grepl(pattern = "standard_format", x = list.dirs(path)))){

      dir.create(save_path)

    }

  }

  #If PopID is NULL use all populations
  if(is.null(PopID)){

    PopID <- pop_codes$PopID

  }

  #Assign species for filtering
  if(is.null(Species)){

    Species <- species_codes$Species

  } else if(all(!Species %in% species_codes$Species)){

    stop("Species provided are not included in the pipelines. Please select from species listed in species_codes")

  }

  #Firstly, check if there are any cases where a requested population does not have any info on a given species
  missing_species <- pop_species_combos %>%
    dplyr::filter(PopCode %in% PopID) %>%
    dplyr::group_by(PopCode) %>%
    dplyr::summarise(total_sp = sum(SpeciesCode %in% Species))

  #If there are any with missing data, give a warning message
  if(any(missing_species$total_sp == 0)){

    missing_species %>%
      dplyr::filter(total_sp == 0) %>%
      purrr::pwalk(.l = list(.$PopCode),
                   .f = ~{

                     message(glue::glue('Population {..1} has no information on the focal species and has been excluded.'))

                   })

  }

  #Now just work with those populations where the population and species are present
  pop_species_subset <- pop_species_combos %>%
    dplyr::filter(PopCode %in% PopID & SpeciesCode %in% Species) %>%
    dplyr::left_join(dplyr::select(pop_codes, PopID, Owner), by = c("PopCode" = "PopID")) %>%
    dplyr::group_by(Owner) %>%
    dplyr::summarise(Species = list(c(unique(SpeciesCode))),
                     Pops = list(c(unique(PopCode))))

  #Find the file path for each of the data owners of interest
  all_dirs <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)
  all_dirs <- all_dirs[grepl(pattern = paste(pop_species_subset$Owner, collapse = "|"), all_dirs)]
  all_dirs <- gsub(pattern = "\\", replacement = "/", x = all_dirs, fixed = TRUE)

  #For each data owner, run the pipeline using the populations requested
  #Return R lists rather than generating .csv files
  R_objects <- purrr::pmap(.l = list(dirs = all_dirs, owner = pop_species_subset$Owner,
                                     pops = pop_species_subset$Pops,
                                     species = pop_species_subset$Species),
                           .f = function(dirs, owner, pops, species){

                           eval(parse(text = glue::glue('format_{owner}(db = dirs, pop = pops, species = species,
                                                output_type = "R")')))

                             })

  #For each of the four tables, go through and combine the outputs
  #Add row numbers for each
  Brood_data <- purrr::map_df(.x = R_objects,
                                   .f = ~{

                                     .x$Brood_data

                                   }) %>%
    dplyr::mutate(Row = seq(1, n())) %>%
    dplyr::select(Row, everything())

  Capture_data <- purrr::map_df(.x = R_objects,
                                     .f = ~{

                                       .x$Capture_data

                                     }) %>%
    dplyr::mutate(Row = seq(1, n())) %>%
    dplyr::select(Row, everything())

  Individual_data <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Individual_data

                                        }) %>%
    dplyr::mutate(Row = seq(1, n())) %>%
    dplyr::select(Row, everything())

  Location_data   <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Location_data

                                        }) %>%
    dplyr::mutate(Row = seq(1, n())) %>%
    dplyr::select(Row, everything())

  #If we want an R output, return a list with the 4 different data frames
  if(output_type == "R"){

    output_object <- list(Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Individual_data = Individual_data,
                          Location_data = Location_data)

    if(save){

      saveRDS(output_object, file = paste0(save_path, "\\", filename, ".RDS"))

    }

    return(output_object)

  } else {

    message("Saving combined .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(save_path, "\\", filename, "_Brood_data.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(save_path, "\\", filename, "_Capture_data.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(save_path, "\\", filename, "Individual_data.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(save_path, "\\", filename, "_Location_data.csv"), row.names = F)

    invisible(NULL)

  }

  # Satisfy RCMD checks
  PopCode <- SpeciesCode <- Owner <- NULL

}
