#' Run multiple data pipelines
#'
#' Run multiple data pipelines. Currently, this produces separate .csv files but
#' will eventually produce combined .csv files for all populations.
#'
#' @param path File path. Location of all folders for data owners. Note, the
#'   folders for each data owner must include the unique code of the data owner
#'   as seen in pop_codes.
#' @param PopID The three-letter code of populations to format as listed in the
#'   \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'    protocol}.
#' @param Species The six-letter code of species to include as listed in the
#'   \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard
#'    protocol}. Note, this argument takes precedence over argument PopID (i.e.
#'   if a population doesn't have the requested species it will not be
#'   formatted.)
#' @param output_type Should the pipeline generate .csv files ('csv') or R objects ('R'). Default: R.
#' @param save TRUE/FALSE. Should the output be saved locally? This is only relevant where
#' `output_type` is 'R'. If output_type is 'csv' 4 (in case of protocol version 1.0.0 and 1.1.0) or 6 (in case of protocol version 2.0.0) .csv files will be created in the save path (specified by `save_path` argument). If output_type is 'R'
#' and `save` is TRUE, an .RDS file will be created in the save path.
#' @param save_path Path where files will be saved if `save` is TRUE. By default, the save
#' path will be path/standard_format.
#' @param filename The file name of the saved file. No file extension is
#' needed, as this will differ depending on `output_type`. By default, file name is
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
                          save = FALSE,
                          save_path = NULL,
                          filename = "standard_format"){

  #Force choose_directory()
  force(path)

  if((output_type == "csv" | (output_type == "R" & save)) & is.null(save_path)){

    save_path <- paste(path, "standard_format", sep = "/")

    if(!any(grepl(pattern = "^standard_format", x = list.dirs(path)))){

      dir.create(save_path)

    }

  }

  #If PopID is NULL use all populations
  if(is.null(PopID)){

    PopID <- pop_codes$PopID

  }

  #Determine operating system
  OS <- tolower(utils::sessionInfo()$running)

  #Drop populations from Access-based primary data if running on Mac
  PopID_Access <- c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "AMM")

  if(grepl(pattern = 'mac', x = OS)){

    if(length(PopID[which(PopID%in%PopID_Access)] > 0)){
      warning(paste0('Pipelines not run for the following populations due to OS incompatibility: ',
                    toString(PopID[which(PopID%in%PopID_Access)]),
                    ". To obtain standard format data for these populations, please run on a Windows OS.")
              )
    }

    PopID <- PopID[which(!(PopID%in%PopID_Access))]

  }else if(!grepl(pattern = 'mac|windows', x = OS)){
    stop(paste0('Operating system ', OS, ' not supported'))
  }

  if(length(PopID) == 0){
    stop(paste0('None of the selected pipeline(s) could not be run due to OS incompatibility. Please run on a Windows OS.'))
  }

  #Assign species for filtering
  if(is.null(Species)){

    Species <- species_codes$Species

  } else if(all(!Species %in% species_codes$Species)){

    stop("Species provided are not included in the pipelines. Please select from species listed in species_codes")

  }

  #Firstly, check if there are any cases where a requested population does not have any info on a given species
  missing_species <- pop_species_combos %>%
    dplyr::filter(.data$PopCode %in% PopID) %>%
    dplyr::group_by(.data$PopCode) %>%
    dplyr::summarise(total_sp = sum(.data$SpeciesCode %in% Species))

  #If there are any with missing data, give a warning message
  if(any(missing_species$total_sp == 0)){

    missing_species %>%
      dplyr::filter(.data$total_sp == 0) %>%
      purrr::pwalk(.l = list(.data$PopCode),
                   .f = ~{

                     message(paste0('Population ', ..1, ' has no information on the focal species and has been excluded.'))

                   })

  }

  #Now just work with those populations where the population and species are present
  pop_species_subset <- pop_species_combos %>%
    dplyr::filter(.data$PopCode %in% PopID & .data$SpeciesCode %in% Species) %>%
    dplyr::left_join(pop_codes %>%
                       dplyr::select("PopID", "Owner"),
                     by = c("PopCode" = "PopID")) %>%
    dplyr::group_by(.data$Owner) %>%
    dplyr::summarise(Species = list(c(unique(.data$SpeciesCode))),
                     Pops = list(c(unique(.data$PopCode))))

  #Find the file path for each of the data owners of interest
  all_dirs <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)
  all_dirs <- all_dirs[grepl(pattern = paste(pop_species_subset$Owner, collapse = "|"), all_dirs)]
  all_dirs <- gsub(pattern = "\\", replacement = "/", x = all_dirs, fixed = TRUE)

  #For each data owner, run the pipeline using the populations requested
  #Return R lists rather than generating .csv files
  R_objects <- purrr::pmap(.l = list(dirs = all_dirs,
                                     owner = pop_species_subset$Owner,
                                     pops = pop_species_subset$Pops,
                                     species = pop_species_subset$Species),
                           .f = function(dirs, owner, pops, species){

                             message(crayon::cyan$bold(paste0('Running ', owner, ' pipeline')))

                             eval(parse(text = paste0('format_', owner, '(db = dirs, pop = pops, species = species, output_type = "R")')))

                           })

  #For each of the four tables, go through and combine the outputs
  #Add row numbers for each
  Brood_data <- purrr::map_df(.x = R_objects,
                                   .f = ~{

                                     .x$Brood_data

                                   }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select("Row", dplyr::everything())

  Capture_data <- purrr::map_df(.x = R_objects,
                                     .f = ~{

                                       .x$Capture_data

                                     }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select("Row", dplyr::everything())

  Individual_data <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Individual_data

                                        }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select("Row", dplyr::everything())

  Location_data   <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Location_data

                                        }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select("Row", dplyr::everything())

  protocol_version <- purrr::map_chr(.x = R_objects,
                                     .f = "protocol_version") %>%
    unique()

  if(length(protocol_version) > 1) {

    stop(paste0('Selected pipelines are based on different protocol versions. Only include pipelines of the same protocol version.'))

  }


  #If we want an R output, return a list with the 4 different data frames
  if(output_type == "R"){

    output_object <- list(Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Individual_data = Individual_data,
                          Location_data = Location_data,
                          protocol_version = protocol_version)

    if(save){

      saveRDS(output_object, file = paste0(save_path, "/", filename, ".RDS"))

    }

    return(output_object)

  } else {

    message("Saving combined .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(save_path, "/", filename, "_Brood_data.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(save_path, "/", filename, "_Capture_data.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(save_path, "/", filename, "_Individual_data.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(save_path, "/", filename, "_Location_data.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(save_path, "/", filename, "_protocol_version.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)

    invisible(NULL)

  }

}
