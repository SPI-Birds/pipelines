#' Run multiple data pipelines
#'
#' Run multiple data pipelines. Currently, this produces separate .csv files but will eventually produce combined .csv files for all sites.
#'
#' @param path File path. Location of all folders for data owners. Note, the folders for each data owner must include the unique code of the data owner as seen in `site_codes`.
#' @param site The three-letter code of sites to format as listed in `site_codes`.
#' @param species The six-letter code of species to include as listed in `species_codes`. Note, this argument takes precedence over argument siteID (i.e., if a site doesn't have the requested species it will not be formatted.)
#' @param output_type Should the pipeline generate .csv files ('csv') or R objects ('R'). Default: R.
#' @param save TRUE/FALSE. Should the output be saved locally? This is only relevant where
#' `output_type` is 'R'. If output_type is 'csv'
#' 4 .csv files will be created in the save path (specified by `save_path` argument). If output_type is 'R'
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
#' HAR_data <- run_pipelines(site = "HAR", species = "PARMAJ", output_type = "R")
#'
#' }

run_pipelines <- function(path = choose_directory(),
                          site = NULL,
                          species = NULL,
                          output_type = "R",
                          save = FALSE,
                          save_path = NULL,
                          filename = "standard_format"){

  #Force choose_directory()
  force(path)

  if((output_type == "csv" | (output_type == "R" & save)) & is.null(save_path)){

    save_path <- paste(path, "standard_format", sep = "/")

    if(!any(grepl(pattern = "standard_format", x = list.dirs(path)))){

      dir.create(save_path)

    }

  }

  #If site is NULL use all sites
  if(is.null(site)){

    site <- site_codes$siteID

  }

  #Determine operating system
  OS <- tolower(utils::sessionInfo()$running)

  #Drop sites from Access-based primary data if running on Mac
  Access_sites <- c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "AMM")

  if(grepl(pattern = 'mac', x = OS)){

    if(length(site[which(site %in% Access_sites)] > 0)){
      warning(paste0('Pipelines not run for the following sites due to OS incompatibility: ',
                    toString(site[which(site %in% Access_sites)]),
                    ". To obtain standard format data for these sites, please run on a Windows OS.")
              )
    }

    site <- site[which(!(site %in% Access_sites))]

  } else if(!grepl(pattern = 'mac|windows', x = OS)){

    stop(paste0('Operating system ', OS, ' not supported'))

  }

  if(length(site) == 0){

    stop(paste0('None of the selected pipeline(s) could not be run due to OS incompatibility. Please run on a Windows OS.'))

  }

  #Assign species for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  } else if(all(!species %in% species_codes$speciesID)){

    stop("Species provided are not included in the pipelines. Please select from species listed in species_codes")

  }

  #Firstly, check if there are any cases where a requested site does not have any info on a given species
  missing_species <- site_species_combos %>%
    dplyr::filter(.data$siteID %in% {{site}}) %>%
    dplyr::group_by(.data$siteID) %>%
    dplyr::summarise(total_species = sum(.data$speciesID %in% {{species}}))

  #If there are any with missing data, give a warning message
  if(any(missing_species$total_species == 0)){

    missing_species %>%
      dplyr::filter(total_species == 0) %>%
      purrr::pwalk(.l = list(.data$siteID),
                   .f = ~{

                     message(paste0('Dite ', ..1, ' has no information on the focal species and has been excluded.'))

                   })

  }

  #Now just work with those sites where the site and species are present
  site_species_subset <- site_species_combos %>%
    dplyr::filter(.data$siteID %in% {{site}} & .data$speciesID %in% {{species}}) %>%
    dplyr::left_join(dplyr::select(site_codes, siteID, institutionID), by = "siteID") %>%
    dplyr::group_by(.data$institutionID) %>%
    dplyr::summarise(Species = list(c(unique(speciesID))),
                     Sites = list(c(unique(siteID))))

  #Find the file path for each of the data owners of interest
  all_dirs <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)
  all_dirs <- all_dirs[grepl(pattern = paste(site_species_subset$institutionID, collapse = "|"), all_dirs)]
  all_dirs <- gsub(pattern = "\\", replacement = "/", x = all_dirs, fixed = TRUE)

  #For each data owner, run the pipeline using the sites requested
  #Return R lists rather than generating .csv files
  R_objects <- purrr::pmap(.l = list(dirs = all_dirs,
                                     owner = site_species_subset$institutionID,
                                     sites = site_species_subset$Sites,
                                     species = site_species_subset$Species),
                           .f = function(dirs, owner, sites, species){

                             message(paste0('Running ', owner, ' pipeline'))

                           eval(parse(text = paste0('format_', owner, '(db = dirs, pop = sites, species = species, output_type = "R")')))

                             })

  #For each of the four tables, go through and combine the outputs
  #Add row numbers for each
  Brood_data <- purrr::map_df(.x = R_objects,
                                   .f = ~{

                                     .x$Brood_data

                                   }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select(Row, dplyr::everything())

  Capture_data <- purrr::map_df(.x = R_objects,
                                     .f = ~{

                                       .x$Capture_data

                                     }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select(Row, dplyr::everything())

  Individual_data <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Individual_data

                                        }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select(Row, dplyr::everything())

  Location_data   <- purrr::map_df(.x = R_objects,
                                        .f = ~{

                                          .x$Location_data

                                        }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Row = seq(1, dplyr::n())) %>%
    dplyr::select(Row, dplyr::everything())

  #If we want an R output, return a list with the 4 different data frames
  if(output_type == "R"){

    output_object <- list(Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Individual_data = Individual_data,
                          Location_data = Location_data)

    if(save){

      saveRDS(output_object, file = paste0(save_path, "/", filename, ".RDS"))

    }

    return(output_object)

  } else {

    message("Saving combined .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(save_path, "/", filename, "_Brood_data.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(save_path, "/", filename, "_Capture_data.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(save_path, "/", filename, "_Individual_data.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(save_path, "/", filename, "_Location_data.csv"), row.names = F)

    invisible(NULL)

  }

}
