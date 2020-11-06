#' Create a subset of SPI-Birds standard data
#'
#' Create a subset of data stored in the standard format.
#'
#' The SPI-Birds team should use the function `run_pipelines`
#' to create a copy of all populations in the standard format,
#' which can be stored locally on the SPI-Birds computer.
#'
#' When a request is made, `subset_datarqst` can be used
#' to take a subset of this larger file to cover the specific
#' populations/species requested by a user. The request can be specified
#' either through the \code{PopID} and \code{Species} arguments, or through
#' the \code{filter} argument.
#'
#' @param file The location of the file in the standard format (.RDS file). This file is created using `run_pipelines`.
#' @param PopID Character vector of three letter population codes. Include all populations that are requested.
#' @param Species Character vector of six letter species codes. Include all species that are requested.
#' @param filter Character vector of unique population species combinations (in the format PopID_Species).
#' Include all unique population species combinations requested. Can be used instead of `PopID` and `Species` arguments.
#' @param include_conflicting = TRUE, individuals with conflicting species information
#' are included in the subset of data. If include_conflicting = FALSE (default), these individuals are removed.
#' Note that this applies only to conflicted species individuals that were identified at least once as any
#' of the species specified in `Species` or `filter`. Data on conflicted species individuals that were never
#' identified as any of the species specified in `Species` or `filter` is never returned.
#' @param output_type is 'R' and can be set to 'csv'. If output_type is 'csv' 4 .csv files will be created in the save path.
#' If output_type is 'R' an .RDS file will be created in the save path, and an R object in the
#' running R session if return_R = TRUE.
#' @param save is TRUE. If save = FALSE, the subset of data is not saved into the save path.
#' @param test If FALSE (default), subset is created from the most recent version of the SPI-Birds standard format.
#' If TRUE, subset is created from R object `pipeline_output` which is created temporarily during package testing.
#' @return Creates 1 RDS or 4 .csv files in a folder at the location of the standard data. Folder and files
#' will be given a unique identifier in the form of the date when the subset was performed.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #Subset data on all species from two populations
#' subset_datarqst(PopID = c("HOG", "CHO"))
#'
#' #Subset data on all population where two species occurs
#' subset_datarqst(Species = c("PARMAJ", "FICALB"))
#'
#' #Create a specific subset of species and population combinations
#' subset_datarqst(filter = c("HOG_PARMAJ", "VEL_FICALB", "VLI_PARMAJ"))
#'
#' }

subset_datarqst <- function(file = file.choose(),
                            PopID = unique(pop_names$code),
                            Species = unique(Species_codes$Code),
                            filter = NULL,
                            include_conflicting = FALSE,
                            output_type = "R",
                            save = TRUE,
                            test = FALSE){

  if(!test){
    readRDS(file = file)
  }else{
    standard_data <- pipeline_output
  }


  if(!is.null(filter)){

    filter_original <- filter

    unique_pops <- unique(stringr::str_split(string = filter, pattern = "_", simplify = TRUE)[,1])

    if(include_conflicting){
      #filter <- c(filter, paste0(unique_pops, '_CCCCCC')) # Use after pipelines updated to standard format 1.1
      filter <- c(filter, paste0(unique_pops, '_CCCCCC'), paste0(unique_pops, '_CONFLICTED'))
    }

    output_brood <- standard_data$Brood_data %>%
      dplyr::mutate(Pop_sp = paste(.data$PopID, .data$Species, sep = "_")) %>%
      dplyr::filter(.data$Pop_sp %in% filter) %>%
      dplyr::select(-.data$Pop_sp)

    output_individual <- standard_data$Individual_data %>%
      dplyr::mutate(Pop_sp = paste(.data$PopID, .data$Species, sep = "_")) %>%
      dplyr::filter(.data$Pop_sp %in% filter) %>%
      dplyr::select(-.data$Pop_sp)

    output_capture <- standard_data$Capture_data %>%
      dplyr::filter(.data$IndvID %in% output_individual$IndvID)

    output_location <- standard_data$Location_data %>%
      dplyr::filter(.data$PopID %in% unique_pops)

  } else {

    Species_original <- Species

    if(include_conflicting){
      #Species <- c(Species, 'CCCCCC') # Use after pipelines updated to standard format 1.1
      Species <- c(Species, 'CCCCCC', 'CONFLICTED')
    }

    output_brood <- standard_data$Brood_data %>%
      dplyr::filter(.data$PopID %in% {{PopID}} & .data$Species %in% {{Species}})

    output_individual <- standard_data$Individual_data %>%
      dplyr::filter(.data$PopID %in% {{PopID}} & .data$Species %in% {{Species}})

    output_capture <- standard_data$Capture_data %>%
      dplyr::filter(.data$IndvID %in% output_individual$IndvID)


    if(!all(unique(Species_codes$Code) %in% Species) & all(unique(pop_names$code) %in% PopID)){

      PopID <- pop_species_combos %>%
        dplyr::filter(.data$species %in% Species) %>%
        dplyr::pull(.data$pop) %>%
        unique()

    }

    output_location <- standard_data$Location_data %>%
      dplyr::filter(.data$PopID %in% {{PopID}})

  }

  #If keeping conflicted species: identify conflicted species individuals that were
  # never identified as one of the species of interest and remove them from output
  if(include_conflicting){

    #Set species of interest
    if(!is.null(filter)){
      SpeciesInt <- unique(stringr::str_split(string = filter_original, pattern = "_", simplify = TRUE)[,2])
    }else{
      SpeciesInt <- Species_original
    }

    #Determine which individuals were never identified as a species of interest
    Irrelevant_IndvIDs <- output_capture %>%
      dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
      dplyr::group_by(.data$IndvID) %>%
      dplyr::summarise(Relevant = purrr::map_chr(.x = list(unique(na.omit(.data$Species))), .f = ~{

        if(any((..1) %in% SpeciesInt)){

          return("yes")

        } else {

          return("no")

        }

      })) %>%
      dplyr::rowwise() %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$Relevant == "no")

    #Remove individuals never identified as one of the species of interest
    output_individual <- output_individual %>%
      filter(!(.data$IndvID %in% Irrelevant_IndvIDs$IndvID))

    output_capture <- output_capture %>%
      filter(!(.data$IndvID %in% Irrelevant_IndvIDs$IndvID))
  }

  #Combine output into one R object (and return it)
  output_data <- list(Brood_data = output_brood,
                      Capture_data = output_capture,
                      Individual_data = output_individual,
                      Location_data = output_location)

  return(output_data)

  if(save){

    #Set save path

    save_path <- paste0(dirname(file), "/", as.character(Sys.Date()), "_DataRequest")

    if(!any(grepl(pattern = paste0(as.character(Sys.Date()), "_DataRequest"), x = list.dirs(dirname(file))))){

      dir.create(save_path)

    }


    #Save output as RDS or .csv files
    if(output_type == "R"){

      message("Output subset as R Data file...")

      saveRDS(output_data, file = paste0(save_path, "/Output_data_", as.character(Sys.Date()), ".RDS"))

    }else{

      message("Output subset as .csv files...")

      utils::write.csv(x = output_brood, file = paste0(save_path, "/Brood_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_capture, file = paste0(save_path, "/Capture_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_individual, file = paste0(save_path, "/Individual_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_location, file = paste0(save_path, "/Location_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      invisible(NULL)
    }

  }

}
