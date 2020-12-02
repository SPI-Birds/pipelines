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
#' the \code{PopSpec} argument.
#'
#' @param file The location of the file in the standard format (.RDS file). This file is created using `run_pipelines`.
#' @param PopID Character vector of three letter population codes. Include all populations that are requested.
#' @param Species Character vector of six letter species codes. Include all species that are requested.
#' @param PopSpec Character vector of unique population species combinations (in the format PopID_Species).
#' Include all unique population species combinations requested. Can be used instead of `PopID` and `Species` arguments.
#' @param include_conflicting = TRUE, individuals with conflicting species information
#' are included in the subset of data. If include_conflicting = FALSE (default), these individuals are removed.
#' Note that this applies only to conflicted species individuals that were identified at least once as any
#' of the species specified in `Species` or `PopSpec`. Data on conflicted species individuals that were never
#' identified as any of the species specified in `Species` or `PopSpec` is never returned.
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
#' subset_datarqst(PopSpec = c("HOG_PARMAJ", "VEL_FICALB", "VLI_PARMAJ"))
#'
#' }

subset_datarqst <- function(file = file.choose(),
                            PopID = unique(pop_names$code),
                            Species = unique(Species_codes$Code),
                            PopSpec = NULL,
                            include_conflicting = FALSE,
                            output_type = "R",
                            save = TRUE,
                            test = FALSE){

  if(!test){
    message(crayon::bold(crayon::green("Select standard data file (RDS format).")))
    standard_data <- readRDS(file = file)
  }else{
    message(crayon::green("Using 'pipeline_output' as standard data."))
    standard_data <- pipeline_output
  }

  if(!is.null(PopSpec)){

    message(crayon::green("Filtering data using 'PopSpec'."))

    #Check that Species and PopID that make up PopSpec are all correct
    split <- stringr::str_split(string = PopSpec, pattern = "_", simplify = TRUE)
    check_pop(split[, 1])
    check_species(split[, 2])

    PopSpec_original <- PopSpec

    unique_pops <- unique(split[,1])

    if(include_conflicting){
      #PopSpec <- c(PopSpec, paste0(unique_pops, '_CCCCCC')) # Use after pipelines updated to standard format 1.1
      PopSpec <- c(PopSpec, paste0(unique_pops, '_CCCCCC'), paste0(unique_pops, '_CONFLICTED'))
    }

    output_brood <- standard_data$Brood_data %>%
      dplyr::mutate(PopID_Species = paste(.data$PopID, .data$Species, sep = "_")) %>%
      dplyr::filter(.data$PopID_Species %in% {{PopSpec}}) %>%
      dplyr::select(-.data$PopID_Species)

    output_individual <- standard_data$Individual_data %>%
      dplyr::mutate(PopID_Species = paste(.data$PopID, .data$Species, sep = "_"),
                    PopID_IndvID = paste(.data$PopID, .data$IndvID, sep = "_")) %>%
      dplyr::filter(.data$PopID_Species %in% {{PopSpec}}) %>%
      dplyr::select(-.data$PopID_Species)

    output_capture <- standard_data$Capture_data %>%
      dplyr::mutate(PopID_Species = paste(.data$CapturePopID, .data$Species, sep = "_"),
                    PopID_IndvID = paste(.data$CapturePopID, .data$IndvID, sep = "_")) %>%
      dplyr::filter(.data$PopID_Species %in% {{PopSpec}}) %>%
      dplyr::filter(.data$PopID_IndvID %in% output_individual$PopID_IndvID) %>%
      dplyr::select(-.data$PopID_Species, -.data$PopID_IndvID)

    output_location <- standard_data$Location_data %>%
      dplyr::filter(.data$PopID %in% {{unique_pops}})

  } else {

    message(crayon::green("Filtering data using 'PopID' and 'Species'."))

    #Check that Species and PopID are correct
    Species_original <- check_species(Species)
    PopID            <- check_pop(PopID)

    if(include_conflicting){
      #Species <- c(Species, 'CCCCCC') # Use after pipelines updated to standard format 1.1
      Species <- c(Species, 'CCCCCC', 'CONFLICTED')
    }

    output_brood <- standard_data$Brood_data %>%
      dplyr::filter(.data$PopID %in% {{PopID}} & .data$Species %in% {{Species}})

    output_individual <- standard_data$Individual_data %>%
      dplyr::mutate(PopID_IndvID = paste(.data$PopID, .data$IndvID, sep = "_")) %>%
      dplyr::filter(.data$PopID %in% {{PopID}} & .data$Species %in% {{Species}})

    output_capture <- standard_data$Capture_data %>%
      dplyr::filter(.data$CapturePopID %in% {{PopID}}  & .data$Species %in% {{Species}}) %>%
      dplyr::mutate(PopID_IndvID = paste(.data$CapturePopID, .data$IndvID, sep = "_")) %>%
      dplyr::filter(.data$PopID_IndvID %in% output_individual$PopID_IndvID) %>%
      dplyr::select(-.data$PopID_IndvID)


    if(!all(unique(Species_codes$Code) %in% Species) & all(unique(pop_names$code) %in% PopID)){

      PopID <- pop_species_combos %>%
        dplyr::filter(.data$species %in% {{Species}}) %>%
        dplyr::pull(.data$pop) %>%
        unique()

    }

    output_location <- standard_data$Location_data %>%
      dplyr::filter(.data$PopID %in% {{PopID}})

  }

  #Drop auxiliary columns
  output_individual <- output_individual %>%
    dplyr::select(-.data$PopID_IndvID)

  #If keeping conflicted species: identify conflicted species individuals that were
  # never identified as one of the species of interest and remove them from output
  if(include_conflicting){

    message("Identifying relevant conflicting-species individuals...")

    #Set species of interest
    if(!is.null(PopSpec)){
      SpeciesInt <- unique(stringr::str_split(string = PopSpec_original, pattern = "_", simplify = TRUE)[,2])
    }else{
      SpeciesInt <- Species_original
    }

    #Make a list of all individuals with conflicting species
    ConflSpec_IndvIDs <- output_individual %>%
      dplyr::filter(.data$Species %in% c('CONFLICTED', 'CCCCCC')) %>%
      dplyr::select(.data$IndvID)

    #Progress bar
    pb <- progress::progress_bar$new(total = length(unique(ConflSpec_IndvIDs$IndvID)),
                                     format = "[:bar] :percent ~:eta remaining",
                                     clear = FALSE)

    #Determine which individuals were never identified as a species of interest
    Irrelevant_IndvIDs <- output_capture %>%
      dplyr::filter(.data$IndvID %in% ConflSpec_IndvIDs$IndvID) %>%
      dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
      dplyr::group_by(.data$IndvID) %>%
      dplyr::summarise(Relevant = purrr::map_chr(.x = list(unique(na.omit(.data$Species))),
                                                 .f = ~{
                                                 pb$tick()
        if(any((..1) %in% SpeciesInt)){

          return("yes")

        } else {

          return("no")

        }

      }), .groups = "drop") %>%
      dplyr::rowwise() %>%
      dplyr::filter(.data$Relevant == "no")

    #Remove individuals never identified as one of the species of interest
    output_individual <- output_individual %>%
      filter(!(.data$IndvID %in% Irrelevant_IndvIDs$IndvID))

    output_capture <- output_capture %>%
      filter(!(.data$IndvID %in% Irrelevant_IndvIDs$IndvID))
  }

  #Arrange outputs chronologically
  output_brood <- output_brood %>%
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Species, .data$BroodID) %>%
    dplyr::mutate(Row = seq(1, n()))

  output_capture <- output_capture %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$CapturePopID, .data$Species, .data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::mutate(Row = seq(1, n()))

  output_individual <- output_individual %>%
    dplyr::arrange(.data$PopID, .data$Species, .data$IndvID) %>%
    dplyr::mutate(Row = seq(1, n()))

  output_location <- output_location %>%
    dplyr::arrange(.data$PopID, .data$LocationType, .data$StartSeason, .data$EndSeason, .data$LocationID) %>%
    dplyr::mutate(Row = seq(1, n()))

  #Combine output into one R object
  output_data <- list(Brood_data = output_brood,
                      Capture_data = output_capture,
                      Individual_data = output_individual,
                      Location_data = output_location)

  if(save){

    #Set save path

    save_path <- paste0(dirname(file), "/", as.character(Sys.Date()), "_DataRequest")

    if(!any(grepl(pattern = paste0(as.character(Sys.Date()), "_DataRequest"), x = list.dirs(dirname(file))))){

      dir.create(save_path)

    }


    #Save output as RDS or .csv files
    if(output_type == "R"){

      saveRDS(output_data, file = paste0(save_path, "/Output_data_", as.character(Sys.Date()), ".RDS"))

      message(crayon::cyan(crayon::bold("Data subset saved as R data file (.RDS).")))

}else if(output_type == "csv"){

      utils::write.csv(x = output_brood, file = paste0(save_path, "/Brood_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_capture, file = paste0(save_path, "/Capture_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_individual, file = paste0(save_path, "/Individual_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      utils::write.csv(x = output_location, file = paste0(save_path, "/Location_data_", as.character(Sys.Date()), ".csv"), row.names = F)

      message(crayon::cyan(crayon::bold("Data subset saved as .csv files.")))

      invisible(NULL)
    }

  }

  #Return R object
  message(crayon::green("Data subset returned as R object."))
  return(output_data)

}
