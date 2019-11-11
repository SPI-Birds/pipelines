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
#' populations/species requested by a user.
#'
#' @param file The location of the file in the standard format (.RDS file). This file is created using `run_pipelines`.
#' @param PopID Character vector of three letter population codes. Include all populations that are requested.
#' @param Species Character vector of six letter species codes. Include all species that are requested.
#' @param filter Character vector of unique population species combinations (in the format PopID_Species).
#' Include all unique population species combinations requested.
#'
#' @return Create 4 .csv files at the location of the standard data. Files will be given a unique number
#' based on the date when the subset was performed.
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
                            filter = NULL){

  standard_data <- readRDS(file = file)

  if(!is.null(filter)){

    unique_pops <- unique(stringr::str_split(string = filter, pattern = "_")[[1]])

    output_brood <- standard_data$Brood_data %>%
      dplyr::mutate(Pop_sp = paste(PopID, Species, sep = "_")) %>%
      dplyr::filter(Pop_sp %in% filter)

    output_capture <- standard_data$Capture_data %>%
      dplyr::mutate(Pop_sp = paste(CapturePopID, Species, sep = "_")) %>%
      dplyr::filter(Pop_sp %in% filter)

    output_individual <- standard_data$Individual_data %>%
      dplyr::mutate(Pop_sp = paste(PopID, Species, sep = "_")) %>%
      dplyr::filter(Pop_sp %in% filter)

    output_location <- standard_data$Location_data %>%
      dplyr::filter(PopID %in% unique_pops)

  } else {

    output_brood <- standard_data$Brood_data %>%
      dplyr::filter(PopID %in% {{PopID}} & Species %in% {{Species}})

    output_capture <- standard_data$Capture_data %>%
      dplyr::filter(CapturePopID %in% {{PopID}} & Species %in% {{Species}})

    output_individual <- standard_data$Individual_data %>%
      dplyr::filter(PopID %in% {{PopID}} & Species %in% {{Species}})

    if(!all(unique(Species_codes$Code) %in% Species) & all(unique(pop_names$code) %in% PopID)){

      PopID <- pop_species_combos %>%
        dplyr::filter(species %in% Species) %>%
        dplyr::pull(pop) %>%
        unique()

    }

    output_location <- standard_data$Location_data %>%
      dplyr::filter(PopID %in% {{PopID}})

  }

  message("Output subset as .csv files...")

  save_path <- dirname(file)

  utils::write.csv(x = output_brood, file = paste0(save_path, "\\Brood_data", as.numeric(Sys.Date()), ".csv"), row.names = F)

  utils::write.csv(x = output_capture, file = paste0(save_path, "\\Capture_data", as.numeric(Sys.Date()), ".csv"), row.names = F)

  utils::write.csv(x = output_individual, file = paste0(save_path, "\\Individual_data", as.numeric(Sys.Date()), ".csv"), row.names = F)

  utils::write.csv(x = output_location, file = paste0(save_path, "\\Location_data", as.numeric(Sys.Date()), ".csv"), row.names = F)

  invisible(NULL)

}
