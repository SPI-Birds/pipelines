#'Construct standard format for data from Dlouhá Loučka, Czechia.
#'
#'A pipeline to produce the standard format for the study site at
#'Dlouhá Loučka, Czechia, administered by the Palacky University.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#'#'@inheritParams pipeline_params
#'
#'@return Generates either 6 .csv files or 6 data frames in the standard format.
#'@export
#'

format_DLO <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  # Load data
  # We read data in as text to prevent coercion issues
  all_data <- readxl::read_excel(paste0(db, "/DLO_PrimaryData.xlsx"), col_types = "text") %>%
    # Convert all cols to snake_case
    janitor::clean_names() %>%
    # Create IDs
    dplyr::mutate(siteID = "DLO",
                  # Ensure unique plotIDs; add siteID prefix
                  plotID = paste0("DLO_", tolower(.data$site)),
                  # Ensure unique locationIDs; requires plot & nestbox
                  locationID = paste(.data$site, .data$nestbox, sep = "_"),
                  # Ensure unique broodIDs; requires year, plot, nestbox, laying_date
                  # This accounts for multiple clutches in a single nestbox, in a single year
                  broodID = paste(.data$year, .data$site, .data$nestbox, .data$x1_egg_date, sep = "_"),
                  # TODO: Uncertainty in species identification (e.g., PA?) is ignored; check with data owner
                  speciesID = dplyr::case_when(.data$species == "CC" ~ species_codes$speciesID[species_codes$speciesCode == "10002"],
                                               .data$species == "PM" ~ species_codes$speciesID[species_codes$speciesCode == "10001"],
                                               grepl(pattern = "PA", x = .data$species) ~ species_codes$speciesID[species_codes$speciesCode == "10005"],
                                               grepl(pattern = "FA", x = .data$species) ~ species_codes$speciesID[species_codes$speciesCode == "10007"],
                                               .data$species == "PP" ~ species_codes$speciesID[species_codes$speciesCode == "10008"],
                                               .data$species == "SE" ~ species_codes$speciesID[species_codes$speciesCode == "10004"],
                                               .data$species == "FH" ~ species_codes$speciesID[species_codes$speciesCode == "10003"],
                                               .data$species == "PasMo" ~ species_codes$speciesID[species_codes$speciesCode == "10006"],
                                               # Other species IDs are excluded - too few observations or unknown species
                                               .data$species == "CerBra" ~ NA_character_,
                                               .data$species == "FX" ~ NA_character_,
                                               .data$species == "Parus" ~ NA_character_,
                                               .data$species == "PhPh" ~ NA_character_)) %>%
    # Convert dates
    dplyr::rowwise() %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$x1_egg_date, .data$hatching_date, .data$fledging_date,
                                         .data$date_of_predation_event, .data$date_f, .data$date_m),
                     .funs = ~{

                       if(is.na(..1)){

                         as.Date(NA)

                       } else {

                         janitor::excel_numeric_to_date(as.numeric(..1))

                       }

                     })

  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_CHO(data = all_data,
                                 species_filter = species,
                                 optional_variables = optional_variables)

}


#' Create brood data table for Dlouhá Loučka, Czechia.
#'
#' Create brood data table in standard format for data from Dlouhá Loučka, Czechia.
#'
#' @param data Data frame. Primary data from Dlouhá Loučka, Czechia.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_DLO <- function(data,
                             species_filter,
                             optional_variables = NULL) {

  Brood_data <- data %>%
    # Remove columns that do not contain relevant brood info
    dplyr::select(-.data$source:-.data$notes_57) %>%
    # Unify ring formats; both lower-case and upper-case letters are used
    dplyr::mutate(femaleID = toupper(.data$f_ring),
                  maleID = toupper(.data$m_ring),
                  observedLayYear = as.integer(lubridate::year(.data$x1_egg_date)),
                  observedLayMonth = as.integer(lubridate::month(.data$x1_egg_date)),
                  observedLayDay = as.integer(lubridate::day(.data$x1_egg_date)),
                  # TODO: Clutch size 8+2: 8 PERATE + 2 Ficedula eggs (according to notes); check with data owner
                  observedClutchSize = dplyr::case_when(.data$clutch == "8+2" ~ "8",
                                                        TRUE ~ .data$clutch),
                  observedClutchSize = as.integer(.data$observedClutchSize),
                  # TODO: Uncertainty in nesting attempt (observedClutchType) (e.g., f?) is ignored; check with data owner
                  observedClutchType = dplyr::case_when(.data$nesting_attempt %in% c("f", "F", "f?") ~ "first",
                                                        .data$nesting_attempt %in% c("r", "R") ~ "replacement",
                                                        .data$nesting_attempt %in% c("s", "S") ~ "second",
                                                        .data$nesting_attempt == "?" ~ NA_character_,
                                                        TRUE ~ NA_character_),
                  observedHatchYear = as.integer(lubridate::year(.data$hatching_date)),
                  observedHatchMonth = as.integer(lubridate::month(.data$hatching_date)),
                  observedHatchDay = as.integer(lubridate::day(.data$hatching_date)),
                  # NB: ? in number hatched is set to NA
                  observedBroodSize = as.integer(dplyr::na_if(.data$no_hatched, "?")),
                  observedFledgeYear = as.integer(lubridate::year(.data$fledging_date)),
                  observedFledgeMonth = as.integer(lubridate::month(.data$fledging_date)),
                  observedFledgeDay = as.integer(lubridate::day(.data$fledging_date)),
                  # NB: ? in number fledged is set to NA
                  observedNumberFledged = as.integer(dplyr::na_if(.data$no_fledged, "?")),
                  row = 1:n())

}

#----------------------#
#TODO: Check with data owner that lower-case and upper-case ring numbers belong to the same individual
#TODO: Check some species IDs with data owner (e.g., Parus)
#TODO: Check nesting attempt uncertainty with data owner (e.g., f?)
#TODO: Check 8+2 clutch size
