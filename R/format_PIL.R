#'Construct standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Pilis-Visegrád Mountains, Hungary, administered by the János Török (Eötvös Loránd University).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{LocationID}: Nestbox numbers are not unique across plots. Therefore, unique locationIDs are
#'a combination of plot and nestbox number.
#'
#'\strong{BroodID}: Unique broods are Year, LocationID, LayingDate (days since Mar 31st).
#'
#'\strong{ExperimentID}: Experiments are listed as affecting number fledglings, clutch size etc.
#' so all experiments are classed as 'COHORT'. ExperimentID '2' is also classified as 'PHENOLOGY'
#' as it is said to affect laying date.
#'
#'\strong{CaptureDate}: Nestlings have a ring date and a nestling measure date column.
#'When there is only a ring date, the nestlings are captured once (ringing and measurement).
#'When there is a ring and nestling measure date these are considered two captures.
#'The ring date is assumed to have no measurements (Mass, Tarsus, WingLength are NA).
#'
#'\strong{Species}: There are a (small) number of hybrid flycatcher broods, with
#'male FICHYP and female FICALB. This causes some problems for our classification of
#'brood and chick species. For broods, we know these are legitimate multi-species
#'broods, so they should not be flagged as warnings. For chicks, we don't have a species
#'code for flycatcher hybrids. Currently, all hybrid broods and chicks are removed
#'(there are only 5 broods and 16 chicks). Species is still included for
#'adult captures as we know the exact species of each parent. We will need to determine
#'how we want to deal with these hybrid broods in the future.
#'
#'\strong{Age_observed}: Age is not explicitly recorded, therefore we classify all
#'adults as NA age. Individuals in nestling columns are assumed to always be pre-fledging
#'and are given age 1.
#'
#'\strong{CaptureAlive, ReleaseAlive}: All individuals are assumed to be captured and released alive.
#'
#'\strong{Location_data}: All unique locationIDs (plot/nestbox number) are
#'assumed to be active across the whole study period.
#'
#'\strong{Mass, Tarsus & WingLength}: There are some character strings in these columns.
#'These variables are silently coerced to numeric such that all character strings
#'are coerced to NA.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_PIL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  #Load data
  #As always, we read in as text to prevent coercion issues
  #We convert the data later
  PIL_data <- readxl::read_excel(path = paste0(db, "/PIL_PrimaryData.xlsx"), col_types = "text", na = "NA") %>%
    janitor::clean_names() %>%
    #Convert all date columns
    #Some are march days some are actual dates (but different formats)
    dplyr::mutate(mar_31 = as.Date(paste(.data$year, 3, 31, sep = "-")),
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(.data$plot, .data$nestbox, sep = "_"),
                  #BroodID requires year, plot, nestbox, laying_date
                  #This accounts for multiple clutches in a year
                  #TODO: check with data custodian what to do with records
                  # where nestbox and/or laying date are NA
                  BroodID = paste(.data$year, .data$plot, .data$nestbox, .data$laying_date,
                                  sep = "_")) %>%
    #Convert march days
    dplyr::mutate(dplyr::across(.cols = c("laying_date", "hdate"),
                                .fns = ~{.data$mar_31 + as.numeric(.x)})) %>%
    #Convert regular dates
    dplyr::mutate(dplyr::across(.cols = c("female_date", "male_date",
                                          "ring_date", "nestling_measure_date"),
                                .fns = ~{

                                  dplyr::case_when(is.na(.x) ~ as.Date(NA),
                                                   grepl(pattern = "\\.", .x) ~ as.Date(.x, format = "%Y.%m.%d"),
                                                   TRUE ~ suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.x))))

                                }))


  # BROOD DATA

  message("Compiling brood data....")

  Brood_data <- create_brood_PIL(PIL_data = PIL_data, species_filter = species)

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_PIL(PIL_data = PIL_data, species_filter = species)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_PIL(Capture_data = Capture_data, protocol_version = protocol_version)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_PIL(PIL_data = PIL_data, protocol_version = protocol_version)

  # WRANGLE DATA FOR EXPORT

  # Add ChickAge to capture data
  # Add hatchdate from brood data
  Capture_data <- Capture_data %>%
    dplyr::left_join(Brood_data %>%
                       dplyr::select("BroodID", "HatchDate_observed"),
                     by = "BroodID") %>%
    dplyr::mutate(ChickAge = dplyr::case_when(is.na(.data$Age_observed) ~ NA_integer_,
                                              !is.na(.data$Age_observed) ~ as.integer(.data$CaptureDate - .data$HatchDate_observed)))
  ## FIXME: many-to-many relationship because of duplicated BroodIDs,
  ## where one or more of the components that make up BroodID are NA.

  # Determine avg measures for each brood
  avg_mass <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16) & !is.na(.data$Mass)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Mass),
                     NumberChicksMass = dplyr::n())

  avg_tarsus <- Capture_data %>%
    dplyr::filter(dplyr::between(.data$ChickAge, 14, 16) & !is.na(.data$Tarsus)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgTarsus = mean(.data$Tarsus),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = "Alternative")

  # Add into Brood_data
  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_mass,
                     by = "BroodID") %>%
    dplyr::left_join(avg_tarsus,
                     by = "BroodID")

  # Remove unneccesary columns in Brood and Capture data
  Brood_data <- Brood_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Brood_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Brood_data))

  Capture_data <- Capture_data %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Capture_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Capture_data))

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("\nAll tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("\nSaving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PIL.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PIL.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>%
                       dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_PIL.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PIL.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_PIL.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data,
                protocol_version = protocol_version))

  }

}

#' Create brood data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create brood data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_PIL <- function(PIL_data, species_filter){

  Brood_data <- PIL_data %>%
    dplyr::mutate(PopID = "PIL",
                  BreedingSeason = as.integer(.data$year),
                  Plot = .data$plot,
                  #They record hybrid broods in the species info
                  #They give this a unique species ID.
                  #Currently, we're just including the major species with > 100 broods
                  #(CYACAE, FICALB, PARMAJ, SITEUR)
                  #Note, we still use our species_codes table even though they use the same 6 letter codes
                  #This is because it will be easier to change if e.g. species are renamed
                  #because we can just update the species_codes table once.
                  Species = dplyr::case_when(.data$species == "CYACAE" ~ species_codes$Species[species_codes$speciesEURINGCode == 14620],
                                             .data$species == "PARMAJ" ~ species_codes$Species[species_codes$speciesEURINGCode == 14640],
                                             .data$species == "FICALB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13480],
                                             .data$species == "SITEUR" ~ species_codes$Species[species_codes$speciesEURINGCode == 14790]),
                  ExperimentID = dplyr::case_when(.data$exp == "0" ~ NA_character_,
                                                  .data$exp == "1" ~ "COHORT",
                                                  .data$exp == "2" ~ "COHORT;PHENOLOGY",
                                                  .data$exp == "3" ~ "COHORT"),
                  LayDate_observed = .data$laying_date,
                  ClutchSize_observed = as.integer(.data$clutch_size),
                  BroodSize_observed = as.integer(.data$number_hatchlings),
                  NumberFledged_observed = as.integer(.data$number_fledglings),
                  HatchDate_observed = .data$hdate,
                  FemaleID = .data$femalering,
                  MaleID = .data$malering) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"))

  return(Brood_data)

}

#' Create capture data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create capture data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_capture_PIL <- function(PIL_data, species_filter){

  # Female captures
  female_capture_data <- PIL_data %>%
    dplyr::select("year":"species", "femalering":"f_mass") %>%
    dplyr::filter(!is.na(.data$femalering)) %>%
    dplyr::mutate(CapturePopID = "PIL",
                  ReleasePopID = "PIL",
                  BreedingSeason = as.integer(.data$year),
                  CapturePlot = .data$plot,
                  ReleasePlot = .data$plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(.data$plot, .data$nestbox, sep = "_"),
                  IndvID = toupper(.data$femalering),
                  CaptureDate = .data$female_date,
                  Age_observed = NA_integer_,
                  ObserverID = .data$f_measure_person,
                  Tarsus = suppressWarnings(as.numeric(.data$f_tarsus))/10,
                  WingLength = suppressWarnings(as.numeric(.data$f_wing)),
                  Mass = suppressWarnings(as.numeric(.data$f_mass))/10,
                  Sex_observed = "F",
                  Species = dplyr::case_when(.data$species == "CYACAE" ~ species_codes$Species[species_codes$speciesEURINGCode == 14620],
                                             .data$species == "PARMAJ" ~ species_codes$Species[species_codes$speciesEURINGCode == 14640],
                                             .data$species == "FICALB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13480],
                                             .data$species == "SITEUR" ~ species_codes$Species[species_codes$speciesEURINGCode == 14790],
                                             .data$species == "FICHIB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13480]))

  # Male captures
  male_capture_data <- PIL_data %>%
    dplyr::select("year":"species",
                  "malering":"m_mass") %>%
    dplyr::filter(!is.na(.data$malering)) %>%
    dplyr::mutate(CapturePopID = "PIL",
                  ReleasePopID = "PIL",
                  BreedingSeason = as.integer(.data$year),
                  CapturePlot = .data$plot,
                  ReleasePlot = .data$plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(.data$plot, .data$nestbox, sep = "_"),
                  IndvID = toupper(.data$malering),
                  CaptureDate = .data$male_date,
                  Age_observed = NA_integer_,
                  ObserverID = .data$m_measure_person,
                  Tarsus = suppressWarnings(as.numeric(.data$m_tarsus))/10,
                  WingLength = suppressWarnings(as.numeric(.data$m_wing)),
                  Mass = suppressWarnings(as.numeric(.data$m_mass))/10,
                  Sex_observed = "M",
                  Species = dplyr::case_when(.data$species == "CYACAE" ~ species_codes$Species[species_codes$speciesEURINGCode == 14620],
                                             .data$species == "PARMAJ" ~ species_codes$Species[species_codes$speciesEURINGCode == 14640],
                                             .data$species == "FICALB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13480],
                                             .data$species == "SITEUR" ~ species_codes$Species[species_codes$speciesEURINGCode == 14790],
                                             .data$species == "FICHIB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13490]))

  # Chick captures
  chick_capture_data <- PIL_data %>%
    #Remove unwanted species
    dplyr::mutate(Species = dplyr::case_when(.data$species == "CYACAE" ~ species_codes$Species[species_codes$speciesEURINGCode == 14620],
                                             .data$species == "PARMAJ" ~ species_codes$Species[species_codes$speciesEURINGCode == 14640],
                                             .data$species == "FICALB" ~ species_codes$Species[species_codes$speciesEURINGCode == 13480],
                                             .data$species == "SITEUR" ~ species_codes$Species[species_codes$speciesEURINGCode == 14790])) %>%
    dplyr::filter(.data$Species %in% species_filter) %>%
    #Remove cases where no chicks were ever ringed
    dplyr::filter(!dplyr::if_all(.cols = tidyselect::contains("nestling_ring"),
                                 .fns = is.na)) %>%
    dplyr::select("BroodID", "year":"nestbox", "Species", "ring_date":"tarsus_15") %>%
    #we need to give two records to chicks that were ringed and measured at different times
    tidyr::pivot_longer(cols = c("ring_date", "nestling_measure_date"),
                        names_to = "date_type", values_to = "CaptureDate") %>%
    #If measure date is NA, then there's only one capture and we can remove it
    dplyr::filter(.data$date_type == "ring_date" | (.data$date_type == "nestling_measure_date" & !is.na(.data$CaptureDate))) %>%
    #Identify every case where there were two captures
    dplyr::group_by(.data$BroodID) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = "nestling_ring_1":"nestling_ring_15",
                        names_to = "ChickNr", values_to = "IndvID") %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    tidyr::pivot_longer(cols = "mass_1":"mass_15",
                        names_to = "ChickNr_Mass", values_to = "Mass") %>%
    tidyr::pivot_longer(cols = "tarsus_1":"tarsus_15",
                        names_to = "ChickNr_Tarsus", values_to = "Tarsus") %>%
    #Convert to just have the number of the chick
    dplyr::mutate(dplyr::across(.cols = tidyselect::contains("ChickNr"),
                                .fns = ~{

                                  stringr::str_remove_all(.x, pattern = "[^0-9]")

                                })) %>%
    dplyr::filter(.data$ChickNr == .data$ChickNr_Mass,
                  .data$ChickNr == .data$ChickNr_Tarsus) %>%
    #When there is a ring and measure date, make tarsus and mass NA at the ring date
    dplyr::mutate(dplyr::across(.cols = c("Mass", "Tarsus"),
                                .fns = ~{

                                  dplyr::case_when(.data$n == 2 & .data$date_type == "ring_date" ~ NA_real_,
                                                   TRUE ~ as.numeric(.x)/10)

                                }),
                  CapturePopID = "PIL",
                  ReleasePopID = "PIL",
                  BreedingSeason = as.integer(.data$year),
                  CapturePlot = .data$plot,
                  ReleasePlot = .data$plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(.data$plot, .data$nestbox, sep = "_"),
                  Age_observed = 1L,
                  ObserverID = .data$nestling_measure_person,
                  Sex_observed = NA_character_,
                  IndvID = toupper(.data$IndvID))

  # Combine data
  Capture_data <- dplyr::bind_rows(female_capture_data,
                                   male_capture_data,
                                   chick_capture_data) %>%
    dplyr::mutate(CaptureTime = NA_character_,
                  WingLength = NA_real_,
                  OriginalTarsusMethod = NA_character_,
                  # We have no information on status of captures/releases,
                  # so we assume all individuals were captured/released alive
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%
    # Determine age at first capture for every individual
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    # Arrange by IndvID and CaptureDate and add unique CaptureID
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup()

  return(Capture_data)

}

#' Create individual table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create full individual data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param Capture_data Output of \code{\link{create_capture_PIL}}.
#' @param protocol_version Character string. The version of the standard protocol on which this pipeline is based.
#'
#' @return A data frame.

create_individual_PIL <- function(Capture_data, protocol_version){

  # Take capture data and determine summary data for each individual
  Indv_info <- Capture_data %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason,
                   .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = dplyr::case_when(length(unique(.data$Species)) == 2 ~ "CCCCCC",
                                                TRUE ~ dplyr::first(.data$Species)),
                     PopID = "PIL",
                     BroodIDLaid = dplyr::first(.data$BroodID),
                     BroodIDFledged = .data$BroodIDLaid,
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = ifelse(all(is.na(.data$Age_observed)), "adult", "chick")) %>%
    dplyr::arrange(.data$RingSeason, .data$IndvID)

  # Retrieve sex information
  Sex_calc <- Capture_data %>%
    dplyr::filter(!is.na(.data$Sex_observed)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$Sex_observed)),
                     unique_sex = list(unique(.data$Sex_observed))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                    TRUE ~ .data$unique_sex[[1]])) %>%
    dplyr::ungroup() %>%
    dplyr::select("IndvID", "Sex_calculated")

  Indv_data <- Indv_info %>%
    dplyr::left_join(Sex_calc,
                     by = "IndvID") %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Individual_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Individual_data))

  return(Indv_data)

}

#' Create location data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create location data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#' @param protocol_version Character string. The version of the standard protocol on which this pipeline is based.
#'
#' @return A data frame.

create_location_PIL <- function(PIL_data, protocol_version){

  Location_data <- tibble::tibble(LocationID = unique(PIL_data$LocationID),
                                  NestboxID = unique(PIL_data$LocationID),
                                  LocationType = "NB",
                                  PopID = "PIL",
                                  Latitude = NA_real_,
                                  Longitude = NA_real_,
                                  StartSeason = min(as.integer(PIL_data$year)),
                                  EndSeason = NA_integer_,
                                  HabitatType = "deciduous") %>%
    # Add missing columns
    dplyr::bind_cols(data_templates[[paste0("v", protocol_version)]]$Location_data[1, !(names(data_templates[[paste0("v", protocol_version)]]$Location_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format and order correctly
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Location_data))

  return(Location_data)

}
