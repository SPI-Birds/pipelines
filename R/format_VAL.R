#'Construct standard format for data from Valsaín, Spain.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Valsaín, Spain, administered by the National Museum of Natural Sciences.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Load data}: Data is in one excel spreadsheet with two sheets with brood info
#'and 15 sheets with chick info (one sheet per year since 2011). Number of sheets for chicks is changing every year
#'(one additional sheet each year), and we assume that the number of sheets for brood info will remain the same
#'#'but we need to check this with data owner.
#'
#'\strong{Plot}: All nestboxes within the true "VAL" population are assigned value "VAL". All other plots are surrounding
#'the historical VAL population
#'
#'\strong{LocationID}: LocationID is a concatenation of Plot and Nestbox number.
#'
#'\strong{BroodID}: BroodID is a concatenation of Year, LocationID and brood number (Year_LocationID_X).
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_VAL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  start_time <- Sys.time()

  message("Importing primary data...")

  #### Determine species and population codes for filtering
  if (is.null(species)) {
    species_filter <- NULL
  } else {
    species_filter <- species
  }

  if (is.null(pop)) {
    pop_filter <- NULL
  } else {
    pop_filter <- pop
  }

  ## Set options
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)

  #Load all data
  data_file    <- paste0(db, "/VAL_PrimaryData.xlsx")

  #Read in data

  #Need to know sheets to load chick data
  #Number of sheets will increase each year
  #Implement a function "read_and_clean_sheet" to solve the issue with missing columns in some sheets
  #Last sheet is the experiment sheet
  all_sheets      <- readxl::excel_sheets(data_file)
  chick_sheet_nrs <- which(stringr::str_detect(all_sheets, pattern = "Chicks"))
  chick_sheet_yrs <- as.integer(stringr::str_extract(all_sheets[chick_sheet_nrs], "[0-9]+"))
  early_broods    <- readxl::read_excel(data_file, sheet = 1, na = c("", "-"), ,
                                        col_types = c(rep("guess", 51), "text",
                                                      rep("guess", 6), "text",
                                                      rep("guess", 3), "text",
                                                      rep("guess", 17), "text",
                                                      rep("guess", 2), "text",
                                                      rep("guess", 23), "text",
                                                      rep("guess", 10))) %>%
    janitor::clean_names()
  late_broods     <- readxl::read_excel(data_file, sheet = 2, na = c("", "-")) %>%
    janitor::clean_names()
  chick_data      <- purrr::map2(.x = chick_sheet_nrs,
                                 .y = chick_sheet_yrs,
                                 .f = ~ read_and_clean_sheet(.x, .y, data_file)) %>%
    dplyr::bind_rows()


  experiment_data <- readxl::read_excel(data_file, sheet = length(all_sheets), na = c("", "-")) %>%
    janitor::clean_names() %>%
    dplyr::rename(ExperimentID = "spi_code")

  GPS_2015 <- sf::st_read(paste0(db, "/VAL_PrimaryData_GPS2015.gpx"), layer = "waypoints")
  GPS_2017 <- sf::st_read(paste0(db, "/VAL_PrimaryData_GPS2017.gpx"), layer = "waypoints")

  All_GPS <- dplyr::bind_rows(GPS_2015, GPS_2017) %>%
    dplyr::mutate(Plot = tidyr::replace_na(stringr::str_extract(.data$name, "[A-Z]"), replace = "VAL"),
                  NestboxID = paste0(.data$Plot, stringr::str_pad(stringr::str_extract(.data$name, "[0-9]+"), width = 3, pad = "0", side = "left")))


  # BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_VAL(early_broods, late_broods, chick_data, experiment_data)

  # CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_VAL(early_broods, late_broods, chick_data, experiment_data)

  # INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_VAL(Capture_data = Capture_data_temp)

  # LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_VAL(Brood_data = Brood_data_temp, All_GPS)


  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Brood_data))) %>%
    ## Add missing columns
    dplyr::bind_cols(data_templates[["v1.1.0"]]$Brood_data[0, !(names(data_templates[["v1.1.0"]]$Brood_data) %in% names(.))] %>%
                       tibble::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Brood_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(dplyr::if_all(
      c("BroodID", "PopID", "BreedingSeason", "Species"), ~ !is.na(.)
    ))


  ## Capture data
  Capture_data <- Capture_data_temp %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Capture_data))) %>%
    ## Add missing columns
    dplyr::bind_cols(data_templates[["v1.1.0"]]$Capture_data[0, !(names(data_templates[["v1.1.0"]]$Capture_data) %in% names(.))] %>%
                       tibble::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Capture_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(
      if_all(c(
        "CaptureID",
        "CapturePopID",
        "BreedingSeason",
        "IndvID",
        "Species",
        CaptureDate
      ), ~ !is.na(.))
    )


  ## Individual data
  Individual_data <- Individual_data_temp %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Individual_data))) %>%
    ## Add missing template columns
    {
      missing_cols <- setdiff(names(data_templates[["v1.1.0"]]$Individual_data), names(.))
      dplyr::mutate(., !!!setNames(rep(list(NA), length(missing_cols)), missing_cols))
    } %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Individual_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(dplyr::if_all(
      c("PopID", "IndvID", "Species", "RingSeason"), ~ !is.na(.)
    ))


  ## Location data
  Location_data <- Location_data_temp %>%
    ## Keep only template columns that exist
    dplyr::select(dplyr::any_of(names(data_templates[["v1.1.0"]]$Location_data))) %>%
    ## Add missing template columns
    {
      missing_cols <- setdiff(names(data_templates[["v1.1.0"]]$Location_data), names(.))
      dplyr::mutate(., !!!setNames(rep(list(NA), length(missing_cols)), missing_cols))
    } %>%
    ## Reorder and keep only template columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Location_data)) %>%
    dplyr::ungroup()


  ## Filter to keep only desired Species if specified for Brood, Capture, and Individual tables
  if (!is.null(species_filter)) {
    Brood_data <- Brood_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))
  }

  ## Filter to keep only desired Pops if specified for Brood, Capture, Individual, and Location tables
  if (!is.null(pop_filter)) {
    Brood_data <- Brood_data %>%
      dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

    Capture_data <- Capture_data %>%
      dplyr::filter(.data$CapturePopID %in% pop_filter & !(is.na(.data$CapturePopID)))

    Individual_data <- Individual_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

    Location_data <- Location_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))
  }

  #### EXPORT DATA

  if (output_type == "csv") {
    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BRG.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BRG.csv"), row.names = F)

    invisible(NULL)
  }

  if (output_type == "R") {
    message("Returning R objects...")

    return(list(
      Brood_data = Brood_data,
      Capture_data = Capture_data,
      Individual_data = Individual_data,
      Location_data = Location_data,
      protocol_version = protocol_version
    ))
  }
}


#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~

#' Create brood data table for Valsaín, Spain.
#'
#' Create brood data table in standard format for data from Valsaín, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 -)
#' @param chick_data Data frame with data on chick captures
#' @param experiment_data Data frame with data on experiments (1991-)
#'
#' @return A data frame.

create_brood_VAL <- function(early_broods, late_broods, chick_data, experiment_data){

  early_broods_format <- early_broods %>%
    dplyr::left_join(experiment_data %>%
                       dplyr::select("year", "ExperimentID"),
                     by = "year") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                  Plot = "VAL", #Nests with no letter prefix are given plot VAL corresponding to true Valsaín population
                  LocationID = paste0("VAL", stringr::str_pad(.data$nido, width = 3, pad = "0", side = "left")),
                  BroodID = paste(.data$LocationID, .data$year, sep = "_"),
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$MarchDay + floor(.data$ld),
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$cs, ## CS is before brood size manipulation.
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + .data$hdate,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = .data$hatchl,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = .data$fledgl,
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = .data$chicks_weight,
                  NumberChicksMass = dplyr::case_when(!is.na(.data$AvgChickMass) ~ .data$bsize13,
                                                      TRUE ~ NA_integer_),
                  AvgTarsus = .data$chicks_tarsus,
                  NumberChicksTarsus = dplyr::case_when(!is.na(.data$AvgTarsus) ~ .data$bsize13,
                                                        TRUE ~ NA_integer_),
                  OriginalTarsusMethod = "Alternative",
                  ExperimentID = dplyr::case_when(is.na(.data$manip) ~ NA_character_,
                                                  TRUE ~ .data$ExperimentID)) %>% #checked with data custodian, ignore manip values for years assigned without experiment
    dplyr::arrange(.data$FemaleID, .data$LayDate_observed) %>%
    # Recreate BroodID to account for multiple broods at the same location within the same year
    dplyr::group_by(.data$BroodID) %>%
    dplyr::mutate(BroodID = paste0(.data$BroodID, "_", 1:dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE)) %>%
    dplyr::select("PopID":"ClutchType_calculated", "ExperimentID")


  late_broods_format <- late_broods %>%
    dplyr::left_join(experiment_data %>%
                       dplyr::select("year", "ExperimentID"),
                     by = "year") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                  Plot = tidyr::replace_na(stringr::str_extract(.data$nest, "[A-Z]"), replace = "VAL"), #checked with data custodian, "VAL" plots are true Valsaín populations, others are nestboxes surrounding VAL pop
                  LocationID = paste0(.data$Plot, stringr::str_pad(stringr::str_extract(.data$nest, "[0-9]+"), width = 3, pad = "0", side = "left")),
                  BroodID = paste(.data$LocationID, .data$year, sep = "_"),
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = dplyr::case_when(stringr::str_detect(.data$nest, "bis") ~ "replacement",
                                                         TRUE ~ "first"), #Checked with data custodian, correct.
                  LayDate_observed = .data$MarchDay + floor(.data$ld),
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$cs,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + floor(.data$hd),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$cs * .data$hatching_suc/100),
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = as.integer(.data$BroodSize_observed * .data$fled_suc/100),
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = .data$chicks_weight,
                  NumberChicksMass = dplyr::case_when(!is.na(.data$AvgChickMass) ~ .data$bsd13,
                                                      TRUE ~ NA_integer_),
                  AvgTarsus = .data$chicks_tarsus,
                  NumberChicksTarsus = dplyr::case_when(!is.na(.data$AvgTarsus) ~ .data$bsd13,
                                                      TRUE ~ NA_integer_),
                                    OriginalTarsusMethod = "Alternative",
                  ExperimentID = dplyr::case_when(is.na(.data$treatment) ~ NA_character_,
                                                  TRUE ~ .data$ExperimentID)) %>%

    # Recreate BroodID to account for multiple broods at the same location within the same year
    dplyr::arrange(.data$FemaleID, as.Date(.data$LayDate_observed, format = "%Y-%m-%d")) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::mutate(BroodID = paste0(.data$BroodID, "_", 1:dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE)) %>%
    dplyr::select("PopID":"ClutchType_calculated",
                  "ExperimentID")



  all_broods <- dplyr::bind_rows(early_broods_format, late_broods_format) %>%
    dplyr::mutate(dplyr::across(c(BreedingSeason, ClutchSize_observed,
                                  BroodSize_observed, NumberFledged_observed,
                                  NumberChicksTarsus, NumberChicksMass), ~ as.integer(.))) %>%
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID)

  return(all_broods)

}

#' Create capture table for Valsaín, Spain.
#'
#' Create full capture data table in standard format for data from Valsaín, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 - ongoing)
#' @param chick_data Data frame with data on chick captures
#' @param experiment_data Data frame with data on experiments (1991-)
#'
#' @return A data frame.

create_capture_VAL <- function(early_broods, late_broods, chick_data, experiment_data){

  #Extract info on adult captures
  early_adult_captures <- early_broods %>%
    dplyr::left_join(experiment_data %>%
                       dplyr::filter(.data$ExperimentID == "SURVIVAL" &
                                       stringr::str_detect(description, "adults|prolactin|Female handicap")) %>% #detailed with data custodian
                       dplyr::select("year", "ExperimentID"),
                     by = "year") %>%
    dplyr::select("year", "nido", "female", "ano_anilla_52", "fage", "surv_fem", "fcapture", "ftarsus", "fwing", "fweight",
                  "male", "ano_anilla_84", "mage", "surv_man", "mcapture", "mtarsus", "mwing", "mweight", "manip", "ExperimentID") %>%
    tidyr::pivot_longer(cols = c(female, male), names_to = "Sex_observed", values_to = "IndvID") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  Species = species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                  BreedingSeason = .data$year,
                  Sex_observed = dplyr::case_when(.data$Sex_observed == "female" ~ "F",
                                                  .data$Sex_observed == "male" ~ "M"),
                  CaptureDate = dplyr::case_when(Sex_observed == "F" ~ .data$MarchDay + tidyr::replace_na(.data$fcapture, 0),
                                                 Sex_observed == "M" ~ .data$MarchDay + tidyr::replace_na(.data$mcapture, 0)),
                  CaptureTime = NA_character_,
                  ObserverID = NA_character_,
                  LocationID = paste0("VAL", stringr::str_pad(.data$nido, width = 3, pad = "0", side = "left")),
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = "VAL",
                  ReleasePopID = "VAL", ReleasePlot = "VAL",
                  Mass = dplyr::case_when(.data$Sex_observed == "F" ~ .data$fweight,
                                          .data$Sex_observed == "M" ~ .data$mweight),
                  Tarsus = dplyr::case_when(.data$Sex_observed == "F" ~ round(.data$ftarsus, digits = 2),
                                            .data$Sex_observed == "M" ~ round(.data$mtarsus, digits = 2)),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = dplyr::case_when(.data$Sex_observed == "F" ~ .data$fwing,
                                                .data$Sex_observed == "M" ~ .data$mwing),
                  Age_observed = dplyr::case_when(.data$Sex_observed == "F" ~ as.integer(5 + (.data$fage - 1)*2),
                                                  .data$Sex_observed == "M" ~ as.integer(5 + (.data$mage - 1)*2)), ## This is real age (i.e. 1 is known to be born previous year = 5)
                  ChickAge = NA_integer_,
                  ExperimentID = dplyr::case_when(is.na(.data$manip) ~ NA_character_,
                                                  TRUE ~ .data$ExperimentID)) %>%
    dplyr::select("IndvID", "Species", "Sex_observed", "BreedingSeason":"ChickAge", "ExperimentID")

  late_adult_captures <- late_broods %>%
    dplyr::left_join(experiment_data %>%
                       dplyr::filter(.data$ExperimentID == "SURVIVAL"  &
                                       stringr::str_detect(description, "adults|prolactin|Female handicap")) %>% #detailed with data custodian)
                       dplyr::select("year", "ExperimentID"),
                     by = "year") %>%
    dplyr::select("year", "nest", "female", "f_age", "f_tarsus", "f_wing", "f_weight", "obs_24",
                  "male", "m_age", "m_tarsus", "m_wing", "m_weight", "obs_38", "hd", "treatment", "ExperimentID") %>%
    tidyr::pivot_longer(cols = c(female, male), names_to = "Sex_observed", values_to = "IndvID") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  Species = species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                  BreedingSeason = .data$year,
                  Sex_observed = dplyr::case_when(.data$Sex_observed == "female" ~ "F",
                                                  .data$Sex_observed == "male" ~ "M"),
                  CaptureDate = .data$MarchDay + .data$hd + 7, ## Adults were captured 7 days after hatching
                  CaptureTime = NA_character_,
                  ObserverID = dplyr::case_when(.data$Sex_observed == "F" ~ .data$obs_24,
                                                .data$Sex_observed == "M" ~ .data$obs_38),
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = tidyr::replace_na(stringr::str_extract(.data$nest, "[A-Z]"), replace = "VAL"),
                  ReleasePopID = "VAL", ReleasePlot = .data$CapturePlot,
                  LocationID = paste0(.data$CapturePlot, stringr::str_pad(stringr::str_extract(.data$nest, "[0-9]+"), width = 3, pad = "0", side = "left")),
                  Mass = dplyr::case_when(.data$Sex_observed == "F" ~ .data$f_weight,
                                          .data$Sex_observed == "M" ~ .data$m_weight),
                  Tarsus = dplyr::case_when(.data$Sex_observed == "F" ~ round(.data$f_tarsus, digits = 2),
                                            .data$Sex_observed == "M" ~ round(.data$m_tarsus, digits = 2)),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = dplyr::case_when(.data$Sex_observed == "F" ~ .data$f_wing,
                                                .data$Sex_observed == "M" ~ .data$m_wing),
                  Age_observed = dplyr::case_when(.data$Sex_observed == "F" ~ as.integer(4 + (.data$f_age * 2)),
                                                  .data$Sex_observed == "M" ~ as.integer(4 + (.data$m_age * 2))),

                  ChickAge = NA_integer_,
                  ExperimentID = dplyr::case_when(is.na(.data$treatment) ~ NA_character_,
                                                  TRUE ~ .data$ExperimentID)) %>%
    dplyr::select("IndvID",
                  "Species",
                  "Sex_observed",
                  "BreedingSeason":"ChickAge",
                  "ExperimentID")

  #No information on chick rings before 2011. This data is not digitised.

  early_chick <- chick_data %>%
    #Combining information from late_broods with information from early_chick to get hd information
    dplyr::left_join(late_broods %>%
                       dplyr::select("nest", "year", "hd"),
                     by = c("year", "NestboxID" = "nest")) %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  IndvID = .data$anilla,
                  Species = species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                  Sex_observed = NA_character_,
                  BreedingSeason = as.integer(.data$year),
                  CaptureDate = .data$MarchDay + .data$hd + 12, ## Chicks were captured 12 days after hatching
                  CaptureTime = dplyr::case_when(.data$hora == "0" ~ NA_character_,
                                                 TRUE ~ suppressWarnings(format(as.POSIXct(Sys.Date() + as.numeric(.data$hora)), "%H:%M", tz="UTC"))),
                  ObserverID = NA_character_,
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = tidyr::replace_na(stringr::str_extract(.data$NestboxID, "[A-Z]"), replace = "VAL"),
                  ReleasePopID = "VAL", ReleasePlot = .data$CapturePlot,
                  LocationID = paste0(.data$CapturePlot, stringr::str_pad(stringr::str_extract(.data$NestboxID, "[0-9]+"), width = 3, pad = "0", side = "left")),
                  Mass = suppressWarnings(round(as.numeric(.data$peso), digits = 2)),
                  Tarsus = suppressWarnings(round(as.numeric(.data$tarso), digits = 2)),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = as.numeric(.data$ala),
                  Age_observed = 1L, #All chicks are caught pre-fledgling (i.e. in nest)
                  ChickAge = 13L, #Chicks are caught and ringed at 13 days
                  ExperimentID = NA_character_) %>%
    dplyr::select("IndvID":"ExperimentID")

  all_captures <- dplyr::bind_rows(early_adult_captures, late_adult_captures, early_chick) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, as.Date(.data$CaptureDate, format = "%Y-%m-%d")) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::mutate(CaptureID = paste(IndvID, 1:dplyr::n(), sep = "_"),
                  BreedingSeason = as.integer(.data$BreedingSeason)) %>%
    dplyr::select("CaptureID", "IndvID":"Age_observed", "Age_calculated", "ChickAge", "ExperimentID")

  return(all_captures)

}

#' Create individual table for Valsaín, Spain.
#'
#' Create full individual data table in standard format for data from Valsaín, Spain.
#'
#' @param Capture_data_temp Output of \code{\link{create_capture_VAL}}.
#'
#' @return A data frame.

create_individual_VAL <- function(Capture_data_temp){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data_temp %>%
    dplyr::mutate(BroodID = paste(.data$LocationID, .data$BreedingSeason, sep = "_")) %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = first(.data$Species),
                     PopID = "VAL",
                     BroodIDLaid = first(BroodID),
                     BroodIDFledged = BroodIDLaid,
                     RingSeason = first(BreedingSeason),
                     RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
                     Sex_genetic = NA_character_,
                     Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex_observed))), .f = ~{

                       if(length(..1) == 0){

                         return(NA_character_)

                       } else if(length(..1) == 1){

                         return(..1)

                       } else {

                         return("C")

                       }

                     }), .groups = "drop") %>%
    dplyr::rowwise() %>%
    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
                  BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

}

#' Create location table for Valsein, Spain.
#'
#' Create full location data table in standard format for data from Valsein, Spain.
#'
#' @param Brood_data Output of \code{\link{create_brood_VAL}}.
#' @param GPS GPS data file.
#'
#' @return A data frame.
#' @export

create_location_VAL <- function(Brood_data_temp, All_GPS){

  #Extract latitude and longitude from gps file
  GPS <- All_GPS %>%
    dplyr::bind_cols(tibble::as_tibble(sf::st_coordinates(.))) %>%
    dplyr::rename(Longitude = "X", Latitude = "Y") %>%
    dplyr::select("NestboxID", "Longitude", "Latitude") %>%
    sf::st_drop_geometry() %>%
    #Where there are multiple records from the same box, just take the first one
    #There is no box movement so these are duplicate
    dplyr::group_by(NestboxID) %>%
    dplyr::distinct(NestboxID, .keep_all = TRUE)


  Location_data <- Brood_data_temp %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestboxID = dplyr::first(.data$LocationID),
                  LocationType = "NB",
                  PopID = "VAL",
                  StartSeason = as.integer(min(.data$BreedingSeason)),
                  EndSeason = NA_integer_, # Boxes are not removed. Some boxes may be used by BT/GT so aren't found here. GT/BT data is collected by other researcher. Alex will contact/give me his contact.
                  HabitatType = "deciduous",
                  .groups = "drop") %>%
    dplyr::left_join(GPS, by = "NestboxID") %>%
    dplyr::select("LocationID":"PopID", "Latitude", "Longitude", everything())

  return(Location_data)

}


#'Function to extract chick sheet despite missing columns (date column)
#'
read_and_clean_sheet <- function(sheet_nr, sheet_yr, data_file) {
  data <- readxl::read_excel(data_file, sheet = sheet_nr, col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(.data$anilla))

  # Check if column "fecha" exist
  if ("fecha" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(
        Date = janitor::excel_numeric_to_date(as.numeric(.data$fecha))
      )
  } else {
    data$Date <- NA
  }

  data %>%
    dplyr::mutate(
      year = sheet_yr,
      NestboxID = .data$nido
    ) %>%
    tidyr::fill(NestboxID, .direction = "down")

}
