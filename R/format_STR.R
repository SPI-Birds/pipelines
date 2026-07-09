#' Construct standard format for data from Strasbourg, France
#'
#' A pipeline to produce the standard format for the hole nesting bird populations in North-Eastern France in and around Strasbourg
#' (Strasbourg, Roberstau, Wantzenau) administered by Sylvie Massemin and Josefa Bleu
#' (Institut Pluridisciplinaire Hubert Curien - CNRS UMR 7178 & Université de Strasbourg).
#'
#' This pipeline is built using SPI-Birds' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{standard format v1.1.0}.
#'
#' This section provides details on data management choices that are unique to these data.
#'
#' \strong{age_calculated}: based on age when tagged. Individuals tagged as "PUL" were assigned "chick". Others were assigned "adult"
#'
#' \strong{ChickAge}: all chicks with measurements were assumed to be 15 days old.
#'
#' \strong{IndvID}: individuals are banded with metal ring with 7 or 8 digits (adults for both species, chicks before 2022) or a "V" followed by 6 digits (for chicks starting 2022)
#'
#' \strong{brood data}: Empty nestboxes or nestboxes occupied by other species ("MAMM") are removed
#'
#' \strong{ClutchType_observed}: Only classified as "1" or "2" in the original dataset. Most likely correspond to breeding attempt ("1" or "2") in the associated nestbox
#' (regardless of female ID or first egg laid on the site)
#'
#'
#' \strong{HabitatType}: roughly defined based on data information "deciduous" for forest population (WAN), "urban" for downtown population (STR) and suburban population (ROB)
#'#'
#' \strong{NestboxID}: generate a second row for the same nest box when there was a gap in monitoring nest box (e.g. if nest box was monitored since 2014, but not
#' monitored in 2019, the first row indicates StartSeason as 2014 and EndSeason as 2018; the second row indicates StartSeason as 2020 and EndSeason as NA)
#'
#'
#' \strong{ExperimentID}: accidental events which may affect breeding attempt are reported as "OTHER"
#'
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 4 .csv files or 4 data frames in the standard format (v1.1.0).
#' @export

format_STR <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       optional_variables = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  start_time <- Sys.time()

  message("Importing primary data...")

  #### Determine species and population codes for filtering
  if(is.null(species)){

    species_filter <- NULL

  } else {

    species_filter <- species

  }

  if(is.null(pop)){

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

  ## Set options
  original_options <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(original_options), add = TRUE, after = FALSE)


  ## Read in nest data
  nest_data <- readxl::read_xlsx(path = paste0(db, "/STR_PrimaryData_Brood.xlsx"),
                                 guess_max = 5000,
                                 col_types = "text") %>%
    janitor::clean_names() %>%
    janitor::remove_empty(which = "rows") %>%

    # Remove rows from empty nest boxes across the season or used by other species than birds (ants, hornets)
    dplyr::filter(!(is.na(.data$espece)  | espece == "MAMM")) %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., ".")),
                  PopID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                           .data$zone == "Foret" ~ "WAN",
                                           TRUE ~ "ROB"),
                  BreedingSeason = as.integer(.data$saison),
                  Species = dplyr::case_when(.data$espece == "PARCAE" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
                                             .data$espece == "PARMAJ" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
                                             TRUE ~ NA_character_), #remove very specific case (unfamiliar species, mixed-brood instances, or error)
                  Plot = toupper(.data$site),
                  nestID = stringr::str_extract(.data$id_nichee, "(?<=^[A-Z]{4}).*(?=_[0-9]{4}_[12]$)"), #regular expression to extract nestbox ID exactly as mentioned in id_nichee (slightly more reliable and corresponding to info in capture data)
                  LocationID = paste(toupper(.data$site), .data$nestID, "NB", sep = "_"),
                  LayDate_observed = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_ponte)), format = "%Y-%m-%d")),
                  HatchDate_observed = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_eclosion)), format = "%Y-%m-%d")),
                  FledgeDate_observed = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_envol)), format = "%Y-%m-%d")),
                  ClutchSize_observed = suppressWarnings(as.integer(.data$tp)),
                  BroodSize_observed = suppressWarnings(as.integer(.data$p_eclos)),
                  NumberFledged_observed = suppressWarnings(as.integer(.data$p_envol)),
                  MaleID = .data$id_male,
                  FemaleID = .data$id_femelle,
                  ClutchType_observed = dplyr::case_when(.data$ponte == "1" ~ "first",
                                                         .data$ponte == "2" ~ "second",
                                                         TRUE ~ "replacement"),
                  ExperimentID = dplyr::case_when(stringr::str_detect(.data$remarques,
                                                                      "probleme nichoir|probleme_nichoir|mort pdt la capture|
                                                                      Comportement poussins|Vitamines E|thermic stress") ~ "OTHER",
                                                  stringr::str_detect(.data$remarques, "Manip IMMUNO") ~ "SURVIVAL",
                                                  TRUE ~ NA_character_)) %>%
    dplyr::group_by(.data$BreedingSeason, .data$LocationID) %>%
    ## Create a unique identifier for breeding event (independently of the one provided by the data owner) for multiple broods within a year
    dplyr::mutate(broodID2 = dplyr::case_when(dplyr::n() > 1 & .data$ClutchType_observed == "second" ~ paste(id_nichoir, BreedingSeason,  "2", sep = "_"),
                                              TRUE ~ paste(id_nichoir, BreedingSeason, "1", sep = "_"))) %>%
    ## Remove potential duplicates
    dplyr::distinct(.data$broodID2, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID, .data$broodID2)





  ## Read in capture data

  capture_data <- readxl::read_xlsx(path = paste0(db, "/STR_PrimaryData_Capture.xlsx"),
                                    guess_max = 5000,
                                    col_types = "text") %>%
    janitor::clean_names() %>%
    janitor::remove_empty(which = "rows") %>%

    ## Rename and process columns
    dplyr::mutate(dplyr::across(where(is.character),
                                ~dplyr::na_if(., "NA")),
                  PopID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                             .data$zone == "Foret" ~ "WAN",
                                             TRUE ~ "ROB"),
                  Species =  dplyr::case_when(.data$espece == "PARCAE" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
                                              .data$espece == "PARMAJ" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
                                              TRUE ~ NA_character_),
                  CaptureDate = suppressWarnings(dplyr::case_when(stringr::str_detect(.data$date, "/") ~ as.Date(.data$date, format = "%d/%m/%Y"),
                                                                  TRUE ~ as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date)), format = "%Y-%m-%d"))),
                  BreedingSeason = lubridate::year(.data$CaptureDate),
                  CaptureTime = dplyr::case_when(stringr::str_detect(.data$heure, "^[[:digit:]]{2}[:][[:digit:]]{2}[::][[:digit:]]{2}$") ~ format(as.POSIXct(.data$heure, format = "%H:%M:%OS"), format = "%H:%M"),
                                                 TRUE ~ suppressWarnings(format(as.POSIXct(as.numeric(.data$heure) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"))), #timezone is set to Paris time zone for summer time (CEST)
                  IndvID = .data$bague,
                  Plot = toupper(.data$site),
                  captureType = dplyr::case_when(.data$type_capture == "AU NID" ~ "nestbox",
                                                 .data$type_capture == "CAGE-PIEGE" ~ "clap-net",
                                                 TRUE ~ "mist-net"),
                  LocationID = dplyr::case_when(.data$type_capture == "AU NID" ~ paste(toupper(.data$site), tolower(.data$nichoir), "NB", sep = "_"),
                                                TRUE ~ paste(toupper(.data$site), "MN", sep = "_")),
                  Tarsus = suppressWarnings(round(as.numeric(.data$lt), 2)),
                  WingLength = suppressWarnings(round(as.numeric(.data$lp), 1)),
                  Mass = suppressWarnings(round(as.numeric(.data$ma), 1)),
                  # Age = dplyr::case_when(.data$age == "PUL" ~ "chick",
                  #                          .data$age == "2A" ~ "juv",
                  #                          .data$age == "+2A" ~ "ad",
                  #                          TRUE ~ NA_character_),
                  Age_observed = dplyr::case_when(.data$age == "PUL" ~ 1L,
                                                  .data$age == "1A" ~ 3L,
                                                  .data$age == "2A" ~ 5L,
                                                  .data$age == "+2A" ~ 6L,
                                                  TRUE ~ 4L),
                  #Clarifying age in chicks
                  #Some chicks are handled twice (some are banded before age 15 but they should all be measured only at age 15)
                  ChickAge = dplyr::case_when(.data$age == "PUL" & !is.na(.data$ma) ~ 15L,
                                              TRUE ~ NA_integer_),
                  Sex_observed = dplyr::case_when(.data$sexe == "?" ~ NA_character_,
                                                 is.na(.data$sexe) ~ NA_character_,
                                                 TRUE ~ sexe),
                  CaptureAlive = dplyr::case_when(.data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),
                  ReleaseAlive = dplyr::case_when(.data$es == "MORT" ~ FALSE,
                                                  .data$action == "Reprise" ~ FALSE,
                                                  TRUE ~ TRUE),
                  OriginalTarsusMethod = "Alternative")  %>%
    dplyr::group_by(.data$bg) %>%
    # Anonymize observers
    dplyr::mutate(recordedBy = paste0("obs_", dplyr::cur_group_id())) %>%
    dplyr::ungroup()

  ## Steps to correctly assign captured chicks to their brood

  #Step 1. merging information from capture_data and nest_data (broadly)
  broodAssignment <- capture_data %>%
    dplyr::filter(.data$age == "PUL") %>%
    dplyr::select("IndvID", "LocationID", "BreedingSeason", "CaptureDate") %>%
    dplyr::left_join(nest_data %>%
                       dplyr::mutate(bandingDate = suppressWarnings(as.Date(janitor::excel_numeric_to_date(as.numeric(.data$date_baguage_p)),
                                                                            format = "%Y-%m-%d"))) %>%
                       dplyr::select("broodID2", "bandingDate", "HatchDate_observed", "LocationID", "BreedingSeason"),
                     by = c("LocationID", "BreedingSeason"),
                     relationship = "many-to-many") %>%

    #Step 2. targetting the right brood within a locationID and Year
    #         (comparing hatching date or banding date from brood_data to captureDate in capture_data)
    dplyr::mutate(diff_hatch = as.numeric(.data$CaptureDate - .data$HatchDate_observed), #estimate number of days between hatch date and capture date
                  diff_band  = as.numeric(.data$CaptureDate - .data$bandingDate), #estimate number of days between banding date and capture date
                  #Create a time reference (based on number of days since hatching when hatch date is available, or number of days since banding when hatch is not available)
                  diff_ref = dplyr::case_when(!is.na(.data$HatchDate_observed) ~ .data$diff_hatch,
                                              is.na(.data$HatchDate_observed) & !is.na(.data$bandingDate) ~ .data$diff_band,
                                              TRUE ~ NA_real_),
                  #Establish a rule based on time reference (that should stay close in days from hatch or banding date)
                  diff_rule = dplyr::case_when(!is.na(.data$HatchDate_observed) ~ .data$diff_ref >= 0 & .data$diff_ref <= 25,
                                               is.na(.data$HatchDate_observed) ~ abs(.data$diff_ref) <= 15,
                                               TRUE ~ FALSE)) %>%

    #Step 3. ensuring each chick is assigned to the right brood
    dplyr::filter(.data$diff_rule) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::slice_min(abs(.data$diff_ref), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()


  # Read in nestbox data

  loc_data <- readr::read_delim(paste0(db, "/STR_PrimaryData_Location.csv"), show_col_types = FALSE) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(LocationID = paste(toupper(.data$site), tolower(.data$nichoir), sep = "_")) %>%
    dplyr::mutate(PopID = dplyr::case_when(.data$zone == "CV" ~ "STR",
                                             .data$zone == "Foret" ~ "WAN",
                                             TRUE ~ "ROB"),

                  HabitatType = dplyr::case_when(.data$zone == "CV" ~ "urban", #Check with data custodians for more details
                                               .data$zone == "Foret" ~ "deciduous",
                                               TRUE ~ "urban"),
                  Latitude = .data$latitude,
                  Longitude = .data$longitude,
                  LocationType = "NB",
                  StartSeason = dplyr::case_when(!is.na(.data$date_pose) ~ as.integer(.data$date_pose),
                                               TRUE ~ NA_integer_),
                  EndSeason = dplyr::case_when(!is.na(.data$date_retrait) ~ as.integer(.data$date_retrait),
                                             .data$remarques == "Non suivi depuis 2023" ~ 2022, #check with data custodians if this works
                                             TRUE ~ NA_integer_),
                  locationDetails = dplyr::case_when(.data$type == "Schwegler" ~ "Schwegler nesting box",
                                                     .data$type == "Bois" ~ "Wooden nesting box",
                                                     .data$type == "Balcon" ~ "Balcony nesting box",
                                                     TRUE ~ NA_character_),
                  # Add a column to detect rows that need to be duplicated to add information about changing nest box type or monitoring gaps
                  count = dplyr::case_when(stringr::str_detect(.data$remarques, "non suivi 2|bois jusqu|Balcon jusque") ~ 2,
                                           TRUE ~ 1)) %>%
    # Duplicate rows (based on count)
    tidyr::uncount(.data$count) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID) %>%
    # ... Add number 2 for duplicated rows (used afterwards to filter the right row)
    dplyr::mutate(count = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Change information on location type when nest box type was changed
    dplyr::mutate(# Adjust StartSeason and EndSeason for case with monitoring gaps ("non suivi") or changes in nest box type ("bois jusqu'", "Balcon jusque")
      EndSeason = dplyr::case_when(count == 1 & stringr::str_detect(.data$remarques, "non suivi 2020|non suivi 2020, 2021|non suivi 2020,2021|bois jusqu'en 2020") ~ 2019,
                                   count == 1 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021") ~ 2018,
                                   count == 1 & stringr::str_detect(.data$remarques, "non suivi 2021") ~ 2020,
                                   count == 1 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2018,
                                   count == 1 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2022,
                                   TRUE ~ EndSeason),
      StartSeason = dplyr::case_when(count == 2 & stringr::str_detect(.data$remarques, "non suivi 2019,2020,2021|non suivi 2020, 2021|non suivi 2021") ~ 2022,
                                     count == 2 & stringr::str_detect(.data$remarques, "non suivi 2020") ~ 2021,
                                     count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2020") ~ 2021,
                                     count == 2 & stringr::str_detect(.data$remarques, "bois jusqu'en 2018") ~ 2019,
                                     count == 2 & stringr::str_detect(.data$remarques, "Balcon jusque 2023") ~ 2023,
                                     TRUE ~ StartSeason)) %>%
    # For each location ID
    dplyr::group_by(.data$LocationID) %>%
    # ... create a location ID
    dplyr::mutate(NestboxID = paste(.data$LocationID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("LocationID", "NestboxID", "PopID", "Latitude", "Longitude", "StartSeason", "EndSeason", "HabitatType", "LocationType")



  #### BROOD DATA
  message("Compiling brood information...")
  Brood_data_temp <- create_brood_STR(nest_data, capture_data, broodAssignment)

  #### CAPTURE DATA
  message("Compiling capture information...")
  Capture_data_temp <- create_capture_STR(capture_data)

  #### INDIVIDUAL DATA
  message("Compiling individual information...")
  Individual_data_temp <- create_individual_STR(Capture_data_temp, Brood_data_temp, broodAssignment)

  #### LOCATION DATA
  message("Compiling location information...")
  Location_data_temp <- create_location_STR(Capture_data_temp, loc_data)

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

  # ## Check column classes
  # purrr::map_df(data_templates$1.1.0$Brood_data, class) == purrr::map_df(Brood_data, class)


  ## Capture data
  Capture_data <- Capture_data_temp %>%
    dplyr::mutate(BreedingSeason = as.integer(.data$BreedingSeason)) %>%
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
        "CaptureDate"
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
    nest_data <- nest_data %>%
      dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_STR.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_STR.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_STR.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_STR.csv"), row.names = F)

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

#' Create brood data table in Strasbourg, France.
#'
#' @param nest_data Data frame of nest data from Strasbourg, France.
#'
#'
#' @return A data frame.
#'
create_brood_STR <- function(nest_data, capture_data, broodAssignment) {

  ## Create a temporary dataframe with measurement information for chicks + broodID
  Chick_temp <- capture_data %>%
    #Remove adults and chick data without measurement
    dplyr::filter(.data$age == "PUL" & !is.na(.data$Mass)) %>%
    dplyr::select("IndvID", "Tarsus", "Mass") %>%
    #integrate broodID information to anticipate merging with nest_data
    dplyr::left_join(
      broodAssignment %>%
        dplyr::select("IndvID", "broodID2"),
      by = "IndvID",
      relationship = "many-to-one") %>%
    #calculate average chick mass and average chick tarsus for each brood
    dplyr::group_by(.data$broodID2) %>%
    dplyr::summarise(
      AvgChickMass = mean(.data$Mass, na.rm = T),
      NumberChicksMass = sum(!is.na(.data$Mass)),
      AvgChickTarsus = dplyr::case_when(sum(!is.na(.data$Tarsus)) > 0 ~ mean(.data$Tarsus, na.rm = TRUE),
                                        TRUE ~ NA_real_),
      NumberChickTarsus = dplyr::case_when(sum(!is.na(.data$Tarsus)) > 0 ~ sum(!is.na(.data$Tarsus)),
                                           TRUE ~ NA_integer_))


  ## Combine primary data to create brood data
  Brood_data_temp <- nest_data %>%
    dplyr::left_join(Chick_temp,
                     by = "broodID2",
                     relationship = "one-to-one") %>%

    ##Remove non-occupied nestboxes or unidentified species
    dplyr::filter(!is.na(.data$Species)) %>%

    ## Create additional variables
    dplyr::mutate(BroodID = paste(.data$BreedingSeason, 1:dplyr::n(), sep = "-")) %>%

    ##

    ## Set improperly formatted IDs to NA
    dplyr::mutate(dplyr::across(c("FemaleID",
                                  "MaleID"),
                                ~ dplyr::case_when(nchar(.) %in% c(7,8) & stringr::str_detect(., "^(V|[0-9])+[:digit:]+$") ~ .,
                                                   TRUE ~ NA_character_))) %>%

    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE)) %>%

    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Brood_data)), dplyr::everything())

  return(Brood_data_temp)

}



#' Create capture data table for Strasbourg, France.
#'
#' @param capture_data, Data frame of individuals (adults and nestlings) ringing records from Strasbourg, France.
#'
#' @return A data frame.

create_capture_STR <- function(capture_data) {


  Capture_data_temp <- capture_data %>%
    ## Create new columns
    dplyr::mutate(
      CapturePopID = .data$PopID,
      ReleasePopID = .data$PopID,
      CapturePlot = .data$Plot,
      ReleasePlot = .data$Plot
    ) %>%
    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    ## Calculate age
    dplyr::group_by(.data$IndvID) %>%
    calc_age(
      ID = .data$IndvID,
      Age = .data$Age_observed,
      Date = .data$CaptureDate,
      Year = .data$BreedingSeason
    ) %>%
    ## Create CaptureID
    ## Arrange
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, as.Date(.data$CaptureDate, format = "%Y-%m-%d")) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    ## Set improperly formatted IDs to NA
    dplyr::mutate(IndvID = dplyr::case_when(nchar(.data$IndvID) %in% c(6,7,8) & stringr::str_detect(.data$IndvID, "^(V|[0-9])+[:digit:]+$") ~ .data$IndvID,
                                            TRUE ~ NA_character_)) %>%
    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Capture_data)), dplyr::everything())


  return(Capture_data_temp)
}





#' Create individual table for Strasbourg, France.
#'
#' @param Capture_data_temp  Capture data output from Strasbourg, France
#'
#' @param Brood_data_temp Brood data output from Strasbourg, France
#'
#' @return A data frame.

create_individual_STR <- function(Capture_data_temp,
                                  Brood_data_temp, broodAssignment){

  Individual_data_temp <- Capture_data_temp %>%
    #### Format and create new data columns
    dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
    dplyr::mutate(PopID = .data$CapturePopID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(RingSeason = as.integer(min(.data$BreedingSeason, na.rm = TRUE))) %>%
    ## Arrange
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    ## Determine individual info
    dplyr::mutate(
      Sex_calculated = purrr::map_chr(
        .x = list(unique(stats::na.omit(.data$Sex_observed))),
        .f = ~ {
          if (length(..1) == 0) {
            return(NA_character_)
          } else if (length(..1) == 1) {
            return(..1)
          } else {
            return("C")
          }
        }
      ),
      Sex_genetic = NA_character_,
      Species = purrr::map_chr(
        .x = list(unique(stats::na.omit(.data$Species))),
        .f = ~ {
          if (length(..1) == 0) {
            return(NA_character_)
          } else if (length(..1) == 1) {
            return(..1)
          } else {
            return("CCCCCC")
          }
        }
      ),
      RingAge = purrr::pmap_chr(
        .l = list(dplyr::first(.data$Age_observed)),
        .f = ~ {
          if (is.na(..1)) {
            return("adult")
          } else if (..1 <= 3L) {
            return("chick")
          } else if (..1 > 3L) {
            return("adult")
          }
        }
      )
    ) %>%

    # ## Join Brood data for Individuals banded as chicks
    # dplyr::mutate(brood_record = dplyr::case_when(
    #   .data$RingAge == "chick" &
    #     .data$RingSeason == .data$BreedingSeason &
    #     !is.na(.data$LocationID) ~ "yes",
    #   .default = NA_character_
    # )) %>%

    ## Retrieve information on broodID (broodID2 here) from broodAssignment
    dplyr::left_join(
      broodAssignment %>%
        dplyr::select("IndvID", "broodID2"),
      by = "IndvID",
      relationship = "many-to-one") %>%

    ## Retrieve BroodID from Brood_data_temp
    dplyr::left_join(
      Brood_data_temp %>%
        dplyr::select("broodID2", "BroodID"),
      by = "broodID2",
      relationship = "many-to-one") %>%

    ## Add BroodID information
    ## Only one unique (non NA) BroodID per individual
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(
      BroodIDLaid = purrr::map_chr(
        .x = list(unique(stats::na.omit(.data$BroodID))),
        .f = ~ {
          if (length(..1) != 1) {
            return(NA_character_)
          } else if (length(..1) == 1) {
            return(..1)
          }
        }
      ),
      BroodIDFledged = .data$BroodIDLaid
    ) %>%
    ## Keep distinct records by PopID and InvdID
    dplyr::distinct(PopID, IndvID, .keep_all = TRUE) %>%
    ## Arrange
    dplyr::arrange(.data$CaptureID) %>%
    dplyr::ungroup() %>%
    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Individual_data)), dplyr::everything())


  return(Individual_data_temp)
}






#' Create location data table for Strasbourg, France.
#'
#' @param loc_data Data frame of nestbox location records from Strasbourg, France.
#'
#' @param Capture_data_temp Capture data output from Strasbourg, France
#'
#' @return A data frame.


create_location_STR <- function(Capture_data_temp,
                                loc_data) {

  ## Create table with information related to mistnet captures
  loc_mn <- Capture_data_temp %>%
    dplyr::filter(.data$captureType != "nestbox") %>%
    dplyr::arrange(.data$Plot, .data$BreedingSeason) %>%
    dplyr::group_by(.data$Plot) %>%
    dplyr::mutate(LocationID = paste(.data$Plot, "MN", sep = "_"),
                  StartSeason = first(.data$BreedingSeason),
                  EndSeason = last(.data$BreedingSeason),
                  LocationType = "MN",
                  HabitatType = dplyr::case_when(.data$PopID == "STR" ~ "urban",
                                                 .data$PopID == "WAN" ~ "deciduous",
                                                 TRUE ~ "urban"),
                  Latitude = NA_real_,
                  Longiture = NA_real_) %>%
    dplyr::distinct(.data$LocationID, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select("LocationID", "LocationType", "PopID", "HabitatType", "StartSeason", "EndSeason")

  ## Create table with nest box information
  Location_data_temp <- dplyr::bind_rows(loc_data, loc_mn) %>%
    dplyr::group_by(.data$LocationID) %>%
    dplyr::mutate(
      dplyr::across(c("StartSeason", "EndSeason"), as.integer),
      ## Keep lat/lon with the most digits for each box
      dplyr::across(c("Latitude", "Longitude"),
                    ~ get_max_precision_value(.x))) %>%
    ## Keep distinct records
    dplyr::distinct(PopID, NestboxID, .keep_all = TRUE) %>%
    dplyr::ungroup()

  return(Location_data_temp)

}

#Function to keep lat/lon with the most digits for each box (dealing with NAs)
get_max_precision_value <- function(x) {
  non_na <- na.omit(x)
  if (length(non_na) == 0) {
    return(NA_real_)
  }
  as.numeric(non_na[which.max(nchar(non_na))])
}
