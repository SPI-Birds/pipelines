#'Construct standard format for data from Bandon Valley, Ireland.
#'
#'A pipeline to produce the standard format for the nest box population in Bandon
#'Valley, Ireland, administered by John Quinn.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: There are records where species is uncertain (e.g. listed as
#''GRETI?'). This uncertainty is ignored. We assume that the suggested species
#'is correct. We include blue tit, great tit, and coal tit. No other species are
#'recorded.
#'
#'\strong{ClutchType_observed}: Clutch type is only listed as 'first' and
#''second'. There is no distinction between 'second' and 'replacement'. We
#'categorise all these as 'second'. Possible distinction between 'second' and
#''replacement' can be made with \strong{ClutchType_calculated}. There are a
#'small number of cases where nest attempt number is uncertain (e.g.
#'`2(MAYBE)`). This uncertainty is ignored.
#'
#'\strong{LayDate_observed, HatchDate_observed, NumberFledged_observed}: There are some cases where
#'values are given with uncertainty (e.g. 97+, 95?). We don't know how much
#'uncertainty is involved here, it is currently ignored, but we need to
#'discuss this with data owner.
#'
#'\strong{ClutchSize_observed}: Cases where clutch size is uncertain (e.g. nests were
#'predated before completion) are treated as NA because clutch size is unknown.
#'
#'\strong{LocationID}: Box numbers are not unique, they are repeated between
#'plots. To generate LocationID, we therefore use Plot_BoxNumber.
#'
#'\strong{BroodID}: Unique BroodID is currently made with
#'Year_Plot_BoxNumber_Day_Month.
#'
#'\strong{BroodIDLaid/Fledged}: Currently, we have no information about the
#'brood where each individual was laid. Therefore, these are currently kept
#'blank.
#'
#'\strong{Age_observed}: There is no recorded capture age. This is left as NA.
#'
#'\strong{AvgEggMass}: Currently we only include records where the day of egg
#'weighing is <= LayDate_observed + ClutchSize_observed. This should be an estimate of the date
#'that incubation began. Once incubation starts, egg weight is not easily
#'comparable because it changes with embryonic development.
#'
#'\strong{AvgChickMass/AvgTarsus}: Individual capture data is not included in
#'the data currently provided. These values are therefore left blank.
#'
#'\strong{StartSeason}: Assume all boxes were placed in the first year of the study.
#'
#'\strong{CaptureAlive, ReleaseAlive}: Assume all individuals were alive when captured and released.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_BAN <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  # Force choose_directory() if used
  force(db)

  # Add species filter
  if(is.null(species)){

    species_filter <- species_codes$speciesID

  } else {

    species_filter <- species

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  message("Importing primary data...")

  # Warnings arise when we coerce records like 'UNKNOWN' into numeric (making NA by coercion)
  # We want this behaviour, so we hide the warnings.
  all_data <- suppressWarnings(readxl::read_excel(paste0(db, "/BAN_PrimaryData.xlsx")) %>%
                                 # Convert all cols to snake_case
                                 janitor::clean_names() %>%
                                 dplyr::mutate(dplyr::across(.cols = everything(),
                                                             .fns = ~{

                                                               replace(x = .x,
                                                                       list = .x %in% c("NA", "na", "unknown"),
                                                                       values = NA)

                                                             })) %>%
                                 # Convert column names to match standard format
                                 dplyr::mutate(siteID = "BAN",
                                               plotID = paste0("BAN_", .data$site),
                                               # Create a unique locationID using plot and box number
                                               locationID = paste(.data$site,
                                                                  stringr::str_pad(string = .data$box_number,
                                                                                   width = 3,
                                                                                   pad = "0"), sep = "_"),
                                               # Ignore uncertainty in species (e.g. GRETI?)
                                               # TODO: Need to check with data owners
                                               speciesID = dplyr::case_when(grepl(pattern = "GRETI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10001, ]$speciesID,
                                                                            grepl(pattern = "BLUTI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10002, ]$speciesID,
                                                                            grepl(pattern = "COATI",
                                                                                  x = .data$species) ~ species_codes[species_codes$speciesCode == 10005, ]$speciesID),
                                               # Create a unique broodID from year, locationID, lay date (in March days)
                                               broodID = paste(.data$year,
                                                               .data$locationID,
                                                               .data$first_egg_lay_date,
                                                               sep = "_"),
                                               # If maleID & femaleID differ from expected format, set to NA
                                               # Ensure that individuals are unique: add institutionID as prefix
                                               maleID = .data$male_id,
                                               femaleID = .data$female_id,
                                               dplyr::across(.cols = c(.data$maleID, .data$femaleID),
                                                             .fns = ~{

                                                               dplyr::case_when(stringr::str_detect(.x, "^[:alpha:]{1,3}[:digit:]{4,6}$") ~ paste0("BAN_", .x),
                                                                                TRUE ~ NA_character_)

                                                             }),
                                               # Dates are recorded in March days, i.e., 1 = March 1st
                                               # Create marchDate as baseline for other dates
                                               # We need to do March 1st - 1 + date to get corresponding calendar date
                                               # (can't use end of Feb + date because of leap years)
                                               marchDate = as.Date(paste0(.data$year, '-03-01'), format = "%Y-%m-%d"),
                                               maleCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_male_trapping_date),
                                               femaleCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_female_trapping_date),
                                               chickCaptureDate = .data$marchDate - 1 + as.numeric(.data$actual_pullus_ringing_date)) %>%
                                 # Filter only the species of interest
                                 dplyr::filter(.data$speciesID %in% species_filter))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_BAN(all_data)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_BAN(all_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_BAN(Capture_data)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_BAN(all_data)

  # WRANGLE DATA FOR EXPORT

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == 'csv'){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BAN.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BAN.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}

#' Create brood data table for Bandon Valley, Ireland.
#'
#' Create brood data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.

create_brood_BAN <- function(data) {

  Brood_data <- data %>%
    dplyr::mutate(
                  # Ignore uncertainty in clutch type (e.g., 2(MAYBE))
                  observedClutchType = dplyr::case_when(grepl(pattern = 1, x = .data$nest_attempt) ~ "first",
                                                        grepl(pattern = 2, x = .data$nest_attempt) ~ "second"),
                  # Ignore uncertainty in lay date (e.g., 97? or 97+)
                  # TODO: Need to check with data owners
                  observedLayDate = .data$marchDate - 1 + as.numeric(gsub(pattern = "\\?|\\+",
                                                                          replacement = "",
                                                                          x = .data$first_egg_lay_date)),
                  observedLayYear = lubridate::year(.data$observedLayDate),
                  observedLayMonth = lubridate::month(.data$observedLayDate),
                  observedLayDay = lubridate::day(.data$observedLayDate),
                  observedClutchSize = dplyr::case_when(stringr::str_detect(.data$final_clutch_size, "[:digit:]+") ~ as.integer(stringr::extract(.data$final_clutch_size, "[:digit:]+")),
                                                        TRUE ~ NA_integer_),
                  #maximumClutchSize =
                  # Ignore uncertainty in hatch date (e.g., ?)
                  observedHatchDate = .data$marchDate - 1 + as.numeric(gsub(pattern = "\\?",
                                                                            replacement = "",
                                                                            x = .data$actual_hatch_date)),
                  observedHatchYear = lubridate::year(.data$observedHatchDate),
                  observedHatchMonth = lubridate::month(.data$observedHatchDate),
                  observedHatchDay = lubridate::day(.data$observedHatchDate),
                  # Ignore uncertainty in number fledged (e.g., 0?)
                  observedNumberFledged = as.integer(gsub(pattern = "\\?",
                                                          replacement = "",
                                                          x = .data$number_fledged)))

  return(Brood_data)

}

#' Create capture data table for Bandon Valley, Ireland.
#'
#' Create capture data table in standard format for data from Bandon Valley,
#' Ireland.
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.

create_capture_BAN <- function(data) {

  Capture_data <- data %>%
    dplyr::select(.data$Species, .data$BreedingSeason, .data$LocationID, .data$Plot,
                  .data$FemaleCaptureDate, .data$MaleCaptureDate,
                  .data$FemaleID, .data$MaleID) %>%
    tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID), names_to = "Sex", values_to = "IndvID") %>%
    dplyr::arrange(.data$Sex) %>%
    dplyr::filter(!is.na(.data$IndvID) & !.data$IndvID %in% c("UNKNOWN", "NA")) %>%
    dplyr::mutate(Sex_observed = dplyr::case_when(grepl(pattern = "Female", .data$Sex) ~ "F",
                                                  grepl(pattern = "Male", .data$Sex) ~ "M"),
                  CaptureDate = as.Date(purrr::pmap_chr(.l = list(.data$Sex_observed, .data$MaleCaptureDate,
                                                                  .data$FemaleCaptureDate),
                                                        .f = ~{

                                                          if(..1 == "F"){

                                                            return(as.character(..3))

                                                          } else {

                                                            return(as.character(..2))

                                                          }

                                                        })),
                  Age_observed = NA_integer_,
                  CaptureTime = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  WingLength = NA_real_, ChickAge = NA_integer_,
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE,
                  CapturePopID = "BAN",
                  ReleasePopID = "BAN", ObserverID = NA_character_,
                  OriginalTarsusMethod = NA_character_,
                  CapturePlot = .data$Plot, ReleasePlot = .data$Plot,
                  ExperimentID = NA_character_) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    dplyr::select(.data$IndvID, .data$Species, .data$Sex_observed,
                  .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$CaptureID, everything())

  return(Capture_data)

}

#' Create individual data table for Bandon Valley, Ireland.
#'
#' Create individual data table in standard format for data from Bandon Valley,
#' Ireland.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_BAN}}.
#'
#' @return A data frame.

create_individual_BAN <- function(Capture_data) {

  Individual_data <- Capture_data %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = unique(stats::na.omit(.data$Species)),
                     PopID = "BAN",
                     BroodIDLaid = NA_character_,
                     BroodIDFledged = NA_character_,
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = dplyr::first(.data$Age_observed),
                     Sex_calculated = purrr::map_chr(.x = list(unique(.data$Sex_observed)),
                                                     .f = ~{

                                                       if(all(c("F", "M") %in% ..1)){

                                                         return("C")

                                                       } else if("F" %in% ..1){

                                                         return("F")

                                                       } else if("M" %in% ..1){

                                                         return("M")

                                                       } else if(is.na(..1)){

                                                         return(NA_character_)

                                                       }

                                                     }),
                     Sex_genetic = NA_character_) %>%
    #Change RingAge to chick/adult
    dplyr::mutate(RingAge = dplyr::case_when(is.na(.data$RingAge) ~ "adult",
                                             .data$RingAge > 3 ~ "adult",
                                             .data$RingAge <= 3 ~ "chick")) %>%
    dplyr::ungroup()

  return(Individual_data)

}

#' Create location data table for Bandon Valley, Ireland.
#'
#' Create location data table in standard format for data from Bandon Valley,
#' Ireland.
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.

create_location_BAN <- function(data) {

  Location_data <- tibble::tibble(LocationID = unique(data$LocationID),
                                  NestboxID = .data$LocationID,
                                  LocationType = "NB",
                                  PopID = "BAN",
                                  Latitude = NA_real_, Longitude = NA_real_,
                                  StartSeason = 2013L,
                                  EndSeason = NA_integer_, HabitatType = NA_character_)

  return(Location_data)

}
