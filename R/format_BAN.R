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
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_BAN <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  #Force choose_directory() if used
  force(db)

  #Add species filter
  if(is.null(species)){

    species_filter <- species_codes$Species

  } else {

    species_filter <- species

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  #Warnings arise when we coerce records like 'UNKNOWN' into numeric (making NA by coercion)
  #We want this behaviour, so we hide the warnings.
  all_data <- suppressWarnings(readxl::read_excel(paste0(db, "/BAN_PrimaryData.xlsx")) %>%
                                 #Convert all cols to snake_case
                                 janitor::clean_names() %>%
                                 dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                                             .fns = ~dplyr::na_if(as.character(.), "NA"))) %>%
                                 #Convert column names to match standard format
                                 dplyr::mutate(BreedingSeason = as.integer(.data$year),
                                               PopID = "BAN",
                                               Plot = .data$site,
                                               #Create a unique LocationID using plot and box number
                                               LocationID = paste(.data$Plot,
                                                                  stringr::str_pad(string = .data$box_number,
                                                                                   width = 3,
                                                                                   pad = "0"), sep = "_"),
                                               #Ignore uncertainty in species (e.g. GRETI?)
                                               #TODO Need to check with data owners
                                               Species = dplyr::case_when(grepl(pattern = "GRETI", x = .data$species) ~ species_codes[species_codes$SpeciesID == 14640, ]$Species,
                                                                          grepl(pattern = "BLUTI", x = .data$species) ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                                                          grepl(pattern = "COATI", x = .data$species) ~ species_codes[species_codes$SpeciesID == 14610, ]$Species),
                                               #Ignore uncertainty in clutch type (e.g. 2(MAYBE))
                                               ClutchType_observed = dplyr::case_when(grepl(pattern = 1, x = .data$nest_attempt) ~ "first",
                                                                                      grepl(pattern = 2, x = .data$nest_attempt) ~ "second"),
                                               March1Date = as.Date(paste0(.data$BreedingSeason, '-03-01'), format = "%Y-%m-%d"),
                                               #Ignore uncertainty in laying date (e.g. 97? or 97+)
                                               #TODO Need to check with data owners
                                               #Laying date is calculated where LayDate 1 = March 1st
                                               #We need to do March 1st - 1 + Laying date to get corresponding calendar date
                                               #(can't use end of Feb + Laying date because of leap years)
                                               LayDate_observed = .data$March1Date - 1 + as.numeric(gsub(pattern = "\\?|\\+",
                                                                                                         replacement = "",
                                                                                                         x = .data$first_egg_lay_date)),
                                               #Create a unique BroodID from Year_Plot_BoxNumber_LayingDay_LayingMonth
                                               BroodID = paste(.data$BreedingSeason, .data$LocationID,
                                                               stringr::str_pad(lubridate::day(.data$LayDate_observed),
                                                                                width = 2, pad = "0"),
                                                               stringr::str_pad(lubridate::month(.data$LayDate_observed),
                                                                                width = 2, pad = "0"), sep = "_"),
                                               AvgEggMass = as.numeric(.data$egg_weight),
                                               NumberEggs = as.integer(.data$number_eggs_weighed),
                                               ClutchSize_observed = as.integer(.data$final_clutch_size),
                                               #Assume incubation begins immediately after the last egg is laid.
                                               StartIncubation = .data$LayDate_observed + .data$ClutchSize_observed,
                                               EggWeighDate = (.data$March1Date - 1 + as.numeric(.data$weigh_date)),
                                               #Distinguish whether egg was being incubated when weighed.
                                               EggWasIncubated = (.data$March1Date - 1 + as.numeric(.data$weigh_date)) > (.data$LayDate_observed + .data$ClutchSize_observed),
                                               #Ignore uncertainty in hatch date (e.g. 97?)
                                               HatchDate_observed = .data$March1Date - 1 + as.numeric(gsub(pattern = "\\?",
                                                                                                           replacement = "",
                                                                                                           x = .data$actual_hatch_date)),
                                               MaleCaptureDate = .data$March1Date - 1 + as.numeric(.data$actual_male_trapping_date),
                                               FemaleCaptureDate = .data$March1Date - 1 + as.numeric(.data$actual_female_trapping_date),
                                               MaleID = .data$male_id, FemaleID = .data$female_id,
                                               ChickCaptureDate = .data$March1Date - 1 + as.numeric(.data$actual_pullus_ringing_date),
                                               #Ignore uncertainty in NumberFledged (e.g. 97?)
                                               NumberFledged_observed = as.integer(gsub(pattern = "\\?",
                                                                                        replacement = "",
                                                                                        .data$number_fledged))) %>%
                                 #Filter only the species of interest
                                 dplyr::filter(.data$Species %in% species_filter))

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BAN.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BAN.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_BAN.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_BAN.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_BAN.txt"),
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

#' Create brood data table for Bandon Valley, Ireland.
#'
#' Create brood data table in standard format for data from Bandon Valley,
#' Ireland.
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.

create_brood_BAN <- function(data) {

  Brood_data <- data %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE, protocol_version = "1.1"),
                  LayDate_min = .data$LayDate_observed,
                  LayDate_max = .data$LayDate_observed,
                  ClutchSize_min = .data$ClutchSize_observed,
                  ClutchSize_max = .data$ClutchSize_observed,
                  HatchDate_min = .data$HatchDate_observed,
                  HatchDate_max = .data$HatchDate_observed,
                  BroodSize_observed = NA_integer_,
                  BroodSize_min = .data$BroodSize_observed,
                  BroodSize_max = .data$BroodSize_observed,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = .data$FledgeDate_observed,
                  FledgeDate_max = .data$FledgeDate_observed,
                  NumberFledged_min = .data$NumberFledged_observed,
                  NumberFledged_max = .data$NumberFledged_observed,
                  AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_,
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_) %>%
    ## Remove egg weights when the day of weighing is during incubation
    dplyr::mutate(AvgEggMass = purrr::map2_dbl(.x = .data$AvgEggMass, .y = .data$EggWasIncubated,
                                               .f = ~{ifelse(..2, NA_real_, as.numeric(..1))})) %>%
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID",
                  "FemaleID", "MaleID",
                  "ClutchType_observed",
                  "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min", "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min", "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
                  "AvgEggMass", "NumberEggs",
                  "AvgChickMass", "NumberChicksMass",
                  "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod",
                  "ExperimentID")

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
    dplyr::select("Species", "BreedingSeason", "LocationID", "Plot",
                  "FemaleCaptureDate", "MaleCaptureDate",
                  "FemaleID", "MaleID") %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"), names_to = "Sex", values_to = "IndvID") %>%
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
    dplyr::select("IndvID", "Species", "Sex_observed",
                  "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID",
                  "CaptureAlive", "ReleaseAlive",
                  "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot",
                  "Mass", "Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated",
                  "ChickAge", "ExperimentID") %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("CaptureID", tidyselect::everything())

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
