#'Construct standard format for data from Bandon Valley, Ireland.
#'
#'A pipeline to produce the standard format for the nest box population in Bandon
#'Valley, Ireland, administered by John Quinn.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
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
#'\strong{LayingDate, HatchDate, NumberFledged}: There are some cases where
#'values are given with uncertainty (e.g. 97+, 95?). We don't know how much
#'uncertainty is involved here, it is ignored.
#'
#'\strong{ClutchSize}: Cases where clutch size is uncertain (e.g. nests were
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
#'weighing is <= LayingDate + ClutchSize. This should be an estimate of the date
#'that incubation began. Once incubation starts, egg weight is not easily
#'comparable because it changes with chick development.
#'
#'\strong{AvgChickMass/AvgTarsus}: Individual capture data is not included in
#'the data currently provided. These values are therefore left blank.
#'
#'\strong{StartSeason}: Assume all boxes were placed in the first year of the study.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_BAN <- function(db = utils::choose.dir(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       debug = FALSE,
                       output_type = 'csv'){

  #Force choose.dir() if used
  force(db)

  #Assign species for filtering
  #If no species are specified, all species are included
  if(is.null(species)){

    species_filter <- Species_codes$Code

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
                                 dplyr::mutate_all(.funs = na_if, y = "NA") %>%
                                 #Convert column names to match standard format
                                 dplyr::mutate(BreedingSeason = as.integer(year),
                                               PopID = "BAN", Plot = site,
                                               #Create a unique LocationID using plot and box number
                                               LocationID = paste(Plot, stringr::str_pad(string = box_number, width = 3, pad = "0"), sep = "_"),
                                               #Ignore uncertainty in species (e.g. GRETI?)
                                               Species = dplyr::case_when(grepl(pattern = "GRETI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                                                          grepl(pattern = "BLUTI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                                                          grepl(pattern = "COATI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                                               #Ignore uncertainty in clutch type (e.g. 2(MAYBE))
                                               ClutchType_observed = dplyr::case_when(grepl(pattern = 1, x = .$nest_attempt) ~ "first",
                                                                                      grepl(pattern = 2, x = .$nest_attempt) ~ "second"),
                                               March1Date = as.Date(glue::glue('{Year}-03-01',
                                                                               Year = BreedingSeason),
                                                                    format = "%Y-%m-%d"),
                                               #Ignore uncertainty in laying date (e.g. 97? or 97+)
                                               #Laying date is calculated where LayingDate 1 = March 1st
                                               #We need to do March 1st - 1 + Laying date to get corresponding calendar date
                                               #(can't use end of Feb + Laying date because of leap years)
                                               LayingDate = March1Date - 1 + as.numeric(gsub(pattern = "\\?|\\+",
                                                                                         replacement = "",
                                                                                         x = first_egg_lay_date)),
                                               #Create a unique BroodID from Year_Plot_BoxNumber_LayingDay_LayingMonth
                                               BroodID = paste(BreedingSeason, LocationID,
                                                               stringr::str_pad(lubridate::day(LayingDate), width = 2, pad = "0"),
                                                               stringr::str_pad(lubridate::month(LayingDate), width = 2, pad = "0"), sep = "_"),
                                               AvgEggMass = as.numeric(egg_weight), NumberEggs = as.integer(number_eggs_weighed),
                                               ClutchSize = as.integer(final_clutch_size),
                                               #Assume incubation begins immediately after the last egg is laid.
                                               StartIncubation = LayingDate + ClutchSize,
                                               EggWeighDate = (March1Date - 1 + as.numeric(weigh_date)),
                                               #Distinguish whether egg was being incubated when weighed.
                                               EggWasIncubated = (March1Date - 1 + as.numeric(weigh_date)) > (LayingDate + ClutchSize),
                                               #Ignore uncertainty in hatch date (e.g. 97?)
                                               HatchDate = March1Date - 1 + as.numeric(gsub(pattern = "\\?",
                                                                                        replacement = "",
                                                                                        x = actual_hatch_date)),
                                               MaleCaptureDate = March1Date - 1 + as.numeric(actual_male_trapping_date),
                                               FemaleCaptureDate = March1Date - 1 + as.numeric(actual_female_trapping_date),
                                               MaleID = male_id, FemaleID = female_id,
                                               ChickCaptureDate = March1Date - 1 + as.numeric(actual_pullus_ringing_date),
                                               #Ignore uncertainty in NumberFledged (e.g. 97?)
                                               NumberFledged = as.integer(gsub(pattern = "\\?",
                                                                               replacement = "",
                                                                               number_fledged))) %>%
                                 #Filter only the species of interest
                                 dplyr::filter(Species %in% species_filter))

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

  # CREATE DEBUG REPORT

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "BAN", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>%
    dplyr::select(-Sex)

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
#' @param data Data frame. Primary data from Bandon Valley.
#'
#' @return A data frame.

create_brood_BAN   <- function(data) {

  Brood_data <- data %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE),
                  LayingDateError = NA_real_,
                  ClutchSizeError = NA_real_,
                  HatchDateError = NA_real_,
                  BroodSize = NA_integer_, BroodSizeError = NA_real_,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_real_,
                  NumberFledgedError = NA_real_,
                  AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_,
                  OriginalTarsusMethod = NA_character_,
                  ExperimentID = NA_character_) %>%
    ## Remove egg weights when the day of weighing is during incubation
    dplyr::mutate(AvgEggMass = purrr::map2_dbl(.x = AvgEggMass, .y = EggWasIncubated,
                                               .f = ~{ifelse(..2, NA_real_, as.numeric(..1))})) %>%
    dplyr::select(BroodID, PopID, BreedingSeason,
                  Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed,
                  ClutchType_calculated,
                  LayingDate, LayingDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus,
                  OriginalTarsusMethod,
                  ExperimentID)

  return(Brood_data)

  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayingDate <- LayingDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- NumberChicksMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL
  EggWasIncubated <- NULL

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
    dplyr::select(Species, BreedingSeason, LocationID, Plot,
                  FemaleCaptureDate, MaleCaptureDate,
                  FemaleID, MaleID) %>%
    reshape2::melt(measure.vars = c("FemaleID", "MaleID"),
                   value.name = "IndvID",
                   variable.name = "Sex") %>%
    dplyr::filter(!is.na(IndvID) & !IndvID %in% c("UNKNOWN", "NA")) %>%
    dplyr::mutate(Sex = dplyr::case_when(grepl(pattern = "Female", .$Sex) ~ "F",
                                         grepl(pattern = "Male", .$Sex) ~ "M"),
                  CaptureDate = as.Date(purrr::pmap_chr(.l = list(Sex, MaleCaptureDate,
                                                      FemaleCaptureDate),
                                            .f = ~{

                                              if(..1 == "F"){

                                                return(as.character(..3))

                                              } else {

                                                return(as.character(..2))

                                              }

                                            })),
                  Age_observed = NA_integer_, CaptureTime = NA_character_,
                  Mass = NA_real_, Tarsus = NA_real_,
                  WingLength = NA_real_, ChickAge = NA_integer_, CapturePopID = "BAN",
                  ReleasePopID = "BAN", ObserverID = NA_character_,
                  OriginalTarsusMethod = NA_character_,
                  CapturePlot = Plot, ReleasePlot = Plot) %>%
    calc_age(ID = IndvID, Age = Age_observed,
             Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::select(IndvID, Species, BreedingSeason,
                  CaptureDate, CaptureTime,
                  ObserverID, LocationID,
                  CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot,
                  Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated,
                  ChickAge, Sex) %>%
    dplyr::ungroup()

  return(Capture_data)

  #Satisfy RCMD Checks
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  FemaleCaptureDate <- MaleCaptureDate <- FemaleID <- MaleID <- NULL

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
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = unique(stats::na.omit(Species)),
                     PopID = "BAN",
                     BroodIDLaid = NA_character_,
                     BroodIDFledged = NA_character_,
                     RingSeason = first(BreedingSeason),
                     RingAge = first(Age_observed),
                     Sex = purrr::map_chr(.x = list(unique(Sex)),
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

                                          })) %>%
    #Change RingAge to chick/adult
    dplyr::mutate(RingAge = dplyr::case_when(is.na(.$RingAge) ~ "adult",
                                             .$RingAge > 3 ~ "adult",
                                             .$RingAge <= 3 ~ "chick"))

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
                                  NestboxID = LocationID,
                                  LocationType = "NB",
                                  PopID = "BAN",
                                  Latitude = NA_real_, Longitude = NA_real_,
                                  StartSeason = 2013L,
                                  EndSeason = NA_integer_, Habitat = NA_character_)

  return(Location_data)

}
