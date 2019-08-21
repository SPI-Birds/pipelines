#'Construct standard summary for data from Bandon Valley, Ireland.
#'
#'A pipeline to produce a standard output for the nest box population in Bandon
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
#'\strong{BroodID}: Unique BroodID is currently made with
#'Year_Plot_LocationID_Day_Month.
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

  all_data <- suppressWarnings(readxl::read_excel(paste0(db, "/MasterBreedingDataAllYears password protected.xlsx")) %>%
                                 janitor::clean_names() %>%
                                 dplyr::mutate_all(.funs = toupper) %>%
                                 dplyr::mutate_all(.funs = na_if, y = "NA") %>%
                                 #Convert column names to meet standard format
                                 dplyr::mutate(BreedingSeason = year,
                                               PopID = "BAN", Plot = site,
                                               LocationID = paste(Plot, stringr::str_pad(string = box_number, width = 3, pad = "0"), sep = "_"),
                                               Species = dplyr::case_when(grepl(pattern = "GRETI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                                                          grepl(pattern = "BLUTI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                                                          grepl(pattern = "COATI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                                               ClutchType_observed = dplyr::case_when(grepl(pattern = 1, x = .$nest_attempt) ~ "first",
                                                                                      grepl(pattern = 2, x = .$nest_attempt) ~ "second"),
                                               March1Date = as.Date(glue::glue('{Year}-03-01',
                                                                               Year = BreedingSeason),
                                                                    format = "%Y-%m-%d"),
                                               LayingDate = March1Date + as.numeric(gsub(pattern = "\\?|\\+",
                                                                                         replacement = "",
                                                                                         x = first_egg_lay_date)),
                                               BroodID = paste(BreedingSeason, LocationID,
                                                               lubridate::day(LayingDate), lubridate::month(LayingDate), sep = "_"),
                                               AvgEggMass = as.numeric(egg_weight), NumberEggs = as.numeric(number_eggs_weighed),
                                               ClutchSize = as.numeric(final_clutch_size),
                                               StartIncubation = LayingDate + ClutchSize,
                                               EggWeighDate = (March1Date + as.numeric(weigh_date)),
                                               EggWasIncubated = (March1Date + as.numeric(weigh_date)) > (LayingDate + ClutchSize),
                                               HatchDate = March1Date + as.numeric(gsub(pattern = "\\?",
                                                                                        replacement = "",
                                                                                        x = actual_hatch_date)),
                                               MaleCaptureDate = March1Date + as.numeric(actual_male_trapping_date),
                                               FemaleCaptureDate = March1Date + as.numeric(actual_female_trapping_date),
                                               MaleID = male_id, FemaleID = female_id,
                                               ChickCaptureDate = March1Date + as.numeric(actual_pullus_ringing_date),
                                               NumberFledged = as.numeric(gsub(pattern = "\\?",
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
                  LayingDateError = NA,
                  ClutchSizeError = NA,
                  HatchDateError = NA,
                  BroodSize = NA, BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledgedError = NA,
                  AvgChickMass = NA, NumberChicksMass = NA,
                  AvgTarsus = NA, NumberChicksTarsus = NA,
                  OriginalTarsusMethod = NA,
                  ExperimentID = NA) %>%
    ## Remove egg weights when the day of weighing is outside our given range
    dplyr::mutate(AvgEggMass = purrr::map2_dbl(.x = AvgEggMass, .y = EggWasIncubated,
                                               .f = ~{ifelse(..2, NA, as.numeric(..1))})) %>%
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
                  Age_observed = NA, CaptureTime = NA, Mass = NA, Tarsus = NA,
                  WingLength = NA, ChickAge = NA, CapturePopID = "BAN",
                  ReleasePopID = "BAN", ObserverID = NA,
                  OriginalTarsusMethod = NA,
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
                     BroodIDLaid = NA,
                     BroodIDFledged = NA,
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
                                  Latitude = NA, Longitude = NA,
                                  StartSeason = 2013,
                                  EndSeason = NA, Habitat = NA)

  return(Location_data)

}
