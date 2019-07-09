#' Construct standard summary for data from Bandon Valley, Ireland.
#'
#' A pipeline to produce a standard output for the nest box population in Bandon
#' Valley, Ireland, administered by John Quinn.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see XXXXX
#' PLACE HOLDER!
#'
#' \strong{Species}: There are records where species is uncertain (e.g. listed
#' as 'GRETI?'). This uncertainty is ignored. We assume that the suggested
#' species is correct. We include blue tit, great tit, and coal tit. No other
#' species are recorded.
#'
#' \strong{ClutchType_observed}: Clutch type is only listed as 'first' and
#' 'second'. There is no distinction between 'second' and 'replacement'. We
#' categorise all these as 'second'. Possible distinction between 'second' and
#' 'replacement' could be made with \strong{ClutchType_calc}.
#'
#' \strong{BroodID}: Unique BroodID is currently made with Year_Plot_LocationID_NestAttempt.
#'
#' \strong{Age_obsv}: There is no recorded capture age. This is left as NA.
#'
#' @param db Directory path. Location of primary data.
#' @param path File path. Location where output csv files will be saved.
#' @param debug For internal use when editing pipelines. If TRUE, pipeline
#'   generates a summary of pipeline data. This includes: a) Histogram of
#'   continuous variables with mean/SD b) unique values of all categorical
#'   variables.
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export
#'
#' @examples
#' format_BAN()
format_BAN <- function(db = choose.dir(), path = ".", debug = FALSE){

  start_time <- Sys.time()

  message("Importing primary data...")

  all_data <- readxl::read_excel(paste0(db, "/MasterBreedingDataAllYears password protected.xlsx")) %>%
    janitor::clean_names() %>%
    #Convert column names to meet standard format
    dplyr::mutate(BreedingSeason = year,
                  PopID = "BAN", Plot = site,
                  LocationID = paste(Plot, stringr::str_pad(string = box_number, width = 3, pad = "0"), sep = "_"),
                  Species = dplyr::case_when(grepl(pattern = "GRETI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             grepl(pattern = "BLUTI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             grepl(pattern = "COATI", x = .$species) ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                  BroodID = paste(BreedingSeason, LocationID, nest_attempt, sep = "_"),
                  ClutchType_observed = dplyr::case_when(.$nest_attempt == 1 ~ "first",
                                                         .$nest_attempt == 2 ~ "second"),
                  March1Date = as.Date(glue::glue('{Year}-03-01',
                                                  Year = BreedingSeason),
                                       format = "%Y-%m-%d"),
                  LayingDate = March1Date + as.numeric(gsub(pattern = "\\?|\\+",
                                                 replacement = "",
                                                 x = first_egg_lay_date)),
                  AvgEggMass = egg_weight, NumberEggs = number_eggs_weighed,
                  EggWeighDate = (March1Date + as.numeric(weigh_date)) - LayingDate,
                  ClutchSize = as.numeric(final_clutch_size),
                  HatchDate = March1Date + as.numeric(gsub(pattern = "\\?",
                                                           replacement = "",
                                                           x = actual_hatch_date)),
                  MaleCaptureDate = March1Date + as.numeric(actual_male_trapping_date),
                  FemaleCaptureDate = March1Date + as.numeric(actual_female_trapping_date),
                  MaleID = male_id, FemaleID = female_id,
                  ChickCaptureDate = March1Date + as.numeric(actual_pullus_ringing_date),
                  NumberFledged = as.numeric(gsub(pattern = "\\?",
                                                  replacement = "",
                                                  number_fledged)))

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- create_brood_BAN(all_data)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Capture_data <- create_capture_BAN(all_data)

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  Individual_data <- create_individual_BAN(Capture_data)

  #################
  # LOCATION DATA #
  #################

  message("Compiling location information...")

  Location_data <- create_location_BAN(all_data)

  #######################
  # CREATE DEBUG OPTION #
  #######################

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "BAN", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

  ###############
  # EXPORT DATA #
  ###############

  message("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_BAN.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_BAN.csv"), row.names = F)

  write.csv(x = Individual_data, file = paste0(path, "\\Indv_data_BAN.csv"), row.names = F)

  write.csv(x = Location_data, file = paste0(path, "\\Location_data_BAN.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

}

create_brood_BAN   <- function(data) {

  Brood_data <- data %>%
    dplyr::mutate(ClutchType_calc = calc_clutchtype(., na.rm = FALSE),
                  LayingDateError = NA,
                  ClutchSizeError = NA,
                  HatchDateError = NA,
                  BroodSize = NA, BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledgedError = NA,
                  AvgChickMass = NA, NumberChicksMass = NA,
                  AvgTarsus = NA, NumberChicksTarsus = NA,
                  ExperimentID = NA) %>%
    ## Remove egg weights when the day of weighing is outside our given range
    ## FOR NOW WE INCLUDE ALL EGG MASS DATES
    dplyr::mutate(AvgEggMass = purrr::map2_dbl(.x = AvgEggMass, .y = EggWeighDate,
                                               .f = ~{ifelse(!between(..2, 0, 20), NA, as.numeric(..1))})) %>%
    dplyr::select(PopID, BreedingSeason,
                  Species, Plot, LocationID,
                  BroodID, FemaleID, MaleID,
                  ClutchType_observed,
                  ClutchType_calc,
                  LayingDate, LayingDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus,
                  ExperimentID)

  return(Brood_data)

}

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
                  CaptureDate = purrr::pmap_chr(.l = list(Sex, MaleCaptureDate,
                                                      FemaleCaptureDate),
                                            .f = ~{

                                              if(..1 == "F"){

                                                return(as.character(..3))

                                              } else {

                                                return(as.character(..2))

                                              }

                                            }),
                  Age_obsv = NA, CaptureTime = NA, Mass = NA, Tarsus = NA,
                  WingLength = NA, ChickAge = NA, CapturePopID = "BAN",
                  ReleasePopID = "BAN") %>%
    calc_age(ID = IndvID, Age = Age_obsv,
             Date = CaptureDate, Year = BreedingSeason,
             showpb = TRUE) %>%
    dplyr::select(IndvID, Species, BreedingSeason,
                  LocationID, CaptureDate, CaptureTime,
                  CapturePopID, CapturePlot = Plot,
                  ReleasePopID, ReleasePlot = Plot,
                  Mass, Tarsus, WingLength, Age_obsv, Age_calc,
                  ChickAge, Sex) %>%
    ungroup()

  return(Capture_data)

}

create_individual_BAN <- function(Capture_data) {

  Individual_data <- Capture_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = unique(na.omit(Species)),
                     PopID = "BAN",
                     RingSeason = first(BreedingSeason),
                     RingAge = first(Age_obsv),
                     Sex = purrr::map_chr(.x = list(unique(Sex)),
                                          .f = ~{

                                            if(all(c("F", "M") %in% ..1)){

                                              return("CONFLICTING SEX")

                                            } else if("F" %in% ..1){

                                              return("F")

                                            } else if("M" %in% ..1){

                                              return("M")

                                            } else if(is.na(..1)){

                                              return("U")

                                            }

                                          }))

  return(Individual_data)

}

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
