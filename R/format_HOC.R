#'Construct standard format for data from Hochstadt, Germany.
#'
#'A pipeline to produce the standard format for the hole nesting bird
#'populations in Hochstadt, Germany administered by Max Plank Institute
#'for Ornithology, Seewiesen (Michaela Hau).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{AvgEggMass:} Clutch mass is recorded in many cases; however, these measurements are
#'taken on or after the start of incubation. As egg mass can change throughout the period of
#'incubation we have not used these data.
#'
#'\strong{Plot:} There is no distinct plot information in Hochstadt. Plot is left blank.
#'
#'\strong{LocationID:} Captures can occur on nest boxes (e.g., with trap, while brooding, at feeder)
#'or with mistnets. The location of all non-nestbox trapping is located next to a known nestbox.
#'Therefore, we simply use nestbox as the LocationID for all captures, even when the capture didn't
#'occur within the nestbox itself. Therefore, Location data only includes location information
#'for nestbox sites.
#'
#'\strong{Age_observed:} All captures listed as 'nestling' are given a EURING code of 1 (i.e. unable to fly).
#'Captures listed as 'adult' can be either '1st year' or 'adult'. We treat '1st year' as birds known to
#'be in their first reproductive season (i.e. 2nd year of life; EURING 5) while 'adult' are birds known to be after
#'hatched before this season, but exact age unknown (i.e. EURING 4). Some cases are listed as '1st year?'.
#'These are treated the same as '1st year'.
#'
#'\strong{ChickAge:} Chick age is sometimes stored with uncertainty (e.g. 14/15). In all these cases we
#'take the lower age.
#'
#'\strong{ExperimentID:} Manipulation of individuals is recorded with each capture. This includes
#'hormonal injections and attaching backpacks. We treat any brood as having been experimented on
#'if any type of manipulation was recorded on any individual associated with a given brood.
#'\strong{Note:} at this moment, ExperimentID is simply recorded as TRUE/FALSE while we try to
#'categorise all experiments.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.0.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_HOC <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.0.0"

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_HOC(db = db)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_HOC(db = db)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_HOC(db = db)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_HOC(db = db)

  # WRANGLE DATA FOR EXPORT

  #Add average chick mass and tarsus for every nest
  #Filter only those captures with chick age (nestlings with no age are excluded)
  chick_measures <- Capture_data %>%
    dplyr::filter(!is.na(.data$ChickAge) & dplyr::between(.data$ChickAge, 14, 16)) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass = dplyr::na_if(length(stats::na.omit(.data$Mass)), 0),
                     AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = dplyr::na_if(length(stats::na.omit(.data$Tarsus)), 0)) %>%
    dplyr::mutate(OriginalTarsusMethod = ifelse(!is.na(.data$AvgTarsus), "Alternative", NA_character_))

  brood_exp <- Capture_data %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(ExperimentID = as.character(any(.data$ExperimentID)))

  Brood_data <- Brood_data %>%
    dplyr::left_join(chick_measures, by = "BroodID") %>%
    dplyr::left_join(brood_exp, by = "BroodID") %>%
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID", "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate", "LayDateError",
                  "ClutchSize", "ClutchSizeError",
                  "HatchDate", "HatchDateError",
                  "BroodSize", "BroodSizeError",
                  "FledgeDate", "FledgeDateError",
                  "NumberFledged", "NumberFledgedError",
                  "AvgEggMass", "NumberEggs",
                  "AvgChickMass", "NumberChicksMass",
                  "AvgTarsus", "NumberChicksTarsus",
                  "OriginalTarsusMethod",
                  "ExperimentID")

  Capture_data <- Capture_data %>%
    dplyr::select(-"BroodID", -"ExperimentID", -"capture_method")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HOC.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HOC.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data %>% dplyr::select(-"Sex", -"BroodID"),
                     file = paste0(path, "\\Capture_data_HOC.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HOC.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_HOC.txt"),
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

#' Create brood data table for Hochstadt.
#'
#' @param db Location of primary data from Hochstadt.
#'
#' @return A data frame with Brood data

create_brood_HOC <- function(db){

  #We read everything in as text and convert it afterwards
  #Even though some columns (e.g. date) work well, they may be broken with newer data.
  #Using text and converting manually should be more robust to data changes
  #They include egg mass, but this is always after incubation so it is not included (we only take egg weight before incubation)
  Brood_data <- readxl::read_excel(path = paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Nests_ID", na = c("", "na"),
                                   col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(BroodID = .data$unique_nest_id,
                  PopID = "HOC",
                  Species = "PARMAJ",
                  Plot = NA_character_,
                  LocationID = paste0("H", .data$nestbox_no),
                  ClutchType_observed = .data$clutch_no,
                  BreedingSeason = as.integer(.data$year),
                  MaleID = .data$social_male_bird_id,
                  FemaleID = .data$social_female_bird_id,
                  LayDate = janitor::excel_numeric_to_date(as.numeric(.data$x1st_egg_lay_date)),
                  LayDateError = as.numeric(.data$lay_date_error),
                  ClutchSize = as.integer(.data$clutch_size),
                  ClutchSizeError = as.numeric(.data$clutch_size_error),
                  HatchDate = janitor::excel_numeric_to_date(as.numeric(.data$hatch_date)),
                  HatchDateError = as.numeric(.data$hatch_date_error),
                  BroodSize = as.integer(.data$hatch_number),
                  BroodSizeError = as.numeric(.data$hatch_number_error),
                  NumberFledged = as.integer(.data$fledge_number),
                  NumberFledgedError = as.numeric(.data$fledge_number_error),
                  FledgeDate = janitor::excel_numeric_to_date(as.numeric(.data$fledge_date)),
                  FledgeDateError = as.numeric(.data$fledge_date_error),
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_) %>%
    dplyr::arrange("BreedingSeason", "FemaleID", "LayDate") %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #No need to order cols yet because we still need to add AvgChickMass etc.
    dplyr::select("BroodID":"NumberEggs", "ClutchType_calculated")

  return(Brood_data)

}

#' Create capture data table for Hochstadt.
#'
#' @param db Location of primary data from Hochstadt.
#'
#' @return A data frame with Capture data

create_capture_HOC <- function(db){

  Capture_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Capture ID", na = c("", "na"),
                                     col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(IndvID = .data$bird_id,
                  BroodID = .data$nest_id,
                  Species = "PARMAJ",
                  ObserverID = .data$measures_taken_by,
                  CapturePopID = "HOC",
                  ReleasePopID = "HOC",
                  CapturePlot = NA_character_,
                  ReleasePlot = NA_character_,
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date)),
                  CaptureTime = paste0(stringr::str_pad(string = (as.numeric(.data$time_capture) * (24*60)) %/% 60,
                                                        width = 2, pad = "0"),
                                       ":", stringr::str_pad(string = round((as.numeric(.data$time_capture) * (24*60)) %% 60),
                                                             width = 2, pad = "0")),
                  BreedingSeason = as.integer(lubridate::year(.data$CaptureDate)),
                  FoundDead = grepl(pattern = "dead|died", .data$status),
                  LocationID = purrr::map_chr(.x = .data$nest_location,
                                              .f = ~{

                                                if(is.na(..1)){

                                                  return(NA_character_)

                                                } else {

                                                  boxnumber <- stats::na.omit(dplyr::na_if(stringr::str_split_1(..1,
                                                                                                                "[^0-9]+"), ""))

                                                  return(paste0("H", boxnumber))

                                                }

                                              }),
                  Mass = as.numeric(.data$mass_g),
                  WingLength = as.numeric(.data$wing_length_mm),
                  Tarsus = as.numeric(.data$tarsus_length_mm),
                  OriginalTarsusMethod = "Alternative") %>%
    dplyr::bind_cols(., purrr::pmap_df(.l = list(age_exact = .$age_exact,
                                                 age_simple = .$age_simple,
                                                 BreedingSeason = .$BreedingSeason),

                                       function(age_exact, age_simple, BreedingSeason){

                                       if(age_simple == "nestling"){

                                         if(age_exact == "nestling" | is.na(age_exact)){

                                           return(tibble::tibble(Age_observed = 1L, ChickAge = NA_integer_))

                                         } else {

                                           return(tibble::tibble(Age_observed = 1L,
                                                                 ChickAge = as.integer(stringr::str_split(age_exact, pattern = "/")[[1]][1])))

                                         }

                                       } else {

                                         return(tibble::tibble(Age_observed = dplyr::case_when(grepl("ADULT", toupper(age_exact)) ~ 4L,
                                                                                               grepl("1ST YEAR", toupper(age_exact)) & BreedingSeason >= 2019 ~ 5L,
                                                                                               grepl("1ST YEAR", toupper(age_exact)) & BreedingSeason < 2019 ~ 4L),
                                                                                               ChickAge = NA_integer_))

                                       }

                                    })) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ExperimentID = any(c(.data$physical_manipulation_present_at_time_of_catching,
                                       .data$physical_manipulation_present_at_time_of_release,
                                       .data$physiological_manipulation) %in% "manipulated")) %>%
    dplyr::ungroup()

  Death_captures <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "DeadBirds ID", na = c("", "na"),
                                       col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date_found)),
                  IndvID = .data$ringnumber) %>%
    #Find cases where that individual was not recorded captured on that date
    dplyr::left_join(Capture_data %>%
                       dplyr::mutate(in_capt = TRUE) %>%
                       dplyr::select("CaptureDate", "IndvID", "in_capt"),
                     by = c("CaptureDate", "IndvID")) %>%
    dplyr::filter(is.na(.data$in_capt)) %>%
    dplyr::mutate(Species = "PARMAJ",
                  BreedingSeason = lubridate::year(.data$CaptureDate),
                  CaptureTime = NA_character_,
                  ObserverID = NA_character_,
                  LocationID = NA_character_,
                  CapturePopID = "HOC",
                  CapturePlot = NA_character_,
                  ReleasePopID = "HOC",
                  ReleasePlot = NA_character_,
                  Mass = NA_real_,
                  Tarsus = NA_real_,
                  OriginalTarsusMethod = NA_character_,
                  WingLength = NA_real_,
                  Age_observed = dplyr::case_when(.data$age == "adult" ~ 4L,
                                                  .data$age == "nestling" ~ 1L),
                  ChickAge = NA_integer_,
                  BroodID = NA_character_,
                  ExperimentID = NA,
                  FoundDead = TRUE,
                  capture_method = NA_character_)

  Capture_data_combined <- dplyr::bind_rows(Capture_data, Death_captures) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason)  %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID", "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot",
                  "Mass", "Tarsus", "OriginalTarsusMethod", "WingLength", "Age_observed",
                  "Age_calculated", "ChickAge", "FoundDead", "BroodID", "ExperimentID", "capture_method")

  return(Capture_data_combined)

}

#' Create individual data table for Hochstadt.
#'
#' @param db Location of individual data from Hochstadt.
#'
#' @return A data frame with Individual data

create_individual_HOC <- function(db){

  #Technically, they already have individual data in a separate table
  #However, we will check this in comparison to capture data
  Individual_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Bird_ID", na = c("", "na"),
                                     col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(IndvID = .data$ring_number,
                  Species = "PARMAJ",
                  Sex = dplyr::case_when(.data$sex == "female" ~ "F",
                                         .data$sex == "male" ~ "M"),
                  PopID = "HOC",
                  RingSeason = lubridate::year(janitor::excel_numeric_to_date(as.numeric(.data$date_ringed))),
                  RingAge = dplyr::case_when(.data$age_simple == "adult" ~ "adult",
                                             .data$age_simple == "nestling" ~ "chick"),
                  BroodIDLaid = .data$nest_of_origin_id,
                  BroodIDFledged = .data$rearing_nest_id) %>%
    dplyr::select("IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge", "Sex")

  return(Individual_data)

}

#' Create location data table for Hochstadt.
#'
#' @param db Location of location data from Hochstadt.
#'
#' @return A data frame with Location data

create_location_HOC <- function(db){

  Location_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Location Data", na = c("", "na"),
                                      col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(LocationID = paste0("H", .data$nestbox_number),
                  NestboxID = .data$LocationID,
                  LocationType = "NB",
                  PopID = "HOC",
                  Latitude = as.numeric(.data$latitude),
                  Longitude = as.numeric(.data$longitude),
                  StartSeason = 2014L,
                  EndSeason = NA_integer_,
                  Habitat = "mixed")

  return(Location_data)

}
