#'Construct standard format for data from Hochstadt, Germany.
#'
#'A pipeline to produce the standard format for the hole nesting bird
#'populations in Hochstadt, Germany administered by Max Plank Institute
#'for Ornithology, Seewiesen (Michaela Hau).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
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
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_HOC <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

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
    dplyr::filter(!is.na(ChickAge) & between(ChickAge, 14, 16)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(Mass, na.rm = TRUE),
                     NumberChicksMass = dplyr::na_if(length(na.omit(Mass)), 0),
                     AvgTarsus = mean(Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = dplyr::na_if(length(na.omit(Tarsus)), 0)) %>%
    dplyr::mutate(OriginalTarsusMethod = ifelse(!is.na(AvgTarsus), "Alternative", NA_character_))

  brood_exp <- Capture_data %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(ExperimentID = as.character(any(ExperimentID)))

  Brood_data <- Brood_data %>%
    dplyr::left_join(chick_measures, by = "BroodID") %>%
    dplyr::left_join(brood_exp, by = "BroodID") %>%
    dplyr::select(BroodID, PopID, BreedingSeason,
                  Species, Plot, LocationID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate, LayDateError,
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

  Capture_data <- Capture_data %>%
    dplyr::select(-BroodID, -ExperimentID, -capture_method)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_HOC.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_HOC.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_HOC.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_HOC.csv"), row.names = F)

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

create_brood_HOC <- function(db){

  #We read everything in as text and convert it afterwards
  #Even though some columns (e.g. date) work well, they may be broken with newer data.
  #Using text and converting manually should be more robust to data changes
  #They include egg mass, but this is always after incubation so it is not included (we only take egg weight before incubation)
  Brood_data <- readxl::read_excel(path = paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Nests_ID", na = c("", "na"),
                                   col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(BroodID = unique_nest_id,
                  PopID = "HOC",
                  Species = "PARMAJ",
                  Plot = NA_character_,
                  LocationID = paste0("H", nestbox_no),
                  ClutchType_observed = clutch_no,
                  BreedingSeason = as.integer(year),
                  MaleID = social_male_bird_id,
                  FemaleID = social_female_bird_id,
                  LayDate = janitor::excel_numeric_to_date(as.numeric(x1st_egg_lay_date)),
                  LayDateError = as.numeric(lay_date_error),
                  ClutchSize = as.integer(clutch_size), ClutchSizeError = as.numeric(clutch_size_error),
                  HatchDate = janitor::excel_numeric_to_date(as.numeric(hatch_date)),
                  HatchDateError = as.numeric(hatch_date_error),
                  BroodSize = as.integer(hatch_number), BroodSizeError = as.numeric(hatch_number_error),
                  NumberFledged = as.integer(fledge_number),
                  NumberFledgedError = as.numeric(fledge_number_error),
                  FledgeDate = janitor::excel_numeric_to_date(as.numeric(fledge_date)),
                  FledgeDateError = as.numeric(fledge_date_error),
                  AvgEggMass = NA_real_, NumberEggs = NA_integer_) %>%
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #No need to order cols yet because we still need to add AvgChickMass etc.
    dplyr::select(BroodID:NumberEggs, ClutchType_calculated)

  return(Brood_data)

}

create_capture_HOC <- function(db){

  Capture_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Capture ID", na = c("", "na"),
                                     col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(IndvID = bird_id, BroodID = nest_id,
                  Species = "PARMAJ", ObserverID = measures_taken_by,
                  CapturePopID = "HOC", ReleasePopID = "HOC",
                  CapturePlot = NA_character_, ReleasePlot = NA_character_,
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(date)),
                  CaptureTime = paste0(stringr::str_pad(string = (as.numeric(time_capture) * (24*60)) %/% 60, width = 2, pad = "0"),
                                       ":", stringr::str_pad(string = round((as.numeric(time_capture) * (24*60)) %% 60), width = 2, pad = "0")),
                  BreedingSeason = as.integer(lubridate::year(CaptureDate)),
                  FoundDead = grepl(pattern = "dead|died", status),
                  LocationID = purrr::map_chr(.x = nest_location, ~{

                    if(is.na(..1)){

                      return(NA_character_)

                    } else {

                      boxnumber <- na.omit(dplyr::na_if(unlist(strsplit(..1, split = "[^0-9]+")), ""))

                      return(paste0("H", boxnumber))

                    }

                  }), Mass = as.numeric(mass_g), WingLength = as.numeric(wing_length_mm),
                  Mass = as.numeric(mass_g), WingLength = as.numeric(wing_length_mm),
                  Tarsus = as.numeric(tarsus_length_mm), OriginalTarsusMethod = "Alternative") %>%
    dplyr::bind_cols(., purrr::map2_df(.x = .$age_exact, .y = .$age_simple,

                                       function(age_exact, age_simple){

                                       if(age_simple == "nestling"){

                                         if(age_exact == "nestling" | is.na(age_exact)){

                                           return(tibble::tibble(Age_observed = 1L, ChickAge = NA_character_))

                                         } else {

                                           return(tibble::tibble(Age_observed = 1L,
                                                                 ChickAge = as.integer(stringr::str_split(age_exact, pattern = "/")[[1]][1])))

                                         }

                                       } else {

                                         return(tibble::tibble(Age_observed = dplyr::case_when(grepl("ADULT", toupper(age_exact)) ~ 4L,
                                                                                               grepl("1ST YEAR", toupper(age_exact)) ~ 5L),
                                                                                               ChickAge = NA_integer_))

                                       }

                                    })) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ExperimentID = any(c(physical_manipulation_present_at_time_of_catching,
                                       physical_manipulation_present_at_time_of_release,
                                       physiological_manipulation) %in% "manipulated")) %>%
    dplyr::ungroup() %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime,
                  ObserverID, LocationID, CapturePopID, CapturePlot, ReleasePopID, ReleasePlot,
                  Mass, Tarsus, OriginalTarsusMethod, WingLength, Age_observed,
                  Age_calculated, ChickAge, ChickAge2, FoundDead, BroodID, ExperimentID, capture_method)

  return(Capture_data)

}

create_individual_HOC <- function(db){

  #Technically, they already have individual data in a separate table
  #However, we will check this in comparison to capture data
  Individual_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Bird_ID", na = c("", "na"),
                                     col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(IndvID = ring_number, Species = "PARMAJ",
                  Sex = dplyr::case_when(sex == "female" ~ "F",
                                         sex == "male" ~ "M"),
                  PopID = "HOC", RingSeason = lubridate::year(janitor::excel_numeric_to_date(as.numeric(date_ringed))),
                  RingAge = dplyr::case_when(age_simple == "adult" ~ "adult",
                                             age_simple == "nestling" ~ "chick"),
                  BroodIDLaid = nest_of_origin_id,
                  BroodIDFledged = rearing_nest_id) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex)

  return(Individual_data)

}

create_location_HOC <- function(db){

  Location_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Location Data", na = c("", "na"),
                                      col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::mutate(LocationID = paste0("H", nestbox_number),
                  NestboxID = LocationID,
                  LocationType = "NB",
                  PopID = "HOC",
                  Latitude = as.numeric(latitude),
                  Longitude = as.numeric(longitude),
                  StartSeason = 2014L,
                  EndSeason = NA_integer_,
                  Habitat = "mixed")

  return(Location_data)

}
