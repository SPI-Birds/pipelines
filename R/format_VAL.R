#'Construct standard format for data from Valsein, Spain.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Valsein, Spain, administered by the National Museum of Natural Sciences.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Load data}: Data is in one excel spreadsheet with two sheets with brood info
#'and 7 sheets with chick info. We assume that the number/location of sheets will not change,
#'but we need to check this with data owner.
#'
#'\strong{BroodID}: BroodID is a concatenation of Year and BoxID (BoxID_Year).
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

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  #Load all data
  data_file    <- paste0(db, "/VAL_PrimaryData.xlsx")
  early_broods <- readxl::read_excel(data_file, sheet = 1, na = c("", "-")) %>%
    janitor::clean_names()
  late_broods  <- readxl::read_excel(data_file, sheet = 2, na = c("", "-")) %>% #FIXME: Are - NA or 0? e.g. 37_2017 Check with data owner. Alex will check.
    janitor::clean_names()
  chick_data   <- purrr::map_df(.x = 3:9,
                                .f = ~{

                                  readxl::read_excel(data_file,
                                                     sheet = ..1,
                                                     col_type = "text") %>%
                                    janitor::clean_names() %>%
                                    #Remove the row with summary data (it will have no ring value)
                                    dplyr::filter(!is.na(.data$anilla)) %>%
                                    #Add the year (starting at 2011)
                                    dplyr::mutate(year = (2011 - 3 + ..1),
                                                  NestboxID = stringr::str_remove(.data$nido, pattern = "[A-Z]"), ##FIXME: What does the letter before the nest mean? Are B1 and 1 the same nestbox?
                                                                                                                  ## 'B' is a different plot. The laying date data is stored separately.
                                                  Date = janitor::excel_numeric_to_date(as.numeric(.data$fecha))) %>%
                                    dplyr::mutate(tidyr::fill(., .data$NestboxID, .direction = "down"))

                                    })
  experiment_data <- readxl::read_excel(data_file, sheet = length(all_sheets), na = c("", "-")) %>%
    janitor::clean_names() %>%
    dplyr::rename(ExperimentID = spi_code)

  GPS_2014 <- sf::st_read(paste0(db, "/VAL_PrimaryData_GPS2014.gpx"), layer = "waypoints")
  GPS_2015 <- sf::st_read(paste0(db, "/VAL_PrimaryData_GPS2015.gpx"), layer = "waypoints")

  All_GPS <- dplyr::bind_rows(GPS_2014, GPS_2015)

  # BROOD DATA
  #Extract Valsein brood data

  message("Compiling brood data....")

  Brood_data <- create_brood_VAL(early_broods, late_broods, chick_data) #FIXME: Still need to add in average chick tarsus/mass measures.

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_VAL(early_broods, late_broods, chick_data)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_VAL(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_VAL(Brood_data = Brood_data, Capture_data = Capture_data, GPS = All_GPS)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_VAL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_VAL.csv"), row.names = F)

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

#' Create brood data table for Valsein, Spain.
#'
#' Create brood data table in standard format for data from Valsein, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 -)
#' @param chick_data Data frame with data on chick captures
#'
#' @return A data frame.

create_brood_VAL <- function(early_broods, late_broods, chick_data){

  early_broods_format <- early_broods %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")), #FIXME: Are dates April days? i.e. 1 = April 1st. Correct.
                  BroodID = paste(.data$nido, .data$year, sep = "_"),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Plot = NA_character_,
                  LocationID = .data$nido,
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$MarchDay + floor(.data$tmed_ld), #FIXME: Is xxx_ld laying date info? How can there be broods in the same box with similar LD (e.g. 1997_117)?
                                                                            #FPUESTA is LD and PUESTA is clutch size
                  LayDate_min = .data$MarchDay + floor(.data$tmin_ld), #FIXME: These represent error in laying date?
                  LayDate_max = .data$MarchDay + floor(.data$tmax_ld),
                  ClutchSize_observed = .data$incub, #FIXME: The number of eggs being incubated? This is comparable to CS in newer data? NO!
                                                     #FIXME: How is this affected by clutch size manipulation? Is it before or after? CS is before brood size manipulation.
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + .data$hdate,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = .data$hatchl, #FIXME: Number of hatchlings observed? Correct.
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = .data$fledgl, #FIXME: Number of fledlgings observed? Correct.
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  ExperimentID = dplyr::case_when(manip %in% c("aumentado", "reducido") ~ c("COHORT;PARENTAGE"))) %>% #FIXME: What do the other experiment values mean? Alex will check these.
    #When there are multiple nests in a brood in a year, give them different letter suffixes
    dplyr::arrange(.data$FemaleID, .data$LayDate_observed) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::mutate(BroodID = paste0(.data$BroodID, letters[1:n()])) %>% #FIXME: How do we deal with multiple broods? No second clutches, only replacement. XXX bis/b that is a replacement.
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(.)) %>%
    dplyr::select(.data$BroodID:.data$ClutchType_observed,
                  .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$NumberFledged_max)

  late_broods_format <- late_broods %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  BroodID = paste(.data$nest, .data$year, sep = "_"),
                  PopID = "VAL",
                  BreedingSeason = .data$year,
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Plot = NA_character_, #FIXME: Are there separate plots? Yes, there is plot B. ~3km away from main plot.
                  LocationID = .data$nest,
                  FemaleID = .data$female,
                  MaleID = .data$male,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$MarchDay + floor(.data$ld),
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$cs,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = .data$MarchDay + floor(.data$hd),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(.data$cs * .data$hatching_suc/100), #FIXME: Is this proportion of clutch that hatch? Correct.
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = as.integer(.data$cs * .data$fled_suc/100), #FIXME: Is this proportion of clutch that fledge or of brood? Proportion of hatchlings that fledge.
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = .data$chicks_wight, #FIXME: This data is not available in any other cases?
                  AvgTarsus = .data$chicks_tarsus,
                  OriginalTarsusMethod = "Alternative",
                  ExperimentID = NA_character_) %>% #FIXME: Translate 'treatment' column to experimentID
    #When there are multiple nests in a brood in a year, give them different letter suffixes
    dplyr::arrange(.data$FemaleID, .data$LayDate_observed) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::mutate(BroodID = paste0(.data$BroodID, letters[1:n()])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(.)) %>%
    dplyr::select(.data$BroodID:.data$ClutchType_observed,
                  .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$ExperimentID)

  #Determine number of chicks measured in each nest
  number_chicks <- chick_data %>%
    dplyr::mutate(BroodID = paste(.data$NestboxID, .data$year, sep = "_")) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(NumberChicksMass = sum(!is.na(peso)),
                     NumberChicksTarsus = sum(!is.na(tarso)),
                     .groups = "drop")

  #Join in to brood data
  late_broods_format <- late_broods_format %>%
    dplyr::left_join(number_chicks, by = "BroodID") %>%
    dplyr::select(.data$BroodID:.data$AvgChickMass, .data$NumberChicksMass,
                  .data$AvgTarsus, .data$NumberChicksTarsus,
                  .data$OriginalTarsusMethod, .data$ExperimentID)

  all_broods <- dplyr::bind_rows(early_broods_format, late_broods_format)

  return(all_broods)

}

#' Create capture table for Valsein, Spain.
#'
#' Create full capture data table in standard format for data from Valsein, Spain.
#'
#' @param early_broods Data frame with data on early broods (1991 - 2010)
#' @param late_broods Data frame with data on late broods (2011 -)
#' @param chick_data Data frame with data on chick captures
#'
#' @return A data frame.

create_capture_VAL <- function(early_broods, late_broods, chick_data){

  #Extract info on adult captures
  early_adult_captures <- early_broods %>%
    dplyr::select(year, nido, female, ano_anilla_52, fage, surv_fem, fdia, ftarso, fala, fpeso,
                  male, ano_anilla_84, mage, surv_man, mdia, mtarso, mala, mpeso) %>%
    tidyr::pivot_longer(cols = c(female, male), names_to = "Sex_observed", values_to = "IndvID") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  BreedingSeason = .data$year,
                  Sex_observed = dplyr::case_when(.data$Sex_observed == "female" ~ "F",
                                                  .data$Sex_observed == "male" ~ "M"),
                  CaptureDate = dplyr::case_when(Sex_observed == "F" ~ .data$MarchDay + tidyr::replace_na(.data$fdia, 0),
                                                 Sex_observed == "M" ~ .data$MarchDay + tidyr::replace_na(.data$mdia, 0)), # FIXME: What is the capture date? Is it from fdia/mdia? Correct.
                  CaptureTime = NA_character_,
                  ObservedID = NA_character_,
                  LocationID = .data$nido,
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = NA_character_,
                  ReleasePopID = "VAL", ReleasePlot = NA_character_,
                  Mass = dplyr::case_when(.data$Sex_observed == "F" ~ .data$fpeso,
                                          .data$Sex_observed == "M" ~ .data$mpeso),
                  Tarsus = dplyr::case_when(.data$Sex_observed == "F" ~ .data$ftarso,
                                            .data$Sex_observed == "M" ~ .data$mtarso),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = dplyr::case_when(.data$Sex_observed == "F" ~ .data$fala,
                                                .data$Sex_observed == "M" ~ .data$mala),
                  Age_observed = dplyr::case_when(.data$Sex_observed == "F" ~ as.integer(4 + (.data$fage - 1)*2),
                                                  .data$Sex_observed == "M" ~ as.integer(4 + (.data$mage - 1)*2)), ## FIXME: What age measurement is this? Real age (i.e. 1 is born previous year)
                  ## For now, I assume it's starting at 4 (>1yo) and going up in units of year.
                  ## i.e. 1 = age 4; 2 = age 6
                  ChickAge = NA_integer_,
                  ExperimentID = NA_character_) %>%   ## FIXME: Does the MANIP column apply also to adults? Yes, Alex will specify this.
    dplyr::select(IndvID, Species, Sex_observed, BreedingSeason:ExperimentID)

  late_adult_captures <- late_broods %>%
    dplyr::select(year, nest, female, f_age, f_tarsus, f_wing, f_weight, obs_24,
                  male, m_age, m_tarsus, m_wing, m_weight, obs_38) %>%
    tidyr::pivot_longer(cols = c(female, male), names_to = "Sex_observed", values_to = "IndvID") %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  BreedingSeason = .data$year,
                  Sex_observed = dplyr::case_when(.data$Sex_observed == "female" ~ "F",
                                                  .data$Sex_observed == "male" ~ "M"),
                  CaptureDate = .data$MarchDay, ##FIXME: New brood data doesn't have any info on when adults were captured. When would this be? HD + 7
                  CaptureTime = NA_character_,
                  ObservedID = dplyr::case_when(.data$Sex_observed == "F" ~ .data$obs_24,
                                                .data$Sex_observed == "M" ~ .data$obs_38),
                  LocationID = .data$nest,
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = NA_character_,
                  ReleasePopID = "VAL", ReleasePlot = NA_character_,
                  Mass = dplyr::case_when(.data$Sex_observed == "F" ~ .data$f_weight,
                                          .data$Sex_observed == "M" ~ .data$m_weight),
                  Tarsus = dplyr::case_when(.data$Sex_observed == "F" ~ .data$f_tarsus,
                                            .data$Sex_observed == "M" ~ .data$m_tarsus),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = dplyr::case_when(.data$Sex_observed == "F" ~ .data$f_wing,
                                                .data$Sex_observed == "M" ~ .data$m_wing),
                  Age_observed = dplyr::case_when(.data$Sex_observed == "F" ~ as.integer(4 + (.data$f_age * 2)),
                                                  .data$Sex_observed == "M" ~ as.integer(4 + (.data$m_age * 2))), ## FIXME: What age measurement is this? It's real age and Alex will fill in blanks if possible.
                  ## For now, I assume it's starting at 4 (>1yo) and going up in units of year.
                  ## i.e. 0 = age 4; 1 = age 6
                  ChickAge = NA_integer_,
                  ExperimentID = NA_character_) %>%   ## FIXME: Does the treatment column apply also to adults? Can affect adults too.
    dplyr::select(IndvID, Species, Sex_observed, BreedingSeason:ExperimentID)

  #FIXME: No information on chick rings before 2011. Were chicks not ringed before this or the data is missing? This data is not digitised.
  early_chick <- chick_data %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  IndvID = .data$anilla,
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Sex_observed = NA_character_,
                  BreedingSeason = .data$year,
                  CaptureDate = .data$Date,
                  CaptureTime = .data$hora,
                  ObserverID = NA_character_,
                  LocationID = .data$NestboxID,
                  CaptureAlive = TRUE, ReleaseAlive = TRUE, ## FIXME: Are these ever FALSE? Chicks died is the difference between BS13 and 3.
                  CapturePopID = "VAL", CapturePlot = NA_character_,
                  ReleasePopID = "VAL", ReleasePlot = NA_character_,
                  Mass = as.numeric(.data$peso),
                  Tarsus = as.numeric(.data$tarso),
                  OriginalTarsusMethod = "Alternative",
                  WingLength = as.numeric(.data$ala),
                  Age_observed = 1L, # FIXME: Assume all are chicks (pre-fledgling). Always caught in nest.
                  ChickAge = NA_integer_, # FIXME: What age are chicks caught/ringed? 13 days? Correct, 13 days.
                  ExperimentID = NA_character_) %>%
    dplyr::select(IndvID:ExperimentID)

  late_chick <- late_broods %>%
    dplyr::select(.data$nest, .data$year, .data$hd, .data$chick1:last_col()) %>%
    tidyr::pivot_longer(cols = .data$chick1:last_col(), values_to = "IndvID") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::mutate(MarchDay = as.Date(paste(.data$year, "03", "31", sep = "-")),
                  Species = Species_codes$Code[Species_codes$SpeciesID == 13490],
                  Sex_observed = NA_character_,
                  BreedingSeason = .data$year,
                  CaptureDate = MarchDay + .data$hd + 13, #FIXME: Assume chicks are captured on day 13. Check with data owner. Yes, 13 days.
                  CaptureTime = NA_character_,
                  ObserverID = NA_character_, #FIXME: Is this the same as one of the female/male observers? Not recorded, Alex will check it.
                  LocationID = .data$nest,
                  CaptureAlive = TRUE, ReleaseAlive = TRUE,
                  CapturePopID = "VAL", CapturePlot = NA_character_,
                  ReleasePopID = "VAL", ReleasePlot = NA_character_,
                  Mass = NA_real_, #FIXME: These chicks are not measured? Alex will provide this for the last 3 years.
                  Tarsus = NA_real_,
                  OriginalTarsusMethod = "Alternative",
                  WingLength = NA_real_,
                  Age_observed = 1L,
                  ChickAge = 13L,
                  ExperimentID = NA_character_) %>%
    dplyr::select(IndvID, Species:ExperimentID)

  all_captures <- dplyr::bind_rows(early_adult_captures, late_adult_captures, early_chick, late_chick) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::mutate(CaptureID = paste(IndvID, 1:n(), sep = "_")) %>%
    dplyr::select(CaptureID, IndvID:Age_observed, Age_calculated, ChickAge, ExperimentID)

  return(all_captures)

}

#' Create individual table for Valsein, Spain.
#'
#' Create full individual data table in standard format for data from Valsein, Spain.
#'
#' @param Capture_data Output of \code{\link{create_capture_VAL}}.
#'
#' @return A data frame.

create_individual_VAL <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
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

create_location_VAL <- function(Brood_data, GPS){

  #Extract latitude and longitude from gps file
  GPS <- GPS %>%
    dplyr::bind_cols(as_tibble(sf::st_coordinates(.))) %>%
    dplyr::rename(Longitude = X, Latitude = Y) %>%
    dplyr::select(NestboxID = name, Longitude, Latitude) %>%
    sf::st_drop_geometry() %>%
    #Where there are multiple records from the same box, just take the first one
    #There is no box movement so these are duplicate
    dplyr::group_by(NestboxID) %>%
    slice(1)

  Location_data <- Brood_data %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestboxID = first(.data$LocationID),
                  LocationType = "NB",
                  PopID = "VAL",
                  StartSeason = as.integer(min(.data$BreedingSeason)),
                  EndSeason = NA_integer_, # Boxes are not removed. Some boxes may be used by BT/GT so aren't found here. GT/BT data is collected by other researcher. Alex will contact/give me his contact.
                  HabitatType = "DEC",
                  .groups = "drop") %>%
    dplyr::left_join(GPS, by = "NestboxID") %>%
    dplyr::select(LocationID:PopID, Latitude, Longitude, everything())

  return(Location_data)

}
