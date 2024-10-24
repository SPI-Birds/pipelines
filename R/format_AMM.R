#'Construct standard format for data from Ammersee, Germany
#'
#'A pipeline to produce the standard format for the nest box population in Ammersee,
#'Germany, administered by Niels Dingemanse.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{Species}: Species code 5 refers to Willow tit/Marsh tit. As we cannot
#'confidently identify species, these are considered Unknown and removed. This
#'currently only occurs once in the data. Similarly, species code 0 refers to
#''unknown' and is treated as NA and removed.
#'
#'\strong{ClutchType_observed}: Code 6 'Replacement First Replacement' is considered to be
#'a 2nd replacement after the first (i.e. not successful clutch) and is defined as 'replacement'.
#'Code 4 'Replacement second' is counted as 'second' in our method because it has come after at least
#'one successful clutch. Code 5 'Probably replacement first or second' is considered unknown and treated
#'as NA.
#'
#'\strong{ClutchSize}: In a small number of cases (13 as of 2019 data) there are eggs from multiple species.
#'In this case, we are giving the clutch size as the number of eggs of both species. We are thinking
#'about ways to more effectively deal with multi-species clutches.
#'
#'\strong{BroodSize}: Use NumberHatched to determine observed BroodSize. BroodSize_minimum is equal to BroodSize
#'observed. BroodSize_maximum is NumberHatched + ErrorHatched (i.e. number of eggs never observed either alive
#'or dead, so status was unknown).
#'
#'\strong{NumberFledged}: Use NumberFledged to determine observed number of fledglings.
#'NumberFledged_minimum is equal to BroodSize_observed.
#'BroodSize_maximum is NumberFledged + ErrorFledged (i.e. number of chicks never observed either alive
#'or dead, so status was unknown).
#'
#'\strong{HatchDate}: The day a clutch hatched.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export

format_AMM <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R"){

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force choose_directory() if used
  force(db)

  # Assign to database location
  dsn <- paste0(gsub("\\\\", "/", db), "\\AMM_PrimaryData.accdb")

  # Assign species for filtering
  # If no species are specified, all species are included
  if(is.null(species)){

    species_filter <- species_codes$Species

  } else {

    species_filter <- species

  }

  start_time <- Sys.time()

  message("Extracting Access tables...")

  # Connect to Access database and export relevant tables to a selected output directory
  access_tables <- c("Catches", "NestBoxes", "Broods",
                     "Chicks", "GenotypesAllYears", "HabitatDescription")

  table_dir <- paste0(db, "/AMM_PrimaryData_tables")

  export_access_db(dsn,
                   table = access_tables,
                   output_dir = table_dir)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_AMM(dir = table_dir,
                                 species_filter = species_filter)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_AMM(Brood_data = Brood_data,
                                     dir = table_dir,
                                     species_filter = species_filter)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_AMM(Capture_data = Capture_data,
                                           Brood_data = Brood_data,
                                           dir = table_dir)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_AMM(Capture_data = Capture_data,
                                       dir = table_dir)

  # WRANGLE DATA FOR EXPORT

  #Calculate average tarsus per brood
  AvgTarsus <- Capture_data %>%
    dplyr::filter(.data$ChickAge == 14L & !is.na(.data$Tarsus)) %>%
    dplyr::group_by(.data$BroodID) %>% ##FIXME: We are taking average tarsus length of all chicks from the
    ##genetic brood not the actual brood where they were measured!!!!!!!!!!!!!!
    ##Do we want this to be the norm? Or would be rather use the average
    ##of the brood of measurement (i.e. after cross-fostering)
    dplyr::summarise(AvgTarsus = mean(.data$Tarsus),
                     NumberChicksTarsus = dplyr::n(),
                     OriginalTarsusMethod = "Alternative")

  #Add average tarsus
  Brood_data <- Brood_data %>%
    dplyr::left_join(AvgTarsus, by = "BroodID") %>%
    dplyr::select("BroodID":"NumberChicksMass", "AvgTarsus",
                  "NumberChicksTarsus", "OriginalTarsusMethod", "ExperimentID")

  #Remove BroodID, no longer needed
  Capture_data <- Capture_data %>%
    dplyr::select("CaptureID":"Age_observed", "Age_calculated", "ChickAge", "ExperimentID")

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if (output_type == 'csv') {

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_AMM.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_AMM.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_AMM.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_AMM.csv"), row.names = FALSE)

    utils::write.table(x = protocol_version, file = paste0(path, "\\protocol_version_AMM.txt"),
                       quote = FALSE, row.names = FALSE, col.names = FALSE)


    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data,
                protocol_version = protocol_version))

  }

}

#' Create brood data table for Ammersee, Germany.
#'
#' Create brood data table in standard format for data from Ammersee, Germany.
#'
#' @param dir Path to directory containing the relevant table exports from the AMM Access database.
#' @param species_filter Species six-letter codes from the standard protocol. Used to filter the data.
#'
#' @return A data frame.

create_brood_AMM   <- function(dir, species_filter) {

  Catches_M <- utils::read.csv(paste0(dir, "/", "Catches", ".csv")) %>%
    dplyr::select("BroodID", "MaleID" = "BirdID", "SexConclusion") %>%
    dplyr::filter(!is.na(.data$BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$SexConclusion == 2L) %>%
    dplyr::select(-"SexConclusion")

  Catches_F <- utils::read.csv(paste0(dir, "/", "Catches", ".csv")) %>%
    dplyr::select("BroodID", "FemaleID" = "BirdID", "SexConclusion") %>%
    dplyr::filter(!is.na(.data$BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$SexConclusion == 1L) %>%
    dplyr::select(-"SexConclusion")

  Nest_boxes <- utils::read.csv(paste0(dir, "/", "NestBoxes", ".csv")) %>%
    dplyr::select("NestBox", "Plot")

  Brood_data <- utils::read.csv(paste0(dir, "/", "Broods", ".csv")) %>%
    dplyr::left_join(Catches_F,
                     by = c("BroodID")) %>%
    dplyr::left_join(Catches_M,
                     by = c("BroodID")) %>%
    dplyr::left_join(Nest_boxes,
                     by = c("NestBox" = "NestBox")) %>%
    # Remove -99 and replace with NA
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(~is.numeric(.)),
                                .fns = ~dplyr::na_if(., -99))) %>%
    # For day data, remove any cases >= 500. These equate to NA
    dplyr::mutate(dplyr::across(.cols = c("HatchDay", "FledgeDay"),
                                .fns = ~{

                                  dplyr::case_when(. >= 500 ~ NA_integer_,
                                                   TRUE ~ .)

                                })) %>%
    dplyr::mutate(BroodID = as.character(.data$BroodID),
                  BreedingSeason = .data$BroodYear,
                  PopID = "AMM",
                  EndMarch = as.Date(paste(.data$BroodYear, "03", "31", sep = "-")),
                  LayDate_observed = .data$EndMarch + .data$FirstEggDay,
                  LayDate_maximum = .data$LayDate_observed,
                  LayDate_minimum = .data$EndMarch + (.data$FirstEggDay - .data$FirstEggDayError),
                  ClutchSize_observed = .data$ClutchSize,
                  ClutchSize_minimum = .data$ClutchSize_observed,
                  ClutchSize_maximum = .data$ClutchSize_observed,
                  HatchDate_observed = .data$EndMarch + .data$HatchDay,
                  HatchDate_maximum = .data$HatchDate_observed,
                  HatchDate_minimum = .data$EndMarch + (.data$HatchDay - .data$HatchDayError),
                  BroodSize_observed = .data$NumberHatched,
                  BroodSize_minimum = .data$NumberHatched,
                  BroodSize_maximum = .data$NumberHatched + .data$ErrorHatched,
                  FledgeDate_observed = .data$EndMarch + .data$FledgeDay,
                  FledgeDate_minimum = .data$EndMarch + .data$FledgeDay,
                  FledgeDate_maximum = .data$EndMarch + .data$FledgeDay,
                  NumberFledged_observed = .data$NumberFledged,
                  NumberFledged_minimum = .data$NumberFledged,
                  NumberFledged_maximum = .data$NumberFledged + .data$FledgedError,
                  Species = dplyr::case_when(.data$Species == 1L ~ !!species_codes$Species[species_codes$SpeciesID == "14640"],
                                             .data$Species == 2L ~ !!species_codes$Species[species_codes$SpeciesID == "14620"],
                                             .data$Species == 3L ~ !!species_codes$Species[species_codes$SpeciesID == "14610"],
                                             .data$Species == 4L ~ !!species_codes$Species[species_codes$SpeciesID == "14790"]),
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_integer_,
                  NumberChicksMass = NA_integer_,
                  BroodSwap_ExperimentID = ifelse(.data$BroodSwap > 0L, list(c("PARENTAGE", "COHORT")), list(NA_character_)),
                  BroodOther_ExperimentID = dplyr::case_when(.data$BroodOtherTreatment %in% c(1L, 2L, 3L, 4L, 5L) ~ list("SURVIVAL"),
                                                             TRUE ~ list(NA_character_)),
                  Plot_ExperimentID = dplyr::case_when(.data$PlotLevelTreatment %in% c(1L, 2L) ~ list("PHENOLOGY"),
                                                       .data$PlotLevelTreatment == 3L ~ list(c("PHENOLOGY", "COHORT", "SURVIVAL")),
                                                       TRUE ~ list(NA_character_))) %>%
    # Combine all experiments together
    dplyr::rowwise() %>%
    dplyr::mutate(ExperimentID = paste(dplyr::union(dplyr::union(.data$BroodSwap_ExperimentID,
                                                                 .data$BroodOther_ExperimentID),
                                                    .data$Plot_ExperimentID),
                                       collapse = ";")) %>%
    dplyr::ungroup() %>%
    # Remove NAs from the experiment list
    dplyr::mutate(ExperimentID = stringr::str_remove_all(.data$ExperimentID, pattern = "NA[;]*|;NA^")) %>%
    # Determine clutch type
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"),
                  ClutchType_observed = dplyr::case_when(.data$ClutchNumber == 1L ~ "first",
                                                         .data$ClutchNumber %in% c(2L, 4L) ~ "second",
                                                         .data$ClutchNumber %in% c(3L, 5L, 6L) ~ "replacement")) %>%
  # Arrange columns
  dplyr::select("BroodID", "PopID", "BreedingSeason",
                "Species", "Plot", "LocationID" = "NestBox", "FemaleID", "MaleID",
                "ClutchType_observed", "ClutchType_calculated",
                "LayDate_observed", "LayDate_minimum", "LayDate_maximum",
                "ClutchSize_observed", "ClutchSize_minimum", "ClutchSize_maximum",
                "HatchDate_observed", "HatchDate_minimum", "HatchDate_maximum",
                "BroodSize_observed", "BroodSize_minimum",
                "BroodSize_maximum",
                "FledgeDate_observed", "FledgeDate_minimum",
                "FledgeDate_maximum",
                "NumberFledged_observed", "NumberFledged_minimum",
                "NumberFledged_maximum",
                "AvgEggMass", "NumberEggs",
                "AvgChickMass", "NumberChicksMass",
                "ExperimentID") %>%
    # Convert to correct formats
    dplyr::mutate(dplyr::across(c("Plot":"MaleID"),
                                as.character)) %>%
    dplyr::mutate(dplyr::across(c("LayDate_observed", "HatchDate_observed", "FledgeDate_observed"),
                                as.Date)) %>%
    # Filter species
    dplyr::filter(.data$Species %in% species_filter)

  return(Brood_data)

}

#' Create capture data table for Ammersee, Germany
#'
#' Create capture data table in standard format for data from Ammersee, Germany.
#'
#' @param Brood_data Data frame. Output from \code{\link{create_brood_AMM}}.
#' @param dir Path to directory containing the relevant table exports from the AMM Access database.
#' @param species_filter Species six-letter codes from the standard protocol. Used to filter the data.
#'
#' @return A data frame.

create_capture_AMM <- function(Brood_data, dir, species_filter) {

  Catches_table <- utils::read.csv(paste0(dir, "/", "Catches", ".csv"))

  Chick_catch_tables <- utils::read.csv(paste0(dir, "/", "Chicks", ".csv"))

  Nestbox_capture <- utils::read.csv(paste0(dir, "/", "NestBoxes", ".csv")) %>%
    dplyr::select("NestBox", "CapturePlot" = "Plot")

  Nestbox_release <- utils::read.csv(paste0(dir, "/", "NestBoxes", ".csv")) %>%
    dplyr::select("NestBox", "ReleasePlot" = "Plot")

  #Adult captures
  Adult_capture <- Catches_table %>%
    dplyr::left_join(Nestbox_capture, by = "NestBox") %>%
    dplyr::mutate(Species = dplyr::case_when(.data$CatchSpecies == 1L ~ !!species_codes$Species[species_codes$SpeciesID == "14640"],
                                             .data$CatchSpecies == 2L ~ !!species_codes$Species[species_codes$SpeciesID == "14620"],
                                             .data$CatchSpecies == 3L ~ !!species_codes$Species[species_codes$SpeciesID == "14610"],
                                             .data$CatchSpecies == 4L ~ !!species_codes$Species[species_codes$SpeciesID == "14790"]),
                  CaptureTimeField = lubridate::as_datetime(.data$CatchTimeField, format = "%Y-%m-%dT%H:%M"),
                  CaptureTime = format(.data$CaptureTimeField, "%H:%M"),
                  IndvID = as.character(.data$BirdID),
                  BreedingSeason = .data$CatchYear,
                  CaptureDate = as.Date(.data$CatchDate),
                  CapturePopID = "AMM",
                  ReleasePopID = "AMM",
                  CapturePlot = as.character(.data$CapturePlot),
                  ReleasePlot = .data$CapturePlot,
                  LocationID = as.character(.data$NestBox),
                  OriginalTarsusMethod = "Alternative",
                  ## These age categories match our EURING codes exactly
                  Age_observed = dplyr::case_match(.data$AgeObserved,
                                                    7 ~ NA_integer_,
                                                    0 ~ NA_integer_,
                                                   .default = .data$AgeObserved),
                  ChickAge = NA_integer_,
                  ObserverID = as.character(dplyr::na_if(.data$FieldObserver, -99L)),
                  BroodID = as.character(.data$BroodID),
                  Sex_observed = dplyr::case_when(.data$SexObserved == 1L ~ "F",
                                                  .data$SexObserved == 2L ~ "M",
                                                  TRUE ~ NA_character_),
                  #If taken to the lab for personality tests this could affect survival
                  #Attaching a transponder could also affect survival
                  ExperimentID = dplyr::case_when(.data$ToLab == 1L ~ "SURVIVAL",
                                                  .data$Transponder == 1L ~ "SURVIVAL",
                                                  TRUE ~ NA_character_),
                  CaptureAlive = dplyr::case_when(.data$CatchType %in% 13L:15L ~ FALSE,
                                                  TRUE ~ TRUE),
                  ReleaseAlive = dplyr::case_when(.data$CatchType %in% 13L:15L ~ FALSE,
                                                  .data$DeadOrAlive == 0L ~ FALSE,
                                                  TRUE ~ TRUE)) %>%
    dplyr::mutate(dplyr::across(.cols = c("BodyMassField", "Tarsus", "WingP3"),
                                .fns = ~ifelse(. <= 0, NA_real_, .))) %>%
    dplyr::select("IndvID",
                  "Species",
                  "Sex_observed",
                  "BreedingSeason",
                  "CaptureDate",
                  "CaptureTime",
                  "ObserverID",
                  "LocationID",
                  "CapturePopID",
                  "CapturePlot",
                  "ReleasePopID",
                  "ReleasePlot",
                  "Mass" = "BodyMassField",
                  "Tarsus",
                  "WingLength" = "WingP3",
                  "Age_observed",
                  "ChickAge",
                  "ExperimentID",
                  "BroodID")

  Chick_capture <- Chick_catch_tables %>%
    dplyr::filter(.data$Egg %in% c(-99L, 0L, 2L)) %>%
    dplyr::left_join(Nestbox_capture, by = "NestBox") %>%
    dplyr::left_join(Nestbox_release, by = c("SwapToNestBox" = "NestBox")) %>%
    dplyr::mutate(EndMarch = as.Date(paste(.data$ChickYear, "03", "31", sep = "-")),
                  CapturePopID = "AMM", ReleasePopID = "AMM") %>%
    #Hatchday >500 and <0 should be NA
    dplyr::mutate(HatchDay = dplyr::case_when((.data$HatchDay >= 500 | .data$HatchDay < 0) ~ NA_integer_,
                                              TRUE ~ .data$HatchDay),
                  # All chicks are GT
                  Species = species_codes$Species[species_codes$SpeciesID == 14640],
                  Sex_observed = NA_character_) %>%
    dplyr::select("Species", "Sex_observed", "BirdID", "ChickYear", "EndMarch", "NestBox", "BroodID",
                  "CapturePlot", "ReleasePlot",
                  "CapturePopID", "ReleasePopID",
                  "HatchDay", "Day2BodyMass", "Day2BodyMassTime",
                  "Day2Observer" = "SwapObserver",
                  "Day6BodyMass", "Day6BodyMassTime", "Day6Observer",
                  "Day14P3", "Day14Tarsus", "Day14BodyMass", "Day14BodyMassTime",
                  "Day14Observer", "Day18BodyMass" = "Day18Bodymass", "Day18BodyMassTime", "Day18Observer") %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                ~dplyr::na_if(as.character(.), "-99"))) %>% #Needed because pivot functions expect coercable classes when making value col
    dplyr::mutate(dplyr::across(tidyselect::contains("BodyMassTime"),
                                ~stringr::str_extract(., "[0-9]{2}:[0-9]{2}"))) %>%
    tidyr::pivot_longer(cols = "Day2BodyMass":"Day18Observer",
                        names_to = "column",
                        values_to = "value") %>%
    dplyr::mutate(Day = stringr::str_extract(.data$column, "[0-9]{1,}"),
                  OriginalTarsusMethod = "Alternative",
                  Age_observed = 1L) %>%
    tidyr::separate(.data$column, into = c(NA, "Variable"), sep = "^Day[0-9]{1,}") %>%
    tidyr::pivot_wider(names_from = "Variable", values_from = "value") %>%
    #Mutate columns back to correct type
    dplyr::mutate(dplyr::across(c("ChickYear", "HatchDay", "Day"), as.integer)) %>%
    dplyr::mutate(dplyr::across(c("BodyMass", "P3", "Tarsus"),
                                ~ifelse(as.numeric(.) <= 0, NA_real_, as.numeric(.)))) %>%
    dplyr::filter(!(is.na(.data$BodyMass) & is.na(.data$P3) & is.na(.data$Tarsus))) %>%
    dplyr::mutate(CaptureDate = as.Date(.data$EndMarch) + .data$HatchDay + .data$Day) %>%
    dplyr::select("IndvID" = "BirdID",
                  "Species",
                  "Sex_observed",
                  "BreedingSeason" = "ChickYear",
                  "CaptureDate",
                  "CaptureTime" = "BodyMassTime",
                  "ObserverID" = "Observer",
                  "LocationID" = "NestBox",
                  "CapturePopID",
                  "CapturePlot",
                  "ReleasePopID",
                  "ReleasePlot",
                  "Mass" = "BodyMass",
                  "Tarsus",
                  "WingLength" = "P3",
                  "Age_observed",
                  "ChickAge" = "Day",
                  "BroodID")

  Capture_data <- dplyr::bind_rows(Adult_capture, Chick_capture) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    # Filter species
    dplyr::filter(.data$Species %in% species_filter) %>%
    #Arrange by ID/Date and add unique capture ID
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::select("CaptureID", tidyselect::everything())

  return(Capture_data)

}

#' Create individual data table for Ammersee, Germany
#'
#' Create individual data table in standard format for data from Ammersee, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_AMM}}.
#' @param Brood_data Data frame. Output from \code{\link{create_brood_AMM}}.
#' @param dir Path to directory containing the relevant table exports from the AMM Access database.
#'
#' @return A data frame.

create_individual_AMM <- function(Capture_data, Brood_data, dir) {

  Sex_genetic <- utils::read.csv(paste0(dir, "/", "GenotypesAllYears", ".csv")) %>%
    dplyr::select("IndvID" = "BirdID", "SexGenetic") %>%
    dplyr::filter(!is.na(.data$IndvID) & !is.na(.data$SexGenetic) & .data$SexGenetic > 0) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$SexGenetic)),
                     unique_sex = list(unique(.data$SexGenetic))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(IndvID = as.character(.data$IndvID),
                  Sex_genetic = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                 .data$unique_sex[[1]] == 1L ~ "F",
                                                 .data$unique_sex[[1]] == 2L ~ "M")) %>%
    dplyr::ungroup() %>%
    dplyr::select("IndvID", "Sex_genetic")

  Sex_calc <- Capture_data %>%
    dplyr::filter(!is.na(.data$Sex_observed)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$Sex_observed)),
                     unique_sex = list(unique(.data$Sex_observed))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                    TRUE ~ .data$unique_sex[[1]])) %>%
    dplyr::ungroup() %>%
    dplyr::select("IndvID", "Sex_calculated")

  Brood_swap_info <- utils::read.csv(paste0(dir, "/", "Chicks", ".csv")) %>%
    dplyr::select("IndvID" = "BirdID", "BroodIDLaid" = "BroodID", "BroodIDFledged" = "SwapToBroodID") %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

  Individual_data <- Capture_data %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Species))),
                                              .f = ~{

                                                if(length(..1) == 0){

                                                  return(NA_character_)

                                                } else if(length(..1) == 1){

                                                  return(..1)

                                                } else {

                                                  return("CCCCCC")

                                                }

                                              }),
                     PopID = "AMM",
                     RingSeason = min(.data$BreedingSeason),
                     RingAge = ifelse(min(.data$Age_observed) != 1L | is.na(min(.data$Age_observed)),
                                      "adult", "chick")) %>%
    dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::left_join(Sex_genetic, by = "IndvID") %>%
    dplyr::left_join(Brood_swap_info, by = "IndvID") %>%
    dplyr::select("IndvID", "Species", "PopID",
                  "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge",
                  "Sex_calculated", "Sex_genetic")

  return(Individual_data)

}

#' Create location data table for Ammersee, Germany.
#'
#' Create location data table in standard format for data from Ammersee, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_AMM}}.
#' @param dir Path to directory containing the relevant table exports from the AMM Access database.
#'
#' @return A data frame.

create_location_AMM <- function(Capture_data, dir) {

  Habitat_data <- utils::read.csv(paste0(dir, "/", "HabitatDescription", ".csv")) %>%
    dplyr::select("NestBox", "Beech":"OtherTree") %>%
    dplyr::group_by(.data$NestBox) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
    dplyr::summarise(DEC = sum(c(.data$Beech, .data$Larch, .data$Maple, .data$Birch,
                                 .data$Oak, .data$Willow, .data$Poplar, .data$Alder, .data$AshTree)),
                     EVE = sum(c(.data$Spruce, .data$Pine))) %>%
    dplyr::mutate(perc_dec = .data$DEC/(.data$DEC + .data$EVE),
                  dominant_sp = dplyr::case_when(.data$perc_dec >= 0.66 ~ "DEC",
                                                 .data$perc_dec < 0.33 ~ "EVE",
                                                 TRUE ~ "MIX"))

  #The vast majority of nest boxes (90%) of nest boxes are surrounded by deciduous or mixed stands. Therefore,
  #we use the 'DEC' category to describe the population.
  #In the future, we could include a nest-box specific habitat type, but that would require us to decide
  #the range over which habitat is sampled (the nest box vicinity, the plot?). To do later.
  table(Habitat_data$dominant_sp)

  start_year <- min(Capture_data$BreedingSeason)

  Location_data <- utils::read.csv(paste0(dir, "/", "NestBoxes", ".csv")) %>%
    dplyr::filter(.data$NestBox != -99L) %>%
    dplyr::mutate(LocationID = as.character(.data$NestBox),
                  NestboxID = as.character(.data$NestBox),
                  Latitude = round(as.numeric(.data$CoordinateLatitude2013), digits = 5), ## Needed because these variables are stored as
                  Longitude = round(as.numeric(.data$CoordinateLongitude2013), digits = 5), ## named vectors, not regular numeric vectors
                  LocationType = "NB",
                  PopID = "AMM",
                  StartSeason = start_year,
                  EndSeason = dplyr::case_when(nchar(.data$NestboxID) == 4 & stringr::str_sub(.data$NestboxID, 1, 2) == "16" ~ 2016L,
                                               TRUE ~ 2019L),
                  HabitatType = "DEC") %>%
    dplyr::select("LocationID",
                  "NestboxID",
                  "LocationType",
                  "PopID",
                  "Latitude",
                  "Longitude",
                  "StartSeason",
                  "EndSeason",
                  "HabitatType")

  return(Location_data)

}
