#'Construct standard format for data from Ammersee, Germany
#'
#'A pipeline to produce the standard format for the nest box population in Ammersee,
#'Germany, administered by Niels Digamanse.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: Species code 5 refers to Willow tit/Marsh tit. As we cannot
#'confidently identify species, these are considered Unknown and removed. This
#'currently only occurs once in the data. Similarly, species code 0 refers to
#''unknown' and is treated as NA and removed.
#'
#'\strong{ClutchType_observed}: Code 6 'Replacement First Replacement' is considered to be
#'a 2nd replacement after the first (i.e. not successful clutch) and is defined as 'replacement'.
#'Code 4 'Replacement second' is counted as 'second' in our method because it has come after at least
#'one successful clutch. Code 5 'Probably repalcement first or second' is considered unknown and treated
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
#'\strong{NumberFledged}: Use NumberFledged to determine observed number of fledlings.
#'NumberFledged_minimum is equal to BroodSize_observed.
#'BroodSize_maximum is NumberFledged + ErrorFledged (i.e. number of chicks never observed either alive
#'or dead, so status was unknown).
#'
#'\strong{HatchDate}: The day a clutch hatched.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_AMM <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  force(db)

  #Assign to database location
  db <- paste0(db, "\\AMM_PrimaryData.accdb")

  #Assign species for filtering
  #If no species are specified, all species are included
  if(is.null(species)){

    species_filter <- Species_codes$Code

  } else {

    species_filter <- species

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  ###N.B. IF THE ACCESS DRIVER AND VERSION OF R ARE NOT 64 BIT THIS WILL RETURN AN ERROR
  #Connect to the AMM database backend.
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_AMM(connection)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_AMM(Brood_data, connection)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_AMM(Capture_data = Capture_data, Brood_data = Brood_data, connection = connection)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_AMM(Capture_data, connection)

  # WRANGLE DATA FOR EXPORT

  #Calculate average tarsus per brood
  AvgTarsus <- Capture_data %>%
    dplyr::filter(.data$ChickAge == 14L & !is.na(.data$Tarsus)) %>%
    dplyr::group_by(.data$BroodID) %>% ##FIXME: We are taking average tarsus length of all chicks from the
    ##genetic brood not the actual brood where they were measured!!!!!!!!!!!!!!
    ##Do we want this to be the norm? Or would be rather use the average
    ##of the brood of measurement (i.e. after cross-fostering)
    dplyr::summarise(AvgTarsus = mean(.data$Tarsus),
                     NumberChicksTarsus = n(),
                     OriginalTarsusMethod = "Alternative")

  #Add average tarsus
  Brood_data <- Brood_data %>%
    dplyr::left_join(AvgTarsus, by = "BroodID") %>%
    dplyr::select(.data$BroodID:.data$NumberChicksMass, .data$AvgTarsus,
                  .data$NumberChicksTarsus, .data$OriginalTarsusMethod, .data$ExperimentID)

  #Remove BroodID, no longer needed
  Capture_data <- Capture_data %>%
    dplyr::select(.data$CaptureID:.data$Age_observed, .data$Age_calculated, .data$ChickAge)

  #Disconnect from database
  DBI::dbDisconnect(connection)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if (output_type == 'csv') {

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_AMM.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_AMM.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_AMM.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_AMM.csv"), row.names = F)

    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}

#' Create brood data table for Ammersee, Germany.
#'
#' Create brood data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_brood_AMM   <- function(connection) {

  Catches_M <- dplyr::tbl(connection, "Catches") %>%
    dplyr::select(.data$BroodID, MaleID = .data$BirdID, .data$SexConclusion) %>%
    dplyr::filter(!is.na(BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$SexConclusion == 2L) %>%
    dplyr::select(-.data$SexConclusion)

  Catches_F <- dplyr::tbl(connection, "Catches") %>%
    dplyr::select(.data$BroodID, FemaleID = .data$BirdID, .data$SexConclusion) %>%
    dplyr::filter(!is.na(BroodID)) %>%
    dplyr::distinct() %>%
    dplyr::filter(.data$SexConclusion == 1L) %>%
    dplyr::select(-.data$SexConclusion)

  Nest_boxes <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::select(.data$NestBox, .data$Plot)

  Brood_data <- dplyr::tbl(connection, "Broods") %>%
    dplyr::left_join(Catches_F, by = c("BroodID")) %>%
    dplyr::left_join(Catches_M, by = c("BroodID")) %>%
    dplyr::left_join(Nest_boxes, by = c("NestBox" = "NestBox")) %>%
    dplyr::collect() %>%
    #Remove -99 and replace with NA
    dplyr::mutate_all(~dplyr::na_if(., -99)) %>%
    #For day data, remove any cases >= 500. These equate to NA
    dplyr::mutate_at(vars(HatchDay, FledgeDay), ~{

      dplyr::case_when(. >= 500 ~ NA_integer_,
                       TRUE ~ .)

    }) %>%
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
                  Species = dplyr::case_when(.data$Species == 1L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14640"],
                                             .data$Species == 2L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14620"],
                                             .data$Species == 3L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14610"],
                                             .data$Species == 4L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14790"]),
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  AvgChickMass = NA_integer_,
                  NumberChicksMass = NA_integer_,
                  BroodSwap_ExperimentID = ifelse(.data$BroodSwap > 0L, "PARENTAGE/COHORT", NA_character_),
                  BroodOther_ExperimentID = dplyr::case_when(.data$BroodOtherTreatment %in% c(1L, 2L, 3L, 4L, 5L) ~ "SURVIVAL",
                                                             TRUE ~ NA_character_),
                  Plot_ExperimentID = dplyr::case_when(.data$PlotLevelTreatment %in% c(1L, 2L) ~ "PHENOLOGY",
                                                       .data$PlotLevelTreatment == 3L ~ "PHENOLOGY;COHORT;SURVIVAL",
                                                       TRUE ~ NA_character_),
                  ExperimentID = paste(.data$BroodSwap_ExperimentID, .data$BroodOther_ExperimentID, .data$Plot_ExperimentID, sep = ";")) %>%
    # Determine clutch type
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE),
                  ClutchType_observed = dplyr::case_when(.data$ClutchNumber == 1L ~ "first",
                                                         .data$ClutchNumber %in% c(2L, 4L) ~ "second",
                                                         .data$ClutchNumber %in% c(3L, 5L, 6L) ~ "replacement")) %>%
    # Arrange columns
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                  .data$Species, .data$Plot, LocationID = .data$NestBox, .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed, .data$LayDate_minimum, .data$LayDate_maximum,
                  .data$ClutchSize_observed, .data$ClutchSize_minimum, .data$ClutchSize_maximum,
                  .data$HatchDate_observed, .data$HatchDate_minimum, .data$HatchDate_maximum,
                  .data$BroodSize_observed, .data$BroodSize_minimum,
                  .data$BroodSize_maximum,
                  .data$FledgeDate_observed, .data$FledgeDate_minimum,
                  .data$FledgeDate_maximum,
                  .data$NumberFledged_observed, .data$NumberFledged_minimum,
                  .data$NumberFledged_maximum,
                  .data$AvgEggMass, .data$NumberEggs,
                  .data$AvgChickMass, .data$NumberChicksMass,
                  .data$ExperimentID) %>%
    # Convert to correct formats
    dplyr::mutate_at(.vars = vars(.data$Plot:.data$MaleID), as.character) %>%
    dplyr::mutate_at(.vars = vars(.data$LayDate_observed, .data$HatchDate_observed, .data$FledgeDate_observed), as.Date)

  Brood_data

}

#' Create capture data table for Ammersee, Germany
#'
#' Create capture data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_capture_AMM <- function(Brood_data, connection) {

  Catches_table      <- dplyr::tbl(connection, "Catches")
  Chick_catch_tables <- dplyr::tbl(connection, "Chicks")
  Nestbox_capture    <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::select(.data$NestBox, CapturePlot = .data$Plot)
  Nestbox_release    <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::select(.data$NestBox, ReleasePlot = .data$Plot)

  #Adult captures
  Adult_capture <- Catches_table %>%
    dplyr::left_join(Nestbox_capture, by = "NestBox") %>%
    dplyr::collect() %>%
    dplyr::mutate(Species = dplyr::case_when(.data$CatchSpecies == 1L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14640"],
                                   .data$CatchSpecies == 2L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14620"],
                                   .data$CatchSpecies == 3L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14610"],
                                   .data$CatchSpecies == 4L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14790"]),
                  CaptureTime = dplyr::na_if(paste(lubridate::hour(.data$CatchTimeField),
                                      lubridate::minute(.data$CatchTimeField), sep = ":"), "NA:NA"),
                  IndvID = as.character(.data$BirdID),
                  BreedingSeason = .data$CatchYear,
                  CaptureDate = as.Date(.data$CatchDate),
                  CapturePopID = "AMM",
                  ReleasePopID = "AMM",
                  CapturePlot = as.character(.data$CapturePlot),
                  ReleasePlot = .data$CapturePlot,
                  LocationID = as.character(.data$NestBox),
                  OriginalTarsusMethod = "Alternative",
                  Age_observed = dplyr::recode(.data$AgeObserved, `7` = NA_integer_, `0` = NA_integer_), ## These age categories match our EURING codes exactly
                  ChickAge = NA_integer_,
                  ObserverID = as.character(dplyr::na_if(.data$FieldObserver, -99L)),
                  BroodID = as.character(.data$BroodID),
                  Sex_observed = dplyr::case_when(.data$SexObserved == 1L ~ "F",
                                                 .data$SexObserved == 2L ~ "M",
                                                 TRUE ~ NA_character_),
                  #If taken to the lab for personality tests this could affect survival
                  #Attaching a transponder could also affect survival
                  CaptureExperimentID = dplyr::case_when(.data$ToLab == 1L ~ "SURVIVAL",
                                                         .data$Transponder == 1L ~ "SURVIVAL",
                                                         TRUE ~ NA_character_),
                  CaptureAlive = dplyr::case_when(.data$CatchType %in% 13L:15L ~ FALSE,
                                                    TRUE ~ TRUE),
                  ReleaseAlive = dplyr::case_when(.data$CatchType %in% 13L:15L ~ FALSE,
                                                  .data$DeadOrAlive == 0L ~ FALSE,
                                                  TRUE ~ TRUE)) %>%
    mutate_at(.vars = vars(.data$BodyMassField, .data$Tarsus, .data$WingP3), .funs = ~ifelse(. <= 0, NA_real_, .)) %>%
    dplyr::select(.data$IndvID,
                  .data$Species,
                  .data$Sex_observed,
                  .data$BreedingSeason,
                  .data$CaptureDate,
                  .data$CaptureTime,
                  .data$ObserverID,
                  .data$LocationID,
                  .data$CapturePopID,
                  .data$CapturePlot,
                  .data$ReleasePopID,
                  .data$ReleasePlot,
                  Mass = .data$BodyMassField,
                  .data$Tarsus,
                  WingLength = .data$WingP3,
                  .data$Age_observed,
                  .data$ChickAge,
                  .data$BroodID)

  Chick_capture <- Chick_catch_tables %>%
    dplyr::filter(Egg %in% c(-99L, 0L, 2L)) %>%
    dplyr::left_join(Nestbox_capture, by = "NestBox") %>%
    dplyr::left_join(Nestbox_release, by = c("SwapToNestBox" = "NestBox")) %>%
    dplyr::mutate(EndMarch = as.Date(paste(.data$ChickYear, "03", "31", sep = "-")),
                  CapturePopID = "AMM", ReleasePopID = "AMM") %>%
    dplyr::collect() %>%
    #Hatchday >500 and <0 should be NA
    dplyr::mutate(HatchDay = dplyr::case_when((.data$HatchDay >= 500 | .data$HatchDay < 0) ~ NA_integer_,
                                              TRUE ~ .data$HatchDay),
                  # All chicks are GT
                  Species = Species_codes$Code[Species_codes$SpeciesID == 14640],
                  Sex_observed = NA_character_) %>%
    dplyr::select(.data$Species, .data$Sex_observed, .data$BirdID, .data$ChickYear, .data$EndMarch, .data$NestBox, .data$BroodID,
                  .data$CapturePlot, .data$ReleasePlot,
                  .data$CapturePopID, .data$ReleasePopID,
                  .data$HatchDay, .data$Day2BodyMass, .data$Day2BodyMassTime,
                  Day2Observer = .data$SwapObserver,
                  .data$Day6BodyMass, .data$Day6BodyMassTime, .data$Day6Observer,
                  .data$Day14P3, .data$Day14Tarsus, .data$Day14BodyMass, .data$Day14BodyMassTime,
                  .data$Day14Observer, Day18BodyMass = .data$Day18Bodymass, .data$Day18BodyMassTime, .data$Day18Observer) %>%
    dplyr::mutate_all(~dplyr::na_if(as.character(.), "-99")) %>% #Needed because pivot functions expect coercable classes when making value col
    dplyr::mutate_at(.vars = vars(contains("BodyMassTime")), ~str_extract(., "[0-9]{2}:[0-9]{2}")) %>%
    tidyr::pivot_longer(data = ., cols = .data$Day2BodyMass:.data$Day18Observer, names_to = "column", values_to = "value") %>%
    dplyr::mutate(Day = str_extract(.data$column, "[0-9]{1,}"),
                  OriginalTarsusMethod = "Alternative",
                  Age_observed = 1L) %>%
    tidyr::separate(.data$column, into = c(NA, "Variable"), sep = "^Day[0-9]{1,}") %>%
    tidyr::pivot_wider(data = ., names_from = .data$Variable, values_from = .data$value) %>%
    #Mutate columns back to correct type
    dplyr::mutate_at(.vars = vars(.data$ChickYear, .data$HatchDay, .data$Day), as.integer) %>%
    dplyr::mutate_at(.vars = vars(.data$BodyMass, .data$P3, .data$Tarsus), ~ifelse(as.numeric(.) <= 0, NA_real_, as.numeric(.))) %>%
    dplyr::filter(!(is.na(.data$BodyMass) & is.na(.data$P3) & is.na(.data$Tarsus))) %>%
    dplyr::mutate(CaptureDate = as.Date(.data$EndMarch) + .data$HatchDay + .data$Day) %>%
    dplyr::select(IndvID = .data$BirdID,
                  .data$Species,
                  .data$Sex_observed,
                  BreedingSeason = .data$ChickYear,
                  .data$CaptureDate,
                  CaptureTime = .data$BodyMassTime,
                  ObserverID = .data$Observer,
                  LocationID = .data$NestBox,
                  .data$CapturePopID,
                  .data$CapturePlot,
                  .data$ReleasePopID,
                  .data$ReleasePlot,
                  Mass = .data$BodyMass,
                  .data$Tarsus,
                  WingLength = .data$P3,
                  .data$Age_observed,
                  ChickAge = .data$Day,
                  .data$BroodID)

  Capture_data <- dplyr::bind_rows(Adult_capture, Chick_capture) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    #Arrange by ID/Date and add unique capture ID
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:n(), sep = "_")) %>%
    dplyr::select(.data$CaptureID, everything())

  Capture_data

}

#' Create individual data table for Ammersee, Germany
#'
#' Create individual data table in standard format for data from Ammersee, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_AMM}}.
#' @param Brood_data Data frame. Output from \code{\link{create_brood_AMM}}.
#'
#' @return A data frame.

create_individual_AMM <- function(Capture_data, Brood_data, connection) {

  Sex_genetic <- dplyr::tbl(connection, "GenotypesAllYears") %>%
    dplyr::select(IndvID = .data$BirdID, .data$SexGenetic) %>%
    dplyr::filter(!is.na(.data$IndvID) & !is.na(.data$SexGenetic) & .data$SexGenetic > 0) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$SexGenetic)),
                     unique_sex = list(unique(.data$SexGenetic))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(IndvID = as.character(.data$IndvID),
                  Sex_genetic = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                .data$unique_sex[[1]] == 1L ~ "F",
                                                .data$unique_sex[[1]] == 2L ~ "M")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$IndvID, .data$Sex_genetic)

  Sex_calc <- Capture_data %>%
    dplyr::filter(!is.na(.data$Sex_observed)) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(length_sex = length(unique(.data$Sex_observed)),
                     unique_sex = list(unique(.data$Sex_observed))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Sex_calculated = dplyr::case_when(.data$length_sex > 1 ~ "C",
                                                TRUE ~ .data$unique_sex[[1]])) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$IndvID, .data$Sex_calculated)

  Brood_swap_info <- dplyr::tbl(connection, "Chicks") %>%
    dplyr::select(IndvID = .data$BirdID, BroodIDLaid = .data$BroodID, BroodIDFledged = .data$SwapToBroodID) %>%
    dplyr::collect() %>%
    dplyr::mutate_all(as.character)

  Capture_data %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(.data$Species))), .f = ~{

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
    RingAge = ifelse(min(.data$Age_observed) != 1L | is.na(min(.data$Age_observed)), "adult", "chick")) %>%
    dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::left_join(Sex_genetic, by = "IndvID") %>%
    dplyr::left_join(Brood_swap_info, by = "IndvID") %>%
    dplyr::select(.data$IndvID, .data$Species, .data$PopID,
                  .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge,
                  .data$Sex_calculated, .data$Sex_genetic)


}

#' Create location data table for Ammersee, Germany.
#'
#' Create location data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_location_AMM <- function(Capture_data, connection) {

  Habitat_data <- dplyr::tbl(connection, "HabitatDescription") %>%
    dplyr::select(NestBox, Beech:OtherTree) %>%
    dplyr::collect() %>%
    dplyr::group_by(NestBox) %>%
    dplyr::summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "keep") %>%
    dplyr::summarise(DEC = sum(c(Beech, Larch, Maple, Birch, Oak, Willow, Poplar, Alder, AshTree)),
                     EVE = sum(c(Spruce, Pine))) %>%
    dplyr::mutate(perc_dec = DEC/(DEC + EVE),
                  dominant_sp = dplyr::case_when(perc_dec >= 0.66 ~ "DEC",
                                                 perc_dec < 0.33 ~ "EVE",
                                                 TRUE ~ "MIX"))

  #The vast majority of nestboxes (90%) of nestboxes are surrounded by deciduous or mixed stands. Therefore,
  #we use the 'DEC' category to describe the population.
  #In the future, we could include a nest-box specific habitat type, but that would require us to decide
  #the range over which habitat is sampled (the nestbox vicinity, the plot?). To do later.
  table(Habitat_data$dominant_sp)

  start_year <- min(Capture_data$BreedingSeason)

  Location_data <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::collect() %>%
    dplyr::filter(.data$NestBox != -99L) %>%
    dplyr::mutate(LocationID = as.character(.data$NestBox),
                  NestboxID = as.character(.data$NestBox),
                  Latitude = as.numeric(.data$CoordinateLatitude2013), ## Needed because these variables are stored as
                  Longitude = as.numeric(.data$CoordinateLongitude2013), ## named vectors, not regular numeric vectors
                  LocationType = "NB",
                  PopID = "AMM",
                  StartSeason = start_year,
                  EndSeason = dplyr::case_when(nchar(.data$NestboxID) == 4 & stringr::str_sub(.data$NestboxID, 1, 2) == "16" ~ 2016L,
                                               TRUE ~ 2019L),
                  HabitatType = "DEC") %>%
    dplyr::select(.data$LocationID,
                  .data$NestboxID,
                  .data$LocationType,
                  .data$PopID,
                  .data$Latitude,
                  .data$Longitude,
                  .data$StartSeason,
                  .data$EndSeason,
                  .data$HabitatType)

  Location_data

}
