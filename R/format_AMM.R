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
#'\strong{BroodSize/NumberFledgedError}: Use ErrorHatched for BroodSizeError, but this really represents minimum (BroodSize)
#'and maximum (BroodSize + ErrorHatched). We need to rethink how we deal with this error. Do we use (BroodSize + ErrorHatched/2) = BroodSize?
#'
#'\strong{AvgChickMass}: Included column BroodWeight, but need to check what day they were weighed.
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

  Capture_data <- create_capture_BAN(all_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_BAN(Capture_data)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_BAN(all_data)

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

#' Create brood data table for Ammersee, Germany.
#'
#' Create brood data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_brood_AMM   <- function(connection) {

  Colour_codes <- dplyr::tbl(connection, "ColourCode") %>%
    dplyr::select(.data$ColourCode, .data$BirdID)

  Nest_boxes <- dplyr::tbl(connection, "NestBoxes") %>%
    dplyr::select(.data$NestBox, .data$Plot)

  Brood_data <- dplyr::tbl(connection, "Broods") %>%
    dplyr::left_join(Colour_codes %>% dplyr::rename(FemaleID = .data$BirdID), by = c("ColourCodeFemale" = "ColourCode")) %>%
    dplyr::left_join(Colour_codes %>% dplyr::rename(MaleID = .data$BirdID), by = c("ColourCodeMale" = "ColourCode")) %>%
    dplyr::left_join(Nest_boxes, by = c("NestBox" = "NestBox")) %>%
    dplyr::mutate(BreedingSeason = .data$BroodYear,
                  PopID = "AMM",
                  EndMarch = as.Date(paste(.data$BroodYear, "03", "31", sep = "-")),
                  LayDate = .data$EndMarch + .data$FirstEggDay,
                  HatchDate = .data$EndMarch + .data$HatchDay,
                  FledgeDate = .data$EndMarch + .data$FledgeDay,
                  ClutchSize_combined = .data$ClutchSize + .data$EggOtherSpecies,
                  BroodSwap_ExperimentID = ifelse(.data$BroodSwap > 0L, "COHORT", NA_character_),
                  BroodOther_ExperimentID = ifelse(.data$BroodOtherTreatment == 3L, "SURVIVAL", NA_character_),
                  Plot_ExperimentID = ifelse(.data$PlotLevelTreatment == 3L, "SURVIVAL", NA_character_),
                  ExperimentID = paste(.data$BroodSwap_ExperimentID, .data$BroodOther_ExperimentID, .data$Plot_ExperimentID, sep = ";")) %>% ##TODO: Decipher other treatment values and check with Niels
    dplyr::collect() %>%
    # Case when not available in SQL Access, needs to be done after collecting
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE),
                  Species = dplyr::case_when(.data$Species == 1L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14640"],
                                             .data$Species == 2L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14620"],
                                             .data$Species == 3L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14610"],
                                             .data$Species == 4L ~ !!Species_codes$Code[Species_codes$SpeciesID == "14790"]),
                  ClutchType_observed = dplyr::case_when(.data$ClutchNumber == 1L ~ "first",
                                                         .data$ClutchNumber == 2L ~ "second",
                                                         .data$ClutchNumber == 3L ~ "replacement",
                                                         .data$ClutchNumber == 4L ~ "second",
                                                         .data$ClutchNumber == 6L ~ "replacement"),
                  ClutchSizeError = NA_integer_,
                  FledgeDateError = NA_integer_)

    dplyr::mutate_at(.vars = c("Plot", "LocationID", "MaleID"), as.character) %>%
    # dplyr::mutate_at(.vars = vars(LayDate, HatchDate, FledgeDate), as.Date) %>%
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                  .data$Species, .data$Plot, LocationID = .data$NestBox, .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate, LayingDateError = .data$FirstEggDayError,
                  .data$ClutchSize, .data$ClutchSizeError,
                  .data$HatchDate, HatchDateError = .data$HatchDayError,
                  BroodSize = .data$NumberHatched, BroodSizeError = .data$ErrorHatched, ## TODO: Consistent way to deal with errors.
                  .data$FledgeDate, .data$FledgeDateError,
                  .data$NumberFledged, NumberFledgedError = .data$FledgedError,
                  AvgChickMass = .data$BroodWeight, NumberChicksMass = .data$BroodWeightBS, ##FIXME: What day were they weighed?
                  .data$ExperimentID)

  Brood_data

}

#' Create capture data table for Ammersee, Germany
#'
#' Create capture data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_capture_AMM <- function(data) {

}

#' Create individual data table for Ammersee, Germany
#'
#' Create individual data table in standard format for data from Ammersee, Germany.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_AMM}}.
#'
#' @return A data frame.

create_individual_AMM <- function(Capture_data) {

}

#' Create location data table for Ammersee, Germany.
#'
#' Create location data table in standard format for data from Ammersee, Germany.
#' @param data Data frame. Primary data from Ammersee.
#'
#' @return A data frame.

create_location_AMM <- function(data) {


}
