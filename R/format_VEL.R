#' Construct standard summary for data from Velky Kosir, Czechia.
#'
#' A pipeline to produce a standard output for the nest box population in Velky
#' Kosir, Czechia, administered by Milos Krist.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' \strong{Species}: There are a small number of records for species other than
#' great tit, blue tit and collared flycatcher. These are currently excluded.
#' Blue tits have 93 recorded broods. This is less than our previous 100 broods
#' cut-off, but they are still included.
#'
#' \strong{ClutchType_calculated}: We assume that any NA records for number fledged
#' are true unknowns (rather than 0s). In which case, we can't always estimate
#' whether a clutch is a second clutch.
#'
#' \strong{ExperimentID}: Currently, we just copy the text directly from the
#' tables. Need to go through an classify each experiment and check with Milos
#' that these classifications are reasonable.
#'
#' \strong{BroodID}: BroodID is currently Year_nestbox_day_month. This accounts
#' for multiple clutches laid in the same nest box.
#'
#' \strong{Tarsus}: Tarsus length is originally measured using Oxford maximum
#' method. We convert this to Svensson's Alternative method using formula
#' described in the standard format.
#'
#' \strong{ClutchSize_observed, BroodSize_observed, NumberFledged_observed}: Some records have uncertainty
#' (e.g. 11+). We're unsure how large this uncertainty is. It's currently
#' ignored, but will talk with data owner to incorporate this.
#'
#' \strong{CaptureDate}: In the raw data for flycatchers there are columns for
#' measurements taken at 6 days and at 13 days. Sometimes, no measurements are
#' recorded (i.e. columns are NA) and we need to decide how to deal with these.
#' In discussion with the data owner we know that chicks are generally ringed at 6 days;
#' therefore, we assume that any ringed chick was captured at 6 days old, even if
#' they have no other measurements. We only include a further capture at 13 days
#' old if some measurements were taken. Those individuals with no mass/tarsus/wing length
#' measures recorded at 13 days were assumed to have not been captured at this time.
#'
#' \strong{CaptureAlive, ReleaseAlive}: All individuals are assumed to be captured and released alive.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export

format_VEL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R") {

  #Force user to select directory
  force(db)

  #Add species filter
  if(is.null(species)){

    species <- species_codes$Species

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  ## Read in flycatcher data. There are some issues will coercion of col types
  ## so we specify column types manually. With this we can skip certain cols.
  ## we skip:
  ## - row number
  ## - whether chicks were observed dead (we currently have no col for this in the standard format)
  ## - Adult wing and forhead patch measures
  ## - Picture and geolocator info
  ## - Info on which eggs were transferred in cross foster
  FICALB_data <- suppressMessages(readxl::read_excel(paste0(db, "/VEL_PrimaryData_flycatchers.xlsx"),
                                                     col_types = c("skip", "numeric", "text",
                                                                   "text", "list",
                                                                   "text", "text",
                                                                   "text", "text",
                                                                   "list", "text",
                                                                   rep("text", 8),
                                                                   rep(c(rep("numeric", 4), "skip"), 8),
                                                                   "text", "list", "numeric",
                                                                   "numeric", "numeric",
                                                                   rep("skip", 6),
                                                                   "text", "list", "text",
                                                                   "numeric", "numeric",
                                                                   "numeric", rep("skip", 13),
                                                                   "text", rep("skip", 16)))) %>%
    janitor::clean_names() %>%
    ## Date info is sometimes recorded as dd/mm/yyyy and sometimes as dd.mm.yyyy.
    ## This causes some issues with date parsing. If we parse as text, it converts
    ## the excel dates into numerics. If we parse as date, it gives NAs for the
    ## cases where date is stored as dd.mm.yyyy.
    ## Instead we parse each one separately, based on the best guess of readxl.
    ## This returns dates for dd/mm/yyyy, but character for dd.mm.yyyy.
    ## Then we just have to go through and make these few character strings into dates.
    dplyr::mutate(dplyr::across(.cols = tidyselect::contains("date"),
                                .fns = function(date){

                                  purrr::map(.x = date,
                                             .f = ~{

                                               if(is.character(.x)){

                                                 return(as.Date(.x, format = "%d.%m.%Y"))

                                               } else {

                                                 return(as.Date(.x))

                                               }

                                             })

                                })) %>%
    tidyr::unnest(cols = c("laying_date", "hatching_date",
                           "date_of_capture_52", "date_of_capture_57")) %>%
    ## CHANGE COL NAMES TO MATCH STANDARD FORMAT
    dplyr::mutate(PopID = "VEL",
                  BreedingSeason =
                    as.integer(.data$year),
                  Species = species_codes[species_codes$SpeciesID == 13480, ]$Species,
                  Plot = .data$plot,
                  LocationID = paste0(toupper(stringr::str_sub(.data$plot, end = 1)), .data$nest),
                  BroodID = paste(.data$BreedingSeason, .data$plot,
                                  stringr::str_pad(string = .data$nest, width = 2, pad = "0"),
                                  stringr::str_pad(string = lubridate::day(.data$laying_date),
                                                   width = 2,
                                                   pad = "0"),
                                  stringr::str_pad(string = lubridate::month(.data$laying_date),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  FemaleID = .data$female_ring,
                  MaleID = .data$male_ring,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = .data$laying_date,
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ## We assume that "+", "-", or "?" in ClutchSize, BroodSize, NumberFledged
                  ## is a measure of uncertainty,
                  ##TODO but we need to check this with data owner
                  ClutchSize_observed = as.integer(gsub(pattern = "\\+|\\?",
                                                        replacement = "", .data$clutch_size)),
                  ClutchSize_min = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\?") ~ 1,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  ClutchSize_max = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\+|\\?") ~ Inf,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  HatchDate_observed = .data$hatching_date,
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(gsub(pattern = "\\+|\\?", replacement = "", .data$number_hatched)),
                  BroodSize_min = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\?") ~ 1,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  BroodSize_max = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\+|\\?") ~ Inf,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = .data$FledgeDate_observed,
                  FledgeDate_max = .data$FledgeDate_observed,
                  NumberFledged_observed = as.integer(gsub(pattern = "\\+|\\?", replacement = "", .data$number_fledged)),
                  NumberFledged_min = dplyr::case_when(stringr::str_detect(.data$number_fledged, "\\?") ~ 1,
                                                       TRUE ~ as.numeric(.data$NumberFledged_observed)),
                  NumberFledged_max = dplyr::case_when(stringr::str_detect(.data$number_fledged, "\\+|\\?") ~ Inf,
                                                       TRUE ~ as.numeric(.data$NumberFledged_observed)),
                  ##ADD EMPTY EGG COLS. NO EGG DATA.
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  ExperimentID = .data$treatment) ##TODO: Categorize experimental descriptions

  ## No columns are excluded except row number and final col.
  ## Final col has some data, but no column name
  ## Dates are dealt with the same way as flycatchers
  ## We only use data on GT and BT. There is a small number of clutches
  ## for other species, but all < 40.
  TIT_data <- readxl::read_excel(paste0(db, "/VEL_PrimaryData_tits.xls"),
                                 col_types = c("skip", "numeric", "text", "text",
                                               "text", "text", "list",
                                               "list", "list", "text",
                                               "text", "text", "text",
                                               "text", "text", "text", "text",
                                               "skip")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::contains("date"),
                                .fns = function(date){

                                  purrr::map(.x = date,
                                             .f = ~{

                                               if(is.character(.x)){

                                                 return(as.Date(.x, format = "%d.%m.%Y"))

                                               } else {

                                                 return(as.Date(.x))

                                               }

                                             })

                                })) %>%
    tidyr::unnest(cols = c("laying_date", "laying_date_minimum", "laying_date_maximum"))

  ## Determine laying date for every nest, accounting for error in nests
  TIT_LD_error <- purrr::pmap_dfr(.l = list(TIT_data$laying_date,
                                            TIT_data$laying_date_maximum,
                                            TIT_data$laying_date_minimum),
                                  .f = ~{
                                    tibble::tibble(LayDate_observed = dplyr::case_when(is.na(..1) ~ mean(c(..2, ..3)),
                                                                                       TRUE ~ ..1),
                                                   LayDate_min = ..3,
                                                   LayDate_max = ..2)
                                  })

  TIT_data <- TIT_data %>%
    dplyr::mutate(BreedingSeason = as.integer(.data$year),
                  Species = dplyr::case_when(.data$species == "blue tit" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$species == "great tit" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species),
                  PopID = "VEL",
                  Plot = .data$plot,
                  LocationID = paste0(toupper(stringr::str_sub(.data$plot, end = 1)),
                                      .data$nest_box),
                  FemaleID = .data$female_ring,
                  MaleID = NA_character_,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = TIT_LD_error$LayDate_observed,
                  LayDate_min = TIT_LD_error$LayDate_min,
                  LayDate_max = TIT_LD_error$LayDate_max,
                  ## We assume that "+", "-", or "?" in ClutchSize, BroodSize, NumberFledged
                  ## is a measure of uncertainty,
                  ##TODO but we need to check this with data owner
                  ClutchSize_observed = as.integer(gsub(pattern = "\\+|\\?",
                                                        replacement = "", .data$clutch_size)),
                  ClutchSize_min = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\?") ~ 1,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  ClutchSize_max = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\+|\\?") ~ Inf,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  HatchDate_observed = as.Date(NA),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(gsub(pattern = "\\+|\\?",
                                                       replacement = "", .data$number_hatched)),
                  BroodSize_min = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\?") ~ 1,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  BroodSize_max = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\+|\\?") ~ Inf,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = as.integer(gsub(pattern = "\\+|\\-|\\?",
                                                           replacement = "", .data$number_fledged)),
                  NumberFledged_min = dplyr::case_when(stringr::str_detect(.data$number_fledged, "\\-|.\\?") ~ 1,
                                                       stringr::str_detect(.data$number_fledged, "^\\?") ~ NA_real_,
                                                       TRUE ~ as.numeric(.data$NumberFledged_observed)),
                  NumberFledged_max = dplyr::case_when(stringr::str_detect(.data$number_fledged, "\\+|.\\?") ~ Inf,
                                                       stringr::str_detect(.data$number_fledged, "^\\?") ~ NA_real_,
                                                       TRUE ~ as.numeric(.data$NumberFledged_observed)),
                  ##ADD EMPTY EGG DATA COLS.
                  AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_,
                  ExperimentID = .data$experiment, ##TODO: Categorize experimental descriptions
                  ## Estimate broodID last because it requires us to estimate LayDate first
                  BroodID = paste(.data$year, .data$plot, stringr::str_pad(string =.data$ nest_box,
                                                                           width = 3, pad = "0"),
                                  stringr::str_pad(string = lubridate::day(.data$LayDate_observed),
                                                   width = 2,
                                                   pad = "0"),
                                  stringr::str_pad(string = lubridate::month(.data$LayDate_observed),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  HabitatType = dplyr::case_when(.data$habitat == "oak" ~ "deciduous",
                                                 .data$habitat == "spruce" ~ "evergreen"))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_VEL(FICALB_data, TIT_data) %>%
    dplyr::filter(.data$Species %in% species)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- dplyr::bind_rows(create_capture_VEL_FICALB(FICALB_data),
                                   create_capture_VEL_TIT(TIT_data)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$Species %in% species)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_VEL(Capture_data) %>%
    dplyr::filter(.data$Species %in% species)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_VEL(db, Brood_data, TIT_data)

  # WRANGLE DATA FOR EXPORT

  ## Combine capture data and brood data to determine avg chick mass and tarsus
  ## Calculate AvgChickMass and AvgTarsus
  avg_measures <- Capture_data %>%
    ## Filter just 13 day old chicks
    dplyr::filter(.data$ChickAge == 13L) %>%
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass = length(stats::na.omit(.data$Mass)),
                     AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(.data$Tarsus)))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_measures, by = "BroodID") %>%
    dplyr::mutate(NumberChicksMass = dplyr::na_if(.data$NumberChicksMass, 0),
                  NumberChicksTarsus = dplyr::na_if(.data$NumberChicksTarsus, 0),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Oxford")) %>%
    dplyr::select("BroodID":"NumberEggs",
                  "AvgChickMass":"NumberChicksTarsus",
                  "OriginalTarsusMethod", "ExperimentID")

  Capture_data <- Capture_data %>%
    dplyr::select("IndvID", "Species", "Sex_observed" = "Sex", 'BreedingSeason',
                  "CaptureDate", "CaptureTime", "ObserverID", "LocationID",
                  "CaptureAlive", "ReleaseAlive",
                  'CapturePopID':"Tarsus", "OriginalTarsusMethod",
                  "WingLength", "Age_observed", "Age_calculated", 'ChickAge',
                  "ExperimentID") %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("CaptureID", tidyselect::everything())

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_VEL.csv"), row.names = F)

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

#' Create brood data table for Velky Kosir
#'
#' Create brood data table in standard format for flycatcher and tit data from Velky
#' Kosir.
#' @param FICALB_data Data frame. Flycatcher data from Velky Kosir.
#' @param TIT_data Data frame. Tit data from Velky Kosir.
#'
#' @return A data frame.

create_brood_VEL <- function(FICALB_data, TIT_data) {

  FICALB_broods <- FICALB_data %>%
    dplyr::arrange(.data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID", "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed":"ExperimentID")

  TIT_broods <- TIT_data %>%
    dplyr::arrange(.data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%
    dplyr::select("BroodID", "PopID", "BreedingSeason",
                  "Species", "Plot", "LocationID", "FemaleID", "MaleID",
                  "ClutchType_observed", "ClutchType_calculated",
                  "LayDate_observed":"ExperimentID")

  return(dplyr::bind_rows(FICALB_broods, TIT_broods))

}

#' Create capture data table for flycatchers in Velky Kosir.
#'
#' Create a capture data table in standard format for only flycatchers in Velky Kosir.
#' Tit data is created with separate function \code{\link{create_capture_VEL_TIT}}.
#' @param FICALB_data Data frame. Flycatcher data from Velky Kosir.
#'
#' @return A data frame.

create_capture_VEL_FICALB <- function(FICALB_data) {

  # Extract data from flycatcher chick captures on the nest
  FICALB_chicks <- FICALB_data %>%
    dplyr::select("BreedingSeason", "Species", "Plot", "BroodID", "LocationID",
                  "LayDate_observed", "ClutchSize_observed", "HatchDate_observed",
                  "x1_young_ring":"x8y_wing", "ExperimentID") %>%
    # Pivot information stored in columns to rows
    tidyr::pivot_longer(cols = tidyselect::starts_with("x"),
                        names_to = c("id", ".value"),
                        names_pattern = "^x([[:digit:]]{1})[[:alpha:]]{0,1}\\_(.*)$") %>%
    dplyr::rename("IndvID" = "young_ring",
                  "Mass6" = "mass6",
                  "Mass13" = "mass13",
                  "Tarsus" = "tarsus",
                  "WingLength" = "wing") %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    # Mass is measured on day 6 and 13, split this in two rows
    tidyr::pivot_longer(cols = c("Mass6", "Mass13"),
                        names_to = c(".value", "ChickAge"),
                        names_pattern = "^(Mass)([[:digit:]]{1,2})$") %>%
    dplyr::mutate(ChickAge = as.integer(ChickAge),
                  # Tarsus and Wing Length are only measured on day 13
                  dplyr::across(.cols = c("Tarsus", "WingLength"),
                                .fns = ~{

                                  dplyr::case_when(.data$ChickAge == 6 ~ NA_real_,
                                                   .data$ChickAge == 13 ~ .x)

                                })) %>%
    # Calculate CaptureDate
    # If no hatch date is known then use laying date + clutch size + incubation + chick age
    # TODO We assume incubation of 15 days, but need to check with Milos.
    # If there is no hatch date OR laying date then just use NA
    dplyr::mutate(CaptureDate = dplyr::case_when(!is.na(.data$HatchDate_observed) ~ .data$HatchDate_observed + lubridate::days(.data$ChickAge),
                                                 !is.na(.data$LayDate_observed) ~ .data$LayDate_observed + lubridate::days(.data$ClutchSize_observed + 15 + .data$ChickAge),
                                                 TRUE ~ as.Date(NA)),
                  CaptureTime = NA_character_,
                  CapturePopID = "VEL",
                  CapturePlot = .data$Plot,
                  ReleasePopID = "VEL",
                  ReleasePlot = .data$Plot,
                  # All chick records were 6 or 13 days, so all are listed as EURING age 1
                  Age_observed = 1L,
                  Age_calculated = NA_integer_,
                  # Convert tarsus to Svennson's alternative
                  Tarsus = convert_tarsus(.data$Tarsus, method = "Oxford"),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Oxford"),
                  ObserverID = NA_character_) %>%
    # For records at 13 days, if there was no mass/tarsus we assume that it was not caught at 13 days
    dplyr::filter(.data$ChickAge == 6 | (.data$ChickAge == 13 & (!is.na(.data$Tarsus) | !is.na(.data$WingLength) | !is.na(.data$Mass)))) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "LocationID", "CaptureDate", "CaptureTime",
                  "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot", "Mass", "Tarsus", "WingLength",
                  "Age_observed", "Age_calculated", "ChickAge", "BroodID", "ObserverID", "OriginalTarsusMethod")

  # Extract data from adult captures
  FICALB_adults <- FICALB_data %>%
    dplyr::select("BreedingSeason", "Species", "Plot", "LocationID", "LayDate_observed",
                  "BroodID", "FemaleID", "date_of_capture_52", "tarsus_53":"wing_55",
                  "MaleID", "date_of_capture_57", "age":"wing_61",
                  "ExperimentID") %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"),
                        values_to = "IndvID",
                        names_to = "Sex") %>%
    dplyr::filter(!is.na(.data$IndvID))

  FICALB_adults <- FICALB_adults %>%
    ## Give individuals a sex, we will use this in our Individual_data table
    dplyr::mutate(Sex = stringr::str_sub(.data$Sex, 0, 1),
                  Tarsus = dplyr::case_when(.data$Sex == "F" ~ .data$tarsus_53,
                                            .data$Sex == "M" ~ .data$tarsus_59),
                  WingLength = dplyr::case_when(.data$Sex == "F" ~ .data$wing_55,
                                                .data$Sex == "M" ~ .data$wing_61),
                  Mass = dplyr::case_when(.data$Sex == "F" ~ .data$mass_54,
                                          .data$Sex == "M" ~ .data$mass_60),
                  # Determine age of males based on 'age' column
                  Age_observed = dplyr::case_when(.data$Sex == "M" & .data$age == "old" ~ 6L,
                                                  .data$Sex == "M" & .data$age == "young" ~ 5L,
                                                  TRUE ~ NA_integer_),
                  CapturePopID = "VEL",
                  ReleasePopID = "VEL",
                  CapturePlot = .data$Plot,
                  ReleasePlot = .data$Plot,
                  ObserverID = NA_character_,
                  CaptureDate = dplyr::case_when(.data$Sex == "F" ~ .data$date_of_capture_52,
                                                 .data$Sex == "M" ~ .data$date_of_capture_57)) %>%
    dplyr::mutate(Tarsus = convert_tarsus(.data$Tarsus, method = "Oxford"),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Oxford")) %>%
    dplyr::select("Species", "BreedingSeason", "LocationID", "BroodID",
                  "Sex":"CaptureDate", "ExperimentID")

  FICALB_alldat <- dplyr::bind_rows(FICALB_chicks, FICALB_adults) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason, showpb = TRUE) %>%
    # We have no information on status of captures/releases, so we assume all individuals were captured/released alive
    dplyr::mutate(CaptureAlive = TRUE,
                  ReleaseAlive = TRUE)

  return(FICALB_alldat)

}

#' Create capture data table for tits in Velky Kosir.
#'
#' Create a capture data table in standard format for great and blue tits in Velky Kosir.
#' Tit data is created with separate function \code{\link{create_capture_VEL_FICALB}}.
#' @param TIT_data Data frame. Tit data from Velky Kosir.
#'
#' @return A data frame.

create_capture_VEL_TIT    <- function(TIT_data) {

  ## There is no chick info for tits
  ## There is only data on females.
  ## Assume that an individual was caught at the start of incubation.
  TIT_capture <- TIT_data %>%
    dplyr::filter(!is.na(.data$FemaleID)) %>%
    dplyr::mutate(Species = dplyr::case_when(.data$species == "blue tit" ~ species_codes[species_codes$SpeciesID == 14620, ]$Species,
                                             .data$species == "great tit" ~ species_codes[species_codes$SpeciesID == 14640, ]$Species),
                  ## Make the capture date the date that incubation would start (laying date + clutch size)
                  CaptureDate = .data$LayDate_observed + .data$ClutchSize_observed,
                  IndvID = .data$FemaleID,
                  CapturePopID = .data$PopID,
                  CapturePlot = .data$Plot,
                  ReleasePopID = .data$PopID,
                  ReleasePlot = .data$Plot,
                  ## There is no explicit info about age.
                  ## They must all be adults, so just give them all EURING 4
                  ## "Hatched before this calendar year"
                  Age_observed = 4L,
                  ObserverID = NA_character_,
                  LocationID = paste(.data$Plot, .data$nest_box, sep = "_"),
                  OriginalTarsusMethod = NA_character_,
                  Sex = "F") %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed,
             Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    # We have no information on status of captures/releases, so we assume all individuals were captured/released alive
    dplyr::mutate(CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%
    dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "ObserverID",
                  "LocationID", "CapturePopID":'ReleasePlot', "Age_observed", "Age_calculated", "Sex",
                  "CaptureAlive", "ReleaseAlive", "ExperimentID")

  return(TIT_capture)

}

#' Create individual data table for Velky Kosir.
#'
#' Create individual data table in standard format for both flycatcher and tit
#' data from Velky Kosir.
#' @param Capture_data Data frame. Combined data from
#'   \code{\link{create_capture_VEL_FICALB}} and
#'   \code{\link{create_capture_VEL_TIT}}.
#'
#' @return A data frame.

create_individual_VEL <- function(Capture_data){

  pb <- progress::progress_bar$new(total = length(unique(Capture_data$IndvID)) * 1)

  Indvidual_data <- Capture_data %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(Species = unique(stats::na.omit(.data$Species)),
                     PopID = "VEL",
                     RingSeason = dplyr::first(.data$BreedingSeason),
                     RingAge = dplyr::case_when(dplyr::first(.data$Age_observed) == 1 ~ "chick",
                                                dplyr::first(.data$Age_observed) != 1 ~ "adult",
                                                is.na(dplyr::first(.data$Age_observed)) ~ "adult"),
                     Sex_calculated = purrr::map_chr(.x = list(unique(.data$Sex)),
                                                     .f = ~{

                                                       pb$tick()

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
                     FirstBroodID = dplyr::first(.data$BroodID)) %>%
    dplyr::mutate(BroodIDLaid = dplyr::case_when(is.na(.data$RingAge) ~ NA_character_,
                                                 .data$RingAge == "adult" ~ NA_character_,
                                                 TRUE ~ .data$FirstBroodID),
                  BroodIDFledged = .data$BroodIDLaid,
                  Sex_genetic = NA_character_) %>%
    dplyr::select("IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged",
                  "RingSeason", "RingAge", "Sex_calculated", "Sex_genetic") %>%
    dplyr::ungroup()

  return(Indvidual_data)

}

#' Create location data table for Velky Kosir.
#'
#' Create location data table in standard format for all nest boxes.
#'
#' @param Brood_data Data frame. Output of \code{\link{create_brood_VEL}}
#' @param TIT_data Data frame. Data frame. Tit data from Velky Kosir. This is
#'   needed to include habitat type information.
#' @param db Location of database file.
#'
#' @return A data frame.

create_location_VEL <- function(db, Brood_data, TIT_data){

  location_data_excel <- readxl::read_excel(paste0(db, "/VEL_PrimaryData_locations.xls"),
                                            col_types = c("text")) %>%
    janitor::clean_names() %>%
    #There is date time info in here. I assume this is date of measurement of XYZ
    #Not relevant for Start/EndSeason
    dplyr::rename("LocationID" = "budka",
                  "Coordinates" = "sirka_delka_hddd_mm_mmm",
                  "Altitude" = "nadmorska_vyska") %>%
    #Separate coordinates into lat and long
    #Currently in degrees, minutes, seconds. Needs to be converted to decimal degrees.
    dplyr::mutate(Latitude = as.numeric(stringr::str_sub(.data$Coordinates, start = 2, end = 3)) + as.numeric(stringr::str_sub(.data$Coordinates, start = 5, end = 10))/60,
                  Longitude = as.numeric(stringr::str_sub(.data$Coordinates, start = 13, end = 14)) + as.numeric(stringr::str_sub(.data$Coordinates, start = 16, end = 21))/60,
                  NestboxID = .data$LocationID,
                  LocationType = "NB",
                  PopID = "VEL",
                  StartSeason = 1997L,
                  EndSeason = NA_integer_) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join({
      TIT_data %>%
        dplyr::group_by(.data$LocationID) %>%
        dplyr::summarise(HabitatType = dplyr::first(.data$HabitatType))},
      by = "LocationID") %>%
    dplyr::select("LocationID", "NestboxID", "LocationType", "PopID",
                  "Latitude", "Longitude", "StartSeason", "EndSeason", "HabitatType")

  #Some nest locations were not recorded in the Location data excel file. We still include these locations
  #but they will have no lat/long info
  location_data_nocoords <- tibble::tibble(LocationID = as.character(stats::na.omit(unique(Brood_data$LocationID))),
                                           NestboxID = .data$LocationID,
                                           LocationType = "NB",
                                           PopID = "VEL",
                                           Latitude = NA_real_,
                                           Longitude = NA_real_,
                                           StartSeason = 1997L,
                                           EndSeason = NA_integer_) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join({
      TIT_data %>%
        dplyr::group_by(.data$LocationID) %>%
        dplyr::summarise(HabitatType = dplyr::first(.data$HabitatType))},
      by = "LocationID") %>%
    ## Exclude locations that were already in the location data excel file.
    dplyr::filter(!.data$LocationID %in% location_data_excel$LocationID)

  return(dplyr::bind_rows(location_data_excel, location_data_nocoords))

}
