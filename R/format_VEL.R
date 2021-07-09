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
#' \strong{CaptureAlive, ReleaseAlive}: Assume all individuals were alive when captured and released.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export
#' @import stringr

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
    dplyr::mutate_at(.vars = dplyr::vars(contains("date")),
                     function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest(cols = c(.data$laying_date, .data$hatching_date, .data$date_of_capture_52, .data$date_of_capture_57)) %>%
    ## CHANGE COL NAMES TO MATCH STANDARD FORMAT
    dplyr::mutate(PopID = "VEL",
                  BreedingSeason = as.integer(.data$year),
                  Species = species_codes[which(species_codes$SpeciesID == 13480), ]$Species,
                  Plot = .data$plot,
                  LocationID = paste0(toupper(stringr::str_sub(.data$plot, end = 1)), .data$nest),
                  BroodID = paste(.data$BreedingSeason, .data$plot, stringr::str_pad(string = .data$nest, width = 2, pad = "0"),
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
                  ClutchSize_observed = as.integer(gsub(pattern = "\\+|\\?", replacement = "", .data$clutch_size)),
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
    dplyr::mutate_at(.vars = dplyr::vars(contains("date")),
                     function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest(cols = c(laying_date, laying_date_minimum, laying_date_maximum))

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
                  Species = dplyr::case_when(.data$species == "blue tit" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species,
                                             .data$species == "great tit" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species),
                  PopID = "VEL",
                  Plot = .data$plot,
                  LocationID = paste0(toupper(stringr::str_sub(.data$plot, end = 1)), .data$nest_box),
                  FemaleID = .data$female_ring,
                  MaleID = NA_character_,
                  ClutchType_observed = NA_character_,
                  LayDate_observed = TIT_LD_error$LayDate_observed,
                  LayDate_min = TIT_LD_error$LayDate_min,
                  LayDate_max = TIT_LD_error$LayDate_max,
                  ## We assume that "+", "-", or "?" in ClutchSize, BroodSize, NumberFledged
                  ## is a measure of uncertainty,
                  ##TODO but we need to check this with data owner
                  ClutchSize_observed = as.integer(gsub(pattern = "\\+|\\?", replacement = "", .data$clutch_size)),
                  ClutchSize_min = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\?") ~ 1,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  ClutchSize_max = dplyr::case_when(stringr::str_detect(.data$clutch_size, "\\+|\\?") ~ Inf,
                                                    TRUE ~ as.numeric(.data$ClutchSize_observed)),
                  HatchDate_observed = as.Date(NA),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = as.integer(gsub(pattern = "\\+|\\?", replacement = "", .data$number_hatched)),
                  BroodSize_min = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\?") ~ 1,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  BroodSize_max = dplyr::case_when(stringr::str_detect(.data$number_hatched, "\\+|\\?") ~ Inf,
                                                   TRUE ~ as.numeric(.data$BroodSize_observed)),
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = as.integer(gsub(pattern = "\\+|\\-|\\?", replacement = "", .data$number_fledged)),
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
    dplyr::filter(Species %in% species)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- dplyr::bind_rows(create_capture_VEL_FICALB(FICALB_data),
                                   create_capture_VEL_TIT(TIT_data)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Species %in% species)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_VEL(Capture_data) %>%
    dplyr::filter(Species %in% species)

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
    dplyr::summarise(AvgChickMass       = mean(.data$Mass, na.rm = TRUE),
                     NumberChicksMass   = length(stats::na.omit(.data$Mass)),
                     AvgTarsus          = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(.data$Tarsus)))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_measures, by = "BroodID") %>%
    dplyr::mutate(NumberChicksMass   = na_if(.data$NumberChicksMass, 0),
                  NumberChicksTarsus = na_if(.data$NumberChicksTarsus, 0),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Oxford")) %>%
    dplyr::select(.data$BroodID:.data$NumberEggs,
                  .data$AvgChickMass:.data$NumberChicksTarsus,
                  .data$OriginalTarsusMethod, .data$ExperimentID)

  Capture_data <- Capture_data %>%
    dplyr::select(.data$IndvID, .data$Species, Sex_observed = .data$Sex, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime, .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID:.data$Tarsus, .data$OriginalTarsusMethod,
                  .data$WingLength, .data$Age_observed, .data$Age_calculated, .data$ChickAge,
                  .data$ExperimentID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$CaptureID, everything())

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
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                  .data$Species, .data$Plot, .data$LocationID, .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$ExperimentID)

  TIT_broods <- TIT_data %>%
    dplyr::arrange(.data$BreedingSeason, .data$Species, .data$FemaleID, .data$LayDate_observed) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%
    dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                  .data$Species, .data$Plot, .data$LocationID, .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed, .data$ClutchType_calculated,
                  .data$LayDate_observed:.data$ExperimentID)

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

  ## First create a table for flycatcher chick captures on the nest
  FICALB_chicks <- FICALB_data %>%
    dplyr::select(.data$BreedingSeason, .data$Species, .data$Plot, .data$BroodID, .data$LocationID,
                  .data$LayDate_observed, .data$ClutchSize_observed, .data$HatchDate_observed,
                  .data$x1_young_ring:.data$x8y_wing, .data$ExperimentID) %>%
    tidyr::pivot_longer(cols = c("x1_young_ring", "x2_young_ring",
                                 "x3_young_ring", "x4_young_ring",
                                 "x5_young_ring", "x6_young_ring",
                                 "x7_young_ring", "x8_young_ring"),
                        values_to = "IndvID", names_to = "ChickNr") %>%
    dplyr::filter(!is.na(.data$IndvID))

  ## Make progress bar. Needs to be twice as long as rows because chicks are
  ## captured at 6 and 13 days
  pb <- progress::progress_bar$new(total = nrow(FICALB_chicks)*2)

  FICALB_chicks <- FICALB_chicks %>%
    dplyr::group_by(.data$ChickNr) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ChickNr = substr(.data$ChickNr, 1, 2)) %>%
    dplyr::mutate(data = purrr::pmap(.l = list(.data$ChickNr, data),
                                     .f = ~{

                                       output <- ..2 %>%
                                         dplyr::select(.data$BreedingSeason:.data$HatchDate_observed, .data$IndvID, contains(..1)) %>%
                                         tidyr::pivot_longer(cols = contains("_mass"), values_to = "Mass", names_to = "variable") %>%
                                         ## Rename tarsus and wing to remove name of chick
                                         dplyr::rename_at(.vars = dplyr::vars(contains("tarsus")), .funs = ~{"Tarsus"}) %>%
                                         dplyr::rename_at(.vars = dplyr::vars(contains("wing")), .funs = ~{"WingLength"}) %>%
                                         ## Add chick age based on the variable name
                                         dplyr::mutate(ChickAge = as.integer(stringr::str_sub(.data$variable, 9, -1)),
                                                       ## Capture date is hatchdate + chick age
                                                       ## If no hatch date is known then use laying date + clutch size + incubation + chick age
                                                       ##TODO We assume incubation of 15 days, but need to check with Milos.
                                                       ## If there is no hatch date OR laying date then just use NA
                                                       CaptureDate = purrr::pmap(.l = list(.data$LayDate_observed,
                                                                                           .data$ClutchSize_observed,
                                                                                           .data$HatchDate_observed,
                                                                                           .data$ChickAge),
                                                                                 .f = ~{

                                                                                   pb$tick()

                                                                                   if(!is.na(..3)){

                                                                                     return(..3 + lubridate::days(..4))

                                                                                   } else if(!is.na(..1)) {

                                                                                     return(..1 + lubridate::days(..2 + 15 + ..4))

                                                                                   } else {

                                                                                     return(NA)

                                                                                   }

                                                                                 }),
                                                       CaptureTime = NA_character_,
                                                       CapturePopID = "VEL",
                                                       CapturePlot = .data$Plot,
                                                       ## All chick records were 6 or 13 days, so all are listed as EURING age 1
                                                       ReleasePopID = "VEL",
                                                       ReleasePlot = .data$Plot,
                                                       Age_observed = 1L,
                                                       Age_calculated = NA_integer_,
                                                       #Convert tarsus to Svennson's alternative
                                                       Tarsus = convert_tarsus(.data$Tarsus, method = "Oxford")) %>%
                                         #dplyr::filter(!all(is.na(Tarsus) & is.na(WingLength) & is.na(Mass))) %>%
                                         tidyr::unnest(.data$CaptureDate) %>%
                                         dplyr::select(.data$IndvID, .data$Species, .data$BreedingSeason, .data$LocationID,
                                                       .data$CaptureDate, .data$CaptureTime, .data$CapturePopID, .data$CapturePlot,
                                                       .data$ReleasePopID, .data$ReleasePlot, .data$Mass, .data$Tarsus,
                                                       .data$WingLength, .data$Age_observed,
                                                       .data$Age_calculated, .data$ChickAge, .data$BroodID)

                                       ## When the chick is 6 days old, then tarsus and wing length are not used
                                       ## They were only collected at 13 days old
                                       output[output$ChickAge == 6, ]$Tarsus <- NA
                                       output[output$ChickAge == 6, ]$WingLength <- NA

                                       return(output)

                                     })) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(ObserverID = NA_character_,
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Oxford")) %>%
    dplyr::select(-.data$ChickNr) %>%
    #For records at 13 days, if there was no mass/tarsus we assume that it was not caught at 13 days
    dplyr::filter(.data$ChickAge == 6 | (.data$ChickAge == 13 & (!is.na(.data$Tarsus) | !is.na(.data$WingLength) | !is.na(.data$Mass))))

  #Then create capture info for every adult
  FICALB_adults <- FICALB_data %>%
    dplyr::select(.data$BreedingSeason, .data$Species, .data$Plot, .data$LocationID, .data$LayDate_observed,
                  .data$BroodID, .data$FemaleID, .data$date_of_capture_52, .data$tarsus_53:.data$wing_55,
                  .data$MaleID, .data$date_of_capture_57, .data$age:.data$wing_61,
                  .data$ExperimentID) %>%
    tidyr::pivot_longer(cols = c("FemaleID", "MaleID"), values_to = "IndvID", names_to = "Sex") %>%
    dplyr::filter(!is.na(.data$IndvID))

  FICALB_adults <- FICALB_adults %>%
    ## Give individuals a sex, we will use this in our Individual_data table
    dplyr::mutate(Sex = stringr::str_sub(.data$Sex, 0, 1),
                  Tarsus = purrr::pmap_dbl(.l = list(.data$Sex, .data$tarsus_53, .data$tarsus_59),
                                           .f = ~{if(..1 == "F") ..2 else ..3}),
                  WingLength = purrr::pmap_dbl(.l = list(.data$Sex, .data$wing_55, .data$wing_61),
                                               .f = ~{if(..1 == "F") ..2 else ..3}),
                  Mass = purrr::pmap_dbl(.l = list(.data$Sex, .data$mass_54, .data$mass_60),
                                         .f = ~{if(..1 == "F") ..2 else ..3}),
                  ##Determine age of males based on 'age' column
                  Age_observed = purrr::pmap_int(.l = list(.data$Sex, .data$age),
                                                 .f = ~{

                                                   if(..1 == "M"){

                                                     return(dplyr::case_when(..2 == "old" ~ 6L,
                                                                             ..2 == "young" ~ 5L))

                                                   } else {

                                                     return(NA_integer_)

                                                   }

                                                 }),
                  CapturePopID = "VEL",
                  ReleasePopID = "VEL",
                  CapturePlot = .data$Plot,
                  ReleasePlot = .data$Plot,
                  ObserverID = NA_character_,
                  CaptureDate = purrr::pmap(.l = list(.data$Sex, .data$date_of_capture_52, .data$date_of_capture_57),
                                            .f = ~{

                                              if(..1 == "F"){

                                                return(..2)

                                              } else {

                                                return(..3)

                                              }

                                            })) %>%
    tidyr::unnest(cols = c(.data$CaptureDate)) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$Tarsus) ~ "Oxford")) %>%
    dplyr::select(.data$Species, .data$BreedingSeason, .data$LocationID, .data$BroodID,
                  .data$Sex:.data$CaptureDate, .data$ExperimentID)

  FICALB_alldat <- dplyr::bind_rows(FICALB_chicks, FICALB_adults) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed, Date = .data$CaptureDate, Year = .data$BreedingSeason, showpb = TRUE) %>%
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
    dplyr::mutate(Species = dplyr::case_when(.data$species == "blue tit" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species,
                                             .data$species == "great tit" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species),
                  ## Make the capture date the date that incubation would start (laying date + clutch size)
                  CaptureDate = .data$LayDate_observed + .data$ClutchSize_observed,
                  IndvID = .data$FemaleID,
                  CapturePopID = .data$PopID, CapturePlot = .data$Plot,
                  ReleasePopID = .data$PopID, ReleasePlot = .data$Plot,
                  ## There is no explicit info about age.
                  ## They must all be adults, so just give them all EURING 4
                  ## "Hatched before this calendar year"
                  Age_observed = 4L,
                  ObserverID = NA_character_,
                  LocationID = paste(.data$Plot, .data$nest_box, sep = "_"),
                  OriginalTarsusMethod = NA_character_,
                  Sex = "F") %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed, Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    # We have no information on status of captures/releases, so we assume all individuals were captured/released alive
    dplyr::mutate(CaptureAlive = TRUE,
                  ReleaseAlive = TRUE) %>%
    dplyr::select(.data$IndvID, .data$Species, .data$BreedingSeason, .data$CaptureDate, .data$ObserverID,
                  .data$LocationID, .data$CapturePopID:.data$ReleasePlot, .data$Age_observed, .data$Age_calculated, .data$Sex,
                  .data$CaptureAlive, .data$ReleaseAlive, .data$ExperimentID)

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

  pb <- progress::progress_bar$new(total = length(unique(Capture_data$IndvID)) * 2)

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
    dplyr::mutate(BroodIDLaid = purrr::pmap_chr(.l = list(.data$RingAge, .data$FirstBroodID),
                                                .f = ~{

                                                  pb$tick()

                                                  if(is.na(..1) | (..1 == "adult")){

                                                    return(NA)

                                                  } else {

                                                    return(..2)

                                                  }

                                                }),
                  BroodIDFledged = .data$BroodIDLaid,
                  Sex_genetic = NA_character_) %>%
    dplyr::select(.data$IndvID, .data$Species, .data$PopID, .data$BroodIDLaid, .data$BroodIDFledged,
                  .data$RingSeason, .data$RingAge, .data$Sex_calculated, .data$Sex_genetic) %>%
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
    dplyr::rename(LocationID = .data$budka,
                  Coordinates = .data$sirka_delka_hddd_mm_mmm,
                  Altitude = .data$nadmorska_vyska) %>%
    #Separate coordinates into lat and long
    #Currently in degrees, minutes, seconds. Needs to be converted to decimal degrees.
    dplyr::mutate(Latitude = as.numeric(stringr::str_sub(.data$Coordinates, start = 2, end = 3)) + as.numeric(stringr::str_sub(.data$Coordinates, start = 5, end = 10))/60,
                  Longitude = as.numeric(stringr::str_sub(.data$Coordinates, start = 13, end = 14)) + as.numeric(stringr::str_sub(.data$Coordinates, start = 16, end = 21))/60,
                  NestboxID = .data$LocationID, LocationType = "NB", PopID = "VEL",
                  StartSeason = 1997L, EndSeason = NA_integer_) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join({
      TIT_data %>%
        dplyr::group_by(.data$LocationID) %>%
        dplyr::summarise(HabitatType = dplyr::first(.data$HabitatType))},
      by = "LocationID") %>%
    dplyr::select(.data$LocationID, .data$NestboxID, .data$LocationType, .data$PopID,
                  .data$Latitude, .data$Longitude, .data$StartSeason, .data$EndSeason, .data$HabitatType)

  #Some nest locations were not recorded in the Location data excel file. We still include these locations
  #but they will have no lat/long info
  location_data_nocoords <- tibble::tibble(LocationID = as.character(stats::na.omit(unique(Brood_data$LocationID))),
                                           NestboxID = .data$LocationID,
                                           LocationType = "NB",
                                           PopID = "VEL",
                                           Latitude = NA_real_,
                                           Longitude = NA_real_,
                                           StartSeason = 1997L, EndSeason = NA_integer_) %>%
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
