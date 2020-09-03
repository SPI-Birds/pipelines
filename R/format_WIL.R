#'Construct standard format for data from the Univeristy of Antwerp.
#'
#'A pipeline to produce the standard format for 2 hole-nesting bird study
#'populations administered by the University of Antwerp.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{LocationID}: I assume that all the columns with K represent different nestbox/location IDs.
#'
#'\strong{LayDate}: Take Date - nr_eggs + 1 from the first observation containing 'ei'. Do we need to account for cell colours?
#'
#'\strong{ClutchSize}: Take largest value in observations containing 'ei'
#'
#'\strong{BroodSize}: Take number of chicks ringed (assume there are no chicks that hatch but are unringed...not great).
#'In the summarised data this is the value of 'NumberFledglings'...are chicks ringed at fledging? Then we only know broodsize through estimation.
#'
#'\string{IndvID}: For chicks, we need to expand out a sequence of rings, using 'chick_expand.R'. There are some that
#'have obvious typos where the number of ringed chicks would be too large. In these cases we return a flag to
#'follow up later.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_WIL <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force choose_directory() if used
  force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  start_time <- Sys.time()

  message("\n Loading all files")

  Bluetit_filepath <- list.files(path = db, pattern = "broedpc", full.names = TRUE)
  Bluetit_filename <- list.files(path = db, pattern = "broedpc")
  Bluetit_data  <- purrr::map(.x = Bluetit_filepath,
                               .f = ~{

                                readxl::read_excel(..1, col_names = FALSE)

                              }) %>%
    setNames(stringr::str_extract(Bluetit_filename, pattern = "[0-9]{4}"))

  Greattit_filepath <- list.files(path = db, pattern = "broedpm", full.names = TRUE)
  Greattit_filename <- list.files(path = db, pattern = "broedpm")
  Greattit_data     <- purrr::map(.x = Greattit_filepath,
                              .f = ~{

                                readxl::read_excel(..1, col_names = FALSE)

                              }) %>%
    setNames(stringr::str_extract(Greattit_filename, pattern = "[0-9]{4}"))

  # BROOD DATA

  message("\n Compiling brood information...")

  Brood_data <- create_brood_WIL(Bluetit_data, Greattit_data)

  # CAPTURE DATA

  message("\n Compiling capture information...")

  Capture_data <- create_capture_WIL(CAPTURE_info, species)

  # INDIVIDUAL DATA

  message("\n Compiling individual information...")

  Individual_data <- create_individual_WIL(INDV_info, CAPTURE_info, species)

  # LOCATION DATA

  message("\n Compiling location information...")

  Location_data <- create_location_WIL(BOX_info)

  #WRANGLE DATA FOR EXPORT

  #We need to check that AvgChickMass and AvgTarsus are correct (i.e. it only uses chicks 14 - 16 days)
  avg_chick_data <- Capture_data %>%
    dplyr::filter(between(ChickAge, 14, 16)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass_capture = mean(Mass, na.rm = TRUE), AvgTarsus_capture = mean(Tarsus, na.rm = TRUE))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_chick_data, by = "BroodID") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(AvgChickMass = ifelse(!is.na(AvgChickMass_capture), AvgChickMass_capture, AvgChickMass),
                  AvgTarsus = ifelse(!is.na(AvgTarsus_capture), AvgTarsus_capture, AvgTarsus)) %>%
    dplyr::select(-AvgChickMass_capture, -AvgTarsus_capture)

  Capture_data <- Capture_data %>%
    dplyr::select(-BroodID)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("\n Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WIL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WIL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WIL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WIL.csv"), row.names = F)

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

#' Create brood data table for data from University of Antwerp, Belgium.
#'
#' Create brood data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param CAPTURE_info Capture data table from the raw data
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_brood_WIL <- function(BT_data, GT_data){

  Bluetit_broods_wIDs <- BT_data %>%
    purrr::map2_df(.x = .,
                .y = names(.),
                .f = function(raw_data, year){

                  print(year)

                  #Top left should always be called Datum (there are some cases where this is left blank)
                  raw_data[1, 1] <- "Datum"

                  #Are there multiple columns with IDs (there can be more than 1 because previous year IDs are also recorded)
                  male_match   <- ifelse(sum(stringr::str_detect(unique(raw_data$`...1`), "^man"), na.rm = TRUE) > 1, paste0("^man.*", year, "$"), "^man")
                  female_match <- ifelse(sum(stringr::str_detect(unique(raw_data$`...1`), "^wij|^vro"), na.rm = TRUE) > 1, paste0("^wij.*", year, "$|^vro.*", year, "$"), "^wij|^vro")


      ID_data <- raw_data %>%
        dplyr::filter(stringr::str_detect(.data$`...1`, "Datum|^ring") | stringr::str_detect(.data$`...1`, male_match) | stringr::str_detect(.data$`...1`, female_match)) %>%
        tidyr::pivot_longer(col = -`...1`) %>%
        dplyr::mutate(column = dplyr::case_when(`...1` == "Datum" ~ "LocationID",
                                                stringr::str_detect(`...1`, "^man") ~ "MaleID",
                                                stringr::str_detect(`...1`, "^wij|^vro") ~ "FemaleID",
                                                stringr::str_detect(`...1`, "^ring") ~ "ChickIDs",
                                                TRUE ~ `...1`)) %>%
        dplyr::select(column, value) %>%
        tidyr::pivot_wider(names_from = column, values_from = value, values_fn = list) %>%
        tidyr::unnest(cols = c(LocationID, MaleID, FemaleID, ChickIDs)) %>%
        dplyr::mutate(chick_expand(ChickIDs)) %>%
        dplyr::select(-ChickIDs) %>%
        dplyr::mutate(LocationID = stringr::str_replace(stringr::str_remove_all(LocationID, pattern = ' '), pattern = '\"', replacement = "''"),
                      PopID = "WIL", BreedingSeason = as.integer(year),
                      Species = Species_codes$Code[Species_codes$SpeciesID == 14620],
                      Plot = NA, BroodID = paste(LocationID, BreedingSeason, sep = "_"))

      Observation_data <- raw_data %>%
        #Use nestbox as the column name so they can be correctly linked back
        dplyr::filter(!stringr::str_detect(.data$`...1`, "^man|^wij|^vro|^ring"))

      Box_vector <- t(Observation_data)[-1, 1]

      #If there are any cases of duplicate boxes
      if (length(Box_vector) != length(unique(Box_vector))) {

        tibble::tibble(NestboxID = Box_vector) %>%
          dplyr::group_by(.data$NestboxID) %>%
          dplyr::mutate(NestboxID = paste(NestboxID, 1:n(), sep = "_")) %>%
          pull(.data$NestboxID) -> Box_vector

      }

      Observation_data <- Observation_data %>%
        setNames(c("Datum", Box_vector)) %>%
        dplyr::filter(stringr::str_detect(.data$Datum, pattern = "^[0-9]+")) %>%
        dplyr::mutate(Datum = convert_dates(.data$Datum)) %>%
        tidyr::pivot_longer(cols = -Datum, names_to = "LocationID", values_to = "Observation") %>%
        dplyr::mutate(LocationID = stringr::str_replace(stringr::str_remove_all(LocationID, pattern = ' '), pattern = '\"', replacement = "''"),
                      BroodID = paste(LocationID, year, sep = "_")) %>%
        dplyr::arrange(BroodID, Datum) %>%
        dplyr::select(-LocationID) %>%
        dplyr::left_join(ID_data, by = "BroodID") %>%
        dplyr::filter(!is.na(Observation))

      Egg_data <- Observation_data %>%
        dplyr::filter(stringr::str_detect(Observation, "[0-9]{1,2} ei")) %>%
        dplyr::mutate(NrEgg = as.numeric(stringr::str_extract(Observation, "[0-9]{1,2}"))) %>%
        dplyr::group_by(BroodID) %>%
        dplyr::summarise(LayDate_observed = Datum[1] - (NrEgg[1] - 1),
                         ClutchSize_observed = max(NrEgg),
                         BroodSize_observed = rowSums(across(starts_with("ChickID"), ~!is.na(.x)))[1],
                         .groups = "drop")

      Fledge_data <- Observation_data %>%
        dplyr::filter(stringr::str_detect(Observation, "[0-9]{1,2}.*jn")) %>%
        dplyr::mutate(Fledge_str = stringr::str_extract(Observation, "[0-9]{1,2}.*jn"),
                      NrFledge = as.numeric(stringr::str_extract(Fledge_str, "[0-9]{1,2}"))) %>%
        dplyr::arrange(BroodID, Datum) %>%
        dplyr::group_by(BroodID) %>%
        dplyr::summarise(FledgeDate_observed = as.Date(NA),
                         NumberFledged_observed = NrFledge[n()],
                         .groups = "drop")

      Brood_data <- Egg_data %>%
        dplyr::left_join(Fledge_data, by = "BroodID") %>%
        dplyr::left_join(ID_data, by = "BroodID") %>%
        dplyr::mutate(ClutchType_observed = NA_character_,
                      ClutchType_calculated = NA_character_, ##FIXME: Need to add this info
                      LayDate_min = as.Date(NA), LayDate_max = as.Date(NA),
                      ClutchSize_min = NA_integer_, ClutchSize_max = NA_integer_,
                      HatchDate_observed = as.Date(NA), HatchDate_min = as.Date(NA), HatchDate_max = as.Date(NA),
                      BroodSize_min = NA_integer_, BroodSize_max = NA_integer_,
                      FledgeDate_min = as.Date(NA), FledgeDate_max = as.Date(NA),
                      NumberFledged_min = NA_integer_, NumberFledged_max = NA_integer_,
                      AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                      AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                      AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_,
                      OriginalTarsusMethod = NA_character_,
                      ExperimentID = NA_character_) %>% ##FIXME: Need to determine if any experiments were carried out
        dplyr::select(BroodID, PopID, BreedingSeason,
                      Species, Plot, LocationID,
                      FemaleID, MaleID,
                      LayDate_observed, LayDate_min, LayDate_max,
                      ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                      HatchDate_observed, HatchDate_min, HatchDate_max,
                      BroodSize_observed, BroodSize_min, BroodSize_max,
                      FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                      NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                      AvgEggMass:ExperimentID)

      return(Brood_data)

    })

  Greattit_broods_wIDs <- GT_data %>%
    purrr::map2_df(.x = .,
                   .y = names(.),
                   .f = function(raw_data, year){

                     print(year)

                     browser(year == "2002")

                     #Top left should always be called Datum (there are some cases where this is left blank)
                     raw_data[1, 1] <- "Datum"

                     #Are there multiple columns with IDs (there can be more than 1 because previous year IDs are also recorded)
                     male_match   <- ifelse(sum(stringr::str_detect(unique(raw_data$`...1`), "^man"), na.rm = TRUE) > 1, paste0("^man.*", year, "$"), "^man")
                     female_match <- ifelse(sum(stringr::str_detect(unique(raw_data$`...1`), "^wij|^vro"), na.rm = TRUE) > 1, paste0("^wij.*", year, "$|^vro.*", year, "$"), "^wij|^vro")

                     #Remove duplicates (which can occur when there are redundant male/female rows)
                     raw_data <- raw_data %>%
                       dplyr::group_by(`...1`) %>%
                       dplyr::slice(1) %>%
                       dplyr::ungroup(`...1`)

                     ID_data <- raw_data %>%
                       dplyr::filter(stringr::str_detect(.data$`...1`, "Datum|^ring") | stringr::str_detect(.data$`...1`, male_match) | stringr::str_detect(.data$`...1`, female_match)) %>%
                       tidyr::pivot_longer(col = -`...1`) %>%
                       dplyr::mutate(column = dplyr::case_when(`...1` == "Datum" ~ "LocationID",
                                                               stringr::str_detect(`...1`, "^man") ~ "MaleID",
                                                               stringr::str_detect(`...1`, "^wij|^vro") ~ "FemaleID",
                                                               stringr::str_detect(`...1`, "^ring") ~ "ChickIDs",
                                                               TRUE ~ `...1`)) %>%
                       dplyr::select(column, value) %>%
                       tidyr::pivot_wider(names_from = column, values_from = value, values_fn = list) %>%

                       #In early years there is no chick data recorded, so we need to create an empty ChickIDs column
                       {

                         if (!"ChickIDs" %in% colnames(.)) {

                           dplyr::mutate(., ChickIDs = list(NA_character_))

                         } else {

                           .

                         }

                       } %>%

                       tidyr::unnest(cols = c(LocationID, MaleID, FemaleID, ChickIDs)) %>%
                       dplyr::mutate(chick_expand(ChickIDs)) %>%
                       dplyr::select(-ChickIDs) %>%
                       dplyr::mutate(LocationID = stringr::str_replace(stringr::str_remove_all(LocationID, pattern = ' '), pattern = '\"', replacement = "''"),
                                     PopID = "WIL", BreedingSeason = as.integer(year),
                                     Species = Species_codes$Code[Species_codes$SpeciesID == 14620],
                                     Plot = NA, BroodID = paste(LocationID, BreedingSeason, sep = "_"))

                     Observation_data <- raw_data %>%
                       #Use nestbox as the column name so they can be correctly linked back
                       dplyr::filter(!stringr::str_detect(.data$`...1`, "^man|^wij|^vro|^ring"))

                     Box_vector <- t(Observation_data)[-1, 1]

                     #If there are any cases of duplicate boxes
                     if (length(Box_vector) != length(unique(Box_vector))) {

                       tibble::tibble(NestboxID = Box_vector) %>%
                         dplyr::group_by(.data$NestboxID) %>%
                         dplyr::mutate(NestboxID = paste(NestboxID, 1:n(), sep = "_")) %>%
                         pull(.data$NestboxID) -> Box_vector

                     }

                     Observation_data <- Observation_data %>%
                       setNames(c("Datum", Box_vector)) %>%
                       dplyr::filter(stringr::str_detect(.data$Datum, pattern = "^[0-9]+")) %>%
                       dplyr::mutate(Datum = convert_dates(.data$Datum)) %>%
                       tidyr::pivot_longer(cols = -Datum, names_to = "LocationID", values_to = "Observation") %>%
                       dplyr::mutate(LocationID = stringr::str_replace(stringr::str_remove_all(LocationID, pattern = ' '), pattern = '\"', replacement = "''"),
                                     BroodID = paste(LocationID, year, sep = "_")) %>%
                       dplyr::arrange(BroodID, Datum) %>%
                       dplyr::select(-LocationID) %>%
                       dplyr::left_join(ID_data, by = "BroodID") %>%
                       dplyr::filter(!is.na(Observation))

                     Egg_data <- Observation_data %>%
                       dplyr::filter(stringr::str_detect(Observation, "[0-9]{1,2}\\s*ei")) %>%
                       dplyr::mutate(NrEgg = as.numeric(stringr::str_extract(Observation, "[0-9]{1,2}"))) %>%
                       dplyr::group_by(BroodID) %>%
                       dplyr::summarise(LayDate_observed = Datum[1] - (NrEgg[1] - 1),
                                        ClutchSize_observed = max(NrEgg),
                                        BroodSize_observed = rowSums(across(starts_with("ChickID"), ~!is.na(.x)))[1],
                                        .groups = "drop")

                     Fledge_data <- Observation_data %>%
                       dplyr::filter(stringr::str_detect(Observation, "[0-9]{1,2}.*j")) %>%
                       dplyr::mutate(Fledge_str = stringr::str_extract(Observation, "[0-9]{1,2}.*j"),
                                     NrFledge = as.numeric(stringr::str_extract(Fledge_str, "[0-9]{1,2}"))) %>%
                       dplyr::arrange(BroodID, Datum) %>%
                       dplyr::group_by(BroodID) %>%
                       dplyr::summarise(FledgeDate_observed = as.Date(NA),
                                        NumberFledged_observed = NrFledge[n()],
                                        .groups = "drop")

                     Brood_data <- Egg_data %>%
                       dplyr::left_join(Fledge_data, by = "BroodID") %>%
                       dplyr::left_join(ID_data, by = "BroodID") %>%
                       dplyr::mutate(ClutchType_observed = NA_character_,
                                     ClutchType_calculated = NA_character_, ##FIXME: Need to add this info
                                     LayDate_min = as.Date(NA), LayDate_max = as.Date(NA),
                                     ClutchSize_min = NA_integer_, ClutchSize_max = NA_integer_,
                                     HatchDate_observed = as.Date(NA), HatchDate_min = as.Date(NA), HatchDate_max = as.Date(NA),
                                     BroodSize_min = NA_integer_, BroodSize_max = NA_integer_,
                                     FledgeDate_min = as.Date(NA), FledgeDate_max = as.Date(NA),
                                     NumberFledged_min = NA_integer_, NumberFledged_max = NA_integer_,
                                     AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                                     AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                                     AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_,
                                     OriginalTarsusMethod = NA_character_,
                                     ExperimentID = NA_character_) %>% ##FIXME: Need to determine if any experiments were carried out
                       dplyr::select(BroodID, PopID, BreedingSeason,
                                     Species, Plot, LocationID,
                                     FemaleID, MaleID,
                                     LayDate_observed, LayDate_min, LayDate_max,
                                     ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                                     HatchDate_observed, HatchDate_min, HatchDate_max,
                                     BroodSize_observed, BroodSize_min, BroodSize_max,
                                     FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                                     NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                                     AvgEggMass:ExperimentID)

                     return(Brood_data)

                   })

}

#' Create capture data table for data from University of Antwerp, Belgium.
#'
#' Create capture data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_capture_WIL <- function(data, species_filter){

  #Capture data includes all times an individual was captured (with measurements
  #like mass, tarsus etc.). This will include first capture as nestling (for
  #residents) This means there will be multiple records for a single individual.

  pb <- dplyr::progress_estimated(n = nrow(data) * 2)

  Capture_data <- data %>%
    #Adjust species and PopID
    dplyr::mutate(CapturePopID = dplyr::case_when(.$CapturePopID == "FR" ~ "BOS",
                                                  .$CapturePopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code,
                                             .$Species == "pc" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code)) %>%
    #Filter by species
    dplyr::filter(Species %in% species_filter) %>%
    #Make tarsus length into standard method (Svensson Alt)
    #Firstly, convert the Svennson's standard measures to Svennson's Alt.
    #Then only use this converted measure when actual Svennson's Alt is unavailable.
    dplyr::mutate(TarsusStandard = convert_tarsus(TarsusStandard, method = "Standard")) %>%
    #Add tarsus and original tarsus method with bind_cols
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(SvenStd = .$TarsusStandard, SvenAlt = .$TarsusAlt),
                                     function(SvenStd, SvenAlt){

                                       pb$print()$tick()

                                       if(!is.na(SvenAlt)){

                                         return(tibble::tibble(Tarsus = SvenAlt, OriginalTarsusMethod = "Alternative"))

                                       } else if(!is.na(SvenStd)){

                                         return(tibble::tibble(Tarsus = SvenStd, OriginalTarsusMethod = "Standard"))

                                       } else {

                                         return(tibble::tibble(Tarsus = NA_real_, OriginalTarsusMethod = NA_character_))

                                       }})) %>%
    #Create NAs and convert date/time
    dplyr::mutate(CaptureDate = lubridate::ymd(CaptureDate),
                  BreedingSeason = as.integer(lubridate::year(CaptureDate)),
                  CaptureTime = na_if(paste(CaptureTime %/% 1,
                                            stringr::str_pad(string = round((CaptureTime %% 1)*60),
                                                             width = 2,
                                                             pad = "0"), sep = ":"), "NA:NA"),
                  ReleasePopID = CapturePopID, ReleasePlot = CapturePlot) %>%
    #Calculate age at capture and chick age based on the LT column
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(.$Age_observed, .$CaptureMethod),
                                     .f = ~{

                                       pb$print()$tick()

                                       # If Age (LT) was not recorded
                                       # instead estimate age from the capture type:
                                       if(is.na(..1) | ..1 == 0){

                                         #Capture type P and PP are chicks in the nest
                                         if(..2 %in% c("P", "PP")){

                                           return(tibble::tibble(Age_observed_new = 1, ChickAge = NA_integer_))

                                           #Captures in mist nets, observed rings, cage traps, roost checks
                                           #must be able to fly. But these can be anything from fledglings
                                           #in first calendar year +
                                           #Comment this out because it's really age calculated rather than observed
                                           # } else if (.y %in% c("FU", "GE", "MN", "ON", "NO", "SK", "SL", "LS")){
                                           #
                                           #   return(tibble::tibble(Age_observed = 2, ChickAge = NA))

                                         } else {

                                           #If no age and no chick capture type is given, then observed age is unknown.
                                           return(tibble::tibble(Age_observed_new = NA_integer_, ChickAge = NA_integer_))

                                         }

                                       #If age is > 5 this is the chick age in days
                                       } else if(..1 > 5){

                                         return(tibble::tibble(Age_observed_new = 1, ChickAge = ..1))

                                       } else {

                                         #If it's 1-5 then we translate into EURING codes for adults
                                         if(..1 %in% c(1, 2)){

                                           return(tibble::tibble(Age_observed_new = 1 + ..1*2, ChickAge = NA_integer_))

                                         } else if (..1 %in% c(3, 4)) {

                                           return(tibble::tibble(Age_observed_new = 4 + (..1 - 3)*2, ChickAge = NA_integer_))

                                         } else {

                                           return(tibble::tibble(Age_observed_new = NA, ChickAge = NA_integer_))

                                         }

                                       }

                                     })) %>%
    #Determine age at first capture for every individual
    dplyr::mutate(ischick = dplyr::case_when(.$Age_observed_new <= 3 ~ 1L)) %>%
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    #Arrange columns
    #Replace Age_observed with Age_observed_new which has been converted to EURING codes
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime,
                  ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed = Age_observed_new, Age_calculated, ChickAge, BroodID)

  return(Capture_data)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  TarsusStandard <- `.` <- ischick <- NULL

}

#' Create individual data table for data from University of Antwerp, Belgium.
#'
#' Create individual data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#' @param CAPTURE_info Capture data table from the raw data
#' @param species_filter 6 letter species codes for filtering data.
#'
#' @return A data frame.

create_individual_WIL <- function(data, CAPTURE_info, species_filter){

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  #To determine which brood an individual is from, we subset the CAPTURE_info table
  #and include only those records where an individual was caught as a nestling (i.e. Age_observed > 5 where age is in days).
  #We then take the nest that this nestling was in when it was caught.
  Indv_broods <- CAPTURE_info %>%
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::filter(!is.na(BroodID) & (Age_observed > 5 | Age_observed == 1)) %>%
    dplyr::summarise(BroodIDLaid = as.character(first(BroodID)))

  #Do this for PopID, capture age and first year as well
  Indv_Pop <- CAPTURE_info %>%
    dplyr::filter(!is.na(CapturePopID)) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::arrange(CaptureDate, .by_group = TRUE) %>%
    dplyr::summarise(PopID = first(CapturePopID),
                     FirstAge = first(Age_observed),
                     FirstYear = as.integer(min(lubridate::year(CaptureDate))))

  #There were no translocations, so BroodIDLaid/Fledged are the same
  Indv_data <- data %>%
    dplyr::left_join(Indv_broods, by = "IndvID") %>%
    dplyr::left_join(Indv_Pop, by = "IndvID") %>%
    #Add and filter by species
    dplyr::mutate(PopID = dplyr::case_when(.$PopID == "FR" ~ "BOS",
                                           .$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code,
                                             .$Species == "pc" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code),
                  Sex = dplyr::case_when(.$Sex %in% c(1, 3) ~ "M",
                                         .$Sex %in% c(2, 4) ~ "F")) %>%
    dplyr::filter(!is.na(Species) & Species %in% species_filter) %>%
    dplyr::mutate(BroodIDFledged = BroodIDLaid,
                  RingSeason = FirstYear,
                  RingAge = dplyr::case_when(.$FirstAge > 5 | .$FirstAge == 1 ~ "chick",
                                             is.na(.$FirstAge) | .$FirstAge <= 5 ~ "adult")) %>%
    select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex)

  return(Indv_data)

}

#' Create location data table for data from University of Antwerp, Belgium.
#'
#' Create location data table in standard format for data from University of
#' Antwerp, Belgium.
#'
#' @param data Data frame. Primary data from University of Antwerp.
#'
#' @return A data frame.

create_location_WIL <- function(data){

  Location_data <- data %>%
    dplyr::mutate(LocationID = GBPL, LocationType = dplyr::case_when(TYPE %in% c("pc", "pm", "cb") | is.na(TYPE) ~ "NB",
                                                                        TYPE == "FPT" ~ "FD",
                                                                        TYPE %in% c("PMO", "&") ~ "MN")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(NestboxID = ifelse(LocationType == "NB", LocationID, NA_character_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PopID = dplyr::case_when(.$SA == "FR" ~ "BOS",
                                           .$SA == "PB" ~ "PEE"),
                  Latitude = Y_deg, Longitude = X_deg,
                  Latitude_Lambert = Y, Longitude_Lambert = X,
                  StartSeason = as.integer(YEARFIRST), EndSeason = as.integer(YEARLAST),
                  Habitat = "deciduous",
                  HasCoords = as.factor(!is.na(Latitude_Lambert))) %>%
    #Split into two groups whether they have coordinates or not
    split(f = .$HasCoords)

  #For the group without coordinates in degrees but with Lambert, we use Lambert coordinates instead.
  #Turn it into an sf object and change the CRS to be WGS84
  true_coords <- sf::st_as_sf(Location_data$'TRUE',
                              coords = c("Longitude_Lambert", "Latitude_Lambert"),
                              crs = 31370) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates()

  Location_data$'TRUE'$Longitude <- true_coords[, 1]
  Location_data$'TRUE'$Latitude <- true_coords[, 2]

  Location_data <- dplyr::bind_rows(Location_data) %>%
    dplyr::select(-HasCoords, -Latitude_Lambert, -Longitude_Lambert)

  return(Location_data)

}



###################
convert_dates <- function(date_vector){

  #Trim and lower in case this will intefere with string matching
  date_vector <- stringr::str_trim(date_vector) %>%
    tolower()

  for (i in 1:nrow(dutch_months)) {

    date_vector <- stringr::str_replace_all(date_vector,
                                            pattern = dutch_months$dutch[i],
                                            replacement = dutch_months$english[i])

  }

  #Split data into numbers and non-numbers (should allow us to work vectorially which will be faster)
  split_dates <- date_vector %>%
    split(stringr::str_detect(date_vector, pattern = "^[0-9]*$"))

  split_dates$`FALSE` <- as.Date(split_dates$`FALSE`, format = "%d %b %Y")
  split_dates$`TRUE`  <- janitor::excel_numeric_to_date(as.numeric(split_dates$`TRUE`))

  output <- do.call(c, split_dates)

  return(output)

}
