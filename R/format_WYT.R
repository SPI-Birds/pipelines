#' Construct standard summary for data from Wytham Woods, UK.
#'
#' A pipeline to produce a standard output for the nest box population in Wytham
#' Woods, UK, administered by Edward Grey Institute Oxford (Ben Sheldon).
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see XXXXX
#' PLACE HOLDER!
#'
#' \strong{ExperimentID}: There are experiment codes given, I have tried to
#' adapt these to the ExperimentID categories described in the standard
#' protocol; however, these need to be checked by data owners. For now I assume:
#' - Egg manipulation (code 2) is clutch size manipulation.
#' - Chick manipulation (code 3) is cross fostering.
#' - Alter temperature in nest box (code 7) only affects phenology.
#' - Feeding manipulation (code 8) affects phenology.
#' - Altering parasites, predation, competition and territory quality (codes 9, 11, 12, 13) all affect survival.
#'
#' \strong{Species}: We include nests form blue tits, great tits, coal tits,
#' marsh tits, and nuthatches. Currently, mixed broods are treated as having no
#' species (NA), but we will fix this. There is one brood with Species 'w',
#' which I suspect is willow tit. This is currently ignored.
#'
#' \strong{AvgEggMass}: There are two columns with mass data, one is a 'legacy'
#' column. When these overlap, they can differ up to 5g! I assume the 'legacy'
#' column is less prefered (due to it's name). I only use this data where no
#' other egg mass data is provided.
#'
#' \strong{AvgChickMass}: As with AvgChickMass, there is also a 'legacy' column.
#' This is only used if the regular column is empty.
#'
#' \strong{HatchDate}: As with AvgEggMass, there is also a 'legacy' column for
#' hatch date. This is only used if the regular column is empty.
#'
#'@inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export

format_WYT <- function(db = utils::choose.dir(),
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

  start_time <- Sys.time()

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_WYT(db = db)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_WYT(db = db, Brood_data = Brood_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_WYT(Capture_data)

  # WRANGLE DATA FOR EXPORT

  #Remove unneeded Capture columns
  Capture_data <- Capture_data %>%
    dplyr::select(-Sex, -BroodIDLaid, -BroodIDFledged, -HatchDate)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WYT.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WYT.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_WYT.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = NA))

  }

}

create_brood_WYT <- function(db){

  Brood_data_raw <- utils::read.csv(paste0(db, "/WYT_PrimaryData_Brood.csv"), header = T,
                              sep = ",", stringsAsFactors = FALSE) %>%
    janitor::clean_names()

  pb <- dplyr::progress_estimated(n = nrow(Brood_data_raw)*3)

  Brood_data <- Brood_data_raw %>%
    #Rename columns to meet our standard format
    dplyr::mutate(BreedingSeason = year, LocationID = nestbox,
                  PopID = "WYT", Plot = toupper(section),
                  Species = dplyr::case_when(.$species == "b" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .$species == "g" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .$species == "c" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                             .$species == "n" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .$species == "m" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code),
                  LayDate = as.Date(lay_date, format = "%d/%m/%Y"),
                  HatchDate = as.Date(purrr::pmap_chr(.l = list(hatch_date, legacy_april_hatch_date),
                                              .f = ~{

                                                pb$print()$tick()

                                                if(!is.na(..1)){

                                                  return(as.character(..1))

                                                } else {

                                                  return(as.character(..2))

                                                }

                                              }), format = "%d/%m/%Y"),
                  NumberEggs = num_eggs_weighed,
                  AvgEggMass = purrr::pmap_dbl(.l = list(total_egg_weight, num_eggs_weighed, legacy_average_egg_weight),
                                               .f = ~{

                                                 pb$print()$tick()

                                                 if(!is.na(..1) & !is.na(..2)){

                                                   return(..1/..2)

                                                 } else {

                                                   return(..3)

                                                 }

                                               }),
                  ClutchSize = clutch_size,
                  BroodSize = num_chicks,
                  NumberFledged = num_fledglings,
                  AvgChickMass = purrr::pmap_dbl(.l = list(mean_chick_weight, legacy_mean_fledge_weight),
                                                 .f = ~{

                                                   pb$print()$tick()

                                                   if(!is.na(..1)){

                                                     return(..1)

                                                   } else {

                                                     return(..2)

                                                   }

                                                 }),
                  NumberChicksMass = num_chicks_ringed,
                  FemaleID = mother,
                  MaleID = father,
                  ExperimentID = dplyr::case_when(.$experiment_codes == "1" ~ "UNKOWN",
                                                  .$experiment_codes == "2" ~ "COHORT",
                                                  .$experiment_codes == "3" ~ "PARENTAGE",
                                                  .$experiment_codes == "7" ~ "PHENOLOGY",
                                                  .$experiment_codes == "8" ~ "PHENOLOGY",
                                                  .$experiment_codes == "9" ~ "SURVIVAL",
                                                  .$experiment_codes == "11" ~ "SURVIVAL",
                                                  .$experiment_codes == "12" ~ "SURVIVAL",
                                                  .$experiment_codes == "13" ~ "SURVIVAL"),
                  BroodID = pnum) %>%
    #Separate out chick ids into different columns. N.B. Currently, there is only
    #the chick id column and no actual info. Therefore, it's hard to know what the
    #max possible number of columns we should include are. I just go with 22 (max
    #number of fledglings ever recorded), although this seems excessive!!
    #I'm basing all this code (e.g. sep used) on the dead chick ids columns.
    #I assume when chick numbers are provided, it will be in the same format.
    tidyr::separate(chick_ids, into = paste0("chick_", seq(1:22)), sep = " ,") %>%
    as_tibble() %>%
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_observed = NA,
                  ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE),
                  LayDateError = NA,
                  ClutchSizeError = NA,
                  HatchDateError = NA,
                  BroodSizeError = NA,
                  FledgeDate = NA,
                  FledgeDateError = NA,
                  NumberFledgedError = NA,
                  AvgTarsus = NA,
                  NumberChicksTarsus = NA) %>%
    dplyr::select(PopID, BreedingSeason,
                  Species, Plot,
                  LocationID, BroodID,
                  FemaleID, MaleID,
                  ClutchType_observed,
                  ClutchType_calc,
                  LayDate, LayDateError,
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

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calc <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- NumberChicksMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

}

create_capture_WYT <- function(db, Brood_data){

  #Load chick capture data
  Chick_captures_old <- readxl::read_xlsx(paste0(db, "/WYT_PrimaryData_Capture.xlsx"),
                                          col_types = "text", sheet = "1947-2012",
                                          na = c("unringed", "unrrunt", "UNRRUNT")) %>%
    janitor::clean_names()

  chick_old_pb <- dplyr::progress_estimated(n = nrow(Chick_captures_old))

  Chick_captures_old <- Chick_captures_old %>%
    dplyr::mutate(Species = dplyr::case_when(toupper(species_code) == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             toupper(species_code) == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             toupper(species_code) == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                             toupper(species_code) == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code),
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(date_time)), CaptureTime = NA_character_) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(BreedingSeason = ifelse(is.na(CaptureDate), as.numeric(stringr::str_sub(pnum, start = 1, end = 4)),
                                          lubridate::year(CaptureDate))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(IndvID = ring_number, CapturePopID = "WYT",
                  CapturePlot = stringr::str_remove_all(string = pnum, pattern = "\\d"),
                  LocationID = stringr::str_sub(pnum, start = 6),
                  #Release is just NA for now because we need to find the location with cross-fostering
                  ReleasePopID = "WYT", ReleasePlot = NA_character_,
                  ObserverID = NA_character_,
                  Mass = as.numeric(weight), WingLength = as.numeric(wing_length),
                  Age_observed = dplyr::case_when(age %in% c("3", "3J") ~ 3L,
                                                  age == "0" ~ NA_integer_,
                                                  age == "1" ~ 1L,
                                                  age == "2" ~ 2L,
                                                  age == "4" ~ 4L,
                                                  age == "5" ~ 5L,
                                                  age == "6" ~ 6L)) %>%
    #Add tarsus. We are assuming that when tarsus method is "S" this is Svensson's Alternative and doesn't need conversion
    #Otherwise (NA or "M") assume it's Oxford.
    #Need to check this with Ella.
    dplyr::bind_cols(purrr::map2_df(.x = .$tarsus_length, .y = .$tarsus_length_method,
                                    .f = ~{

                                      chick_old_pb$tick()$print()

                                      if(is.na(..1)){

                                        return(tibble::tibble(Tarsus = NA_real_,
                                                              OriginalTarsusMethod = NA_character_))

                                      }

                                      if(is.na(..2) | ..2 == "M"){

                                        return(tibble::tibble(Tarsus = convert_tarsus(as.numeric(..1), method = "Oxford"),
                                                              OriginalTarsusMethod = "Oxford"))

                                      } else {

                                        return(tibble::tibble(Tarsus = as.numeric(..1),
                                                              OriginalTarsusMethod = "Oxford"))

                                      }

                                    })) %>%
    #Include sex and brood info for linked to Brood_data and Individual_data
    #Sex 'n' is assumed to be a typo and treated as male
    #As always, we ignore sex uncertainty
    dplyr::mutate(Sex = dplyr::case_when(grepl(toupper(sex), pattern = "F") ~ "F",
                                         grepl(toupper(sex), pattern = "M|N") ~ "M")) %>%
    dplyr::bind_cols(purrr::pmap_df(.l = list(.$Age_observed, .$pnum, .$origin_pnum),
                                    .f = ~{

                                      if(!..1 %in% c(1, 3)){

                                        return(tibble::tibble(BroodIDLaid = NA_character_,
                                                              BroodIDFledged = NA_character_))

                                      } else {

                                        if(is.na(..3)){

                                          return(tibble::tibble(BroodIDLaid = ..2,
                                                                BroodIDFledged = ..2))

                                        } else {

                                          return(tibble::tibble(BroodIDLaid = ..3,
                                                                BroodIDFledged = ..2))

                                        }

                                      }

                                    })) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate,
                  CaptureTime, ObserverID, LocationID, CapturePopID,
                  CapturePlot, ReleasePopID, ReleasePlot, Mass,
                  Tarsus, OriginalTarsusMethod, WingLength,
                  Age_observed, Sex, BroodIDLaid, BroodIDFledged)

  Chick_captures_new <- readxl::read_xlsx(paste0(db, "/WYT_PrimaryData_Capture.xlsx"),
                                          col_types = "text", sheet = "2013-2018",
                                          na = c("unringed", "unrrunt", "UNRRUNT")) %>%
    janitor::clean_names()

  chick_new_pb <- dplyr::progress_estimated(n = nrow(Chick_captures_new))

  Chick_captures_new <- Chick_captures_new %>%
    dplyr::mutate(Species = dplyr::case_when(toupper(bto_species_code) == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             toupper(bto_species_code) == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             toupper(bto_species_code) == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                             toupper(bto_species_code) == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             toupper(bto_species_code) == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code),
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(date_time) %/% 1),
                  CaptureTime = paste(stringr::str_pad(string = ((as.numeric(date_time) %% 1) * 24) %/% 1,
                                                       width = 2, pad = "0"),
                                      stringr::str_pad(string = round((((as.numeric(date_time) %% 1) * 24) %% 1) * 60),
                                                       width = 2, pad = "0"), sep = ":")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(BreedingSeason = ifelse(is.na(CaptureDate), as.numeric(stringr::str_sub(pnum, start = 1, end = 4)),
                                          lubridate::year(CaptureDate))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(IndvID = bto_ring, CapturePopID = "WYT",
                  CapturePlot = stringr::str_remove_all(string = pnum, pattern = "\\d"),
                  LocationID = stringr::str_sub(pnum, start = 6),
                  #Release is just NA for now because we need to find the location with cross-fostering
                  ReleasePopID = "WYT", ReleasePlot = NA_character_,
                  ObserverID = NA_character_,
                  Mass = as.numeric(weight), WingLength = as.numeric(wing_length),
                  #Currently just treat ages as is (assume they are EURING codes)
                  Age_observed = as.integer(age)) %>%
    #Add tarsus. We are assuming that when tarsus method is "S" this is Svensson's Alternative and doesn't need conversion
    #Otherwise (NA or "M") assume it's Oxford.
    #Need to check this with Ella.
    dplyr::bind_cols(purrr::map2_df(.x = .$tarsus_length, .y = .$tarsus_length_method,
                                    .f = ~{

                                      chick_new_pb$tick()$print()

                                      if(is.na(..1)){

                                        return(tibble::tibble(Tarsus = NA_real_,
                                                              OriginalTarsusMethod = NA_character_))

                                      }

                                      if(is.na(..2) | toupper(..2) == "M"){

                                        return(tibble::tibble(Tarsus = convert_tarsus(as.numeric(..1), method = "Oxford"),
                                                              OriginalTarsusMethod = "Oxford"))

                                      } else {

                                        return(tibble::tibble(Tarsus = as.numeric(..1),
                                                              OriginalTarsusMethod = "Oxford"))

                                      }

                                    })) %>%
    #Include sex and brood info for linked to Brood_data and Individual_data
    #Sex 'n' is assumed to be a typo and treated as male
    #As always, we ignore sex uncertainty
    dplyr::mutate(Sex = sex)  %>%
    dplyr::bind_cols(purrr::map2_df(.x = .$Age_observed, .y = .$pnum,
                                    .f = ~{

                                      if(!..1 %in% c(1, 3)){

                                        return(tibble::tibble(BroodIDLaid = NA_character_,
                                                              BroodIDFledged = NA_character_))

                                      } else {

                                        return(tibble::tibble(BroodIDLaid = ..2,
                                                              BroodIDFledged = ..2))

                                      }

                                    })) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate,
                  CaptureTime, ObserverID, LocationID, CapturePopID,
                  CapturePlot, ReleasePopID, ReleasePlot, Mass,
                  Tarsus, OriginalTarsusMethod, WingLength,
                  Age_observed, Sex, BroodIDLaid, BroodIDFledged)

  Capture_data <- dplyr::bind_rows(Chick_captures_new, Chick_captures_old) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::left_join(Brood_data %>%
                       select(BroodID, HatchDate), by = c("BroodIDLaid" = "BroodID")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ChickAge = ifelse(Age_observed == 1, CaptureDate - HatchDate, NA_integer_)) %>%
    dplyr::ungroup()

  return(Capture_data)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_obsv <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  FemaleID <- MaleID <- chick_1 <- chick_22 <- NULL

}

create_individual_WYT <- function(Capture_data){

  indv_pb <- dplyr::progress_estimated(n = length(unique(na.omit(Capture_data$IndvID))) * 4 + 1)

  #For every individual determine their unchanged individual information
  Individual_data <- Capture_data %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(Species))), .f = ~{

      indv_pb$tick()$print()

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }), PopID = "WYT",
    BroodIDLaid = purrr::map_chr(.x = list(unique(na.omit(BroodIDLaid))), .f = ~{

      indv_pb$tick()$print()

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }),
    BroodIDFledged = purrr::map_chr(.x = list(unique(na.omit(BroodIDFledged))), .f = ~{

      indv_pb$tick()$print()

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }),
    RingSeason = first(BreedingSeason),
    RingAge = ifelse(all(is.na(Age_calculated)), NA_character_, ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult"))),
    Sex = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{

      indv_pb$tick()$print()

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("C")

      }

    }))

  return(Individual_data)

}
