#'Construct standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Pilis-Visegrád Mountains, Hungary, administered by the János Török (Eötvös Loránd University).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{LocationID}: Nestbox numbers are not unique across plots. Therefore, unique locationIDs are
#'a combination of plot and nestbox number.
#'
#'\strong{BroodID}: Unique broods are Year, LocationID, LayingDate (days since Mar 31st).
#'
#'\strong{ExperimentID}: Experiments are listed as affecting number fledglings, clutch size etc.
#' so all experiments are classed as 'COHORT'. ExperimentID '2' is also classified as 'PHENOLOGY'
#' as it is said to affect laying date.
#'
#'\strong{CaptureDate}: Nestlings have a ring date and a nestling measure date column.
#'When there is only a ring date, the nestlings are captured once (ringing and measurement).
#'When there is a ring and nestling measure date these are considered two captures.
#'The ring date is assumed to have no measurements (Mass, Tarsus, WingLength are NA).
#'
#'\strong{Species}: There are a (small) number of hybrid flycatcher broods, with
#'male FICHYP and female FICALB. This causes some problems for our classification of
#'brood and chick species. For broods, we know these are legitimate multi-species
#'broods, so they should not be flagged as warnings. For chicks, we don't have a species
#'code for flycatcher hybrids. Currently, all hybrid broods and chicks are removed
#'(there are only 5 broods and 16 chicks). Species is still included for
#'adult captures as we know the exact species of each parent. We will need to determine
#'how we want to deal with these hybrid broods in the future.
#'
#'\strong{Age_observed}: Age is not explicitly recorded, therefore we classify all
#'adults as NA age. Individuals in nestling columns are assumed to always be pre-fledging
#'and are given age 1.
#'
#'\strong{Location_data}: All unique locationIDs (plot/nestbox number) are
#'assumed to be active across the whole study period.
#'
#'\strong{Mass, Tarsus & WingLength}: There are some character strings in these columns.
#'These variables are silently coerced to numeric such that all character strings
#'are coerced to NA.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_PIL <- function(db = choose_directory(),
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

  #Load data
  #As always, we read in as text to prevent coercion issues
  #We convert the data later
  PIL_data <- readxl::read_excel(path = paste0(db, "/PIL_PrimaryData.xlsx"), col_types = "text", na = "NA") %>%
    janitor::clean_names() %>%
    #Convert all date columns
    #Some are march days some are actual dates (but different formats)
    dplyr::mutate(mar_31 = as.Date(paste(year, 3, 31, sep = "-")),
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(plot, nestbox, sep = "_"),
                  #BroodID requires year, plot, nestbox, laying_date
                  #This accounts for multiple clutches in a year
                  BroodID = paste(year, plot, nestbox, laying_date, sep = "_")) %>%
    #Convert march days
    dplyr::mutate_at(.vars = vars(laying_date, hdate), .funs = ~{mar_31 + as.numeric(..1)}) %>%
    #Convert regular dates
    dplyr::rowwise() %>%
    dplyr::mutate_at(.vars = vars(female_date, male_date, ring_date, nestling_measure_date), .funs = ~{

      if(is.na(..1)){

        as.Date(NA)

      } else {

        if(grepl(pattern = "\\.", ..1)){

          as.Date(..1, format = "%Y.%m.%d")

        } else {

          janitor::excel_numeric_to_date(as.numeric(..1))

        }

      }

    }) %>%
    dplyr::ungroup()

  # BROOD DATA

  message("\n Compiling brood data....")

  Brood_data <- create_brood_PIL(PIL_data = PIL_data, species_filter = species)

  # CAPTURE DATA

  message("\n Compiling capture data....")

  Capture_data <- create_capture_PIL(PIL_data = PIL_data, species_filter = species)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_PIL(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_PIL(PIL_data = PIL_data)

  # WRANGLE DATA FOR EXPORT

  # Add ChickAge to capture data
  # Add hatchdate from brood data
  Capture_data <- Capture_data %>%
    dplyr::left_join(dplyr::select(Brood_data, BroodID, HatchDate), by = "BroodID") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ChickAge = ifelse(is.na(Age_observed), NA_integer_, CaptureDate - HatchDate)) %>%
    dplyr::ungroup()

  #Determine avg measures for each brood
  avg_mass <- Capture_data %>%
    dplyr::filter(between(ChickAge, 14, 16) & !is.na(Mass)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(Mass),
                     NumberChicksMass = n())

  avg_tarsus <- Capture_data %>%
    dplyr::filter(between(ChickAge, 14, 16) & !is.na(Tarsus)) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgTarsus = mean(Tarsus),
                     NumberChicksTarsus = n(),
                     OriginalTarsusMethod = "Alternative")

  #Add into Brood_data
  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_mass, by = "BroodID") %>%
    dplyr::left_join(avg_tarsus, by = "BroodID")

  #Remove unneccesary cols in Brood and Capture data
  Brood_data <- Brood_data %>%
    dplyr::select(BroodID, PopID, BreedingSeason,
                  Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed,
                  ClutchType_calculated,
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
    dplyr::select(IndvID, Species,
                  BreedingSeason, CaptureDate,
                  CaptureTime, ObserverID,
                  LocationID, CapturePopID,
                  CapturePlot, ReleasePopID,
                  ReleasePlot, Mass, Tarsus,
                  OriginalTarsusMethod,
                  WingLength, Age_observed,
                  Age_calculated,
                  ChickAge)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PIL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PIL.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_PIL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PIL.csv"), row.names = F)

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

#' Create brood data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create brood data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_brood_PIL <- function(PIL_data, species_filter){

  Brood_data <- PIL_data %>%
    dplyr::mutate(PopID = "PIL",
                  BreedingSeason = as.integer(year),
                  Plot = plot,
                  #They record hybrid broods in the species info
                  #They give this a unique species ID.
                  #Currently, we're just including the major species with > 100 broods
                  #(CYACAE, FICALB, PARMAJ, SITEUR)
                  #Note, we still use our Species_codes table even though they use the same 6 letter codes
                  #This is because it will be easier to change if e.g. species are renamed
                  #because we can just update the Species_codes table once.
                  Species = dplyr::case_when(species == "CYACAE" ~ Species_codes$Code[Species_codes$SpeciesID == 14620],
                                             species == "PARMAJ" ~ Species_codes$Code[Species_codes$SpeciesID == 14640],
                                             species == "FICALB" ~ Species_codes$Code[Species_codes$SpeciesID == 13480],
                                             species == "SITEUR" ~ Species_codes$Code[Species_codes$SpeciesID == 14790]),
                  ExperimentID = dplyr::case_when(exp == "0" ~ NA_character_,
                                                  exp == "1" ~ "COHORT",
                                                  exp == "2" ~ "COHORT;PHENOLOGY",
                                                  exp == "3" ~ "COHORT"),
                  LayDate = laying_date, ClutchSize = as.integer(clutch_size),
                  BroodSize = as.integer(number_hatchlings),
                  NumberFledged = as.integer(number_fledglings), HatchDate = hdate,
                  FemaleID = femalering, MaleID = malering,
                  ClutchType_observed = NA_character_, ClutchSizeError = NA_real_,
                  LayDateError = NA_real_, HatchDateError = NA_real_,
                  BroodSizeError = NA_real_,
                  FledgeDate = NA_real_, FledgeDateError = NA_real_,
                  NumberFledgedError = NA_real_, AvgEggMass = NA_real_,
                  NumberEggs = NA_integer_) %>%
    dplyr::filter(Species %in% species_filter) %>%
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE))

  return(Brood_data)

}

#' Create capture data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create capture data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame.

create_capture_PIL <- function(PIL_data, species_filter){

  female_capture_data <- PIL_data %>%
    dplyr::select(year:species, femalering:f_mass) %>%
    dplyr::filter(!is.na(femalering)) %>%
    dplyr::mutate(CapturePopID = "PIL", ReleasePopID = "PIL",
                  BreedingSeason = as.integer(year),
                  CapturePlot = plot, ReleasePlot = plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(plot, nestbox, sep = "_"),
                  IndvID = toupper(femalering), CaptureDate = female_date,
                  Age_observed = NA_integer_,
                  ObserverID = f_measure_person,
                  Tarsus = suppressWarnings(as.numeric(f_tarsus))/10,
                  WingLength = suppressWarnings(as.numeric(f_wing)),
                  Mass = suppressWarnings(as.numeric(f_mass))/10, Sex = "F",
                  Species = dplyr::case_when(species == "CYACAE" ~ Species_codes$Code[Species_codes$SpeciesID == 14620],
                                             species == "PARMAJ" ~ Species_codes$Code[Species_codes$SpeciesID == 14640],
                                             species == "FICALB" ~ Species_codes$Code[Species_codes$SpeciesID == 13480],
                                             species == "SITEUR" ~ Species_codes$Code[Species_codes$SpeciesID == 14790],
                                             species == "FICHIB" ~ Species_codes$Code[Species_codes$SpeciesID == 13480]))

  male_capture_data <- PIL_data %>%
    dplyr::select(year:species,
                  malering:m_mass) %>%
    dplyr::filter(!is.na(malering)) %>%
    dplyr::mutate(CapturePopID = "PIL", ReleasePopID = "PIL",
                  BreedingSeason = as.integer(year),
                  CapturePlot = plot, ReleasePlot = plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(plot, nestbox, sep = "_"),
                  IndvID = toupper(malering), CaptureDate = male_date,
                  Age_observed = NA_integer_,
                  ObserverID = m_measure_person,
                  Tarsus = suppressWarnings(as.numeric(m_tarsus))/10,
                  WingLength = suppressWarnings(as.numeric(m_wing)),
                  Mass = suppressWarnings(as.numeric(m_mass))/10, Sex = "M",
                  Species = dplyr::case_when(species == "CYACAE" ~ Species_codes$Code[Species_codes$SpeciesID == 14620],
                                             species == "PARMAJ" ~ Species_codes$Code[Species_codes$SpeciesID == 14640],
                                             species == "FICALB" ~ Species_codes$Code[Species_codes$SpeciesID == 13480],
                                             species == "SITEUR" ~ Species_codes$Code[Species_codes$SpeciesID == 14790],
                                             species == "FICHIB" ~ Species_codes$Code[Species_codes$SpeciesID == 13490]))

  chick_capture_data <- PIL_data %>%
    #Remove unwanted species
    dplyr::mutate(Species = dplyr::case_when(species == "CYACAE" ~ Species_codes$Code[Species_codes$SpeciesID == 14620],
                                             species == "PARMAJ" ~ Species_codes$Code[Species_codes$SpeciesID == 14640],
                                             species == "FICALB" ~ Species_codes$Code[Species_codes$SpeciesID == 13480],
                                             species == "SITEUR" ~ Species_codes$Code[Species_codes$SpeciesID == 14790])) %>%
    dplyr::filter(Species %in% species_filter) %>%
    #Remove cases where no chicks were ever ringed
    dplyr::filter_at(.vars = vars(contains("nestling_ring")), .vars_predicate = any_vars(!is.na(.))) %>%
    dplyr::select(BroodID, year:species, ring_date:tarsus_15) %>%
    #we need to give two records to chicks that were ringed and measured at different times
    tidyr::pivot_longer(cols = c(ring_date, nestling_measure_date), names_to = "date_type", values_to = "CaptureDate") %>%
    #If measure date is NA, then there's only one capture and we can remove it
    dplyr::filter(date_type == "ring_date" | (date_type == "nestling_measure_date" & !is.na(CaptureDate))) %>%
    #Identify every case where there were two captures
    dplyr::group_by(BroodID) %>%
    dplyr::mutate(n = n()) %>%
    tidyr::pivot_longer(cols = nestling_ring_1:nestling_ring_15, names_to = "ChickNr", values_to = "IndvID") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    tidyr::pivot_longer(cols = mass_1:mass_15, names_to = "ChickNr_Mass", values_to = "Mass") %>%
    tidyr::pivot_longer(cols = tarsus_1:tarsus_15, names_to = "ChickNr_Tarsus", values_to = "Tarsus") %>%
    #Convert to just have the number of the chick
    dplyr::mutate_at(.vars = vars(contains("ChickNr")), .funs = ~{stringr::str_remove_all(..1, pattern = "[^0-9]")}) %>%
    dplyr::filter(ChickNr == ChickNr_Mass & ChickNr == ChickNr_Tarsus) %>%
    #When there is a ring and measure date, make tarsus and mass NA at the ring date
    dplyr::rowwise() %>%
    dplyr::mutate_at(.vars = vars(Mass, Tarsus), .funs = ~ ifelse(n == 2 & date_type == "ring_date", NA, .)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CapturePopID = "PIL", ReleasePopID = "PIL",
                  BreedingSeason = as.integer(year),
                  CapturePlot = plot, ReleasePlot = plot,
                  #Nestbox numbers are not unique across plots
                  LocationID = paste(plot, nestbox, sep = "_"),
                  Age_observed = 1L,
                  ObserverID = nestling_measure_person,
                  Tarsus = suppressWarnings(as.numeric(Tarsus))/10,
                  Mass = suppressWarnings(as.numeric(Mass))/10, Sex = NA_character_,
                  IndvID = toupper(IndvID))

  #Combine data
  Capture_data <- dplyr::bind_rows(female_capture_data, male_capture_data, chick_capture_data) %>%
    dplyr::mutate(CaptureTime = NA_character_, WingLength = NA_real_, OriginalTarsusMethod = NA_character_) %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason)

  return(Capture_data)

}

#' Create individual table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create full individual data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param Capture_data Output of \code{\link{create_capture_PIL}}.
#'
#' @return A data frame.

create_individual_PIL <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CONFLICTED")

      }

    }), PopID = "PIL",
    BroodIDLaid = first(BroodID), BroodIDFledged = BroodIDLaid,
    RingSeason = first(BreedingSeason), RingAge = ifelse(all(is.na(Age_observed)), "adult", "chick"),
    Sex = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("C")

      }

    })) %>%
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

}

#' Create location data table for Pilis-Visegrád Mountains, Hungary.
#'
#' Create location data table in standard format for data from Pilis-Visegrád Mountains, Hungary.
#'
#' @param PIL_data Data frame with primary data from Pilis-Visegrád Mountains
#'
#' @return A data frame.

create_location_PIL <- function(PIL_data){

  Location_data <- tibble::tibble(LocationID = unique(PIL_data$LocationID),
                                  NestboxID = unique(PIL_data$LocationID),
                                  LocationType = "NB",
                                  PopID = "PIL",
                                  Latitude = NA_real_, Longitude = NA_real_,
                                  StartSeason = min(as.integer(PIL_data$year)),
                                  EndSeason = NA_integer_, Habitat = "deciduous")

  return(Location_data)

}
