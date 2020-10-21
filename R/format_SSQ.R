#' Construct standard format for data from Santo Stefano Quisquina, Italy.
#'
#' A pipeline to produce the standard format for the great and blue tit population
#' in Santo Stefano Quisquina, Sicly, Italy, administered by Camillo Cusimano
#' and Daniela Campobello.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{BroodID}: Unique BroodID is constructed using:
#' BreedingSeason_LocationID_LayDate (April days)
#'
#' \strong{Species}: In the individual data, there are some cases where an
#' IndvID is associated with >1 species. I assume these are just typos and I
#' will just take the first species.
#'
#' \strong{CaptureDate}: No exact capture date is currently given. For adults we
#' use the laying date of the nest as a proxy for capture date. Chicks were only
#' ever captured on the nest, we used laying date + clutch size + 15 days
#' incubation + 12 days. This is because chicks were ringed at 12 days old at
#' the latest.
#'
#' \strong{Age_calculated}: All ringed chicks were assumed to be ringed at EURING code
#' 1 (i.e. pre-fledging).
#'
#' \strong{Individual_data}: There are cases where chicks from different nests are
#' given the same ring number. Unsure if this is the rings being reused or a
#' typo. Currently, I leave it as is and assume this is a typo that needs to be
#' fixed in the primary data.
#'
#' \strong{StartSeason}: Some nest boxes were replaced over the course of
#' the study; however, these replacements were not explicitly recorded.
#' Therefore, we list all nestboxes as functioning for the full study period.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export

format_SSQ <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  db <- paste0(db, "/SSQ_PrimaryData.xlsx")

  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- readxl::read_excel(db) %>%
    #Clean all names with janitor to snake_case
    janitor::clean_names(case = "upper_camel") %>%
    #Remove the column 'Row'. This is just the row number, we have this already.
    dplyr::select(-Row) %>%
    janitor::remove_empty(which = "rows") %>%
    #Change column names to match consistent naming
    dplyr::mutate(BreedingSeason = as.integer(Year), LayDate = Ld, ClutchSize = as.integer(Cs),
                  HatchDate = Hd, BroodSize = as.integer(Hs), NumberFledged = as.integer(Fs),
                  FemaleID = FId, MaleID = MId, LocationID = NestId,
                  Plot = HabitatOfRinging,
                  Latitude = YCoord, Longitude = XCoord) %>%
    #Add species codes
    dplyr::mutate(Species = dplyr::case_when(.$Species == "Parus major" ~ species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                                             .$Species == "Cyanistes caeruleus" ~ species_codes[which(species_codes$SpeciesID == 14620), ]$Species)) %>%
    #Filter species
    dplyr::filter(Species %in% species) %>%
    #Add other missing data:
    #- PopID
    #- BroodID (Year_LocationID_LayDate)
    #- ClutchType_observed
    #- FledgeDate
    #Pad LocationID so they are all the same length
    dplyr::mutate(PopID = "SSQ",
                  LocationID = stringr::str_pad(LocationID, width = 3, pad = "0"),
                  BroodID = paste(BreedingSeason, LocationID, stringr::str_pad(LayDate, width = 3, pad = "0"), sep = "_"),
                  ClutchType_observed = dplyr::case_when(.$Class == 1 ~ "first",
                                                         .$Class == 3 ~ "second",
                                                         .$Class == 2 ~ "replacement"),
                  FledgeDate = NA, AvgEggMass = NA, NumberEggs = NA, AvgChickMass = NA, NumberChicksMass = NA, AvgTarsus = NA, NumberChicksTarsus = NA,
                  LayDateError = NA, ClutchSizeError = NA, HatchDateError = NA, BroodSizeError = NA,
                  FledgeDateError = NA, NumberFledgedError = NA, ExperimentID = NA,
                  LayDate = as.Date(paste(BreedingSeason, "03-01", sep = "-"), format = "%Y-%m-%d") + LayDate - 1,
                  HatchDate = as.Date(paste(BreedingSeason, "03-01", sep = "-"), format = "%Y-%m-%d") + HatchDate - 1)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_SSQ(all_data)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_SSQ(all_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_SSQ(all_data, Capture_data, Brood_data)

  # LOCATION DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_SSQ(all_data)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_SSQ.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_SSQ.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_SSQ.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_SSQ.csv"), row.names = F)

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

#' Create brood data table for Santo Stefano Quisquina, Italy.
#'
#' Create brood data table in standard format for data from Santo Stefano
#' Quisquina, Italy
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#'
#' @return A data frame.

create_brood_SSQ <- function(data){

  #Determine ClutchType_calculated
  clutchtype <- dplyr::progress_estimated(n = nrow(data))

  Brood_data <- data %>%
    #Arrange data for use with ClutchType_calculated (should be chronological)
    dplyr::arrange(BreedingSeason, FemaleID, LayDate) %>%
    #Calculate clutch type
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE),
                  OriginalTarsusMethod = NA) %>%
    #Arrange columns to match standard protocol
    dplyr::select(BreedingSeason, Species, PopID, Plot,
                  LocationID, BroodID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate, LayDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID) %>%
    dplyr::ungroup()

  return(Brood_data)

}

#' Create capture data table for Santo Stefano Quisquina, Italy.
#'
#' Create capture data table in standard format for data from Santo Stefano
#' Quisquina, Italy
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#'
#' @return A data frame.

create_capture_SSQ <- function(data){

  Adult_captures <- data %>%
    dplyr::select(BreedingSeason, PopID, Plot, LocationID, Species, LayDate, FemaleID, FAge, MaleID, MAge) %>%
    #Combine column FemaleID and MaleID
    reshape2::melt(measure.vars = c("FemaleID", "MaleID"), value.name = "IndvID") %>%
    #Remove all NAs, we're only interested in cases where parents were ID'd.
    dplyr::filter(!is.na(IndvID)) %>%
    #Make a single Age column. If variable == "FemaleID", then use FAge and visa versa
    dplyr::rowwise() %>%
    dplyr::mutate(Age = ifelse(variable == "FemaleID", as.integer(FAge), as.integer(MAge))) %>%
    dplyr::ungroup() %>%
    #Convert these age values to current EURING codes
    #If NA, we know it's an adult but don't know it's age
    #We don't want to assume anything here
    dplyr::mutate(Age_observed = dplyr::case_when(.$Age == 1 ~ 5L,
                                                  .$Age == 2 ~ 6L)) %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #Treat CaptureDate of adults as the Laying Date
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = LayDate,
                  CaptureTime = NA) %>%
    dplyr::select(-variable, -LayDate, -FAge, -MAge)

  #Also extract chick capture information
  Chick_captures <- data %>%
    dplyr::select(BreedingSeason, Species, PopID, Plot, LocationID, LayDate, ClutchSize, Chick1Id:Chick13Id) %>%
    #Create separate rows for every chick ID
    reshape2::melt(id.vars = c("BreedingSeason", "Species", "PopID", "Plot", "LocationID", "LayDate", "ClutchSize"), value.name = "IndvID") %>%
    #Remove NAs
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #For chicks, we currently don't have the version of the individual level capture data.
    #For now, we use LayDate + ClutchSize + 15 (incubation days in SSQ) + 12.
    #Chicks were captured and weighed at 12 days old at the latest
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = LayDate + ClutchSize + 27,
                  CaptureTime = NA, Age_observed = 1, Age = 1L) %>%
    dplyr::select(-variable, -LayDate, -ClutchSize)

  #Combine Adult and chick data
  Capture_data <- dplyr::bind_rows(Adult_captures, Chick_captures) %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    #Add NA for morphometric measures and chick age
    #ChickAge (in days) is NA because we have no exact CaptureDate
    dplyr::mutate(Mass = NA, Tarsus = NA, OriginalTarsusMethod = NA,
                  WingLength = NA,
                  ChickAge = NA, ObserverID = NA) %>%
    calc_age(ID = IndvID, Age = Age, Date = CaptureDate, Year = BreedingSeason) %>%
  #Order variables to match other data
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, IndvID, Species,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot,
                  Mass, Tarsus, WingLength, Age_observed, Age_calculated, ChickAge)

  return(Capture_data)

}

#' Create individual data table for Santo Stefano Quisquina, Italy.
#'
#' Create individual data table in standard format for data from Santo Stefano
#' Quisquina, Italy
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#' @param Capture_data Data frame. Generate by \code{\link{create_capture_SSQ}}.
#' @param Brood_data Data frame. Generate by \code{\link{create_brood_SSQ}}.
#'
#' @return A data frame.

create_individual_SSQ <- function(data, Capture_data, Brood_data){

  #Create a list of all chicks
  Chick_IDs <- data %>%
    dplyr::select(BroodID, Chick1Id:Chick13Id) %>%
    reshape2::melt(id.vars = "BroodID", value.name = "IndvID") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::select(-variable, BroodIDLaid = BroodID)

  #Determine summary data for every captured individual
  Individual_data <- Capture_data %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = first(Species),
                     RingSeason = as.integer(min(lubridate::year(CaptureDate))),
                     RingAge = dplyr::case_when(is.na(first(Age_observed)) ~ "adult",
                                                first(Age_observed) == 1 ~ "chick",
                                                first(Age_observed) > 1 ~ "adult")) %>%
    dplyr::mutate(Sex = dplyr::case_when(.$IndvID %in% Brood_data$FemaleID ~ "F",
                                         .$IndvID %in% Brood_data$MaleID ~ "M",
                                         .$IndvID %in% Brood_data$MaleID & .$IndvID %in% Brood_data$FemaleID ~ "C")) %>%
    #Join in BroodID from the reshaped Chick_IDs table
    dplyr::left_join(Chick_IDs, by = "IndvID") %>%
    dplyr::mutate(BroodIDFledged = BroodIDLaid,
                  PopID = "SSQ") %>%
    select(IndvID, Species, PopID, BroodIDLaid,
           BroodIDFledged, RingSeason, RingAge, Sex)

}

#' Create location data table for Santo Stefano Quisquina, Italy.
#'
#' Create location data table in standard format for data from Santo Stefano
#' Quisquina, Italy
#' @param data Data frame. Primary data from Santo Stefano Quisquina.
#'
#' @return A data frame.

create_location_SSQ <- function(data){

  Location_data <- data %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(LocationType = "NB",
                     PopID = "SSQ",
                     StartSeason = 1993L, EndSeason = NA_integer_) %>%
    dplyr::mutate(NestboxID = LocationID) %>%
    #Join in first latitude and longitude data recorded for this box.
    #It's not clear why these are ever different, need to ask.
    dplyr::left_join(data %>% group_by(LocationID) %>% slice(1) %>% select(LocationID, Latitude, Longitude), by = "LocationID") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason)

}
