#'Construct standard format for data from Choupal, Portugal.
#'
#'A pipeline to produce the standard format for the great tit population in
#'Choupal, Portugal, administered by the University of Coimbra.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{NumberFledged_observed}: This population has no estimation of actual fledgling
#'numbers. The last time nests are counted is 14 days post hatching. We use this
#'as an estimation of fledgling numbers. This also affects
#'\emph{ClutchType_calculated} as we use the estimate of fledgling numbers to
#'distinguish second/replacement clutches.
#'
#'\strong{Age_observed}: Translation of age records: \itemize{ \item Any
#'individual caught as a chick was assumed to have a EURING code of 1: 'Pullus:
#'nestling or chick, unable to fly freely, still able to be caught by hand.'
#'\item Any individual listed as 'first year' was given a EURING code of 5: a
#'bird hatched last calendar year and now in its second calendar year. \item Any
#'individual listed as 'adult' was given a EURING code of 6: full-grown bird
#'hatched before last calendar year; year of hatching otherwise unknown.}
#'
#'\strong{ClutchType_observed}: In the raw data, there is no distinction between
#''second' and 'replacement' clutches. Any clutch recorded as '2nd' is assumed
#'to be a 'second' clutch under our definitions. 'ClutchType_calculated' may
#'give a more appropriate estimate of clutch type for this data.
#'
#'\strong{ClutchSize_observed, BroodSize_observed, NumberFledged_observed}: We currently only use records
#'of clutch, brood, and fledgling numbers that are recorded explicitly in the
#'data. This means that there are some nests where chicks have capture records,
#'but the \emph{Brood data} table will not give any value of NumberFledged_observed.
#'(e.g. see BroodID 2004_NA). These capture records should be included, but we
#'need to determine the amount of potential uncertainty around these records.
#'
#'\strong{LocationID}: For individuals captures in mist nets (specified by
#'trapping method column), a single LocationID "MN1" is used.
#'
#'\strong{StartSeason}: Assume all boxes were placed in the first year of the study.
#'
#'\strong{CaptureAlive, ReleaseAlive}: Assume all individuals were alive when captured and released.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export
#'@importFrom magrittr `%T>%`

format_CHO <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force choose_directory() if used
  force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- readxl::read_excel(paste(db, "CHO_PrimaryData.xlsx", sep = "/")) %>%
    #Clean all names with janitor into snake_case
    janitor::clean_names(case = "upper_camel") %T>%
    #There is one column with "º" that doesn't convert to ASCII with janitor
    #This appears the be a deeper issue than janitor (potentially in unicode translation)
    #Therefore, we will do the translation manually
    {colnames(.) <- iconv(colnames(.), "", "ASCII", sub = "")} %>%
    #Change species to "PARMAJ" because it's only PARMAJ in Choupal
    dplyr::mutate(Species = "PARMAJ",
                  PopID = "CHO",
                  Plot = NA_character_) %>%
    dplyr::filter(.data$Species %in% species) %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year using Year_BroodID
    dplyr::mutate(BroodID = paste(.data$Year, stringr::str_pad(.data$BroodId, width = 3, pad = "0"), sep = "_"),
                  IndvID = .data$Ring,
                  CaptureDate = lubridate::ymd(paste0(.data$Year, "-01-01")) + .data$JulianDate,
                  CaptureTime = format.POSIXct(.data$Time, format = "%H:%M:%S"),
                  ChickAge = as.integer(dplyr::na_if(.data$ChickAge, "na")),
                  BreedingSeason = as.integer(.data$Year),
                  #If an individual was caught in a mist net give a generic LocationID (MN1)
                  #Otherwise, give the box number.
                  LocationID = purrr::pmap_chr(.l = list(.data$TrapingMethod, as.character(.data$Box)),
                                               .f = ~{

                                                 if(..1 == "mist net"){

                                                   return("MN1")

                                                 } else {

                                                   return(stringr::str_pad(..2, width = 3, pad = "0"))

                                                 }

                                               }),
                  AvgEggMass = .data$MeanEggWeight,
                  NumberEggs = .data$NoEggsWeighted)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_CHO(all_data)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_CHO(all_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_CHO(all_data, Capture_data = Capture_data)

  # NESTBOX DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_CHO(all_data)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CHO.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CHO.csv"), row.names = FALSE)

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

#' Create brood data table for Choupal, Portugal.
#'
#' Create brood data table in standard format for data from Choupal,
#' Portugal.
#' @param data Data frame. Primary data from Choupal.
#'
#' @return A data frame.

create_brood_CHO <- function(data){

  #The data is currently stored as capture data (i.e. each row is a capture)
  #This means there are multiple records for each brood, which we don't want.

  #Remove all mist-net captures
  data <- data %>%
    dplyr::filter(.data$TrapingMethod != "mist net")

  #Identify any adults caught on the brood
  #Reshape data so that MaleID and FemaleID are separate columns for each brood
  Parents <- data %>%
    #Remove all records with chicks
    dplyr::filter(.data$Age != "C") %>%
    #Select only the Brood, Individual Id and their sex
    dplyr::select("BroodID", "IndvID", "Sex") %>%
    # "No ringed/no ring" becomes NA
    dplyr::mutate(dplyr::across("IndvID",
                                ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    #Reshape data so that we have a MaleID and FemaleID column
    #Rather than an individual row for each parent capture
    tidyr::pivot_longer(cols = c("IndvID")) %>%
    tidyr::pivot_wider(names_from = "Sex", values_from = "value") %>%
    dplyr::rename("FemaleID" = "F", "MaleID" = "M") %>%
    dplyr::select(-"name") %>%
    dplyr::arrange(.data$BroodID)

  #Determine whether clutches are 2nd clutch
  #Determine if there is mean egg mass data
  Brood_info <- data %>%
    #For each brood, if a clutch is listed as '2nd' make it 'second' otherwise
    #'first'. For ClutchType_observed we are not giving broods the value
    #'replacement' as we don't have enough info.
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(ClutchType_observed = ifelse("2nd" %in% .data$SecondClutch, "second", "first"))

  #Finally, we add in average mass and tarsus measured for all chicks at 14 - 16d
  #Subset only those chicks that were 14 - 16 days when captured.
  avg_measure <- data %>%
    dplyr::filter(.data$Age == "C" & !is.na(.data$ChickAge) & dplyr::between(.data$ChickAge, 14, 16)) %>%
    #For every brood, determine the average mass and tarsus length
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Weight, na.rm = TRUE),
                     NumberChicksMass = length(stats::na.omit(.data$Weight)),
                     AvgTarsus = mean(.data$Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(.data$Tarsus))) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Alternative"))

  Brood_data <- data %>%
    #Join in information on parents and clutch type
    dplyr::left_join(Parents, by = "BroodID") %>%
    dplyr::left_join(Brood_info, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    dplyr::select(-"CodeLine":-"Ring", -"JulianDate":-"StanderdisedTime", -"TrapingMethod",
                  -"BroodId":-"Smear", -"TotalEggWeight", -"IndvID") %>%
    #Turn all remaining columns to characters
    #melt/cast requires all values to be of the same type
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    #Melt and cast data so that we return the first value of relevant data for each brood
    #e.g. laying date, clutch size etc.
    #I've checked manually and the first value is always correct in each brood
    dplyr::group_by(.data$BroodID, .data$Species, .data$Year, .data$Site, .data$Box, .data$FemaleID, .data$MaleID) %>%
    dplyr::summarise(dplyr::across(.cols = tidyselect::everything(),
                                   .fns = ~dplyr::first(.)), .groups = "drop") %>%
    #Add in population/plot info
    #Convert LayDate and HatchDate to date objects
    dplyr::mutate(LayDate_observed = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$LayingDateJulian),
                  LayDate_min = as.Date(NA),
                  LayDate_max = as.Date(NA),
                  ClutchSize_observed = .data$FinalClutchSize,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$HatchingDateJulian),
                  HatchDate_min = as.Date(NA),
                  HatchDate_max = as.Date(NA),
                  BroodSize_observed = .data$NoChicksHatched,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = as.Date(NA),
                  FledgeDate_max = as.Date(NA),
                  NumberFledged_observed = .data$NoChicksOlder14D,
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_) %>%
    #Arrange data chronologically for each female for clutchtype calculation
    dplyr::arrange(.data$Year, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1")) %>%
    #Select relevant columns and rename
    dplyr::select("BroodID", "PopID",
                  "BreedingSeason", "Species",
                  "Plot", "LocationID",
                  "FemaleID", "MaleID",
                  "ClutchType_observed",
                  "ClutchType_calculated",
                  "LayDate_observed", "LayDate_min", "LayDate_max",
                  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
                  "HatchDate_observed", "HatchDate_min", "HatchDate_max",
                  "BroodSize_observed", "BroodSize_min", "BroodSize_max",
                  "FledgeDate_observed", "FledgeDate_min", "FledgeDate_max",
                  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
                  "AvgEggMass", "NumberEggs") %>%
    #Join in average chick measurements
    dplyr::left_join(avg_measure, by = "BroodID") %>%
    #Convert everything back to the right format after making everything character
    #for the reshape
    dplyr::mutate(ExperimentID = NA_character_,
                  AvgEggMass = as.numeric(.data$AvgEggMass)) %>%
    dplyr::mutate(dplyr::across(c("BreedingSeason",
                                  "ClutchSize_observed":"ClutchSize_max",
                                  "BroodSize_observed":"BroodSize_max",
                                  "NumberFledged_observed":"NumberFledged_max",
                                  "NumberEggs"), as.integer))

  return(Brood_data)

}

#' Create capture data table for Choupal, Portugal.
#'
#' Create capture data table in standard format for data from Choupal,
#' Portugal.
#' @param data Data frame. Primary data from Choupal.
#'
#' @return A data frame.

create_capture_CHO <- function(data){

  #Take all data and add population/plot info
  #There is only one population/plot
  Capture_data <- data %>%
    dplyr::mutate(CapturePopID = .data$PopID, ReleasePopID = .data$PopID,
                  CapturePlot = .data$Plot, ReleasePlot = .data$Plot) %>%
    #Arrange chronologically for each individual
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(YoungestCatch = dplyr::first(.data$Age), FirstYr = dplyr::first(.data$BreedingSeason)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age_observed = dplyr::case_when(is.na(.data$Age) ~ NA_integer_,
                                                  .data$Age == "C" ~ 1L,
                                                  .data$YoungestCatch == "C" & .data$BreedingSeason == .data$FirstYr ~ 3L,
                                                  .data$Age == "first year" ~ 5L,
                                                  .data$Age == "adult" ~ 4L)) %>%
    calc_age(ID = .data$IndvID, Age = .data$Age_observed, Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%
    #Arrange data for each individual chronologically
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    #Replace 'na' with NA in Sex
    dplyr::mutate(Sex_observed = dplyr::na_if(x = .data$Sex, y = "na"),
                  ObserverID = NA_character_,
                  OriginalTarsusMethod = "Alternative",
                  # We have no information on status of captures/releases, so we assume all individuals were captured/released alive
                  CaptureAlive = TRUE,
                  ReleaseAlive = TRUE,
                  ExperimentID = NA_character_) %>%
    #Select out only those columns we need
    dplyr::select("IndvID", "Species",
                  "Sex_observed", "BreedingSeason",
                  "CaptureDate", "CaptureTime",
                  "ObserverID", "LocationID",
                  "CaptureAlive", "ReleaseAlive",
                  "CapturePopID", "CapturePlot",
                  "ReleasePopID", "ReleasePlot",
                  "Mass" = "Weight", "Tarsus", "OriginalTarsusMethod",
                  "WingLength" = "Wing",
                  "Age_observed", "Age_calculated",
                  "ChickAge", "ExperimentID") %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select("CaptureID", tidyselect::everything())

  return(Capture_data)

}

#' Create individual data table for Choupal, Portugal.
#'
#' Create individual data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#' @param Capture_data Data frame. Output from \code{\link{create_capture_CHO}}.
#'
#' @return A data frame.

create_individual_CHO <- function(data, Capture_data){

  # Calculate sex from observed sex in Capture data
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

  #Determine first age, brood, and ring year of each individual
  Individual_data <- data %>%
    #Arrange data for each individual chronologically
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%
    #For every individual
    dplyr::group_by(.data$PopID, .data$IndvID) %>%
    #Determine the first recorded broodID, year and age.
    #Determine if there were any records where sex was identified.
    dplyr::summarise(FirstBrood = dplyr::first(.data$BroodID),
                     FirstYr = as.integer(dplyr::first(.data$Year)),
                     FirstAge = dplyr::first(.data$Age),
                     Species = "PARMAJ") %>%
    dplyr::mutate(#Only assign a brood ID if they were first caught as a chick
      #Otherwise, the broodID will be their first clutch as a parent
      BroodIDLaid = purrr::pmap_chr(.l = list(FirstBrood = .data$FirstBrood,
                                              FirstAge = .data$FirstAge),
                                    function(FirstBrood, FirstAge){

                                      if(is.na(FirstAge) || FirstAge != "C"){

                                        return(NA)

                                      } else {

                                        return(FirstBrood)

                                      }

                                    }),
      #We have no information on cross-fostering, so we assume the brood laid and ringed are the same
      BroodIDFledged = .data$BroodIDLaid,
      #Determine age at ringing as either chick or adult.
      RingAge = dplyr::case_when(.data$FirstAge == "C" ~ "chick",
                                 is.na(.data$FirstAge) ~ "adult",
                                 .data$FirstAge != "C" ~ "adult")) %>%
    dplyr::left_join(Sex_calc, by = "IndvID") %>%
    dplyr::select("IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged",
                  "RingSeason" = "FirstYr", "RingAge", "Sex_calculated") %>%
    dplyr::mutate(Sex_genetic = NA_character_) %>%
    dplyr::ungroup()

  return(Individual_data)

}

#' Create location data table for Choupal, Portugal.
#'
#' Create location data table in standard format for data from Choupal,
#' Portugal.
#' @param data Data frame. Primary data from Choupal.
#'
#' @return A data frame.

create_location_CHO <- function(data){

  #There are no coordinates or box type information
  Location_data <- dplyr::tibble(LocationID = stats::na.omit(unique(data$LocationID)),
                                 NestboxID = stats::na.omit(unique(data$LocationID))) %>%
    dplyr::mutate(LocationType = dplyr::case_when(.data$LocationID == "MN1" ~ "MN",
                                                  .data$LocationID != "MN1" ~ "NB"),
                  PopID = "CHO",
                  Latitude = NA_real_, Longitude = NA_real_,
                  StartSeason = 2003L, EndSeason = NA_integer_,
                  HabitatType = "Deciduous")

  return(Location_data)

}


#----------------------#
#FIXME What about individual IDs "branco", "no ring" and "not ringed"?
