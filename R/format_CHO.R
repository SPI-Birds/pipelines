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
    dplyr::filter(Species %in% species) %>%
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
           AvgEggMass = .data$MeanEggWeight, NumberEggs = .data$NoEggsWeighted)

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

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CHO.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CHO.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CHO.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CHO.csv"), row.names = F)

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
    dplyr::select(.data$BroodID, .data$IndvID, .data$Sex) %>%
    # "No ringed/no ring" becomes NA
    dplyr::mutate(IndvID = purrr::map_chr(.x = .data$IndvID,
                                          .f = ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    #Reshape data so that we have a MaleID and FemaleID column
    #Rather than an individual row for each parent capture
    tidyr::pivot_longer(cols = c(.data$IndvID)) %>%
    tidyr::pivot_wider(names_from = .data$Sex, values_from = .data$value) %>%
    dplyr::rename(FemaleID = `F`, MaleID = `M`) %>%
    select(-.data$name) %>%
    arrange(.data$BroodID)

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
    dplyr::filter(.data$Age == "C" & !is.na(.data$ChickAge) & between(.data$ChickAge, 14, 16)) %>%
    #For every brood, determine the average mass and tarsus length
    dplyr::group_by(.data$BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(.data$Weight, na.rm = T),
                     NumberChicksMass = length(stats::na.omit(.data$Weight)),
                     AvgTarsus = mean(.data$Tarsus, na.rm = T),
                     NumberChicksTarsus = length(stats::na.omit(.data$Tarsus))) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.data$AvgTarsus) ~ "Alternative"))

  Brood_data <- data %>%
    #Join in information on parents and clutch type
    dplyr::left_join(Parents, by = "BroodID") %>%
    dplyr::left_join(Brood_info, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    dplyr::select(-.data$CodeLine:-.data$Ring, -.data$JulianDate:-.data$StanderdisedTime, -.data$TrapingMethod,
                  -.data$BroodId:-.data$Smear, -.data$TotalEggWeight, -.data$IndvID) %>%
    #Turn all remaining columns to characters
    #melt/cast requires all values to be of the same type
    dplyr::mutate_all(as.character) %>%
    #Melt and cast data so that we return the first value of relevant data for each brood
    #e.g. laying date, clutch size etc.
    #I've checked manually and the first value is always correct in each brood
    dplyr::group_by(.data$BroodID, .data$Species, .data$Year, .data$Site, .data$Box, .data$FemaleID, .data$MaleID) %>%
    dplyr::summarise(across(.cols = everything(), .fns = first), .groups = "drop") %>%
    #Add in population/plot info
    #Convert LayDate and HatchDate to date objects
    dplyr::mutate(LayDate_observed = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$LayingDateJulian),
                  LayDate_min = .data$LayDate_observed,
                  LayDate_max = .data$LayDate_observed,
                  ClutchSize_observed = .data$FinalClutchSize,
                  ClutchSize_min = .data$ClutchSize_observed,
                  ClutchSize_max = .data$ClutchSize_observed,
                  HatchDate_observed = lubridate::ymd(paste0(.data$Year, "-01-01")) + as.numeric(.data$HatchingDateJulian),
                  HatchDate_min = .data$HatchDate_observed,
                  HatchDate_max = .data$HatchDate_observed,
                  BroodSize_observed = .data$NoChicksHatched,
                  BroodSize_min = .data$BroodSize_observed,
                  BroodSize_max = .data$BroodSize_observed,
                  FledgeDate_observed = as.Date(NA),
                  FledgeDate_min = .data$FledgeDate_observed,
                  FledgeDate_max = .data$FledgeDate_observed,
                  NumberFledged_observed = .data$NoChicksOlder14D,
                  NumberFledged_min = .data$NumberFledged_observed,
                  NumberFledged_max = .data$NumberFledged_observed) %>%
    #Arrange data chronologically for each female for clutchtype calculation
    dplyr::arrange(.data$Year, .data$FemaleID, .data$LayDate_observed) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #Select relevant columns and rename
    dplyr::select(.data$BroodID, .data$PopID,
                  .data$BreedingSeason, .data$Species,
                  .data$Plot, .data$LocationID,
                  .data$FemaleID, .data$MaleID,
                  .data$ClutchType_observed,
                  .data$ClutchType_calculated,
                  .data$LayDate_observed, .data$LayDate_min, .data$LayDate_max,
                  .data$ClutchSize_observed, .data$ClutchSize_min, .data$ClutchSize_max,
                  .data$HatchDate_observed, .data$HatchDate_min, .data$HatchDate_max,
                  .data$BroodSize_observed, .data$BroodSize_min, .data$BroodSize_max,
                  .data$FledgeDate_observed, .data$FledgeDate_min, .data$FledgeDate_max,
                  .data$NumberFledged_observed, .data$NumberFledged_min, .data$NumberFledged_max,
                  .data$AvgEggMass, .data$NumberEggs) %>%
    #Join in average chick measurements
    dplyr::left_join(avg_measure, by = "BroodID") %>%
    #Convert everything back to the right format after making everything character
    #for the reshape
    dplyr::mutate(ExperimentID = NA_character_,
                  AvgEggMass = as.numeric(.data$AvgEggMass)) %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$BreedingSeason,
                                         .data$ClutchSize_observed:.data$ClutchSize_max,
                                         .data$BroodSize_observed:.data$BroodSize_max,
                                         .data$NumberFledged_observed:.data$NumberFledged_max,
                                         .data$NumberEggs), as.integer) %>%
    dplyr::as_tibble()

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
    dplyr::select(.data$IndvID, .data$Species,
                  .data$Sex_observed, .data$BreedingSeason,
                  .data$CaptureDate, .data$CaptureTime,
                  .data$ObserverID, .data$LocationID,
                  .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot,
                  .data$ReleasePopID, .data$ReleasePlot,
                  Mass = .data$Weight, .data$Tarsus, .data$OriginalTarsusMethod,
                  WingLength = .data$Wing,
                  .data$Age_observed, .data$Age_calculated,
                  .data$ChickAge, .data$ExperimentID) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, 1:n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$CaptureID, everything())

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
    dplyr::select(.data$IndvID, .data$Sex_calculated)

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
      BroodIDLaid = purrr::pmap_chr(.l = list(FirstBrood = .data$FirstBrood, FirstAge = .data$FirstAge),
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
    dplyr::select(.data$IndvID, .data$Species, .data$PopID, .data$BroodIDLaid, .data$BroodIDFledged,
                  RingSeason = .data$FirstYr, .data$RingAge, .data$Sex_calculated) %>%
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


#----------------------
#FIXME What about individual IDs "branco", "no ring" and "not ringed"?
