#'Construct standard format for data from Choupal, Portugal.
#'
#'A pipeline to produce the standard format for the great tit population in
#'Choupal, Portugal, administered by the University of Coimbra.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{NumberFledged}: This population has no estimation of actual fledgling
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
#'\strong{ClutchSize, BroodSize, NumberFledged}: We currently only use records
#'of clutch, brood, and fledgling numbers that are recorded explicitly in the
#'data. This means that there are some nests where chicks have capture records,
#'but the \emph{Brood data} table will not give any value of NumberFledged.
#'(e.g. see BroodID 2004_NA). These capture records should be included, but we
#'need to determine the amount of potential uncertainty around these records.
#'
#'\strong{LocationID}: For individuals captures in mist nets (specified by
#'trapping method column), a single LocationID "MN1" is used.
#'
#'\strong{StartSeason}: Assume all boxes were placed in the first year of the study.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_CHO <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       debug = FALSE,
                       output_type = "csv"){

  #Force choose.dir() if used
  force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- readxl::read_excel(paste(db, "database Choupal.xlsx", sep = "\\")) %>%
    #Clean all names with janitor into snake_case
    janitor::clean_names(case = "upper_camel") %T>%
    #There is one column with "º" that doesn't convert to ASCII with janitor
    #This appears the be a deeper issue than janitor (potentially in unicode translation)
    #Therefore, we will do the translation manually
    {colnames(.) <- iconv(colnames(.), "", "ASCII", sub = "")} %>%
    #Change species to "PARMAJ" because it's only PARMAJ in Choupal
    dplyr::mutate(Species = "PARMAJ",
           PopID = "CHO", Plot = NA_character_) %>%
    dplyr::filter(Species %in% species) %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year using Year_BroodID
    dplyr::mutate(BroodID = paste(Year, stringr::str_pad(BroodId, width = 3, pad = "0"), sep = "_"),
           IndvID = Ring,
           CaptureDate = lubridate::ymd(paste0(Year, "-01-01")) + JulianDate,
           Time = format.POSIXct(Time, format = "%H:%M:%S"),
           ChickAge = as.integer(dplyr::na_if(ChickAge, "na")),
           BreedingSeason = as.integer(Year),
           #If an individual was caught in a mist net give a generic LocationID (MN1)
           #Otherwise, give the box number.
           LocationID = purrr::pmap_chr(.l = list(TrapingMethod, as.character(Box)),
                                        .f = ~{

                                          if(..1 == "mist net"){

                                            return("MN1")

                                          } else {

                                            return(stringr::str_pad(..2, width = 2, pad = "0"))

                                          }

                                        }))

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- create_capture_CHO(all_data)

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_CHO(all_data)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_CHO(all_data)

  # NESTBOX DATA

  message("Compiling nestbox information...")

  Location_data <- create_location_CHO(all_data)

  # CREATE DEBUG REPORT

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "CHO", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

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
    dplyr::filter(TrapingMethod != "mist net")

  #Identify any adults caught on the brood
  #Reshape data so that MaleID and FemaleID are separate columns for each brood
  Parents <- data %>%
    #Remove all records with chicks
    dplyr::filter(Age != "C") %>%
    #Select only the Brood, Individual Id and their sex
    dplyr::select(BroodID, IndvID, Sex) %>%
    # "No ringed/no ring" becomes NA
    dplyr::mutate(IndvID = purrr::map_chr(.x = IndvID,
                                          .f = ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    #Reshape data so that we have a MaleID and FemaleID column
    #Rather than an individual row for each parent capture
    reshape2::melt(id = c("BroodID", "Sex")) %>%
    reshape2::dcast(BroodID ~ Sex) %>%
    dplyr::rename(FemaleID = `F`, MaleID = `M`)

  #Determine whether clutches are 2nd clutch
  ClutchType_obsv <- data %>%
    #For each brood, if a clutch is listed as '2nd' make it 'second' otherwise
    #'first'. For ClutchType_observed we are not giving broods the value
    #'replacement' as we don't have enough info.
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(ClutchType_observed = ifelse("2nd" %in% SecondClutch, "second", "first"))

  #Finally, we add in average mass and tarsus measured for all chicks at 14 - 16d
  #Subset only those chicks that were 14 - 16 days when captured.
  avg_measure <- data %>%
    dplyr::filter(!is.na(ChickAge) & between(ChickAge, 14, 16)) %>%
    #For every brood, determine the average mass and tarsus length
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(Weight, na.rm = T),
                     NumberChicksMass = length(stats::na.omit(Weight)),
                     AvgTarsus = mean(Tarsus, na.rm = T),
                     NumberChicksTarsus = length(stats::na.omit(Tarsus))) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.$AvgTarsus) ~ "Alternative"))

  Brood_data <- data %>%
    #Join in information on parents and clutch type
    dplyr::left_join(Parents, by = "BroodID") %>%
    dplyr::left_join(ClutchType_obsv, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    dplyr::select(-CodeLine:-Ring, -JulianDate:-StanderdisedTime, -TrapingMethod,
                  -BroodId:-Smear, -MeanEggWeight:-NEggsWeighted, -IndvID) %>%
    #Turn all remaining columns to characters
    #melt/cast requires all values to be of the same type
    dplyr::mutate_all(as.character) %>%
    #Melt and cast data so that we return the first value of relevant data for each brood
    #e.g. laying date, clutch size etc.
    #I've checked manually and the first value is always correct in each brood
    reshape2::melt(id = c("BroodID", "Species", "Year", "Site", "Box", "FemaleID", "MaleID")) %>%
    reshape2::dcast(BroodID + Species + Year + Site + Box + FemaleID + MaleID ~ ...,
                    fun.aggregate = first) %>%
    #Add in population/plot info
    #Convert LayingDate and HatchDate to date objects
    dplyr::mutate(FledgeDate = as.Date(NA),
                  HatchDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(HatchingDateJulian),
                  LayingDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(LayingDateJulian),
                  LayingDateError = NA_real_, ClutchSizeError = NA_real_,
                  HatchDateError = NA_real_, BroodSizeError = NA_real_,
                  FledgeDateError = NA_real_, NumberFledgedError = NA_real_,
                  AvgEggMass = NA_real_, NumberEggs = NA_integer_, NumberFledged = NoChicksOlder14D) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #Select relevant columns and rename
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot,
                  LocationID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayingDate, LayingDateError,
                  ClutchSize = FinalClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize = NoChicksHatched, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs) %>%
    #Join in average chick measurements
    dplyr::left_join(avg_measure, by = "BroodID") %>%
    #Convert everything back to the right format after making everything character
    #for the reshape
    dplyr::mutate(ExperimentID = NA_character_,
                  BreedingSeason = as.integer(BreedingSeason),
                  ClutchSize = as.integer(ClutchSize),
                  BroodSize = as.integer(BroodSize),
                  NumberFledged = as.integer(NumberFledged))

  return(Brood_data)

  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayingDate <- LayingDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL
  Age <- IndvID <- Sex <- M <- SecondClutch <- ChickAge <- NULL
  Weight <- Tarsus <- CodeLine <- Ring <- JulianDate <- StanderdisedTime <- BroodId <- NULL
  Smear <- MeanEggWeight <- NEggsWeighted <- Year <- HatchingDateJulian <- LayingDateJulian <- NULL
  NoChicksOlder14D <- TrapingMethod <- FinalCltchSize <- NoChicksHatched <- FinalClutchSize <- NULL

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
    dplyr::mutate(CapturePopID = PopID, ReleasePopID = PopID,
                  CapturePlot = Plot, ReleasePlot = Plot,
                  ischick = dplyr::case_when(.$Age == "C" ~ 1L,
                                             .$Age != "C" ~ 4L)) %>%
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    #Also include observed age (not calculated)
    dplyr::mutate(Age_observed = dplyr::case_when(.$Age == "C" ~ 1,
                                                  .$Age == "first year" ~ 5,
                                                  .$Age == "adult" ~ 6),
                  ObserverID = NA_character_,
                  OriginalTarsusMethod = "Alternative") %>%
    #Select out only those columns we need.
    dplyr::select(IndvID, Species, BreedingSeason,
                  CaptureDate, CaptureTime = Time,
                  ObserverID, ObserverID, LocationID,
                  CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot,
                  Mass = Weight, Tarsus, OriginalTarsusMethod,
                  WingLength = Wing, Age_observed, Age_calculated, ChickAge) %>%
    dplyr::ungroup()

  return(Capture_data)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  ischick <- Time <- NULL


}

#' Create individual data table for Choupal, Portugal.
#'
#' Create individual data table in standard format for data from Choupal,
#' Portugal.
#'
#' @param data Data frame. Primary data from Choupal.
#'
#' @return A data frame.

create_individual_CHO <- function(data){

  #Determine first age, brood, and ring year of each individual
  Individual_data <- data %>%
    #Arrange data for each individual chronologically
    dplyr::arrange(IndvID, CaptureDate) %>%
    #Replace 'na' with NA in Sex
    dplyr::mutate(Sex = na_if(x = Sex, y = "na")) %>%
    #For every individual
    dplyr::group_by(PopID, IndvID) %>%
    #Determine the first recorded broodID, year and age.
    #Determine if there were any records where sex was identified.
    dplyr::summarise(FirstBrood = first(BroodID),
              FirstYr = as.integer(first(Year)),
              FirstAge = as.integer(first(Age)),
              Species = "PARMAJ",
              Sex = ifelse(all(is.na(Sex)), NA_character_,
                           ifelse(all(stats::na.omit(Sex) %in% "M"), "M",
                                  ifelse(all(stats::na.omit(Sex) %in% "F"), "F",
                                         ifelse(all(c("F", "M") %in% Sex), "C", NA_character_))))) %>%
    dplyr::mutate(#Only assign a brood ID if they were first caught as a chick
      #Otherwise, the broodID will be their first clutch as a parent
      BroodIDLaid = purrr::pmap_chr(.l = list(FirstBrood = .$FirstBrood, FirstAge = .$FirstAge),
                                    function(FirstBrood, FirstAge){

                                      if(is.na(FirstAge) || FirstAge != "C"){

                                        return(NA)

                                      } else {

                                        return(FirstBrood)

                                      }

                                    }),
      #We have no information on cross-fostering, so we assume the brood laid and ringed are the same
      BroodIDFledged = BroodIDLaid,
      #Determine age at ringing as either chick or adult.
      RingAge = dplyr::case_when(.$FirstAge == "C" ~ "chick",
                                 is.na(.$FirstAge) ~ "adult",
                                 .$FirstAge != "C" ~ "adult")) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason = FirstYr, RingAge, Sex) %>%
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
  Location_data <- dplyr::tibble(LocationID = c(stats::na.omit(unique(data$Box)), "MN1"),
                          NestboxID = c(stats::na.omit(unique(data$Box)), "MN1")) %>%
    dplyr::mutate(LocationType = purrr::pmap_chr(.l = list(LocationID),
                                             .f = ~{
                                               if(..1 == "MN1"){

                                                 return("MN")

                                               } else {

                                                 return("NB")

                                               }}), PopID = "CHO",
                          Latitude = NA_real_, Longitude = NA_real_,
                          StartSeason = 2003L, EndSeason = NA_integer_,
                          Habitat = "Deciduous")

  return(Location_data)

}
