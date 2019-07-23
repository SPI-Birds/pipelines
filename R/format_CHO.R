#' Construct standard summary for data from Choupal, Portugal.
#'
#' A pipeline to produce a standard output for the great tit population
#' in Choupal, Portugal, administered by the University of Coimbra.
#' Output follows the HNB standard breeding data format.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' \strong{NumberFledged}: This population has no estimation of actual fledgling numbers.
#' The last time nests are counted is 14 days post hatching. We use this as an estimation of fledling numbers.
#' This also affects \emph{ClutchType_calc} as \emph{NumberFledged} is used to estimated second clutches.
#'
#' \strong{MinAge, RingAge}: Any individual caught as a chick was assumed to
#' have a EUring code of 1: 'Pullus: nestling or chick, unable to fly freely, still able to be caught by hand.'
#' Any individual not caught as a chick was assumed to have a EUring code of 4:
#' 'After first full year: year of hatching unknown'.
#'
#' \strong{ClutchType_observed}: In the raw data, there is no distinction between 'second' and 'replacement' clutches.
#' Any clutch recorded as '2nd' is assumed to be a 'second' clutch under our definitions.
#' 'ClutchType_calc' may give a more appropriate estimate of clutch type for this data.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2
#' @import magrittr

format_CHO <- function(db = choose.dir(),
                       species = NULL,
                       path = ".",
                       debug = FALSE){

  #Force choose.dir() if used
  force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  } else {



  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- read_excel(paste(db, "database Choupal.xlsx", sep = "\\")) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel") %>%
    #Change species to "PARMARJ" because it's only PARMAJ in Choupal
    #Add PopID and plot
    mutate(Species = "PARMAJ",
           PopID = "CHO", Plot = NA) %>%
    dplyr::filter(Species %in% species) %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year
    #Convert capture date into a date object (not Julian days)
    #Need this to order captures chronologically
    #Fix time format (it's currently trying to estimate date)
    mutate(BroodID = paste(Year, BroodId, sep = "_"),
           CaptureDate = lubridate::ymd(paste0(Year, "-01-01")) + JulianDate,
           Time = format.POSIXct(Time, format = "%H:%M:%S"),
           ChickAge = as.numeric(na_if(ChickAge, "na")),
           BreedingSeason = Year, LocationID = Box)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Capture_data <- create_capture_CHO(all_data)

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- create_brood_CHO(all_data)

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  Individual_data <- create_individual_CHO(all_data)

  ################
  # NESTBOX DATA #
  ################

  message("Compiling nestbox information...")

  Location_data <- create_location_CHO(all_data)

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "CHO", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

  message("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CHO.csv"), row.names = F)

  write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CHO.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CHO.csv"), row.names = F)

  write.csv(x = Location_data, file = paste0(path, "\\Location_data_CHO.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

}

####################################################################################################

create_capture_CHO <- function(data){

  #Take all data and add population/plot info
  #There is only one population/plot
  Capture_data <- data %>%
    dplyr::mutate(IndvID = Ring, CapturePopID = PopID, ReleasePopID = PopID,
           CapturePlot = Plot, ReleasePlot = Plot,
           ischick = dplyr::case_when(.$Age == "C" ~ 1,
                                      .$Age != "C" ~ 4)) %>%
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::ungroup() %>%
    #Also include observed age (not calculated)
    dplyr::mutate(Age_observed = dplyr::case_when(.$Age == "C" ~ 1,
                                                  .$Age == "first year" ~ 5,
                                                  .$Age == "adult" ~ 6),
                  ObserverID = NA, OriginalTarsusMethod = "Alternative") %>%
    #Select out only those columns we need.
    dplyr::select(IndvID, Species, BreedingSeason,
                  CaptureDate, CaptureTime = Time,
                  ObserverID, ObserverID,
                  CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot,
                  Mass = Weight, Tarsus, OriginalTarsusMethod,
                  WingLength = Wing, Age_observed, Age_calculated, ChickAge)

  return(Capture_data)

}

####################################################################################################

create_brood_CHO <- function(data){

  #The data is currently stored as capture data (i.e. each row is a capture)
  #This means there are multiple records for each brood, which we don't want.

  #Subset only those records with a brood ID.
  ##THERE IS ONE RECORD THAT WAS MEANT TO BE CAUGHT AT A BOX BUT NO ID RECORDED
  ##NEED TO WORK OUT HOW TO DEAL WITH THIS!!
  Brood_data_temp <- data %>%
    filter(!is.na(BroodId))

  #Identify any adults caught on the brood
  #Reshape data so that sex is a column not a row (i.e. MaleID, FemaleID)
  Parents <- Brood_data_temp %>%
    #Remove all records with chicks
    dplyr::filter(Age != "C") %>%
    #Select only the Brood, Individual Id and their sex
    dplyr::select(BroodID, Ring, Sex) %>%
    # "No ringed/no ring" becomes NA
    dplyr::mutate(Ring = map_chr(.x = Ring, .f = ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    #Reshape data so that we have a MaleID and FemaleID column
    #Rather than an individual row for each parent
    reshape2::melt(id = c("BroodID", "Sex")) %>%
    reshape2::dcast(BroodID ~ Sex) %>%
    dplyr::rename(FemaleID = `F`, MaleID = `M`)

  #Determine whether clutches are 2nd clutch
  ClutchType_obsv <- Brood_data_temp %>%
    #For each brood, if a clutch is listed as '2nd' make it 'second' otherwise 'first'.
    #For ClutchType_observed we are excluding 'replacement' as we don't have enough info.
    group_by(BroodID) %>%
    summarise(ClutchType_observed = ifelse("2nd" %in% SecondClutch, "second", "first"))

  Brood_data <- Brood_data_temp %>%
    #Join in information on parents and clutch type
    dplyr::left_join(Parents, by = "BroodID") %>%
    dplyr::left_join(ClutchType_obsv, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    dplyr::select(-CodeLine:-Ring, -JulianDate:-StanderdisedTime, -TrapingMethod,
                  -BroodId:-Smear, -MeanEggWeight:-NºEggsWeighted) %>%
    #Turn all remaining columns to characters
    #melt/cast requires all values to be of the same type
    dplyr::mutate_all(as.character) %>%
    #Melt and cast data so that we return the first value of relevant brood data
    #e.g. laying date, clutch size etc.
    #I've checked manually and the first value is always correct in each brood
    reshape2::melt(id = c("BroodID", "Species", "Year", "Site", "Box", "FemaleID", "MaleID")) %>%
    reshape2::dcast(BroodID + Species + Year + Site + Box + FemaleID + MaleID ~ ..., fun.aggregate = first) %>%
    #Add in population/plot info
    #Convert LayingDate and HatchDate to date objects
    dplyr::mutate(FledgeDate = NA,
           HatchDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(HatchingDateJulian),
           LayingDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(LayingDateJulian),
           LayingDateError = NA, ClutchSizeError = NA, HatchDateError = NA,
           BroodSizeError = NA, FledgeDateError = NA, NumberFledgedError = NA,
           AvgEggMass = NA, NumberEggs = NA, NumberFledged = NoChicksOlder14D) %>%
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
           AvgEggMass, NumberEggs)

  #Finally, we add in average mass and tarsus measured for all chicks at 14d
  #From Capture_data, subset only those chicks that were 14 - 16 days when captured.
  avg_measure <- data %>%
    dplyr::filter(!is.na(ChickAge) & between(ChickAge, 14, 16)) %>%
    #For every brood, determine the average mass and tarsus length
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass = mean(Weight, na.rm = T),
              NumberChicksMass = n(),
              AvgTarsus = mean(Tarsus, na.rm = T),
              NumberChicksTarsus = NumberChicksMass,
              OriginalTarsusMethod = "Alternative")

  #Add AvgMass and AvgTarsus to the Brood_data tibble
  Brood_data <- left_join(Brood_data, avg_measure, by = "BroodID") %>%
    dplyr::mutate(ExperimentID = NA)

  return(Brood_data)

}

####################################################################################################

create_individual_CHO <- function(data){

  #Determine first age, brood, and ring year of each individual
  Individual_data <- data %>%
    #Arrange data for each individual chronologically
    dplyr::arrange(Ring, CaptureDate) %>%
    #Replace 'na' with NA in Sex
    dplyr::mutate(Sex = na_if(x = Sex, y = "na")) %>%
    #For every individual
    dplyr::group_by(PopID, Ring) %>%
    #Determine the first recorded broodID, year and age.
    #Determine if there were any records where sex was identified.
    dplyr::summarise(FirstBrood = first(BroodID),
              FirstYr = first(Year),
              FirstAge = first(Age),
              Species = "PARMAJ",
              Sex = ifelse(all(is.na(Sex)), "U",
                           ifelse(any(Sex %in% "M"), "M",
                                  ifelse(any(Sex %in% "F"), "F", NA)))) %>%
    dplyr::mutate(#Only assign a brood ID if they were first caught as a chick
      #Otherwise, the broodID will be their first clutch as a parent
      BroodIDLaid = purrr::pmap_chr(.l = list(FirstBrood = .$FirstBrood,
                                              FirstAge = .$FirstAge),
                                    .f = function(FirstBrood, FirstAge){

                                      if(is.na(FirstAge) || FirstAge != "C"){

                                        return(NA)

                                      } else {

                                        return(FirstBrood)

                                      }

                                    }),
      #We have no information on cross-fostering, so we assume the brood laid and ringed are the same
      BroodIDFledged = BroodIDLaid,
      #Determine age at ringing with EUring code.
      #If caught as a chick assign 1: Pallus.
      #If caught as adult assign 4: >1yo, exact age unknown.
      RingAge = purrr::map_dbl(.x = .$FirstAge,
                               .f = function(.x){

                                 if(is.na(.x) || .x != "C"){

                                   return(4)

                                 } else {

                                   return(1)

                                 }

                               })) %>%
    select(IndvID = Ring, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason = FirstYr, RingAge, Sex)

  return(Individual_data)

}

####################################################################################################

create_location_CHO <- function(data){

  #There are no coordinates or box type information
  #Nestbox data is therefore just
  Location_data <- tibble(LocationID = unique(data$Box),
                          NestboxID = unique(data$Box),
                          LocationType = "NB", PopID = "CHO",
                          Latitude = NA, Longitude = NA,
                          StartSeason = NA, EndSeason = NA,
                          Habitat = "Deciduous") %>%
    filter(!is.na(LocationID))

  return(Location_data)

}
