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
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2
#' @import magrittr

format_Portugal <- function(db = NULL,
                            Species = NULL,
                            path = "."){

  #Find database path
  if(is.null(db)){

    message("Please select a database location...")

    db <- file.choose()

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- read_excel(db) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel") %>%
    #Change species to "GT" because it's only GT in Choupal
    #Add PopID and plot
    mutate(Species = "GT",
           PopID = "CHO", Plot = NA) %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year
    #Convert capture date into a date object (not Julian days)
    #Need this to order captures chronologically
    #Fix time format (it's currently trying to estimate date)
    mutate(BroodID = paste(Year, BroodId, sep = "_"),
           CaptureDate = lubridate::ymd(paste0(Year, "-01-01")) + JulianDate,
           Time = format.POSIXct(Time, format = "%H:%M:%S"))

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  #Take all data and add population/plot info
  #There is only one population/plot
  Capture_data <- all_data %>%
    mutate(CapturePopID = PopID, ReleasePopID = PopID,
           CapturePlot = Plot, ReleasePlot = Plot) %>%
    #Determine age at first capture for every individual
    #First arrange the data chronologically within each individual
    arrange(Ring, CaptureDate) %>%
    #Then, for each individual, determine the first age and year of capture
    group_by(Ring) %>%
    mutate(FirstAge = first(Age),
           FirstYear = first(Year)) %>%
    ungroup() %>%
    #Calculate age at each capture using EUring codes
    mutate(Age_calc = purrr::pmap_dbl(.l = list(Age = .$FirstAge,
                                              Year1 = .$FirstYear,
                                              YearN = .$Year),
                                    .f = function(Age, Year1, YearN){

                                      # If age at first capture is unknown
                                      # we cannot determine age at later captures
                                      if(is.na(Age)){

                                        return(NA)

                                      }

                                      #Determine number of years since first capture...
                                      diff_yr <- (YearN - Year1)

                                      #If it was not caught as a chick...
                                      if(Age != "C"){

                                        #If it's listed as 'first year' then we know it was EURING code 5 at first capture
                                        if(Age == "first year"){

                                          return(5 + 2*diff_yr)

                                        #Otherwise, when it was first caught it was at least EURING code 6.
                                        } else if(Age == "adult"){

                                          #Use categories where age is uncertain
                                          #(6, 8)
                                          return(6 + 2*diff_yr)

                                        }

                                      } else {

                                        #If it was caught as a chick
                                        if(diff_yr == 0){

                                          #Make the age at first capture 1 (nestling/unable to fly)
                                          #N.B. There is no distinction between chick and fledgling in the data
                                          return(1)

                                        } else {


                                          #Otherwise, use categories where age is certain (5, 7, etc.)
                                          return(3 + 2*diff_yr)

                                        }

                                      }

                                    })) %>%
    #Also include observed age (not calculated)
    rowwise() %>%
    mutate(Age_obsv = ifelse(Age == "C", 1, ifelse(Age == "first year", 5, ifelse(Age == "adult", 6, NA)))) %>%
    ungroup() %>%
    #Select out only those columns we need.
    select(CaptureDate, CaptureTime = Time,
           IndvID = Ring, Species,
           CapturePopID, CapturePlot,
           ReleasePopID, ReleasePlot,
           Mass = Weight, Tarsus, WingLength = Wing,
           Age_obsv, Age_calc, ChickAge)

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  #The data is currently stored as capture data (i.e. each row is a capture)
  #This means there are multiple records for each brood, which we don't want.

  #Subset only those records with a brood ID.
  ##THERE IS ONE RECORD THAT WAS MEANT TO BE CAUGHT AT A BOX BUT NO ID RECORDED
  ##NEED TO CHECK THIS!!
  Brood_data <- all_data %>%
    filter(!is.na(BroodId))

  #Identify any adults caught on the brood
  #Reshape data so that sex is a column not a row (i.e. MaleID, FemaleID)
  Parents <- Brood_data %>%
    #Remove all records with chicks
    filter(Age != "C") %>%
    #Select only the Brood, Individual Id and their sex
    select(BroodID, Ring, Sex) %>%
    # "No ringed/no ring" becomes NA
    mutate(Ring = map_chr(.x = Ring, .f = ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    #Reshape data so that we have a MaleID and FemaleID column
    #Rather than an individual row for each parent
    reshape2::melt(id = c("BroodID", "Sex")) %>%
    reshape2::dcast(BroodID ~ Sex) %>%
    rename(FemaleID = `F`, MaleID = `M`)

  #Determine whether clutches are 2nd clutch
  ClutchType_obsv <- Brood_data %>%
    #For each brood, if a clutch is listed as '2nd' make it 'second' otherwise 'first'.
    #For ClutchType_observed we are excluding 'replacement' as we don't have enough info.
    group_by(BroodID) %>%
    summarise(ClutchType_observed = ifelse("2nd" %in% SecondClutch, "second", "first"))

  Brood_data <- Brood_data %>%
    #Join in information on parents and clutch type
    left_join(Parents, by = "BroodID") %>%
    left_join(ClutchType_obsv, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    select(-CodeLine:-Ring, -JulianDate:-StanderdisedTime, -TrapingMethod,
           -BroodId:-Smear, -MeanEggWeight:-NºEggsWeighted) %>%
    #Turn all remaining columns to characters
    #melt/cast requires all values to be of the same type
    mutate_all(as.character) %>%
    #Melt and cast data so that we return the first value of relevant brood data
    #e.g. laying date, clutch size etc.
    #I've checked manually and the first value is always correct in each brood
    melt(id = c("BroodID", "Species", "Year", "Site", "Box", "FemaleID", "MaleID")) %>%
    dcast(BroodID + Species + Year + Site + Box + FemaleID + MaleID ~ ..., fun.aggregate = first) %>%
    #Determine the 30 day cut-off for each year
    group_by(Year) %>%
    mutate(cutoff = tryCatch(expr = min(as.numeric(LayingDateJulian), na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine cumulative fledgling information for each clutch
    # Arrange data chronologically for each female in each year
    arrange(Year, FemaleID, as.numeric(LayingDateJulian)) %>%
    group_by(Year, FemaleID)

  #Determine the cumulative number of fledglings produced up until the current
  #clutch Use this to determine if a clutch is second/replacement We don't
  #want cumsum because that includes the current row (e.g. 1, 2, 3 = 1, 3, 6)
  #We want the first value to always be 0 because we're interested in the data
  fledge_calc <- function(x, na.rm = TRUE){

    if(na.rm){

      #This func assumes that all NAs are just 0s.
      #This is needed because otherwise cumsum returns all NAs
      #However, all we need to know is if there was atleast 1 successful nest before the current nest
      x[!complete.cases(x)] <- 0

      nrs <- cumsum(x)

    } else {

      x <- is.na(x)

      nrs <- cumsum(x)

    }

    return(c(0, nrs[1:(length(nrs) - 1)]))

  }

  Brood_data <- Brood_data %>%
    mutate(total_fledge_narm = fledge_calc(NoChicksOlder14D, na.rm = TRUE),
           total_fledge_na = fledge_calc(NoChicksOlder14D, na.rm = FALSE),
           row = 1:n()) %>%
    ungroup()

  #Create a progress bar
  clutchtype <- dplyr::progress_estimated(n = nrow(Brood_data))

  Brood_data <- Brood_data %>%
    #For every clutch, go through and calculate the clutch type.
    mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge_narm,
                                                       any_na = .$total_fledge_na,
                                                       LD = as.numeric(.$LayingDateJulian)),
                                             .f = function(rows, femID, cutoff_date, nr_fledge_before, nr_fledge_now, LD){

                                               clutchtype$tick()$print()

                                               #Firstly, check if the nest has laying date infor.
                                               #If not, we cannot calculate ClutchType, return NA
                                               if(is.na(LD)){

                                                 return(NA)

                                               }

                                               #Next, check if the female is banded
                                               #If a female is unbanded we assume the nest can NEVER be secondary
                                               #If she had had a successful clutch before she would have been caught and banded
                                               #Therefore it can only be first or replacement (based on 30d rule)
                                               if(is.na(femID)){

                                                 #If the clutch was laid after the cut-off it's a replacement
                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, it's a first clutch
                                                   return("first")

                                                 }

                                               }

                                               #If the female is banded, then we need to apply all rules
                                               #If it's the first nest recorded for this female in this year...
                                               if(rows == 1){

                                                 #...and it doesn't meet the 30 day rule, then it is a replacement
                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, we assume it was the first clutch
                                                   return("first")

                                                 }

                                               #If it's NOT the first nest of the season for this female
                                               } else {

                                                 #If there were some successful clutches before this point...
                                                 if(nr_fledge_before > 0){

                                                   #Then it is a second clutch
                                                   #Even if some of the clutches were not checked (i.e. were NAs)
                                                   return("second")

                                                 } else {

                                                   #Otherwise, if there were no fledglings before this point
                                                   #But there was at least 1 NA
                                                   #Then we can't determine the clutch type
                                                   if(any_na > 0){

                                                     return(NA)

                                                   } else {

                                                     return("replacement")

                                                   }

                                                 }

                                               }

                                             })) %>%
    #Add in population/plot info
    #Convert LayingDate and HatchDate to date objects
    mutate(FledgeDate = NA,
           HatchDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(HatchingDateJulian),
           LayingDate = lubridate::ymd(paste0(Year, "-01-01")) + as.numeric(LayingDateJulian)) %>%
    #Select relevant columns and rename
    select(SampleYear = Year, Species, PopID, Plot,
           LocationID = Box, BroodID, FemaleID, MaleID,
           ClutchType_observed,
           ClutchType_calc, LayingDate,
           ClutchSize = FinalClutchSize,
           HatchDate, BroodSize = NoChicksHatched,
           FledgeDate,
           NumberFledged = NoChicksOlder14D)

  #Finally, we add in average mass and tarsus measured for all chicks at 14d
  #From Capture_data, subset only those chicks that were 14 - 16 days when captured.
  avg_measure <- all_data %>%
    filter(!is.na(ChickAge) & ChickAge != "na") %>%
    mutate(ChickAge = as.numeric(ChickAge)) %>%
    filter(between(ChickAge, 14, 16)) %>%
    #For every brood, determine the average mass and tarsus length
    group_by(BroodID) %>%
    summarise(AvgMass = mean(Weight, na.rm = T), AvgTarsus = mean(Tarsus, na.rm = T))

  #Add AvgMass and AvgTarsus to the Brood_data tibble
  Brood_data <- left_join(Brood_data, avg_measure, by = "BroodID")

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  #Determine first age, brood, and ring year of each individual
  Indv_data <- all_data %>%
    #Arrange data for each individual chronologically
    arrange(Ring, CaptureDate) %>%
    #Replace 'na' with NA in Sex
    mutate(Sex = na_if(x = Sex, y = "na")) %>%
    #For every individual
    group_by(PopID, Ring) %>%
    #Determine the first recorded broodID, year and age.
    #Determine if there were any records where sex was identified.
    summarise(FirstBrood = first(BroodID),
              FirstYr = first(Year),
              FirstAge = first(Age),
              Sex = ifelse(all(is.na(Sex)), "U",
                           ifelse(any(Sex %in% "M"), "M",
                                  ifelse(any(Sex %in% "F"), "F", NA)))) %>%
    mutate(#Only assign a brood ID if they were first caught as a chick
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
           BroodIDRinged = BroodIDLaid,
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
    select(Ring, Species, PopID, BroodIDLaid, BroodIDRinged, RingYear = FirstYr, RingAge, Sex)

  ################
  # NESTBOX DATA #
  ################

  message("Compiling nestbox information...")

  #There are no coordinates or box type information
  #Nestbox data is therefore just
  Nestbox_data <- tibble(LocationID = unique(all_data$Box),
           NestboxID = unique(all_data$Box),
           NestBoxType = NA, PopID = "CHO",
           Latitude = NA, Longitude = NA,
           StartYear = NA, EndYear = NA) %>%
    filter(!is.na(LocationID))

  message("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CHO.csv"), row.names = F)

  write.csv(x = Indv_data, file = paste0(path, "\\Indv_data_CHO.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CHO.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_CHO.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

}
