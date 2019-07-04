#' Construct standard summary for data from Santo Stefano Quisquina, Italy.
#'
#' A pipeline to produce a standard output for the great and blue tit population
#' in Santo Stefano Quisquina, Sicly, Italy, administered by Camillo Cusimano
#' and Daniela Campobello.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see XXXXX
#' PLACE HOLDER!
#'
#' \strong{BroodID}: For now we make BroodID as a combination of: year of nest,
#' nestID, layingdate. This is necessary because there are no other ways to distinguish
#' a two broods laid in the same nest and same year.
#'
#' \strong{Species}: In the individual data, there are some cases where
#' an IndvID is associated with >1 species. I assume these are just typos
#' and I will just take the first species.
#'
#' \strong{CaptureDate}: No exact capture date is given. Adults were only ever
#' captured with a nest, therefore, we use the laying date of the nest as a
#' proxy for capture date. Chick were also only ever captured on the nest, we
#' used laying date + 34 days as a proxy for capture date. 34 days was used to
#' include: egg laying phase (8 eggs on average), incubation phase (12 days??),
#' 14 days post hatching when chicks are often ringed.
#'
#' \strong{Age_calc}: All ringed chicks were assumed to be ringed at EURING code 1 (i.e. pre-fledging).
#' For adults where no age was provided, we assumed that first observation was 6 (i.e. at least 2 years old)
#'
#' \strong{Indv_data}: There are cases where chicks from different nests are given the same ring number.
#' Unsure if this is the rings being reused or a typo. Currently, I leave it as is and assume this is a typo that
#' needs to be fixed.
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring
#'   codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#' @param debug For internal use when editing pipelines. If TRUE, pipeline
#'   generates a summary of pipeline data. This includes: a) Histogram of
#'   continuous variables with mean/SD b) unique values of all categorical
#'   variables.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2

format_SSQ <- function(db = file.choose(),
                       Species = NULL,
                       path = ".",
                       debug = FALSE){

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- readxl::read_excel(db) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel") %>%
    #Remove the column 'Row'. This is just the row number, we have this already.
    dplyr::select(-Row) %>%
    janitor::remove_empty(which = "rows") %>%
    #Change column names to match consistent naming
    dplyr::rename(SampleYear = Year, LayingDate = Ld, ClutchSize = Cs,
                  HatchDate = Hd, BroodSize = Hs, NumberFledged = Fs,
                  FemaleID = FId, MaleID = MId, LocationID = NestId,
                  Plot = HabitatOfRinging,
                  Latitude = YCoord, Longitude = XCoord) %>%
    #Add species codes
    dplyr::left_join(filter(Species_codes, SpeciesID %in% c("14640", "14620")) %>%
                mutate(Species = c("Parus major", "Cyanistes caeruleus")) %>%
                select(Species, Code), by = "Species") %>%
    #Add other missing data:
    #- PopID
    #- BroodID (Year_NestID)
    #- ClutchType_observed
    #- FledgeDate
    dplyr::mutate(PopID = "SIC",
                  BroodID = paste(SampleYear, LocationID, LayingDate, sep = "_"),
                  ClutchType_observed = dplyr::case_when(.$Class == 1 ~ "first",
                                                         .$Class == 3 ~ "second",
                                                         .$Class == 2 ~ "replacement")) %>%
    dplyr::mutate(Species = Code, FledgeDate = NA, AvgEggMass = NA, NrEgg = NA, AvgChickMass = NA, NrChickMass = NA, AvgTarsus = NA, NrChickTarsus = NA)

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  #Determine ClutchType_calc
  clutchtype <- dplyr::progress_estimated(n = nrow(all_data))

  Brood_data <- all_data %>%
    #Determine the 30 day cut-off for all species
    group_by(PopID, SampleYear, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(SampleYear, Species, FemaleID) %>%
    group_by(SampleYear, Species, FemaleID) %>%
    #Assume NAs in Fledglings are 0s.
    mutate(total_fledge = calc_cumfledge(x = NumberFledged, na.rm = T),
           total_fledge_na = calc_cumfledge(x = NumberFledged, na.rm = F),
           row = 1:n()) %>%
    ungroup() %>%
    mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       na_fledge_before = .$total_fledge_na,
                                                       LD = .$LayingDate),
                                             .f = function(rows, femID, cutoff_date, nr_fledge_before, LD){

                                               clutchtype$tick()$print()

                                               #Firstly, check if the nest has a LD
                                               #If not, we cannot calculate BroodType

                                               if(is.na(LD)){

                                                 return(NA)

                                               }

                                               #Next, check if the female is banded
                                               #If a female is unbanded we assume the nest can NEVER be secondary
                                               #If she had had a successful clutch before she would have been caught and banded
                                               #Therefore it can only be first or replacement (based on 30d rule)
                                               if(is.na(femID)){

                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   return("first")

                                                 }

                                               }

                                               #If she is banded, then we need to apply all rules
                                               #If it's the first nest recorded for this female in this year...
                                               if(rows == 1){

                                                 #If it doesn't meet the 30 day rule, then name it as replacement
                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, we assume it was the first clutch
                                                   return("first")

                                                 }

                                               #If it's NOT the first nest of the season for this female
                                               } else {

                                                 #If there have been no fledglings before this point..
                                                 if(nr_fledge_before == 0){

                                                   #If there was atleast one NA record before this one
                                                   #then we don't know if number of fledged before is
                                                   #0 or >0. Therefore, we have to say NA.
                                                   if(na_fledge_before > 0){

                                                     return(NA)

                                                   } else {

                                                     #Otherwise, we can be confident that
                                                     #number of fledge before is 0
                                                     #and it must be a replacement
                                                     return("replacement")

                                                   }

                                                 } else {

                                                   #If there has been atleast one clutch
                                                   #that previously produced fledgligns
                                                   #then this nest is 'second'
                                                   #N.B. This is the case even if one of the previous nests
                                                   #was an NA. We just need to know if it's >0, not the exact number
                                                   return("second")

                                                 }

                                               }

                                             })) %>%
    select(SampleYear, Species, PopID, Plot,
           LocationID, BroodID, FemaleID, MaleID,
           ClutchType_observed, ClutchType_calc,
           LayingDate:BroodSize, FledgeDate, NumberFledged,
           AvgEggMass, NrEgg,
           AvgChickMass, NrChickMass,
           AvgTarsus, NrChickTarsus)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Adult_captures <- all_data %>%
    dplyr::select(SampleYear, PopID, Plot, LocationID, Species, LayingDate, FemaleID, FAge, MaleID, MAge) %>%
    reshape2::melt(measure.vars = c("FemaleID", "MaleID"), value.name = "IndvID") %>%
    #Remove all NAs, we're only interested in cases where parents were ID'd.
    dplyr::filter(!is.na(IndvID)) %>%
    #Make a single Age_obsv column. If variable == "FemaleID", then use FAge and visa versa
    rowwise() %>%
    dplyr::mutate(Age = ifelse(variable == "FemaleID", FAge, MAge)) %>%
    ungroup() %>%
    #Convert these age values to current EURING codes
    #If NA, we know it's an adult but don't know it's age
    #We don't want to assume anything here
    dplyr::left_join(tibble::tibble(Age = c(1, 2),
                                    Age_obsv = c(5, 6)), by = "Age") %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #Treat CaptureDate as Laying Date (currently in days since March 1st)
    #Check with Camilo about this.
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = as.Date(paste(SampleYear, "03", "01", sep = "-"), format = "%Y-%m-%d") - 1 + LayingDate,
                  CaptureTime = NA) %>%
    dplyr::select(-variable, -LayingDate, -FAge, -MAge)

  Chick_captures <- all_data %>%
    dplyr::select(SampleYear, Species, PopID, Plot, LocationID, LayingDate, Chick1Id:Chick13Id) %>%
    reshape2::melt(id.vars = c("SampleYear", "Species", "PopID", "Plot", "LocationID", "LayingDate"), value.name = "IndvID") %>%
    #Remove NAs
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #For chicks, use LayingDate + 34 for capture date 34 because this is an
    #estimate of laying date + laying time (~8 eggs on average) + incubation
    #time (~12 days) + 14 days (normal age when chicks are ringed)
    #Using LayingDate, rather than HatchDate, because LayingDate is recorded much more often.
    #CHECK WITH CAMILLO
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = as.Date(paste(SampleYear, "03", "01", sep = "-"), format = "%Y-%m-%d") - 1 + LayingDate + 34,
                  CaptureTime = NA, Age_obsv = 1, Age = 1) %>%
    dplyr::select(-variable, -LayingDate)

  #Combine Adult and chick data
  Capture_data <- dplyr::bind_rows(Adult_captures, Chick_captures) %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    #Add NA for morphometric measures and chick age
    #ChickAge is NA because we have no exact CaptureDate
    dplyr::mutate(Mass = NA, Tarsus = NA, WingLength = NA,
                  ChickAge = NA) %>%
    #Also determine Age_calc
    group_by(IndvID) %>%
    mutate(FirstAge = first(Age),
           FirstYear = first(SampleYear)) %>%
    ungroup() %>%
    #Calculate age at each capture using EUring codes
    dplyr::mutate(Age_calc = purrr::pmap_dbl(.l = list(Age = .$FirstAge,
                                                       Year1 = .$FirstYear,
                                                       YearN = .$SampleYear),
                                             .f = function(Age, Year1, YearN){

                                               #Determine number of years since first capture...
                                               diff_yr <- (YearN - Year1)

                                               #If it was not caught as a chick...
                                               if(Age != 1 | is.na(Age)){

                                                 #If it's listed as 'first year' then we know it was EURING code 5 at first capture
                                                 if(!is.na(Age) & Age == 5){

                                                   return(5 + 2*diff_yr)

                                                 #Otherwise, when it was first caught it was at least EURING code 6.
                                                 #This also applies to birds with Age == NA, this is because the only birds
                                                 #That have NA are adults (all chicks are listed as Age = 1)
                                                 } else {

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
    #Order variables to match other data
    dplyr::select(CaptureDate, CaptureTime, IndvID, Species,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot,
                  Mass, Tarsus, WingLength, Age_obsv, Age_calc, ChickAge)

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  #Create a list of all chicks
  Chick_IDs <- all_data %>%
    dplyr::select(BroodID, Chick1Id:Chick13Id) %>%
    reshape2::melt(id.vars = "BroodID", value.name = "IndvID") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::select(-variable, BroodIDLaid = BroodID)

  Indv_data <- Capture_data %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = first(Species),
                     RingYear = min(lubridate::year(CaptureDate)),
                     RingAge = first(Age_obsv)) %>%
    dplyr::rowwise() %>%
    #For sex, we only know if an individual was a female...
    #Adults were never caught as males and chicks were never sexed.
    dplyr::mutate(Sex = ifelse(IndvID %in% Brood_data$FemaleID, "F", NA)) %>%
    dplyr::ungroup() %>%
    #Join in BroodID from the reshaped Chick_IDs table
    dplyr::left_join(Chick_IDs, by = "IndvID") %>%
    dplyr::mutate(BroodIDRinged = BroodIDLaid,
                  PopID = "SSQ") %>%
    select(IndvID, Species, PopID, BroodIDLaid,
           BroodIDRinged, RingYear, RingAge, Sex)

  ################
  # NESTBOX DATA #
  ################

  message("Compiling nestbox information...")

  Nestbox_data <- all_data %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestBoxType = NA,
                     PopID = "SSQ",
                     StartYear = NA, EndYear = NA) %>%
    dplyr::mutate(NestboxID = LocationID) %>%
    #Join in first latitude and longitude data recorded for this box.
    #It's not clear why these are ever different, need to ask.
    dplyr::left_join(all_data %>% group_by(LocationID) %>% slice(1) %>% select(LocationID, Latitude, Longitude), by = "LocationID") %>%
    dplyr::select(LocationID, NestboxID, NestBoxType, PopID, Latitude, Longitude, StartYear, EndYear)

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "SSQ", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Indv_data)

  }

  message("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_SSQ.csv"), row.names = F)

  write.csv(x = Indv_data, file = paste0(path, "\\Indv_data_SSQ.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_SSQ.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_SSQ.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

}
