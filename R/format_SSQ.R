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
#' nestID, layingdate. This is necessary because there are no other ways to
#' distinguish a two broods laid in the same nest and same year.
#'
#' \strong{Species}: In the individual data, there are some cases where an
#' IndvID is associated with >1 species. I assume these are just typos and I
#' will just take the first species.
#'
#' \strong{CaptureDate}: No exact capture date is currently given. For adults we
#' use the laying date of the nest as a proxy for capture date. Chick were only
#' ever captured on the nest, we used laying date + clutch size + 15 days
#' incubation + 12 days. This is because chicks were ringed at 12 days old at
#' the latest.
#'
#' \strong{Age_calc}: All ringed chicks were assumed to be ringed at EURING code
#' 1 (i.e. pre-fledging). For adults where no age was provided, we assumed that
#' first observation was 6 (i.e. at least 2 years old)
#'
#' \strong{Individual_data}: There are cases where chicks from different nests are
#' given the same ring number. Unsure if this is the rings being reused or a
#' typo. Currently, I leave it as is and assume this is a typo that needs to be
#' fixed in the primary data.
#'
#' \strong{Nestbox StartYear}: Some nest boxes were replaced over the course of
#' the study; however, these replacements were not explicitly recorded.
#' Therefore, we list all nestboxes as functioning for the full study period.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export

format_SSQ <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       debug = FALSE,
                       output_type = "csv"){

  #Force user to select directory
  force(db)

  db <- paste0(db, "\\Data Sicily CusimanoC_MassaB.xlsx")

  if(is.null(species)){

    species <- Species_codes$Code

  }

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
    dplyr::rename(BreedingSeason = Year, LayingDate = Ld, ClutchSize = Cs,
                  HatchDate = Hd, BroodSize = Hs, NumberFledged = Fs,
                  FemaleID = FId, MaleID = MId, LocationID = NestId,
                  Plot = HabitatOfRinging,
                  Latitude = YCoord, Longitude = XCoord) %>%
    #Add species codes
    dplyr::mutate(Species = dplyr::case_when(.$Species == "Parus majoe" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code,
                                             .$Species == "Cyanistes caeruleus" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code)) %>%
    #Filter species
    dplyr::filter(Species %in% species) %>%
    #Add other missing data:
    #- PopID
    #- BroodID (Year_NestID)
    #- ClutchType_observed
    #- FledgeDate
    dplyr::mutate(PopID = "SSQ",
                  BroodID = paste(BreedingSeason, LocationID, LayingDate, sep = "_"),
                  ClutchType_observed = dplyr::case_when(.$Class == 1 ~ "first",
                                                         .$Class == 3 ~ "second",
                                                         .$Class == 2 ~ "replacement")) %>%
    dplyr::mutate(Species = Code, FledgeDate = NA, AvgEggMass = NA, NumberEgg = NA, AvgChickMass = NA, NumberChicksMass = NA, AvgTarsus = NA, NumberChicksTarsus = NA,
                  LayingDateError = NA, ClutchSizeError = NA, HatchDateError = NA, BroodSizeError = NA,
                  FledgeDateError = NA, NumberFledgedError = NA, ExperimentID = NA)

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  #Determine CluchType_calculated
  clutchtype <- dplyr::progress_estimated(n = nrow(all_data))

  Brood_data <- all_data %>%
    #Determine the 30 day cut-off for all species
    group_by(PopID, BreedingSeason, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(BreedingSeason, Species, FemaleID) %>%
    group_by(BreedingSeason, Species, FemaleID) %>%
    #Assume NAs in Fledglings are 0s.
    mutate(total_fledge = calc_cumfledge(x = NumberFledged, na.rm = T),
           total_fledge_na = calc_cumfledge(x = NumberFledged, na.rm = F),
           row = 1:n()) %>%
    ungroup() %>%
    mutate(CluchType_calculated = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       na_fledge_before = .$total_fledge_na,
                                                       LD = .$LayingDate),
                                             .f = function(rows, femID, cutoff_date,
                                                           nr_fledge_before, na_fledge_before,
                                                           LD){

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
    dplyr::select(BreedingSeason, Species, PopID, Plot,
           LocationID, BroodID, FemaleID, MaleID,
           ClutchType_observed, CluchType_calculated,
           LayingDate, LayingDateError,
           ClutchSize, ClutchSizeError,
           HatchDate, HatchDateError,
           BroodSize, BroodSizeError,
           FledgeDate, FledgeDateError,
           NumberFledged, NumberFledgedError,
           AvgEggMass, NrEgg,
           AvgChickMass, NrChickMass,
           AvgTarsus, NrChickTarsus, ExperimentID) %>%
    #Change laying date and hatch date to date objects
    dplyr::mutate(LayingDate = as.Date(paste0("01/03/", BreedingSeason)) + LayingDate - 1,
                  HatchDate = as.Date(paste0("01/03/", BreedingSeason)) + HatchDate - 1)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Adult_captures <- all_data %>%
    dplyr::select(BreedingSeason, PopID, Plot, LocationID, Species, LayingDate, FemaleID, FAge, MaleID, MAge) %>%
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
    dplyr::mutate(Age_obsv = dplyr::case_when(.$Age == 1 ~ 5,
                                              .$Age == 2 ~ 6)) %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #Treat CaptureDate of adults as the Laying Date (currently in days since March 1st)
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = as.Date(paste(BreedingSeason, "03", "01", sep = "-"), format = "%Y-%m-%d") - 1 + LayingDate,
                  CaptureTime = NA) %>%
    dplyr::select(-variable, -LayingDate, -FAge, -MAge)

  Chick_captures <- all_data %>%
    dplyr::select(BreedingSeason, Species, PopID, Plot, LocationID, LayingDate, ClutchSize, Chick1Id:Chick13Id) %>%
    reshape2::melt(id.vars = c("BreedingSeason", "Species", "PopID", "Plot", "LocationID", "LayingDate", "ClutchSize"), value.name = "IndvID") %>%
    #Remove NAs
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::rename(CapturePopID = PopID, CapturePlot = Plot) %>%
    #For chicks, we currently don't have the version of the individual level capture data.
    #For now, we use LayingDate + ClutchSize + 15 (incubation days in SSQ) + 12.
    #Chicks were captured and weighed at 12 days old at the latest
    dplyr::mutate(ReleasePopID = CapturePopID, ReleasePlot = CapturePlot,
                  CaptureDate = as.Date(paste(BreedingSeason, "03", "01", sep = "-"), format = "%Y-%m-%d") - 1 + LayingDate + ClutchSize + 27,
                  CaptureTime = NA, Age_obsv = 1, Age = 1) %>%
    dplyr::select(-variable, -LayingDate, -ClutchSize)

  #Combine Adult and chick data
  Capture_data <- dplyr::bind_rows(Adult_captures, Chick_captures) %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    #Add NA for morphometric measures and chick age
    #ChickAge (in days) is NA because we have no exact CaptureDate
    dplyr::mutate(Mass = NA, Tarsus = NA, WingLength = NA,
                  ChickAge = NA) %>%
    #Also determine Age_calc
    group_by(IndvID) %>%
    mutate(FirstAge = first(Age),
           FirstYear = first(BreedingSeason)) %>%
    ungroup() %>%
    #Calculate age at each capture using EURING codes
    dplyr::mutate(Age_calc = purrr::pmap_dbl(.l = list(Age = .$FirstAge,
                                                       Year1 = .$FirstYear,
                                                       YearN = .$BreedingSeason),
                                             .f = function(Age, Year1, YearN){

                                               #Determine number of years since first capture...
                                               diff_yr <- (YearN - Year1)

                                               #If it was not caught as a chick...
                                               if(Age != 1 | is.na(Age)){

                                                 #If it's listed as EURING 5,
                                                 #then its age is known at first capture
                                                 if(!is.na(Age) & Age == 5){

                                                   return(5 + 2*diff_yr)

                                                 #Otherwise, when it was first caught it was at least EURING code 6.
                                                 #This also applies to birds with both Age == 6 (where they were recorded as being >2yo)
                                                 #and Age == NA. We assume any bird that was a known 2nd year would be listed as such.
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

  Individual_data <- Capture_data %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = first(Species),
                     RingYear = min(lubridate::year(CaptureDate)),
                     RingAge = dplyr::case_when(is.na(first(Age_obsv)) ~ "adult",
                                                first(Age_obsv) == 1 ~ "chick",
                                                first(Age_obsv) > 1 ~ "adult")) %>%
    dplyr::mutate(Sex = dplyr::case_when(.$IndvID %in% Brood_data$FemaleID ~ "F",
                                         .$IndvID %in% Brood_data$MaleID ~ "M")) %>%
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

  Location_data <- all_data %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestBoxType = NA,
                     PopID = "SSQ",
                     StartYear = 1993, EndYear = NA) %>%
    dplyr::mutate(NestboxID = LocationID) %>%
    #Join in first latitude and longitude data recorded for this box.
    #It's not clear why these are ever different, need to ask.
    dplyr::left_join(all_data %>% group_by(LocationID) %>% slice(1) %>% select(LocationID, Latitude, Longitude), by = "LocationID") %>%
    dplyr::select(LocationID, NestboxID, NestBoxType, PopID, Latitude, Longitude, StartYear, EndYear)

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "SSQ", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

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
