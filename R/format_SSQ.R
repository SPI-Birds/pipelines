#' Construct standard summary for data from Santo Stefano Quisquina, Italy.
#'
#' A pipeline to produce a standard output for the great and blue tit population
#' in Santo Stefano Quisquina, Sicly, Italy, administered by Camillo Cusimano
#' and Daniel Campobello.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' @param db Location of database file.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#' @param debug For internal use when editing pipelines. If TRUE, pipeline
#'   generates a summary of pipeline data. This
#'   includes: a) Histogram of continuous variables with mean/SD b) unique
#'   values of all categorical variables.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2

format_SSQ <- function(db = NULL,
                       Species = NULL,
                       path = ".",
                       debug = FALSE){

  #Find database path
  if(is.null(db)){

    message("Please select a database location...")

    db <- file.choose()

  }

  #Record start time to provide processing time to the user.
  start_time <- Sys.time()

  #Read in data with readxl
  all_data <- readxl::read_excel(db) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel") %>%
    #Remove the column 'Row'. This is just the row number, we have this already.
    dplyr::select(-Row) %>%
    janitor::remove_empty(which = "rows")

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- all_data %>%
    left_join(filter(Species_codes, SpeciesID %in% c("14640", "14620")) %>%
                mutate(Species = c("Parus major", "Cyanistes caeruleus")) %>%
                select(Species, Code), by = "Species") %>%
    mutate(PopID = "SIC",
           BroodID = paste(Year, NestId, sep = "_")) %>%
    left_join(tibble::tibble(ClutchType_observed = c("first", "second", "replacement"),
                             Class = c(1, 3, 2)), by = "Class") %>%
    #Change column names to match consistent naming
    dplyr::rename(SampleYear = Year, LayingDate = Ld, ClutchSize = Cs,
                  HatchDate = Hd, BroodSize = Hs, NumberFledged = Fs,
                  FemaleID = FId, MaleID = MId) %>%
    dplyr::mutate(Species = Code, FledgeDate = NA)

  #Determine ClutchType_calc
  clutchtype <- dplyr::progress_estimated(n = nrow(Brood_data))

  Brood_data <- Brood_data %>%
    #Determine the 30 day cut-off for all species
    group_by(PopID, SampleYear, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(SampleYear, Species, FemaleID) %>%
    group_by(SampleYear, Species, FemaleID) %>%
    #Assume NAs in Fledglings are 0s.
    ### NEED TO CHECK WITH CAMILLO ABOUT THIS
    mutate(total_fledge = calc_cumfledge(x = NumberFledged, na.rm = T),
           row = 1:n()) %>%
    ungroup() %>%
    mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
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

                                                   #Then it is a replacement
                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, it is a secondary clutch
                                                   return("second")

                                                 }

                                               }

                                             })) %>%
    select(SampleYear, Species, PopID, Plot = HabitatOfRinging,
           LocationID = NestId, BroodID, FemaleID, MaleID,
           ClutchType_observed, ClutchType_calc,
           LayingDate:NumberFledged)


}
