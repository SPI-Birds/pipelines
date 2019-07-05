#' Construct standard summary for data from Velky Kosir, Czechia.
#'
#' A pipeline to produce a standard output for the nest box population
#' in Velky Kosir, Czechia, administered by Milos Krist.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see XXXXX
#' PLACE HOLDER!
#'
#' \strong{Species}: There are a small number of records for species other than
#' great tit, blue tit and collared flycatcher. These are currently excluded.
#' Blue tits have 93 recorded broods. This is less than our previous 100 broods
#' cut-off, but they are still included.
#'
#' \strong{ClutchType_calc}: We assume that any NA records for number fledged
#' are true unknowns (rather than 0s). In which case, we can't always
#' estimate whether a clutch is a second clutch.
#'
#' \strong{ExperimentID}: Currently, we just copy the text directly
#' from the tables. Need to go through an classify each experiment
#' and check with Milos that these classifications are reasonable.
#'
#' \strong{BroodID}: BroodID is currently Year_nestbox_day_month.
#' This accounts for multiple clutches laid in the same nest box.
#'
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

format_VEL <- function(db = choose.dir(),
                       Species = NULL,
                       path = ".",
                       debug = FALSE) {

  start_time <- Sys.time()

  ## Read in flycatcher data. There are some issues will coercion of col types
  ## so we specify column types manually. With this we can skip certain cols.
  ## we skip:
  ## - row number
  ## - whether chicks were observed dead (we currently have no col for this in the standard format)
  ## - Adult wing and forhead patch measures
  ## - Picture and geolocator info
  ## - Info on which eggs were transferred in cross foster
  FICALB_data <- readxl::read_excel(paste0(db, "/Velky_Kosir_flycatchers.xlsx"),
                                    col_types = c("skip", "numeric", "text",
                                                  "text", "list",
                                                  "numeric", "text",
                                                  "text", "numeric",
                                                  "list", "numeric",
                                                  rep("text", 8),
                                                  rep(c(rep("numeric", 4), "skip"), 8),
                                                  "text", "list", "numeric",
                                                  "numeric", "numeric",
                                                  rep("skip", 6),
                                                  "text", "list", "text",
                                                  "numeric", "numeric",
                                                  "numeric", rep("skip", 13),
                                                  "text", rep("skip", 16))) %>%
    janitor::clean_names() %>%
    ## Date info is sometimes recorded as dd/mm/yyyy and sometimes as dd.mm.yyyy.
    ## This causes some issues with date parsing. If we parse as text, it converts
    ## the excel dates into numerics. If we parse as date, it gives NAs for the
    ## cases where date is stored as dd.mm.yyyy.
    ## Instead we parse each one separately, based on the best guess of readxl.
    ## This returns dates for dd/mm/yyyy, but character for dd.mm.yyyy.
    ## Then we just have to go through and make these few character strings into dates.
    dplyr::mutate_at(.vars = vars(contains("date")),
                     .funs = function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest() %>%
    ## CHANGE COL NAMES TO MATCH STANDARD FORMAT
    dplyr::mutate(SampleYear = year,
                  Species = Species_codes[which(Species_codes$SpeciesID == 13480), ]$Code,
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = nest,
                  BroodID = paste(SampleYear, nest, lubridate::day(laying_date), lubridate::month(laying_date), sep = "_"),
                  FemaleID = female_ring, MaleID = male_ring,
                  ClutchType_observed = NA,
                  LayingDate = laying_date, LayingDateError = NA,
                  ClutchSize = clutch_size, ClutchSizeError = NA,
                  HatchDate = hatching_date, HatchDateError = NA,
                  BroodSize = number_hatched, BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledged = number_fledged, NumberFledgedError = NA,
                  ##ADD MORPHO COLUMNS LATER
                  AvgEggMass = NA, NrEggs = NA,
                  AvgChickMass = NA, NrChicksMass = NA,
                  AvgTarsus = NA, NrChicksTarsus = NA,
                  ExperimentID = treatment)

  ## No columns are excluded except row number and final col.
  ## Final col has some data, but no column name
  ## Dates are dealt with the same way as flycatchers
  ## We only use data on GT and BT. There is a small number of clutches
  ## for other species, but all < 40.
  TIT_data <- readxl::read_excel(paste0(db, "/Velky_Kosir_tits.xls"),
                                    col_types = c("skip", "numeric", "text", "text",
                                                  "text", "text", "list",
                                                  "list", "list", "text",
                                                  "text", "text", "text",
                                                  "text", "text", "text", "text",
                                                  "skip")) %>%
    janitor::clean_names() %>%
    dplyr::filter(species %in% c("blue tit", "great tit")) %>%
    dplyr::mutate_at(.vars = vars(contains("date")),
                     .funs = function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest()

  #Determine laying date for every nest, accounting for error in nests
  TIT_LD_error <- purrr::pmap_dfr(.l = list(TIT_data$laying_date,
                                            TIT_data$laying_date_maximum,
                                            TIT_data$laying_date_minimum),
                                  .f = ~{

                                    if(!is.na(..1)){

                                      return(tibble::tibble(LayingDate = ..1,
                                                            LayingDateError = NA))

                                    } else {

                                      return(tibble::tibble(LayingDate = mean(c(..2, ..3)),
                                                            LayingDateError = as.numeric((..2 - ..3)/2)))

                                    }

                                  })

  TIT_data <- TIT_data %>%
    dplyr::mutate(SampleYear = year,
                  Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = nest_box,
                  FemaleID = female_ring, MaleID = NA,
                  ClutchType_observed = NA,
                  LayingDate = TIT_LD_error$LayingDate,
                  LayingDateError = TIT_LD_error$LayingDateError,
                  ##FOR NOW, I'M JUST ASSUMING THAT 11+ CLUTCH SIZE
                  ##MEANS CLUTCH 11 THAT WAS ARTIFIICALLY INCREASED
                  ##THEREFORE, I JUST REMOVE THE + AND ADD THAT IT WAS AN EXPERIMENTAL NEST
                  ##NEED TO CHECK WITH MILOS
                  ClutchSize = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", clutch_size)),
                  ClutchSizeError = NA,
                  ##DO THE SAME FOR BROOD SIZE AND NUMBER FLEDGED
                  HatchDate = NA, HatchDateError = NA,
                  BroodSize = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", number_hatched)),
                  BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledged = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", number_fledged)),
                  NumberFledgedError = NA,
                  ##THERE IS NO MORPHOMETRICS FOR TITS
                  AvgEggMass = NA, NrEggs = NA,
                  AvgChickMass = NA, NrChicksMass = NA,
                  AvgTarsus = NA, NrChicksTarsus = NA,
                  ExperimentID = experiment,
                  ## Estimate broodID last because it requires us to estimate LayingDate first
                  BroodID = paste(year, nest_box, lubridate::day(LayingDate), lubridate::month(LayingDate), sep = "_"))

  ##############
  # BROOD DATA #
  ##############

  Brood_data <- create_brood_VEL(FICALB_data, TIT_data)

  ################
  # CAPTURE DATA #
  ################

  Capture_data <- create_capture_VEL(FICALB_data, TIT_data)

}


create_brood_VEL <- function(FICALB_data, TIT_data) {

  FICALB_broods <- FICALB_data %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(SampleYear:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  TIT_broods <- TIT_data %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(SampleYear:LocationID, BroodID,
                  FemaleID:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  return(dplyr::bind_rows(FICALB_broods, TIT_broods))

}

create_capture_VEL <- function(FICALB_data, TIT_data) {

  ## First create a table for flycatcher chick captures on the nest
  FICALB_chicks <- FICALB_data %>%
    dplyr::select(SampleYear, Species, Plot, BroodID, LayingDate, ClutchSize, HatchDate, x1_young_ring:x8y_wing) %>%
    reshape2::melt(measure.vars = c("x1_young_ring", "x2_young_ring",
                                               "x3_young_ring", "x4_young_ring",
                                               "x5_young_ring", "x6_young_ring",
                                               "x7_young_ring", "x8_young_ring"),
                              value.name = "IndvID", variable.name = "ChickNr") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    dplyr::group_by(ChickNr) %>%
    tidyr::nest() %>%
    dplyr::mutate(ChickNr = substr(ChickNr, 1, 2)) %>%
    dplyr::mutate(data = purrr::pmap(.l = list(ChickNr, data),
                  .f = ~{

                    output <- ..2 %>%
                      dplyr::select(SampleYear:HatchDate, IndvID, contains(..1)) %>%
                      reshape2::melt(id.vars = c(1:8, 10, 12), value.name = "Mass") %>%
                      ## Rename tarsus and wing to remove name of chick
                      dplyr::rename_at(.vars = vars(contains("tarsus")), .funs = ~{"Tarsus"}) %>%
                      dplyr::rename_at(.vars = vars(contains("wing")), .funs = ~{"WingLength"}) %>%
                      ## Add chick age based on the variable name
                      dplyr::mutate(ChickAge = as.numeric(stringr::str_sub(variable, 9, -1)),
                                    ## Capture date is hatchdate + chick age
                                    ## If no hatch date is known then use laying date + clutch size + incubation + chick age
                                    ## We assume incubation of 15 days, but need to check with Milos.
                                    ## If there is no hatch date OR laying date then just use NA
                                    CaptureDate = purrr::pmap(.l = list(LayingDate, ClutchSize, HatchDate, ChickAge),
                                                              .f = ~{

                                                                if(!is.na(..3)){

                                                                  return(..3 + lubridate::days(..4))

                                                                } else if(!is.na(..1)) {

                                                                  return(..1 + lubridate::days(..2 + 15 + ..4))

                                                                } else {

                                                                  return(NA)

                                                                }

                                                              }),
                                    CaptureTime = NA, CapturePopID = "VEL", CapturePlot = Plot,
                                    ## All chick records were 6 or 13 days, so all are listed as EURING age 1
                                    ReleasePopID = "VEL", ReleasePlot = Plot, Age_obsv = 1, Age_calc = NA) %>%
                      tidyr::unnest(CaptureDate) %>%
                      dplyr::select(SampleYear, IndvID, Species, CaptureDate, CaptureTime, CapturePopID, CapturePlot,
                                    ReleasePopID, ReleasePlot, Mass, Tarsus, WingLength, Age_obsv,
                                    Age_calc, ChickAge)

                    ## When the chick is 6 days old, then tarsus and wing length are not used
                    ## They were only collected at 13 days old
                    output[output$ChickAge == 6, ]$Tarsus <- NA
                    output[output$ChickAge == 6, ]$WingLength <- NA

                    return(output)

                  })) %>%
    tidyr::unnest(data) %>%
    dplyr::select(-ChickNr)

  FICALB_adults <- FICALB_data %>%
    dplyr::select(SampleYear, Species, Plot, LocationID, LayingDate, BroodID, LayingDate, FemaleID, date_of_capture_52, tarsus_53:wing_55,
                  MaleID, date_of_capture_57, age:wing_61) %>%
    reshape2::melt(measure.vars = c("FemaleID", "MaleID"), value.name = "IndvID", variable.name = "Sex") %>%
    dplyr::filter(!is.na(IndvID)) %>%
    ## Give individuals a sex, we will use this in our Individual_data table
    dplyr::mutate(Sex = stringr::str_sub(Sex, 0, 1),
                  Tarsus = purrr::pmap_dbl(.l = list(Sex, tarsus_53, tarsus_59),
                                           .f = ~{if(..1 == "F") ..2 else ..3}),
                  WingLength = purrr::pmap_dbl(.l = list(Sex, wing_55, wing_61),
                                               .f = ~{if(..1 == "F") ..2 else ..3}),
                  Mass = purrr::pmap_dbl(.l = list(Sex, mass_54, mass_60),
                                         .f = ~{if(..1 == "F") ..2 else ..3}),
                  ##Determine age of males based on 'age' column
                  Age_obsv = purrr::pmap_dbl(.l = list(Sex, age),
                                             .f = ~{

                                               if(..1 == "M"){

                                                 return(dplyr::case_when(..2 == "old" ~ 6,
                                                                  ..2 == "young" ~ 5))

                                               } else {

                                                 return(NA)

                                               }

                                             }),
                  CaptureDate = purrr::pmap(.l = list(Sex, date_of_capture_52, date_of_capture_57),
                                                .f = ~{

                                                  if(..1 == "F"){

                                                    return(..2)

                                                  } else {

                                                    return(..3)

                                                  }

                                                }),
                  CapturePopID = "VEL", ReleasePopID = "VEL",
                  CapturePlot = Plot, ReleasePlot = Plot) %>%
    tidyr::unnest() %>%
    dplyr::select(Species, SampleYear, LocationID, BroodID,
                  IndvID:CaptureDate, Sex)

  FICALB_alldat <- dplyr::bind_rows(FICALB_chicks, FICALB_adults) %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    ## Calculate age at capture
    dplyr::group_by(IndvID) %>%
    dplyr::mutate(FirstAge = first(Age_obsv),
                  FirstYear = first(SampleYear)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age_calc = purrr::pmap_dbl(.l = list(Age = FirstAge,
                                                       Year1 = FirstYear,
                                                       YearN = SampleYear),
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

                                             }))

}
