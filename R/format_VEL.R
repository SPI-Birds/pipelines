#' Construct standard summary for data from Velky Kosir, Czechia.
#'
#' A pipeline to produce a standard output for the nest box population in Velky
#' Kosir, Czechia, administered by Milos Krist.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{Species}: There are a small number of records for species other than
#' great tit, blue tit and collared flycatcher. These are currently excluded.
#' Blue tits have 93 recorded broods. This is less than our previous 100 broods
#' cut-off, but they are still included.
#'
#' \strong{ClutchType_calc}: We assume that any NA records for number fledged
#' are true unknowns (rather than 0s). In which case, we can't always estimate
#' whether a clutch is a second clutch.
#'
#' \strong{ExperimentID}: Currently, we just copy the text directly from the
#' tables. Need to go through an classify each experiment and check with Milos
#' that these classifications are reasonable.
#'
#' \strong{BroodID}: BroodID is currently Year_nestbox_day_month. This accounts
#' for multiple clutches laid in the same nest box.
#'
#' \strong{Tarsus}: Tarsus length is originally measured using Oxford maximum
#' method. We convert this to Svensson's Alternative method using formula
#' described in the standard format.
#'
#' \strong{ClutchSize, BroodSize, NumberFledged:} Some records have uncertainty
#' (e.g. 11+). We're unsure how large this uncertainty is. It's currently
#' ignored, but will talk with data owner to incorporate this.
#'
#' \strong{CaptureDate:} In the raw data for flycatchers there are columns for
#' measurements taken at 6 days and at 13 days. Sometimes, no measurements are
#' recorded (i.e. columns are NA) and we need to decide how to deal with these.
#' In discussion with the data owner we know that chicks are generally ringed at 6 days;
#' therefore, we assume that any ringed chick was captured at 6 days old, even if
#' they have no other measurements. We only include a further capture at 13 days
#' old if some measurements were taken. Those individuals with no mass/tarsus/wing length
#' measures recorded at 13 days were assumed to have not been captured at this time.
#'
#'
#' @inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export
#' @import stringr

format_VEL <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R") {

  #Force user to select directory
  force(db)

  #Add species filter
  if(is.null(species)){

    species <- Species_codes$Code

  }

  start_time <- Sys.time()

  message("Importing primary data...")

  ## Read in flycatcher data. There are some issues will coercion of col types
  ## so we specify column types manually. With this we can skip certain cols.
  ## we skip:
  ## - row number
  ## - whether chicks were observed dead (we currently have no col for this in the standard format)
  ## - Adult wing and forhead patch measures
  ## - Picture and geolocator info
  ## - Info on which eggs were transferred in cross foster
  FICALB_data <- suppressMessages(readxl::read_excel(paste0(db, "/VEL_PrimaryData_flycatchers.xlsx"),
                                    col_types = c("skip", "numeric", "text",
                                                  "text", "list",
                                                  "text", "text",
                                                  "text", "text",
                                                  "list", "text",
                                                  rep("text", 8),
                                                  rep(c(rep("numeric", 4), "skip"), 8),
                                                  "text", "list", "numeric",
                                                  "numeric", "numeric",
                                                  rep("skip", 6),
                                                  "text", "list", "text",
                                                  "numeric", "numeric",
                                                  "numeric", rep("skip", 13),
                                                  "text", rep("skip", 16)))) %>%
    janitor::clean_names() %>%
    ## Date info is sometimes recorded as dd/mm/yyyy and sometimes as dd.mm.yyyy.
    ## This causes some issues with date parsing. If we parse as text, it converts
    ## the excel dates into numerics. If we parse as date, it gives NAs for the
    ## cases where date is stored as dd.mm.yyyy.
    ## Instead we parse each one separately, based on the best guess of readxl.
    ## This returns dates for dd/mm/yyyy, but character for dd.mm.yyyy.
    ## Then we just have to go through and make these few character strings into dates.
    dplyr::mutate_at(.vars = vars(contains("date")),
                     function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest(cols = c(laying_date, hatching_date, date_of_capture_52, date_of_capture_57)) %>%
    ## CHANGE COL NAMES TO MATCH STANDARD FORMAT
    dplyr::mutate(PopID = "VEL",
                  BreedingSeason = as.integer(year),
                  Species = Species_codes[which(Species_codes$SpeciesID == 13480), ]$Code,
                  Plot = plot,
                  LocationID = paste0(toupper(stringr::str_sub(plot, end = 1)), nest),
                  BroodID = paste(BreedingSeason, plot, stringr::str_pad(string = nest, width = 2, pad = "0"),
                                  stringr::str_pad(string = lubridate::day(laying_date),
                                                   width = 2,
                                                   pad = "0"),
                                  stringr::str_pad(string = lubridate::month(laying_date),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  FemaleID = female_ring, MaleID = male_ring,
                  ClutchType_observed = NA_character_,
                  LayDate = laying_date, LayDateError = NA_real_,
                  ClutchSize = as.integer(gsub(pattern = "\\+|\\-", replacement = "", clutch_size)), ClutchSizeError = NA,
                  HatchDate = hatching_date, HatchDateError = NA_real_,
                  BroodSize = as.integer(gsub(pattern = "\\+|\\-", replacement = "", number_hatched)), BroodSizeError = NA,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_real_,
                  NumberFledged = as.integer(gsub(pattern = "\\+|\\-", replacement = "", number_fledged)), NumberFledgedError = NA,
                  ##ADD EMPTY EGG COLS. NO EGG DATA.
                  AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                  ExperimentID = treatment)

  ## No columns are excluded except row number and final col.
  ## Final col has some data, but no column name
  ## Dates are dealt with the same way as flycatchers
  ## We only use data on GT and BT. There is a small number of clutches
  ## for other species, but all < 40.
  TIT_data <- readxl::read_excel(paste0(db, "/VEL_PrimaryData_tits.xls"),
                                    col_types = c("skip", "numeric", "text", "text",
                                                  "text", "text", "list",
                                                  "list", "list", "text",
                                                  "text", "text", "text",
                                                  "text", "text", "text", "text",
                                                  "skip")) %>%
    janitor::clean_names() %>%
    dplyr::mutate_at(.vars = vars(contains("date")),
                     function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest(cols = c(laying_date, laying_date_minimum, laying_date_maximum))

  ## Determine laying date for every nest, accounting for error in nests
  TIT_LD_error <- purrr::pmap_dfr(.l = list(TIT_data$laying_date,
                                            TIT_data$laying_date_maximum,
                                            TIT_data$laying_date_minimum),
                                  .f = ~{

                                    if(!is.na(..1)){

                                      return(tibble::tibble(LayDate = ..1,
                                                            LayDateError = NA_real_))

                                    } else {

                                      ## THERE ARE CASES WHERE THERE IS A MAXIMUM BUT NO MINIMUM
                                      ## WE TREAT THESE AS NAs
                                      return(tibble::tibble(LayDate = mean(c(..2, ..3)),
                                                            LayDateError = as.numeric((..2 - ..3)/2)))

                                    }

                                  })

  TIT_data <- TIT_data %>%
    dplyr::mutate(BreedingSeason = as.integer(year),
                  Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = paste0(toupper(stringr::str_sub(plot, end = 1)), nest_box),
                  FemaleID = female_ring, MaleID = NA_character_,
                  ClutchType_observed = NA_character_,
                  LayDate = TIT_LD_error$LayDate,
                  LayDateError = TIT_LD_error$LayDateError,
                  ##FOR NOW, I'M JUST ASSUMING THAT 11+ CLUTCH SIZE
                  ##MEANS CLUTCH 11 THAT WAS ARTIFIICALLY INCREASED
                  ##THEREFORE, I JUST REMOVE THE + AND ADD THAT IT WAS AN EXPERIMENTAL NEST
                  ##NEED TO CHECK WITH MILOS
                  ClutchSize = as.integer(gsub(pattern = "\\+|\\-|\\?", replacement = "", clutch_size)),
                  ClutchSizeError = NA_real_,
                  ##DO THE SAME FOR BROOD SIZE AND NUMBER FLEDGED
                  HatchDate = as.Date(NA), HatchDateError = NA_real_,
                  BroodSize = as.integer(gsub(pattern = "\\+|\\-|\\?", replacement = "", number_hatched)),
                  BroodSizeError = NA_real_,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_real_,
                  NumberFledged = as.integer(gsub(pattern = "\\+|\\-|\\?", replacement = "", number_fledged)),
                  NumberFledgedError = NA_real_,
                  ##ADD EMPTY EGG DATA COLS.
                  AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                  ExperimentID = experiment,
                  ## Estimate broodID last because it requires us to estimate LayDate first
                  BroodID = paste(year, plot, stringr::str_pad(string = nest_box,
                                                         width = 3, pad = "0"),
                                  stringr::str_pad(string = lubridate::day(LayDate),
                                                                   width = 2,
                                                                   pad = "0"),
                                  stringr::str_pad(string = lubridate::month(LayDate),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  Habitat = dplyr::case_when(.$habitat == "oak" ~ "deciduous",
                                             .$habitat == "spruce" ~ "evergreen"))

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data <- create_brood_VEL(FICALB_data, TIT_data) %>%
    dplyr::filter(Species %in% species)

  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data <- dplyr::bind_rows(create_capture_VEL_FICALB(FICALB_data),
                                   create_capture_VEL_TIT(TIT_data)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Species %in% species)

  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_VEL(Capture_data) %>%
    dplyr::filter(Species %in% species)

  # LOCATION DATA

  message("Compiling location information...")

  Location_data <- create_location_VEL(db, Brood_data, TIT_data)

  # WRANGLE DATA FOR EXPORT

  ## Combine capture data and brood data to determine avg chick mass and tarsus
  ## Calculate AvgChickMass and AvgTarsus
  avg_measures <- Capture_data %>%
    ## Filter just 13 day old chicks
    dplyr::filter(ChickAge == 13L) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass       = mean(Mass, na.rm = TRUE),
                     NumberChicksMass   = length(stats::na.omit(Mass)),
                     AvgTarsus          = mean(Tarsus, na.rm = TRUE),
                     NumberChicksTarsus = length(stats::na.omit(Tarsus)))

  Brood_data <- Brood_data %>%
    dplyr::left_join(avg_measures, by = "BroodID") %>%
    dplyr::mutate(NumberChicksMass   = na_if(NumberChicksMass, 0),
                  NumberChicksTarsus = na_if(NumberChicksTarsus, 0),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$AvgTarsus) ~ "Oxford")) %>%
    dplyr::select(BroodID:NumberEggs, AvgChickMass:NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)

  Capture_data <- Capture_data %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime,
                  ObserverID, LocationID, CapturePopID:Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge)

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  # EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_VEL.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_VEL.csv"), row.names = F)

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

#' Create brood data table for Velky Kosir
#'
#' Create brood data table in standard format for flycatcher and tit data from Velky
#' Kosir.
#' @param FICALB_data Data frame. Flycatcher data from Velky Kosir.
#' @param TIT_data Data frame. Tit data from Velky Kosir.
#'
#' @return A data frame.

create_brood_VEL <- function(FICALB_data, TIT_data) {

  FICALB_broods <- FICALB_data %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayDate) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(BroodID, PopID, BreedingSeason,
                  Species, Plot, LocationID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate:ExperimentID)

  TIT_broods <- TIT_data %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayDate) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(BroodID, PopID, BreedingSeason,
                  Species, Plot, LocationID, FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate:ExperimentID)

  return(dplyr::bind_rows(FICALB_broods, TIT_broods))

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  NumberEggs <- AvgChickMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

}

#' Create capture data table for flycatchers in Velky Kosir.
#'
#' Create a capture data table in standard format for only flycatchers in Velky Kosir.
#' Tit data is created with separate function \code{\link{create_capture_VEL_TIT}}.
#' @param FICALB_data Data frame. Flycatcher data from Velky Kosir.
#'
#' @return A data frame.

create_capture_VEL_FICALB <- function(FICALB_data) {

  ## First create a table for flycatcher chick captures on the nest
  FICALB_chicks <- FICALB_data %>%
    dplyr::select(BreedingSeason, Species, Plot, BroodID, LocationID, LayDate, ClutchSize, HatchDate, x1_young_ring:x8y_wing) %>%
    reshape2::melt(measure.vars = c("x1_young_ring", "x2_young_ring",
                                    "x3_young_ring", "x4_young_ring",
                                    "x5_young_ring", "x6_young_ring",
                                    "x7_young_ring", "x8_young_ring"),
                   value.name = "IndvID", variable.name = "ChickNr") %>%
    dplyr::filter(!is.na(IndvID))

  ## Make progress bar. Needs to be twice as long as rows because chicks are
  ## captured at 6 and 13 days.
  pb <- dplyr::progress_estimated(n = nrow(FICALB_chicks)*2)

  FICALB_chicks <- FICALB_chicks %>%
    dplyr::group_by(ChickNr) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ChickNr = substr(ChickNr, 1, 2)) %>%
    dplyr::mutate(data = purrr::pmap(.l = list(ChickNr, data),
                                     .f = ~{

                                       output <- ..2 %>%
                                         dplyr::select(BreedingSeason:HatchDate, IndvID, contains(..1)) %>%
                                         reshape2::melt(id.vars = c(1:9, 11, 13), value.name = "Mass") %>%
                                         ## Rename tarsus and wing to remove name of chick
                                         dplyr::rename_at(.vars = vars(contains("tarsus")), .funs = ~{"Tarsus"}) %>%
                                         dplyr::rename_at(.vars = vars(contains("wing")), .funs = ~{"WingLength"}) %>%
                                         ## Add chick age based on the variable name
                                         dplyr::mutate(ChickAge = as.integer(stringr::str_sub(variable, 9, -1)),
                                                       ## Capture date is hatchdate + chick age
                                                       ## If no hatch date is known then use laying date + clutch size + incubation + chick age
                                                       ## We assume incubation of 15 days, but need to check with Milos.
                                                       ## If there is no hatch date OR laying date then just use NA
                                                       CaptureDate = purrr::pmap(.l = list(LayDate, ClutchSize, HatchDate, ChickAge),
                                                                                 .f = ~{

                                                                                   pb$print()$tick()

                                                                                   if(!is.na(..3)){

                                                                                     return(..3 + lubridate::days(..4))

                                                                                   } else if(!is.na(..1)) {

                                                                                     return(..1 + lubridate::days(..2 + 15 + ..4))

                                                                                   } else {

                                                                                     return(NA)

                                                                                   }

                                                                                 }),
                                                       CaptureTime = NA_character_, CapturePopID = "VEL", CapturePlot = Plot,
                                                       ## All chick records were 6 or 13 days, so all are listed as EURING age 1
                                                       ReleasePopID = "VEL", ReleasePlot = Plot, Age_observed = 1L, Age_calculated = NA_integer_,
                                                       #Convert tarsus to Svennson's alternative
                                                       Tarsus = convert_tarsus(Tarsus, method = "Oxford")) %>%
                                         #dplyr::filter(!all(is.na(Tarsus) & is.na(WingLength) & is.na(Mass))) %>%
                                         tidyr::unnest(CaptureDate) %>%
                                         dplyr::select(IndvID, Species, BreedingSeason, LocationID, CaptureDate, CaptureTime, CapturePopID, CapturePlot,
                                                       ReleasePopID, ReleasePlot, Mass, Tarsus, WingLength, Age_observed,
                                                       Age_calculated, ChickAge, BroodID)

                                       ## When the chick is 6 days old, then tarsus and wing length are not used
                                       ## They were only collected at 13 days old
                                       output[output$ChickAge == 6, ]$Tarsus <- NA
                                       output[output$ChickAge == 6, ]$WingLength <- NA

                                       return(output)

                                     })) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(ObserverID = NA_character_,
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$Tarsus) ~ "Oxford")) %>%
    dplyr::select(-ChickNr) %>%
    #For records at 13 days, if there was no mass/tarsus we assume that it was not caught at 13 days
    dplyr::filter(ChickAge == 6 | (ChickAge == 13 & (!is.na(Tarsus) | !is.na(WingLength) | !is.na(Mass))))

  #Then create capture info for every adult.
  FICALB_adults <- FICALB_data %>%
    dplyr::select(BreedingSeason, Species, Plot, LocationID, LayDate, BroodID, LayDate, FemaleID, date_of_capture_52, tarsus_53:wing_55,
                  MaleID, date_of_capture_57, age:wing_61) %>%
    reshape2::melt(measure.vars = c("FemaleID", "MaleID"), value.name = "IndvID", variable.name = "Sex") %>%
    dplyr::filter(!is.na(IndvID))

  FICALB_adults <- FICALB_adults %>%
    ## Give individuals a sex, we will use this in our Individual_data table
    dplyr::mutate(Sex = stringr::str_sub(Sex, 0, 1),
                  Tarsus = purrr::pmap_dbl(.l = list(Sex, tarsus_53, tarsus_59),
                                           .f = ~{if(..1 == "F") ..2 else ..3}),
                  WingLength = purrr::pmap_dbl(.l = list(Sex, wing_55, wing_61),
                                               .f = ~{if(..1 == "F") ..2 else ..3}),
                  Mass = purrr::pmap_dbl(.l = list(Sex, mass_54, mass_60),
                                         .f = ~{if(..1 == "F") ..2 else ..3}),
                  ##Determine age of males based on 'age' column
                  Age_observed = purrr::pmap_int(.l = list(Sex, age),
                                                 .f = ~{

                                                   if(..1 == "M"){

                                                     return(dplyr::case_when(..2 == "old" ~ 6L,
                                                                             ..2 == "young" ~ 5L))

                                                   } else {

                                                     return(NA_integer_)

                                                   }

                                                 }),
                  CapturePopID = "VEL", ReleasePopID = "VEL",
                  CapturePlot = Plot, ReleasePlot = Plot,
                  ObserverID = NA_character_,
                  CaptureDate = purrr::pmap(.l = list(Sex, date_of_capture_52, date_of_capture_57),
                                            .f = ~{

                                              if(..1 == "F"){

                                                return(..2)

                                              } else {

                                                return(..3)

                                              }

                                            })) %>%
    tidyr::unnest(cols = c(CaptureDate)) %>%
    dplyr::mutate(OriginalTarsusMethod = dplyr::case_when(!is.na(.$Tarsus) ~ "Oxford")) %>%
    dplyr::select(Species, BreedingSeason, LocationID, BroodID,
                  Sex:CaptureDate)

  FICALB_alldat <- dplyr::bind_rows(FICALB_chicks, FICALB_adults) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason, showpb = TRUE)

  return(FICALB_alldat)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  x1_young_ring <- x8y_wing <- ChickNr <- data <- FemaleID <- date_of_capture_52 <- tarsus_53 <- NULL
  wing_55 <- MaleID <- date_of_capture_57 <- age <- wing_61 <- tarsus_59 <- mass_54 <- mass_60 <- NULL

}

#' Create capture data table for tits in Velky Kosir.
#'
#' Create a capture data table in standard format for great and blue tits in Velky Kosir.
#' Tit data is created with separate function \code{\link{create_capture_VEL_FICALB}}.
#' @param TIT_data Data frame. Tit data from Velky Kosir.
#'
#' @return A data frame.

create_capture_VEL_TIT    <- function(TIT_data) {

  ## There is no chick info for tits
  ## There is only data on females.
  ## Assume that an individual was caught at the start of incubation.
  TIT_capture <- TIT_data %>%
    dplyr::filter(!is.na(FemaleID)) %>%
    dplyr::mutate(Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  ## Make the capture date the date that incubation would start (laying date + clutch size)
                  CaptureDate = LayDate + ClutchSize,
                  IndvID = FemaleID,
                  CapturePopID = PopID, CapturePlot = Plot,
                  ReleasePopID = PopID, ReleasePlot = Plot,
                  ## There is no explicit info about age.
                  ## They must all be adults, so just give them all EURING 4
                  ## "Hatched before this calendar year"
                  Age_observed = 4L,
                  ObserverID = NA_character_,
                  LocationID = paste(Plot, nest_box, sep = "_"),
                  OriginalTarsusMethod = NA_character_,
                  Sex = "F") %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, ObserverID, LocationID, CapturePopID:ReleasePlot,
                  Age_observed, Age_calculated, Sex)

  return(TIT_capture)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  FemaleID <- LayDate <- ClutchSize <- NULL

}

#' Create individual data table for Velky Kosir.
#'
#' Create individual data table in standard format for both flycatcher and tit
#' data from Velky Kosir.
#' @param Capture_data Data frame. Combined data from
#'   \code{\link{create_capture_VEL_FICALB}} and
#'   \code{\link{create_capture_VEL_TIT}}.
#'
#' @return A data frame.

create_individual_VEL     <- function(Capture_data){

  pb <- dplyr::progress_estimated(n = length(unique(Capture_data$IndvID)) * 2)

  Indvidual_data <- Capture_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = unique(stats::na.omit(Species)),
                     PopID = "VEL",
                     RingSeason = first(BreedingSeason),
                     RingAge = dplyr::case_when(first(Age_observed) == 1 ~ "chick",
                                                first(Age_observed) != 1 ~ "adult",
                                                is.na(first(Age_observed)) ~ "adult"),
                     Sex = purrr::map_chr(.x = list(unique(Sex)),
                                          .f = ~{

                                            pb$print()$tick()

                                            if(all(c("F", "M") %in% ..1)){

                                              return("C")

                                            } else if("F" %in% ..1){

                                              return("F")

                                            } else if("M" %in% ..1){

                                              return("M")

                                            } else if(is.na(..1)){

                                              return(NA_character_)

                                            }

                                          }),
                     FirstBroodID = first(BroodID)) %>%
    dplyr::mutate(BroodIDLaid = purrr::pmap_chr(.l = list(RingAge, FirstBroodID),
                                               .f = ~{

                                                 pb$print()$tick()

                                                 if(is.na(..1) | (..1 == "adult")){

                                                   return(NA)

                                                 } else {

                                                   return(..2)

                                                 }

                                               }),
                     BroodIDFledged = BroodIDLaid) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex) %>%
    dplyr::ungroup()

  return(Indvidual_data)

}

#' Create location data table for Velky Kosir.
#'
#' Create location data table in standard format for all nest boxes.
#' @param Brood_data Data frame. Output of \code{\link{create_brood_VEL}}
#' @param TIT_data Data frame. Data frame. Tit data from Velky Kosir. This is
#'   needed to include habitat type information.
#'
#' @return A data frame.

create_location_VEL       <- function(db, Brood_data, TIT_data){

  location_data_excel <- readxl::read_excel(paste0(db, "/VEL_PrimaryData_locations.xls"),
                                      col_types = c("text")) %>%
    janitor::clean_names() %>%
    #There is date time info in here. I assume this is date of measurement of XYZ
    #Not relevant for Start/EndSeason
    dplyr::rename(LocationID = budka,
                  Coordinates = sirka_delka_hddd_mm_mmm,
                  Altitude = nadmorska_vyska) %>%
    #Separate coordinates into lat and long
    #Currently in degrees, minutes, seconds. Needs to be converted to decimal degrees.
    dplyr::mutate(Latitude = as.numeric(stringr::str_sub(Coordinates, start = 2, end = 3)) + as.numeric(stringr::str_sub(Coordinates, start = 5, end = 10))/60,
                  Longitude = as.numeric(stringr::str_sub(Coordinates, start = 13, end = 14)) + as.numeric(stringr::str_sub(Coordinates, start = 16, end = 21))/60,
                  NestboxID = LocationID, LocationType = "NB", PopID = "VEL", StartSeason = 1997L, EndSeason = NA_integer_) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join(TIT_data %>% dplyr::group_by(LocationID) %>% dplyr::summarise(Habitat = first(Habitat)), by = "LocationID") %>%
    dplyr::select(LocationID, NestboxID, LocationType, PopID, Latitude, Longitude, StartSeason, EndSeason, Habitat)

  #Some nest locations were not recorded in the Location data excel file. We still include these locations
  #but they will have no lat/long info
  location_data_nocoords <- tibble::tibble(LocationID = as.character(stats::na.omit(unique(Brood_data$LocationID))),
                                           NestboxID = LocationID,
                                           LocationType = "NB",
                                           PopID = "VEL",
                                           Latitude = NA_real_,
                                           Longitude = NA_real_,
                                           StartSeason = 1997L, EndSeason = NA_integer_) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join(TIT_data %>% dplyr::group_by(LocationID) %>% dplyr::summarise(Habitat = first(Habitat)), by = "LocationID") %>%
    ## Exclude locations that were already in the location data excel file.
    dplyr::filter(!LocationID %in% location_data_excel$LocationID)

  return(dplyr::bind_rows(location_data_excel, location_data_nocoords))

}
