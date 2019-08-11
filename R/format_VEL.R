#' Construct standard summary for data from Velky Kosir, Czechia.
#'
#' A pipeline to produce a standard output for the nest box population in Velky
#' Kosir, Czechia, administered by Milos Krist.
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
#' \strong{ClutchSize, BroodSize, NumberFledged}: Some records have uncertainty
#' (e.g. 11+). We're unsure how large this uncertainty is. It's currently
#' ignored, but will talk with data owner to incorporate this.
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
                       debug = FALSE,
                       output_type = "csv") {

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
  FICALB_data <- suppressMessages(readxl::read_excel(paste0(db, "/Velky_Kosir_flycatchers.xlsx"),
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
    dplyr::mutate(PopID = "VEL",
                  BreedingSeason = year,
                  Species = Species_codes[which(Species_codes$SpeciesID == 13480), ]$Code,
                  Plot = plot,
                  LocationID = stringr::str_pad(string = nest, width = 2, pad = "0"),
                  BroodID = paste(BreedingSeason, nest, stringr::str_pad(string = lubridate::day(laying_date),
                                                                         width = 2,
                                                                         pad = "0"),
                                  stringr::str_pad(string = lubridate::month(laying_date),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  FemaleID = female_ring, MaleID = male_ring,
                  ClutchType_observed = NA,
                  LayingDate = laying_date, LayingDateError = NA,
                  ClutchSize = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", clutch_size)), ClutchSizeError = NA,
                  HatchDate = hatching_date, HatchDateError = NA,
                  BroodSize = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", number_hatched)), BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledged = as.numeric(gsub(pattern = "\\+|\\-", replacement = "", number_fledged)), NumberFledgedError = NA,
                  ##ADD EMPTY EGG COLS. NO EGG DATA.
                  AvgEggMass = NA, NumberEggs = NA,
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

  ## Determine laying date for every nest, accounting for error in nests
  TIT_LD_error <- purrr::pmap_dfr(.l = list(TIT_data$laying_date,
                                            TIT_data$laying_date_maximum,
                                            TIT_data$laying_date_minimum),
                                  .f = ~{

                                    if(!is.na(..1)){

                                      return(tibble::tibble(LayingDate = ..1,
                                                            LayingDateError = NA))

                                    } else {

                                      ## THERE ARE CASES WHERE THERE IS A MAXIMUM BUT NO MINIMUM
                                      ## WE TREAT THESE AS NAs
                                      return(tibble::tibble(LayingDate = mean(c(..2, ..3)),
                                                            LayingDateError = as.numeric((..2 - ..3)/2)))

                                    }

                                  })

  TIT_data <- TIT_data %>%
    dplyr::mutate(BreedingSeason = year,
                  Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = stringr::str_pad(nest_box, 3, pad = "0"),
                  FemaleID = female_ring, MaleID = NA,
                  ClutchType_observed = NA,
                  LayingDate = TIT_LD_error$LayingDate,
                  LayingDateError = TIT_LD_error$LayingDateError,
                  ##FOR NOW, I'M JUST ASSUMING THAT 11+ CLUTCH SIZE
                  ##MEANS CLUTCH 11 THAT WAS ARTIFIICALLY INCREASED
                  ##THEREFORE, I JUST REMOVE THE + AND ADD THAT IT WAS AN EXPERIMENTAL NEST
                  ##NEED TO CHECK WITH MILOS
                  ClutchSize = as.numeric(gsub(pattern = "\\+|\\-|\\?", replacement = "", clutch_size)),
                  ClutchSizeError = NA,
                  ##DO THE SAME FOR BROOD SIZE AND NUMBER FLEDGED
                  HatchDate = NA, HatchDateError = NA,
                  BroodSize = as.numeric(gsub(pattern = "\\+|\\-|\\?", replacement = "", number_hatched)),
                  BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledged = as.numeric(gsub(pattern = "\\+|\\-|\\?", replacement = "", number_fledged)),
                  NumberFledgedError = NA,
                  ##ADD EMPTY EGG DATA COLS.
                  AvgEggMass = NA, NumberEggs = NA,
                  ExperimentID = experiment,
                  ## Estimate broodID last because it requires us to estimate LayingDate first
                  BroodID = paste(year, nest_box, stringr::str_pad(string = lubridate::day(LayingDate),
                                                                   width = 2,
                                                                   pad = "0"),
                                  stringr::str_pad(string = lubridate::month(LayingDate),
                                                   width = 2,
                                                   pad = "0"), sep = "_"),
                  Habitat = dplyr::case_when(.$habitat == "oak" ~ "deciduous",
                                             .$habitat == "spruce" ~ "evergreen"))

  ##############
  # BROOD DATA #
  ##############

  message("Compiling brood information...")

  Brood_data <- create_brood_VEL(FICALB_data, TIT_data) %>%
    dplyr::filter(Species %in% species)

  ################
  # CAPTURE DATA #
  ################

  message("Compiling capture information...")

  Capture_data <- dplyr::bind_rows(create_capture_VEL_FICALB(FICALB_data),
                                   create_capture_VEL_TIT(TIT_data)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Species %in% species)

  ###################
  # INDIVIDUAL DATA #
  ###################

  message("Compiling individual information...")

  Individual_data <- create_individual_VEL(Capture_data) %>%
    dplyr::filter(Species %in% species)

  #################
  # LOCATION DATA #
  #################

  message("Compiling location information...")

  Location_data <- create_location_VEL(Brood_data, TIT_data)

  ###########################
  # WRANGLE DATA FOR EXPORT #
  ###########################

  ## Combine capture data and brood data to determine avg chick mass and tarsus
  ## Calculate AvgChickMass and AvgTarsus
  avg_measures <- Capture_data %>%
    ## Filter just 13 day old chicks
    dplyr::filter(ChickAge == 13) %>%
    dplyr::group_by(BroodID) %>%
    dplyr::summarise(AvgChickMass       = mean(Mass, na.rm = TRUE),
                     NumberChicksMass    = length(stats::na.omit(Mass)),
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

  #######################
  # CREATE DEBUG OPTION #
  #######################

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "VEL", Brood_data = Brood_data, Capture_data = Capture_data, Indv_data = Individual_data)

  }

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  ###############
  # EXPORT DATA #
  ###############

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
#' @export
create_brood_VEL          <- function(FICALB_data, TIT_data) {

  FICALB_broods <- FICALB_data %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(PopID:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  TIT_broods <- TIT_data %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(BreedingSeason:LocationID, BroodID,
                  FemaleID:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  return(dplyr::bind_rows(FICALB_broods, TIT_broods))

  #Satisfy RCMD Check
  `.` <- AvgEggMass <- BroodID <- NULL
  PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calc <- NULL
  LayingDate <- LayingDateError <- ClutchSize <- ClutchSizeError <- NULL
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
#' @export
create_capture_VEL_FICALB <- function(FICALB_data) {

  ## First create a table for flycatcher chick captures on the nest
  FICALB_chicks <- FICALB_data %>%
    dplyr::select(BreedingSeason, Species, Plot, BroodID, LocationID, LayingDate, ClutchSize, HatchDate, x1_young_ring:x8y_wing) %>%
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
                      dplyr::mutate(ChickAge = as.numeric(stringr::str_sub(variable, 9, -1)),
                                    ## Capture date is hatchdate + chick age
                                    ## If no hatch date is known then use laying date + clutch size + incubation + chick age
                                    ## We assume incubation of 15 days, but need to check with Milos.
                                    ## If there is no hatch date OR laying date then just use NA
                                    CaptureDate = purrr::pmap(.l = list(LayingDate, ClutchSize, HatchDate, ChickAge),
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
                                    CaptureTime = NA, CapturePopID = "VEL", CapturePlot = Plot,
                                    ## All chick records were 6 or 13 days, so all are listed as EURING age 1
                                    ReleasePopID = "VEL", ReleasePlot = Plot, Age_obsv = 1, Age_calculated = NA,
                                    #Convert tarsus to Svennson's alternative
                                    Tarsus = convert_tarsus(Tarsus, method = "Oxford")) %>%
                      tidyr::unnest(CaptureDate) %>%
                      dplyr::select(IndvID, Species, BreedingSeason, LocationID, CaptureDate, CaptureTime, CapturePopID, CapturePlot,
                                    ReleasePopID, ReleasePlot, Mass, Tarsus, WingLength, Age_obsv,
                                    Age_calculated, ChickAge, BroodID)

                    ## When the chick is 6 days old, then tarsus and wing length are not used
                    ## They were only collected at 13 days old
                    output[output$ChickAge == 6, ]$Tarsus <- NA
                    output[output$ChickAge == 6, ]$WingLength <- NA

                    return(output)

                  })) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(ObserverID = NA,
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$Tarsus) ~ "Oxford")) %>%
    dplyr::select(-ChickNr)

  #Then create capture info for every adult.
  FICALB_adults <- FICALB_data %>%
    dplyr::select(BreedingSeason, Species, Plot, LocationID, LayingDate, BroodID, LayingDate, FemaleID, date_of_capture_52, tarsus_53:wing_55,
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
    dplyr::select(Species, BreedingSeason, LocationID, BroodID,
                  IndvID:CaptureDate, Sex)

  FICALB_alldat <- dplyr::bind_rows(FICALB_chicks, FICALB_adults) %>%
    calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate, Year = BreedingSeason, showpb = TRUE)

  return(FICALB_alldat)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_obsv <- NULL
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
#' @export
create_capture_VEL_TIT    <- function(TIT_data) {

  ## There is no chick info for tits
  ## There is only data on females.
  ## Assume that an individual was caught at the start of incubation.
  TIT_capture <- TIT_data %>%
    dplyr::filter(!is.na(FemaleID)) %>%
    dplyr::mutate(Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  ## Make the capture date the date that incubation would start (laying date + clutch size)
                  CaptureDate = LayingDate + ClutchSize,
                  IndvID = FemaleID,
                  CapturePopID = PopID, CapturePlot = Plot,
                  ReleasePopID = PopID, ReleasePlot = Plot,
                  ## There is no explicit info about age.
                  ## They must all be adults, so just give them all EURING 4
                  ## "Hatched before this calendar year"
                  Age_obsv = 4) %>%
    calc_age(ID = IndvID, Age = Age_obsv, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::select(BreedingSeason, IndvID, Species, CaptureDate, CapturePopID:ReleasePlot,
                  Age_obsv, Age_calculated)

  return(TIT_capture)

  #Satisfy RCMD Check
  Species <- IndvID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_obsv <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- ReleasePopID <- Mass <- Tarsus <- NULL
  OriginalTarsusMethod <- WingLength <- Age_calculated <- ChickAge <- NULL
  FemaleID <- LayingDate <- ClutchSize <- NULL

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
#' @export
create_individual_VEL     <- function(Capture_data){

  pb <- dplyr::progress_estimated(n = length(unique(Capture_data$IndvID)) * 2)

  Indvidual_data <- Capture_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = unique(stats::na.omit(Species)),
                     PopID = "VEL",
                     RingSeason = first(BreedingSeason),
                     RingAge = first(Age_obsv),
                     Sex = purrr::map_chr(.x = list(unique(Sex)),
                                          .f = ~{

                                            pb$print()$tick()

                                            if(all(c("F", "M") %in% ..1)){

                                              return("CONFLICTING SEX")

                                            } else if("F" %in% ..1){

                                              return("F")

                                            } else if("M" %in% ..1){

                                              return("M")

                                            } else if(is.na(..1)){

                                              return("U")

                                            }

                                          }),
                     FirstBroodID = first(BroodID)) %>%
    dplyr::mutate(BroodIDLaid = purrr::pmap_chr(.l = list(RingAge, FirstBroodID),
                                               .f = ~{

                                                 pb$print()$tick()

                                                 if(is.na(..1) | (..1 != 1)){

                                                   return(NA)

                                                 } else {

                                                   return(..2)

                                                 }

                                               }),
                     BroodIDRinged = BroodIDLaid) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDRinged, RingSeason, RingAge, Sex) %>%
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
#' @export
create_location_VEL       <- function(Brood_data, TIT_data){

  ## Determine all used LocationIDs in Brood_data. These should be all locations.
  ## Assume that nestboxes are the same for Tits and Flycatchers.
  location_data <- tibble::tibble(LocationID = as.character(stats::na.omit(unique(Brood_data$LocationID))),
                                  NestboxID = LocationID,
                                  LocationType = "NB",
                                  PopID = "VEL",
                                  Latitude = NA,
                                  Longitude = NA,
                                  StartSeason = 1997, EndSeason = NA) %>%
    ## Join in habitat data from TIT_data table
    dplyr::left_join(TIT_data %>% dplyr::group_by(LocationID) %>% dplyr::summarise(Habitat = first(Habitat)), by = "LocationID")

}
