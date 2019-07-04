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
  FICHYP_data <- readxl::read_excel(paste0(db, "/Velky_Kosir_flycatchers.xlsx"),
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
    tidyr::unnest()

  TIT_data    <- readxl::read_excel(paste0(db, "/Velky_Kosir_tits.xls")) %>%
    janitor::clean_names()

  ##############
  # BROOD DATA #
  ##############

  Brood_data <- create_brood_VEL(FICHYP_data, TIT_data)

}


create_brood_VEL <- function(FICALB_data, TIT_data) {

  FICALB_broods <- FICALB_data %>%
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
                  ##ADD MORPHO COLUMNS TO BIND WITH TIT DATA
                  ##WILL ADD THIS INFO LATER WHEN WE HAVE CAPTURE DATA
                  AvgEggMass = NA, NrEggs = NA,
                  AvgChickMass = NA, NrChicksMass = NA,
                  AvgTarsus = NA, NrChicksTarsus = NA,
                  ExperimentID = treatment) %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(SampleYear:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  TIT_broods <- TIT_data %>%
    dplyr::mutate(SampleYear = year,
                  Species = dplyr::case_when(.$species == "blue tit" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code,
                                             .$species == "great tit" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code),
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = nest_box, BroodID = paste(year, nest_box, lubridate::day(laying_date), lubridate::month(laying_date), sep = "_"),
                  FemaleID = female_ring, MaleID = NA,
                  ClutchType_observed = NA,
                  ## NEED TO ACCOUNT FOR CASES WHERE LAYINGDATE HAS ERROR.
                  ## FOR NOW I DON'T DO THIS
                  LayingDate = laying_date, LayingDateError = NA,
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
                  ExperimentID = experiment) %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::select(SampleYear:ClutchType_observed, ClutchType_calc,
                  LayingDate:ExperimentID)

  return(dplyr::bind_rows(FICALB_broods, TIT_broods))

}
