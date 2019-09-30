find_box <- function(string, position = 1){

  if(is.na(string) | position == (nchar(string) + 1)){

    return(NA_character_)

  }

  split_string <- strsplit(string, "")[[1]]

  if(is.na(suppressWarnings(as.numeric(split_string[position])))){

    return(find_box(string = string, position = position + 1))

  } else {

    return(paste(split_string[position:nchar(string)], collapse = ""))

  }

}

############################################################################

#' Parameter documentation for all pipelines
#'
#'@param db Location of database file.
#'@param species Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. If blank will return all major species.
#'@param pop The three-letter code of population as listed in the \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. For data owners with multiple populations (e.g. NIOO, UAN) where a single
#'  pipeline is used for many populations this argument is used to extract data from
#'  individual populations. For other pipelines that contain only one population
#'  this argument can be ignored.
#'@param path Location where output csv files will be saved.
#'@param debug For internal use when editing pipelines. If TRUE, pipeline
#'  generates a summary of pipeline data. This includes: a) Histogram of
#'  continuous variables with mean/SD b) unique values of all categorical
#'  variables.
#'@param output_type Should the pipeline generate .csv files ('csv') or R objects ('R').
#'
#'@name pipeline_params
NULL


#' Parameter documentation for brood data checks
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#'
#'@name checks_brood_params
NULL

#' Parameter documentation for capture data checks
#'
#' @param Capture_data Data frame. Capture data output from pipeline.
#'
#'@name checks_capture_params
NULL

#' Parameter documentation for individual data checks
#'
#' @param Individual_data Data frame. Individual data output from pipeline.
#'
#'@name checks_individual_params
NULL

#' Parameter documentation for location data checks
#'
#' @param Location_data Data frame. Location data output from pipeline.
#'
#'@name checks_location_params
NULL

############################################################################

utils::globalVariables(c(".", "AvgChickTarsus", "AvgChickMass", "AvgTarsus", "PopID",
                         "CapturePlot", "ReleasePlot", "CapturePopID", "ReleasePopID",
                         "Weight", "Wing", "Mass",
                         "BroodID", "RingSeries", "Time", "LayDate",
                         "ClutchSize", "HatchDate", "variable",
                         "IndvID", "Species", "BreedingSeason",
                         "Age_observed", "Sex", "CaptureDate", "CaptureTime",
                         "Year", "Age", "BroodIDLaid", "BroodIDFledged",
                         "FirstYr", "RingAge", "Age_calculated",
                         "RingSeason", "Age_obsv", "FirstBroodID",
                         "LocationID", "FirstYear", "Aukko", "Malli",
                         "NestboxID", "LocationType", "Latitude",
                         "Longitude", "StartSeason", "EndSeason",
                         "Habitat", "GBPL", "Y_deg", "X_deg",
                         "YEARFIRST", "YEARLAST", "HasCoords",
                         "LeftTarsusLength", "Day", "Month", "CatchDate",
                         "Pop_name", "year", "site", "Plot", "box_number",
                         "March1Date", "first_egg_lay_date",
                         "egg_weight", "number_eggs_weighed", "final_clutch_size",
                         "weigh_date", "actual_hatch_date",
                         "actual_male_trapping_date", "actual_female_trapping_date",
                         "male_id", "female_id", "actual_pullus_ringing_date",
                         "number_fledged", "Species_codes", "BroodId",
                         "Ring", "JulianDate", "ChickAge",
                         "TrapingMethod", "Box", "NumberFledgedError",
                         "AvgEggMass", "OriginalTarsusMethod",
                         "ExperimentID", "Name", "ID", "Area", "AreaGroup",
                         "UserPlaceName", "AreaID", "SpeciesID",
                         "Description", "GeneticBroodID", "Sexe",
                         "RingYear", "RingNumber", "Individual",
                         "CaptureLocation", "species_pb",
                         "BroodIDRinged", "ReleaseLocation", "CaptureType",
                         "CaptureID", "Wing_Length", "MinAge", "BroodSpecies",
                         "BroodLocationID", "BroodLocation", "BroodYear", "RingNumberFemale",
                         "RingNumberMale", "LayDate", "LayDateDeviation",
                         "NumberHatched", "NumberHatchedDeviation",
                         "FledgeDate", "NumberFledged", "NumberFledgedDeviation",
                         "BroodSizeError", "NumberFledgedError",
                         "LayDateError", "BroodSize", "BroodSizeError",
                         "ClutchSizeError",
                         "NumberFledgedError", "SampleYear", "Mar1",
                         "Female_ring", "Male_ring", "ClutchType_observed",
                         "ClutchType_calc", "ClutchType_calculated",
                         "CluthcSizeError", "HatchDateError", "ExperimentID",
                         "Location", "StartYear", "EndYear",
                         "Row", "Ld", "Cs", "Hd", "Hs", "Fs",
                         "FId", "MId", "NestId", "HabitatOfRinging",
                         "YCoord", "XCoord", "Code", "NrEgg", "NrChickMass",
                         "FAge", "MAge", "Chick1Id", "Chick13Id",
                         "NN", "SOORT", "GB", "PL", "LD", "JAE",
                         "AE", "AEN", "NP", "PD", "PU", "LO",
                         "TY", "RM", "RW", "AW", "WD", "WU", "ME",
                         "GN", "GT", "GG", "CON", "SA", "NNN1", "NNN2",
                         "NNN3", "NNBI", "NNTRI", "VERL",
                         "coorx", "coory", "comm", "year", "gbpl",
                         "RN", "NRN", "KLR", "NKLR", "TAGTY",
                         "TAG", "NTAG", "GS", "VD", "LT", "VW",
                         "VLL", "GEW", "UUR", "TA", "BL", "BH",
                         "DMVL", "BLOED", "RUI", "COMM", "RECNUM",
                         "SPLOT", "TANEW", "TEEK", "klr_old",
                         "soort", "rn", "sex", "gbj", "cbj",
                         "gb", "vd", "nrn", "pit", "pitdate",
                         "klr1", "klr1date",
                         "klr2", "klr2date",
                         "klr3", "klr3date",
                         "klr4", "klr4date",
                         "molsex", "vll_med", "vll_n",
                         "cta_med", "cta_n", "ctanew_med",
                         "ctanew_n", "tarsus_med",
                         "tarsus_n", "tarsus_ty",
                         "%T>%", "Age_calc", "COORX", "COORY",
                         "FemaleID", "FledgeDateError", "GT_dist_gg",
                         "MaleID", "NestBoxType", "NrChickTarsus", "NumberChicksMass",
                         "NumberChicksTarsus", "NumberEggs", "SPLIT", "Tarsus",
                         "WingLength", "cgj", "chick_ids", "clutch_size",
                         "code", "data", "experiment", "father", "female_ring",
                         "group", "hatch_date", "hatching_date", "lat", "latitude",
                         "lay_date", "laying_date", "legacy_april_hatch_date",
                         "legacy_average_egg_weight", "legacy_mean_fledge_weight",
                         "long", "longitude", "male_ring", "mean_chick_weight",
                         "mother", "nest", "nest_box", "nestbox", "num_chicks",
                         "num_chicks_ringed", "num_eggs_weighed", "num_fledglings",
                         "number_hatched", "owner", "plot", "pop_names",
                         "py", "section", "total_egg_weight", "treatment",
                         "capture_ref_values_list", "AccuracyOfDate",
                         "Observer", "RingAgeObsv", "ObserverID",
                         "Age_observed_new", "choose.dir",
                         "pop_species_combos", "pop", "species",
                         "total_sp", "TotalEggWeight",
                         "lieu", "nic", "BoxNumber", "an", "date_ponte",
                         "grpo", "date_eclo", "pulecl", "pulenv",
                         "mort", "mbag", "fbag", "pulbag1", "pulbag14",
                         "YoungestCatch", "date_mesure", "heure", "espece",
                         "bague", "aile", "tarsed", "tarseg", "becna",
                         "poids", "obs", "dest", "orig", "TarsusRight",
                         "age_plume", "MeanEggWeight", "NEggsWeighted",
                         "date_of_capture_52", "date_of_capture_57",
                         "laying_date_minimum", "laying_date_maximum",
                         "experience", "explique", "proto",
                         "age", "FoundDead", "ObservedSex",
                         "GeneticSex", "ExperimentDescription1",
                         "ExperimentDescription2", "CaptureMethod",
                         "ChickNr", "ObservedSex", "GeneticSex"))
