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

############################################################################

utils::globalVariables(c(".", "AvgChickTarsus", "AvgChickMass", "AvgTarsus", "PopID",
                         "CapturePlot", "ReleasePlot", "CapturePopID", "ReleasePopID",
                         "Weight", "Wing", "Mass",
                         "BroodID", "RingSeries", "Time", "LayingDate",
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
                         "LayingDateError", "BroodSize", "BroodSizeError",
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
                         "py", "section", "total_egg_weight", "treatment"))
