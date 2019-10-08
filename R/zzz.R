quality_check_description_pdf <- "

\\section{Introduction}

Welcome to the SPI-Birds quality check report. This report shows the results of a number of standard data quality tests that are
run on data stored in the format described in the \\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{SPI-Birds Standard Protocol}.

\\subsection{How to use this report}

All tests either look for 'warnings' (values that we consider to be uncommon or unusual) and 'potential errors' (values that we consider to be impossible).
Tests are run on each of the four tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data.
These tests generally work on individual rows (but see the list of tests below).
When a 'warning' or 'potential error' is identified a line will be included in the report will information on the type of test that was violated
and the \\emph{row number} of the corresponding record. The \\emph{row number} refers to the column \\emph{Row} in the corresponding
table output in the standard format. It does \\textbf{not} refer to row numbers in the primary data.

\\subsection{Types of tests}

The tests include:
\\begin{itemize}
  \\item Test for missing data. Identify any columns where no data is available (i.e. only NA records).
  \\item Test of data format. Identify any columns where the format of the data (e.g. date, integer) is not as expected.
  \\item Test for uexpected clutch/brood/fledgling number differences. We assume that clutch size >= brood size >= number fledged. Identify any columns where this assumption is not met.
  \\item Test for uexpected lay/hatch/fledge date differences. We assume that lay date < hatch date < fledge date. Identify any rows where this assumption is not met.
  \\item Test for unexpected clutch/brood/fledgling number values. Identify any rows where clutch size, brood size, or number fledged are larger than expected.
  \\item Test for unexpected mass/tarsus values. In Capture data, identify any rows where mass or tarsus are larger or smaller than expected for
  a given species. 'warning' and 'potential error' values are currently the 95% and 99.5% quantiles of mass and tarsus from data collected at Hoge Veluwe for each species.
\\end{itemize}"

quality_check_titlepage_pdf <- "\\renewcommand{\\familydefault}{\\sfdefault}
\\begin{titlepage}
	\\centering % Center everything on the title page
	\\scshape % Use small caps for all text on the title page
	\\vspace*{1.5\\baselineskip} % White space at the top of the page
% ===================
%	Title Section
% ===================

	\\rule{13cm}{1.6pt}\\vspace*{-\\baselineskip}\\vspace*{2pt} % Thick horizontal rule
	\\rule{13cm}{0.4pt} % Thin horizontal rule

		\\vspace{0.75\\baselineskip} % Whitespace above the title
% ========== Title ===============
	{	\\Huge SPI-Birds Quality Check\\\\}
% ======================================
		\\vspace{0.75\\baselineskip} % Whitespace below the title
	\\rule{13cm}{0.4pt}\\vspace*{-\\baselineskip}\\vspace{3.2pt} % Thin horizontal rule
	\\rule{13cm}{1.6pt} % Thick horizontal rule

		\\vspace{0.75\\baselineskip} % Whitespace after the title block

% =================
%	Version number
% =================

    \\vspace{3mm}

    \\today

    \\vspace{1.25\\baselineskip}

% =================
%	Information
% =================
	{\\large Produced by: SPI-Birds team (Antica Culina, Liam D. Bailey, Stefan Vriend \\& Marcel E. Visser)} \\\\
	\\vfill
\\end{titlepage}"

quality_check_description_html <- "



"


##############################################################################

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
