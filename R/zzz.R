quality_check_description_pdf <- "

\\section{Introduction}

Welcome to the SPI-Birds quality check report. This report shows the results of a number of standard data quality checks that can be
used on any data that have been created following the \\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{SPI-Birds Standard Protocol}.

\\subsection{How to use this report}

All checks either look for 'potential errors' (values that we believe are impossible e.g. negative values) or 'warnings' (values that we believe are unlikely).
Checks are run on the four tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data.
These checks generally work on individual rows (but see the list of checks below).
When a 'warning' or 'potential error' is identified a line will be included in the report with information on the type of check that was violated
and the \\emph{row number} of the corresponding record. The \\emph{row number} refers to the column \\emph{Row} in the corresponding
table output in the standard format. It does \\textbf{not} refer to row numbers in the primary data. In addition, during the process of the quality
check two new columns ('warning' and 'error') will be added to each of the four data tables to allow potentially spurious records
to be easily identified.

\\subsection{Verification of flagged records}

Some of the records flagged as a 'warning' or 'potential error' are likely to be uncommon but true observations. We don't want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an 'approve-listing' procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of this document. We hope that this will make the quality check reports more useful for data owners and users.

\\subsection{Types of checks}

The checks include:
\\begin{itemize}
  \\item \\textbf{Check for missing data}. Identify any empty columns (i.e. where all records are NA) and return a 'warning'.
  It is possible for empty columns to occur and this check will simply flag these empty columns so they can easily be identified by users.
  \\item \\textbf{Check of data format}. Identify any columns where the format of the data is not as expected (e.g. date, integer) and return a 'potential error'.
  All columns (even empty columns) should be of an expected class. When this does not occur, it is an indiciation of problems in the underlying pipelines.
  \\item \\textbf{Check for discrepancies in clutch size/brood size/fledgling numbers}. We assume that clutch size >= brood size >= number fledged. Identify any rows where this assumption is not met.
  Where a brood has not been experimentally manipulated a 'potential error' will be returned. Where a brood has been experimentally manipulated a 'warning' is returned, as the discrepancy may be explained by the experimental procedure.
  \\item \\textbf{Check for discrepancies in unexpected lay/hatch/fledge dates}. We assume that lay date < hatch date < fledge date. Identify any rows where this assumption is not met and return a 'potential error'.
  \\item \\textbf{Check for unusual clutch/brood/fledgling numbers}. Identify any rows where clutch size, brood size, or number fledged are larger than expected and return a 'warning'. Expected values will differ between species.
  \\item \\textbf{Check for impossible clutch/brood/fledgling numbers}. Identify any rows where clutch size, brood size, or number fledged are negative and return a 'potential error'.
  \\item \\textbf{Check for unexpected mass/tarsus values}. In Capture data, identify any rows where mass or tarsus are larger or smaller than expected for
  a given species. 'warning' values for adults are currently drawn from the data validation values used at the NIOO.
  'warning' values for chicks are the 95\\% confidence interval of a chick growth curve (logistic model) applied to data from Hoge Veluwe.
  \\item \\textbf{Check for multi-species broods}. Currently, we identify any rows where the species of the male and female parent differ and return a 'potential error'.
  In the future, we will also check for discrepancies between the species of parents and chicks and also within a brood.
  \\item \\textbf{Check for discrepancies between brood size and capture records}. Identify any rows where the number of chicks captured at a brood in Capture data is different to the brood size listed in Brood data.
  Where the number of chick captures in a brood is less than the brood size we return a 'warning'. This may occur if not all chicks in a brood are ringed.
  Where the number of chick captures in a brood is more than the brood size we return
  \\item \\textbf{Check that brood and individual ID are unique}. Identify any rows where the IndvID column in Individual data or the BroodID column in Brood data are
  duplicated and return a 'potential error'. These identity variables should be unique within a population.
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
	{\\large Produced by: SPI-Birds team \\\\(Antica Culina, Liam D. Bailey, Chlo\\'e R. Nater, \\\\Stefan J.G. Vriend, Zuzana Zajkov\\'a \\& Marcel E. Visser)} \\\\
	\\vfill
\\end{titlepage}"

quality_check_description_html <- "



"

############################################################################

#' Parameter documentation for all pipelines
#'
#'@param db Location of database file.
#'@param species Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. If blank will return all major species.
#'@param pop The three-letter code of population as listed in the \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. For data owners with multiple populations (e.g. NIOO, UAN) where a single
#'  pipeline is used for many populations this argument is used to extract data from
#'  individual populations. For other pipelines that contain only one population
#'  this argument can be ignored.
#'@param path Location where output csv files will be saved.
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

#' Return documentation for data checks
#'
#' @return
#' A list of:
#' \item{CheckList}{A summary dataframe of check warnings and errors.}
#' \item{WarningRows}{A vector of rows with warnings.}
#' \item{ErrorRows}{A vector of rows with errors.}
#' \item{Warnings}{A list of row-by-row warnings.}
#' \item{Errors}{A list of row-by-row errors.}
#' @name checks_return
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
                         "Age_observed_new",
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
                         "ChickNr", "ObservedSex", "GeneticSex",
                         "Anro", "AvgChickMass_capture", "AvgTarsus_capture",
                         "BirdStatus", "Brood_RingNr", "CaptureDateNestling",
                         "CaptureTimeNestling", "CaptureAgeNestling",
                         "Chicks", "Dead", "Delfh", "DestBoxNumber",
                         "Destination", "Dist", "FetLut", "Haapa", "Halku",
                         "Havno", "Head", "Ika", "Ip", "Jalat", "Kataja",
                         "Kk", "Kkk", "Klo", "Koe", "Koir", "Koiras",
                         "Koivu", "Kokor", "Korel", "Kork",
                         "Kpv", "Ktar", "Kukor", "Kunta", "Kunto",
                         "Kuor", "Kurel", "Kuusi", "Laji", "LastRingNumber",
                         "Latitude_Lambert", "Lent", "Leppa", "Leve", "Lihas",
                         "Lisa", "Lkoe", "LocationID_join", "LocationName",
                         "Longitude_Lambert", "Makor", "Manty", "Marel", "MassNestling",
                         "Mety", "Onil", "Opak", "Opys", "OrigBoxNumber",
                         "Paikka", "Paino", "Pun", "Puulaji", "Puut", "Pv",
                         "Rasik", "Rasika", "Reference", "Reng", "Rtapa", "Sarja",
                         "Siipi", "Sp", "Suku", "Sulsat", "Tark", "Tark2",
                         "Tila", "Tot", "Totpain", "Totrel", "Tsyy", "Tunnus",
                         "Toumi", "Value", "Vari", "Varpaat", "Vart",
                         "Vjalka", "Vkas", "Vlent", "Vnil", "Vpak", "Vpys", "Vuos", "Wb",
                         "WingLengthNestling", "X", "Y", "abr_station",
                         "chick_mass_cutoffs", "ChickAgeNestling", "Mihin",
                         "Mista", "Mkk", "Mkuor", "Mpv", "Mpy", "Mtar",
                         "Mulu", "Naaras", "Nuro", "Ojalka", "Okas", "Olent",
                         "Pajut", "Pesa", "Pihlaja", "Pitu", "Pohja",
                         "Poik", "Pontlaji", "Psulka", "Ptapa", "Tuomi",
                         "la", "lo", "ni", "nichoir", "ChickAge2", "Coordinates",
                         "ExperimentCode", "age_offspring", "bird_id",
                         "budka", "capture_method", "clutch_no", "clutch_size_error",
                         "date_ringed", "fledge_date", "fledge_date_error",
                         "fledge_number", "fledge_number_error", "hatch_date_error",
                         "hatch_number", "hatch_number_error", "lay_date_error",
                         "mass_g", "measures_taken_by", "nadmorska_vyska",
                         "nest_id", "nest_location", "nest_of_origin_id",
                         "nestbox_no", "nestbox_number", "physical_manipulation_present_at_time_of_release",
                         "physical_manipulation_present_at_time_of_catching",
                         "physiological_manipulation", "rearing_nest_id",
                         "ring_number", "sirka_delka_hddd_mm_mmm", "social_female_bird_id",
                         "social_male_bird_id", "status", "tarsus_length_mm", "time_capture",
                         "unique_nest_id", "wing_lengh_mm", "x1st_egg_lay_date"))
