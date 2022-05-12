description_errors_pdf <- "

\\section{Introduction}

The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.

\\subsection{How do the data quality checks work?}

The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as \\emph{potential errors} or \\emph{warnings}, which are stored into two separate reports.

\\begin{enumerate}
\\item The first report (i.e., 'Potential Errors'; \\textbf{this document}) contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column Row in the corresponding data table in the standard format and does not refer to the row number in the primary data.
\\item The second report (i.e., 'Warnings and Verified Records') contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don't want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an 'approve-listing' procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.
\\end{enumerate}

In addition to these two reports, two columns (Warning and Error) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.
"

description_warnings_pdf <- "

\\section{Introduction}

The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.

\\subsection{How do the data quality checks work?}

The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as \\emph{potential errors} or \\emph{warnings}, which are stored into two separate reports.

\\begin{enumerate}
\\item The first report (i.e., 'Potential Errors') contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column Row in the corresponding data table in the standard format and does not refer to the row number in the primary data.
\\item The second report (i.e., 'Warnings and Verified Records'; \\textbf{this document}) contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don't want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an 'approve-listing' procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.
\\end{enumerate}

In addition to these two reports, two columns (Warning and Error) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.
"

check_descriptions_pdf <- "
\\newpage

\\subsection{Check descriptions}

Below is a summary of all standard data quality checks included in the SPI-Birds quality check procedure. A detailed description checks can be found \\href{https://github.com/SPI-Birds/documentation/blob/master/quality_check/SPI-Birds_quality-check-protocol_v1.1.pdf}{\\underline{here}}.

Brood checks:
  \\begin{itemize}
\\item \\textbf{B1}. Compare clutch size and brood size. Clutch size is expected to be larger or equal to brood size. Broods that do not meet this expectation are flagged as a warning (in case they were experimentally manipulated) or a potential error (in case they were not experimentally manipulated).
\\item \\textbf{B2}. Compare brood size and number of fledglings. Brood size is expected to be larger or equal to number of fledglings. Broods that do not meet this expectation are flagged as a warning (in case they were experimentally manipulated) or a potential error (in case they were not experimentally manipulated).
\\item \\textbf{B3}. Compare lay date and hatch date. Lay date is expected to be earlier than hatch date. Broods that do not meet this expectation are flagged as a potential error.
\\item \\textbf{B4}. Compare hatch date and fledge date. Hatch date is expected to be earlier than fledge date. Broods that do not meet this expectation are flagged as a potential error.
\\item \\textbf{B5a-c}. Compare clutch size (a), brood size (b), and number of fledglings (c) against reference values. Reference values are generated by population- and species-specific data if the number of observations is sufficiently large (n >= 100). Records are flagged as a potential error if they are negative or larger than 2 times the 99th percentile.
\\item \\textbf{B5d}. Compare lay date against reference values. Reference values are generated by population- and species-specific data if the number of observations is sufficiently large (n >= 100). Records are flagged as a potential error if they are earlier than January 1st or later than December 31st of the same year.
\\item \\textbf{B6}. Compare brood size and the number of chicks recorded in Individual data. These numbers are expected to be equal. Records where brood size is larger than the number of recorded chicks are flagged as a warning, as some chicks might have died before ringing. In experimentally manipulated broods, brood size might be smaller than the number of recorded chicks. If so, the record is flagged as a warning. In non-manipulated broods, brood size should never be smaller than the number of chicks recorded in Individual data. If so, the record is flagged as potential error.
\\item \\textbf{B7}. Check that brood identities (BroodID) are unique within populations. BroodIDs are not expected to be unique among populations. Records with non-unique BroodIDs are flagged as a potential error.
\\item \\textbf{B8}. Check that the order of clutch types per breeding female per breeding season is correct. Replacement and second clutches can occur in any order but never before first clutches. First clutches that are not listed first are flagged as a potential error.
\\item \\textbf{B9}. Compare species of mother and father. The species of the parents are expected to be the same in the majority of broods. Common, biologically possible multi-species broods are flagged as a warning. Other combinations are flagged as a potential error.
\\item \\textbf{B10}. Compare species of parents and the brood itself. The species of the parents and their brood are expected to be the same. Broods with a combination of species for which brood fostering is known to exist are flagged as a warning. Other combinations of species are flagged as a potential error.
\\item \\textbf{B11}. Compare species of brood and the chicks in the brood. The species of the brood and the chicks are expected to be the same. Broods with a combination of species for which brood fostering is known to exist are flagged as a warning. Other combinations of species are flagged as a potential error.
\\item \\textbf{B12}. Check that the sex of the mother is female. Broods where mothers are listed as male are flagged as a potential error.
\\item \\textbf{B13}. Check that the sex of the father is male. Broods where fathers are listed as female are flagged as a potential error.
\\item \\textbf{B14}. Check that all individuals recorded as parents of a brood appear at least once in Capture data. Broods with parents missing from Capture data are flagged as a potential error.
\\item \\textbf{B15}. Check that all nest locations appear in Location data. Missing locations are flagged as a potential error.
\\end{itemize}

Capture checks:
  \\begin{itemize}
\\item \\textbf{C1a-b}. Compare mass (a) and tarsus (b) against reference values. Reference values for mass in adults and tarsus in both adults and chicks are generated by population- and species-specific data if the number of observations is sufficiently large (n >= 100). Reference values for mass in chicks are either generated for each age using a logistic growth model or, if that fails, per age if the number of observations is sufficiently large (n >= 100). Records are flagged as a potential error if they are negative or larger than 2 times the 99th percentile.
\\item \\textbf{C2}. Check that the chick age values (in number of days since hatching) are within the expected duration of the nesting period. Expected number of days are between 0 and 30. Values outside this range are flagged as a potential error. Impossible chick age may be caused by problems with hatch date.
\\item \\textbf{C3}. Check that the adults captured on a nest during the breeding season are listed as the parents of that nest in Brood data. Adults that are not marked as the parents of the nest are flagged as a warning. A record might be flagged for instance when the capture location of the nest encompasses more than just the nest.
\\item \\textbf{C4}. Check that the observed age of chronologically ordered captures of an individual is correct. The age recorded in an individual's subsequent capture is expected to be equal when the capture was in the same year or larger when the capture was in a later year. Records of an individual caught as an adult before records of the same individual caught as a chick are flagged as a potential error. Other records where the observed age of a capture is larger than the age of a subsequent capture are flagged as a warning.
\\item \\textbf{C5}. Check that all individuals in Capture data have a record in Individual data. Missing individuals should never occur because Individual data is usually a direct product of Capture data. If it does occur, it is an indication of problems in the underlying pipeline. Missing individuals are flagged as a potential error.
\\item \\textbf{C6}. Check that all capture locations appear in Location data. Missing locations are flagged as a potential error.
\\end{itemize}

Individual checks:
  \\begin{itemize}
\\item \\textbf{I1}. Check that individual identities (IndvID) are unique within populations. IndvIDs are not expected to be unique among populations. Records with non-unique IndvIDs are flagged as a potential error.
\\item \\textbf{I2}. Check that the brood identities for an individual (BroodIDLaid, BroodIDFledged) match the correct nest in Capture data. Chicks caught on a nest that are not associated with corresponding brood identities are flagged as a potential error.
\\item \\textbf{I3}. Check for the uncertainty in the sex of an individual. Individuals that have been recorded as both male and female in Capture data are marked as conflicted ('C') by the pipeline and flagged as a potential error.
\\item \\textbf{I4}. Check for the uncertainty in the species of an individual. Individuals that have been recorded as different species in Capture data are marked as conflicted ('CCCCCC') by the pipeline and flagged as a potential error.
\\item \\textbf{I5}. Check that individuals in Individual data appear at least once in Capture data. Missing individuals should never occur because Individual data is usually a direct product of Capture data. If it does occur, it is an indication of problems in the underlying pipeline. Missing individuals are flagged as a potential error.
\\end{itemize}

Location checks:
  \\begin{itemize}
\\item \\textbf{L1}. Check that the coordinates of locations are close to the centre point of the study site. Records that are 20 km or farther from the centre point are flagged as a potential error. This check also produces a map of all location records.
\\end{itemize}"

titlepage_errors_pdf <- "\\renewcommand{\\familydefault}{\\sfdefault}
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
{	\\Huge SPI-Birds \\\\ Standard Data Quality Checks\\\\}
{ \\LARGE Potential Errors \\\\}
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
	Produced by:\\\\
	\\vspace{4mm}
	{\\Large SPI-Birds team} \\\\
	\\vspace{4mm}
	Stefan J.G. Vriend, Liam D. Bailey, Chris Tyson, \\\\Antica Culina, \\& Marcel E. Visser \\\\
	\\vfill
\\end{titlepage}"

titlepage_warnings_pdf <- "\\renewcommand{\\familydefault}{\\sfdefault}
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
{	\\Huge SPI-Birds \\\\ Standard Data Quality Checks\\\\}
{ \\LARGE Warnings and Verified Records \\\\}
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
	Produced by:\\\\
	\\vspace{4mm}
	{\\Large SPI-Birds team} \\\\
	\\vspace{4mm}
	Stefan J.G. Vriend, Liam D. Bailey, Chris Tyson, \\\\Antica Culina, \\& Marcel E. Visser \\\\
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
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}. If blank will return all major species.
#'@param site The three-letter code of the study site as listed in the \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{standard
#'  protocol}. For data owners with multiple study sites (e.g., NIOO, UAN) where a single
#'  pipeline is used for many study sites, this argument is used to extract data from
#'  individual study sites. For other pipelines that contain only one study site
#'  this argument can be ignored.
#'@param path Location where output csv files will be saved.
#'@param output_type Should the pipeline generate .csv files ('csv') or R objects ('R').
#'
#'@name pipeline_params
NULL


#' Parameter documentation for brood data checks
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#'
#'@name checks_brood_params
NULL

#' Parameter documentation for capture data checks
#'
#' @param Capture_data Data frame. Capture data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#'
#'@name checks_capture_params
NULL

#' Parameter documentation for individual data checks
#'
#' @param Individual_data Data frame. Individual data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#'
#'@name checks_individual_params
NULL

#' Parameter documentation for location data checks
#'
#' @param Location_data Data frame. Location data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
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
                         "unique_nest_id", "wing_lengh_mm", "x1st_egg_lay_date",
                         "M", "fit", "x", "lower", "upper", "Warning_max", "N",
                         "CTcal", "Sex_calculated", "Sex_genetic", "Warning_max",
                         "Stage", "Logis", "BroodSize_observed", "NumberFledged_observed",
                         "ClutchSize_observed", "HatchDate_observed", "FledgeDate_observed",
                         "LayDate_observed", "IndvSpecies", "SpeciesComp",
                         "OtherSpeciesChicks", "date_time", "Site", "PLACE", "dummy_data", "where",
                         "Distance"))
