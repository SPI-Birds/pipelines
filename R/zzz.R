description_errors_pdf <- "

\\section{Introduction}

The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.

\\subsection{How do the data quality checks work?}

The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as \\emph{potential errors} or \\emph{warnings}, which are stored into two separate reports.

\\begin{enumerate}
\\item The first report (i.e., 'Potential Errors'; \\textbf{this document}) contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column \\texttt{row} in the corresponding data table in the standard format and does not refer to the row number in the primary data.
\\item The second report (i.e., 'Warnings and Verified Records') contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don't want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an 'approve-listing' procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.
\\end{enumerate}

In addition to these two reports, two columns (\\texttt{rowWarning} and \\texttt{rowError}) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.
"

descriptions_errors_html <- c(
  "# Introduction",
  "The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.",
  "",
  "## How do the data quality checks work?",
  "",
  "The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as <em>potential errors</em> or <em>warnings</em>, which are stored into two separate reports.",
  "",
  "<ol>",
  '<li> The first report (i.e., "Potential Errors"; <strong>this document</strong>) contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column <TT>row</TT> in the corresponding data table in the standard format and does not refer to the row number in the primary data.</li>',
  '<li> The second report (i.e., "Warnings and Verified Records" contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don&#39;t want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an "approve-listing" procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.</li>',
  "</ol>",
  "",
  "In addition to these two reports, two columns (<TT>rowWarning</TT> and <TT>rowError</TT>) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.",
  ""
)

description_warnings_pdf <- "

\\section{Introduction}

The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.

\\subsection{How do the data quality checks work?}

The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as \\emph{potential errors} or \\emph{warnings}, which are stored into two separate reports.

\\begin{enumerate}
\\item The first report (i.e., 'Potential Errors') contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column \\texttt{row} in the corresponding data table in the standard format and does not refer to the row number in the primary data.
\\item The second report (i.e., 'Warnings and Verified Records'; \\textbf{this document}) contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don't want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an 'approve-listing' procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.
\\end{enumerate}

In addition to these two reports, two columns (\\texttt{rowWarning} and \\texttt{rowError}) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.
"

descriptions_warnings_html <- c(
  "# Introduction",
  "The SPI-Birds standard data quality checks are part of the SPI-Birds standard workflow. The aim of the data quality checks is to increase the integrity of the data by highlighting unlikely and impossible values in data that have been created following the SPI-Birds Standard Protocol.",
  "",
  "## How do the data quality checks work?",
  "",
  "The standard data quality check procedure involves a number of checks that work on individual rows of each of the tables described in the SPI-Birds Standard Protocol: Brood data, Capture data, Individual data, and Location data. These quality checks identify suspicious data and flag them as <em>potential errors</em> or <em>warnings</em>, which are stored into two separate reports.",
  "",
  "<ol>",
  '<li> The first report (i.e., "Potential Errors") contains a list of potential errors, which are values that are considered as impossible (e.g., negative values for clutch size). Whenever a record is flagged as a potential error, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. This row number refers to the column <TT>row</TT> in the corresponding data table in the standard format and does not refer to the row number in the primary data.</li>',
  '<li> The second report (i.e., "Warnings and Verified Records"; <strong>this document</strong>) contains a list of warnings, which are values that are considered possible but unlikely (e.g., a brood of individuals of more than one species). Similar to the potential errors, whenever a record is flagged as a warning, a line is added to this report with information on the type of check that was violated and the row number of the corresponding record. Besides a list of warnings, this report contains a list of verified records. Some of the records flagged as a warning or potential error are likely to be uncommon but true observations. We don`t want these same values to be flagged each time a new quality check is conducted. To overcome this, we have implemented an "approve-listing" procedure that will prevent true records, that have been verified by the data owner, from appearing in future quality check reports. These records are listed at the end of the second document. We hope that this will make the quality check reports more useful for data owners and users.</li>',
  "</ol>",
  "",
  "In addition to these two reports, two columns (<TT>rowWarning</TT> and <TT>rowError</TT>) have been added to each of the data tables in the standard format to allow data users to easily identify and filter out potentially spurious records.",
  ""
)

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
\\item \\textbf{B5d-f}. Compare lay date (d), hatch date (e), and fledge date (f) against reference values. Reference values are generated by population- and species-specific data if the number of observations is sufficiently large (n >= 100). Records are flagged as a potential error if they are earlier than January 1st or later than December 31st of the same year.
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

check_descriptions_html <- c(
  "## Check descriptions",
  "",
  'Below is a summary of all standard data quality checks included in the SPI-Birds quality check procedure. A detailed description checks can be found <a href="https://github.com/SPI-Birds/documentation/blob/master/quality_check/SPI-Birds_quality-check-protocol_v1.1.pdf">here</a>.',
  "",
  "Brood checks:",
  "<ul>",
  "<li><strong>B1</strong>. Compare clutch size and brood size. Clutch size is expected to be larger or equal to brood size. Broods that do not meet this expectation are flagged as a warning (in case they were experimentally manipulated) or a potential error (in case they were not experimentally manipulated).</li>",
  "<li><strong>B2</strong>. Compare brood size and number of fledglings. Brood size is expected to be larger or equal to number of fledglings. Broods that do not meet this expectation are flagged as a warning (in case they were experimentally manipulated) or a potential error (in case they were not experimentally manipulated).</li>",
  "<li><strong>B3</strong>. Compare lay date and hatch date. Lay date is expected to be earlier than hatch date. Broods that do not meet this expectation are flagged as a potential error.</li>",
  "<li><strong>B4</strong>. Compare hatch date and fledge date. Hatch date is expected to be earlier than fledge date. Broods that do not meet this expectation are flagged as a potential error.</li>",
  "<li><strong>B5a-c</strong>. Compare clutch size (a), brood size (b), and number of fledglings (c) against reference values. Reference values are generated by population- and species-specific data if the number of observations is sufficiently large (n &ge; 100). Records are flagged as a potential error if they are negative or larger than 2 times the 99th percentile.</li>",
  "<li><strong>B5d-f</strong>. Compare lay date (d), hatch date (e), and fledge date (f) against reference values. Reference values are generated by population- and species-specific data if the number of observations is sufficiently large (n &ge; 100). Records are flagged as a potential error if they are earlier than January 1st or later than December 31st of the same year.</li>",
  "<li><strong>B6</strong>. Compare brood size and the number of chicks recorded in Individual data. These numbers are expected to be equal. Records where brood size is larger than the number of recorded chicks are flagged as a warning, as some chicks might have died before ringing. In experimentally manipulated broods, brood size might be smaller than the number of recorded chicks. If so, the record is flagged as a warning. In non-manipulated broods, brood size should never be smaller than the number of chicks recorded in Individual data. If so, the record is flagged as potential error.</li>",
  "<li><strong>B7</strong>. Check that brood identities (BroodID) are unique within populations. BroodIDs are not expected to be unique among populations. Records with non-unique BroodIDs are flagged as a potential error.</li>",
  "<li><strong>B8</strong>. Check that the order of clutch types per breeding female per breeding season is correct. Replacement and second clutches can occur in any order but never before first clutches. First clutches that are not listed first are flagged as a potential error.</li>",
  "<li><strong>B9</strong>. Compare species of mother and father. The species of the parents are expected to be the same in the majority of broods. Common, biologically possible multi-species broods are flagged as a warning. Other combinations are flagged as a potential error.</li>",
  "<li><strong>B10</strong>. Compare species of parents and the brood itself. The species of the parents and their brood are expected to be the same. Broods with a combination of species for which brood fostering is known to exist are flagged as a warning. Other combinations of species are flagged as a potential error.</li>",
  "<li><strong>B11</strong>. Compare species of brood and the chicks in the brood. The species of the brood and the chicks are expected to be the same. Broods with a combination of species for which brood fostering is known to exist are flagged as a warning. Other combinations of species are flagged as a potential error.</li>",
  "<li><strong>B12</strong>. Check that the sex of the mother is female. Broods where mothers are listed as male are flagged as a potential error.</li>",
  "<li><strong>B13</strong>. Check that the sex of the father is male. Broods where fathers are listed as female are flagged as a potential error.</li>",
  "<li><strong>B14</strong>. Check that all individuals recorded as parents of a brood appear at least once in Capture data. Broods with parents missing from Capture data are flagged as a potential error.</li>",
  "<li><strong>B15</strong>. Check that all nest locations appear in Location data. Missing locations are flagged as a potential error.</li>",
  "</ul>",
  "",
  "Capture checks:",
  "<ul>",
  "<li><strong>C1a-b</strong>. Compare mass (a) and tarsus (b) against reference values. Reference values for mass in adults and tarsus in both adults and chicks are generated by population- and species-specific data if the number of observations is sufficiently large (n &ge; 100). Reference values for mass in chicks are either generated for each age using a logistic growth model or, if that fails, per age if the number of observations is sufficiently large (n &ge; 100). Records are flagged as a potential error if they are negative or larger than 2 times the 99th percentile.</li>",
  "<li><strong>C2</strong>. Check that the chick age values (in number of days since hatching) are within the expected duration of the nesting period. Expected number of days are between 0 and 30. Values outside this range are flagged as a potential error. Impossible chick age may be caused by problems with hatch date.</li>",
  "<li><strong>C3</strong>. Check that the adults captured on a nest during the breeding season are listed as the parents of that nest in Brood data. Adults that are not marked as the parents of the nest are flagged as a warning. A record might be flagged for instance when the capture location of the nest encompasses more than just the nest.</li>",
  "<li><strong>C4</strong>. Check that the observed age of chronologically ordered captures of an individual is correct. The age recorded in an individual&#39;s subsequent capture is expected to be equal when the capture was in the same year or larger when the capture was in a later year. Records of an individual caught as an adult before records of the same individual caught as a chick are flagged as a potential error. Other records where the observed age of a capture is larger than the age of a subsequent capture are flagged as a warning.</li>",
  "<li><strong>C5</strong>. Check that all individuals in Capture data have a record in Individual data. Missing individuals should never occur because Individual data is usually a direct product of Capture data. If it does occur, it is an indication of problems in the underlying pipeline. Missing individuals are flagged as a potential error.</li>",
  "<li><strong>C6</strong>. Check that all capture locations appear in Location data. Missing locations are flagged as a potential error.</li>",
  "</ul>",
  "",
  "Individual checks:",
  "<ul>",
  "<li><strong>I1</strong>. Check that individual identities (IndvID) are unique within populations. IndvIDs are not expected to be unique among populations. Records with non-unique IndvIDs are flagged as a potential error.</li>",
  "<li><strong>I2</strong>. Check that the brood identities for an individual (BroodIDLaid, BroodIDFledged) match the correct nest in Capture data. Chicks caught on a nest that are not associated with corresponding brood identities are flagged as a potential error.</li>",
  '<li><strong>I3</strong>. Check for the uncertainty in the sex of an individual. Individuals that have been recorded as both male and female in Capture data are marked as conflicted ("C") by the pipeline and flagged as a potential error.</li>',
  '<li><strong>I4</strong>. Check for the uncertainty in the species of an individual. Individuals that have been recorded as different species in Capture data are marked as conflicted ("CCCCCC") by the pipeline and flagged as a potential error.</li>',
  "<li><strong>I5</strong>. Check that individuals in Individual data appear at least once in Capture data. Missing individuals should never occur because Individual data is usually a direct product of Capture data. If it does occur, it is an indication of problems in the underlying pipeline. Missing individuals are flagged as a potential error.</li>",
  "</ul>",
  "",
  "Location checks:",
  "<ul>",
  "<li><strong>L1</strong>. Check that the coordinates of locations are close to the centre point of the study site. Records that are 20 km or farther from the centre point are flagged as a potential error. This check also produces a map of all location records.</li>",
  "</ul>",
  ""
)

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


############################################################################

#' Parameter documentation for all pipelines
#'
#' @param db Location of database file.
#' @param species Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. If blank will return all major species.
#' @param pop The three-letter code of population as listed in the \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}. For data owners with multiple populations (e.g. NIOO, UAN) where a single
#'  pipeline is used for many populations this argument is used to extract data from
#'  individual populations. For other pipelines that contain only one population
#'  this argument can be ignored.
#' @param path Location where output csv files will be saved.
#' @param output_type Should the pipeline generate .csv files ('csv') or R objects ('R').
#'
#' @name pipeline_params
NULL


#' Parameter documentation for brood data checks
#'
#' @param Brood_data Data frame. Brood data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#' @param skip Character. Identifiers of the individual quality checks (CheckID) that should be skipped.
#'
#' @name checks_brood_params
NULL

#' Parameter documentation for capture data checks
#'
#' @param Capture_data Data frame. Capture data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#' @param skip Character. Identifiers of the individual quality checks (CheckID) that should be skipped.
#'
#' @name checks_capture_params
NULL

#' Parameter documentation for individual data checks
#'
#' @param Individual_data Data frame. Individual data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#' @param skip Character. Identifiers of the individual quality checks (CheckID) that should be skipped.
#'
#' @name checks_individual_params
NULL

#' Parameter documentation for location data checks
#'
#' @param Location_data Data frame. Location data output from pipeline.
#' @param approved_list List object. List of approved records from brood_approved_list.csv,
#' capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.
#' @param output Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).
#' @param skip Character. Identifiers of the individual quality checks (CheckID) that should be skipped.
#'
#' @name checks_location_params
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

utils::globalVariables(c(
  ".", "GT_dist_gg",
  # Column names used in test_min_max_columns NSE
  "ClutchSize", "BroodSize", "NumberFledged",
  "ClutchSize_observed", "ClutchSize_min", "ClutchSize_max",
  "BroodSize_min", "BroodSize_max",
  "NumberFledged_observed", "NumberFledged_min", "NumberFledged_max",
  "observedClutchSize", "minimumClutchSize", "maximumClutchSize",
  "observedBroodSize", "minimumBroodSize", "maximumBroodSize",
  "observedNumberFledged", "minimumNumberFledged", "maximumNumberFledged"
))
