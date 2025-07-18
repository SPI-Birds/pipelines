% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_PEW.R
\name{format_PEW}
\alias{format_PEW}
\title{Construct standard format data from Peerdsbos West, Belgium (PEW)}
\usage{
format_PEW(
  db = choose_directory(),
  species = NULL,
  pop = NULL,
  path = ".",
  output_type = "R"
)
}
\arguments{
\item{db}{Location of database file.}

\item{species}{Species of interest. The 6 letter codes of all the species of
interest as listed in the
\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
protocol}. If blank will return all major species.}

\item{pop}{The three-letter code of population as listed in the \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{standard
protocol}. For data owners with multiple populations (e.g. NIOO, UAN) where a single
pipeline is used for many populations this argument is used to extract data from
individual populations. For other pipelines that contain only one population
this argument can be ignored.}

\item{path}{Location where output csv files will be saved.}

\item{output_type}{Should the pipeline generate .csv files ('csv') or R objects ('R').}
}
\value{
4 data tables in the standard format (version 1.1.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
}
\description{
A pipeline to produce the standard format for bird study population
at the Peerdsbos West, Belgium, administered by Wendt Müller
(previously by Arne Iserbyt).
}
\details{
This section provides details on data management choices that are unique to
this data. For a general description of the standard format please see see
\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.

\strong{IndvID}: Generally, the unique ID is an 8-character digit number reflecting
the metal ring number. In 4 cases, this information was not provided (NA), therefore
those observations were removed from the dataset (correspond to chicks ringed in
the year 2017 in the nestboxes 97, 98, 102, kk12).
Several individuals were not identified by the unique ring number, only by
the information reflecting the sex, nestbox and year (i.e. "Unknown_M2019NestK97").
Those IndvID were unified as "unringed" and are included in the Brood_data,
but not included in the Individual_data.

\strong{ClutchSize_observed} Where clutch size has ?, recorded value is used
as ClutchSize_observed. ClutchSize_min/max is defined by No of chicks D3.

\strong{NumberFledged} Information provided by the data owner: The number of
fledged nestlings were not consistently monitored. However, nestlings were
ringed at day 14, so very close to fledging. In almost all cases, the number
of ringed chicks will be the same as number of fledged chicks.

\strong{BroodSize_observed} We used the information provided in the raw data
in the column indicating the number of chicks at day 3.

\strong{HatchDate_observed} For several broods the hatch date was not provided.
When neither hatch date nor capture date were provided, we input fake date
("breeding season-06-01", 1st of June of the corresponding breeding season).

\strong{CaptureDate} For several chicks, where the capture date was indicated
as "D14", the capture date was calculated as hatch date + 14 days.
For several adult individuals, where the capture date was missing,
the fake date was input ("breeding season, 04-01", 1st of April of the corresponding
breeding season).

\strong{Latitude, Longitude} The exact coordinates of the nestboxes are not available.
Data owner provided a map of the location, which can be georeferenced in case of
interest or necessity of the data user.

\strong{NOTES} Until we recive the final data from the data owner, we are aware
of the following issues in the final standardized data:

There are two records in the Brood_data for the BroodID "2017_109", with different number of chicks and dates
(currently we do not know which one is the correct one).

There are two records in the Brood_data for the BroodID "2015_24", the correct one is the one where
FemaleID is 13619319 and MaleID is 11714676.

There are two records in the Brood_data for the BroodID "2016_60", the correct one is the one where
FemaleID is 13617052 and MaleID should be unknow.
Data owner note: Female 12706296 in box 60 in 2016 is erroneous and can be removed from the data.
Female 13617052 was the only female in box 60 in 2016.
She layed a complete clutch, which never hatched.
The male was never caught and remains unknown.

There are two records in the Brood_data for the BroodID "2017_49", the correct one is the one where
FemaleID 13619466 is and MaleID is 13617031.
Data owner: The only couple in 49 in 2017 was 13619466 (female) and  13617031 (male).
partner 12706106 should be removed (or replaced by the true female 13619466).

Based on previous comment of data owner, the capture of the IndvID 12706296
in box 60 in 2016 is erroneous and can be removed from the data.

In the Capture_data, there are 6 erroneous records related to the nest 109 from 2017
(related to the Brood_data double record for the BroodID "2017_109"):
- 1 extra record for the male 13617008
- 2 extra records for the female 13619461
- 1 extra record for each chick 14156383, 14156384, 14156385
}
