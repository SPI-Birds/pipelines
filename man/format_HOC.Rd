% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_HOC.R
\name{format_HOC}
\alias{format_HOC}
\title{Construct standard format for data from Hochstadt, Germany.}
\usage{
format_HOC(
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
4 data tables in the standard format (version 1.0.0). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
}
\description{
A pipeline to produce the standard format for the hole nesting bird
populations in Hochstadt, Germany administered by Max Plank Institute
for Ornithology, Seewiesen (Michaela Hau).
}
\details{
This section provides details on data management choices that are unique to
this data. For a general description of the standard protocl please see
\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.

\strong{AvgEggMass:} Clutch mass is recorded in many cases; however, these measurements are
taken on or after the start of incubation. As egg mass can change throughout the period of
incubation we have not used these data.

\strong{Plot:} There is no distinct plot information in Hochstadt. Plot is left blank.

\strong{LocationID:} Captures can occur on nest boxes (e.g., with trap, while brooding, at feeder)
or with mistnets. The location of all non-nestbox trapping is located next to a known nestbox.
Therefore, we simply use nestbox as the LocationID for all captures, even when the capture didn't
occur within the nestbox itself. Therefore, Location data only includes location information
for nestbox sites.

\strong{Age_observed:} All captures listed as 'nestling' are given a EURING code of 1 (i.e. unable to fly).
Captures listed as 'adult' can be either '1st year' or 'adult'. We treat '1st year' as birds known to
be in their first reproductive season (i.e. 2nd year of life; EURING 5) while 'adult' are birds known to be after
hatched before this season, but exact age unknown (i.e. EURING 4). Some cases are listed as '1st year?'.
These are treated the same as '1st year'.

\strong{ChickAge:} Chick age is sometimes stored with uncertainty (e.g. 14/15). In all these cases we
take the lower age.

\strong{ExperimentID:} Manipulation of individuals is recorded with each capture. This includes
hormonal injections and attaching backpacks. We treat any brood as having been experimented on
if any type of manipulation was recorded on any individual associated with a given brood.
\strong{Note:} at this moment, ExperimentID is simply recorded as TRUE/FALSE while we try to
categorise all experiments.
}
