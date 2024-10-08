% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_PIL.R
\name{format_PIL}
\alias{format_PIL}
\title{Construct standard format for data from Pilis-Visegrád Mountains, Hungary.}
\usage{
format_PIL(
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
A pipeline to produce the standard format for the hole nesting bird population
in Pilis-Visegrád Mountains, Hungary, administered by the János Török (Eötvös Loránd University).
}
\details{
This section provides details on data management choices that are unique to
this data. For a general description of the standard protocl please see
\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.

\strong{LocationID}: Nestbox numbers are not unique across plots. Therefore, unique locationIDs are
a combination of plot and nestbox number.

\strong{BroodID}: Unique broods are Year, LocationID, LayingDate (days since Mar 31st).

\strong{ExperimentID}: Experiments are listed as affecting number fledglings, clutch size etc.
so all experiments are classed as 'COHORT'. ExperimentID '2' is also classified as 'PHENOLOGY'
as it is said to affect laying date.

\strong{CaptureDate}: Nestlings have a ring date and a nestling measure date column.
When there is only a ring date, the nestlings are captured once (ringing and measurement).
When there is a ring and nestling measure date these are considered two captures.
The ring date is assumed to have no measurements (Mass, Tarsus, WingLength are NA).

\strong{Species}: There are a (small) number of hybrid flycatcher broods, with
male FICHYP and female FICALB. This causes some problems for our classification of
brood and chick species. For broods, we know these are legitimate multi-species
broods, so they should not be flagged as warnings. For chicks, we don't have a species
code for flycatcher hybrids. Currently, all hybrid broods and chicks are removed
(there are only 5 broods and 16 chicks). Species is still included for
adult captures as we know the exact species of each parent. We will need to determine
how we want to deal with these hybrid broods in the future.

\strong{Age_observed}: Age is not explicitly recorded, therefore we classify all
adults as NA age. Individuals in nestling columns are assumed to always be pre-fledging
and are given age 1.

\strong{CaptureAlive, ReleaseAlive}: All individuals are assumed to be captured and released alive.

\strong{Location_data}: All unique locationIDs (plot/nestbox number) are
assumed to be active across the whole study period.

\strong{Mass, Tarsus & WingLength}: There are some character strings in these columns.
These variables are silently coerced to numeric such that all character strings
are coerced to NA.
}
