% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{species_codes}
\alias{species_codes}
\title{Species codes and taxonomy}
\format{
A data frame with 10 rows and 6 variables
\describe{
  \item{id}{Internal species identifier.}
  \item{scientificName}{Scientific name (genus + specific epithet) following the taxonomy of the \href{https://itis.gov}{Integrated Taxonomic Information System (ITIS)}.}
  \item{vernacularName}{English common name.}
  \item{speciesTSN}{Taxonomic serial number (TSN) used by the \href{https://itis.gov}{ITIS}.}
  \item{speciesEURINGCode}{Code used by \href{https://euring.org/}{EURING}. NA for species not included in EURING.}
  \item{Species}{(Formerly used 6-letter species identifier. The first three letters indicate the genus, the last three letters indicate the specific epithet.}
  }
}
\usage{
species_codes
}
\description{
Table on the species included in SPI-Birds pipelines with information on internal codes, external codes (EURING and ITIS TSN), and common and scientific names.
}
\keyword{datasets}
