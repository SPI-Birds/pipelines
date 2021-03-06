% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brood_check.R
\name{compare_broodsize_chicknumber}
\alias{compare_broodsize_chicknumber}
\title{Compare brood size with number of chicks captured}
\usage{
compare_broodsize_chicknumber(Brood_data, Individual_data, approved_list)
}
\arguments{
\item{Brood_data}{Data frame. Brood data output from pipeline.}

\item{Individual_data}{Data frame. Individual data output from pipeline.}

\item{approved_list}{List object. List of approved records from brood_approved_list.csv,
capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv}
}
\value{
A list of:
\item{CheckList}{A summary dataframe of check warnings and errors.}
\item{WarningRows}{A vector of rows with warnings.}
\item{ErrorRows}{A vector of rows with errors.}
\item{Warnings}{A list of row-by-row warnings.}
\item{Errors}{A list of row-by-row errors.}
}
\description{
Compare BroodSize in Brood_data with the number of chicks captured in Capture_data. We expect these numbers to be equal. Records where BroodSize is larger than the number of chicks captured results in a warning, because chicks might have died before ringing and measuring. Records where BroodSize is smaller than the number of chicks captured results in an error, because this should not be possible.
}
\details{
Check ID: B7.
}
