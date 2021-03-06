% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual_check.R
\name{check_individuals_captures}
\alias{check_individuals_captures}
\title{Check that all individuals in Individual_data appear in Capture_data}
\usage{
check_individuals_captures(Individual_data, Capture_data, approved_list)
}
\arguments{
\item{Individual_data}{Data frame. Individual data output from pipeline.}

\item{Capture_data}{Data frame. Capture data output from pipeline.}

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
Check that all individuals recorded in Individual_data appear at least once in Capture_data. This should never occur, because Individual_data is usually a direct product of Capture_data (i.e., all unique individuals from Capture_data). If we find missing individuals, we need to check the pipeline code.
}
\details{
Check ID: I6.
}
