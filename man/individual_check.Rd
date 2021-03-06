% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual_check.R
\name{individual_check}
\alias{individual_check}
\title{Perform quality checks on individual data}
\usage{
individual_check(
  Individual_data,
  Capture_data,
  Location_data,
  check_format = TRUE,
  approved_list
)
}
\arguments{
\item{Individual_data}{Data frame. Individual data output from pipeline.}

\item{Capture_data}{Data frame. Capture data output from pipeline.}

\item{Location_data}{Data frame. Location data output from pipeline.}

\item{check_format}{\code{TRUE} or \code{FALSE}. If \code{TRUE}, the check on variable format (i.e. \code{\link{check_format_individual}}) is included in the quality check. Default: \code{TRUE}.}

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
A wrapper that runs all single checks related to \code{Individual_data}.
}
\details{
The following individual data checks are performed:
\itemize{
\item \strong{I1}: Check if the formats of each column in \code{Individual_data} match with the standard format using \code{\link{check_format_individual}}.
\item \strong{I2}: Check if the IDs of individuals are unique using \code{\link{check_unique_IndvID}}.
\item \strong{I3}: Check if all chicks have BroodID using \code{\link{check_BroodID_chicks}}.
\item \strong{I4}: Check if individuals have no conflicting sex using \code{\link{check_conflicting_sex}}.
\item \strong{I5}: Check if individuals have no conflicting species using \code{\link{check_conflicting_species}}.
\item \strong{I6}: Check if individuals in Individual_data appear in Capture_data  using \code{\link{check_individuals_captures}}.
}
}
