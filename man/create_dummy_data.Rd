% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummy_data.R
\name{create_dummy_data}
\alias{create_dummy_data}
\title{Create quality check dummy data}
\usage{
create_dummy_data()
}
\value{
List of 4 dataframes:
\item{Brood_data}{Dummy brood data.}
\item{Capture_data}{Dummy capture data.}
\item{Individual_data}{Dummy individual data.}
\item{Location_data}{Dummy location data.}
}
\description{
Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}. In each dataframe, rows are created to test single checks from \code{\link{quality_check}}. A CheckID column is added to each dataframe to mark which rows serve to test each check. See a detailed description of the dummy data in \code{\link{dummy_data}}.
}
