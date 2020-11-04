#' Species EURING codes and letter codes
#'
#' Letter codes to easily identify species (English names)
#' @format A data frame with 10 rows and 4 variables
#' \describe{
#'   \item{SpeciesID}{Species EUring code.}
#'   \item{Code}{Species letter code.}
#'   \item{CommonName}{Common English name.}
#'   \item{BinomialName}{Genus and species name.}
#'   }
#'@name Species_codes
NULL

#'Population names and locations
#'
#'Information and identifying codes for all populations and data owners.
#'@format A data frame with 23 rows and 4 variables
#'\describe{
#'  \item{code}{Three-letter population code.}
#'  \item{name}{Name of population.}
#'  \item{country}{Name of country where population is situated.}
#'  \item{owner}{Letter code for data owner. Note, multiple populations can have
#'  the same data owner.} }
#'@name pop_names
NULL

#'Details for plotting png files of birds
#'
#'@format A data frame with 6 rows and 5 variables
#'\describe{
#'  \item{species}{Six-letter species code.}
#'  \item{label}{English common name of species, used to create labels below .png files.}
#'  \item{scale}{Size of png relative to the size of great tit.}
#'  \item{base_colour}{Colour used to plot data from this species (based on plumage colour).}
#'  \item{top_colour}{Secondary colour used to plot data from this species (based on plumage colour).}}
#'@name bird_png_data
NULL

#'List of reference values for capture data
#'
#'Plausible capture data values that can be used to detect outliers.
#'@format A list with 40 data frames:
#'
#' \itemize{
#' \item{10 species: \code{PARMAJ}, \code{CYACAE}, \code{FICHYP}, \code{SITEUR}, \code{PERATE}, \code{PASMON}, \code{FICALB}, \code{POEPAL}, \code{POECIN}, \code{PHOPHO}}
#' \item{2 stages: \code{Adult}, \code{Chick}}
#' \item{2 variables: \code{Mass}, \code{Tarsus}}
#' }
#'
#' Each species-stage-variable combination has the following reference values:
#'\describe{
#'  \item{Warning_min}{Lower bound of a variable's expected values. A value smaller than this threshold is considered unusual and will result in a warning.}
#'  \item{Warning_max}{Upper bound of a variable's expected values. A value larger than this threshold is considered unusual and will result in a warning.}
#'  \item{Error_min}{Lower bound of a variable's unusual values. A value smaller than this threshold is considered impossible and will result in an error.}
#'  \item{Error_max}{Upper bound of a variable's unusual values. A value larger than this threshold is considered impossible and will result in an error.}
#'  }
#'@name capture_ref_values
NULL

#'Spatial data polygon of great tit distribution.
#'
#'Distribution of great tits used for plotting all populations.
#'@format A spatial data polygon.
#'@name GT_dist_gg
NULL

#'Combination of all PopID and Species from all pipelines.
#'
#'A data frame with each row containing a unique population
#'and species combination.
#'@format A data frame with 60 rows and 2 variables.
#'\describe{
#'    \item{pop}{Three letter population ID.}
#'    \item{species}{Six letter species ID.}
#'}
#'@name pop_species_combos
NULL

#' Quality check dummy data
#'
#' A dummy pipeline output specifically created to test single checks from \code{\link{quality_check}}. See a detailed description of which rows correspond to which checks in 'Details'.
#'
#' @format A list of 4 data frames:
#' \itemize{
#'    \item{Brood_data}
#'    \item{Capture_data}
#'    \item{Individual_data}
#'    \item{Location_data}
#' }
#'
#' @details
#' \strong{Brood data}:
#' \itemize{
#'   \item Row 1 represents a non-manipulated brood with larger brood size than clutch size (part of 'B2: Comparing clutch and brood sizes'; see \code{\link{compare_clutch_brood}}).
#'   \item Row 2 represents a manipulated brood with larger brood size than clutch size (part of 'B2: Comparing clutch and brood sizes'; see \code{\link{compare_clutch_brood}}).
#'   \item Row 3 represents a non-manipulated brood with larger fledgling number than brood size (part of 'B3: Comparing brood sizes and fledgling numbers'; see \code{\link{compare_brood_fledglings}}).
#'   \item Row 4 represents a manipulated brood with larger fledgling number than brood size (part of 'B3: Comparing brood sizes and fledgling numbers'; see \code{\link{compare_brood_fledglings}}).
#'   \item Row 5 represents a brood with an earlier hatching than laying date (part of 'B4: Comparing laying and hatching dates'; see \code{\link{compare_laying_hatching}}).
#'   \item Row 6 represents a brood with an earlier fledging than hatching date (part of 'B5: Comparing hatching and fledging dates'; see \code{\link{compare_hatching_fledging}}).
#'   \item Row 7-14 represent broods with improbable values in ClutchSize, BroodSize and NumberFledged per species (part of: 'B6: Checking brood variable values against reference values'; see \code{\link{check_values_brood}}).
#'   \item Row 15-22 represent broods with impossible values in ClutchSize, BroodSize and NumberFledged per species (part of: 'B6: Checking brood variable values against reference values'; see \code{\link{check_values_brood}}).
#'   \item Row 23 represents a brood with parents of different species (part of: 'B7: Checking parent species'; see \code{\link{check_parent_species}}).
#'   \item Rows 24-25 represent broods with, respectively, a larger and smaller BroodSize than number of chicks in Individual_data (part of: 'B8: Comparing brood size and number of chicks captured'; see \code{\link{compare_broodsize_chicknumber}}).
#'   \item Rows 26-27 represent duplicated broods within a population (part of 'B9: Checking unique brood IDs'; see \code{\link{check_unique_BroodID}}).
#' }
#'
#' \strong{Capture data}:
#' \itemize{
#'   \item Row 1-8 represent adults with improbable values in Mass and Tarsus (part of 'C2: Checking capture variable values against reference values'; see \code{\link{check_values_capture}}).
#'   \item Row 9-16 represent chicks with improbable values in Mass and Tarsus (part of 'C2: Checking capture variable values against reference values'; see \code{\link{check_values_capture}}).
#'   \item Row 17-24 represent adults with impossible values in Mass and Tarsus (part of 'C2: Checking capture variable values against reference values'; see \code{\link{check_values_capture}}).
#'   \item Row 25- 32 represent chicks with impossible values in Mass and Tarsus (part of 'C2: Checking capture variable values against reference values'; see \code{\link{check_values_capture}}).
#'   \item Row 33 represents a chick caught in a nest box, but without a BroodID (part of 'I3: Checking that chicks have BroodIDs'; see \code{\link{check_BroodID_chicks}}).
#' }
#'
#' \strong{Individual data}:
#' \itemize{
#'   \item Row 1-2 represent duplicated individuals within a population (part of 'I2: Checking unique individual IDs'; see \code{\link{check_unique_IndvID}}).
#'   \item Row 3-4 represent duplicated individuals among populations (part of 'I2: Checking unique individual IDs'; see \code{\link{check_unique_IndvID}}).
#'   \item Row 5 represents a chick caught in a nest box, but without a BroodID (part of 'I3: Checking that chicks have BroodIDs'; see \code{\link{check_BroodID_chicks}}).
#'   \item Row 6 represents an individual with conflicting sex (part of 'I4: Checking that individuals have no conflicting sex'; see \code{\link{check_conflicting_sex}}).
#'   \item Rows 7-8 represent the parents of a brood (a female and male, respectively) of different species (part of: 'B7: Checking parent species'; see \code{\link{check_parent_species}}).
#'   \item Rows 9-12 represent two chicks of two broods (part of: 'B8: Comparing brood size and number of chicks captured'; see \code{\link{compare_broodsize_chicknumber}}).
#' }
#'
#' \strong{Location data}:
#' \itemize{
#'   \item Row 1 represents a nest box location corresponding to a chick without a BroodID (part of 'I3: Checking that chicks have BroodIDs'; see \code{\link{check_BroodID_chicks}}).
#' }
#'
#' @name dummy_data
NULL

#' List of verified records to exempted from future quality check
#'
#' List of records that were flagged as warning/error but have been verified by the data owner to be correct.
#'
#' @format A list of 4 data frames:
#' \itemize{
#'    \item{Brood_approved_list}
#'    \item{Capture_approved_list}
#'    \item{Individual_approved_list}
#'    \item{Location_approved_list}
#' }
#'
#' Each data frame has 3 variables:
#' \itemize{
#'    \item{PopID, unique identifier for population.}
#'    \item{A unique identifier for records in each particular data frame (i.e., BroodID, CaptureID, IndvID, LocationID).}
#'    \item{CheckID, unique identifier for individual quality check.}
#' }
#'
#' @name approved_list
NULL

#' CheckIDs and variable names
#'
#' Data frame to link variable names and CheckIDs to be used in whitelisting of previously flagged records.
#'
#' @format Data frame with 2 variables:
#' \describe{
#' \item{CheckID}{Unique identifier for individual quality check.}
#' \item{Var}{Variable name.}
#' }
#'
#' @name checkID_var
NULL
