#' Species EURING codes and letter codes
#'
#' Letter codes to easily identify species (English names)
#' @format A data frame with 10 rows and 4 variables
#' \describe{
#'   \item{SpeciesID}{Species EUring code.}
#'   \item{Species}{Species letter code.}
#'   \item{CommonName}{Common English name.}
#'   \item{BinomialName}{Genus and species name.}
#'   }
#'@name species_codes
species_codes <- utils::read.csv(system.file("extdata", "species_codes.csv", package = "pipelines", mustWork = TRUE))

#'Population names and locations
#'
#'Information and identifying codes for all populations and data owners.
#'@format A data frame with 26 rows and 4 variables
#'\describe{
#'  \item{PopID}{Three-letter population code.}
#'  \item{PopName}{Name of population.}
#'  \item{Country}{Name of country where population is situated.}
#'  \item{Owner}{Letter code for data owner. Note, multiple populations can have
#'  the same data owner.}
#'  \item{OwnerName}{Name of owner.}}
#'@name pop_codes
pop_codes <- utils::read.csv(system.file("extdata", "pop_codes.csv", package = "pipelines", mustWork = TRUE))

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
#'@format A data frame with 97 rows and 2 variables.
#'\describe{
#'    \item{PopCode}{Three letter population ID.}
#'    \item{SpeciesCode}{Six letter species ID.}
#'}
#'@name pop_species_combos
pop_species_combos <- utils::read.csv(system.file("extdata", "pop_species_combos.csv", package = "pipelines", mustWork = TRUE))

#' Quality check dummy data
#'
#' A dummy pipeline output specifically created to test single checks from \code{\link{quality_check}}. A CheckID column is added to each dataframe to mark which rows serve to test each check. See a detailed description of which rows correspond to which checks in 'Details'.
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
#'
#' \strong{Brood data}:
#' \itemize{
#'   \item Rows 1-4 are records that test the functionality of check 'B1: Comparing clutch and brood sizes' (see \code{\link{compare_clutch_brood}}): a non-manipulated brood with larger clutch size than brood size, a non-manipulated brood with larger brood size than clutch size, a manipulated brood with larger clutch size than brood size, and a manipulated brood with larger brood size than clutch size.
#'   \item Rows 5-8 are records that test the functionality of check 'B2: Comparing brood sizes and fledgling numbers' (see \code{\link{compare_brood_fledglings}}): a non-manipulated brood with larger brood size than fledgling number, a non-manipulated brood with larger fledgling number than brood size, a manipulated brood with larger brood size than fledgling number, and a manipulated brood with larger fledgling number than brood size.
#'   \item Rows 9-10 are records that test the functionality of check 'B3: Comparing laying and hatching dates' (see \code{\link{compare_laying_hatching}}): a brood with an earlier laying than hatching date, and a brood with an earlier hatching than a laying date.
#'   \item Rows 11-12 are records that test the functionality of check 'B4: Comparing hatching and fledging dates' (see \code{\link{compare_hatching_fledging}}): a brood with an earlier hatching than fledging date, and a brood with an earlier fledging than a hatching date.
#'   \item Rows 13-629 are records that test the functionality of checks 'B5a-d: Checking brood variable values against reference values' (see \code{\link{check_values_brood}}): broods with probable, unlikely and impossible values in ClutchSize_observed (B6a), BroodSize_observed (B6b), NumberFledged_observed (B6c) and LayDate_observed (B6d).
#'   \item Rows 630-632 are records that test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}): a brood with a brood size equal to the number of chicks in Capture_data, a brood with a brood size larger than the number of chicks in Capture_data, and a brood with a brood size smaller than the number of chicks in Capture_data.
#'   \item Rows 633-635 are records that test the functionality of check 'B7: Checking that brood IDs are unique' (see \code{\link{check_unique_BroodID}}): a brood with a unique BroodID, and two broods with the same BroodID.
#'   \item Rows 636-641 are records that test the functionality of check 'B8: Checking that clutch type order is correct' (see \code{\link{check_clutch_type_order}}): two broods by one breeding pair in one season in the correct order of creation, two broods by one breeding pair in one season in the correct order of creation but "first" brood missing, two broods by one breeding pair in one season in the incorrect order of creation.
#'   \item Rows 642-644 are records that test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}): a brood with parents of the same species, and a brood with parents of different species.
#'   \item Rows 645-646 are records that are needed to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents (see \code{\link{check_adult_parent_nest}}): a brood with the same individuals as parents as the individuals caught on that nest, and a brood with individuals different from the individuals that are caught on this nest.
#'   \item Rows 647-649 are records that test the functionality of check 'B10: Comparing species of brood and of parents' (see \code{\link{compare_species_brood_parents}}): a brood of the same species as both of the parents, and a brood of a different species than either of the parents.
#'   \item Rows 650-652 are records that test the functionality of check 'B11: Comparing species of brood and of chicks' (see \code{\link{compare_species_brood_chicks}}): a brood of the same species as the chicks in that brood, and a brood of a different species than either of the chicks in that brood.
#'   \item Rows 653-654 are records that test the functionality of check 'B12: Checking sex of mothers' (see \code{\link{check_sex_mothers}}): a brood where assigned mother is female and a brood where assigned mother is listed as male.
#'   \item Rows 655-656 are records that test the functionality of check 'B13: Checking sex of fathers' (see \code{\link{check_sex_fathers}}): a brood where assigned father is male and a brood where assigned father is listed as female.
#' }
#'
#' \strong{Capture data}:
#' \itemize{
#'   \item Rows 1-4 are records that are needed to test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}): chicks in equal, smaller or larger number than recorded as brood size in Brood_data.
#'   \item Rows 5-10 are records that are needed test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}): parents of the same or different species than recorded in Brood_data.
#'   \item Rows 11-988 are records that test the functionality of checks 'C1a-b: Checking capture variable values against reference values' (see \code{\link{check_values_capture}}): adults and chicks with probable, unlikely and impossible values in Mass (C1a) and Tarsus (C1b).
#'   \item Rows 989-990 are records that test the functionality of check 'C2: Checking chick age values' (see \code{\link{check_chick_age}}): a chick with a correct chick age, and a chick with an incorrect chick age.
#'   \item Rows 991-995 are records that are needed to test the functionality of check 'I1: Checking unique individual IDs' (see \code{\link{check_unique_IndvID}}): individuals with IDs that are unique, duplicated among populations and duplicated within populations.
#'   \item Rows 996-997 are records that are needed to test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}): a chick with a BroodID, and a chick without a BroodID.
#'   \item Rows 998-999 are records that are needed to test the functionality of check 'I3: Checking that individuals have no conflicting sex' (see \code{\link{check_conflicting_sex}}): an individual with certain sex, and an individual with uncertain sex (i.e. recorded as both male and female).
#'   \item Rows 1000-1001 are records that are needed to test the functionality of check 'I4:Checking that individuals have no conflicting species' (see \code{\link{check_conflicting_species}}): an individual with certain species, and an individual with uncertain species (i.e. recorded as two different species).
#'   \item Row 1002 is a record that is needed to test the functionality of check 'I5: Checking that individuals in Individual_data also appear in Capture_data' (see \code{\link{check_individuals_captures}}): an individual in Individual_data that is also recorded in Capture_data.
#'   \item Rows 1003-1005 are records that test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents' (see \code{\link{check_adult_parent_nest}}): a parent caught on a nest and marked as the parent of that brood, a parent caught in a mistnet, and a parent caught on a nest but not marked as the parent of that brood.
#'   \item Rows 1004-1011 are records that test the functionality of check 'C4: Checking that the age of subsequent captures is ordered correctly' (see \code{\link{check_age_captures}}): two captures in subsequent years with correct order of age, two captures in the same year with same age, two captures in subsequent years with incorrect order of age (but both are adults), two captures in subsequent years with incorrect order of age (adult caught before chick).
#' }
#'
#' \strong{Individual data}:
#' \itemize{
#'   \item Rows 1-4 are records that are needed to test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}): chicks in equal, smaller or larger number than recorded as brood size in Brood_data.
#'   \item Rows 5-10 are records that are needed test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}): parents of the same or different species than recorded in Brood_data.
#'   \item Rows 11-15 are records that test the functionality of check 'I1: Checking unique individual IDs' (see \code{\link{check_unique_IndvID}}): individuals with IDs that are unique, duplicated among populations and duplicated within populations.
#'   \item Rows 16-17 are records that test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}): a chick with a BroodID, and a chick without a BroodID.
#'   \item Rows 18-19 are records that test the functionality of check 'I3: Checking that individuals have no conflicting sex' (see \code{\link{check_conflicting_sex}}): an individual with certain sex, and an individual with uncertain sex (i.e. recorded as both male and female).
#'   \item Rows 20-21 are records that test the functionality of check 'I4: Checking that individuals have no conflicting species' (see \code{\link{check_conflicting_species}}): an individual with certain species, and an individual with uncertain species (i.e. recorded as two different species).
#'   \item Rows 22-23 are records that test the functionality of check 'I5: Checking that individuals in Individual_data also appear in Capture_data' (see \code{\link{check_individuals_captures}}): an individual that is also recorded in Capture_data, and an individual that is missing from Capture_data.
#'   \item Rows 24-29 are records that are needed to test the functionality of check 'B10: Comparing species of brood and of parents' (see \code{\link{compare_species_brood_parents}}): parents of the same species as their brood, and parents of different species than their brood.
#'   \item Rows 30-35 are records that are needed to test the functionality of check 'B11: Comparing species of brood and of chicks' (see \code{\link{compare_species_brood_chicks}}): chicks of the same species as the brood they are in, and chicks of different species than the brood they are in.
#' }
#'
#' \strong{Location data}:
#' \itemize{
#'   \item Rows 1-2 is a record that is needed to test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}): location information of a nestbox.
#'   \item Rows 3-5 are records that are needed to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents' (see \code{\link{check_adult_parent_nest}}): location information of two nestboxes and a mist net.
#'   \item Row 6-107 are records that are needed to test the functionality of check 'L1: Checking capture locations coordinates' (see \code{\link{check_coordinates}}): Capture locations within 15 km of study site centre point and one capture locations farther away.
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
#' Data frame to link variable names and CheckIDs to be used in approve-listing of previously flagged records.
#'
#' @format Data frame with 2 variables:
#' \describe{
#' \item{CheckID}{Unique identifier for individual quality check.}
#' \item{Variable}{Variable name.}
#' }
#'
#' @name checkID_variable_combos
checkID_variable_combos <- utils::read.csv(system.file("extdata", "checkID_variable_combos.csv", package = "pipelines", mustWork = TRUE))


#' List of cutoffs used to define errors and warnings for chick mass
#'
#'
#' @format Data frame with 4 variables and 248 rows:
#' \describe{
#' \item{Species}{6 letter code to identify species}
#' \item{ChickAge}{Age at which cutoff is applied}
#' \item{Reference}{In what context should the cutoff be used (e.g. warning, error)}
#' \item{Value}{Value of the cutoff}
#' }
#'
#' @name chick_mass_cutoffs
NULL

#' Common hybrids
#'
#' Data frame with common hybrid broods. Can be used in quality checks to compare species across parents, between parents and brood, and between brood and nestlings.
#'
#' @format Data frame with 2 variables:
#' \describe{
#' \item{Species1}{6 letter code for species of record 1}
#' \item{Species2}{6 letter code for species of record 2}
#' }
#'
#' @name common_hybrids
common_hybrids <- utils::read.csv(system.file("extdata", "common_hybrids.csv", package = "pipelines", mustWork = TRUE))

#'capture_data_template template
#'
#'Tibble giving names and associated classes for all the columns in the Capture data for the standard protocol v1.1.0
#'@format Tibble
#'@name capture_data_template
NULL

#'individual_data_template template
#'
#'Tibble giving names and associated classes for all the columns in the Individual data for the standard protocol v1.1.0
#'@format Tibble
#'@name individual_data_template
NULL

#'brood_data_template template
#'
#'Tibble giving names and associated classes for all the columns in the Brood data for the standard protocol v1.1.0
#'@format Tibble
#'@name brood_data_template
NULL

#'location_data_template template
#'
#'Tibble giving names and associated classes for all the columns in the Location data for the standard protocol v1.1.0
#'@format Tibble
#'@name location_data_template
NULL

#' List of standard format column names (protocol version 1.1)
#'
#' All columns names appearing in the 4 tables of the standard format, organized
#' in a list.
#' @format A list with 4 elements. Each element is a character vector.
#' \describe{
#'   \item{Brood}{Column names in `Brood_data`.}
#'   \item{Capture}{Column names in `Capture_data`.}
#'   \item{Individual}{Column names in `Individual_data`.}
#'   \item{Location}{Column names in `Location_data`.}
#'   }
#'@name column_names_v1.1
column_names_df_v1.1 <- utils::read.csv(system.file("extdata", "column_names_v1.1.csv", package = "pipelines", mustWork = TRUE), na.strings = "")
column_names_v1.1 <- list(
  Brood = column_names_df_v1.1$Brood[which(!is.na(column_names_df_v1.1$Brood))],
  Capture = column_names_df_v1.1$Capture[which(!is.na(column_names_df_v1.1$Capture))],
  Individual = column_names_df_v1.1$Individual[which(!is.na(column_names_df_v1.1$Individual))],
  Location = column_names_df_v1.1$Location[which(!is.na(column_names_df_v1.1$Location))]
)
rm(column_names_df_v1.1)
