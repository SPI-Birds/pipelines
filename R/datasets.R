#' Species taxonomic ranks and codes
#'
#' Species information, including various taxonomic ranks, internal and external codes.
#' @format A data frame with 34 rows and 13 variables
#' \describe{
#'   \item{speciesCode}{SPI-Birds' internal persistent identifier for a species.}
#'   \item{speciesID}{SPI-Birds' 6-letter species identifier. First three letters indicate the generic name, last three letters indicate the specific epithet. Note that this identifier might change if a species' genus or species indicator change.}
#'   \item{speciesEURINGCode}{Species code used in EURING. NA for non-European species.}
#'   \item{kingdom}{Scientific name of the kingdom in which the species is identified.}
#'   \item{phylum}{Scientific name of the phylum in which the species is identified.}
#'   \item{class}{Scientific name of the class in which the species is identified.}
#'   \item{order}{Scientific name of the order in which the species is identified.}
#'   \item{family}{Scientific name of the family in which the species is identified.}
#'   \item{genus}{Scientific name of the genus in which the species is identified.}
#'   \item{specificEpithet}{Scientific name of the species epithet.}
#'   \item{infraspecificEpithet}{Scientific name of the infraspecific epithet (e.g. sub-species).}
#'   \item{scientificNameAuthorship}{Authorship information of the scientific name, including date information if known.}
#'   \item{vernacularName}{Common English name.}
#'   }
#'@name species_codes
species_codes <- utils::read.csv(system.file("extdata", "species_codes.csv", package = "pipelines", mustWork = TRUE),
                                 colClasses = c("integer", rep("character", 12)), na.strings = "") %>%
  tibble::as_tibble()

#'Study site names and locations
#'
#'Information and identifying codes for all study sites and data owners.
#'
#'@format A data frame with 114 rows and 11 variables
#'\describe{
#'  \item{siteID}{Three-letter identifier for the study site.}
#'  \item{siteName}{Name of the study site.}
#'  \item{country}{Name of the country in which the study site is located.}
#'  \item{countryCode}{Standard code for the country, using \href{https://www.iso.org/iso-3166-country-codes.html}{ISO 3166-1 alpha-2}.}
#'  \item{institutionID}{Three- or four-letter identifier for data owner/institution. Note, multiple study sites can have
#'  the same data owner.}
#'  \item{institutionCode}{Name of owner/institution.}
#'  \item{decimalLatitude}{Geographic latitude of the geographic center of the study site in decimal degrees.}
#'  \item{decimalLongitude}{Geographic longitude of the geographic center of the study site in decimal degrees.}
#'  \item{locationAccordingTo}{Source of location information. Either "data owner" or "data".}
#'  \item{data}{Logical. Are the data available through SPI-Birds?}
#'  \item{standardFormat}{Logical. If data are available, are they standardized?}
#'  }
#'@name site_codes
site_codes <- utils::read.csv(system.file("extdata", "site_codes.csv", package = "pipelines", mustWork = TRUE)) %>%
  tibble::as_tibble()

#'Habitat types and descriptions
#'
#'Habitat descriptions according to European Nature Information System (EUNIS) \href{https://www.eea.europa.eu/data-and-maps/data/eunis-habitat-classification-1}{Habitats Classification Scheme} (version 2012).
#'
#'@format A data frame with 4185 rows and 4 variables
#'\describe{
#'  \item{habitatID}{Identifier for the habitat as provided by EUNIS.}
#'  \item{habitatLevel}{Hierarchical level for the habitat as provided by EUNIS.}
#'  \item{habitatType}{Name or short description for the habitat as proved by EUNIS.}
#'  \item{habitatDetails}{Detailed description or definition for the habitat as provided by EUNIS.}
#'  }
#'@name habitat_codes
habitat_codes <- utils::read.csv(system.file("extdata", "habitat_codes.csv", package = "pipelines", mustWork = TRUE),
                                 colClasses = "character") %>%
  tibble::as_tibble()

#'Spatial data polygon of great tit distribution.
#'
#'Distribution of great tits used for plotting all populations.
#'@format A spatial data polygon.
#'@name GT_dist_gg
NULL

#'Combination of all siteID and speciesID from all pipelines.
#'
#'A data frame with each row containing a unique site and species combination.
#'
#'@format A data frame with 143 rows and 2 variables.
#'\describe{
#'    \item{siteID}{Three-letter site ID.}
#'    \item{speciesID}{Six-letter species ID.}
#'}
#'@name site_species_combos
site_species_combos <- utils::read.csv(system.file("extdata", "site_species_combos.csv", package = "pipelines", mustWork = TRUE), colClasses = "character") %>%
  tibble::as_tibble()

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
#'   \item Rows 1-4 are records that are used to test the functionality of check 'B1: Comparing clutch and brood sizes' (see \code{\link{compare_clutch_brood}}). The records are a non-manipulated brood with larger clutch size than brood size, a non-manipulated brood with larger brood size than clutch size, a manipulated brood with larger clutch size than brood size, and a manipulated brood with larger brood size than clutch size.
#'   \item Rows 5-8 are records that are used to test the functionality of check 'B2: Comparing brood sizes and fledgling numbers' (see \code{\link{compare_brood_fledglings}}). The records are a non-manipulated brood with larger brood size than fledgling number, a non-manipulated brood with larger fledgling number than brood size, a manipulated brood with larger brood size than fledgling number, and a manipulated brood with larger fledgling number than brood size.
#'   \item Rows 9-10 are records that are used to test the functionality of check 'B3: Comparing laying and hatching dates' (see \code{\link{compare_laying_hatching}}). The records are a brood with an earlier laying than hatching date, and a brood with an earlier hatching than a laying date.
#'   \item Rows 11-12 are records that are used to test the functionality of check 'B4: Comparing hatching and fledging dates' (see \code{\link{compare_hatching_fledging}}). The records are a brood with an earlier hatching than fledging date, and a brood with an earlier fledging than a hatching date.
#'   \item Rows 13-629 are records that are used to test the functionality of checks 'B5a-d: Checking brood variable values against reference values' (see \code{\link{check_values_brood}}). The records are broods with probable, unlikely and impossible values in ClutchSize_observed (B5a), BroodSize_observed (B6b), NumberFledged_observed (B5c) and LayDate_observed (B5d).
#'   \item Rows 630-632 are records that are used to test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}). The records are a brood with a brood size equal to the number of chicks in Capture_data, a brood with a brood size larger than the number of chicks in Capture_data, and a brood with a brood size smaller than the number of chicks in Capture_data.
#'   \item Rows 633-635 are records that are used to test the functionality of check 'B7: Checking that brood IDs are unique' (see \code{\link{check_unique_BroodID}}). The records are a brood with a unique BroodID, and two broods with the same BroodID.
#'   \item Rows 636-641 are records that are used to test the functionality of check 'B8: Checking that clutch type order is correct' (see \code{\link{check_clutch_type_order}}). The records are two broods by one breeding pair in one season in the correct order of creation, two broods by one breeding pair in one season in the correct order of creation but "first" brood missing, two broods by one breeding pair in one season in the incorrect order of creation.
#'   \item Rows 642-644 are records that are used to test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}). The records are a brood with parents of the same species, and a brood with parents of different species.
#'   \item Rows 645-646 are records that are used to to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents (see \code{\link{check_adult_parent_nest}}). The records are a brood with the same individuals as parents as the individuals caught on that nest, and a brood with individuals different from the individuals that are caught on this nest.
#'   \item Rows 647-649 are records that are used to test the functionality of check 'B10: Comparing species of brood and of parents' (see \code{\link{compare_species_brood_parents}}). The records are a brood of the same species as both of the parents, and a brood of a different species than either of the parents.
#'   \item Rows 650-652 are records that are used to test the functionality of check 'B11: Comparing species of brood and of chicks' (see \code{\link{compare_species_brood_chicks}}). The records are a brood of the same species as the chicks in that brood, and a brood of a different species than either of the chicks in that brood.
#'   \item Rows 653-654 are records that are used to test the functionality of check 'B12: Checking sex of mothers' (see \code{\link{check_sex_mothers}}). The records are a brood where assigned mother is female and a brood where assigned mother is listed as male.
#'   \item Rows 655-656 are records that are used to test the functionality of check 'B13: Checking sex of fathers' (see \code{\link{check_sex_fathers}}). The records are a brood where assigned father is male and a brood where assigned father is listed as female.
#'   \item Rows 657-660 are records that are used to test the functionality of check 'B14: Checking that parents appear in Capture_data' (see \code{\link{check_parents_captures}}). The records are a brood with both parents present in Capture_data, and broods with either or both parents missing from Capture_data.
#'   \item Rows 661-663 are records that used to test the functionality of check 'B15: Checking that nest locations appear in Location_data' (see \code{\link{check_brood_locations}}). The records are a brood with a location that does appear in Location_data, and a brood with a location that does not appear in Location_data.
#' }
#'
#' \strong{Capture data}:
#' \itemize{
#'   \item Rows 1-4 are records that are used to test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}). The records are chicks in equal, smaller or larger number than recorded as brood size in Brood_data.
#'   \item Rows 5-10 are records that are used to test the functionality of check 'B8: Checking that clutch type order is correct' (see \code{\link{check_clutch_type_order}}). The records are mothers with correct and incorrect clutch orders.
#'   \item Rows 11-16 are records that are used to test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}). The records are parents of the same or different species than recorded in Brood_data.
#'   \item Rows 17-994 are records that are used to test the functionality of checks 'C1a-b: Checking capture variable values against reference values' (see \code{\link{check_values_capture}}). The records are adults and chicks with probable, unlikely and impossible values in Mass (C1a) and Tarsus (C1b).
#'   \item Rows 995-996 are records that are used to test the functionality of check 'C2: Checking chick age values' (see \code{\link{check_chick_age}}). The records are a chick with a correct chick age, and a chick with an incorrect chick age.
#'   \item Rows 997-999 are records that are used to test the functionality of check 'I1: Checking unique individual IDs' (see \code{\link{check_unique_IndvID}}). The records are individuals with IDs that are unique, duplicated among populations and duplicated within populations.
#'   \item Rows 1000-1001 are records that are used to test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}). The records are a chick with a BroodID, and a chick without a BroodID.
#'   \item Rows 1002-1003 are records that are used to test the functionality of check 'I3: Checking that individuals have no conflicting sex' (see \code{\link{check_conflicting_sex}}). The records are an individual with certain sex, and an individual with uncertain sex (i.e. recorded as both male and female).
#'   \item Rows 1004-1005 are records that are used to test the functionality of check 'I4: Checking that individuals have no conflicting species' (see \code{\link{check_conflicting_species}}). The records are an individual with certain species, and an individual with uncertain species (i.e. recorded as two different species).
#'   \item Row 1006 is a record that is used to test the functionality of check 'I5: Checking that individuals in Individual_data also appear in Capture_data' (see \code{\link{check_individuals_captures}}). The record is an individual that is also recorded in Individual_data.
#'   \item Rows 1007-1009 are records that are used to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents' (see \code{\link{check_adult_parent_nest}}). The records are a parent caught on a nest and marked as the parent of that brood, a parent caught in a mist net, and a parent caught on a nest but not marked as the parent of that brood.
#'   \item Rows 1010-1015 are records that are used to test the functionality of check 'B10: Comparing species of brood and of parents' (see \code{\link{compare_species_brood_parents}}). The records are parents with the same and different species.
#'   \item Rows 1016-1021 are records that are used to test the functionality of check 'B11: Comparing species of brood and of chicks' (see \code{\link{compare_species_brood_chicks}}). The records are parents with the same and different species.
#'   \item Rows 1022-1025 are records that are used to test the functionality of check 'B12: Checking sex of mothers' (see \code{\link{check_sex_mothers}}). The records are mothers with correct and incorrect sex.
#'   \item Rows 1026-1029 are records that are used to test the functionality of check 'B13: Checking sex of fathers' (see \code{\link{check_sex_fathers}}). The records are fathers with correct and incorrect sex.
#'   \item Rows 1030-1037 are records that are used to test the functionality of check 'C4: Checking that the age of subsequent captures is ordered correctly' (see \code{\link{check_age_captures}}). The records are two captures in subsequent years with correct order of age, two captures in the same year with same age, two captures in subsequent years with incorrect order of age (but both are adults), two captures in subsequent years with incorrect order of age (adult caught before chick).
#'   \item Rows 1038-1139 are records that are used to test the functionality of check 'L1: Checking location coordinates' (see \code{\link{check_coordinates}}). The records are captures on locations within a study area.
#'   \item Rows 1140-1141 are records that are used to test the functionality of check 'C5: Checking that individuals in Capture_data also appear in Individual_data' (see \code{\link{check_captures_individuals}}). The records are an individual that is also recorded in Individual_data, and one that is missing from Individual_data.
#'   \item Rows 1142-1145 are records that are used to test the functionality of check 'B14: Checking that parents appear in Capture_data' (see \code{\link{check_parents_captures}}). The records are parents of broods where either none or one parent is missing.
#'   \item Rows 1146-1149 are records that used to test the functionality of check 'B15: Checking that nest locations appear in Location_data' (see \code{\link{check_brood_locations}}). The records are the parents of the broods, caught on locations that appear or do not appear in Location_data.
#'   \item Rows 1150-1151 are records that used to test the functionality of check 'C6: Checking that capture locations appear in Location_data' (see \code{\link{check_capture_locations}}). The records are captures on a location that does appear in Location_data, and on a location that does not appear in Location_data.
#' }
#'
#' \strong{Individual data}:
#' \itemize{
#'   \item Rows 1-4 are records that are used to test the functionality of check 'B6: Comparing brood size with number of chicks captured' (see \code{\link{compare_broodsize_chicknumber}}). The records are chicks in equal, smaller or larger number than recorded as brood size in Brood_data.
#'   \item Rows 5-10 are records that are used to test the functionality of check 'B8: Checking that clutch type order is correct' (see \code{\link{check_clutch_type_order}}). The records are mothers with correct and incorrect clutch orders.
#'   \item Rows 11-16 are records that are used to test the functionality of check 'B9: Comparing parent species' (see \code{\link{compare_species_parents}}). The records are parents of the same or different species than recorded in Brood_data.
#'   \item Rows 17-944 are records that are used to test the functionality of checks 'C1a-b: Checking capture variable values against reference values' (see \code{\link{check_values_capture}}). The records are adults and chicks with probable, unlikely and impossible values in Mass (C1a) and Tarsus (C1b).
#'   \item Rows 995-996 are records that are used to test the functionality of check 'C2: Checking chick age values' (see \code{\link{check_chick_age}}). The records are a chick with a correct chick age, and a chick with an incorrect chick age.
#'   \item Rows 997-999 are records that are used to test the functionality of check 'I1: Checking unique individual IDs' (see \code{\link{check_unique_IndvID}}). The records are individuals with IDs that are unique, duplicated among populations and duplicated within populations.
#'   \item Rows 1000-1001 are records that are used to test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}). The records are a chick with a BroodID, and a chick without a BroodID.
#'   \item Rows 1002-1003 are records that are used to test the functionality of check 'I3: Checking that individuals have no conflicting sex' (see \code{\link{check_conflicting_sex}}). The records are an individual with certain sex, and an individual with uncertain sex (i.e. recorded as both male and female).
#'   \item Rows 1004-1005 are records that are used to test the functionality of check 'I4: Checking that individuals have no conflicting species' (see \code{\link{check_conflicting_species}}). The records are an individual with certain species, and an individual with uncertain species (i.e. recorded as two different species).
#'   \item Rows 1006-1007 are records that are used to test the functionality of check 'I5: Checking that individuals in Individual_data also appear in Capture_data' (see \code{\link{check_individuals_captures}}). The records are an individual that is also recorded in Capture_data, and an individual that is missing from Capture_data.
#'   \item Rows 1008-1010 are records that are used to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents' (see \code{\link{check_adult_parent_nest}}). The records are a parent caught on a nest and marked as the parent of that brood, a parent caught in a mist net, and a parent caught on a nest but not marked as the parent of that brood.
#'   \item Rows 1011-1016 are records that are used to test the functionality of check 'B10: Comparing species of brood and of parents' (see \code{\link{compare_species_brood_parents}}). The records are parents of the same species as their brood, and parents of different species than their brood.
#'   \item Rows 1017-1022 are records that are used to test the functionality of check 'B11: Comparing species of brood and of chicks' (see \code{\link{compare_species_brood_chicks}}). The records are chicks of the same species as the brood they are in, and chicks of different species than the brood they are in.
#'   \item Rows 1023-1026 are records that are used to test the functionality of check 'B12: Checking sex of mothers' (see \code{\link{check_sex_mothers}}). The records are parents with correct and incorrect sex.
#'   \item Rows 1027-1030 are records that are used to test the functionality of check 'B13: Checking sex of fathers' (see \code{\link{check_sex_fathers}}). The records are parents with correct and incorrect sex.
#'   \item Rows 1031-1034 are records that are used to test the functionality of check 'C4: Checking that the age of subsequent captures is ordered correctly' (see \code{\link{check_age_captures}}). The records are chicks and adults caught in correct order (chick, adult) and incorrect order (adult, chick).
#'   \item Rows 1035-1136 are records that are used to test the functionality of check 'L1: Checking location coordinates' (see \code{\link{check_coordinates}}). The records are captures of individuals within a study area.
#'   \item Rows 1137 is a record that is used to test the functionality of check 'C5: Checking that individuals in Capture_data also appear in Individual_data' (see \code{\link{check_captures_individuals}}). The record is an individual that is also recorded in Capture_data.
#'   \item Rows 1138-1141 are records that are used to test the functionality of check 'B14: Checking that parents appear in Capture_data' (see \code{\link{check_parents_captures}}). The records are parents of broods where either none or one parent is missing.
#'   \item Rows 1142-1145 are records that used to test the functionality of check 'B15: Checking that nest locations appear in Location_data' (see \code{\link{check_brood_locations}}). The records are the parents of the broods, caught on locations that appear or do not appear in Location_data.
#'   \item Rows 1146-1147 are records that are used to test the functionality of check 'C6: Checking that capture locations appear in Location_data' (see \code{\link{check_capture_locations}}). The records are individuals captured on locations that do and do not appear in Location_data.
#' }
#'
#' \strong{Location data}:
#' \itemize{
#'   \item Rows 1-2 are records that are used to test the functionality of check 'I2: Checking that chicks have BroodIDs' (see \code{\link{check_BroodID_chicks}}). The records describe location information of a nest box.
#'   \item Rows 3-5 are records that are used to test the functionality of check 'C3: Checking that adults caught on nest are listed are the parents' (see \code{\link{check_adult_parent_nest}}). The records describe location information of two nest boxes and a mist net.
#'   \item Rows 6-107 are records that are used to test the functionality of check 'L1: Checking location coordinates' (see \code{\link{check_coordinates}}). The records are locations within 20 km of study site centre point and one location farther away.
#'   \item Row 108 is a record that is used to test the functionality of check 'B15: Checking that nest locations appear in Location_data' (see \code{\link{check_brood_locations}}). The record is a nest location of a brood in Brood_data.
#'   \item Rows 109 is a record that is used to test the functionality of check 'C6: Checking that capture locations appear in Location_data' (see \code{\link{check_capture_locations}}). The record is a capture location of an individual in Capture_data.
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
