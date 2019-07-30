#' Species EURING codes and letter codes
#'
#' Letter codes to easily identify species (English names)
#' @format A data frame with 8 rows and 4 variables
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
#'@format A data frame with 15 rows and 4 variables
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

#'List of expected values for brood data.
#'
#'Plausible brood data values that can be used to detect outliers.
#'@format A list with 2 items.
#'\describe{
#'  \item{PARMAJ}{Expected values of clutch size, brood size and number of fledglings for great tits.}
#'  \item{FICALB}{Expected values of clutch size, brood size and number of fledglings for collared flycatchers.}}
#'@name brood_ref_values_list
NULL

#'List of expected values for capture data.
#'
#'Plausible capture data values that can be used to detect outliers.
#'@format A list with 2 items.
#'\describe{
#'  \item{PARMAJ}{Expected values of mass and tarsus for great tits.}
#'  \item{FICALB}{Expected values of mass and tarsus for collared flycatchers.}}
#'@name capture_ref_values_list
NULL

#'Spatial data polygon of great tit distribution.
#'
#'Distribution of great tits used for plotting all populations.
#'@format A spatial data polygon.
#'@name GT_dist_gg
NULL
