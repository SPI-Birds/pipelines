#' Check that a species entry is correct
#'
#' @param Species Vector of 6-letter species codes as recorded in `species_codes`.
#'
#' @return Vector of 6-letter species codes.
#' @export
#'
#' @examples
#' #Codes are correct and are returned
#' check_species("PARMAJ")
#'
#' #Codes are incorrect and throws error
#' #check_species("WRONG")

check_species <- function(Species) {

  if (any(!Species %in% species_codes$speciesID)) {

    stop(paste("\n speciesID is incorrect:",
               crayon::yellow(Species[!Species %in% species_codes$speciesID])))

  }

  return(Species)

}

#' Check that a site entry is correct
#'
#' @param Site Vector of 3-letter site codes as recorded in `site_codes`.
#'
#' @return Vector of 3-letter site codes.
#' @export
#'
#' @examples
#' #Codes are correct and are returned
#' check_site("AMM")
#'
#' #Codes are incorrect and throws error
#' #check_site("WRONG")

check_site <- function(Site) {

  if (any(!Site %in% site_codes$siteID)) {

    stop(paste("\n siteID is incorrect:",
               crayon::yellow(Site[!Site %in% site_codes$siteID])))

  }

  return(Site)

}
