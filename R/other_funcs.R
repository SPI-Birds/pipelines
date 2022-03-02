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

    stop(paste("\n Species code is incorrect:",
               crayon::yellow(Species[!Species %in% species_codes$speciesID])))

  }

  return(Species)

}

#' Check that a species entry is correct
#'
#' @param PopID Vector of 6-letter species codes as recorded in `species_codes`.
#'
#' @return Vector of 3-letter population codes.
#' @export
#'
#' @examples
#' #Codes are correct and are returned
#' check_pop("AMM")
#'
#' #Codes are incorrect and throws error
#' #check_pop("WRONG")

check_pop <- function(PopID) {

  if (any(!PopID %in% pop_codes$PopID)) {

    stop(paste("\n PopID is incorrect:",
               crayon::yellow(PopID[!PopID %in% pop_codes$PopID])))

  }

  return(PopID)

}
