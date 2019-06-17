#' Calculate cumulative number of fledgings
#'
#' For a given nest, determine the cumulative number of fledglings in all nests
#' before this. This is used to calculate ClutchType_calc. The function also
#' includes functionality to return whether number of fledglings in previous
#' nests was measured or not (i.e. were there NAs in any nests before the
#' current one). This is neccesary in populations where the number of fledglings
#' is not measured consistently
#' @param x Column NumberFledged in the Brood_data table
#' @param na.rm Logical. If TRUE, returns cumulative number of fledglings
#' where NA is assumed to be 0. If FALSE, returns a vector of logicals
#' showing whether any nests before the current nest were unmeasured (NA).
#'
#' @return A vector of numerics (if na.rm = TRUE) or logicals (na.rm = FALSE)
#' @export
#'
#' @examples
#' #Assuming NA is 0
#' #Return vector of numerics.
#' fledge_calc(x = c(1, 3, NA, 4), na.rm = TRUE)
#'
#' #Do not assume NA is 0.
#' #Return a vector of logicals showing whether an NA occurred before
#' #the current record.
#' fledge_calc(x = c(1, 3, NA, 4), na.rm = FALSE)
calc_cumfledge <- function(x, na.rm = TRUE){

  if(na.rm){

    #This func assumes that all NAs are just 0s.
    #This is needed because otherwise cumsum returns all NAs
    #However, all we need to know is if there was atleast 1 successful nest before the current nest
    x[!complete.cases(x)] <- 0

    nrs <- cumsum(x)

  } else {

    x <- is.na(x)

    nrs <- cumsum(x)

  }

  if(length(x) == 1){

    return(0)

  } else {

    if(na.rm){

      return(c(0, nrs[1:(length(nrs) - 1)]))

    } else {

      return(as.logical(c(0, nrs[1:(length(nrs) - 1)])))

    }

  }

}
