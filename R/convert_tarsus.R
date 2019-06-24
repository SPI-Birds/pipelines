#' Convert tarsus measurements to Svennson's Alternative
#'
#' Convert tarsus lenght measurements in Svennson's Standard and Oxford Maximum
#' to Svensson's Alternative.
#'
#' We consider Svensson's alternative to be the most reliable measurement
#' method.
#' @param x A numeric vector. Tarsus length measurements (mm).
#' @param method Character string. The measurement method of the original data.
#'   Can be: "Standard" (Svennson's Standard) or "Oxford" (Oxford maximum).
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#'
#' convert_tarsus(x = c(15, 20, 22), method = "Standard")
convert_tarsus <- function(x, method){

  if(method == "Standard"){

    return(6.158 + 0.777 * x)

  } else if(method == "Oxford"){

    return(3.64549 + 0.72005 * x)

  }

}
