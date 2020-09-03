#' Convert sequence of rings to a data frame in wide format
#'
#' @param chick_rings Character string. Sequence of chick rings separatead by hyphen (e.g. "1000 - 01").
#' Can include multiple sequences separated by spaces (e.g. "1020 - 21 2020 - 22") or
#' a combination of a ring sequence and single rings  (e.g. "1020 - 21 2000").
#'
#' @return A tibble in wide format (each column is an individual ring)
#' @export
#'
#' @examples
#' #Regular ring sequence
#' chick_expand("1000 - 01")
#'
#' #Multiple sequences
#' chick_expand("1020 - 21 2020 - 22")
#'
#' #Sequence and single ring
#' chick_expand("1020 - 21 2000")

chick_expand <- function(chick_rings) {

  #Firstly, we want to split sequence of rings (e.g. xxx-xxx) and single rings (xxxx).
  #These can be included in the same column separated by spaces, but the '-' can also have spaces, so we can't just split by ' '
  raw_rings <- chick_rings %>%
    #Trim whitespace to prevent empty 'rings' being created
    stringr::str_trim() %>%
    #Pattern searches for a hyphen surrounded by any number of spaces and removes spaces
    stringr::str_replace_all(., pattern = "\\s*-\\s*", replacement = "-") %>%
    #Some are separated by , some by " ", some by both
    stringr::str_split(., pattern = "\\,*\\s+|\\,+\\s*")

  #We now have a list of all ring sequences/single rings separated for each brood
  split <- purrr::map_df(.x = raw_rings, .f = ~{

    ring_seq <- stringr::str_split(..1, pattern = "-") %>%
      purrr::map(.f = ~{

        if (all(is.na(..1))) {

          NA_character_

        } else {

          if (length(..1) == 1) {

            ..1

          } else {

            #Trim again to remove internal spaces
            x <- stringr::str_trim(..1) %>%
              #Remove any leading 0s to make sure that length of prefix and suffix are correct
              stringr::str_remove(pattern = "^0*")

            prefix <- stringr::str_sub(x[1], end = -(nchar(x[2]) + 1))
            start  <- stringr::str_sub(x[1], start = -nchar(x[2])) %>% as.numeric()
            end    <- x[2] %>% as.numeric()
            suffix <- seq(start, end, 1)

            paste0(prefix, suffix)

          }

        }

      }) %>%
      unlist()

    tibble(col_name = paste0("ChickID_", 1:length(ring_seq)),
           ID = ring_seq) %>%
      tidyr::pivot_wider(names_from = col_name, values_from = ID)

  })

  split

} ## FIXME: Make into a generic func that also works for HAR/KEV data
