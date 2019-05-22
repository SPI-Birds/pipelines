#' Title
#'
#' @param db
#' @param Species
#' @param path
#'
#' @return
#' @export
#' @import readxl
#' @import janitor
#' @import reshape2
#'
#' @examples
format_Portugal <- function(db = NULL,
                            Species = NULL,
                            path = "."){

  if(is.null(db)){

    print("Please select a database location...")

    db <- file.choose()

  }

  #Read in data with readxl
  all_data <- read_excel(db) %>%
    #Clean all names with janitor
    janitor::clean_names(case = "upper_camel") %>%
    #Change species to "GT" because it's only GT
    mutate(Species = "GT")

  ##############
  # BROOD DATA #
  ##############

  #Subset only those records with a brood ID.
  Brood_data <- all_data %>%
    filter(!is.na(BroodId)) %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year
    mutate(BroodID = glue::glue('{Year}_{BroodId}'),
           PopID = "POR", Plot = NA,
           LocationID = Box)

  #Determine adults caught on the brood
  #Assume these are the parents
  #Reshape data so that sex is a column not a row
  Parents <- Brood_data %>%
    filter(Age != "C") %>%
    select(BroodID, Ring, Sex) %>%
    #'No ringed/no ring' becomes NA
    mutate(Ring = map_chr(.x = Ring, .f = ~ifelse(grepl(pattern = "ring", .x), NA, .x))) %>%
    reshape2::melt(id = c("BroodID", "Sex")) %>%
    reshape2::dcast(BroodID ~ Sex) %>%
    rename(FemaleID = `F`, MaleID = `M`)

  #Determine whether clutches are 2nd clutch
  ClutchType_obsv <- Brood_data %>%
    group_by(BroodID) %>%
    summarise(ClutchType_observed = ifelse("2nd" %in% SecondClutch, "second", "first"))

  #Join these back in to the original data
  #Brood_data <- Brood_data %>%
  Brood_data <- Brood_data %>%
    left_join(Parents, by = "BroodID") %>%
    #Determine 2nd clutch
    #For now, just assume that anything listed as '2nd'
    #Is a true 2nd clutch not replacement.
    #I will ask.
    group_by(BroodID) %>%







}
