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

  #The data is currently stored as capture data (i.e. each row is a capture)
  #This means there are multiple records for each brood, which we don't want.

  #Subset only those records with a brood ID.
  ##THERE IS ONE RECORD THAT WAS MEANT TO BE CAUGHT AT A BOX BUT NO ID RECORDED
  ##NEED TO CHECK THIS!!
  #We're not interested in chick captures
  #These contain no brood data
  Brood_data <- all_data %>%
    filter(!is.na(BroodId) & Age != "C") %>%
    #BroodIDs are not unique (they are repeated each year)
    #We need to create unique IDs for each year
    mutate(BroodID = glue::glue('{Year}_{BroodId}'))

  #Determine adults caught on the brood
  #Assume these are the parents
  #Reshape data so that sex is a column not a row
  Parents <- Brood_data %>%
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

  Brood_data <- Brood_data %>%
    left_join(Parents, by = "BroodID") %>%
    left_join(ClutchType_obsv, by = "BroodID") %>%
    #Now we can melt and reshape our data
    #Remove columns that do not contain relevant brood info
    select(-CodeLine:-Ring, -JulianDate:-StanderdisedTime, -TrapingMethod,
           -BroodId:-Smear, -MeanEggWeight:-NºEggsWeighted) %>%
    group_by(BroodID) %>%
    melt(id = c("BroodID", "Species", "Year", "Site", "Box", "FemaleID", "MaleID")) %>%
    dcast(BroodID + Species + Year + Site + Box + FemaleID + MaleID ~ ..., fun.aggregate = first) %>%
    rowwise() %>%
    mutate(NoChicksOlder14D = ifelse(is.na(NoChicksOlder14D), 0, as.numeric(NoChicksOlder14D))) %>%
    ungroup() %>%
    #Determine the 30 day cut-off for all species
    ### NEED TO GO THROUGH AND DETERMINE WHETHER THE 30 DAY CUT OFF IS REASONABLE FOR ALL SPECIES
    ### LOOK AT THIS LATER
    group_by(Year, Species) %>%
    mutate(cutoff = tryCatch(expr = min(as.numeric(LayingDateJulian), na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(Year, Species, FemaleID) %>%
    group_by(Year, Species, FemaleID) %>%
    mutate(total_fledge = cumsum(NoChicksOlder14D), row = 1:n()) %>%
    ungroup() %T>%
    {clutchtype <<- dplyr::progress_estimated(n = nrow(.))} %>%
    mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       nr_fledge_now = .$NoChicksOlder14D,
                                                       LD = as.numeric(.$LayingDateJulian)),
                                             .f = function(rows, femID, cutoff_date, nr_fledge_before, nr_fledge_now, LD){

                                               clutchtype$tick()$print()

                                               #Firstly, check if the nest has a LD
                                               #If not, we cannot calculate BroodType

                                               if(is.na(LD)){

                                                 return(NA)

                                               }

                                               #Next, check if the female is banded
                                               #If a female is unbanded we assume the nest can NEVER be secondary
                                               #If she had had a successful clutch before she would have been caught and banded
                                               #Therefore it can only be first or replacement (based on 30d rule)
                                               if(is.na(femID)){

                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   return("first")

                                                 }

                                               }

                                               #If she is banded, then we need to apply all rules
                                               #If it's the first nest recorded for this female in this year...
                                               if(rows == 1){

                                                 #If it doesn't meet the 30 day rule, then name it as replacement
                                                 if(LD > cutoff_date){

                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, we assume it was the first clutch
                                                   return("first")

                                                 }

                                                 #If it's NOT the first nest of the season for this female
                                               } else {

                                                 #If there have been no fledglings before this point..
                                                 if(nr_fledge_before - nr_fledge_now == 0){

                                                   #Then it is a replacement
                                                   return("replacement")

                                                 } else {

                                                   #Otherwise, it is a secondary clutch
                                                   return("second")

                                                 }

                                               }

                                             })) %>%
    mutate(PopID = "CHO", Plot = NA, HatchDate = NA,
           FledgeDate = NA) %>%
    #Select relevant columns and rename
    select(SampleYear = Year, Species, PopID, Plot,
           LocationID = Box, BroodID, FemaleID, MaleID,
           ClutchType_observed,
           ClutchType_calc, LayingDate = LayingDateJulian,
           ClutchSize = FinalClutchSize,
           HatchDate, BroodSize = NoChicksHatched,
           FledgeDate,
           NumberFledged = NoChicksOlder14D)


}
