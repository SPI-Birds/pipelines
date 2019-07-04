format_VEL <- function(db = choose.dir(),
                       Species = NULL,
                       path = ".",
                       debug = FALSE) {

  start_time <- Sys.time()

  ## Read in flycatcher data. There are some issues will coercion of col types
  ## so we specify column types manually. With this we can skip certain cols.
  ## we skip:
  ## - row number
  ## - whether chicks were observed dead (we currently have no col for this in the standard format)
  ## - Adult wing and forhead patch measures
  ## - Picture and geolocator info
  ## - Info on which eggs were transferred in cross foster
  FICHYP_data <- readxl::read_excel(paste0(db, "/Velky_Kosir_flycatchers.xlsx"),
                                    col_types = c("skip", "numeric", "text",
                                                  "text", "list",
                                                  "numeric", "text",
                                                  "text", "numeric",
                                                  "list", "numeric",
                                                  rep("text", 8),
                                                  rep(c(rep("numeric", 4), "skip"), 8),
                                                  "text", "list", "numeric",
                                                  "numeric", "numeric",
                                                  rep("skip", 6),
                                                  "text", "list", "text",
                                                  "numeric", "numeric",
                                                  "numeric", rep("skip", 13),
                                                  "text", rep("skip", 16))) %>%
    janitor::clean_names() %>%
    ## Date info is sometimes recorded as dd/mm/yyyy and sometimes as dd.mm.yyyy.
    ## This causes some issues with date parsing. If we parse as text, it converts
    ## the excel dates into numerics. If we parse as date, it gives NAs for the
    ## cases where date is stored as dd.mm.yyyy.
    ## Instead we parse each one separately, based on the best guess of readxl.
    ## This returns dates for dd/mm/yyyy, but character for dd.mm.yyyy.
    ## Then we just have to go through and make these few character strings into dates.
    dplyr::mutate_at(.vars = vars(contains("date")),
                     .funs = function(date){

                       purrr::map(.x = date,
                                  .f = ~{

                                    if(is.character(.x)){

                                      return(as.Date(.x, format = "%d.%m.%Y"))

                                    } else {

                                      return(as.Date(.x))

                                    }

                                  })

                     }) %>%
    tidyr::unnest()

  TIT_data    <- readxl::read_excel(paste0(db, "/Velky_Kosir_tits.xls")) %>%
    janitor::clean_names()

  ##############
  # BROOD DATA #
  ##############

  Brood_data <- create_brood_VEL(FICHYP_data, TIT_data)

}


create_brood_VEL <- function(FICHYP_data, TIT_data) {

  FICHYP_broods <- FICHYP_data %>%
    dplyr::mutate(SampleYear = year,
                  Species = Species_codes[which(Species_codes$SpeciesID == 13490), ]$Code,
                  PopID = "VEL",
                  Plot = plot,
                  LocationID = NA,
                  BroodID = paste(SampleYear, nest, sep = "_"),
                  FemaleID = female_ring, MaleID = male_ring,
                  ClutchType_observed = NA,
                  LayingDate = as.Date(laying_date, format = "%d/%m/%Y"), LayingDateError = NA,
                  ClutchSize = clutch_size, ClutchSizeError = NA,
                  HatchDate = hatching_date, HatchDateError = NA,
                  BroodSize = number_hatched, BroodSizeError = NA,
                  NumberFledged = number_fledged, NumberFledgedError = NA) %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::group_by(PopID, SampleYear, Species) %>%
    dplyr::mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + lubridate::days(30),
                                    warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    dplyr::group_by(SampleYear, Species, FemaleID) %>%
    mutate(total_fledge = calc_cumfledge(x = NumberFledged, na.rm = T),
           total_fledge_na = calc_cumfledge(x = NumberFledged, na.rm = F),
           row = 1:n()) %>%
    ungroup() %>%
    mutate(ClutchType_calc2 = purrr::pmap_chr(.l = list(rows = .$row,
                                                       femID = .$FemaleID,
                                                       cutoff_date = .$cutoff,
                                                       nr_fledge_before = .$total_fledge,
                                                       na_fledge_before = .$total_fledge_na,
                                                       LD = .$LayingDate),
                                             .f = function(rows, femID, cutoff_date,
                                                           nr_fledge_before, na_fledge_before,
                                                           LD){

                                               # clutchtype$tick()$print()

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
                                                 if(nr_fledge_before == 0){

                                                   #If there was atleast one NA record before this one
                                                   #then we don't know if number of fledged before is
                                                   #0 or >0. Therefore, we have to say NA.
                                                   if(na_fledge_before > 0){

                                                     return(NA)

                                                   } else {

                                                     #Otherwise, we can be confident that
                                                     #number of fledge before is 0
                                                     #and it must be a replacement
                                                     return("replacement")

                                                   }

                                                 } else {

                                                   #If there has been atleast one clutch
                                                   #that previously produced fledgligns
                                                   #then this nest is 'second'
                                                   #N.B. This is the case even if one of the previous nests
                                                   #was an NA. We just need to know if it's >0, not the exact number
                                                   return("second")

                                                 }

                                               }

                                             }))

}
