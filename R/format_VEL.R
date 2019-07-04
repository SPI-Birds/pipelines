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
                  LayingDate = laying_date, LayingDateError = NA,
                  ClutchSize = clutch_size, ClutchSizeError = NA,
                  HatchDate = hatching_date, HatchDateError = NA,
                  BroodSize = number_hatched, BroodSizeError = NA,
                  NumberFledged = number_fledged, NumberFledgedError = NA) %>%
    dplyr::arrange(SampleYear, Species, FemaleID) %>%
    #Calculate clutchtype
    dplyr::mutate(ClutchType_calc = calc_clutchtype(data = ., na.rm = FALSE))

}
