#' Construct standard summary for data from the Univeristy of Antwerp.
#'
#' A pipeline to produce a standard output for 2 hole-nesting bird study populations
#' administered by the University of Amsterdam.
#' Output follows the HNB standard breeding data format.
#'
#' This section provides details on data management choices that are unique to the NIOO database.
#' For a general description of the standard format please see XXXXX PLACE HOLDER!
#'
#' \strong{ClutchType_observed}: The raw data distinguishes second and third
#' nests and first second and third replacements. We group these all as 'second'
#' and 'replacement'.
#'
#' \strong{ExperimentID}: Experimental codes are provided in their original
#' format. These still need to be translated into our experimental groups.
#'
#' \strong{Tarsus}: Tarsus is measured using Svennson's Standard in early years and
#' Svennson's Alternative in later years. When Svennson's Alternative is available this is used,
#' otherwise we use converted Svensson's Standard, using \code{\link[HNBStandFormat]{convert_tarsus}}.
#' @param db Directory where raw data files are stored.
#' @param Species A numeric vector. Which species should be included (EUring codes)? If blank will return all major species (see details below).
#' @param path Location where output csv files will be saved.
#' @param debug For internal use when editing pipelines. If TRUE, pipeline
#'   generates a summary of pipeline data. This
#'   includes: a) Histogram of continuous variables with mean/SD b) unique
#'   values of all categorical variables.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import dplyr
#' @import purrr

format_UAN <- function(db = choose.dir(),
                       Species = NULL,
                       path = ".",
                       debug = FALSE){

  start_time <- Sys.time()

  all_files <- list.files(path = db, pattern = ".csv", full.names = TRUE)

  BOX_info <- read.csv(all_files[grepl("BOX", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  BROOD_info <- read.csv(all_files[grepl("BR", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  INDV_info <- read.csv(all_files[grepl("IND", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  PLOT_info <- read.csv(all_files[grepl("PL", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  CAPTURE_info <- read.csv(all_files[grepl("VG", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  #Create a table with brood information.
  clutchtype <- dplyr::progress_estimated(n = nrow(BROOD_info))

  browser()

  Brood_data <- BROOD_info %>%
    #Link the plot codes to the plot code (GB)
    dplyr::left_join(select(PLOT_info, PopID = SA, GB = gb), by = "GB") %>%
    #Convert all other columns into standard format
    dplyr::mutate(SampleYear = year,
           Species = ifelse(SOORT == "pm", "GT", ifelse(SOORT == "pc", "BT", NA)),
           PopID = PopID, Plot = GB,
           LocationID = PL, BroodID = NN,
           FemaleID = RW, MaleID = RM,
           LayingDate = lubridate::dmy(LD), LayingDateError = NA,
           ClutchSize = AE, ClutchSizeError = NA,
           HatchDate = NA, HatchDateError = NA,
           BroodSize = NP, BroodSizeError = NA,
           FledgeDate = NA, FledgeDateError = NA,
           NumberFledged = PU, NumberFledgedError = NA,
           AvgMass = GG, AvgTarsus = GT,
           ExperimentID = exp) %>%
    #Include ClutchType info
    #Firstly, convert ClutchType_observed
    dplyr::left_join(tibble::tibble(TY = 0:9,
                                    ClutchType_observed = c(NA, "First", "Second", "Replacement",
                                                            "Replacement", "Replacement",
                                                            "Second", "Second", "Replacement",
                                                            "First")), by = "TY") %>%
    #Create ClutchType_calc
    group_by(PopID, SampleYear, Species) %>%
    mutate(cutoff = tryCatch(expr = min(LayingDate, na.rm = T) + 30,
                             warning = function(...) return(NA))) %>%
    # Determine brood type for each nest based on female ID
    arrange(SampleYear, Species, FemaleID) %>%
    group_by(SampleYear, Species, FemaleID) %>%
    #Assume that any NAs in the Number of fledglings should be 0s.
    mutate(total_fledge_narm = calc_cumfledge(NumberFledged, na.rm = TRUE),
           row = 1:n()) %>%
    ungroup() %>%
    dplyr::mutate(ClutchType_calc = purrr::pmap_chr(.l = list(rows = .$row,
                                                              femID = .$FemaleID,
                                                              cutoff_date = .$cutoff,
                                                              nr_fledge_before = .$total_fledge_narm,
                                                              LD = .$LayingDate),
                                                    .f = function(rows, femID, cutoff_date, nr_fledge_before, LD){

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

                                                        if(lubridate::ymd(LD) > lubridate::ymd(cutoff_date)){

                                                          return("replacement")

                                                        } else {

                                                          return("first")

                                                        }

                                                      }

                                                      #If she is banded, then we need to apply all rules
                                                      #If it's the first nest recorded for this female in this year...
                                                      if(rows == 1){

                                                        #If it doesn't meet the 30 day rule, then name it as replacement
                                                        if(lubridate::ymd(LD) > lubridate::ymd(cutoff_date)){

                                                          return("replacement")

                                                        } else {

                                                          #Otherwise, we assume it was the first clutch
                                                          return("first")

                                                        }

                                                        #If it's NOT the first nest of the season for this female
                                                      } else {

                                                        #If there have been no fledglings before this point..
                                                        if(nr_fledge_before == 0){

                                                          #Then it is a replacement
                                                          return("replacement")

                                                        } else {

                                                          #Otherwise, it is a secondary clutch
                                                          return("second")

                                                        }

                                                      }

                                                    })) %>%
    dplyr::select(SampleYear, Species, PopID, Plot, LocationID,
                  BroodID, FemaleID, MaleID, ClutchType_observed,
                  ClutchType_calc, LayingDate:ExperimentID)


  ################
  # CAPTURE DATA #
  ################

  print("Compiling capture information...")

  #Capture data includes all times an individual was captured (with measurements
  #like mass, tarsus etc.). This will include first capture as nestling (for
  #residents) This means there will be multiple records for a single individual.

  Capture_data <- CAPTURE_info %>%
    #Make tarsus length into standard method (Svensson Alt)
    #Firstly, convert the Svennson's standard measures to Svennson's Alt.
    dplyr::mutate(TA = convert_tarsus(TA, method = "Standard"),
                  Tarsus = purrr::pmap_dbl(.l = list(SvenStd = .$TA,
                                                     SvenAlt = .$TANEW),
                                           .f = function(SvenStd, SvenAlt){

                                             if(!is.na(SvenAlt)){

                                               return(SvenAlt)

                                             } else if(!is.na(SvenStd)){

                                               return(SvenStd)

                                             } else {

                                               return(NA)

                                             }

                                           })) %>%
    #There is no information on release location, so I assume it's the same as the capture location.
    transmute(CaptureDate = lubridate::dmy(VD),
              CaptureTime = lubridate::hm(paste(UUR %/% 1, round(UUR %% 1)*60, sep = ":")),
              IndvID = RN, Species = ifelse(SOORT == "pm", "GT", ifelse(SOORT == "pc", "BT", NA)),
              Type = ifelse(VW %in% c("P", "PP"), "Nestling", "Adult"),
               CaptureLocation = SA, ReleaseLocation = SA,
              Mass = GEW, Tarsus = Tarsus,
              WingLength = VLL)

  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  Sex_table <- data_frame(sex = c(1, 2, 3, 4, 5),
                          Sex = c("male", "female", "male?", "female?", "sex unknown"))

  #To determine which brood an individual is from, we subset the CAPTURE_info table to include only those records where an individual was caught as a nestling (P).
  #We then take the nest that this nestling was in when it was caught.
  Indv_broods <- CAPTURE_info %>%
    group_by(RN) %>%
    filter(!is.na(NN) & VW == "P") %>%
    summarise(BroodIDLaid = NN[1]) %>%
    rename(rn = RN)

  #I assume that the broodID laid and fledged are the same in this case.
  Indv_data <- INDV_info %>%
    left_join(Sex_table, by = "sex") %>%
    left_join(Indv_broods, by = "rn") %>%
    mutate(BroodIDFledged = NA,
           Species = ifelse(soort == "pm", "GT", ifelse(soort == "pc", "BT", NA)),
           IndvID = rn, RingDate = lubridate::dmy(klr1date), RingYear = lubridate::year(RingDate),
           RingAge = lubridate::year(RingDate) - gbj,
           Status = ifelse(mode == "P", "Resident", "Immigrant"), Sex = Sex) %>%
    select(BroodIDLaid, BroodIDFledged, Species, IndvID, RingYear, RingAge, Status, Sex)

  ################
  # NESTBOX DATA #
  ################

  print("Compiling nestbox information...")

  Nestbox_data <- BOX_info %>%
    transmute(NestboxID = GBPL, PopID = SA, Latitude = Y_deg, Longitude = X_deg, StartYear = YEARFIRST, EndYear = YEARLAST)

  ###################
  # POPULATION DATA #
  ###################

  print("Compiling population summary information...")

  Plot_species <- Brood_data %>%
    group_by(PopID) %>%
    filter(!is.na(Species)) %>%
    summarise(Species = paste(unique(Species), collapse = ","))

  Pop_data <- PLOT_info %>%
    filter(SA != "") %>%
    rename(PopID = SA) %>%
    group_by(PopID) %>%
    summarise(StartYear = min(FIRSTY), EndYear = max(LASTY)) %>%
    left_join(Plot_species, by = "PopID") %>%
    left_join(Nestbox_data %>%
                group_by(PopID) %>%
                summarise(TotalNestbox = length(unique(NestboxID))), by = "PopID") %>%
    mutate(PopName = c("Boshoek", "Peerdsbos"))

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_UAN.csv"), row.names = F)

  write.csv(x = Indv_data, file = paste0(path, "\\Indv_data_UAN.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_UAN.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_UAN.csv"), row.names = F)

  write.csv(x = Pop_data, file = paste0(path, "\\Summary_data_UAN.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
