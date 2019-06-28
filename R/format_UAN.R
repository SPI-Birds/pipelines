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

  all_files <- list.files(path = db, pattern = ".xlsx", full.names = TRUE)

  BOX_info <- readxl::read_excel(all_files[grepl("BOX", all_files)])
  BROOD_info <- readxl::read_excel(all_files[grepl("BR", all_files)])
  INDV_info <- readxl::read_excel(all_files[grepl("IND", all_files)])
  PLOT_info <- readxl::read_excel(all_files[grepl("PL", all_files)])
  CAPTURE_info <- readxl::read_excel(all_files[grepl("VG", all_files)])

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  #Create a table with brood information.
  clutchtype <- dplyr::progress_estimated(n = nrow(BROOD_info))

  #Create table with species names for left join
  Species_codes <- Species_codes %>%
    dplyr::mutate(SOORT = c("pm", "pc", NA, NA, NA, NA)) %>%
    dplyr::select(SOORT, Species = Code)

  #Create table with PopID
  Pop_codes <- tibble::tibble(SA = c("FR", "PB"),
                              PopID = c("BOS", "PEE"))

  Brood_data <- BROOD_info %>%
    #Link in Pop_codes
    dplyr::left_join(Pop_codes, by = "SA") %>%
    #Remove only great tit and blue tit (other species have < 100 nests)
    dplyr::filter(SOORT %in% c("pc", "pm")) %>%
    dplyr::left_join(Species_codes, by = "SOORT") %>%
    #Convert all other columns into standard format
    dplyr::mutate(SampleYear = year,
           PopID = PopID, Plot = GB,
           LocationID = PL, BroodID = NN,
           FemaleID = RW, MaleID = RM,
           LayingDate = format.POSIXct(LD, format = "%Y-%m-%d"), LayingDateError = NA,
           ClutchSize = AE, ClutchSizeError = NA,
           HatchDate = NA, HatchDateError = NA,
           BroodSize = NP, BroodSizeError = NA,
           FledgeDate = NA, FledgeDateError = NA,
           NumberFledged = PU, NumberFledgedError = NA,
           AvgEggMass = NA, NrEggs = NA,
           AvgChickMass = GG, NrChicksMass = GN,
           AvgTarsus = GT, NrChicksTarsus = GN,
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
    #Add in PopID
    dplyr::left_join(Pop_codes, by = "SA") %>%
    #Make tarsus length into standard method (Svensson Alt)
    #Firstly, convert the Svennson's standard measures to Svennson's Alt.
    #Then only use this converted measure when actual Svennson's Alt is unavailable.
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
    #Also join in Species
    #Remove only great tit and blue tit (other species have < 100 nests)
    dplyr::filter(SOORT %in% c("pc", "pm")) %>%
    dplyr::left_join(Species_codes, by = "SOORT") %>%
    #There is no information on release location, so I assume it's the same as the capture location.
    dplyr::mutate(CaptureDate = lubridate::ymd(VD),
                  SampleYear = lubridate::year(CaptureDate),
              CaptureTime = lubridate::hm(paste(UUR %/% 1, round(UUR %% 1)*60, sep = ":")),
              IndvID = RN,
              CapturePopID = PopID, CapturePlot = GB,
              ReleasePopID = PopID, ReleasePlot = GB,
              Mass = GEW, Tarsus = Tarsus,
              WingLength = VLL) %>%
    #Calculate age at capture and chick age based on the LT column
    dplyr::bind_cols(purrr::map2_dfr(.x = .$LT, .y = .$VW,
                                    .f = ~{

                                      # If Age (LT) was not recorded
                                      # instead estimate age from the capture type:
                                      if(is.na(.x) | .x == 0){

                                        #Capture type P and PP are chicks in the nest
                                        if(.y %in% c("P", "PP")){

                                          return(tibble::tibble(Age_obsv = 1, ChickAge = NA))

                                        #Captures in mist nets, observed rings, cage traps, roost checks
                                        #must be able to fly. But these can be anything from fledglings
                                        #in first calendar year +
                                        } else if (.y %in% c("FU", "GE", "MN", "ON", "NO", "SK", "SL", "LS")){

                                          return(tibble::tibble(Age_obsv = 2, ChickAge = NA))

                                        } else {

                                          #If no age or capture type is given, then age is unknown.
                                          return(tibble::tibble(Age_obsv = NA, ChickAge = NA))

                                        }

                                      }

                                      #If age is > 10 this is the chick age in days
                                      if(.x > 5){

                                        return(tibble::tibble(Age_obsv = 1, ChickAge = .x))

                                      } else {

                                        #If it's <10 then we translate into EURING codes for adults
                                        if(.x %in% c(1, 2)){

                                          return(tibble::tibble(Age_obsv = 1 + .x*2, ChickAge = NA))

                                        } else {

                                          return(tibble::tibble(Age_obsv = 3 + (.x - 3)*2, ChickAge = NA))

                                        }

                                      }

                                    })) %>%
  #Determine age at first capture for every individual
  #First arrange the data chronologically within each individual
  arrange(IndvID, CaptureDate) %>%
    #Then, for each individual, determine the first age and year of capture
    group_by(IndvID) %>%
    mutate(FirstAge = first(Age_obsv),
           FirstYear = first(SampleYear)) %>%
    ungroup() %>%
    #Calculate age at each capture using EUring codes
    mutate(Age_calc = purrr::pmap_dbl(.l = list(IndvID = .$IndvID,
                                                Age = .$FirstAge,
                                                Year1 = .$FirstYear,
                                                YearN = .$SampleYear),
                                      .f = function(IndvID, Age, Year1, YearN){

                                        # If age at first capture is unknown
                                        # or the bird is unringed
                                        # we cannot determine age at later captures
                                        if(is.na(Age) | is.na(IndvID)){

                                          return(NA)

                                        }

                                        #Determine number of years since first capture...
                                        diff_yr <- (YearN - Year1)

                                        #If it was not caught as a chick...
                                        if(Age != 1){

                                          #Use categories where age is uncertain
                                          #(6, 8)
                                          return(4 + 2*diff_yr)

                                        } else {

                                          #If it was caught as a chick
                                          if(diff_yr == 0){

                                              return(1)

                                          } else {

                                            #Otherwise, use categories where age is certain (5, 7, etc.)
                                            return(3 + 2*diff_yr)

                                          }

                                        }

                                      }))

  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  Sex_table <- data_frame(sex = c(1, 2, 3, 4, 5),
                          Sex = c("M", "F", "M", "F", "U"))

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
