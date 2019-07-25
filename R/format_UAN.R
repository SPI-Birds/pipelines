#' Construct standard summary for data from the Univeristy of Antwerp.
#'
#' A pipeline to produce a standard output for 2 hole-nesting bird study populations
#' administered by the University of Antwerp.
#' Output follows the HNB standard breeding data format.
#'
#' This section provides details on data management choices that are unique to this data.
#' For a general description of the standard format please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#' \strong{ClutchType_observed}: The raw data distinguishes second and third
#' nests and first second and third replacements. We group these all as 'second'
#' and 'replacement'.
#'
#' \strong{ClutchSizeError}: The raw data includes a column to determine whether clutch size
#' was counted with or without a brooding female. The presence of a brooding female
#' can effect the uncertainty in the count. After discussions with the data owner
#' clutch size counted with a brooding female have an error of 2.
#'
#' \strong{ExperimentID}: Experimental codes are provided in their original
#' format. These still need to be translated into our experimental groups.
#'
#' \strong{Tarsus}: Tarsus is measured using Svennson's Standard in early years and
#' Svennson's Alternative in later years. When Svennson's Alternative is available this is used,
#' otherwise we use converted Svensson's Standard, using \code{\link[HNBStandFormat]{convert_tarsus}}.
#'
#'@inheritParams pipeline_params
#'
#' @return Generates 4 .csv files with data in a standard format.
#' @export

format_UAN <- function(db = choose.dir(),
                       species = NULL,
                       path = ".",
                       debug = FALSE){

  #Force choose.dir() if used
  force(db)

  #Assign species for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

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

  Brood_data <- create_brood_UAN(BROOD_info, species)

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
                  CaptureTime = na_if(paste(UUR %/% 1,
                                            stringr::str_pad(string = round((UUR %% 1)*60),
                                                             width = 2,
                                                             pad = "0"), sep = ":"), "NA:NA"),
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

                                      #If age is > 5 this is the chick age in days
                                      if(.x > 5){

                                        return(tibble::tibble(Age_obsv = 1, ChickAge = .x))

                                      } else {

                                        #If it's 1-5 then we translate into EURING codes for adults
                                        if(.x %in% c(1, 2)){

                                          return(tibble::tibble(Age_obsv = 1 + .x*2, ChickAge = NA))

                                        } else {

                                          return(tibble::tibble(Age_obsv = 4 + (.x - 3)*2, ChickAge = NA))

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

                                        } else {

                                          #Determine number of years since first capture...
                                          diff_yr <- (YearN - Year1)

                                          #Increase the age by 2*number of years.
                                          #We don't need to determine whether it was
                                          #first caught as check etc.
                                          #the nuance in the age is already in Age_obsv
                                          return(Age + 2*diff_yr)

                                        }

                                      })) %>%
    #Select just the required cols
    dplyr::select(IndvID, Species, CaptureDate, CaptureTime, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, WingLength,
                  Age_obsv, Age_calc, ChickAge) %>%
    #Arrange by individual and date/time
    dplyr::arrange(IndvID, CaptureDate, CaptureTime)

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
    summarise(BroodIDLaid = first(NN)) %>%
    rename(rn = RN)

  Indv_Pop <- Capture_data %>%
    dplyr::filter(!is.na(CapturePopID)) %>%
    group_by(IndvID) %>%
    summarise(PopID = first(CapturePopID)) %>%
    rename(rn = IndvID)

  #I assume that the broodID laid and fledged are the same in this case.
  Indv_data <- INDV_info %>%
    #Also join in Species
    #Remove only great tit and blue tit (other species have < 100 nests)
    dplyr::rename(SOORT = soort) %>%
    dplyr::filter(SOORT %in% c("pc", "pm")) %>%
    dplyr::left_join(Species_codes, by = "SOORT") %>%
    dplyr::left_join(Sex_table, by = "sex") %>%
    dplyr::left_join(Indv_broods, by = "rn") %>%
    dplyr::left_join(Indv_Pop, by = "rn") %>%
    mutate(BroodIDRinged = BroodIDLaid,
           IndvID = rn, RingDate = lubridate::dmy(klr1date), RingYear = lubridate::year(RingDate),
           RingAge = lubridate::year(RingDate) - gbj,
           Sex = Sex) %>%
    select(IndvID, Species, PopID, BroodIDLaid, BroodIDRinged, RingYear, RingAge, Sex)

  ################
  # NESTBOX DATA #
  ################

  print("Compiling nestbox information...")

  Nestbox_data <- BOX_info %>%
    ## NEED TO CONVERT COORDINATES TO LAT/LONG IN WGS84
    transmute(LocationID = GBPL, PopID = SA, Latitude = Y_deg, Longitude = X_deg, StartYear = YEARFIRST, EndYear = YEARLAST)

  ###################
  # POPULATION DATA #
  ###################

  # print("Compiling population summary information...")
  #
  # Plot_species <- Brood_data %>%
  #   group_by(PopID) %>%
  #   filter(!is.na(Species)) %>%
  #   summarise(Species = paste(unique(Species), collapse = ","))
  #
  # Pop_data <- PLOT_info %>%
  #   filter(SA != "") %>%
  #   rename(PopID = SA) %>%
  #   group_by(PopID) %>%
  #   summarise(StartYear = min(FIRSTY), EndYear = max(LASTY)) %>%
  #   left_join(Plot_species, by = "PopID") %>%
  #   left_join(Nestbox_data %>%
  #               group_by(PopID) %>%
  #               summarise(TotalNestbox = length(unique(NestboxID))), by = "PopID") %>%
  #   mutate(PopName = c("Boshoek", "Peerdsbos"))

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_UAN.csv"), row.names = F)

  write.csv(x = Indv_data, file = paste0(path, "\\Indv_data_UAN.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_UAN.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_UAN.csv"), row.names = F)

  # write.csv(x = Pop_data, file = paste0(path, "\\Summary_data_UAN.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}

#############################################################################################

create_brood_UAN <- function(data, species_filter){

  #Convert column names to make it easier to understand what is being done
  data <- dplyr::rename(data, BroodID = NN, Species = SOORT, Plot = GB, LocationID = PL,
                        LayingDate = LD, ClutchSizeError = JAE,
                        ClutchSize = AE, NrUnhatchedChicks = AEN,
                        TotalHatchedChicks = NP, NrDeadChicks = PD,
                        NumberFledged = PU, LDInterruption = LO,
                        ClutchType_observed = TY, MaleID = RM, FemaleID = RW,
                        Unknown = AW, ChickWeighAge = WD, ChickWeighTime = WU,
                        ObserverID = ME, NumberChicksMass = GN,
                        AvgTarsus = GT, AvgChickMass = GG,
                        AvgChickBodyCondition = CON, PopID = SA,
                        NestNumberFirstBrood = NNN1,
                        NestNumberSecondBrood = NNN2,
                        NestNumberThirdBrood = NNN3,
                        NestNumberBigamousMale = NNBI,
                        NestNumberTrigamousMale = NNTRI,
                        StageAbandoned = VERL,
                        ExperimentID = exp,
                        Longitude = coorx, Latitude = coory,
                        Comments = comm, BreedingSeason = year,
                        UniqueLocationCode = gbpl)

  #Create a table with brood information.
  clutchtype <- dplyr::progress_estimated(n = nrow(BROOD_info))

  Brood_data <- data %>%
    #Convert columns to expected values
    dplyr::mutate(PopID = dplyr::case_when(.$PopID == "FR" ~ "BOS",
                                           .$PopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code,
                                             .$Species == "pc" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code),
                  ClutchType_observed = dplyr::case_when(.$ClutchType_observed %in% c(1, 9) ~ "first",
                                                         .$ClutchType_observed %in% c(2, 6, 7) ~ "second",
                                                         .$ClutchType_observed %in% c(3, 4, 5, 8) ~ "replacement"),
                  ClutchSizeError = dplyr::case_when(.$ClutchSizeError == "J" ~ 0,
                                                     .$ClutchSizeError == "N" ~ 2)) %>%
    #Remove only species chosen.
    dplyr::filter(Species %in% species_filter) %>%
    #Add NA columns and convert dates
    dplyr::mutate(LayingDate = lubridate::ymd(LayingDate),
                  LayingDateError = NA,
                  HatchDate = NA, HatchDateError = NA,
                  BroodSizeError = NA,
                  FledgeDate = NA, FledgeDateError = NA,
                  NumberFledgedError = NA,
                  AvgEggMass = NA, NumberEggs = NA,
                  NumberChicksTarsus = NumberChicksMass) %>%
    #Calculate clutchtype, assuming NAs are true unknowns
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(., na.rm = FALSE)) %>%
    #Order columns
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot,
                  LocationID, FemaleID, MaleID, ClutchType_observed,
                  ClutchType_calculated,
                  LayingDate, LayingDateError,
                  ClutchSize, ClutchSizeError,
                  HatchDate, HatchDateError,
                  BroodSize, BroodSizeError,
                  FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError,
                  AvgEggMass, NumberEggs,
                  AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus,
                  ExperimentID)

  ## NEED TO ADD ORIGINAL TARSUS METHOD BASED ON CAPTURE DATA LATER
  return(Brood_data)

}
