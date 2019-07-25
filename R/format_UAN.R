#'Construct standard summary for data from the Univeristy of Antwerp.
#'
#'A pipeline to produce a standard output for 2 hole-nesting bird study
#'populations administered by the University of Antwerp. Output follows the HNB
#'standard breeding data format.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{ClutchType_observed}: The raw data distinguishes second and third
#'nests and first second and third replacements. We group these all as 'second'
#'and 'replacement'.
#'
#'\strong{ClutchSizeError}: The raw data includes a column to determine whether
#'clutch size was counted with or without a brooding female. The presence of a
#'brooding female can effect the uncertainty in the count. After discussions
#'with the data owner clutch size counted with a brooding female have an error
#'of 2.
#'
#'\strong{ExperimentID}: Experimental codes are provided in their original
#'format. These still need to be translated into our experimental groups.
#'
#'\strong{Tarsus}: Tarsus is measured using Svennson's Standard in early years
#'and Svennson's Alternative in later years. When Svennson's Alternative is
#'available this is used, otherwise we use converted Svensson's Standard, using
#'\code{\link[HNBStandFormat]{convert_tarsus}}.
#'
#'\strong{Age}: For Age_observed: \itemize{
#'\item If a capture has a recorded
#'ChickAge or the Capture Type is listed as 'chick' it is given a EURING code 1:
#'nestling or chick, unable to fly freely, still able to be caught by hand.
#'\item Recorded value 1 (first calendar year) is given a EURING code 3:
#'full-grown bird hatched in the breeding season of this calendar year.
#'\item Recorded value 2 (second calendar year) is given a EURING code 5:
#'a bird hatched last calendar year and now in its second calendar year.
#'\item Recorded value 3 (>first calendar year) is given a EURING code 4:
#'full-grown bird hatched before this calendar year;
#'year of hatching otherwise unknown.
#'\item Recorded value 4 (>second calendar year) is given a EURING code 6:
#'full-grown bird hatched before last calendar year; year of hatching otherwise
#'unknown.
#'\item After discussing with data owners, recorded value 5 (full grown age unknown)
#'is given NA.
#'
#'For Age_calculated \itemize{
#'\item Any capture record with EURING <= 3 is considered to have a known age
#'(i.e. EURING codes 5, 7, 9, etc.). We consider identification in the nest
#'or as fledglings to be reliable indicators of year of hatching.
#'\item Any capture record with EURING >3 is considered to have an uncertain age
#'(i.e. EURING codes 4, 6, 8, etc.). We consider aging of adults to be too
#'uncertain.
#'}
#'}
#'
#'\strong{Latitude and Longitude}: Location data is stored in Lambert72 CRS.
#'This has been converted to WGS84 to be standard with other systems.
#'
#'@inheritParams pipeline_params
#'
#'@return Generates 4 .csv files with data in a standard format.
#'@export

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

  ## Rename columns
  BROOD_info <- dplyr::rename(BROOD_info, BroodID = NN, Species = SOORT, Plot = GB, NestboxNumber = PL,
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
                              LocationID = gbpl)

  #Rename columns to make it easier to understand
  CAPTURE_info <- dplyr::rename(CAPTURE_info, Species = SOORT,
                                IndvID = RN, MetalRingStatus = NRN,
                                ColourRing = KLR, ColourRingStatus = NKLR,
                                TagType = TAGTY, TagID = TAG,
                                TagStatus = NTAG, BroodID = NN,
                                Sex = GS, CaptureDate = VD,
                                Age_observed = LT,
                                CapturePlot = GB, NestBoxNumber = PL,
                                CaptureMethod = VW,
                                ObserverID = ME, WingLength = VLL,
                                Mass = GEW, CaptureTime = UUR,
                                TarsusStandard = TA,
                                BeakLength = BL, BeakHeight = BH,
                                MoultStatus = DMVL,
                                DNASample = BLOED,
                                MoultScore = RUI, Comments = COMM,
                                PrimaryKey = RECNUM, SplitRing = SPLIT,
                                TarsusAlt = TANEW, Longitude = COORX,
                                Latitude = COORY, CapturePopID = SA,
                                Ticks = TEEK, ExperimentID = exp,
                                OldColourRing = klr_old,
                                LocationID = gbpl)

  INDV_info <- dplyr::rename(INDV_info, Species = soort,
                             IndvID = rn, Sex = sex,
                             BirthYear = gbj, BirthYearKnown = cgj,
                             CaptureType = mode, PlotID = gb,
                             FirstCaptureDate = vd,
                             MetalRingStatus = nrn,
                             TotalRecords = n,
                             TagCode = pit, TagPlacementDate = pitdate,
                             ColourRing1 = klr1, ColourRing1PlacementDate = klr1date,
                             ColourRing2 = klr2, ColourRing2PlacementDate = klr2date,
                             ColourRing3 = klr3, ColourRing3PlacementDate = klr3date,
                             ColourRing4 = klr4, ColourRing4PlacementDate = klr4date,
                             MolecularSex = molsex, MedianWingLength = vll_med,
                             NrWingLengthObservations = vll_n,
                             MedianTarsusStandard = cta_med,
                             NrTarsusStandardObservations = cta_n,
                             MedianTarsusAlt = ctanew_med,
                             NrTarsusAltObservations = ctanew_n,
                             MedianAllTarsus = tarsus_med,
                             AllTarsusObservations = tarsus_n,
                             TarsusMethod = tarsus_ty)

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  Brood_data <- create_brood_UAN(BROOD_info, species)

  ################
  # CAPTURE DATA #
  ################

  print("Compiling capture information...")

  Capture_data <- create_capture_UAN(CAPTURE_info, species)

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

###################################################################################################

create_capture_UAN <- function(data, species_filter){

  #Rename columns to make it easier to understand
  data <- dplyr::rename(data, Species = SOORT,
                        IndvID = RN, MetalRingStatus = NRN,
                        ColourRing = KLR, ColourRingStatus = NKLR,
                        TagType = TAGTY, TagID = TAG,
                        TagStatus = NTAG, BroodID = NN,
                        Sex = GS, CaptureDate = VD,
                        Age_observed = LT,
                        CapturePlot = GB, LocationID = PL,
                        CaptureMethod = VW,
                        ObserverID = ME, WingLength = VLL,
                        Mass = GEW, CaptureTime = UUR,
                        TarsusStandard = TA,
                        BeakLength = BL, BeakHeight = BH,
                        MoultStatus = DMVL,
                        DNASample = BLOED,
                        MoultScore = RUI, Comments = COMM,
                        PrimaryKey = RECNUM, SplitRing = SPLIT,
                        TarsusAlt = TANEW, Longitude = COORX,
                        Latitude = COORY, CapturePopID = SA,
                        Ticks = TEEK, ExperimentID = exp,
                        OldColourRing = klr_old,
                        UniqueLocationCode = gbpl)

  #Capture data includes all times an individual was captured (with measurements
  #like mass, tarsus etc.). This will include first capture as nestling (for
  #residents) This means there will be multiple records for a single individual.

  pb <- dplyr::progress_estimated(n = nrow(data))

  Capture_data <- data %>%
    #Adjust species and PopID
    dplyr::mutate(CapturePopID = dplyr::case_when(.$CapturePopID == "FR" ~ "BOS",
                                           .$CapturePopID == "PB" ~ "PEE"),
                  Species = dplyr::case_when(.$Species == "pm" ~ Species_codes[which(Species_codes$SpeciesID == 14640), ]$Code,
                                             .$Species == "pc" ~ Species_codes[which(Species_codes$SpeciesID == 14620), ]$Code)) %>%
    #Filter by species
    dplyr::filter(Species %in% species_filter) %>%
    #Make tarsus length into standard method (Svensson Alt)
    #Firstly, convert the Svennson's standard measures to Svennson's Alt.
    #Then only use this converted measure when actual Svennson's Alt is unavailable.
    dplyr::mutate(TarsusStandard = convert_tarsus(TarsusStandard, method = "Standard")) %>%
    #Add tarsus and original tarsus method with bind_cols
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(SvenStd = .$TarsusStandard, SvenAlt = .$TarsusAlt),
                                     .f = function(SvenStd, SvenAlt){

                                       if(!is.na(SvenAlt)){

                                         return(tibble::tibble(Tarsus = SvenAlt, OriginalTarsusMethod = "Alternative"))

                                       } else if(!is.na(SvenStd)){

                                         return(tibble::tibble(Tarsus = SvenStd, OriginalTarsusMethod = "Standard"))

                                       } else {

                                         return(tibble::tibble(Tarsus = NA, OriginalTarsusMethod = NA))

                                       }})) %>%
    #Create NAs and convert date/time
    dplyr::mutate(CaptureDate = lubridate::ymd(CaptureDate),
                  BreedingSeason = lubridate::year(CaptureDate),
                  CaptureTime = na_if(paste(CaptureTime %/% 1,
                                            stringr::str_pad(string = round((CaptureTime %% 1)*60),
                                                             width = 2,
                                                             pad = "0"), sep = ":"), "NA:NA"),
                  ReleasePopID = CapturePopID, ReleasePlot = CapturePlot) %>%
    #Calculate age at capture and chick age based on the LT column
    dplyr::bind_cols(purrr::pmap_dfr(.l = list(.$Age_observed, .$CaptureMethod),
                                     .f = ~{

                                       pb$print()$tick()

                                       # If Age (LT) was not recorded
                                       # instead estimate age from the capture type:
                                       if(is.na(..1) | ..1 == 0){

                                         #Capture type P and PP are chicks in the nest
                                         if(..2 %in% c("P", "PP")){

                                           return(tibble::tibble(Age_observed = 1, ChickAge = NA))

                                           #Captures in mist nets, observed rings, cage traps, roost checks
                                           #must be able to fly. But these can be anything from fledglings
                                           #in first calendar year +
                                           #Comment this out because it's really age calculated rather than observed
                                         # } else if (.y %in% c("FU", "GE", "MN", "ON", "NO", "SK", "SL", "LS")){
                                         #
                                         #   return(tibble::tibble(Age_observed = 2, ChickAge = NA))

                                         } else {

                                           #If no age and no chick capture type is given, then observed age is unknown.
                                           return(tibble::tibble(Age_observed = NA, ChickAge = NA))

                                         }

                                       }

                                       #If age is > 5 this is the chick age in days
                                       if(..1 > 5){

                                         return(tibble::tibble(Age_observed = 1, ChickAge = ..1))

                                       } else {

                                         #If it's 1-5 then we translate into EURING codes for adults
                                         if(..1 %in% c(1, 2)){

                                           return(tibble::tibble(Age_observed = 1 + ..1*2, ChickAge = NA))

                                         } else if (..1 %in% c(3, 4)) {

                                           return(tibble::tibble(Age_observed = 4 + (..1 - 3)*2, ChickAge = NA))

                                         } else {

                                           return(tibble::tibble(Age_observed = NA, ChickAge = NA))

                                         }

                                       }

                                     })) %>%
    #Determine age at first capture for every individual
    dplyr::mutate(ischick = dplyr::case_when(.$Age_observed <= 3 ~ 1)) %>%
    calc_age(ID = IndvID, Age = ischick, Date = CaptureDate, Year = BreedingSeason) %>%
    #Arrange columns
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime,
                  ObserverID, LocationID, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge)

  return(Capture_data)

}

#############################################################################################
create_location_UAN <- function(data){

  Location_data <- BOX_info %>%
    dplyr::transmute(LocationID = GBPL, NestboxID = GBPL,
                     PopID = dplyr::case_when(.$SA == "FR" ~ "BOS",
                                              .$SA == "PB" ~ "PEE"),
                     Latitude = Y_deg, Longitude = X_deg,
                     StartSeason = YEARFIRST, EndSeason = YEARLAST,
                     Habitat = "deciduous",
                     HasCoords = as.factor(!is.na(Latitude))) %>%
    #Split into two groups whether they have coordinates or not
    split(f = .$HasCoords)

  #For the group with coordinates, turn it into an sf object
  #and change the CRS to be WGS84
  true_coords <- sf::st_as_sf(Location_data$'TRUE',
                              coords = c("Longitude", "Latitude"),
                              crs = 31370) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_coordinates()

  Location_data$'TRUE'$Longitude <- true_coords[, 1]
  Location_data$'TRUE'$Latitude <- true_coords[, 2]

  Location_data <- dplyr::bind_rows(Location_data) %>%
    dplyr::select(-HasCoords)

  return(Location_data)

}
