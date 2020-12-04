#' #' Construct standard summary for data from Peerdsbos West, Belgium.
#' #' Actively started 27/11/2020
#'
#'
#'
#' #### ---------------------------------------------------------------------------------------~
#' #### BEFORE CLOSING THE SCRIPT/PROJECT, COMMENT ALL ROWS, TO BE ABLE TO LOAD ALL FUNCTIONS NEXT TIME
#' #### ---------------------------------------------------------------------------------------~
#'
#'
#'
#' # #### ---------------------------------------------------------------------------------------~
#' # #### SHOULD BE BASED ON THE NEW V. 1.1.0 OF THE STANDARD PROTOCOL
#' # #### GET ALL THE VARIABLES FROM THE PROTOCOL PDF
#' # line <- readLines("C:/Users/ZuzanaZ/Dropbox/GITHUB_PROJECTS/SPI-Birds/notes_zz/prot_names_v110.txt")
#' # prot_names <- gsub(pattern = "[[:digit:]]", replacement = "", x = line)
#' # prot_names <- gsub(pattern = "\\.", replacement = "", x = prot_names)
#' # prot_names <- trimws(prot_names)
#' #
#' # prot_names
#' #
#' # Individual_data_names <- prot_names[2:10]
#' # Brood_data_names      <- prot_names[12:47]
#' # Capture_data_names    <- prot_names[49:71]
#' # Location_data_names   <- prot_names[73:81]
#' # #### ---------------------------------------------------------------------------------------~
#'
#'
#'
#'
#'
#' #### ---------------------------------------------------------------------------------------~
#' #### Questions/Doubts DATA OWNERS
#' # What is "Neophobia Transponder"?
#' # Code "Experiment"?
#' # OriginalTarsusMethod
#' # Check experimentID
#'
#' #### Questions/Doubts TEAM
#' # What to do with strange ID, like "MetalRing&Tag_M2019NestK55" (34 individuals)?
#' # What do do with individuals without ID?
#' # Where does the chick-rearing data go? To Individual data?
#'
#' #### ---------------------------------------------------------------------------------------~
#'
#'
#'
#'
#' library(pipelines)
#'
#'
#' db_dir <- choose_directory() ## temp copy in C:\Users\ZuzanaZ\Dropbox\POSTDOC\POSTDOC_NIOO_NETHERLANDS_2020\SPI-birds_project\data_pipeline_temp_copy\PEW_PEWrdsbosWest_Belgium
#' db <- paste0(db_dir, "/PEW_PrimaryData.xlsx")
#'
#' message("Importing primary data...")
#'
#' pew_data <- readxl::read_excel(db,
#'                                col_types = c("text", "text", "text",
#'                                              "text", "text", "numeric", "date",
#'                                              "text", "text", "text", "text", "text",
#'                                              "text", "text", "text", "text", "text",
#'                                              "text", "text", "text", "text", "text",
#'                                              "text", "numeric", "date", "date",
#'                                              "text", "date", "numeric", "numeric",
#'                                              "text", "numeric", "numeric", "numeric",
#'                                              "numeric", "numeric", "numeric",
#'                                              "numeric", "numeric", "numeric")) %>%
#'   janitor::clean_names(case = "upper_camel") %>%
#'   janitor::remove_empty(which = "rows") %>%
#'   #### Solve NA values (some are explicitly stated as character "NA")
#'   dplyr::mutate(across(where(is.character), .fns = ~replace(., . ==  "NA" , NA))) %>%
#'   #### Convert to corresponding format
#'   dplyr::mutate(Year = as.integer(Year),
#'                 Date = as.Date(Date),
#'                 Tarsus = as.numeric(Tarsus),
#'                 Mass = as.numeric(Mass),
#'                 TarsusPartner = as.numeric(TarsusPartner),
#'                 MassPartner = as.numeric(MassPartner),
#'                 ClutchSize = as.integer(ClutchSize),
#'                 DateEgg1 = as.Date(DateEgg1),
#'                 HatchDateD0 = as.Date(HatchDateD0),
#'                 NumberOfRingedChicks = as.integer(NumberOfRingedChicks),
#'                 DateRingingChicks = as.Date(DateRingingChicks),
#'                 NoOfChicksD3 = as.integer(NoOfChicksD3),
#'                 BroodMassD3 = as.numeric(BroodMassD3),
#'                 ObservationTimeH = as.numeric(ObservationTimeH),
#'                 Seks = substr(Seks, 1, 1),
#'                 Species = "CYACAE",
#'                 PopID = "PEW") %>%
#'   #### Rename variables to standardized format
#'   dplyr::rename(BreedingSeason = Year,
#'                 IndvID = Id,
#'                 Sex =  Seks,
#'                 NestboxID = Nest,
#'                 ObserverID = Measurer) %>%
#'   #### Remove columns which we do not store in the standardized format
#'   dplyr::select(-FeatherCollection ,
#'                 -BreathRateTime50Breaths,
#'                 -FeathersPartner,
#'                 -BreathRatePartnerTime50Breaths,
#'                 -BloodSample,
#'                 -TimeBloodSample,
#'                 -BloodSampleDuration,
#'                 # -NewRing,
#'                 -VisitRateVisitsH,
#'                 -VrPartner,
#'                 -VisitsAlternated,
#'                 -VisitsAlternatedPartner,
#'                 -VisitsSync10,
#'                 -ChickAgeOfBehavObserv,
#'                 -MateStrategy) %>%
#'   #### Reorder columns
#'   dplyr::select(BreedingSeason,
#'                 Species,
#'                 PopID,
#'                 IndvID,
#'                 Sex,
#'                 Age,
#'                 everything())
#'
#'
#'
#'
#' #### check columns with NAs
#' # sort(apply(pew_data, 2, function(x) sum(is.na(x))), decreasing = TRUE)
#'
#'
#'
#' #### --------------------------------------~
#' #### INDIVIDUAL DATA                    #### done, need to check
#' #### --------------------------------------~
#'
#' #### Still to solve:
#' # There are some individuals (for example 13617043), where the rows of data related
#' # to the same individual seem complementary, but are in two rows with
#' # insufficient information when treating the rows separately.
#' # How to deal with that?
#' # >>> So far, I remove duplicated rows at the end of the function,
#' # but there are some rows still maintained, as there is some difference in one
#' # of the columns of the dataframe.
#' #
#' # There is no data about broodID, so far I used NA.
#' #
#' # Remove data with no date in the primary data?
#'
#'
#'
#' message("Compiling individual information...")
#'
#' Individual_data <- create_individual_PEW(pew_data)
#'   dplyr::filter(Species %in% species)
#'
#'
#' create_individual_PEW <- function(data){
#'
#'   Individual_data <-
#'     data %>%
#'     #### Remove rows without bird ID
#'     filter(!is.na(IndvID)) %>%
#'     #### CHECK: Remove rows without date
#'     # filter(!is.na(Date)) %>%
#'     #### Format and create new data columns
#'     dplyr::select(IndvID, Species, PopID, Age, Method, BreedingSeason, Date, Sex) %>%
#'     dplyr::rename(RingSeason = BreedingSeason) %>%
#'     dplyr::mutate(RingAge = dplyr::case_when(Age == "0" & Method == "ChickRinging" ~ "chick",
#'                                              Age == "1" ~ "adult",
#'                                              Age == ">=2" ~ "adult"),
#'                   BroodIDLaid    = NA_character_,
#'                   BroodIDFledged = NA_character_) %>%
#'     group_by(IndvID) %>%
#'     dplyr::mutate(Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{
#'                                                             if(length(..1) == 0){
#'                                                               return(NA_character_)
#'                                                             } else if(length(..1) == 1){
#'                                                               return(..1)
#'                                                             } else {
#'                                                               return("C")
#'                                                             }
#'                                                           }),
#'                   Sex_genetic = NA_character_) %>%
#'     dplyr::ungroup() %>%
#'     #### Final arrangement
#'     dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
#'                   RingSeason, RingAge, Sex_calculated, Sex_genetic) %>%
#'     #### Remove duplicated rows (CHECK)
#'     dplyr::distinct()
#'
#'   return(Individual_data)
#'
#'   }
#'
#'
#' #### VARIABLES
#' # cat(Individual_data_names, sep="\n")
#'
#'
#' #### --------------------------------------~
#' #### CAPTURUE DATA                      #### in process
#' #### --------------------------------------~
#'
#'
#' # create_capture_PEW <- function(data){
#'
#' Capture_data <-
#'   pew_data %>%
#'   #### ASSUMPTION: Date is obligatory, not allowing for NAs,
#'   # therefore we exclude observations without the Date
#'   # (CHECK, as we may lose some information)
#'   filter(!(is.na(Date))) %>%
#'   #### Rename variables
#'   dplyr::rename(CaptureDate = Date) %>%
#'   #### Create new variables
#'   mutate(#### ASSUMPTION: There is no captureID info, so far I use this (CHECK)
#'          CaptureID = paste(IndvID, "CaptureNumber", sep = "_"),
#'          #### ASSUMPTION: Some times are strange, to simplify, round to whole hour
#'          CaptureTime = ifelse(is.na(ObservationTimeH),
#'                               NA,
#'                               paste(sprintf("%02d", round(ObservationTimeH, 0)),
#'                                     "00", sep = ":")),
#'          Sex_observed = Sex,
#'          LocationID = NA_character_,
#'          CaptureAlive = ifelse(Method != "Found dead", TRUE, FALSE),
#'          #### ASSUMPTION: the same as CaptureAlive, as there is no additional data
#'          ReleaseAlive = CaptureAlive,
#'          CapturePopID = PopID,
#'          CapturePlot = NA_character_,
#'          ReleasePopID = ifelse(ReleaseAlive == TRUE, CapturePopID, NA_character_),
#'          ReleasePlot = ifelse(ReleaseAlive == TRUE, CapturePlot, NA_character_),
#'          #### ASSUMPTION: use Alternative, as there is no information
#'          OriginalTarsusMethod = "Alternative",
#'          WingLength = NA_integer_,
#'          ExperimentID = dplyr::case_when(Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
#'                                            "COHORT; PARENTAGE",
#'                                          Experiment == "2h Temp D4" ~ "SURVIVAL",
#'                                          Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
#'                                            "SURVIVAL; PARENTAGE")
#'          #### VARIABLES TO FINISH:
#'          # Age_observed =
#'          # Age_calculated
#'          # ChickAge
#'
#'
#'
#'   )
#'
#'
#' # #### Final arrangement
#' # dplyr::select(CaptureID, IndvID, Species, Sex_observed, BreedingSeason,
#' #               CaptureDate, CaptureTime, ObserverID, LocationID,
#' #               CaptureAlive, ReleaseAlive, CapturePopID, CapturePlot,
#' #               ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
#' #               WingLength, Age_observed, Age_calculated, ChickAge, ExperimentID)
#'
#'
#'
#' # }
#'
#'
#'
#' #### VARIOUS ASSUMPTIONS:
#' # For Age: - there are some idividuals with NA age, but with partner and the DateRingingChick
#' #             >> we assume they are adults (example IndvID 12706844)
#' # For CaptureDate: - there are some individuals with NA in Date, but in some there is DateRingingChick,
#' #                    can we use that and assume that as Date?
#' #                  - so far, remove, individuals without Date, as this is obligatory based on the Protocol
#' # For CaptureTime: - Some times are strange, to simplify, round to whole hour for now
#' # For OriginalTarsusMethod: - no data, for now assume "Alternative"
#'
#'
#' #### VARIABLES
#' # cat(Capture_data_names, sep="\n")
#' # CaptureID         check (should be IndvID_CaptureNumber)
#' # IndvID            ok
#' # Species           check
#' # Sex_observed      ok
#' # BreedingSeason    ok (year)
#' # CaptureDate       check (for now, removing those without date)
#' # CaptureTime       check (for now, using ObservationTimeH variable, majority NAs)
#' # ObserverID        ok
#' # LocationID        check (for now, NA)
#' # CaptureAlive      ok
#' # ReleaseAlive      check (for now, the same as CaptrueAlive)
#' # CapturePopID      ok
#' # CapturePlot       ok
#' # ReleasePopID      ok
#' # ReleasePlot       ok
#' # Mass              ok
#' # Tarsus            ok
#' # OriginalTarsusMethod   check (for now, use Alternative, as there is no information)
#' # WingLength        ok (NA, no data)
#' # Age_observed
#' # Age_calculated
#' # ChickAge
#' # ExperimentID      check (verify if the catergories are correctly assigned)
#'
#'
#'
#'
#' #### --------------------------------------~
#' #### BROOD DATA                         ####
#' #### --------------------------------------~
#'
#'
#' #### Exclude not breeding data
#' # filter(Method != "Night Control Winter")
#'
#'
#'
#' message("Compiling brood information...")
#' Brood_data <- create_brood_PEW(data)
#'
#'
#' create_brood_PEW <- function(data){
#'
#'
#'
#'
#'
#'
#'   # #### Final arrangement
#'   # dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID,
#'   #               FemaleID, MaleID, ClutchType_observed, ClutchType_calculated,
#'   #               LayDate_observed, LayDate_min, LayDate_max,
#'   #               ClutchSize_observed, ClutchSize_min, ClutchSize_max,
#'   #               HatchDate_observed, HatchDate_min, HatchDate_max,
#'   #               BroodSize_observed, BroodSize_min, BroodSize_max,
#'   #               FledgeDate_observed, FledgeDate_min, FledgeDate_max,
#'   #               NumberFledged_observed, NumberFledged_min, NumberFledged_max,
#'   #               AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass,
#'   #               AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)
#'
#'
#'
#' }
#'
#'
#'
#'
#'
#' #### VARIABLES
#' # cat(Brood_data_names, sep="\n")
#' # BroodID
#' # PopID
#' # BreedingSeason
#' # Species
#' # Plot
#' # LocationID
#' # FemaleID
#' # MaleID
#' # ClutchType_observed
#' # ClutchType_calculated
#' # LayDate_observed
#' # LayDate_min
#' # LayDate_max
#' # ClutchSize_observed
#' # ClutchSize_min
#' # ClutchSize_max
#' # HatchDate_observed
#' # HatchDate_min
#' # HatchDate_max
#' # BroodSize_observed
#' # BroodSize_min
#' # BroodSize_max
#' # FledgeDate_observed
#' # FledgeDate_min
#' # FledgeDate_max
#' # NumberFledged_observed
#' # NumberFledged_min
#' # NumberFledged_max
#' # AvgEggMass NumberEggs
#' # AvgChickMass
#' # NumberChicksMass
#' # AvgTarsus
#' # NumberChicksTarsus
#' # OriginalTarsusMethod
#' # ExperimentID
#'
#'
#'
#' #### --------------------------------------~
#' #### LOCATION DATA                      #### done, check
#' #### --------------------------------------~
#'
#' #### There is no information about coordinates of nestboxes >> use the same of location ?
#' #### LocationID >> assume is the same as LocationID ?
#'
#' message("Compiling location information...")
#' Location_data <- create_location_PEW(pew_data)
#'
#'
#' create_location_PEW <- function(data){
#'
#'   Location_data <-
#'     data %>%
#'     dplyr::select(BreedingSeason, Date, NestboxID, PopID) %>%
#'     group_by(NestboxID) %>%
#'     arrange(BreedingSeason, Date) %>%
#'     dplyr::mutate(StartSeason = first(BreedingSeason),
#'                   EndSeason = last(BreedingSeason)) %>%
#'     select(-BreedingSeason, -Date) %>%
#'     dplyr::distinct() %>%
#'     dplyr::ungroup() %>%
#'     dplyr::mutate(LocationID = NestboxID, # CHECK
#'                   LocationType = "NB",
#'                   Latitude  = 51.266667,
#'                   Longitude = 4.466667,
#'                   HabitatType = "deciduous") %>%
#'     #### Final arrangement
#'     dplyr::select(LocationID, NestboxID, LocationType, PopID,
#'                   Latitude, Longitude, StartSeason, EndSeason, HabitatType)
#'
#'   return(Location_data)
#'
#' }
#'
#' #### VARIABLES
#' # cat(Location_data_names, sep="\n")
#'
#'
