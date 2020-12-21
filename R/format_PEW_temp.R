#' Construct standard summary for data from Peerdsbos West, Belgium.
#' Actively started 27/11/2020



#### ---------------------------------------------------------------------------------------~
#### BEFORE CLOSING THE SCRIPT/PROJECT, COMMENT ALL ROWS, TO BE ABLE TO LOAD ALL FUNCTIONS NEXT TIME
#### ---------------------------------------------------------------------------------------~



# #### ---------------------------------------------------------------------------------------~
# #### SHOULD BE BASED ON THE NEW V. 1.1.0 OF THE STANDARD PROTOCOL
# #### GET ALL THE VARIABLES FROM THE PROTOCOL PDF
# line <- readLines("C:/Users/ZuzanaZ/Dropbox/GITHUB_PROJECTS/SPI-Birds/notes_zz/prot_names_v110.txt")
# prot_names <- gsub(pattern = "[[:digit:]]", replacement = "", x = line)
# prot_names <- gsub(pattern = "\\.", replacement = "", x = prot_names)
# prot_names <- trimws(prot_names)
#
# prot_names
#
# Individual_data_names <- prot_names[2:10]
# Brood_data_names      <- prot_names[12:47]
# Capture_data_names    <- prot_names[49:71]
# Location_data_names   <- prot_names[73:81]
# #### ---------------------------------------------------------------------------------------~





#### ---------------------------------------------------------------------------------------~
#### Questions/Doubts DATA OWNERS
# What is "Neophobia Transponder"?
# OriginalTarsusMethod
# Do they have coordinates for each nestbox?
# End season in location data: 2019 or NA (do they remove them or not?)


#### Questions/Doubts TEAM
# calc_clutchsize only for old version of protocol names of variables?



#### ---------------------------------------------------------------------------------------~

#### HELPDOC
# What do do with observations without ID?
# pew_data %>%
#   filter(is.na(IndvID)) %>%
#   # View()
#   pull(NestboxID)
# There are 4 observations without ID, we remove those observations,
# as any additional information is provided (body measurements, etc.)
# Observations are from 2017, nestboxes 97, 98, 102, kk12.


#### NOTES:
#### Experiment "BSM - Griffioen et al. 2019 PeerJ" was done with hatched nestlings, not with eggs.


#### ---------------------------------------------------------------------------------------~


library(pipelines)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

db <- choose_directory() ## temp copy in C:\Users\ZuzanaZ\Dropbox\POSTDOC\POSTDOC_NIOO_NETHERLANDS_2020\SPI-birds_project\data_pipeline_temp_copy\PEW_PEWrdsbosWest_Belgium
db <- paste0(db, "/PEW_PrimaryData.xlsx")

message("Importing primary data...")

pew_data <- readxl::read_excel(db,
                               col_types = c("text", "text", "text",
                                             "text", "text", "numeric", "date",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "text", "text",
                                             "text", "numeric", "date", "date",
                                             "text", "date", "numeric", "numeric",
                                             "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric")) %>%
  janitor::clean_names(case = "upper_camel") %>%
  janitor::remove_empty(which = "rows") %>%
  #### Solve NA values (some are explicitly stated as character "NA")
  dplyr::mutate(across(where(is.character), .fns = ~replace(., . ==  "NA" , NA))) %>%
  #### Convert to corresponding format and rename
  dplyr::mutate(BreedingSeason = as.integer(Year),
                CaptureDate = as.Date(Date),
                Tarsus = as.numeric(Tarsus),
                Mass = as.numeric(Mass),
                TarsusPartner = as.numeric(TarsusPartner),
                MassPartner = as.numeric(MassPartner),
                ClutchSize = as.integer(ClutchSize),
                DateEgg1 = as.Date(DateEgg1),
                HatchDateD0 = as.Date(HatchDateD0),
                NumberOfRingedChicks = as.integer(NumberOfRingedChicks),
                DateRingingChicks = as.Date(DateRingingChicks),
                NoOfChicksD3 = as.integer(NoOfChicksD3),
                BroodMassD3 = as.numeric(BroodMassD3),
                ObservationTimeH = as.numeric(ObservationTimeH),
                # Seks = substr(Seks, 1, 1),
                Species = "CYACAE",
                PopID = "PEW",
                NestboxID = tolower(Nest),
                BroodID = ifelse(Method %in% c("Catch adults nestbox", "ChickRinging"),
                                 paste(Year, NestboxID, "PEW", sep = "_"), NA)) %>%
  #### Rename variables to standardized format
  dplyr::rename(IndvID = Id,
                Sex =  Seks,
                ObserverID = Measurer) %>%
  #### Remove columns which we do not store in the standardized format
  dplyr::select(-FeatherCollection ,
                -BreathRateTime50Breaths,
                -FeathersPartner,
                -BreathRatePartnerTime50Breaths,
                -BloodSample,
                -TimeBloodSample,
                -BloodSampleDuration,
                -VisitRateVisitsH,
                -VrPartner,
                -VisitsAlternated,
                -VisitsAlternatedPartner,
                -VisitsSync10,
                -ChickAgeOfBehavObserv,
                -MateStrategy,
                -Nest) %>%
  #### Reorder columns
  dplyr::select(BreedingSeason,
                Species,
                PopID,
                IndvID,
                Sex,
                Age,
                everything()) %>%
  #### Remove observations without ID
  dplyr::filter(!is.na(IndvID)) %>%
  #### Details
  dplyr::mutate(PartnerId = ifelse(PartnerId == "MetalRing_FemK89_2019",
                                   "MetalRing_F2019_K89", PartnerId))





#### --------------------------------------~
#### BROOD DATA                         #### done, need check
#### --------------------------------------~


message("Compiling brood information...")

Brood_data <- create_brood_PEW(data = pew_data)


create_brood_PEW <- function(data) {

  parents_brood_data <-
    data %>%
    #### Exclude non-breeding data, exclude chicks
    #### Use only CatchAdultsNestbox, where all relevant information is.
    #### However, for one nest in the clutch size is only in the CatchIncubation Method !!
    #### Ignore for now
    filter(Method == "Catch adults nestbox") %>%
    #### Rename variables
    dplyr::rename(LocationID = NestboxID) %>%


    ####
    # Warning message: ??
    # Values are not uniquely identified; output will contain list-cols.
    # May use this way after fixing same-sex couples (see below)
    # tidyr::pivot_wider(names_from = Sex,
    #                    values_from = IndvID) %>%
    # dplyr::rename(FemaleID = Female,
    #               MaleID = Male) %>%
    # mutate(FemaleID = ifelse(Sex == "Female", IndvID, NA_character_),
    #        MaleID = ifelse(Sex == "Male", IndvID, NA_character_))
    ####

  #### Get IDs of females and males for each breeding couple
  #### Assuming that if female id is noted, the partner id is male (vice versa)
  #### CHECK: There are cases where there are NAs for male or female ID in the clutch
  dplyr::mutate(FemaleID = case_when(Sex == "Female" ~ IndvID,
                                     Sex == "Male" ~ PartnerId),
                MaleID = case_when(Sex == "Male" ~ IndvID,
                                   Sex == "Female" ~ PartnerId),
                ExperimentID = dplyr::case_when(Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                  "COHORT; PARENTAGE",
                                                Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                  "SURVIVAL; PARENTAGE")) %>%
    #### Remove unnecessary variables which may cause duplicated rows
    #### Exlcude also Date column, as for few broods, there may be
    #### several catches of parents, but the brood parameters are the same
    dplyr::select(-c(Date, IndvID, Sex, Age, PartnerId, NumberTransponder,
                     NewRing, CaptureDate, NeophobiaTransponder,
                     Tarsus, Mass, AgePartner, TarsusPartner, MassPartner,
                     ObserverID, ObservationTimeH, Experiment)) %>%
    #### Remove duplicated rows (as we get one row for males and females for the same brood)
    dplyr::distinct() %>%
    #### Remove rows with no information about the brood
    filter(!(is.na(ClutchSize) & is.na(DateEgg1) & is.na(HatchDateD0) &
               is.na(NumberOfRingedChicks) & is.na(DateRingingChicks) &
               is.na(NoOfChicksD3) & is.na(BroodMassD3))) %>%

    #### CHECK: Remove this after solving the issue below
    #### Identify possible errors or duplicates (rank >1)
    group_by(BroodID) %>%
    mutate(rank = row_number()) %>%
    ungroup()


  #### SOLVE THIS  >> ask data owners (3 BROODS)
  # There are some couples where IvdvID belongs to female, and PartnerId
  # (when checked as IndvID) also belongs to female (or both belong to males).
  # How to deal with that?
  #
  #  2015_24_PEW: IndvId: 13619319  Female PartnerId: 11714676
  #                       11714676  Female PartnerId: 13619319
  #  2015_24_PEW both females
  #  2016_60_PEW both females
  #
  #  2017_49_PEW 12706106 is male, but this way ends up as female, 13617031 is male,
  #               13619466 is female, 13617031 is male >> there is actually female with male,
  #               in other record the same male with other male as partner
  ####

  #### Wait to resolve the above, so far keep only one of each conflicted records
  #### After resolving, remove these 3 lines of the below code and carry on in the "parents_brood_data" pipes
  parents_brood_data_2 <-
    parents_brood_data %>%
    dplyr::filter(rank == 1) %>%
    #### Create new variables
    dplyr::mutate(Plot = NA_character_,
                  # LayDate_observed = DateEgg1, ## for new version of calc_clutchtype
                  LayDate = DateEgg1,
                  LayDate_min = NA_character_,
                  LayDate_max = NA_character_,
                  ClutchSize_observed = ClutchSize,
                  ClutchSize_min = NA_integer_,
                  ClutchSize_max = NA_integer_,
                  HatchDate_observed = HatchDateD0,
                  HatchDate_min = NA_character_,
                  HatchDate_max = NA_character_,
                  BroodSize_observed = NA_integer_,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = NA_character_,
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  #### ASSUMPTION: There is no specific information about the
                  # number of chicks fledged, use the number of chicks ringed
                  # !? in few cases this number is higher than clutch size
                  # NumberFledged_observed = NumberOfRingedChicks, ## for new version of calc_clutchtype
                  NumberFledged = NumberOfRingedChicks,
                  NumberFledged_min = NA_integer_,
                  NumberFledged_max = NA_integer_,
                  AvgEggMass = NA,
                  NumberEggs = NA_integer_,
                  #### Metadata states that only the first clutches are recorded
                  ClutchType_observed = "first") %>%
  dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE))


  #### Get chick measurements per brood
  chicks_measurements <-
    data %>%
    filter(Method == "ChickRinging") %>%
    dplyr::select(IndvID, BroodID, Tarsus, Mass) %>%
    group_by(BroodID) %>%
    dplyr::summarise(#### NOTE: There are some data referred as BroodMassD3,
                      # I do not use them.
                      AvgChickMass = mean(as.numeric(Mass), na.rm = TRUE),
                      NumberChicksMass = sum(!is.na(Mass)),
                      AvgTarsus = mean(as.numeric(Tarsus), na.rm = TRUE),
                      NumberChicksTarsus = sum(!is.na(Tarsus)),
                      OriginalTarsusMethod = NA_character_) %>%
    ungroup() %>%
    dplyr::mutate(AvgTarsus = ifelse(AvgTarsus == "NaN", NA, AvgTarsus),
                  AvgChickMass = ifelse(AvgChickMass == "NaN", NA, AvgChickMass),
                  NumberChicksMass = ifelse(NumberChicksMass == 0, NA, NumberChicksMass),
                  NumberChicksTarsus = ifelse(NumberChicksTarsus == 0, NA, NumberChicksTarsus))


  #### Join parents and chick data
  Brood_data <-
    parents_brood_data_2 %>%
    left_join(chicks_measurements, by = "BroodID") %>%
    #### Rename
    dplyr::rename(LayDate_observed = LayDate,
                  NumberFledged_observed = NumberFledged) %>%
    #### Final arrangement
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot, LocationID,
                  FemaleID, MaleID,
                  ClutchType_observed, ClutchType_calculated,
                  LayDate_observed, LayDate_min, LayDate_max,
                  ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                  HatchDate_observed, HatchDate_min, HatchDate_max,
                  BroodSize_observed, BroodSize_min, BroodSize_max,
                  FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                  NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                  AvgEggMass, NumberEggs, AvgChickMass, NumberChicksMass,
                  AvgTarsus, NumberChicksTarsus, OriginalTarsusMethod, ExperimentID)

  return(Brood_data)

}

#### VARIABLES
# cat(Brood_data_names, sep = "\n")




#### --------------------------------------~
#### CAPTURUE DATA                      #### done
#### --------------------------------------~

# AGE_CALCULATED SEEMS NOT CORRECT
# (maybe also because the Age_observed was not correctly assigned?)

# Liam:
#   Seems to me that there are some records that are not captures (i.e. they don't have a date)
#   but just include brood information. So, I would explicitly remove the records with no date
#   from Capture_data but still use them in Brood_data.
#   In other cases where there are multiple captures in a year (with dates)
#   these should be treated as legitimate multiple captures.




Capture_data <- create_capture_PEW(pew_data, Brood_data)


create_capture_PEW <- function(pew_data, Brood_data) {

  Brood_data_sel <-
    Brood_data %>%
    dplyr::select(BreedingSeason, Species, PopID, BroodID, LocationID,
                  HatchDate_observed)


  Capture_data_temp <-
    pew_data %>%
    #### Rename variables
    dplyr::rename(LocationID = NestboxID) %>%
    dplyr::select(-DateRingingChicks) %>%

    #### Create new variables
    dplyr::mutate(#### ASSUMPTION: There is no captureID info, so I made this up
                  CaptureID = paste(IndvID, LocationID,
                                    paste0(lubridate::year(CaptureDate),
                                           sprintf("%02d", lubridate::month(CaptureDate)),
                                           sprintf("%02d", lubridate::day(CaptureDate))),
                                    sep = "_"),
      #### ASSUMPTION: Some times are strange, to simplify, round to whole hour
      #### ? do they correspond only to the experimental observations? keep/remove?
      CaptureTime = ifelse(is.na(ObservationTimeH),
                           NA,
                           paste(sprintf("%02d", round(ObservationTimeH, 0)),
                                 "00", sep = ":")),
      Sex_observed = ifelse(Sex == "Chick", NA_character_, substr(Sex, 1, 1)),
      CaptureAlive = ifelse(Method != "Found dead", TRUE, FALSE),
      #### ASSUMPTION: the same as CaptureAlive, as there is no additional data
      ReleaseAlive = CaptureAlive,
      CapturePopID = PopID,
      CapturePlot = NA_character_,
      ReleasePopID = ifelse(ReleaseAlive == TRUE, CapturePopID, NA_character_),
      ReleasePlot = ifelse(ReleaseAlive == TRUE, CapturePlot, NA_character_),
      #### ASSUMPTION: use NA, as there is no information
      OriginalTarsusMethod = NA_character_,
      WingLength = NA,
      ExperimentID = dplyr::case_when(Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                        "COHORT; PARENTAGE",
                                      Experiment == "2h Temp D4" ~ "SURVIVAL",
                                      Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                        "SURVIVAL; PARENTAGE"),
      #### NOT SURE ABOUT THIS
      Age_observed = case_when(Age == "0" & Method == "ChickRinging" ~ 1,
                               Age == "1" ~ 5,
                               Age == ">=2" ~ 7,
                               #### Account for controls in December
                               Age == "1" & Method == "Night Control Winter" &
                                 lubridate::month(CaptureDate) == 12 ~ 6,
                               #### Account for controls in January & February
                               Age == ">=2" & Method == "Night Control Winter" &
                                 lubridate::month(CaptureDate) %in% c(1, 2) ~ 8,
                               is.na(Age) ~ 5),
      Age_observed = as.integer(Age_observed))


  Capture_data <-
    Capture_data_temp %>%
    left_join(Brood_data_sel,
              by = c("BreedingSeason", "Species", "PopID", "LocationID", "BroodID")) %>%
    #### The age of captured chicks in days since hatching
    dplyr::mutate(ChickAge = if_else(Age_observed == 1,
                                     as.integer(difftime(CaptureDate, HatchDate_observed)),
                                     NA_integer_),
                  BroodIDLaid = BroodID) %>%

    #### NEW VERSION OF THE FUNCTION (TO FINISH!!)
    # dplyr::mutate(Age_calculated = calc_age())

    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate,
             Year = BreedingSeason, showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(CaptureID, IndvID, Species, Sex_observed, BreedingSeason,
                  CaptureDate, CaptureTime, ObserverID, LocationID,
                  CaptureAlive, ReleaseAlive, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge, ExperimentID)

    # #### NOTE: KEEP IN CAPTURE DATA TO BE INCLUDED IN INDIVIDUAL INFORMATION,
    # #### BUT REMOVE IT BEFORE FINAL OUPUT!!
    # #### Remove records with no date
    # #### CHECK: doing this, almost 500 records of chicks will be removed,
    # #### as there is no CaptureDate !!!
    # filter(!is.na(CaptureDate))

  return(Capture_data)

}

#### VARIABLES
# cat(Capture_data_names, sep="\n")


#### --------------------------------------~
#### INDIVIDUAL DATA                    #### IN PROCESS, need to check
#### --------------------------------------~

#### Still to solve:
# There are some individuals (for example 13617043), where the rows of data related
# to the same individual seem complementary, but are in two rows with
# insufficient information when treating the rows separately.
# How to deal with that?
# >>> So far, I remove duplicated rows at the end of the function,
# but there are some rows still maintained, as there is some difference in one
# of the columns of the dataframe.
#
# Remove data with no date in the primary data?



message("Compiling individual information...")

Individual_data <- create_individual_PEW(Capture_data)

# dplyr::filter(Species %in% species)



#### change the function to get input from Capture data, not from pew data


create_individual_PEW <- function(data = Capture_data){

  Individual_data <-
    data %>%
    #### NOTE: Keep rows without Capture date
    #### Format and create new data columns
    dplyr::mutate(RingAge = ifelse(Age_observed == 1, "chick", "adult"),
                  #### Theoretical, we actually do not have LaidDate, for several nests
                  # there is only DateEgg1.
                  # In metadata, there is a note that second and other broods are not recorded,
                  # so can we assume that each brood is the first and only one?
                  # BroodIDLaid = ifelse(RingAge == "chick",
                  #                      paste(BreedingSeason, LocationID,
                  #                            paste0(lubridate::year(LaidDate),
                  #                                   sprintf("%02d", lubridate::month(LaidDate)),
                  #                                   sprintf("%02d", lubridate::day(LaidDate))),
                  #                            sep = "_"),
                  #                      NA_character_)
                  BroodIDLaid    = ifelse(RingAge == "chick",
                                          paste(BreedingSeason, LocationID, sep = "_"),
                                          NA_character_),
                  BroodIDFledged = BroodIDLaid) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::mutate(Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex_observed))), .f = ~{
                                                            if(length(..1) == 0){
                                                              return(NA_character_)
                                                            } else if(length(..1) == 1){
                                                              return(..1)
                                                            } else {
                                                              return("C")
                                                            }
                                                          }),
                  Sex_genetic = NA_character_) %>%
    dplyr::arrange(CaptureDate) %>%
    dplyr::summarize(Species = first(Species),
                     PopID = "PEW",
                     RingSeason = first(BreedingSeason),
                     RingAge = first(RingAge),
                     BroodIDLaid = first(BroodIDLaid),
                     BroodIDFledged = first(BroodIDFledged),
                     Sex_calculated = first(Sex_calculated),
                     Sex_genetic = first(Sex_genetic)) %>%
    dplyr::ungroup() %>%
    #### Final arrangement
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge, Sex_calculated, Sex_genetic) %>%
    # #### Remove duplicated rows (CHECK)
    dplyr::distinct()

  return(Individual_data)

  }


#### VARIABLES
# cat(Individual_data_names, sep="\n")



#### --------------------------------------~
#### LOCATION DATA                      #### done, check with data owner
#### --------------------------------------~

#### There is no information about coordinates of nestboxes
# >> use the same ? >> ask Data owner

message("Compiling location information...")
Location_data <- create_location_PEW(pew_data)


create_location_PEW <- function(data) {

  Location_data <-
    data %>%
    dplyr::select(BreedingSeason, Date, NestboxID, PopID) %>%
    group_by(NestboxID) %>%
    arrange(BreedingSeason, Date) %>%
    dplyr::summarise(StartSeason = first(BreedingSeason),
                     #### CHECK WITH DATA OWNER
                     #### EndSeason = last(BreedingSeason))
                     EndSeason = NA_character_,
                     LocationID = unique(NestboxID),
                     PopID = unique(PopID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LocationType = "NB",
                  HabitatType = "deciduous",
                  #### CHECK WITH DATA OWNER
                  #### Are there coordinates for each nestbox?
                  Latitude  = 51.266667,
                  Longitude = 4.466667) %>%
    #### Final arrangement
    dplyr::select(LocationID, NestboxID, LocationType, PopID,
                  Latitude, Longitude, StartSeason, EndSeason, HabitatType)

  return(Location_data)

}

#### VARIABLES
# cat(Location_data_names, sep="\n")


