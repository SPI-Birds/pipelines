#' Construct standard summary for data from Peerdsbos West, Belgium.
#' Actively started 27/11/2020


#### ---------------------------------------------------------------------------------------~
#### Questions/Doubts DATA OWNERS
# OriginalTarsusMethod
# Do they have coordinates for each nestbox?
# Do Observation hours correspond only to experimental data? Keep or remove them?



#### ---------------------------------------------------------------------------------------~
#### HELPDOC
# Observations without ID?
# pew_data %>%
#   filter(is.na(IndvID)) %>%
#   pull(NestboxID)
# There are 4 observations without ID, we remove those observations,
# as any additional information is provided (body measurements, etc.)
# Observations are from 2017, nestboxes 97, 98, 102, kk12.
#
#
#### NOTES:
#### Experiment "BSM - Griffioen et al. 2019 PeerJ" was done with hatched nestlings, not with eggs.
#### ---------------------------------------------------------------------------------------~

# pew <- format_PEW()

format_PEW <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # Force user to select directory
  force(db)

  # Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  start_time <- Sys.time()

  #### Primary data

  message("Importing primary data...")

  pew_data <- readxl::read_excel(path =  paste0(db, "/PEW_PrimaryData.xlsx"),
                                 col_types = c("text", "text", "text",
                                               "text", "text", "numeric", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "text", "text", "text",
                                               "text", "text", "date", "text",
                                               "text", "text", "numeric", "numeric",
                                               "text", "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric"),
                                 na = "NA") %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    #### Convert to corresponding format and rename
    dplyr::mutate(BreedingSeason = as.integer(Year),
                  #### Handle specifically the date column to preserve the information
                  #### regarding the D14 values for chicks
                  Date_temp = ifelse(Date == "D14", NA_character_, Date),
                  D14Chicks = ifelse(Date == "D14", "yes", "no"),
                  CaptureDate = janitor::excel_numeric_to_date(as.numeric(Date_temp),
                                                      date_system = "modern"),
                  Tarsus = as.numeric(Tarsus),
                  Mass = as.numeric(Mass),
                  ClutchSize = as.integer(stringr::str_replace(string = ClutchSize,
                                                               pattern = "\\?",
                                                               replace = "")),
                  DateEgg1 = as.Date(DateEgg1),
                  NumberOfRingedChicks = case_when(DateRingingChicks %in% c("verlaten incubatie", "all dead d1") ~ 0L,
                                                   HatchDateD0 %in% c("abandoned", "bumblebee nest", "dead embryos") ~ 0L,
                                                   TRUE ~ as.integer(NumberOfRingedChicks)),
                  HatchDateD0 = janitor::excel_numeric_to_date(as.numeric(HatchDateD0),
                                                                      date_system = "modern"),
                  DateRingingChicks = janitor::excel_numeric_to_date(as.numeric(DateRingingChicks),
                                                                     date_system = "modern"),
                  NoOfChicksD3 = as.integer(NoOfChicksD3),
                  BroodMassD3 = as.numeric(BroodMassD3),
                  NewRing = toupper(NewRing),
                  Species = "CYACAE",
                  PopID = "PEW",
                  NestboxID = tolower(Nest),
                  BroodID = ifelse(Method %in% c("Catch adults nestbox", "ChickRinging",
                                                 "Catch incubation"),
                                   paste(Year, NestboxID, "PEW", sep = "_"), NA)) %>%
    #### Rename variables to standardized format
    dplyr::rename(IndvID = Id,
                  Sex =  Seks,
                  ObserverID = Measurer) %>%
    #### Remove columns which we do not store in the standardized format
    dplyr::select(-ObservationTimeH,
                  -FeatherCollection ,
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
                  -Nest,
                  -MassPartner,
                  -AgePartner,
                  -TarsusPartner,
                  -Date,
                  -Date_temp) %>%
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
                                     "MetalRing_F2019_K89", PartnerId)) %>%
    #### Corrected information from data owner
    dplyr::mutate(Sex = ifelse(IndvID == "11714676", "Male", Sex),
                  PartnerId = ifelse(PartnerId == "12706296" & BroodID == "2016_60_PEW",
                                     NA_character_, PartnerId),
                  # The only couple in 49 in 2017 was 13619466 (female) and  13617031 (male).
                  # partner 12706106 should be removed (or replaced by the true female 13619466).
                  PartnerId = ifelse(PartnerId == "12706106" & BroodID == "2017_49_PEW",
                                     "13619466", PartnerId),
                  # 2016_60_PEW:  FEMALE: 12706296 in box 60 in 2016 is erroneous and can be removed from the data.
                  # Female 13617052 was the only female in box 60 in 2016.
                  # She layed a complete clutch, which never hatched.
                  # The male was never caught and remains unknown.
                  rem = ifelse((IndvID == "12706296" & BroodID == "2016_60_PEW"),
                               "yes", "no")) %>%
    dplyr::filter(rem == "no" | is.na(rem)) %>%
    dplyr::select(-rem) %>%
    dplyr::distinct()





  #### BROOD DATA
  #### done, need check

  message("Compiling brood information...")

  Brood_data <- create_brood_PEW(data = pew_data)


  #### CAPTURUE DATA
  #### done

  # AGE_CALCULATED SEEMS NOT CORRECT ?
  # (maybe also because the Age_observed was not correctly assigned?)
  #
  # Liam:
  #   Seems to me that there are some records that are not captures (i.e. they don't have a date)
  #   but just include brood information. So, I would explicitly remove the records with no date
  #   from Capture_data but still use them in Brood_data.
  #   In other cases where there are multiple captures in a year (with dates)
  #   these should be treated as legitimate multiple captures.

  message("Compiling capture information...")

  Capture_data <- create_capture_PEW(pew_data, Brood_data)


  #### INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data <- create_individual_PEW(Capture_data)


  #### LOCATION DATA
  #### done, check with data owner

  message("Compiling location information...")

  Location_data <- create_location_PEW(pew_data)

  #### FINAL ARRANGEMENT
  Capture_data <-
    Capture_data %>%
    dplyr::filter(!is.na(CaptureDate))

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  #### EXPORT DATA

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PEW.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PEW.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }


}





#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~

create_brood_PEW <- function(data) {

  parents_brood_data <-
    data %>%
    #### Exclude non-breeding data, exclude chicks
    #### Trying to use both
    filter(Method %in% c("Catch adults nestbox", "Catch incubation")) %>%
    #### Rename variables
    dplyr::rename(LocationID = NestboxID) %>%
    #### Get IDs of females and males
    tidyr::pivot_wider(names_from = Sex,
                       values_from = IndvID) %>%
    dplyr::rename(FemaleID = Female,
                  MaleID = Male) %>%
    dplyr::mutate(FemaleID = ifelse(is.na(FemaleID) & !is.na(MaleID), PartnerId, FemaleID),
                  MaleID = ifelse(is.na(MaleID) & !is.na(FemaleID), PartnerId, MaleID)) %>%
    dplyr::mutate(ExperimentID = dplyr::case_when(Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                  "COHORT; PARENTAGE",
                                                Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                  "SURVIVAL; PARENTAGE")) %>%
    #### Remove unnecessary variables which may cause duplicated rows
    #### Exclude also Date column, as for few broods, there may be
    #### several catches of parents, but the brood parameters are the same
    dplyr::select(-c(Age, PartnerId, NumberTransponder,
                     NewRing, CaptureDate, NeophobiaTransponder,
                     Tarsus, Mass, ObserverID, Experiment, D14Chicks)) %>%
    #### Remove duplicated rows (as we get one row for males and females for the same brood)
    dplyr::distinct() %>%
    #### Remove rows with no information about the brood
    dplyr::filter(!(is.na(ClutchSize) & is.na(DateEgg1) & is.na(HatchDateD0) &
                    is.na(NumberOfRingedChicks) & is.na(DateRingingChicks) &
                    is.na(NoOfChicksD3) & is.na(BroodMassD3))) %>%
    #### Remove one specific case causing double register for the same BroodID
    dplyr::filter(!(BroodID == "2017_109_PEW" & Method == "Catch incubation")) %>%
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
                  #### For few broods, there is number of chicks on day 3
                  BroodSize_observed = NoOfChicksD3,
                  BroodSize_min = NA_integer_,
                  BroodSize_max = NA_integer_,
                  FledgeDate_observed = NA_character_,
                  FledgeDate_min = NA_character_,
                  FledgeDate_max = NA_character_,
                  # NumberFledged_observed = NumberOfRingedChicks, ## for new version of calc_clutchtype
                  NumberFledged = NA_integer_,
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
    dplyr::summarise(AvgChickMass = mean(as.numeric(Mass), na.rm = TRUE),
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
    parents_brood_data %>%
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

create_capture_PEW <- function(pew_data, Brood_data) {

  Brood_data_sel <-
    Brood_data %>%
    # dplyr::select(BreedingSeason, Species, PopID, BroodID, LocationID,
    dplyr::select(BroodID, LocationID, HatchDate_observed)

  # >> mirar github comment de Liam
  # parece que habra que poner fecha inventada, dado que no existe hatchdate, ni capture date

  Capture_data_temp <-
    pew_data %>%
    #### Rename variables
    dplyr::rename(LocationID = NestboxID) %>%
    dplyr::select(-DateRingingChicks) %>%
    #### Create new variables
    dplyr::group_by(IndvID) %>%
    dplyr::arrange(Year, CaptureDate) %>%
    dplyr::mutate(CaptureID = paste(IndvID, row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(CaptureTime  = NA_character_,
                  Sex_observed = ifelse(Sex == "Chick", NA_character_, substr(Sex, 1, 1)),
                  CaptureAlive = ifelse(Method != "Found dead", TRUE, FALSE),
                  ReleaseAlive = CaptureAlive,
                  CapturePopID = PopID,
                  CapturePlot  = NA_character_,
                  ReleasePopID = ifelse(ReleaseAlive == TRUE, CapturePopID, NA_character_),
                  ReleasePlot  = ifelse(ReleaseAlive == TRUE, CapturePlot, NA_character_),
                  #### ASSUMPTION: use NA, as there is no information
                  OriginalTarsusMethod = NA_character_,
                  WingLength = NA_real_,
                  ExperimentID = dplyr::case_when(Experiment == "BSM - Griffioen et al. 2019 PeerJ" ~
                                                    "COHORT; PARENTAGE",
                                                  Experiment == "2h Temp D4" ~ "SURVIVAL",
                                                  Experiment == "2h Temp D4 + Handicaping Male: Griffioen et al. 2019 Front Ecol&Evol" ~
                                                    "SURVIVAL; PARENTAGE"),
                  Age_observed = case_when(Age == "0" & Method == "ChickRinging" ~ 1L,
                                           Age == "1" ~ 5L,
                                           Age == ">=2" ~ 6L,
                                           #### Account for controls in January & February
                                           Age == "1" & Method == "Night Control Winter" &
                                             lubridate::month(CaptureDate) %in% c(1, 2) ~ 6L,
                                           #### Account for controls in January & February
                                           Age == ">=2" & Method == "Night Control Winter" &
                                             lubridate::month(CaptureDate) %in% c(1, 2) ~ 8L,
                                           is.na(Age) ~ 4L))


  Capture_data <-
    Capture_data_temp %>%
    left_join(Brood_data_sel,
              by = c("LocationID", "BroodID")) %>%
    #### Calculate Capture date for chicks from Hatch date
    mutate(CaptureDate = if_else(is.na(CaptureDate) & D14Chicks == "yes",
                                 HatchDate_observed + 14,
                                 CaptureDate)) %>%

  #### There are still 340 without capture date
  # nocd  <- filter(Capture_data, is.na(CaptureDate))


    #### The age of captured chicks in days since hatching
    dplyr::mutate(ChickAge = if_else(Age_observed == 1,
                                     as.integer(difftime(CaptureDate, HatchDate_observed)),
                                     NA_integer_),
                  BroodIDLaid = BroodID) %>%

    #### NEW VERSION OF THE FUNCTION (TO FINISH!!)
    # dplyr::mutate(Age_calculated = calc_age())

    #### OLD VERSION OF THE FUNCTION
    calc_age(ID = IndvID,
             Age = Age_observed,
             Date = CaptureDate,
             Year = BreedingSeason,
             showpb = TRUE) %>%
    #### Final arrangement
    dplyr::select(CaptureID, IndvID, Species, Sex_observed, BreedingSeason,
                  CaptureDate, CaptureTime, ObserverID, LocationID,
                  CaptureAlive, ReleaseAlive, CapturePopID, CapturePlot,
                  ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge, ExperimentID)

    # #### Remove records with no date
    # #### CHECK: doing this, almost 340 records of chicks will be removed,
    # #### as there is no CaptureDate !!!
    # filter(!is.na(CaptureDate))
    # #### NOTE: KEEP IN CAPTURE DATA TO BE INCLUDED IN INDIVIDUAL INFORMATION,
    # #### BUT REMOVE IT BEFORE FINAL OUPUT!!

  return(Capture_data)

}

create_individual_PEW <- function(data){

  Individual_data <-
    data %>%
    #### NOTE: Keep rows without Capture date
    #### Format and create new data columns
    group_by(IndvID) %>%
    dplyr::summarise(Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex_observed))), .f = ~{
                                                          if(length(..1) == 0){
                                                            return(NA_character_)
                                                          } else if(length(..1) == 1){
                                                            return(..1)
                                                          } else {
                                                            return("C")
                                                          }
                                                        }),
                      Sex_genetic = NA_character_,
                      Species = first(Species),
                      PopID = "PEW",
                      RingSeason = min(BreedingSeason),
                      RingAge = ifelse(min(Age_observed) == 1, "chick", "adult"),
                      BroodIDLaid = ifelse(RingAge == "chick",
                                           paste(BreedingSeason, LocationID, sep = "_"),
                                           NA_character_),
                      BroodIDFledged = BroodIDLaid) %>%
    dplyr::ungroup() %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged,
                  RingSeason, RingAge, Sex_calculated, Sex_genetic)

  return(Individual_data)

  }

create_location_PEW <- function(data) {

  Location_data <-
    data %>%
    dplyr::select(BreedingSeason, CaptureDate, NestboxID, PopID) %>%
    group_by(NestboxID) %>%
    arrange(BreedingSeason, CaptureDate) %>%
    dplyr::summarise(StartSeason = min(BreedingSeason, na.rm = TRUE),
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



