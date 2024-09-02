#'Construct standard format for data from Can Cata, Spain
#'
#'A pipeline to produce the standard format for the nest box population in Can Cata, Spain, administered by Juan Carlos Senar
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard format please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#'\strong{BroodID}: Unique combination of nest box (LocationID), breeding season and species.
#'\strong{Capture Time}:Not available for now.Time data exists but could not be read properly (even as text) as it contains either "." or ":" as separator in excel file. This needs to be fixed in the original file.
#'\strong{ChickAge}: This is calculated as date of measurement- date of hatching. Due to errors in either of these dates it can sometimes differ greatly from 14-15 days old
#'\strong{Sex}: Errors in sexing were ignored so that "M?" was read as "M.
#'\strong{ClutchType_observed}: Brood types 1a, 2a and REP were coded as "First", "Second" and "Replacement", respectively
#'\strong{ExperimentID}: "PARENTAGE" includes cross-fostering
#'\strong{LocationID}: NA for captures not done in nest boxes
#'\string{Age_observed}: In the field, birds were described as "Adult", "Yearling" or "Juvenile." Following EURING conventions, adults were hence coded as 6, Yearlings as 5 and Juveniles as 3 or 4 depending on the month observed (3 if captured in the same year it hatched, 4 otherwise)
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export
#'

format_CAC <- function(db = choose.dir(),#choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = 'R'){

  #Force choose_directory() if used
  #force(db)

  #Assign to database location
  db <- paste0(gsub("\\\\", "/", db), "\\CAC_PrimaryData.xlsx")

  #### Determine species and population codes for filtering
  if(is.null(species)){

    species_filter <- species_codes$Species

  } else {

    species_filter <- species

  }

  if(is.null(pop)){

    pop_filter <- pop_codes$PopID

  } else {

    pop_filter <- pop

  }

  ## Start time
  start_time <- Sys.time()

  message("Importing primary data...")

  ## Set options
  options(dplyr.summarise.inform = FALSE)


  #########################################################
  ###Get primary data in a form that can be used, fix bugs
  #########################################################

  ## 1- Get brood information from primary data
  ##Notes: duplicate box-year but each time it is for a different species. Incorporate species in BroodID.
  ##Some boxes have an a or b but it is unclear why
  ##parent ring number column sometimes contains other things: fixed
  ## 1-A) Read in brood data blue tits
  brood_data_CC <- readxl::read_xlsx(path = db, guess = 5000,sheet= "Data base breeding Cc") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")  %>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14620,]$Species,
                     BreedingSeason = .data$Year,
                     LocationID = .data$NestBox,
                     ClutchType_observed = dplyr::case_when(.data$Brood == "1a" ~ "first",
                                                            .data$Brood == "2a" ~ "second",
                                                            .data$Brood == "REP" ~ "replacement",
                                                            .data$Brood == "2a?" ~ NA_character_,
                                                            TRUE ~ NA_character_),
                     BroodID = paste(.data$NestBox,.data$Year,.data$Species,sep="_"),
                     FemaleID= dplyr::case_when(grepl("no",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("anella",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("anilla",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("si",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("Sn A",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("!",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("pvc",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                grepl("/",.data$Female) ~ NA_character_,
                                                TRUE ~ .data$Female),
                     MaleID=dplyr::case_when(grepl("no",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("anella",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("pvc",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("/",.data$Male) ~ NA_character_,
                                             TRUE ~ .data$Male),
                     AvgEggMass = NA_real_,
                     NumberEggs = NA_real_,
                     LayDate_observed =as.Date(as.numeric(.data$LayingDate), origin = "1899-12-30"),
                     ClutchSize_observed =as.integer(.data$ClutchSize),
                     HatchDate_observed = as.Date(as.numeric(.data$HatchingDate), origin = "1899-12-30"),
                     FledgeDate_observed = as.Date(as.numeric(.data$FledglingDate), origin = "1899-12-30"),
                     BroodSize_observed = NA_integer_,
                     NumberFledged_observed = as.integer(.data$NumberFledglings),
                     ExperimentID = dplyr::case_when(!is.na(.data$Crossfostering) ~"PARENTAGE",
                                                     TRUE ~ NA_character_))%>%
    dplyr::filter(is.na(ClutchSize_observed) |ClutchSize_observed>0)%>%
    dplyr::filter(!(is.na(.data$ClutchSize_observed)&#remove nests in which nothing happened
                      is.na(.data$LayDate_observed)&
                      is.na(.data$HatchDate_observed)&
                      is.na(NumberFledged_observed)))%>%
    dplyr::mutate(FemaleID=gsub("+Bl","",FemaleID,fixed=TRUE))%>%
    dplyr::arrange(.data$BreedingSeason, .data$LocationID)



  ## 1-B) Read in brood data great tits
  brood_data_PM <- readxl::read_xlsx(path = db, guess = 5000,sheet= "Data base breeding PM") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")  %>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     BreedingSeason = .data$Year,
                     LocationID = .data$NestBox,
                     ClutchType_observed = dplyr::case_when(.data$Brood == "1a" ~ "first",
                                                            .data$Brood == "2a" ~ "second",
                                                            .data$Brood == "REP" ~ "replacement",
                                                            .data$Brood == "2a?" ~ NA_character_,
                                                            TRUE ~ NA_character_),
                     BroodID = paste(.data$NestBox,.data$Year,.data$Species,sep="_"),
                     FemaleID=dplyr::case_when(grepl("no",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("anella",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("anilla",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("si",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("?",.data$Female, fixed = TRUE) ~ NA_character_,
                                                          grepl("!",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("pvc",.data$Female, ignore.case = TRUE) ~ NA_character_,
                                                          grepl("/",.data$Female) ~ NA_character_,
                                                          TRUE ~ .data$Female),
                     MaleID=dplyr::case_when(grepl("no",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("anella",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("pvc",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("ó",.data$Male, ignore.case = TRUE) ~ NA_character_,
                                             grepl("/",.data$Male) ~ NA_character_,
                                             TRUE ~ .data$Male),
                     AvgEggMass = NA_real_,
                     NumberEggs = NA_real_,
                     LayDate_observed =as.Date(as.numeric(.data$LayingDate), origin = "1899-12-30"),
                     ClutchSize_observed =as.integer(.data$ClutchSize),
                     HatchDate_observed = as.Date(as.numeric(.data$HatchingDate), origin = "1899-12-30"),
                     FledgeDate_observed = as.Date(as.numeric(.data$FledglingDate), origin = "1899-12-30"),
                     BroodSize_observed = NA_integer_,
                     NumberFledged_observed = as.integer(.data$NumberFledglings),
                     ExperimentID = dplyr::case_when(!is.na(.data$Crossfostering) ~"PARENTAGE",
                                                     TRUE ~ NA_character_))%>%
    dplyr::filter(is.na(ClutchSize_observed) |ClutchSize_observed>0)%>%
    dplyr::filter(!(is.na(.data$ClutchSize_observed)&#remove nests in which nothing happened
                      is.na(.data$LayDate_observed)&
                      is.na(.data$HatchDate_observed)&
                      is.na(NumberFledged_observed)))%>%

    dplyr::arrange(.data$BreedingSeason, .data$LocationID)

  ## 1-C) Combine brood data from both species

  brood_data <- dplyr::bind_rows(brood_data_CC,
                                 brood_data_PM) %>%
    dplyr::mutate(dplyr::across(c(ClutchSize_observed,
                                  BroodSize_observed,
                                  NumberFledged_observed,
                                  NumberEggs), as.integer)) %>%
    dplyr::arrange(.data$BreedingSeason, .data$LocationID,.data$LayDate_observed)


  ## 2- Get capture information from primary data
  ##Notes: Time data exists but could not be read properly (even as text) as it contains either "." or ":" as separator in excel file
  ## 2-A) Read in capture data blue tits
  cap_data_CC <- readxl::read_xlsx(path = db, guess = 2000,sheet= "Ring # Adults Cc") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")%>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14620,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$Ring,  "^[:alnum:]{6,8}$") ~ .data$Ring,
                                                       TRUE ~ NA_character_)),
                     Sex_observed = dplyr::case_when(.data$Sx == "M" ~ "M",
                                                     .data$Sx == "M?" ~ "M",
                                                     .data$Sx == "F" ~ "F",
                                                     .data$Sx == "F?" ~ "F",
                                                     TRUE ~ NA_character_),
                     CaptureDate = format(as.Date(.data$Date), "%Y-%m-%d"),
                     BreedingSeason = as.integer(format(.data$Date, "%Y")),
                     CaptureMonth = as.integer(format(.data$Date, "%m")),
                     Age_observed = dplyr::case_when(.data$Age == "Y" ~ 5L,#Need to check with the data custodian
                                                     .data$Age == "A" ~ 6L,
                                                     .data$Age == "J"~ dplyr::case_when(.data$CaptureMonth %in% c(1,2,3,4) ~ 4L,
                                                                                 .data$CaptureMonth %in% c(5:12 )~ 3L),
                                                     TRUE ~ NA_real_),
                     LocationID = dplyr::case_when(grepl("CN",.data$Loc)~as.numeric(gsub("[A-Z_ ]*", "",.data$Loc)),
                                                   TRUE~ NA_integer_),#captures that are not done in nest boxes (CN)= location is NA
                     Mass = round(as.numeric(.data$Mass),1),
                     WingLength = round(as.numeric(.data$WingLength),1),
                     Tarsus = round(as.numeric(.data$Tarsus),1),
                     CaptureAlive=NA_character_,
                     ReleaseAlive=NA_character_,
                     BroodID=NA_character_,
                     ExperimentID = NA_character_)%>%#Need to check with the data custodian
    dplyr::mutate(LocationID= gsub("CN","",LocationID))

  ## 2-B) Read in capture data great tits

  cap_data_PM <- readxl::read_xlsx(path = db, guess = 6000,sheet= "Ring # Adults Pm") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")%>%
    dplyr::mutate(Pes=dplyr::case_when ( as.character(.data$Pes) %in% .data$Pvc ~ NA_character_,#fix errors in the Pes column
                                  TRUE~ as.character(.data$Pes))) %>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$RingNumber,  "^[:alnum:]{6,8}$") ~ .data$RingNumber,
                                                       TRUE ~ NA_character_)),
                     Sex_observed = dplyr::case_when(.data$Sx == "M" ~ "M",
                                                     .data$Sx == "M?" ~ "M",
                                                     .data$Sx == "F" ~ "F",
                                                     .data$Sx == "F?" ~ "F",
                                                     TRUE ~ NA_character_),
                     CaptureDate = format(as.Date(.data$Date), "%Y-%m-%d"),
                     BreedingSeason = as.integer(format(.data$Date, "%Y")),
                     CaptureMonth = as.integer(format(.data$Date, "%m")),
                     Age_observed = dplyr::case_when(.data$Age == "Y" ~ 5L,
                                                     .data$Age == "A" ~ 6L,
                                                     .data$Age == "J"~ dplyr::case_when(.data$CaptureMonth %in% c(1,2,3,4) ~ 4L,
                                                                                 .data$CaptureMonth %in% c(5:12 )~ 3L),
                                                     TRUE ~ NA_real_),
                     LocationID = dplyr::case_when(grepl("CN",.data$LocCnTp)~as.numeric(gsub("[A-Z_ ]*", "",.data$LocCnTp)),
                                                   TRUE~ NA_integer_),#captures that are not done in nest boxes (CN)= location is NA
                     Mass = round(as.numeric(.data$Pes),1),
                     WingLength = round(as.numeric(.data$Ala),1),
                     Tarsus = round(as.numeric(.data$Tars),1),
                     CaptureAlive=NA_character_,
                     ReleaseAlive=NA_character_,
                     BroodID=NA_character_,
                     ExperimentID = NA_character_) %>%
    dplyr::mutate(LocationID= gsub("CN","",LocationID))


  ## 2-C) Combine capture data from both species

  cap_data <- dplyr::bind_rows(cap_data_CC,
                               cap_data_PM) %>%
    dplyr::arrange(.data$BreedingSeason, .data$LocationID)

  ## 3- Get chick information from primary data
  ##Notes: There is no data on brood of origin for individual chicks and it is unclear whether the box nr. is the nest of rearing or origin (one has 14 chicks)

  ## 3-A) Read in chick data blue tits

  chick_data_CC <- readxl::read_xlsx(path = db, guess = 2000,sheet= "Ring # Fledged chicks Cc") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")%>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14620,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$Ring,  "^[:alnum:]{6,8}$") ~ .data$Ring,
                                                       TRUE ~ NA_character_)),
                     Sex_observed = NA_character_,
                     CaptureDate = format(as.Date(.data$Data), "%Y-%m-%d"),
                     BreedingSeason = as.integer(format(.data$Data, "%Y")),
                     CaptureMonth = as.integer(format(.data$Data, "%m")),
                     Age_observed = 1L,
                     LocationID = as.numeric(gsub("[A-Z_ ]*", "",.data$Loc)),
                     Mass = round(as.numeric(.data$Mass),1),
                     WingLength = NA_real_,
                     Tarsus = round(as.numeric(.data$Tarsus),1),
                     CaptureAlive=NA_character_,
                     ReleaseAlive=NA_character_,
                     BroodID=paste(.data$LocationID,.data$BreedingSeason,.data$Species,sep="_")) %>%
    dplyr::left_join(brood_data  %>%
                       dplyr::select(BroodID, ExperimentID),
                     by = "BroodID") %>%
    dplyr::mutate(LocationID= gsub("CN","",LocationID))

  ## 3-B) Read in chick data great tits

  chick_data_PM <- readxl::read_xlsx(path = db, guess = 2000,sheet= "Ring # Fledged chicks Pm") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\?",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")%>%
    dplyr::transmute(PopID = "CAC",
                     Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                     IndvID = toupper(dplyr::case_when(stringr::str_detect(.data$RingNumber,  "^[:alnum:]{6,8}$") ~ .data$RingNumber,
                                                       TRUE ~ NA_character_)),
                     Sex_observed = NA_character_,
                     CaptureDate = format(as.Date(.data$Date), "%Y-%m-%d"),
                     BreedingSeason = as.integer(format(.data$Date, "%Y")),
                     CaptureMonth = as.integer(format(.data$Date, "%m")),
                     Age_observed = 1L,
                     LocationID = as.numeric(gsub("[A-Z_ ]*", "",.data$LocCnTp)),
                     Mass = round(as.numeric(.data$Mass),1),
                     WingLength = NA_real_,
                     Tarsus = round(as.numeric(.data$Tarsus),1),
                     CaptureAlive=NA_character_,
                     ReleaseAlive=NA_character_,
                     BroodID=paste(.data$LocationID,.data$BreedingSeason,.data$Species,sep="_"))%>%
    dplyr::left_join(brood_data  %>%
                       dplyr::select(BroodID, ExperimentID),
                     by = "BroodID") %>%
    dplyr::mutate(LocationID= gsub("CN","",LocationID))

  ## 3-C) Combine chick data from both species

  chick_data <- dplyr::bind_rows(chick_data_CC,
                                 chick_data_PM) %>%
    dplyr::arrange(.data$BreedingSeason, .data$LocationID)

  ## 4- Combine all capture data

  all_cap_data <- dplyr::bind_rows(chick_data,
                                   cap_data) %>%
    dplyr::arrange(.data$BreedingSeason, .data$LocationID)#%>%
    #dplyr::filter(!is.na(.data$LocationID))

  ### 5- Get location information from primary data
  location_data <- readxl::read_xlsx(path = db, guess = 2000,sheet= "GPS nest boxes") %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~gsub(pattern = "~|'|\\? ",
                             replacement = "",
                             iconv(.,
                                   from = "UTF-8",
                                   to = 'ASCII//TRANSLIT')),
                       .cols = tidyselect::everything()) %>%
    janitor::clean_names(case = "upper_camel")%>%
    dplyr::rename(LocationID=IdNestBox,
                  Plot=CodeArea,
                  Latitude=Lat,
                  Longitude=Long) %>%
    dplyr::mutate(StartSeason = dplyr::case_when(
      LocationID < 172 ~ 1998,
      LocationID >= 172 & LocationID <= 182 ~ 2008,
      LocationID > 182 ~ 2013))%>%
    dplyr::select(LocationID,Plot,Latitude,Longitude,StartSeason)

  ###########################################
  ###Define all the functions
  ##########################################

  #' Create brood data table for Can Cata, Spain.
  #'
  #' @param brood_data Brood data compiled from primary data from Can Cata, Spain.
  #'
  #' @param rr_data Ringing data compiled from primary data from Can Cata, Spain.
  #'
  #' @return A data frame.

  create_brood_CAC   <- function(brood_data,chick_data) {
    ## Create brood data
    Brood_data_temp <- brood_data %>%
      dplyr::mutate(OriginalTarsusMethod = "Alternative",
                    LocationID=as.numeric(gsub("[A-Z_ ]*", "",.data$LocationID)))%>%#remove the a and b =same coordinate

      ##Add plot information
      dplyr::left_join(location_data %>%
                         dplyr::select(LocationID,
                                       Plot),by="LocationID")%>%

      ## Get chick summary stats
      dplyr::left_join(chick_data %>%
                         dplyr::select(BroodID,
                                       IndvID,
                                       Mass,
                                       Tarsus) %>%

                         ## For each brood, get summary stats
                         dplyr::group_by(.data$BroodID) %>%
                         dplyr::summarise(AvgChickMass = round(mean(Mass, na.rm = T), 1),
                                          NumberChicksMass = sum(!is.na(.data$Mass)),
                                          AvgTarsus = round(mean(Tarsus, na.rm = T), 2),
                                          NumberChicksTarsus = sum(!is.na(.data$Tarsus))) %>%

                         ## Replace NaNs and 0 with NA
                         dplyr::mutate(dplyr::across(c(AvgChickMass,AvgTarsus), ~dplyr::na_if(., NaN)))) %>%

      dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "1.1"))%>%

      tibble::as_tibble()

    return(Brood_data_temp)

  }

  #' Create capture data table in standard format for data from Can Cata, Spain.
  #'
  #' @param rr_data Data frame. Primary data from ringing records.
  #'
  #' @return A data frame.

  create_capture_CAC <- function(all_cap_data,Brood_data_temp) {

    ## Captures from ringing data
    Capture_data_temp <- all_cap_data %>%

      dplyr::mutate(LocationID=as.numeric(.data$LocationID))%>%

      ##Add plot information
      dplyr::left_join(location_data %>%
                         dplyr::select(LocationID,
                                       Plot),by="LocationID")%>%

      ## Rename variables
      dplyr::rename(CapturePopID = .data$PopID,
                    CapturePlot=.data$Plot) %>%

      ## Remove NAs from key columns
      dplyr::filter_at(dplyr::vars( CapturePopID,
                                    IndvID,
                                    BreedingSeason,
                                    Species,
                                    CaptureDate),
                       dplyr::all_vars(!is.na(.))) %>%

      ## Create capture  ID
      dplyr::arrange(.data$BreedingSeason,
                     .data$IndvID,
                     .data$CaptureDate) %>%
      dplyr::group_by(.data$IndvID) %>%
      dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_"),
                    OriginalTarsusMethod = "Alternative",
                    ReleasePopID = .data$CapturePopID,
                    ReleasePlot=.data$CapturePlot)%>%

      #Add Chick age
      dplyr::left_join(brood_data %>%
                         dplyr::mutate(HatchDate_observed=as.Date(.data$HatchDate_observed))%>%
                         dplyr::select(HatchDate_observed,BroodID))%>%
      dplyr::mutate(ChickAge=dplyr::case_when(Age_observed ==1 ~ as.integer(difftime(.data$CaptureDate,.data$HatchDate_observed,units="days")),
                                       TRUE~NA_integer_))%>%#some values are negative or >21 days
      #Add age_calculated
      calc_age(ID = .data$IndvID, Age = .data$Age_observed,
               Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%


      return(Capture_data_temp)

  }

  #' Create individual data table in standard format for data from Can Cata, Spain.
  #'
  #' @param Capture_data_temp Data frame. Output from \code{\link{create_capture_CAC}}.
  #'
  #' @return A data frame.


  create_individual_CAC <- function(Capture_data_temp) {

    ## Create individual data from capture data
    Individual_data_temp <- Capture_data_temp %>%

      ## Arrange
      dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%

      #### Format and create new data columns
      dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
      dplyr::mutate(PopID = .data$CapturePopID) %>%
      dplyr::group_by(.data$IndvID) %>%
      dplyr::mutate(Species = species_codes[species_codes$SpeciesID == 14640,]$Species,
                    Sex_calculated = purrr::map_chr(.x = list(unique(stats::na.omit(.data$Sex_observed))),
                                                    .f = ~{
                                                      if(length(..1) == 0){
                                                        return(NA_character_)
                                                      } else if(length(..1) == 1){
                                                        return(..1)
                                                      } else {
                                                        return("C")
                                                      }
                                                    }),
                    Sex_genetic = NA_character_,
                    RingSeason = min(.data$BreedingSeason, na.rm = T),
                    RingAge = purrr::pmap_chr(.l = list(dplyr::first(.data$Age_observed)),
                                              .f = ~{
                                                if(is.na(..1)){
                                                  return("adult")
                                                } else if(..1 <= 3L){
                                                  return("chick")
                                                } else if(..1 > 3L){
                                                  return("adult")
                                                }
                                              }),
                    BroodIDLaid = dplyr::case_when(.data$ExperimentID=="PARENTAGE"~ NA_character_,
                                            TRUE ~ .data$BroodID),
                    BroodIDFledged = dplyr::case_when(.data$ExperimentID=="PARENTAGE"~ NA_character_,
                                               TRUE ~ .data$BroodID)) %>%

      ## Keep distinct records by PopID and InvdID
      dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%

      ## Reorder columns
      dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())

    return(Individual_data_temp)

  }
  #' Create location data table in standard format for data from Can Cata, Spain.
  #'
  #' @param Brood_data_temp Data frame. Output from \code{\link{create_brood_CAC}}.
  #'
  #' @param nest_coord_data Data frame. Primary data on nest coordinates.
  #'
  #' @return A data frame.

  create_location_CAC <- function(location_data) {

    Location_data_temp <- location_data%>%
      dplyr::select(LocationID,Latitude,Longitude,StartSeason)%>%
      dplyr::mutate(PopID = "CAC",
                    NestBoxID=.data$LocationID,
                    LocationType = "NB",
                    EndSeason = NA_integer_,
                    HabitatType = "mixed")



    return(Location_data_temp)

  }


  ###########################################
  ###Run all the functions and save data
  ##########################################

  # BROOD DATA

  message("Compiling brood information...")

  Brood_data_temp <- create_brood_CAC(brood_data, chick_data)


  # CAPTURE DATA

  message("Compiling capture information...")

  Capture_data_temp <- create_capture_CAC(all_cap_data,brood_data)


  # INDIVIDUAL DATA

  message("Compiling individual information...")

  Individual_data_temp <- create_individual_CAC(Capture_data_temp)#this is taking a long time


  # LOCATION DATA

  message("Compiling location information...")

  Location_data_temp <- create_location_CAC(location_data)


  #### PROCESSING FINAL DATA TO EXPORT

  ## Brood data
  Brood_data <- Brood_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(brood_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(brood_data_template[0, !(names(brood_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(brood_data_template)) %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)


  ## Capture data
  Capture_data <- Capture_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(capture_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(capture_data_template[0, !(names(capture_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(capture_data_template)) %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)


  ## Individual data
  Individual_data <- Individual_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(individual_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(individual_data_template[0, !(names(individual_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(individual_data_template))  %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)


  ## Location data
  Location_data <- Location_data_temp %>%

    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(location_data_template))) %>%

    ## Add missing columns
    dplyr::bind_cols(location_data_template[0, !(names(location_data_template) %in% names(.))] %>%
                       tibble::add_row()) %>%

    ## Reorder columns
    dplyr::select(names(location_data_template))  %>%
    dplyr::ungroup()

  # ## Check column classes
  # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  if (output_type == 'csv') {

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_CAC.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_CAC.csv"), row.names = F)

    invisible(NULL)

  }

  if (output_type == "R") {

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = Location_data))

  }

}




