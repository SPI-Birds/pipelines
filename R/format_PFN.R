#' Construct standard format for data from East Dartmoor, UK
#'
#'A pipeline to produce the standard format for the hole-nesting bird population
#'in East Dartmoor, UK, administered by PiedFlyNet.
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{Species}: By default, pipeline outputs will include great tit \emph{Parus
#'major}; blue tit \emph{Cyanistes caeruleus}; pied flycatcher \emph{Ficedula
#'hypoleuca}; Eurasian nuthatch \emph{Sitta europaea}; coal tit \emph{Periparus
#'ater}; marsh tit \emph{Poecile palustris}, and common redstart \emph{Phoenicurus phoenicurus}.
#'Other minority species are excluded.
#'
#'\strong{Capture data}: Capture data is extracted directly from information on broods, separately for
#'males, females, and chicks, and then merged. Ringing data (first capture) therefore usually comes
#'from chick capture data, while recaptures come from captures of parents at the nests.
#'
#'\strong{Unidentified individuals}: Some individuals have not been given unique IDs. They appear
#'in the primary data as: "RUNT", "ringed", "ringed left", "ringed right", "unringed", or "Unringed".
#'In the Brood data, we assume unknown identity and treat ID as NA. From Capture data and Individual
#'data, these individuals are omitted entirely.
#'
#'\strong{LayDate_observed and HatchDate_observed}: Information is provided as date of first egg (DFE) and
#'date of hatching (DH). These are given as integer number, and represent days after a set
#'starting date (day 0). Day 0 is assumed to be 31. March every year (to be confirmed).
#'
#'\strong{Age}: Adult individuals are only recorded as being either the breeding male or breeding females.
#'The observed age for all adults is therefore EURING code 4: adult with otherwise unknown age. When possible,
#'we calculate age more specifically using information on previous captures, and hatch date for individuals
#'ringed as chicks (see below).
#'
#'\strong{ChickAge}: For every capture, we estimate the age of a chick as the difference between the hatch date
#'taken from BroodIDFledged (in Individual_data) and the CaptureDate.
#'
#'\strong{Location Start- and EndSeason}: The first and last breeding seasons in which specific locations
#'(nestboxes) were used are defined from the perspective of the birds, not the researchers, i.e. StartSeason
#' and EndSeason of a nestbox represent the first and last breeding season in which birds bred in it.
#'
#'@inheritParams pipeline_params
#'
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export
#'


format_PFN <- function(db,
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  #Record start time to estimate processing time.
  start_time <- Sys.time()

  #Load primary brood data
  Primary_data <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_EDartmoor.csv"), na.strings = c("", "?"),colClasses = "character")

  #Load complete PiedFlyNet ringing database
  CMR_data <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_All_CMR.csv"), na.strings = c("", "?", "UNK", "-"),colClasses = "character") # TODO:Confirm with Malcolm that SITE = "UNK" refers to unknown/unassigned locations


  # PREPARATION: RE-RINGING DATA

  # Find all birds that were re-ringed at least once
  ReRing <- CMR_data[,c('RING', 'RING2')] %>%
    dplyr::filter(!is.na(.data$RING2))

  # Collate all ring numbers used for each individual (with max. number of re-ringing events = 3)
  ReRingTable <- make_ReRingTable(raw_data = ReRing, N = 3) %>%

    # Make new individual ID for re-ringed birds
    dplyr::mutate(ReRingID = paste0('MULTIRING',.data$ReRingNr)) %>%
    dplyr::select(RingNr, ReRingID)


  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_EDM(CMR_data = CMR_data, Primary_data = Primary_data, ReRingTable = ReRingTable)


  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_EDM(Primary_data = Primary_data, ReRingTable = ReRingTable)


  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_EDM(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_EDM(Brood_data = Brood_data, Capture_data = Capture_data)


  # WRANGLE DATA FOR EXPORT

  # Capture data: Merge in information on ChickAge (from Brood_data) & remove unnecessary columns
  ChickAge_data <- Brood_data[,c('BroodID', 'ChickAge')]
  Capture_data <- dplyr::left_join(Capture_data, ChickAge_data, by = 'BroodID')
  Capture_data$ChickAge[which(Capture_data$Age_observed > 1)] <- NA_integer_

  Capture_data <- Capture_data %>%
    dplyr::select(-BroodID)

  # Brood data: Remove unnecessary columns
  Brood_data <- Brood_data %>%
    dplyr::select(-ChickAge)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_EDM.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_EDM.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_EDM.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_EDM.csv"), row.names = F)

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



#' Create brood data table for EastDartmoor.
#'
#' @param Primary_data Primary data from EastDartmoor.
#' @param ReRingTable Table containing ring numbers and unique identifiers for re-ringed individuals
#'
#' @return A data frame with Brood data


create_brood_EDM <- function(Primary_data, ReRingTable){

  ## Pre) Determine a vector of "bad" (nonconclusive) IDs
  #This will be used downstream in point 7)
  badIDs <- c("RUNT", "ringed", "ringed left", "ringed right", "unringed", "Unringed")


  ## 1) Rename columns that are equivalent (content and format) to columns in the standard format
  Brood_data <- Primary_data %>%
    dplyr::rename(Plot = Popn,
                  LocationID = Box) %>%

    ## 2) Add population ID and reformat colums with equivalent content but different format
    dplyr::mutate(PopID = "EDM",
                  BreedingSeason = as.integer(Year),
                  Species = dplyr::case_when(Species == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                            Species == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                            Species == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                            Species == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                            Species == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                            Species == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                            Species == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                            Species == "WREN" ~ NA_character_, # Missing, 1 observation only
                                            Species == "TREEC" ~ NA_character_), # Missing, 1 observation only
                  ClutchType = dplyr::case_when(CltCd == "1" ~ "first",
                                                        CltCd == "2" ~ "replacement",
                                                        CltCd == "3" ~ "second"),
                  LayDate = as.Date(paste('31/03/', BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(DFE),
                  ClutchSize = dplyr::case_when(as.integer(.data$CltSize) > 0 ~ as.integer(.data$CltSize),
                                                as.integer(.data$CltSize) == 0 | is.na(.data$CltSize) ~ NA_integer_),
                  ClutchSize_min = dplyr::case_when(is.na(.data$CltSize) | as.integer(.data$CltSize) == 0 ~ suppressWarnings(pmax(as.integer(.data$MinEggs), as.integer(.data$UnHatch)+as.integer(.data$Hatch), as.integer(.data$UnHatch)+as.integer(.data$Fledged), na.rm = TRUE))),
                  HatchDate = as.Date(paste('31/03/', .data$BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(.data$DH),
                  BroodSize = as.integer(.data$Hatch),
                  NumberFledged = as.integer(.data$Fledged),
                  ChickAge = as.integer(as.Date(.data$YoungDate, format = "%d/%m/%Y") - .data$HatchDate)) %>%
                  NumberFledged = as.integer(Fledged),
                  ChickAge = as.integer(as.Date(YoungDate, format = "%d/%m/%Y") - HatchDate)) %>%

    # 4) Add columns that are based on calculations
    dplyr::arrange(BreedingSeason, FemaleID, LayDate)
    # --> This sorting is required for the function "calc_clutchtype" to work

    # 4.1) Calculate means and observation numbers for fledgling weight/tarsus length
    Brood_averages <- Brood_data %>%
    dplyr::filter(between(ChickAge, 14, 16)) %>%  #Remove chicks outside the age range
    dplyr::select(BroodID, contains("Weight."), contains("Tarsus.")) %>% #Select only weight and tarsus cols
    tidyr::pivot_longer(cols = -BroodID) %>%  #Rearrange so they're individual rows rather than individual columns
    dplyr::filter(!is.na(value)) %>% #Remove Nas
    dplyr::mutate(name = gsub(pattern = "Weight", replacement = "ChickMass", gsub(pattern = ".Y\\d+", replacement = "", x = name))) %>% #Change the names so it's not 'Weight.Y1' and 'Tarsus.Y1' but just 'ChickMass' and 'Tarsus'
    dplyr::group_by(BroodID, name) %>% #Group by brood, weight and tarsus
    dplyr::summarise(Avg = mean(as.numeric(value), na.rm = TRUE), Number = n()) %>% #Extract the mean and sample size
    tidyr::pivot_wider(names_from = name, values_from = c(Avg, Number), names_sep = "") %>% #Move this back into cols so we have AvgChickMass, NumberChickMass etc.
    dplyr::ungroup()

    # 4.2) Add calculated columns
    Brood_data <- Brood_data %>%
      dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = Brood_data, na.rm = FALSE)) %>%
      dplyr::left_join(Brood_averages, by = "BroodID")


  # 5) Rename columns and add columns without data
  Brood_data <- Brood_data %>%
      dplyr::mutate(ClutchType_observed = ClutchType,
                    NumberChicksMass = NumberChickMass,
                    NumberChicksTarsus = NumberTarsus,
                    LayDate_observed = LayDate,
                    LayDate_min = as.Date(NA),
                    LayDate_max = as.Date(NA),
                    ClutchSize_observed = ClutchSize,
                    #ClutchSize_min = NA_integer_, # Now added above
                    ClutchSize_max = NA_integer_,
                    HatchDate_observed = HatchDate,
                    HatchDate_min = as.Date(NA),
                    HatchDate_max = as.Date(NA),
                    BroodSize_observed = BroodSize,
                    BroodSize_min = NA_integer_,
                    BroodSize_max = NA_integer_,
                    FledgeDate_observed = as.Date(NA),
                    FledgeDate_min = as.Date(NA),
                    FledgeDate_max = as.Date(NA),
                    NumberFledged_observed = NumberFledged,
                    NumberFledged_min = NA_integer_,
                    NumberFledged_max = NA_integer_,
                    AvgEggMass =  NA_real_,
                    NumberEggs = NA_integer_,
                    OriginalTarsusMethod = "Alternative",
                    ExperimentID = NA_character_) %>%

      # 6) Remove broods from species not included in Species_codes
      dplyr::filter(Species %in% Species_codes$Code) %>%

      # 7) Replace non-conclusive male and female IDs with NA
      dplyr::mutate(FemaleID = dplyr::case_when(!(FemaleID%in%badIDs) ~ FemaleID,
                                                FemaleID%in%badIDs ~ NA_character_),
                    MaleID = dplyr::case_when(!(MaleID%in%badIDs) ~ MaleID,
                                                MaleID%in%badIDs ~ NA_character_))

  # 8) Convert ring numbers for re-ringed females and males
  ReRingTableF <- ReRingTable %>%
    dplyr::rename(FemaleID = .data$RingNr,
                  FemaleReRingID = .data$ReRingID)

  ReRingTableM <- ReRingTable %>%
    dplyr::rename(MaleID = .data$RingNr,
                  MaleReRingID = .data$ReRingID)

  Brood_data <- Brood_data %>%
    dplyr::left_join(ReRingTableF, by = 'FemaleID') %>%
    dplyr::left_join(ReRingTableM, by = 'MaleID') %>%
    dplyr::mutate(FemaleID = ifelse(is.na(.data$FemaleReRingID), .data$FemaleID, .data$FemaleReRingID),
                  MaleID = ifelse(is.na(.data$MaleReRingID), .data$MaleID, .data$MaleReRingID)) %>%

      # 9) Select and arrange columns for output
      dplyr::select(BroodID, PopID, BreedingSeason,
                    Species, Plot, LocationID, FemaleID, MaleID,
                    ClutchType_observed, ClutchType_calculated,
                    LayDate_observed, LayDate_min, LayDate_max,
                    ClutchSize_observed, ClutchSize_min, ClutchSize_max,
                    HatchDate_observed, HatchDate_min, HatchDate_max,
                    BroodSize_observed, BroodSize_min, BroodSize_max,
                    FledgeDate_observed, FledgeDate_min, FledgeDate_max,
                    NumberFledged_observed, NumberFledged_min, NumberFledged_max,
                    AvgEggMass, NumberEggs,
                    AvgChickMass, NumberChicksMass,
                    AvgTarsus, NumberChicksTarsus,
                    OriginalTarsusMethod,
                    ExperimentID, ChickAge)


  # Return data
  return(tibble::as_tibble(Brood_data))

  # Satisfy RCMD check
  PopID <- BroodID <- PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- NULL
  ClutchType <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDate_observed <- LayDate_min <- LayData_max <- NULL
  ClutchSize <- ClutchSize_observed <- ClutchSize_min <- ClutchSize_max <- NULL
  HatchDate <- HatchDate_observed <- HatchDate_min <- HatchData_max <- NULL
  BroodSize <- BroodSize_observed <- BroodSize_min <- BroodSize_max <- NULL
  FledgeDate <- FledgeDate_observed <- FledgeDate_min <- FledgeDate_max <- NULL
  NumberFledged <- NumberFledged_observed <- NumberFledged_min <- NumberFledged_max <- NULL
  AvgEggMass <- AvgChickMass <- AvgTarsus <- NULL
  NumberEggs <- NumberChicksMass <- NumberChicksTarsus <- NumberChickMass <- NumberTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

  Year <- Box <- Popn <- CltCd <- CltSize <- Hatch <- Fledged <- Success <- NULL
  DAR <- N1 <- DFE <- DH <- MinEggs <- UnHatch <- AverageMass <- Outcome <- NULL
  MaleDate <- MaleRingDate <- FemaleDate <- FemaleRingDate <- FemaleStatus <- FemaleMinAge <- NULL
  YoungDate <- Young1 <- Young2 <- Young3 <- Young4 <- Young5 <- Young6 <- NULL
  Young7 <- Young8 <- Young9 <- Young10 <- Young11 <- NULL
  Weight.Y1 <- Weight.Y2 <- Weight.Y3 <- Weight.Y4 <- Weight.Y5 <- Weight.Y6 <- NULL
  Weight.Y7 <- Weight.Y8 <- Weight.Y9 <- Weight.Y10 <- Weight.Y11  <- NULL
  Tarsus.Y1 <- Tarsus.Y2 <- Tarsus.Y3 <- Tarsus.Y4 <- Tarsus.Y5 <- Tarsus.Y6 <- NULL
  Tarsus.Y7 <- Tarsus.Y8 <- Tarsus.Y9 <- Tarsus.Y10 <- Tarsus.Y11  <- NULL

}



#' Create capture data table for EastDartmoor from ringing data.
#'
#' @param CMR_data Complete raw PiedFlyNet ringing data.
#' @param Primary_data Primary brood data from EastDartmoor.
#' @param ReRingTable Table containing ring numbers and unique identifiers for re-ringed individuals
#'
#' @return A data frame with Capture data

create_capture_EDM <- function(CMR_data, Primary_data, ReRingTable){

  # 1) Add information on re-ringing IDs to capture data
  ReRingTable <- ReRingTable %>%
    dplyr::rename(RING = .data$RingNr)

  Capture_data <- CMR_data %>%
    dplyr::left_join(ReRingTable, by = 'RING') %>%
    dplyr::mutate(RING = ifelse(is.na(.data$ReRingID), .data$RING, .data$ReRingID))

  ## 2) Subset ringing database to contain only captures relevant to population
  LocationList <- unique(Primary_data$Popn)
  Capture_data <- subset(Capture_data, PLACE %in% LocationList)

  ## 3) Rename columns that are equivalent (content and format) to columns in the standard format
  Capture_data <- Capture_data %>%
    dplyr::rename(IndvID = .data$RING,
                  CapturePlot = .data$PLACE,
                  LocationID = .data$SITE,
                  Sex_observed = .data$SEX) %>%

    ## 4) Add population ID and reformat colums with equivalent content but different format
  #Capture_data <- Capture_data %>%
    dplyr::mutate(PopID = "EDM",
                  Species = dplyr::case_when(.data$SPEC == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .data$SPEC == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                             .data$SPEC == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .data$SPEC == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                             .data$SPEC == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             .data$SPEC == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .data$SPEC == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code),
                    CaptureDate = as.Date(.data$DATE, format = "%d/%m/%Y"),
                    CaptureTime = ifelse(!grepl("00:00", Capture_data$DATE), sub(".* ", "", Capture_data$DATE), NA),
                    ObserverID = dplyr::case_when(!is.na(.data$INIT) ~ .data$INIT,
                                                  is.na(.data$INIT) & !is.na(.data$RINGINIT) ~ .data$RINGINIT),
                    CapturePopID = "EDM",
                    ReleasePopID = "EDM",
                    ReleasePlot = CapturePlot,
                    Mass_CMR = as.numeric(.data$WT),
                    Tarsus_CMR = dplyr::case_when(is.na(.data$TSMTD) ~ as.numeric(.data$TARSUS),
                                              .data$TSMTD == "S" ~ as.numeric(.data$TARSUS),
                                              .data$TSMTD == "M" ~ (as.numeric(.data$TARSUS)*0.72005)+3.64549),
                    OriginalTarsusMethod = dplyr::case_when(is.na(.data$TSMTD) ~ "Alternative",
                                                            .data$TSMTD == "S" ~ "Alternative",
                                                            .data$TSMTD == "M" ~ "Oxford"),
                    WingLength = as.numeric(.data$WING),
                    Age_observed = as.integer(str_remove(.data$AGE, "J")),
                    Age_calculated = NA_integer_, # Will be added later
                    #ChickAge = NA_integer_, # Will be added later
                    stringsAsFactors = FALSE) %>%

    ## 5) Add additional colums that need to be derived from original columns
    #Capture_data <- Capture_data %>%
    dplyr::mutate(BreedingSeason = as.integer(format(.data$CaptureDate, "%Y")),
                  CaptureAlive = ifelse(.data$RTYPE == "X", FALSE, TRUE),
                  ReleaseAlive = ifelse(.data$RTYPE == "X", FALSE, TRUE),
                  ExperimentID = dplyr::case_when(.data$COND == "M" ~ "SURVIVAL",
                                                  is.na(.data$COND) ~ NA_character_)
    )



  ## 6) Add chick measurement and brood ID data from brood table (primary data)

  # Extract chick mass, tarsus, and BroodID from brood table
  Chick_Capture_list <- list()
  for(i in 1:nrow(Primary_data)){
    Chick_Capture_list[[i]] <- data.frame(IndvID = unname(t(Primary_data[i, paste0("Young", c(1:11))])),
                                          BroodID = Primary_data$BroodID[i], # Will be removed later
                                          CaptureDate = as.Date(Primary_data$YoungDate[i], format = "%d/%m/%Y"),

                                          Mass_B = as.numeric(Primary_data[i, paste0("Weight.Y", c(1:11))]),
                                          Tarsus_B = as.numeric(Primary_data[i, paste0("Tarsus.Y", c(1:11))]),
                                          stringsAsFactors = FALSE)
  }

  Chick_Capture_data <- dplyr::bind_rows(Chick_Capture_list)
  Chick_Capture_data <- subset(Chick_Capture_data, !is.na(IndvID))

  # Merge additional chick data into capture data
  Capture_data <- Capture_data %>%
    dplyr::left_join(Chick_Capture_data, by = c("IndvID", "CaptureDate")) %>%

  # Summarise information on chick mass and tarsus measurements
    dplyr::mutate(Mass = dplyr::case_when(!is.na(.data$Mass_CMR) ~ .data$Mass_CMR,
                                          is.na(.data$Mass_CMR) & !is.na(.data$Mass_B) ~ .data$Mass_B),
                  Tarsus = dplyr::case_when(!is.na(.data$Tarsus_CMR) ~ .data$Tarsus_CMR,
                                   is.na(.data$Tarsus_CMR) & !is.na(.data$Tarsus_B) ~ .data$Tarsus_B)) %>%

    ## 7) Exclude entries not included in the standard format

    # Remove all individuals from species not included in Species_codes
    dplyr::filter(.data$Species %in% Species_codes$Code) %>%

    # Remove all observations not involving a capture/dead recovery (i.e. resightings)
    dplyr::filter(.data$RTYPE != 'O') %>%

    ## 8) Calculate age at capture

    # Sort chronologically within individual
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$CaptureTime) %>%

      #Calculate age at each capture based on first capture
    calc_age(ID = .data$IndvID, Age = .data$Age_observed, Date = .data$CaptureDate, Year = .data$BreedingSeason) %>%

    ## 9) Make CaptureID

    # Write unique capture identifier as IndvID-CaptureNumber
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste0(.data$IndvID, '-', dplyr::row_number())) %>%
    dplyr::ungroup() %>%

    ## 10) Select required columns
    dplyr::select(CaptureID, IndvID, Species, Sex_observed,
                  BreedingSeason, CaptureDate, CaptureTime, ObserverID,
                  LocationID, CaptureAlive, ReleaseAlive,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot,
                  Mass, Tarsus, OriginalTarsusMethod, WingLength,
                  Age_observed, Age_calculated, ExperimentID,
                  BroodID)

  ## 11) Return data
  return(Capture_data)
}

#' Create individual data table for EastDartmoor.
#'
#' @param Capture_data Capture data table generated by create_capture_EDM
#'
#' @return A data frame with Individual data

create_individual_EDM <- function(Capture_data){

  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::arrange(IndvID, BreedingSeason, CaptureDate, CaptureTime) %>%
    dplyr::group_by(IndvID) %>%

    #* CN: Loop over the species assigned to each individual and check the number of different assignments

    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CCCCCC")

      }

    }), PopID = "EDM",
    BroodIDLaid = first(BroodID),
    BroodIDFledged = BroodIDLaid, #* CN: This would have to be different if there were cross-fostering experiments
    RingSeason = first(BreedingSeason),
    RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
    Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(Sex_observed))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("C")

      }

    })) %>%
    dplyr::rowwise() %>%


    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
                  BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%

    # Add a column for genetic sex (not available here)
    dplyr::mutate(Sex_genetic = NA_character_) %>%

    # Sort
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

  #Satisfy RCMD Check
  Species <- IndvID <- PopID <- BroodIDLaid <- BroodIDFledged <- NULL
  RingSeason <- RingAge <- Sex <- NULL

  PopID <- Species <- IndvID <- BroodID <- BreedingSeason <- LocationID <- Plot <- Sex_observed <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- CapturePlot <- NULL
  ReleasePopID <- ReleasePlot <- Mass <- Tarsus <- OriginalTarsusMethod <- NULL
  WingLength <- Age_observed <- Age_calculated <- NULL
  CaptureAlive <- ReleaseAlive <- ExperimentID <- NULL
}

#' Create location data table for EastDartmoor.
#'
#' @param Brood_data Brood data table generated by create_brood_EDM
#' @param Capture_data Capture data table generated by create_capture_EDM
#'
#' @return A data frame with Location data

create_location_EDM <- function(Brood_data, Capture_data){

  # 1) Load and format additional data on nest boxes
  Location_details <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_EDartmoor_Nestboxes.csv"), na.strings = c("", "?"),colClasses = "character") %>%

    dplyr::rename(NestboxID = Box,
                  Plot = Popn,
                  Latitude = lat,
                  Longitude = long,
                  StartSeason = First,
                  EndSeason = Last) %>%

    dplyr::mutate(Latitude = suppressWarnings(as.numeric(Latitude)),
                  Longitude = suppressWarnings(as.numeric(Longitude)),
                  StartSeason = suppressWarnings(as.integer(StartSeason)),
                  EndSeason = suppressWarnings(as.integer(EndSeason)))

  # 2) Start a dataframe with all unique LocationIDs / NestboxIDs from brood data
  # NOTE: In this case, they are identical. Otherwise, this would have to be all unique combinations of LocationID and NestboxID

  Location_data <- tibble(LocationID = unique(Brood_data$LocationID),
                          NestboxID = unique(Brood_data$LocationID)) %>%

    # 3) Add additional columns
    dplyr::mutate(LocationType = 'NB',
                  PopID = 'EDM',
                  HabitatType = 'deciduous') %>%

    # 4) Merge in detailed information
    dplyr::left_join(Location_details, by = "NestboxID")


  # 5) Collect information on additional capture locations (from capture data)
  CapLocations <- tibble(LocationID = unique(Capture_data$LocationID)) %>%
    dplyr::filter(!(.data$LocationID %in% Location_data$LocationID)) %>%
    dplyr::mutate(NestboxID = NA_character_,
                  LocationType = NA_character_,
                  PopID = 'EDM',
                  Latitude = NA_real_,
                  Longitude = NA_real_,
                  StartSeason = NA_integer_,
                  EndSeason = NA_integer_,
                  HabitatType = 'deciduous')

  # 6) Combine data and select relevant columns
  Location_data <- Location_data %>%
    dplyr::bind_rows(CapLocations) %>%
    dplyr::select(LocationID, NestboxID,
                  LocationType, PopID,
                  Latitude, Longitude,
                  StartSeason, EndSeason,
                  HabitatType)

  return(Location_data)

  # Satisfy RCMD check
  Box <- Popn <- lat <- long <- First <- Last <- NULL
  LocationID <- NestboxID <- LocationType <- PopID <- NULL
  Latitude <- Longitude <- StartSeason <- EndSeason <- HabitatType <- NULL

  BroodID <- BreedingSeason <- Species <- Plot <- NULL
  FemaleID <- MaleID <- NULL
  ClutchType <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDate_observed <- LayDate_min <- LayData_max <- NULL
  ClutchSize <- ClutchSize_observed <- ClutchSize_min <- ClutchSize_max <- NULL
  HatchDate <- HatchDate_observed <- HatchDate_min <- HatchData_max <- NULL
  BroodSize <- BroodSize_observed <- BroodSize_min <- BroodSize_max <- NULL
  FledgeDate <- FledgeDate_observed <- FledgeDate_min <- FledgeDate_max <- NULL
  NumberFledged <- NumberFledged_observed <- NumberFledged_min <- NumberFledged_max <- NULL
  AvgEggMass <- NumberEggs <- AvgChickMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

}



#---------------------------------------
# NOTES:
# Automatically add template for function documentation: cmd+alt+shift+R ("Insert Roxygen Skeleton")
