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
#'\strong{Primary data sources}: Brood data is extracted from data in a
#'brood table/nest monitoring format. 'Capture data is extracted from Malcolm Burgess'
#'version of the BTO ringing database for all birds marked within the PiedFlyNet.
#'The format of this ringing data is IPMR, the 'old' BTO ringing database format, and Malcolm
#'has his own pipeline to convert the new BTO ringing database format to the old one.
#'Since all chick measurements are complete in the brood table/nest monitoring data but some have
#'not been added to the BTO ringing data, the pipeline merges chick trait values from the former
#'into Capture data.
#'
#'\strong{Unidentified individuals}: Some individuals have not been given unique IDs. They appear
#'in the primary data as: "RUNT", "ringed", "ringed left", "ringed right", "unringed", or "Unringed".
#'In the Brood data, we assume unknown identity and treat ID as NA. From Capture data and Individual
#'data, these individuals are omitted entirely.
#'
#'\strong{Re-ringing events}: Some individuals carry multiple rings (up to 3 in this data) throughout
#'their lives, and re-ringing events are recorded in the BTO ringing database. The pipeline identifies
#'birds with several rings, and assigns them a new unique individual identifier of the the form
#''MULTIRINGx', where 'x' is an integer number.
#'
#'\strong{LayDate_observed and HatchDate_observed}: Information is provided as date of first egg (DFE) and
#'date of hatching (DH). These are given as integer number, and represent days after a set
#'starting date (day 0). Day 0 is assumed to be 31. March every year (to be confirmed).
#'
#'\strong{ChickAge}: For every capture, we estimate the age of a chick as the difference between the hatch date
#'taken from BroodIDFledged (in Individual_data) and the CaptureDate.
#'
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
    dplyr::select(.data$RingNr, .data$ReRingID)


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
    dplyr::select(-.data$BroodID)

  # Brood data: Remove unnecessary columns
  Brood_data <- Brood_data %>%
    dplyr::select(-.data$ChickAge)

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
#Create a vector of all non-NA ID records for both males and females (need for chicks too?)
allIDs <- na.omit(c(Primary_data$MaleID, Primary_data$FemaleID))
#Return only those that don't match the expected ringing format (i.e. XXX9999)
#"^[A-Z]{1,}[0-9]{1,}$" is a regular expression that looks for IDs that follow a pattern:
#- Starts with at least one capital letter: '^[A-Z]{1,}'
#- Ends with at least one number: '[0-9]{1,}$'
badIDs <- unique(allIDs[!stringr::str_detect(allIDs, pattern = "^[A-Z]{1,}[0-9]{1,}$")])


  ## 1) Rename columns that are equivalent (content and format) to columns in the standard format
  Brood_data <- Primary_data %>%
    dplyr::rename(Plot = .data$Popn,
                  LocationID = .data$Box) %>%

    ## 2) Add population ID and reformat colums with equivalent content but different format
    dplyr::mutate(PopID = "EDM",
                  BreedingSeason = as.integer(Year),
                  Species = dplyr::case_when(.data$Species == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                             .data$Species == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                             .data$Species == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                             .data$Species == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                             .data$Species == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                             .data$Species == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                             .data$Species == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                             .data$Species == "WREN" ~ NA_character_, # Missing, 1 observation only
                                             .data$Species == "TREEC" ~ NA_character_), # Missing, 1 observation only
                  ClutchType = dplyr::case_when(.data$CltCd == "1" ~ "first",
                                                .data$CltCd == "2" ~ "replacement",
                                                .data$CltCd == "3" ~ "second"),
                  LayDate = as.Date(paste('31/03/', .data$BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(.data$DFE),
                  ClutchSize = dplyr::case_when(as.integer(.data$CltSize) > 0 ~ as.integer(.data$CltSize),
                                                as.integer(.data$CltSize) == 0 | is.na(.data$CltSize) ~ NA_integer_),
                  ClutchSize_min = dplyr::case_when(is.na(.data$CltSize) | as.integer(.data$CltSize) == 0 ~ suppressWarnings(pmax(as.integer(.data$MinEggs), as.integer(.data$UnHatch)+as.integer(.data$Hatch), as.integer(.data$UnHatch)+as.integer(.data$Fledged), na.rm = TRUE))),
                  HatchDate = as.Date(paste('31/03/', .data$BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(.data$DH),
                  BroodSize = as.integer(.data$Hatch),
                  NumberFledged = as.integer(.data$Fledged),
                  ChickAge = as.integer(as.Date(.data$YoungDate, format = "%d/%m/%Y") - .data$HatchDate)) %>%

    # 4) Add columns that are based on calculations
    dplyr::arrange(.data$BreedingSeason, .data$FemaleID, .data$LayDate)
    # --> This sorting is required for the function "calc_clutchtype" to work

    # 4.1) Calculate means and observation numbers for fledgling weight/tarsus length
    Brood_averages <- Brood_data %>%
    dplyr::filter(between(.data$ChickAge, 14, 16)) %>%  #Remove chicks outside the age range
    dplyr::select(.data$BroodID, contains("Weight."), contains("Tarsus.")) %>% #Select only weight and tarsus cols
    tidyr::pivot_longer(cols = -.data$BroodID) %>%  #Rearrange so they're individual rows rather than individual columns
    dplyr::filter(!is.na(.data$value)) %>% #Remove Nas
    dplyr::mutate(name = gsub(pattern = "Weight", replacement = "ChickMass", gsub(pattern = ".Y\\d+", replacement = "", x = .data$name))) %>% #Change the names so it's not 'Weight.Y1' and 'Tarsus.Y1' but just 'ChickMass' and 'Tarsus'
    dplyr::group_by(.data$BroodID, .data$name) %>% #Group by brood, weight and tarsus
    dplyr::summarise(Avg = mean(as.numeric(.data$value), na.rm = TRUE), Number = n()) %>% #Extract the mean and sample size
    tidyr::pivot_wider(names_from = .data$name, values_from = c(.data$Avg, .data$Number), names_sep = "") %>% #Move this back into cols so we have AvgChickMass, NumberChickMass etc.
    dplyr::ungroup()

    # 4.2) Add calculated columns
    Brood_data <- Brood_data %>%
      dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = Brood_data, na.rm = FALSE)) %>%
      dplyr::left_join(Brood_averages, by = "BroodID")


  # 5) Rename columns and add columns without data
  Brood_data <- Brood_data %>%
      dplyr::mutate(ClutchType_observed = .data$ClutchType,
                    NumberChicksMass = .data$NumberChickMass,
                    NumberChicksTarsus = .data$NumberTarsus,
                    LayDate_observed = .data$LayDate,
                    LayDate_min = as.Date(NA),
                    LayDate_max = as.Date(NA),
                    ClutchSize_observed = .data$ClutchSize,
                    #ClutchSize_min = NA_integer_, # Now added above
                    ClutchSize_max = NA_integer_,
                    HatchDate_observed = .data$HatchDate,
                    HatchDate_min = as.Date(NA),
                    HatchDate_max = as.Date(NA),
                    BroodSize_observed = .data$BroodSize,
                    BroodSize_min = NA_integer_,
                    BroodSize_max = NA_integer_,
                    FledgeDate_observed = as.Date(NA),
                    FledgeDate_min = as.Date(NA),
                    FledgeDate_max = as.Date(NA),
                    NumberFledged_observed = .data$NumberFledged,
                    NumberFledged_min = NA_integer_,
                    NumberFledged_max = NA_integer_,
                    AvgEggMass =  NA_real_,
                    NumberEggs = NA_integer_,
                    OriginalTarsusMethod = "Alternative",
                    ExperimentID = NA_character_) %>%

      # 6) Remove broods from species not included in Species_codes
      dplyr::filter(.data$Species %in% Species_codes$Code) %>%

      # 7) Replace non-conclusive male and female IDs with NA
      dplyr::mutate(FemaleID = dplyr::case_when(!(.data$FemaleID%in%badIDs) ~ .data$FemaleID,
                                                .data$FemaleID%in%badIDs ~ NA_character_),
                    MaleID = dplyr::case_when(!(.data$MaleID%in%badIDs) ~ .data$MaleID,
                                              .data$MaleID%in%badIDs ~ NA_character_))

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
      dplyr::select(.data$BroodID, .data$PopID, .data$BreedingSeason,
                    .data$Species, .data$Plot, .data$LocationID, .data$FemaleID, .data$MaleID,
                    .data$ClutchType_observed, .data$ClutchType_calculated,
                    .data$LayDate_observed, .data$LayDate_min, .data$LayDate_max,
                    .data$ClutchSize_observed, .data$ClutchSize_min, .data$ClutchSize_max,
                    .data$HatchDate_observed, .data$HatchDate_min, .data$HatchDate_max,
                    .data$BroodSize_observed, .data$BroodSize_min, .data$BroodSize_max,
                    .data$FledgeDate_observed, .data$FledgeDate_min, .data$FledgeDate_max,
                    .data$NumberFledged_observed, .data$NumberFledged_min, .data$NumberFledged_max,
                    .data$AvgEggMass, .data$NumberEggs,
                    .data$AvgChickMass, .data$NumberChicksMass,
                    .data$AvgTarsus, .data$NumberChicksTarsus,
                    .data$OriginalTarsusMethod,
                    .data$ExperimentID, .data$ChickAge)


  # Return data
  return(tibble::as_tibble(Brood_data))
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
                    ReleasePlot = .data$CapturePlot,
                    Mass_CMR = as.numeric(.data$WT),
                    Tarsus_CMR = dplyr::case_when(is.na(.data$TSMTD) ~ as.numeric(.data$TARSUS),
                                              .data$TSMTD == "S" ~ as.numeric(.data$TARSUS),
                                              .data$TSMTD == "M" ~ (x = as.numeric(.data$TARSUS), method = "Oxford"),
                    OriginalTarsusMethod = dplyr::case_when(is.na(.data$TSMTD) ~ "Alternative",
                                                            .data$TSMTD == "S" ~ "Alternative",
                                                            .data$TSMTD == "M" ~ "Oxford"),
                    WingLength = as.numeric(.data$WING),
                    Age_observed = as.integer(str_remove(.data$AGE, "J")),
                    Age_calculated = NA_integer_, # Will be added later
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
                                          #CaptureDate = as.Date(Primary_data$YoungDate[i], format = "%d/%m/%Y"),

                                          Mass_B = as.numeric(Primary_data[i, paste0("Weight.Y", c(1:11))]),
                                          Tarsus_B = as.numeric(Primary_data[i, paste0("Tarsus.Y", c(1:11))]),
                                          stringsAsFactors = FALSE)
  }

  Chick_Capture_data <- dplyr::bind_rows(Chick_Capture_list)
  Chick_Capture_data <- subset(Chick_Capture_data, !is.na(IndvID))

  # Merge additional chick data into capture data
  #Capture_data <- Capture_data %>%
  #  dplyr::left_join(Chick_Capture_data, by = c("IndvID", "CaptureDate")) %>%

  Capture_data <- Capture_data %>%
    dplyr::left_join(Chick_Capture_data, by = "IndvID") %>%

  # Summarise information on chick mass and tarsus measurements
    #dplyr::mutate(Mass = dplyr::case_when(!is.na(.data$Mass_CMR) ~ .data$Mass_CMR,
    #                                      is.na(.data$Mass_CMR) & !is.na(.data$Mass_B) ~ .data$Mass_B),
    #              Tarsus = dplyr::case_when(!is.na(.data$Tarsus_CMR) ~ .data$Tarsus_CMR,
    #                               is.na(.data$Tarsus_CMR) & !is.na(.data$Tarsus_B) ~ .data$Tarsus_B)) %>%
    dplyr::mutate(Mass = dplyr::case_when(!is.na(.data$Mass_CMR) ~ .data$Mass_CMR,
                                          is.na(.data$Mass_CMR) & !is.na(.data$Mass_B) & .data$Age_observed == 1 ~ .data$Mass_B,
                                          is.na(.data$Mass_CMR) & !is.na(.data$Mass_B) & .data$Age_observed != 1 ~ NA_real_),
                  Tarsus = dplyr::case_when(!is.na(.data$Tarsus_CMR) ~ .data$Tarsus_CMR,
                                            is.na(.data$Tarsus_CMR) & !is.na(.data$Tarsus_B) & .data$Age_observed == 1 ~ .data$Tarsus_B,
                                            is.na(.data$Tarsus_CMR) & !is.na(.data$Tarsus_B) & .data$Age_observed != 1 ~ NA_real_)) %>%

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
    dplyr::select(.data$CaptureID, .data$IndvID, .data$Species, .data$Sex_observed,
                  .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime, .data$ObserverID,
                  .data$LocationID, .data$CaptureAlive, .data$ReleaseAlive,
                  .data$CapturePopID, .data$CapturePlot, .data$ReleasePopID, .data$ReleasePlot,
                  .data$Mass, .data$Tarsus, .data$OriginalTarsusMethod, .data$WingLength,
                  .data$Age_observed, .data$Age_calculated, .data$ExperimentID,
                  .data$BroodID)

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
    dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
    dplyr::group_by(.data$IndvID) %>%

    #* CN: Loop over the species assigned to each individual and check the number of different assignments

    dplyr::summarise(Species = purrr::map_chr(.x = list(unique(na.omit(.data$Species))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("CCCCCC")

      }

    }), PopID = "EDM",
    BroodIDLaid = first(.data$BroodID),
    BroodIDFledged = .data$BroodIDLaid, # Identical, as no cross-fostering experiments were made
    RingSeason = first(.data$BreedingSeason),
    RingAge = ifelse(any(.data$Age_calculated %in% c(1, 3)), "chick", ifelse(min(.data$Age_calculated) == 2, NA_character_, "adult")),
    Sex_calculated = purrr::map_chr(.x = list(unique(na.omit(.data$Sex_observed))), .f = ~{

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
    dplyr::mutate(BroodIDLaid = ifelse(.data$RingAge == "chick", .data$BroodIDLaid, NA),
                  BroodIDFledged = .data$BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%

    # Add a column for genetic sex (not available here)
    dplyr::mutate(Sex_genetic = NA_character_) %>%

    # Sort
    dplyr::arrange(.data$RingSeason, .data$IndvID)

  return(Indv_data)
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

    dplyr::rename(NestboxID = .data$Box,
                  Plot = .data$Popn,
                  Latitude = .data$lat,
                  Longitude = .data$long,
                  StartSeason = .data$First,
                  EndSeason = .data$Last) %>%

    dplyr::mutate(Latitude = suppressWarnings(as.numeric(.data$Latitude)),
                  Longitude = suppressWarnings(as.numeric(.data$Longitude)),
                  StartSeason = suppressWarnings(as.integer(.data$StartSeason)),
                  EndSeason = suppressWarnings(as.integer(.data$EndSeason)))

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
    dplyr::select(.data$LocationID, .data$NestboxID,
                  .data$LocationType, .data$PopID,
                  .data$Latitude, .data$Longitude,
                  .data$StartSeason, .data$EndSeason,
                  .data$HabitatType)

  return(Location_data)
}



#---------------------------------------
# NOTES:
# Automatically add template for function documentation: cmd+alt+shift+R ("Insert Roxygen Skeleton")
