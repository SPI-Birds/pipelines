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
#'\strong{LayDate and HatchDate}: Information is provided as date of first egg (DFE) and
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

  #Load primary data
  Primary_data <- utils::read.csv(file = paste0(db, "/PFN_PrimaryData_EDartmoor.csv"), na.strings = c("", "?"),
                                colClasses = "character")
  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_PFN(Primary_data = Primary_data)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_PFN(Primary_data = Primary_data)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_PFN(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_PFN(Brood_data = Brood_data)


  # WRANGLE DATA FOR EXPORT

  # Capture data: Merge in information on ChickAge (from Brood_data) & remove unnecessary columns
  ChickAge_data <- Brood_data[,c('BroodID', 'ChickAge')]
  Capture_data <- dplyr::left_join(Capture_data, ChickAge_data, by = 'BroodID')
  Capture_data$ChickAge[which(Capture_data$Age_observed > 1)] <- NA_integer_

  Capture_data <- Capture_data %>%
    dplyr::select(-BroodID, -Sex)

  # Brood data: Remove unnecessary columns
  Brood_data <- Brood_data %>%
    dplyr::select(-ChickAge)

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PFN.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PFN.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_PFN.csv"), row.names = F)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PFN.csv"), row.names = F)

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
#'
#' @return A data frame with Brood data


create_brood_PFN <- function(Primary_data){

  ## Pre) Determine a vector of "bad" (nonconclusive) IDs
  #This will be used downstream in point 7)
  badIDs <- c("RUNT", "ringed", "ringed left", "ringed right", "unringed", "Unringed")

  ## 0) Load data
  #We read everything in as text and convert it afterwards
  #Even though some columns (e.g. date) work well, they may be broken with newer data.
  #Using text and converting manually should be more robust to data changes

    Brood_data <- Primary_data %>%

    ## 1) Rename columns that are equivalent (content and format) to columns in the standard format
    dplyr::rename(Plot = Popn,
                  LocationID = Box) %>%

    ## 2) Add population ID and reformat colums with equivalent content but different format
    dplyr::mutate(PopID = "PFN",
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
                  ClutchType_observed = dplyr::case_when(CltCd == "1" ~ "first",
                                                        CltCd == "2" ~ "replacement",
                                                        CltCd == "3" ~ "second"), # Needs double-checking with data owner
                  LayDate = as.Date(paste('31/03/', BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(DFE),
                  # NOTE: The date calculations require knowing from which date onwards DFE and DH are determined.
                  #       In Smith et al. (2011), a paper mentioned on PiedFly.Net and of which Malcolm is a co-author, I found that 61 corresponds to 31. May. This would indicate that the "baseline" is 31. March, with DFE or DH = 1 corresponding to 1. April
                  #       This NEEDS TO BE VERIFIED by the data owner!
                  #
                  #       REFERENCE:
                  #       Smith, Ken W., et al. "Large-scale variation in the temporal patterns of the frass fall of defoliating caterpillars in oak woodlands in Britain: implications for nesting woodland birds." Bird Study 58.4 (2011): 506-511.

                  ClutchSize = as.integer(CltSize),
                  HatchDate = as.Date(paste('31/03/', BreedingSeason, sep = ''), format = "%d/%m/%Y") + as.numeric(DH), # See note on LayingDate
                  BroodSize = as.integer(Hatch),
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
      dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = Brood_data, na.rm = FALSE),
                    NumberEggs = as.integer(Hatch) + as.integer(UnHatch)) %>%
      dplyr::left_join(Brood_averages, by = "BroodID")


  # 5) Rename chick sample size columns and add columns without data
  Brood_data <- Brood_data %>%
      dplyr::mutate(NumberChicksMass = NumberChickMass,
                    NumberChicksTarsus = NumberTarsus,
                    LayDateError = NA_real_,
                    ClutchSizeError = NA_real_,
                    HatchDateError = NA_real_,
                    BroodSizeError = NA_real_,
                    FledgeDate = as.Date(NA),
                    FledgeDateError = NA_real_,
                    NumberFledgedError = NA_real_,
                    AvgEggMass =  NA_real_,
                    NumberEggs = NA_integer_,
                    OriginalTarsusMethod = NA_character_,
                    ExperimentID = NA_character_) %>%

      # 6) Remove broods from species not included in Species_codes
      dplyr::filter(Species %in% Species_codes$Code) %>%

      # 7) Replace non-conclusive male and female IDs with NA
      dplyr::mutate(FemaleID = dplyr::case_when(!(FemaleID%in%badIDs) ~ FemaleID,
                                                FemaleID%in%badIDs ~ NA_character_),
                    MaleID = dplyr::case_when(!(MaleID%in%badIDs) ~ MaleID,
                                                MaleID%in%badIDs ~ NA_character_)) %>%

      # 8) Select and arrange columns for output
      dplyr::select(BroodID, PopID, BreedingSeason,
                    Species, Plot, LocationID, FemaleID, MaleID,
                    ClutchType_observed, ClutchType_calculated,
                    LayDate, LayDateError,
                    ClutchSize, ClutchSizeError,
                    HatchDate, HatchDateError,
                    BroodSize, BroodSizeError,
                    FledgeDate, FledgeDateError,
                    NumberFledged, NumberFledgedError,
                    AvgEggMass, NumberEggs,
                    AvgChickMass, NumberChicksMass,
                    AvgTarsus, NumberChicksTarsus,
                    OriginalTarsusMethod,
                    ExperimentID, ChickAge)


  # Return data
  return(tibble::as_tibble(Brood_data))

  # Satisfy RCMD check
  PopID <- BroodID <- PopID <- BreedingSeason <- Species <- Plot <- LocationID <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
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

#' Create capture data table for EastDartmoor.
#'
#' @param Primary_data Primary data from EastDartmoor.
#'
#' @return A data frame with Capture data

create_capture_PFN <- function(Primary_data){

  Capture_data <- Primary_data

  # 1) Male capture data
  Male_Capture_data <- data.frame(IndvID = Capture_data$MaleID,
                                  BroodID = Capture_data$BroodID, # Will be removed later
                                  Species = dplyr::case_when(Capture_data$Species == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                                             Capture_data$Species == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                                             Capture_data$Species == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                                             Capture_data$Species == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                                             Capture_data$Species == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                                             Capture_data$Species == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                                             Capture_data$Species == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                                             Capture_data$Species == "WREN" ~ NA_character_,
                                                             Capture_data$Species == "TREEC" ~ NA_character_),
                                  BreedingSeason = as.integer(Capture_data$Year),
                                  CaptureDate = as.Date(Capture_data$MaleDate, format = "%d/%m/%Y"),
                                  #CaptureTime = strptime(NA, format = "%hh:%mm"),
                                  CaptureTime = NA_character_,
                                  ObserverID = NA_character_,
                                  LocationID = Capture_data$Box,
                                  CapturePopID = "PFN",
                                  CapturePlot = Capture_data$Popn,
                                  ReleasePopID = "PFN",
                                  ReleasePlot = Capture_data$Popn,
                                  Mass = NA_real_,
                                  Tarsus = NA_real_,
                                  OriginalTarsusMethod = NA_character_,
                                  WingLength = NA_real_,
                                  Age_observed = 4L,
                                  Age_calculated = NA_integer_, # Will be added later
                                  #ChickAge = NA_integer_, # Will be added later
                                  Sex = "M", # Will be removed later
                                  stringsAsFactors = FALSE)

  # 2) Female capture data
  Female_Capture_data <- data.frame(IndvID = Capture_data$FemaleID,
                                    BroodID = Capture_data$BroodID, # Will be removed later
                                    Species = dplyr::case_when(Capture_data$Species == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                                               Capture_data$Species == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                                               Capture_data$Species == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                                               Capture_data$Species == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                                               Capture_data$Species == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                                               Capture_data$Species == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                                               Capture_data$Species == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                                               Capture_data$Species == "WREN" ~ NA_character_,
                                                               Capture_data$Species == "TREEC" ~ NA_character_),
                                    BreedingSeason = as.integer(Capture_data$Year),
                                    CaptureDate = as.Date(Capture_data$FemaleDate, format = "%d/%m/%Y"),
                                    #CaptureTime = strptime(NA, format = "%hh:%mm"),
                                    CaptureTime = NA_character_,
                                    ObserverID = NA_character_,
                                    LocationID = Capture_data$Box,
                                    CapturePopID = "PFN",
                                    CapturePlot = Capture_data$Popn,
                                    ReleasePopID = "PFN",
                                    ReleasePlot = Capture_data$Popn,
                                    Mass = NA_real_,
                                    Tarsus = NA_real_,
                                    OriginalTarsusMethod = NA_character_,
                                    WingLength = NA_real_,
                                    Age_observed = 4L,
                                    Age_calculated = NA_integer_, # Will be added later
                                    #ChickAge = NA_integer_, # Will be added later
                                    Sex = "F", # Will be removed later
                                    stringsAsFactors = FALSE)

  # 3) Chick capture data
  Chick_Capture_list <- list()
  for(i in 1:nrow(Capture_data)){
    Chick_Capture_list[[i]] <- data.frame(IndvID = unname(t(Capture_data[i, paste0("Young", c(1:11))])),
                                          BroodID = Capture_data$BroodID[i], # Will be removed later
                                          Species = dplyr::case_when(Capture_data$Species[i] == "BLUTI" ~ Species_codes[Species_codes$SpeciesID == 14620, ]$Code,
                                                                     Capture_data$Species[i] == "PIEFL" ~ Species_codes[Species_codes$SpeciesID == 13490, ]$Code,
                                                                     Capture_data$Species[i] == "GRETI" ~ Species_codes[Species_codes$SpeciesID == 14640, ]$Code,
                                                                     Capture_data$Species[i] == "REDST" ~ Species_codes[Species_codes$SpeciesID == 11220, ]$Code,
                                                                     Capture_data$Species[i] == "MARTI" ~ Species_codes[Species_codes$SpeciesID == 14400, ]$Code,
                                                                     Capture_data$Species[i] == "NUTHA" ~ Species_codes[Species_codes$SpeciesID == 14790, ]$Code,
                                                                     Capture_data$Species[i] == "COATI" ~ Species_codes[Species_codes$SpeciesID == 14610, ]$Code,
                                                                     Capture_data$Species[i] == "WREN" ~ NA_character_,
                                                                     Capture_data$Species[i] == "TREEC" ~ NA_character_),
                                          BreedingSeason = as.integer(Capture_data$Year[i]),
                                          CaptureDate = as.Date(Capture_data$FemaleDate[i], format = "%d/%m/%Y"),
                                          #CaptureTime = strptime(NA, format = "%hh:%mm"),
                                          CaptureTime = NA_character_,
                                          ObserverID = NA_character_,
                                          LocationID = Capture_data$Box[i],
                                          CapturePopID = "PFN",
                                          CapturePlot = Capture_data$Popn[i],
                                          ReleasePopID = "PFN",
                                          ReleasePlot = Capture_data$Popn[i],
                                          Mass = as.numeric(Capture_data[i, paste0("Weight.Y", c(1:11))]),
                                          Tarsus = as.numeric(Capture_data[i, paste0("Tarsus.Y", c(1:11))]),
                                          OriginalTarsusMethod = NA_character_,
                                          WingLength = NA_real_,
                                          Age_observed = 1L,
                                          Age_calculated = NA_integer_, # Will be added later
                                          #ChickAge = NA_integer_, # Will be added later
                                          Sex = NA_character_, # Will be removed later
                                          stringsAsFactors = FALSE)
  }

  Chick_Capture_data <- dplyr::bind_rows(Chick_Capture_list)


  # 4) Combine data and remove missing IDs

  Capture_data <- dplyr::bind_rows(Chick_Capture_data, Male_Capture_data, Female_Capture_data) %>%

  # 5) Remove all individuals with missing IDs and/or from species not included in Species_codes
  dplyr::filter(Species %in% Species_codes$Code & !is.na(IndvID) & !(IndvID%in%c("RUNT", "ringed", "ringed left", "ringed right", "unringed", "Unringed"))) %>%

  # 6) Calculate age at capture

  # Sort chronologically within individual
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason)

  # 7) Return data
  return(Capture_data)

  #Satisfy RCMD Check
  PopID <- Species <- IndvID <- BroodID <- BreedingSeason <- LocationID <- Plot <- Sex <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- CapturePlot <- NULL
  ReleasePopID <- ReleasePlot <- Mass <- Tarsus <- OriginalTarsusMethod <- NULL
  WingLength <- Age_observed <- Age_calculated <- NULL

  Year <- Box <- Popn <- CltCd <- CltSize <- Hatch <- Fledged <- Success <- NULL
  DAR <- N1 <- DFE <- DH <- MinEggs <- UnHatch <- AverageMass <- Outcome <- NULL
  MaleID <- MaleDate <- MaleRingDate <- NULL
  FemaleID <- FemaleDate <- FemaleRingDate <- FemaleStatus <- FemaleMinAge <- NULL
  YoungDate <- Young1 <- Young2 <- Young3 <- Young4 <- Young5 <- Young6 <- NULL
  Young7 <- Young8 <- Young9 <- Young10 <- Young11 <- NULL
  Weight.Y1 <- Weight.Y2 <- Weight.Y3 <- Weight.Y4 <- Weight.Y5 <- Weight.Y6 <- NULL
  Weight.Y7 <- Weight.Y8 <- Weight.Y9 <- Weight.Y10 <- Weight.Y11  <- NULL
  Tarsus.Y1 <- Tarsus.Y2 <- Tarsus.Y3 <- Tarsus.Y4 <- Tarsus.Y5 <- Tarsus.Y6 <- NULL
  Tarsus.Y7 <- Tarsus.Y8 <- Tarsus.Y9 <- Tarsus.Y10 <- Tarsus.Y11  <- NULL

}

#' Create individual data table for EastDartmoor.
#'
#' @param Capture_data Capture data table generated by create_capture_PFN
#'
#' @return A data frame with Individual data

create_individual_PFN <- function(Capture_data){

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

        return("CONFLICTED")

      }

    }), PopID = "PFN",
    BroodIDLaid = first(BroodID),
    BroodIDFledged = BroodIDLaid, #* CN: This would have to be different if there were cross-fostering experiments
    RingSeason = first(BreedingSeason),
    RingAge = ifelse(any(Age_calculated %in% c(1, 3)), "chick", ifelse(min(Age_calculated) == 2, NA_character_, "adult")),
    Sex = purrr::map_chr(.x = list(unique(na.omit(Sex))), .f = ~{

      if(length(..1) == 0){

        return(NA_character_)

      } else if(length(..1) == 1){

        return(..1)

      } else {

        return("C") #* CN: Only one letter used here to keep it at the same length as sex

      }

    })) %>%
    dplyr::rowwise() %>%

    #* CN: Another way to do a for loop

    #For each individual, if their ring age was 1 or 3 (caught in first breeding year)
    #Then we take their first BroodID, otherwise it is NA
    dplyr::mutate(BroodIDLaid = ifelse(RingAge == "chick", BroodIDLaid, NA),
                  BroodIDFledged = BroodIDLaid) %>%
    #Ungroup to prevent warnings in debug report
    dplyr::ungroup() %>%
    dplyr::arrange(RingSeason, IndvID)

  return(Indv_data)

  #Satisfy RCMD Check
  Species <- IndvID <- PopID <- BroodIDLaid <- BroodIDFledged <- NULL
  RingSeason <- RingAge <- Sex <- NULL

  BroodID <- BreedingSeason <- LocationID <- Plot <- Age_observed <- NULL
  CaptureDate <- CaptureTime <- ObserverID <- CapturePopID <- CapturePlot <- NULL
  ReleasePopID <- ReleasePlot <- Mass <- Tarsus <- OriginalTarsusMethod <- NULL
  WingLength <- Age_observed <- Age_calculated <- NULL
}

#' Create location data table for EastDartmoor.
#'
#' @param Brood_data Brood data table generated by create_brood_PFN
#'
#' @return A data frame with Location data

create_location_PFN <- function(Brood_data){

  # 1) Start a dataframe with all unique LocationIDs / NestboxIDs
  # NOTE: In this case, they are identical. Otherwise, this would have to be all unique combinations of LocationID and NestboxID

  Location_data <- tibble(LocationID = unique(Brood_data$LocationID),
                              NestboxID = unique(Brood_data$LocationID)) %>%

    # 2) Add additional columns
    dplyr::mutate(LocationType = 'NB',
                  PopID = 'PFN',
                  Latitude = NA_real_,
                  Longitude = NA_real_,
                  Habitat = NA_character_) %>%

     # 3) Merge information on nestbox usage by birds
    dplyr::left_join(calc_birdNBuse(Brood_data = Brood_data), by = "LocationID") %>%

    # 4) Select relevant columns
    dplyr::select(LocationID, NestboxID,
                  LocationType, PopID,
                  Latitude, Longitude,
                  StartSeason, EndSeason,
                  Habitat)

  return(Location_data)

  # Satisfy RCMD check
  LocationID <- NestboxID <- LocationType <- PopID <- NULL
  Latitude <- Longitude <- StartSeason <- EndSeason <- Habitat <- NULL

  BroodID <- BreedingSeason <- Species <- Plot <- NULL
  FemaleID <- MaleID <- ClutchType_observed <- ClutchType_calculated <- NULL
  LayDate <- LayDateError <- ClutchSize <- ClutchSizeError <- NULL
  HatchDate <- HatchDateError <- BroodSize <- BroodSizeError <- NULL
  FledgeDate <- FledgeDateError <- NumberFledged <- NumberFledgedError <- NULL
  AvgEggMass <- NumberEggs <- AvgChickMass <- AvgTarsus <- NumberChicksTarsus <- NULL
  OriginalTarsusMethod <- ExperimentID <- NULL

}



#---------------------------------------
# NOTES:
# Automatically add template for function documentation: cmd+alt+shift+R ("Insert Roxygen Skeleton")
