#' Title
#'
#'Brief description
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

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_PFN(db = db)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_PFN(db = db)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_PFN(Capture_data = Capture_data)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_PFN(Brood_data = Brood_data)

  
  # WRANGLE DATA FOR EXPORT

  # Capture data: Merge in information on ChickAge (from Brood_data) & remove unnecessary columns
  ChickAge_data <- Brood_data[,c('BroodID', 'ChickAge')]
  Capture_data <- merge(Capture_data, ChickAge_data, by = 'BroodID', all.x = T, all.y = F)
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
#' @param db Location of primary data from EastDartmoor.
#'
#' @return A data frame with Brood data

# 0) Defining helper function
mean_countObs <- function(x){
  mean <- mean(as.numeric(x), na.rm = T)
  noObs <- length(which(!is.na(x)))
  return(c(mean, noObs))
}


create_brood_PFN <- function(db){

  ## Load data
  #We read everything in as text and convert it afterwards
  #Even though some columns (e.g. date) work well, they may be broken with newer data.
  #Using text and converting manually should be more robust to data changes

    Brood_data <- read.csv(file = paste0(db, "/PFN_PrimaryData.csv"), na.strings = c("", "?"),
                                    colClasses = "character") %>%

    ## Rename columns that are equivalent (content and format) to columns in the standard format
    dplyr::rename(Plot = Popn,
                  LocationID = Box) %>%

    ## 2) Add population ID and reformat colums with equivalent content but different format
    dplyr::mutate(PopID = "PFN",
                  BreedingSeason = as.numeric(Year),
                  Species = dplyr::case_when(Species == "BLUTI" ~ "CYACAE",
                                            Species == "PIEFL" ~ "FICHYP",
                                            Species == "GRETI" ~ "PARMAJ",
                                            Species == "REDST" ~ "NA", # Missing, 160 observations --> Should be added as a new code!
                                            Species == "MARTI" ~ "POEPAL",
                                            Species == "NUTHA" ~ "SITEUR",
                                            Species == "COATI" ~ "PERATE",
                                            Species == "WREN" ~ "NA", # Missing, 1 observation only
                                            Species == "TREEC" ~ "NA"), # Missing, 1 observation only
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
                  YoungDate = as.Date(YoungDate, format = "%d/%m/%Y")) %>%
  # NOTE: YoungDate is not part of the standard format, but we need it here to determine chick age, and therefore whether or not we can use the chick measurements) 
    
      # 4) Add columns that are based on calculations
      dplyr::arrange(BreedingSeason, FemaleID, LayDate)
      # --> This sorting is required for the function "calc_clutchtype" to work
    
    # 4.1) Calculate means and observation numbers for fledgling weight/tarsus length
    Weight_data <- Brood_data[,paste("Weight.Y", c(1:11), sep = "")]
    Tarsus_data <- Brood_data[,paste("Tarsus.Y", c(1:11), sep = "")]
  
    Weight_data_sum <- do.call("rbind", sapply(1:nrow(Weight_data), FUN = function(x)  mean_countObs(Weight_data[x,]), simplify = FALSE))
    Tarsus_data_sum <- do.call("rbind", sapply(1:nrow(Tarsus_data), FUN = function(x)  mean_countObs(Tarsus_data[x,]), simplify = FALSE))
  
    # 4.2) Add calculated columns
    Brood_data <- Brood_data %>%
      dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = Brood_data, na.rm = FALSE),
                    NumberEggs = as.integer(Hatch) + as.integer(UnHatch),
                    AvgChickMass = Weight_data_sum[,1],
                    NumberChicksMass = as.integer(Weight_data_sum[,2]),
                    AvgTarsus = Tarsus_data_sum[,1],
                    NumberChicksTarsus = as.integer(Tarsus_data_sum[,2]),
                    ChickAge = as.integer(YoungDate - HatchDate))

  # 4.3) Remove chick measurement data when chick age was outside the 14-16 day window
  Brood_data$AvgChickMass[which(!between(Brood_data$ChickAge, 14, 16))] <- NA_real_
  Brood_data$NumberChicksMass[which(!between(Brood_data$ChickAge, 14, 16))] <- NA_integer_
  Brood_data$AvgTarsus[which(!between(Brood_data$ChickAge, 14, 16))] <- NA_real_
  Brood_data$NumberChicksTarsus[which(!between(Brood_data$ChickAge, 14, 16))] <- NA_integer_

  # 5) Add columns without data
  Brood_data <- Brood_data %>%
      dplyr::mutate(LayDateError = NA_integer_,
                    ClutchSizeError = NA_integer_,
                    HatchDateError = NA_integer_,
                    BroodSizeError = NA_integer_,
                    FledgeDate = as.Date(NA, format = "%d/%m/%Y"),
                    FledgeDateError = NA_integer_,
                    NumberFledgedError = NA_integer_,
                    AvgEggMass =  NA_real_,
                    OriginalTarsusMethod = NA_character_,
                    ExperimentID = NA_character_) %>%
    
      # 6) Select and arrange columns for output
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
  return(Brood_data)

}

#' Create capture data table for EastDartmoor.
#'
#' @param db Location of primary data from EastDartmoor.
#'
#' @return A data frame with Capture data

create_capture_PFN <- function(db){

  Capture_data <- read.csv(file = paste0(db, "/PFN_PrimaryData.csv"), na.strings = c("", "?"),
                           colClasses = "character")
  
  # 1) Male capture data
  Male_Capture_data <- data.frame(IndvID = Capture_data$MaleID, 
                                  BroodID = Capture_data$BroodID, # Will be removed later
                                  Species = dplyr::case_when(Capture_data$Species == "BLUTI" ~ "CYACAE",
                                                             Capture_data$Species == "PIEFL" ~ "FICHYP",
                                                             Capture_data$Species == "GRETI" ~ "PARMAJ",
                                                             Capture_data$Species == "REDST" ~ "NA",
                                                             Capture_data$Species == "MARTI" ~ "POEPAL",
                                                             Capture_data$Species == "NUTHA" ~ "SITEUR",
                                                             Capture_data$Species == "COATI" ~ "PERATE",
                                                             Capture_data$Species == "WREN" ~ "NA",
                                                             Capture_data$Species == "TREEC" ~ "NA"), 
                                  BreedingSeason = as.numeric(Capture_data$Year),
                                  CaptureDate = as.Date(Capture_data$MaleDate, format = "%d/%m/%Y"),
                                  CaptureTime = NA,
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
                                  Age_observed = 2L,
                                  Age_calculated = NA_integer_, # Will be added later
                                  #ChickAge = NA_integer_, # Will be added later
                                  Sex = "M", # Will be removed later
                                  stringsAsFactors = FALSE) 
  
  # 2) Female capture data
  Female_Capture_data <- data.frame(IndvID = Capture_data$FemaleID, 
                                    BroodID = Capture_data$BroodID, # Will be removed later
                                    Species = dplyr::case_when(Capture_data$Species == "BLUTI" ~ "CYACAE",
                                                               Capture_data$Species == "PIEFL" ~ "FICHYP",
                                                               Capture_data$Species == "GRETI" ~ "PARMAJ",
                                                               Capture_data$Species == "REDST" ~ "NA",
                                                               Capture_data$Species == "MARTI" ~ "POEPAL",
                                                               Capture_data$Species == "NUTHA" ~ "SITEUR",
                                                               Capture_data$Species == "COATI" ~ "PERATE",
                                                               Capture_data$Species == "WREN" ~ "NA",
                                                               Capture_data$Species == "TREEC" ~ "NA"), 
                                    BreedingSeason = as.numeric(Capture_data$Year),
                                    CaptureDate = as.Date(Capture_data$FemaleDate, format = "%d/%m/%Y"),
                                    CaptureTime = NA,
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
                                    Age_observed = 2L,
                                    Age_calculated = NA_integer_, # Will be added later
                                    #ChickAge = NA_integer_, # Will be added later
                                    Sex = "F", # Will be removed later
                                    stringsAsFactors = FALSE)
  
  # 3) Chick capture data
  Chick_Capture_list <- list()
  for(i in 1:nrow(Capture_data)){
    Chick_Capture_list[[i]] <- data.frame(IndvID = unname(t(Capture_data[i, paste0("Young", c(1:11))])), 
                                          BroodID = Capture_data$BroodID[i], # Will be removed later
                                          Species = dplyr::case_when(Capture_data$Species[i] == "BLUTI" ~ "CYACAE",
                                                                     Capture_data$Species[i] == "PIEFL" ~ "FICHYP",
                                                                     Capture_data$Species[i] == "GRETI" ~ "PARMAJ",
                                                                     Capture_data$Species[i] == "REDST" ~ "NA",
                                                                     Capture_data$Species[i] == "MARTI" ~ "POEPAL",
                                                                     Capture_data$Species[i] == "NUTHA" ~ "SITEUR",
                                                                     Capture_data$Species[i] == "COATI" ~ "PERATE",
                                                                     Capture_data$Species[i] == "WREN" ~ "NA",
                                                                     Capture_data$Species[i] == "TREEC" ~ "NA"), 
                                          BreedingSeason = as.numeric(Capture_data$Year[i]),
                                          CaptureDate = as.Date(Capture_data$FemaleDate[i], format = "%d/%m/%Y"),
                                          CaptureTime = NA,
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
  
  Capture_data <- dplyr::bind_rows(Chick_Capture_data, Male_Capture_data, Female_Capture_data)
  
  Capture_data <- subset(Capture_data, !is.na(IndvID))
  
  
  # 5) Calculate age at capture 

  # Sort chronologically within individual
  Capture_data <- Capture_data %>%
    dplyr::arrange(IndvID, CaptureDate, CaptureTime) %>%
    #Calculate age at each capture based on first capture
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason)
    
  # 6) Return data
  return(Capture_data)

}

#' Create individual data table for EastDartmoor.
#'
#' @param db Location of individual data from EastDartmoor.
#'
#' @return A data frame with Individual data

create_individual_PFN <- function(Capture_data){
  
  #Take capture data and determine summary data for each individual
  Indv_data <- Capture_data %>%
    dplyr::filter(!is.na(IndvID)) %>%
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
  
}

#' Create location data table for EastDartmoor.
#'
#' @param db Location of location data from EastDartmoor.
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
                  StartSeason = NA_real_,
                  EndSeason = NA_real_,
                  Habitat = NA_real_)

  return(Location_data)

}



#---------------------------------------
# NOTES:
# Automatically add template for function documentation: cmd+alt+shift+R ("Insert Roxygen Skeleton")

