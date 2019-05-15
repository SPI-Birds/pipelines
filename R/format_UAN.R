#' Construct standard summary for University of Antwerp populations
#'
#' @param data_folder Folder location where raw .csv files are stored.
#' @param path Location where output files will be saved.
#'
#' @return Generates 5 .csv files with data in a standard format.
#' @export
#' @import dplyr
#' @import purrr

format_UAN <- function(data_folder = choose.dir(), path = "."){

  start_time <- Sys.time()

  all_files <- list.files(path = data_folder, pattern = ".csv", full.names = TRUE)

  BOX_info <- read.csv(all_files[grepl("BOX", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  BROOD_info <- read.csv(all_files[grepl("BR", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  INDV_info <- read.csv(all_files[grepl("IND", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  PLOT_info <- read.csv(all_files[grepl("PL", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  CAPTURE_info <- read.csv(all_files[grepl("VG", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)

  ##############
  # BROOD DATA #
  ##############

  print("Compiling brood information...")

  #Create a table with brood information.

  Brood_data <- BROOD_info %>%
    #Link the plot codes to the plot code (GB)
    left_join(select(PLOT_info, PopID = SA, GB = gb), by = "GB") %>%
    #Convert all other columns into standard format
    transmute(BroodYear = year,
           Species = ifelse(SOORT == "pm", "GT", ifelse(SOORT == "pc", "BT", NA)),
           PopID = PopID,
           BroodID = NN, NestboxID = PL,
           FemaleID = RW, MaleID = RM,
           ClutchType = ifelse(TY == 1, "First", ifelse(TY > 1, "Second/replacement", NA)),
           LayingDate = lubridate::dmy(LD),
           ClutchSize = AE, HatchDate = NA, NumberHatched = NP, FledgeDate = NA, NumberFledged = PU)

  ################
  # CAPTURE DATA #
  ################

  print("Compiling capture information...")

  #Capture data includes all times an individual was captured (with measurements like mass, tarsus etc.).
  #This will include first capture as nestling (for residents)
  #This means there will be multiple records for a single individual.

  Capture_data <- CAPTURE_info %>%
    tibble::rownames_to_column() %>%
    #Add tarsus measurements and specify the measurement method used (Svennson standard/short and Svennson alternative/long).
    #When both measurments are available, Svennson's alternative is used.
    left_join(pmap_df(.l = list(.$TA, .$TANEW, .$rowname), .f = function(old_tarsus, new_tarsus, rowname){

      data_frame(rowname = rowname, Tarsus = ifelse(is.na(new_tarsus), old_tarsus, new_tarsus),
                 Method = ifelse(!is.na(new_tarsus), "Svennson alternative", ifelse(!is.na(old_tarsus), "Svennson standard", NA)))

    }), by = "rowname") %>%
    #There is no information on release location, so I assume it's the same as the capture location.
    transmute(CaptureDate = lubridate::dmy(VD),
              Species = ifelse(SOORT == "pm", "GT", ifelse(SOORT == "pc", "BT", NA)),
              Type = ifelse(VW %in% c("P", "PP"), "Nestling", "Adult"),
              IndvID = RN, CaptureLocation = SA, ReleaseLocation = SA,
              Mass = GEW, Tarsus = Tarsus, TarsusMethod = Method,
              WingLength = VLL)

  ###################
  # INDIVIDUAL DATA #
  ###################

  print("Compiling individual information...")

  #This is a summary of each individual and general lifetime information (e.g. sex, resident/immigrant)

  Sex_table <- data_frame(sex = c(1, 2, 3, 4, 5),
                          Sex = c("male", "female", "male?", "female?", "sex unknown"))

  #To determine which brood an individual is from, we subset the CAPTURE_info table to include only those records where an individual was caught as a nestling (P).
  #We then take the nest that this nestling was in when it was caught.
  Indv_broods <- CAPTURE_info %>%
    group_by(RN) %>%
    filter(!is.na(NN) & VW == "P") %>%
    summarise(BroodIDLaid = NN[1]) %>%
    rename(rn = RN)

  #I assume that the broodID laid and fledged are the same in this case.
  Indv_data <- INDV_info %>%
    left_join(Sex_table, by = "sex") %>%
    left_join(Indv_broods, by = "rn") %>%
    mutate(BroodIDFledged = NA,
           Species = ifelse(soort == "pm", "GT", ifelse(soort == "pc", "BT", NA)),
           IndvID = rn, RingDate = lubridate::dmy(klr1date), RingYear = lubridate::year(RingDate),
           RingAge = lubridate::year(RingDate) - gbj,
           Status = ifelse(mode == "P", "Resident", "Immigrant"), Sex = Sex) %>%
    select(BroodIDLaid, BroodIDFledged, Species, IndvID, RingYear, RingAge, Status, Sex)

  ################
  # NESTBOX DATA #
  ################

  print("Compiling nestbox information...")

  Nestbox_data <- BOX_info %>%
    transmute(NestboxID = GBPL, PopID = SA, Latitude = Y_deg, Longitude = X_deg, StartYear = YEARFIRST, EndYear = YEARLAST)

  ###################
  # POPULATION DATA #
  ###################

  print("Compiling population summary information...")

  Plot_species <- Brood_data %>%
    group_by(PopID) %>%
    filter(!is.na(Species)) %>%
    summarise(Species = paste(unique(Species), collapse = ","))

  Pop_data <- PLOT_info %>%
    filter(SA != "") %>%
    rename(PopID = SA) %>%
    group_by(PopID) %>%
    summarise(StartYear = min(FIRSTY), EndYear = max(LASTY)) %>%
    left_join(Plot_species, by = "PopID") %>%
    left_join(Nestbox_data %>%
                group_by(PopID) %>%
                summarise(TotalNestbox = length(unique(NestboxID))), by = "PopID") %>%
    mutate(PopName = c("Boshoek", "Peerdsbos"))

  print("Saving .csv files...")

  write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_UAN.csv"), row.names = F)

  write.csv(x = Indv_data, file = paste0(path, "\\Indv_data_UAN.csv"), row.names = F)

  write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_UAN.csv"), row.names = F)

  write.csv(x = Nestbox_data, file = paste0(path, "\\Nestbox_data_UAN.csv"), row.names = F)

  write.csv(x = Pop_data, file = paste0(path, "\\Summary_data_UAN.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
