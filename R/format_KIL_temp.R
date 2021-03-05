#### Load brood data after 1995
kil_data_95 <-
  readxl::read_excel(path = paste0(db, "/Data_Pm_1995-2001_ADULTS.xlsx"), sheet = 1) %>%
  janitor::clean_names(case = "upper_camel") %>%
  janitor::remove_empty(which = "rows") %>%
  #### Convert to corresponding format and rename
  dplyr::mutate(Species = species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                PopID   = "KIL",
                BreedingSeason = as.integer(.data$Year),
                NestboxID = tolower(.data$NestId),
                FemaleID = as.character(.data$FemaleRingNo),
                MaleID = as.character(.data$MaleRingNo),
                BreedingAttempt = case_when(.data$Brood == "First" ~ 1L,
                                            .data$Brood == "Second" ~ 2L),
                BroodID = paste(.data$Year, .data$NestboxID, .data$BreedingAttempt,
                                sep = "_")) %>%
  #### Reorder columns
  dplyr::select(.data$BreedingSeason,
                .data$Species,
                .data$PopID,
                everything(),
                -.data$NestId,
                -.data$FemaleRingNo,
                -.data$MaleRingNo,
                -.data$Year)

### Prepare info about individuals/captures from data from 1995
adults_data_95 <-
  kil_data_95 %>%
  dplyr::select(.data$Species, .data$PopID, .data$BreedingSeason,
                .data$FemaleID, .data$MaleID, .data$BroodID, .data$NestboxID,
                .data$FemaleAgeEuringCode, .data$FemaleCatchDate1May1St,
                .data$MaleAgeEuringCode, .data$MaleCatchDate1May1St,
                .data$AdultFemaleTarsusMm, .data$AdultMaleTarsusMm,
                .data$AdultFemaleMassG, .data$AdultMaleMassG,
                .data$AdultFemaleWingMm, .data$AdultMaleWingMm) %>%
  tidyr::pivot_longer(cols = c(.data$FemaleID, .data$MaleID,),
                      names_to = "Sex_observed",
                      values_to = "IndvID") %>%
  dplyr::mutate(Sex_observed = substr(.data$Sex_observed, start = 1, stop = 1),
                Tarsus = case_when(.data$Sex_observed == "F" ~ AdultFemaleTarsusMm,
                                   .data$Sex_observed == "M" ~ AdultMaleTarsusMm),
                Mass = case_when(.data$Sex_observed == "F" ~ AdultFemaleMassG,
                                 .data$Sex_observed == "M" ~ AdultMaleMassG),
                WingLength = case_when(.data$Sex_observed == "F" ~ AdultFemaleWingMm,
                                       .data$Sex_observed == "M" ~ AdultMaleWingMm),
                CatchDate1May1St = case_when(.data$Sex_observed == "F" ~ FemaleCatchDate1May1St,
                                             .data$Sex_observed == "M" ~ MaleCatchDate1May1St),
                CaptureDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                      format = "%Y-%m-%d") + .data$CatchDate1May1St - 1,
                CaptureTime = NA_character_,
                AgeEuringCode = case_when(.data$Sex_observed == "F" ~ as.integer(FemaleAgeEuringCode),
                                          .data$Sex_observed == "M" ~ as.integer(MaleAgeEuringCode))) %>%
  #### Remove records where the partner is not known
  dplyr::filter(!is.na(.data$IndvID)) %>%
  dplyr::select(-.data$AdultFemaleTarsusMm,
                -.data$AdultMaleTarsusMm,
                -.data$AdultFemaleMassG,
                -.data$AdultMaleMassG,
                -.data$AdultFemaleWingMm,
                -.data$AdultMaleWingMm,
                -.data$FemaleCatchDate1May1St,
                -.data$MaleCatchDate1May1St,
                -.data$FemaleAgeEuringCode,
                -.data$MaleAgeEuringCode)


#### Load data after 1995 chicks
chicks_data_95 <-
  readxl::read_excel(path = paste0(db, "/Data_nestlings_pm1995-2001.xls"),
                     sheet = 1,
                     col_types = c("numeric", "text", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "text", "numeric")) %>%
  janitor::clean_names(case = "upper_camel") %>%
  janitor::remove_empty(which = "rows") %>%
  #### ASK DATA OWNER: Now remove temporarily this record
  dplyr::filter(.data$NestlingRingNo != 0) %>%
  #### ASK DATA OWNER TO CHANGE IN RAW DATA
  dplyr::mutate(NestlingTarsus = str_replace(.data$NestlingTarsus,
                                             pattern = ",",
                                             replacement = ".")) %>%
  #### Convert to corresponding format and rename
  dplyr::mutate(Species = species_codes[which(species_codes$SpeciesID == 14640), ]$Species,
                PopID   = "KIL",
                BreedingSeason = as.integer(.data$Year),
                NestboxID = tolower(.data$NestId),
                BroodID = paste(.data$Year, .data$NestboxID, .data$BreedingAttempt,
                                sep = "_"),
                IndvID = as.character(.data$NestlingRingNo),
                CaptureDate = as.Date(paste(.data$BreedingSeason, "05-01", sep = "-"),
                                      format = "%Y-%m-%d") + .data$RingDate1May1St - 1,
                CaptureTime = if_else(!is.na(.data$RingTime),
                                      (str_replace(sprintf("%05.2f", RingTime),
                                                   pattern = "\\.",
                                                   replacement = ":")),
                                      NA_character_),
                AgeEuringCode = 1L,
                Mass = as.numeric(.data$NestlingMass),
                Tarsus = as.numeric(.data$NestlingTarsus)) %>%
  dplyr::select(-.data$Year,
                -.data$NestId,
                -.data$NestlingRingNo,
                -.data$RingDate,
                -.data$RingDate1May1St,
                -.data$RingTime)
