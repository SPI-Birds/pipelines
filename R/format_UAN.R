format_UAN <- function(data_folder = choose.dir(), path = "."){

  start_time <- Sys.time()

  all_files <- list.files(path = data_folder, pattern = ".csv", full.names = TRUE)

  BOX_info <- read.csv(all_files[grepl("BOX", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  BROOD_info <- read.csv(all_files[grepl("BR", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  INDV_info <- read.csv(all_files[grepl("IND", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  PLOT_info <- read.csv(all_files[grepl("PL", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)
  CAPTURE_info <- read.csv(all_files[grepl("VG", all_files)], header = T, sep = ",", stringsAsFactors = FALSE)

  Brood_table <- BROOD_info %>%
    transmute(Brood_year = year,
           Species = ifelse(SOORT == "pm", "GT", ifelse(SOORT == "pc", "BT", NA)),
           NestboxID = PL, BroodID = NN,
           FemaleID = RW, MaleID = RM,
           ClutchType = ifelse(TY == 1, "First", ifelse(TY > 1, "Second/replacement", NA)),
           LayingDate = lubridate::dmy(LD),
           ClutchSize = AE, NumberHatched = NP, NumberFledged = PU) %>%
    write.csv(x = ., file = paste0(path, "\\Brood_data.csv"), row.names = F)

  Catch_table <- CAPTURE_info %>%
    tibble::rownames_to_column() %>%
    left_join(pmap_df(.l = list(.$TA, .$TANEW, .$rowname), .f = function(old_tarsus, new_tarsus, rowname){

      data_frame(rowname = rowname, Tarsus = ifelse(is.na(new_tarsus), old_tarsus, new_tarsus),
                 Method = ifelse(!is.na(new_tarsus), "Svennson alternative", ifelse(!is.na(old_tarsus), "Svennson standard", NA)))

    }), by = "rowname") %>%
    transmute(Date = lubridate::dmy(VD),
              Type = ifelse(VW %in% c("P", "PP"), "Nestling", "Adult"),
              IndvID = RN, Location = GB,
              Tarsus = Tarsus, Tarsus_method = Method,
              Mass = GEW, Wing_Length = VLL) %>%
    write.csv(x = ., file = paste0(path, "\\Capture_data.csv"), row.names = F)

  Sex_table <- data_frame(sex = c(1, 2, 3, 4, 5),
                          Sex = c("male", "female", "male?", "female?", "sex unknown"))

  Indv_table <- INDV_info %>%
    left_join(Sex_table, by = "sex") %>%
    transmute(Species = ifelse(soort == "pm", "GT", ifelse(soort == "pc", "BT", NA)),
           IndvID = rn, RingDate = lubridate::dmy(klr1date),
           RingAge = lubridate::year(RingDate) - gbj,
           Status = ifelse(mode == "P", "Resident", "Immigrant"), Sex = Sex) %>%
    write.csv(x = ., file = paste0(path, "\\Indv_data.csv"), row.names = F)

  time <- difftime(Sys.time(), start_time, units = "sec")

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
