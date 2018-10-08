#' Construct standard summary for Hoge Veluwe population
#'
#' @param db Location of database
#'
#' @return
#' @export
#'
#' @examples
format_HOG <- function(db, Species = c(14620, 14640), tables = c("Brood", "Indv", "Capture"), path = "."){

  start_time <- Sys.time()

  ###N.B. THIS SEEMS TO REQUIRE R 32bit, it returns errors in 64bit
  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  #Location info
  #Location_data <- tbl(connection, "dbo_tbl_Location")
  #Area_data     <- tbl(connection, "dbo_tbl_Area")

  #Subset only adults of designated species
  #Adult_data  <- tbl(connection, "Adult_info") %>%
  #  filter(BroodID %in% Species)

  if("Brood" %in% tables){

    print("Generating Brood_data table...")

    #Subset only broods of designated species
    Brood_data  <- tbl(connection, "dbo_tbl_Brood") %>%
      filter(BroodSpecies %in% Species) %>%
      #Create a new column that has the 2 letter ID for species
      mutate(Species = ifelse(BroodSpecies == 14620, "BT", "GT")) %>%
      #Extract basic info that we want:
      # - BroodYear
      # - BroodSpecies
      # - BroodLocationID (will be translated to an area name)
      # - RingNumberFemale
      # - RingNumberMale
      # - LayDate
      # - ClutchSize
      # - HatchDate
      # - NumberHatched
      # - FledgeDate
      # - NumberFledged
      select(BroodYear, Species, BroodLocationID, RingNumberFemale, RingNumberMale, LayDate, ClutchSize, HatchDate, NumberHatched, FledgeDate, NumberFledged) %>%
      #Sort by species and date
      arrange(BroodSpecies, BroodYear) %>%
      write.csv(x = ., file = paste0(path, "\\Brood_data.csv"), row.names = F)

  }

  if("Indv" %in% tables){

    print("Generating Indv_data table...")

    Sex_data <- tbl(connection, "dbo_tl_Sexe") %>%
      #Just select the Sex ID and its description
      select(Sexe = ID, Sex = Description)

    #Subset only broods of designated species
    Indv_data   <- tbl(connection, "dbo_tbl_Individual") %>%
      filter(SpeciesID %in% Species) %>%
      #Create a new column that has the 2 letter ID for species
      mutate(Species = ifelse(SpeciesID == 14620, "BT", "GT")) %>%
      #Remove basic info that we want:
      # - BroodID
      # - Species
      # - Sex
      # - RingYear
      # - RingAge
      # - RingNumber
      select(BroodID, Species, Sexe, RingYear, RingAge, RingNumber) %>%
      #Add in sex description
      left_join(Sex_data, by = "Sexe") %>%
      #Remove old sex info
      select(-Sexe) %>%
      #Sort by species and age
      arrange(Species, RingYear) %>%
      write.csv(x = ., file = paste0(path, "\\Indv_data.csv"), row.names = F)

  }

  if("Capture" %in% tables){

    print("Generating Capture_data table...")

    Age_data <- tbl(connection, "dbo_tl_Age") %>%
      select(Age_join = ID, Age = Description)

    #Filter just GT and BT
    Capture_data <- tbl(connection, "dbo_vw_MI_CaptureCaptureData") %>%
      filter(SpeciesID %in% Species) %>%
      #Remove only the basic info we need
      # -CaptureDate
      # -CaptureLocation
      # -IndividualNumber
      # -SpeciesID
      # -Weight
      # -Tarsus
      # -Wing_Length
      # -Age
      select(CaptureDate, CaptureLocationID, IndividualNumber, SpeciesID, Weight, Tarsus, Wing_Length, Age_join = Age) %>%
      left_join(Age_data, by = "Age_join") %>%
      select(-Age_join) %>%
      write.csv(x = ., file = paste0(path, "\\Capture_data.csv"), row.names = F)

  }

  time <- difftime(Sys.time(), start_time, units = "sec")

  dbDisconnect(connection)

  print(paste0("All tables generated in ", round(time, 2), " seconds"))

}
