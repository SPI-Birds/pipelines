testthat::skip_if(!exists("data_path"))

pipeline_output <- format_GLA(db = paste0(data_path, "/GLA_Glasgow_Scotland"))

test_that("GLA outputs all files...", {

  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  #Individual ACJ2064 - female, ringed as adult
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Sex_calculated, "F") # Should be female
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Species, "CYACAE") # Should be a blue tit
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingSeason, 2020) # Should be 2020
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingAge, "adult") # Should be an adult

  #Individual AXB1234 - male, ringed as chick
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$Sex_calculated, "M") # Should be male
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$Species, "CYACAE") # Should be a blue tit
  expect_equal(!is.na(subset(GLA_data, IndvID == "AXB1234")$BroodIDLaid), TRUE) # Shouldhave a BroodIDLaid
  expect_equal(!is.na(subset(GLA_data, IndvID == "AXB1234")$BroodIDFledged), TRUE) # Should have a BroodIDFledged
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$RingSeason, 2018) # Should be 2018
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$RingAge, "chick") # Should be a chick

  #Individual TX11924 - uncertain sex and species
  expect_equal(subset(GLA_data, IndvID == "TX11924")$Sex_calculated, "C") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "TX11924")$Species, "CCCCCC") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "TX11924")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "TX11924")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "TX11924")$RingSeason, 2015) # Should be 2015 (seen in multiple years)
  expect_equal(subset(GLA_data, IndvID == "TX11924")$RingAge, "adult") # Should be an adult

  #Individual AFE3038 - uncertain sex
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$Sex_calculated, "C") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$Species, "CYACAE") # Should be a blue tit
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$RingSeason, 2019) # Should be 2019 (seen in two years)
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$RingAge, "adult") # Should be an adult

  ## Individual observed in multiple populations
  expect_equal(nrow(subset(GLA_data, IndvID == "ACJ2041")), 2) # 2 records
  expect_equal(subset(GLA_data, IndvID == "ACJ2041")$PopID, c("SCE", "SAL")) # SCE and SAL pop codes

  ## Individual observed in multiple populations and ringed as chick, but part of a cross fostering experiment, so broodIDlaid is NA, but broodIDfledged is known
  expect_equal(nrow(subset(GLA_data, IndvID == "AFE3840")), 2) # SCE and SAL pop codes
  expect_equal(dplyr::n_distinct(subset(GLA_data, IndvID == "AFE3840")$BroodIDLaid), 1) # 1 BroodIDLaid value
  expect_equal(is.na(unique(subset(GLA_data, IndvID == "AFE3840")$BroodIDLaid)), TRUE) # BroodIDLaid is NA
  expect_equal(!is.na(unique(subset(GLA_data, IndvID == "AFE3840")$BroodIDFledged)), TRUE) # BroodIDFledged is not NA

  ## Case where individual hatched in a replacement clutch
  expect_equal(dplyr::n_distinct(subset(GLA_data, IndvID == "AFE3178")$BroodIDLaid), 1) # 1 BroodIDLaid value

})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  ## General brood data
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$ClutchSize_observed, 10)
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$BroodSize_observed, 9)
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$NumberFledged_observed, 9)
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_observed, as.Date("2014-04-25"))
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_min, lubridate::NA_Date_)
  expect_equal(subset(GLA_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_max, lubridate::NA_Date_)

  ## Case where there were multiple clutches laid at the same location
  expect_equal(nrow(subset(GLA_data, BreedingSeason == "2019"
                           & PopID == "SAL"
                           & LocationID == "249")), 2)

  ## Brood where clutch type observed = replacement
  expect_equal(subset(GLA_data, BreedingSeason == "2015"
                      & PopID == "SAL"
                      & LocationID == "235" &
                        is.na(LayDate_observed))$ClutchType_observed, "replacement")

  ## Brood where chick weight, but not tarsus is measured
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$AvgChickMass, 10.1)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$NumberChicksMass, 8)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$AvgTarsus, NA_real_)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$NumberChicksTarsus, NA_integer_)

  ## Case where species is ambiguous, but the species information from the ringing data  was used to assign species for the brood
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2020"
                      & PopID == "SAL"
                      & LocationID == "204")$Species, "PARMAJ")

  ## Case where both FemaleID and MaleID are known
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2020"
                      & PopID == "KEL"
                      & LocationID == "534")$FemaleID, "ACJ2320")
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2020"
                      & PopID == "KEL"
                      & LocationID == "534")$MaleID, "ACJ2305")

  ## Case where female had two nests in the same year
  expect_equal(nrow(subset(GLA_data, FemaleID == "TX11502" & BreedingSeason == 2017)), 2)

  ## Cases where dates are in different formats for laying columns in primary data
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "710")$LayDate_observed, as.Date("2014-05-05"))
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "710")$LayDate_max, lubridate::NA_Date_)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "729")$LayDate_max, lubridate::NA_Date_)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "724")$LayDate_min, lubridate::NA_Date_)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2015"
                      & PopID == "GAR"
                      & LocationID == "724")$LayDate_min, lubridate::NA_Date_)
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2017"
                      & PopID == "SAL"
                      & LocationID == "227")$LayDate_min, lubridate::NA_Date_)

  ## Check experiment groups
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2016"
                      & PopID == "KEL"
                      & LocationID == "548")$ExperimentID, "PARENTAGE")

  expect_equal(subset(GLA_data,
                      BreedingSeason == "2016"
                      & PopID == "KEL"
                      & LocationID == "550")$ExperimentID, "OTHER")

  expect_equal(subset(GLA_data,
                      BreedingSeason == "2017"
                      & PopID == "SCE"
                      & LocationID == "41")$ExperimentID, "OTHER")

  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "SCE"
                      & LocationID == "175")$ExperimentID, "OTHER")

  expect_equal(subset(GLA_data,
                      BreedingSeason == "2019"
                      & PopID == "SAL"
                      & LocationID == "229")$ExperimentID, "COHORT")

  ## Check incorrect IDs
  expect_equal(subset(GLA_data,
                      BreedingSeason == "2018"
                      & PopID == "SCE"
                      & LocationID == "51")$MaleID, NA_character_)




})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  ## Case where chick found dead after ringed
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$ReleaseAlive, FALSE) # Should be False
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$ReleasePopID, NA_character_) # Should be NA (since dead)
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$Species, "CYACAE") # Should be blue tit
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$Mass, 9.42) # Should be 9.42
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$BreedingSeason, 2020) # Should be 2019
  expect_equal(subset(GLA_data, IndvID == "ACJ2314")$Age_observed, 1) # Should be 1

  ## Case where individual recorded at different locations in nest and ringing data
  expect_equal(nrow(subset(GLA_data, IndvID == "S034047" & BreedingSeason <= 2018)), 3) # Three records
  expect_equal(subset(GLA_data, IndvID == "S034047" &
                        BreedingSeason == 2017)$Sex_observed, "F") # Female
  expect_equal(subset(GLA_data, IndvID == "S034047" &
                        CaptureDate == as.Date("2018-05-01"))$LocationID, "65") # LocationID 65

  ## Check that all IndvIDs conform to expected format
  expect_true(all(stringr::str_detect(subset(GLA_data)$IndvID, "^[[:digit:][:alpha:]]{7}$")))

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  ## Nestbox 728 in GAR
  expect_equal(subset(GLA_data, LocationID == "728" &
                        PopID == "GAR")$LocationType, "NB") ## Nestbox
  expect_equal(subset(GLA_data, LocationID == "728" &
                        PopID == "GAR")$HabitatType, "urban") ## Urban
  expect_equal(subset(GLA_data, LocationID == "728" &
                        PopID == "GAR")$StartSeason, 2015) ## 2015
  expect_equal(subset(GLA_data, LocationID == "728" &
                        PopID == "GAR")$EndSeason, NA_integer_) ## NA

  ## Same nestbox number at 3 populations
  #LocationType is as expected
  expect_equal(subset(GLA_data, LocationID == "10")$PopID, c("CAS", "KEL", "SCE"))


})

