context("Run data quality check on Glasgow, Scotland, pipeline output")

test_that("GLA outputs all files...", {

  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("CAS", "GAR", "KEL", "SAL", "SCE") %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "CAS", "GAR", "KEL", "SAL", "SCE")

  #Individual ACJ2064 - female
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Sex_calculated, "F") # Should be female
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Species, "CYACAE") # Should be a blue tit
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingSeason, 2020) # Should be 2020
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingAge, "adult") # Should be an adult

  #Individual AXB1234 - male
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$Sex_calculated, "M") # Should be male
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$Species, "CYACAE") # Should be a blue tit
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$RingSeason, 2020) # Should be 2020
  expect_equal(subset(GLA_data, IndvID == "AXB1234")$RingAge, "adult") # Should be an adult

  #Individual TX11924 - uncertain sex and species
  expect_equal(subset(GLA_data, IndvID == "TX11924")$Sex_calculated, "C") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "TX11924")$Species, "CCCCCC") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "TX11924")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "TX11924")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "TX11924")$RingSeason, 2015) # Should be 2015 (seen in multiple years)
  expect_equal(subset(GLA_data, IndvID == "TX11924")$RingAge, "adult") # Should be an adult

  #Individual AFE3038 - uncertain sex
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$Sex_calculated, "C") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$Species, "CYACAE") # Should be conflicted
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$BroodIDLaid, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$BroodIDFledged, NA_character_) # Should be NA
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$RingSeason, 2019) # Should be 2015 (seen in two years)
  expect_equal(subset(GLA_data, IndvID == "AFE3038")$RingAge, "adult") # Should be an adult

})

test_that("Brood_data returns an expected outcome...", {






})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "CAS", "GAR", "KEL", "SAL", "SCE") %>%
    arrange(.data$IndvID, .data$BreedingSeason)

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2) up to and including 2018
  expect_equal(nrow(subset(GLA_data, IndvID == "S034047"  & BreedingSeason <= 2018)), 2) # Two records
  #Test the first capture of the female
  expect_equal(subset(GLA_data, IndvID == "S034047")$Sex_observed[1], "F") # Female
  expect_equal(subset(GLA_data, IndvID == "S034047")$LocationID[1], "65") # Nest 65
  expect_equal(min(subset(GLA_data, IndvID == "S034047")$CaptureDate, na.rm = TRUE), as.Date("2017-04-23")) # First caught 2017-04-23

  #Test 2: Male caught as adult
  #Test the female has the correct number of capture records (2) up to and including 2020
  expect_equal(nrow(subset(GLA_data, IndvID == "AFE3005")), 2) # Two records
  #Test the second capture of the male
  expect_equal(subset(GLA_data, IndvID == "AFE3005")$Sex_observed[2], "M") # Male
  expect_equal(subset(GLA_data, IndvID == "AFE3005")$LocationID[2], "545") # Nest 65
  expect_equal(min(subset(GLA_data, IndvID == "AFE3005")$CaptureDate, na.rm = TRUE), as.Date("2020-04-30"))

  #Test 2: Female caught as adult and at three different nests up to and including 2020
  #Test the female has the correct number of capture records (3) AXB1206
  expect_equal(nrow(subset(GLA_data, IndvID == "AXB1206")), 3) # Two records
  #Test the second capture of the male
  expect_equal(subset(GLA_data, IndvID == "AXB1206")$Sex_observed[1], "F") # Female
  expect_equal(subset(GLA_data, IndvID == "AXB1206")$LocationID[2], "543") # Nest 543
  expect_equal(min(subset(GLA_data, IndvID == "AXB1206")$CaptureDate, na.rm = TRUE), as.Date("2018-05-08"))

})

test_that("Location_data returns an expected outcome...", {




})

