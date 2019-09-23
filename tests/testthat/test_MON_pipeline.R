context("Run data quality check on Montpellier pipeline output")

test_that("MON outputs all files...", {

  expect_true(all(c("COR", "ROU") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("COR", "ROU") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("COR", "ROU") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("COR", "ROU") %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("COR", "ROU"))

  #Test 1: Brood where clutch type = first
  #BroodID 1984_pir_35_02_05 should be CYACAE
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$Species, "CYACAE")
  #BroodID 1984_pir_35_02_05 should have clutch type calculated 'first'
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$ClutchType_calculated, "first")
  #Check laying date is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$LayingDate, as.Date("1984-05-02"))
  #Check clutch size, brood size, and number fledged is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$BroodSize, 4)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$NumberFledged, 0)
  #AvgChickMass and AvgTarsus should be NA, these have not been included yet
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_02_05")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  #BroodID 1980_pir_36_30_05 should be CYACAE
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$Species, "CYACAE")
  #BroodID 1980_pir_36_30_05 should have clutch type calculated 'first'
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$ClutchType_calculated, "replacement")
  #Check laying date is as expected
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$LayingDate, as.Date("1980-05-30"))
  #Check clutch size, brood size, and number fledged is as expected
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$BroodSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$NumberFledged, 5)
  #AvgChickMass and AvgTarsus should be NA, these have not been included yet
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_30_05")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  #BroodID 1984_pir_47_10_06 should be CYACAE
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$Species, "CYACAE")
  #BroodID 1984_pir_47_10_06 should have clutch type calculated 'first'
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$ClutchType_calculated, "replacement")
  #Check laying date is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$LayingDate, as.Date("1984-06-10"))
  #Check clutch size, brood size, and number fledged is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$ClutchSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$BroodSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$NumberFledged, 7)
  #AvgChickMass and AvgTarsus should be NA, these have not been included yet
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  #BroodID 1984_pir_47_10_06 should be CYACAE
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$Species, "CYACAE")
  #BroodID 1984_pir_47_10_06 should have clutch type calculated 'first'
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$ClutchType_calculated, "replacement")
  #Check laying date is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$LayingDate, as.Date("1984-06-10"))
  #Check clutch size, brood size, and number fledged is as expected
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$ClutchSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$BroodSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$NumberFledged, 7)
  #AvgChickMass and AvgTarsus should be NA, these have not been included yet
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_10_06")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("COR", "ROU"))

  #Test 1: Male caught first as adult
  #Individual C044309 should be listed as a male coal tit
  expect_equal(subset(MON_data, IndvID == "2221101")$Sex, "M")
  expect_equal(subset(MON_data, IndvID == "2221101")$Species, "PERATE")
  #She should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDFledged, NA_character_)
  #Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(MON_data, IndvID == "2221101")$RingSeason, 1991)
  expect_equal(subset(MON_data, IndvID == "2221101")$RingAge, "adult")

  #Test 2: Female caught first as adult
  #Individual 2546616 should be listed as a female blue tit
  expect_equal(subset(MON_data, IndvID == "2546616")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "2546616")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDFledged, NA_character_)
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "2546616")$RingSeason, 1981)
  expect_equal(subset(MON_data, IndvID == "2546616")$RingAge, "adult")

  #Test 3: Caught as chick
  #Individual 8189538 should be listed as a female great tit
  expect_equal(subset(MON_data, IndvID == "8189538")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "8189538")$Species, "PARMAJ")
  #Should have BroodID Laid and Fledged of XXXX (NOT YET INCLUDED, this should return an error)
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDLaid, "XXX")
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDFledged, "XXX")
  #Ring season should be 2017 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "8189538")$RingSeason, 2017)
  expect_equal(subset(MON_data, IndvID == "8189538")$RingAge, "chick")

})
