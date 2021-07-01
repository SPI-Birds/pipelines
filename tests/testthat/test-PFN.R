testthat::skip_if(!exists("data_path"))

pipeline_output <- format_PFN(db = paste0(data_path, "/PFN_PiedFlyNet_UK"))

PFN_PopIDs <- c('DIN', 'EDM', 'KAT', 'NAG', 'OKE', 'TEI')

test_that("format_PFN outputs all tables...", {

  expect_true(all(PFN_PopIDs %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PFN_PopIDs %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PFN_PopIDs %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PFN_PopIDs %in% pipeline_output$Location_data$PopID))

})


# test_that("Tables contain all required columns in the correct order...", {
#
#   expect_true(all(colnames(pipeline_output$Brood_data) == column_names_v1.1$Brood))
#   expect_true(all(colnames(pipeline_output$Capture_data) == column_names_v1.1$Capture))
#   expect_true(all(colnames(pipeline_output$Individual_data) == column_names_v1.1$Individual))
#   expect_true(all(colnames(pipeline_output$Location_data) == column_names_v1.1$Location))
#
# })
# NOTE: I have disabled this for the moment. Right now, this test fails when run
# on the output of run_pipelines() containing ANY other populations not
# formatted according to standard protocol v1.1.


test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% PFN_PopIDs)

  #Test 1: Brood where clutch type = first (CltCd = 1 in raw)
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$ClutchType_calculated, "first") #
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$LayDate_observed, as.Date("2016-03-31") + 35)
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$ClutchSize_observed, 8L)
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$BroodSize_observed, 3L) # Info from IPMR
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$NumberFledged_observed, 3L)
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$AvgChickMass, mean(c(10.88, 10.81, 12.19)))
  expect_equal(subset(PFN_data, BroodID == "EDM-6219")$AvgTarsus, mean(c(16.4, 15.7, 17.3)))

  #Test 2: Brood where clutch type = replacement (because first is known to have failed, CltCd = 2 in raw)
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$ClutchType_observed, "replacement")
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$LayDate_observed, as.Date("2015-03-31") + 45)
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$ClutchSize_observed, 10L)
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$BroodSize_observed, 2L) # Info from IPMR
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$NumberFledged_observed, 2L)
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$AvgChickMass, mean(c(8.61, 9.8)))
  expect_equal(subset(PFN_data, BroodID == "EDM-6077")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$ClutchType_observed, "replacement")
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$LayDate_observed, as.Date("1994-03-31") + 74)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$ClutchSize_observed, 4L)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$BroodSize_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "EDM-2729")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second (CltCd = 3 in raw)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$ClutchType_observed, "second")
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$ClutchType_calculated, "second")
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$LayDate_observed, as.Date("2007-03-31") + 68)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$ClutchSize_observed, 4L)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$BroodSize_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "EDM-4653")$AvgTarsus, NA_real_)

  #Test 5: Brood where clutch type cannot be calculated (because female ID and LayDate are NA)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$LayDate_observed, as.Date(NA))
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$BroodSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "EDM-3952")$AvgTarsus, NA_real_)

  # Test 6: A brood from DIN
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$LayDate_observed, as.Date(NA))
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$BroodSize_observed, 7L) # Info from IPMR
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$NumberFledged_observed, NA_integer_)
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "DIN-2936")$AvgTarsus, NA_real_)

  # Test 7: A brood from KAT
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$Species, "PHOPHO")
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$LayDate_observed, as.Date("1978-03-31") + 74)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$HatchDate_observed, as.Date("1978-03-31") + 90)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$ClutchSize_observed, 6L)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$BroodSize_observed, 4L)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$NumberFledged_observed, 3L)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "KAT-155")$AvgTarsus, NA_real_)

  # Test 8: A brood from NAG
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$LayDate_observed, as.Date("2000-03-31") + 38)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$HatchDate_observed, as.Date("2000-03-31") + 58)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$BroodSize_observed, 6L)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$NumberFledged_observed, 6L)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "NAG-1737")$AvgTarsus, NA_real_)

  # Test 9: A brood from OKE
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$LayDate_observed, as.Date(NA))
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$HatchDate_observed, as.Date("2020-03-31") + 50)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$BroodSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$NumberFledged_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$AvgTarsus, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$MaleID, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "OKE-4599")$FemaleID, "Z113813")

  # Test 10: A brood from TEI
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$ClutchType_observed, "first")
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$LayDate_observed, as.Date("2002-03-31") + 40)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$HatchDate_observed, as.Date("2002-03-31") + 59)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$BroodSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$NumberFledged_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$AvgTarsus, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$MaleID, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "TEI-639")$FemaleID, "P865913")
})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% PFN_PopIDs)

  #Test 1: First as adult
  expect_equal(subset(PFN_data, IndvID == "B80960")$Sex_calculated, "F")
  expect_equal(subset(PFN_data, IndvID == "B80960")$Species, "FICHYP")
  expect_equal(subset(PFN_data, IndvID == "B80960")$PopID, "EDM")
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDLaid, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDFledged, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingSeason, 1955L)
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingAge, "adult")

  #Test 2: First as chick
  expect_equal(subset(PFN_data, IndvID == "K185273")$Sex_calculated, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "K185273")$Species, "FICHYP")
  expect_equal(subset(PFN_data, IndvID == "K185273")$PopID, "EDM")
  expect_equal(subset(PFN_data, IndvID == "K185273")$BroodIDLaid, "EDM-3176")
  expect_equal(subset(PFN_data, IndvID == "K185273")$BroodIDFledged, "EDM-3176")
  expect_equal(subset(PFN_data, IndvID == "K185273")$RingSeason, 1997)
  expect_equal(subset(PFN_data, IndvID == "K185273")$RingAge, "chick")

  #Test 3: First as a chick in one population, later captured adult in another
  expect_equal(subset(PFN_data, IndvID == "S846128")$Sex_calculated, c("F", "F"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$Species, c("FICHYP", "FICHYP"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$BroodIDLaid, c("TEI-1537", "TEI-1537"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$BroodIDFledged, c("TEI-1537","TEI-1537"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$RingSeason, c(2017, 2017))
  expect_equal(subset(PFN_data, IndvID == "S846128")$RingAge, c("chick", "chick"))

  #Test 4: Twice as adult in different populations
  expect_equal(subset(PFN_data, IndvID == "R723892")$Sex_calculated, c("F", "F"))
  expect_equal(subset(PFN_data, IndvID == "R723892")$Species, c("FICHYP", "FICHYP"))
  expect_equal(subset(PFN_data, IndvID == "R723892")$BroodIDLaid, rep(NA_character_, 2))
  expect_equal(subset(PFN_data, IndvID == "R723892")$BroodIDFledged, rep(NA_character_, 2))
  expect_equal(subset(PFN_data, IndvID == "R723892")$RingSeason, c(2006, 2006))
  expect_equal(subset(PFN_data, IndvID == "R723892")$RingAge, c("adult", "adult"))
})


test_that("Capture data returns an expected outcome...", {

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% PFN_PopIDs)

  #Test 1: Individual ringed as a chick (no chick measurement info)
  #Test that there are the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "Y369483")), 14)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[1], as.Date("2012-06-11"))
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[14], as.Date("2019-06-03"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[14], 16L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[14], 17L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Mass[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Mass[14], 12.2)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Tarsus[14], 17.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$WingLength[14], NA_real_)

  #Test 2: Individual ringed as a chick (chick measurement info from brood data)
  #Test that there are the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "Z286392")), 3)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "Z286392")$CaptureDate[1], as.Date("2015-05-29"))
  expect_equal(subset(PFN_data, IndvID == "Z286392")$CaptureDate[3], as.Date("2017-05-28"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_observed[3], 4L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_calculated[3], 7L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Mass[1], 13.73)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Mass[3], 12.5)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Tarsus[1], 16.4)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Tarsus[3], 18.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$WingLength[3], 81)

  # Test 3: individual only caught as an adult (and recovered dead)
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "V143745")), 5)
  #Test that the first and last captures are as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureDate[1], as.Date("2014-03-05"))
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureDate[5], as.Date("2016-04-21"))
  #Test that first and last age observed is as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_observed[1], 6L)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_observed[5], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_calculated[5], 8L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$Mass[1], 17.9)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Mass[5], NA_real_)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Tarsus[5], NA_real_)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$WingLength[1], 73)
  expect_equal(subset(PFN_data, IndvID == "V143745")$WingLength[5], NA_real_)
  #Test that the capture event is correctly characterized
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$ReleaseAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureAlive[5], FALSE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$ReleaseAlive[5], FALSE)

  #Test 4: A re-ringed bird
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "D140987")), 3+8)
  #Test that the first and last captures are as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[1], as.Date("2013-06-13"))
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[3+8], as.Date("2020-06-04"))
  #Test that first and last age observed is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[3+8], 18L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[3+8], 18L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$Mass[1], 11.8)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Mass[3+8], 12.7)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Tarsus[3+8], 17.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$WingLength[1], 77)
  expect_equal(subset(PFN_data, IndvID == "D140987")$WingLength[3+8], 79)

  #Test 5: An individual from KAT (separate IPMR data)
  expect_equal(nrow(subset(PFN_data, IndvID == "R059131")), 2)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "R059131")$CaptureDate[1], as.Date("2003-06-11"))
  expect_equal(subset(PFN_data, IndvID == "R059131")$CaptureDate[2], as.Date(NA))
  expect_equal(subset(PFN_data, IndvID == "R059131")$BreedingSeason[2], 2008)
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "R059131")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "R059131")$Age_observed[2], 13L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "R059131")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "R059131")$Age_calculated[2], 13L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "R059131")$Mass[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "R059131")$Mass[2], NA_real_)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "R059131")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "R059131")$Tarsus[2], NA_real_)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "R059131")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "R059131")$WingLength[2], NA_real_)

  #Test 6: An individual from DIN (separate IPMR data)
  expect_equal(nrow(subset(PFN_data, IndvID == "H424593")), 3)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "H424593")$CaptureDate[1], as.Date("1991-06-14"))
  expect_equal(subset(PFN_data, IndvID == "H424593")$CaptureDate[3], as.Date("1994-06-03"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "H424593")$Age_observed[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "H424593")$Age_observed[3], 10L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "H424593")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "H424593")$Age_calculated[3], 10L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "H424593")$Mass[1], 13.1)
  expect_equal(subset(PFN_data, IndvID == "H424593")$Mass[3], 14.7)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "H424593")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "H424593")$Tarsus[3], NA_real_)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "H424593")$WingLength[1], 78)
  expect_equal(subset(PFN_data, IndvID == "H424593")$WingLength[3], 79)

  #Test 7: An individual captured in multiple populations
  expect_equal(nrow(subset(PFN_data, IndvID == "S846128")), 2)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "S846128")$CaptureDate[1], as.Date("2017-06-06"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$CaptureDate[2], as.Date("2018-06-06"))
  expect_equal(subset(PFN_data, IndvID == "S846128")$CapturePopID[1], "TEI")
  expect_equal(subset(PFN_data, IndvID == "S846128")$CapturePopID[2], "OKE")
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "S846128")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "S846128")$Age_observed[2], 4L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "S846128")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "S846128")$Age_calculated[2], 5L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "S846128")$Mass[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "S846128")$Mass[2], 11.6)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "S846128")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "S846128")$Tarsus[2], NA_real_)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "S846128")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "S846128")$WingLength[2], 76)

  #Test 6: An individual captured with conflicting Nest and IPMR information
  expect_equal(nrow(subset(PFN_data, IndvID == "K758755")), 2)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "K758755")$CaptureDate[1], as.Date("1997-06-13"))
  expect_equal(subset(PFN_data, IndvID == "K758755")$CaptureDate[2], as.Date("1999-06-13"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "K758755")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "K758755")$Age_observed[2], 7L)
  #Test that age calculated is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "K758755")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "K758755")$Age_calculated[2], 7L)
  #Test that wing length is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "K758755")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "K758755")$WingLength[2], 78)
  #Test that capture plot is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "K758755")$CapturePlot[1], "DIN")
  expect_equal(subset(PFN_data, IndvID == "K758755")$CapturePlot[2], "DIN")
  #Test that status before and after capture is as expected
  expect_equal(subset(PFN_data, IndvID == "K758755")$CaptureAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "K758755")$CaptureAlive[2], FALSE)
  expect_equal(subset(PFN_data, IndvID == "K758755")$ReleaseAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "K758755")$ReleaseAlive[2], FALSE)
})


test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only EDM data
  PFN_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "EDM")

  #Test 1: Nestbox check
  expect_true(subset(PFN_data, LocationID == "HIDE9")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(PFN_data, LocationID == "HIDE9")$NestboxID == "HIDE9")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$StartSeason, 1987L)
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$EndSeason, 2019L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$PopID, "EDM")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Latitude, 2) %>% setNames(nm = NULL), 278419)
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Longitude, 2) %>% setNames(nm = NULL), 78643)
  #Check that habitat type is correct
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$HabitatType, "deciduous")

})
