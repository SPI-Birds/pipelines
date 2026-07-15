pipeline_output <- format_BRG(db = paste0(data_path, "/BRG_Bergen_Norway"))

test_that("BRG outputs all files...", {
  expect_true(all("BRG" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("BRG" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("BRG" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("BRG" %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")
})

test_that("Brood_data returns an expected outcome...", {
  # Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "BRG")

  # Test 1: Brood with only FemaleID known
  expect_equal(subset(BRG_data, BroodID == "2017-1")$Species, "PARMAJ")
  expect_equal(subset(BRG_data, BroodID == "2017-1")$FemaleID, "EK06516")
  expect_equal(subset(BRG_data, BroodID == "2017-1")$MaleID, NA_character_)
  expect_equal(subset(BRG_data, BroodID == "2017-1")$LayDate_observed, as.Date("2017-05-04"))
  expect_equal(subset(BRG_data, BroodID == "2017-1")$ClutchSize_observed, 8L)
  expect_equal(subset(BRG_data, BroodID == "2017-1")$BroodSize_observed, 8L)
  expect_equal(subset(BRG_data, BroodID == "2017-1")$NumberFledged_observed, 8L)
  expect_equal(round(subset(BRG_data, BroodID == "2017-1")$AvgChickMass, 2), 17.04)
  expect_equal(subset(BRG_data, BroodID == "2017-1")$NumberChicksMass, 7L)

  # Test 2: Brood with only MaleID known
  expect_equal(subset(BRG_data, BroodID == "2017-2")$Species, "CYACAE")
  expect_equal(subset(BRG_data, BroodID == "2017-2")$FemaleID, NA_character_)
  expect_equal(subset(BRG_data, BroodID == "2017-2")$MaleID, "HH96001")
  expect_equal(subset(BRG_data, BroodID == "2017-2")$LayDate_observed, as.Date("2017-05-05"))
  expect_equal(subset(BRG_data, BroodID == "2017-2")$ClutchSize_observed, 8L)

  # Test 3: Brood with both parents known
  expect_equal(subset(BRG_data, BroodID == "2017-3")$Species, "PARMAJ")
  expect_equal(subset(BRG_data, BroodID == "2017-3")$FemaleID, "EK06510")
  expect_equal(subset(BRG_data, BroodID == "2017-3")$MaleID, "EK06503")
  expect_equal(subset(BRG_data, BroodID == "2017-3")$LayDate_observed, as.Date("2017-04-23"))
  expect_equal(subset(BRG_data, BroodID == "2017-3")$ClutchSize_observed, 8L)
  expect_equal(subset(BRG_data, BroodID == "2017-3")$BroodSize_observed, 7L)
  expect_equal(subset(BRG_data, BroodID == "2017-3")$NumberFledged_observed, 6L)

  # Test 4: Brood in which EK06579/EK06602 were raised as chicks
  expect_equal(subset(BRG_data, BroodID == "2018-25")$Species, "PARMAJ")
  expect_equal(subset(BRG_data, BroodID == "2018-25")$LayDate_observed, as.Date("2018-04-24"))
  expect_equal(subset(BRG_data, BroodID == "2018-25")$ClutchSize_observed, 7L)
  expect_equal(subset(BRG_data, BroodID == "2018-25")$BroodSize_observed, 7L)
  expect_equal(subset(BRG_data, BroodID == "2018-25")$NumberFledged_observed, 6L)
})

test_that("Individual data returns an expected outcome...", {
  # Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "BRG")

  # Test 1: Ringed as chick, recruited (male)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$Species, "PARMAJ")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$Sex_calculated, "M")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$BroodIDLaid, "2018-25")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$BroodIDFledged, "2018-25")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$RingSeason, 2018L)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$RingAge, "chick")

  # Test 2: Ringed as chick, recruited (female)
  expect_equal(subset(BRG_data, IndvID == "EK06602")$Species, "PARMAJ")
  expect_equal(subset(BRG_data, IndvID == "EK06602")$Sex_calculated, "F")
  expect_equal(subset(BRG_data, IndvID == "EK06602")$BroodIDLaid, "2018-26")
  expect_equal(subset(BRG_data, IndvID == "EK06602")$BroodIDFledged, "2018-26")
  expect_equal(subset(BRG_data, IndvID == "EK06602")$RingSeason, 2018L)
  expect_equal(subset(BRG_data, IndvID == "EK06602")$RingAge, "chick")

  # Test 3: First caught as adult (female)
  expect_equal(subset(BRG_data, IndvID == "HH96002")$Species, "CYACAE")
  expect_equal(subset(BRG_data, IndvID == "HH96002")$Sex_calculated, "F")
  expect_equal(subset(BRG_data, IndvID == "HH96002")$BroodIDLaid, NA_character_)
  expect_equal(subset(BRG_data, IndvID == "HH96002")$BroodIDFledged, NA_character_)
  expect_equal(subset(BRG_data, IndvID == "HH96002")$RingSeason, 2017L)
  expect_equal(subset(BRG_data, IndvID == "HH96002")$RingAge, "adult")

  # Test 4: First caught as adult (male)
  expect_equal(subset(BRG_data, IndvID == "HD86406")$Species, "PERATE")
  expect_equal(subset(BRG_data, IndvID == "HD86406")$Sex_calculated, "M")
  expect_equal(subset(BRG_data, IndvID == "HD86406")$BroodIDLaid, NA_character_)
  expect_equal(subset(BRG_data, IndvID == "HD86406")$BroodIDFledged, NA_character_)
  expect_equal(subset(BRG_data, IndvID == "HD86406")$RingSeason, 2019L)
  expect_equal(subset(BRG_data, IndvID == "HD86406")$RingAge, "adult")
})

test_that("Capture data returns an expected outcome...", {
  # Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "BRG")

  # Test 1: Individual ringed as chick, recaptured 3 more times as adult
  expect_equal(nrow(subset(BRG_data, IndvID == "EK06579")), 4)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$CaptureID, c("EK06579_1", "EK06579_2", "EK06579_3", "EK06579_4"))
  expect_equal(subset(BRG_data, IndvID == "EK06579")$CaptureDate[1], as.Date("2018-05-29"))
  expect_equal(subset(BRG_data, IndvID == "EK06579")$CaptureDate[4], as.Date("2021-05-30"))
  expect_equal(subset(BRG_data, IndvID == "EK06579")$CaptureTime[1], "10:00")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$CaptureTime[4], "18:30")
  expect_equal(subset(BRG_data, IndvID == "EK06579")$ChickAge[1], 15L)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$Mass[1], 17.5)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$WingLength[4], 77)
  # Age observed as chick (1), then full-grown but not full year (5), then adult (6)
  expect_equal(subset(BRG_data, IndvID == "EK06579")$Age_observed, c(1L, 5L, 6L, 6L))
  expect_equal(subset(BRG_data, IndvID == "EK06579")$Age_calculated, c(1L, 5L, 7L, 9L))

  # Test 2: Individual caught only as adult, across seasons
  expect_equal(nrow(subset(BRG_data, IndvID == "HH96002")), 4)
  expect_equal(subset(BRG_data, IndvID == "HH96002")$CaptureID, c("HH96002_1", "HH96002_2", "HH96002_3", "HH96002_4"))
  expect_equal(subset(BRG_data, IndvID == "HH96002")$CaptureDate[1], as.Date("2017-06-02"))
  expect_equal(subset(BRG_data, IndvID == "HH96002")$CaptureDate[4], as.Date("2021-06-03"))
  expect_equal(subset(BRG_data, IndvID == "HH96002")$Sex_observed, rep("F", 4))
  expect_equal(subset(BRG_data, IndvID == "HH96002")$CaptureTime[4], "09:30")

  # Test 3: Dead on capture and release
  expect_equal(subset(BRG_data, IndvID == "EK06556")$CaptureAlive, FALSE)
  expect_equal(subset(BRG_data, IndvID == "EK06556")$ReleaseAlive, FALSE)
})

test_that("Location_data returns an expected outcome...", {
  # Take a subset of only BRG data
  BRG_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "BRG")

  # Test 1: Nestbox check
  expect_true(subset(BRG_data, LocationID == "33")$LocationType == "NB")
  # Expect LocationID and NestboxID are the same
  expect_true(subset(BRG_data, LocationID == "33")$NestboxID == "33")
  # Expect StartSeason is as expected, no EndSeason recorded
  expect_equal(subset(BRG_data, LocationID == "33")$StartSeason, 2017L)
  expect_equal(subset(BRG_data, LocationID == "33")$EndSeason, NA_integer_)
  # Check that LocationID is in the expected PopID
  expect_equal(subset(BRG_data, LocationID == "33")$PopID, "BRG")
  # Check HabitatType translation from primary data ("mixed")
  expect_equal(subset(BRG_data, LocationID == "15")$HabitatType, "mixed")
})

### Test protocol compliance
test_protocol_compliance(pipeline_output)
