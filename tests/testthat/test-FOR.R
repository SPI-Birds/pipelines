pipeline_output <- format_FOR(db = paste0(data_path, "/FOR_ForstenriederPark_Germany"))

test_that("FOR outputs all files...", {
  expect_true(all("FOR" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("FOR" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("FOR" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("FOR" %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")
})

test_that("Brood_data returns an expected outcome...", {
  # Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "FOR")

  # Test 1: Brood observed as "replacement" but calculated as "second"
  # (comes after a successful clutch, so our method counts it as second)
  expect_equal(subset(FOR_data, BroodID == "288")$Species, "PARMAJ")
  expect_equal(subset(FOR_data, BroodID == "288")$FemaleID, "C3F5939")
  expect_equal(subset(FOR_data, BroodID == "288")$MaleID, "C3F5936")
  expect_equal(subset(FOR_data, BroodID == "288")$ClutchType_observed, "replacement")
  expect_equal(subset(FOR_data, BroodID == "288")$ClutchType_calculated, "second")
  expect_equal(subset(FOR_data, BroodID == "288")$LayDate_observed, as.Date("2019-05-29"))
  expect_equal(subset(FOR_data, BroodID == "288")$ClutchSize_observed, 7L)
  expect_equal(subset(FOR_data, BroodID == "288")$BroodSize_observed, 5L)
  expect_equal(subset(FOR_data, BroodID == "288")$NumberFledged_observed, 4L)

  # Test 2: Brood calculated as "replacement", with chick measurements
  expect_equal(subset(FOR_data, BroodID == "260")$Species, "PARMAJ")
  expect_equal(subset(FOR_data, BroodID == "260")$FemaleID, "C3F5908")
  expect_equal(subset(FOR_data, BroodID == "260")$MaleID, "C3F7119")
  expect_equal(subset(FOR_data, BroodID == "260")$ClutchType_calculated, "replacement")
  expect_equal(subset(FOR_data, BroodID == "260")$ClutchSize_observed, 9L)
  expect_equal(subset(FOR_data, BroodID == "260")$BroodSize_observed, 9L)
  expect_equal(subset(FOR_data, BroodID == "260")$NumberFledged_observed, 7L)
  expect_equal(round(subset(FOR_data, BroodID == "260")$AvgChickMass, 2), 14.63)
  expect_equal(subset(FOR_data, BroodID == "260")$NumberChicksMass, 9L)

  # Test 3: Brood in which B4P0112 was laid (a "first" clutch with chick mass/tarsus)
  expect_equal(subset(FOR_data, BroodID == "470")$Species, "CYACAE")
  expect_equal(subset(FOR_data, BroodID == "470")$ClutchType_calculated, "first")
  expect_equal(round(subset(FOR_data, BroodID == "470")$AvgChickMass, 2), 10.51)
  expect_equal(subset(FOR_data, BroodID == "470")$NumberChicksMass, 8L)
  expect_equal(subset(FOR_data, BroodID == "470")$AvgTarsus, 16.4)
  expect_equal(subset(FOR_data, BroodID == "470")$NumberChicksTarsus, 8L)
})

test_that("Individual data returns an expected outcome...", {
  # Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "FOR")

  # Test 1: Ringed as chick, recruited (male)
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$Species, "CYACAE")
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$Sex_calculated, "M")
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$BroodIDLaid, "470")
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$BroodIDFledged, "470")
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$RingSeason, 2020L)
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$RingAge, "chick")

  # Test 2: Ringed as chick, later died as adult
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$Species, "CYACAE")
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$Sex_calculated, "M")
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$BroodIDLaid, "1103")
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$RingSeason, 2021L)
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$RingAge, "chick")

  # Test 3: First caught as adult (female), no brood of origin
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$Species, "CYACAE")
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$Sex_calculated, "F")
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$BroodIDLaid, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$BroodIDFledged, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$RingSeason, 2020L)
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$RingAge, "adult")
})

test_that("Capture data returns an expected outcome...", {
  # Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "FOR")

  # Test 1: Individual ringed as chick, recaptured as adult across seasons
  expect_equal(nrow(subset(FOR_data, IndvID == "B4P0112")), 3)
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$CaptureID, c("B4P0112_1", "B4P0112_2", "B4P0112_3"))
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$CaptureDate[1], as.Date("2020-05-17"))
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$CaptureDate[3], as.Date("2022-05-14"))
  # First capture is as a chick (age observed 1), with a recorded chick age
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$Age_observed[1], 1L)
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$ChickAge[1], 15L)
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$Mass[1], 10.51)
  # Age calculated increments across recaptures
  expect_equal(subset(FOR_data, IndvID == "B4P0112")$Age_calculated, c(1L, 5L, 7L))

  # Test 2: Individual that died on release (ReleaseAlive == FALSE)
  # Ringed as a chick alive in 2021, then died as an adult in 2022
  expect_equal(nrow(subset(FOR_data, IndvID == "B4P0234")), 2)
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$CaptureAlive, c(TRUE, TRUE))
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$ReleaseAlive, c(TRUE, FALSE))
  expect_equal(subset(FOR_data, IndvID == "B4P0234")$CaptureDate[2], as.Date("2022-05-18"))

  # Test 3: Adult-only individual, caught twice in its first season
  expect_equal(nrow(subset(FOR_data, IndvID == "B4J6297")), 2)
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$Age_observed, c(6L, 6L))
  expect_equal(subset(FOR_data, IndvID == "B4J6297")$Sex_observed[2], "F")
})

test_that("Location_data returns an expected outcome...", {
  # Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "FOR")

  # LocationIDs are unique per year (density-manipulation experiment), so they
  # carry the breeding-season suffix and join to the capture/brood data.

  # Test 1: Nestbox with coordinates
  expect_true(subset(FOR_data, LocationID == "C02_2020")$LocationType == "NB")
  expect_true(subset(FOR_data, LocationID == "C02_2020")$NestboxID == "C02_2020")
  expect_equal(subset(FOR_data, LocationID == "C02_2020")$PopID, "FOR")
  expect_equal(round(subset(FOR_data, LocationID == "C02_2020")$Latitude, 4), 48.0835)
  expect_equal(round(subset(FOR_data, LocationID == "C02_2020")$Longitude, 4), 11.4660)
  # Location is specific to a single season
  expect_equal(subset(FOR_data, LocationID == "C02_2020")$StartSeason, 2020L)
  expect_equal(subset(FOR_data, LocationID == "C02_2020")$EndSeason, 2020L)
  expect_equal(subset(FOR_data, LocationID == "C02_2020")$HabitatType, "deciduous")

  # Test 2: Same physical box, different year, has its own record
  expect_equal(subset(FOR_data, LocationID == "D60_2019")$StartSeason, 2019L)
  expect_equal(round(subset(FOR_data, LocationID == "D60_2019")$Latitude, 4), 48.0755)

  # Test 3: Capture LocationIDs resolve in Location_data
  expect_true(all(
    c("C02_2020", "D60_2019") %in% pipeline_output$Location_data$LocationID
  ))
})

### Test protocol compliance
test_protocol_compliance(pipeline_output)
