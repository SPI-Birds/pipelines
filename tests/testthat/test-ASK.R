testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_ASK(db = paste0(data_path, "/ASK_Askainen_Finland"),
                              optional_variables = "all")

test_that("ASK outputs all files...", {

  expect_true("ASK" %in% pipeline_output$Brood_data$siteID)
  expect_true("ASK" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("ASK" %in% pipeline_output$Individual_data$siteID)
  #expect_true("ASK" %in% pipeline_output$Measurement_data$siteID) # Measurement_data is empty
  expect_true("ASK" %in% pipeline_output$Location_data$siteID)
  #expect_true("ASK" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for subadults and chicks

  # Take a subset of ASK - Individual_data
  ASK_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "ASK")

  # Test 1: Subadult great tit female
  # Individual P003106 should be listed as a female great tit
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$calculatedSex, "F")
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$broodIDLaid, NA_character_)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$broodIDFledged, NA_character_)
  # Her ring year should be 1956 with a ringStage of 'subadult'
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$ringYear, 1956)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "P003106"))$ringStage, "subadult")

  # Test 2: Subadult pied flycatcher male
  # Individual J000405 should be listed as a male pied flycatcher
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$calculatedSex, "M")
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$broodIDLaid, NA_character_)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$broodIDFledged, NA_character_)
  # His ring year should be 1973 with a ringStage of 'subadult'
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$ringYear, 1973)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J000405"))$ringStage, "subadult")

  # Test 3: Pied flycatcher chick (not caught later)
  # Individual J000405 should be listed as a pied flycatcher with unknown sex
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$calculatedSex, NA_character_)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodIDLaid & broodIDFledged should be 1980_007_1
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$broodIDLaid, "1980_007_181100")
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$broodIDFledged, "1980_007_181100")
  # Ring year should be 1973 with a ringStage of 'chick'
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$ringYear, 1980)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700243"))$ringStage, "chick")

  # Test 4: Blue tit chick (caught later and sex known)
  # Individual J700018 should be listed as a female blue tit
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$calculatedSex, "F")
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodIDLaid & broodIDFledged should be 1980_048_1
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$broodIDLaid, "1980_048_181079")
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$broodIDFledged, "1980_048_181079")
  # Ring year should be 1973 with a ringStage of 'chick'
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$ringYear, 1980)
  expect_equal(subset(ASK_data, individualID == paste0("ASK_", "J700018"))$ringStage, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of clutch type

  # Take a subset of ASK - Brood_data
  ASK_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "ASK")

  # Test 1: Brood clutch type = first
  # broodID 1949_242_178322  should be pied flycatcher
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodID 1949_242_178322 should have clutch type observed & calculated 'first'
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedClutchType, "first")
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$calculatedClutchType, "first")
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$nestAttemptNumber, 1)
  # Laying date should be "1949-05-25"
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedLayYear, 1949)
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedLayMonth, 5)
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedLayDay, 25)
  # Clutch size should be 7, brood size should be unknown, fledgling number should be 4
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedClutchSize, 7)
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedBroodSize, NA_integer_)
  expect_equal(subset(ASK_data, broodID == "1949_242_178322")$observedNumberFledged, 4)

  # Test 2: Brood clutch type = second
  # broodID 1955_094_182362 should be great tit
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 1955_094_182362 should have clutch type observed 'replacement' & calculated 'second'
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedClutchType, "replacement")
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$calculatedClutchType, "second")
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$nestAttemptNumber, 1) # Nest attempt = 1, because male is unknown
  # Laying date should be "1955-06-25"
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedLayYear, 1955)
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedLayMonth, 6)
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedLayDay, 25)
  # Clutch size should be 10, brood size should be unknown, fledgling number should be 9
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedClutchSize, 10)
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedBroodSize, NA_integer_)
  expect_equal(subset(ASK_data, broodID == "1955_094_182362")$observedNumberFledged, 9)

  # Test 3: Brood clutch type = replacement, where replacement is known (i.e. previous clutch was observed/recorded)
  # broodID 1963_268_179073 should be great tit
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 1963_268_179073 should have clutch type observed & calculated 'second'
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedClutchType, "replacement")
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$calculatedClutchType, "replacement")
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$nestAttemptNumber, 1) # Nest attempt = 1, because male is unknown
  # Laying date should be "1963-06-16"
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedLayYear, 1963)
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedLayMonth, 6)
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedLayDay, 16)
  # Clutch size should be 7, brood size & fledgling number should be unknown
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedClutchSize, 7)
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedBroodSize, NA_integer_)
  expect_equal(subset(ASK_data, broodID == "1963_268_179073")$observedNumberFledged, NA_integer_)

  # Test 4: Brood clutch type = replacement, where replacement calculated from 30-day cutoff
  # broodID 1984_277_181442 should be blue tit
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodID 1984_277_181442 should have clutch type observed 'first' & calculated 'replacement'
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedClutchType, "first")
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$calculatedClutchType, "replacement")
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$nestAttemptNumber, 1) # Nest attempt = 1, because female is unknown
  # Laying date should be "1984-06-28"
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedLayYear, 1984)
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedLayMonth, 6)
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedLayDay, 28)
  # Clutch size should be 7, brood size should be unknown, fledgling number should be 6
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedClutchSize, 7)
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedBroodSize, NA_integer_)
  expect_equal(subset(ASK_data, broodID == "1984_277_181442")$observedNumberFledged, 6)

})

test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for captures as both chicks, males, and females

  # Take a subset of ASK - Capture_data
  ASK_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "ASK")

  # Test 1: Female caught as subadult
  # Test that the individual has the correct number of capture records (10)
  expect_equal(nrow(subset(ASK_data, individualID == paste0("ASK_", "P136052"))), 10)
  # Test that the 1st capture of the female is as expected (1967-05-23; laying date + clutch size)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureYear), 1967)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureDay), 23)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$releaseRingNumber), "P136052")
  # Test that the 10th capture of the female is as expected (1972-05-17; laying date + clutch size)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureYear, 10), 1972)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureMonth, 10), 5)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureDay, 10), 17)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$captureRingNumber, 10), "P136052")
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$releaseRingNumber, 10), "P136052")
  # Test that exactAge is as expected (NA, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$minimumAge), 1)
  # Test that minimumAge is correct on 10th capture (6, because it's caught as a subadult, and 5 years later)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P136052"))$minimumAge, 10), 6)

  # Test 2: Male caught as subadult
  # Test that the individual has the correct number of capture records (9)
  expect_equal(nrow(subset(ASK_data, individualID == paste0("ASK_", "X055889"))), 9)
  # Test that the 1st capture of the male is as expected (1947-05-31; laying date + clutch size)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureYear), 1947)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureDay), 31)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$releaseRingNumber), "X055889")
  # Test that the 9th capture of the male is as expected (1951-06-22; laying date + clutch size)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureYear, 9), 1951)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureMonth, 9), 6)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureDay, 9), 22)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$captureRingNumber, 9), "X055889")
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$releaseRingNumber, 9), "X055889")
  # Test that exactAge is as expected (NA, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$minimumAge), 1)
  # Test that minimumAge is correct on 9th capture (5, because it's caught as a subadult, and 4 years later)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "X055889"))$minimumAge, 9), 5)

  # Test 3: Caught as chick
  # Test that the individual has the correct number of capture records (11)
  expect_equal(nrow(subset(ASK_data, individualID == paste0("ASK_", "P023011"))), 11)
  # Test that the 1st capture of the chick is as expected (1960-06-14)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureYear), 1960)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureDay), 14)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$releaseRingNumber), "P023011")
  # Test that the 11th capture of the chick is as expected (1965-06-18)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureYear, 11), 1965)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureMonth, 11), 6)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureDay, 11), 18)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$captureRingNumber, 11), "P023011")
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$releaseRingNumber, 11), "P023011")
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$exactAge), 0)
  # Test that exactAge is correct on 11th capture (5, because it's caught as a chick, and 5 years later)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$exactAge, 11), 5)
  # Test that minimumAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$minimumAge), 0)
  # Test that minimumAge is correct on 11th capture (5, because it's caught as a chick, and 5 years later)
  expect_equal(dplyr::nth(subset(ASK_data, individualID == paste0("ASK_", "P023011"))$minimumAge, 11), 5)

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nesting locations with precise and imprecise coordinates

  # Take a subset of WHZ - Location_data
  ASK_data <- dplyr::filter(pipeline_output$Location_data, siteID == "ASK")

  # Test 1: Nest box with coordinates
  # Nestbox 004 should be type "nest", and put up in 1941
  expect_equal(subset(ASK_data, locationID == "004")$locationType, "nest")
  expect_equal(subset(ASK_data, locationID == "004")$startYear, 1941)

  # Back-transform coordinates to test accuracy
  coords1 <- sf::st_transform(sf::st_as_sf(subset(ASK_data, locationID == "004"),
                                           coords = c("decimalLongitude", "decimalLatitude"),
                                           crs = 4326),
                              crs = 2393) %>%
    sf::st_coordinates()

  # Test that the back-transformed coordinates match with the original crs
  expect_equal(round(coords1[,1]), 3212686)
  expect_equal(round(coords1[,2]), 6719792)

  # Test 2: Nest box with imprecise coordinates (i.e., which are set to NA)
  # Nestbox 001 should be type "nest", and put up in 1941
  expect_equal(subset(ASK_data, locationID == "001")$locationType, "nest")
  expect_equal(subset(ASK_data, locationID == "001")$startYear, 1941)
  expect_equal(subset(ASK_data, locationID == "001")$decimalLatitude, NA_real_)
  expect_equal(subset(ASK_data, locationID == "001")$decimalLongitude, NA_real_)

})


test_that("Expected columns are present", {

  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood")

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture")

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual")

  ## Measurement data: Test that all columns are present
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location")

  ## Experiment data: Test that all columns are present
  test_col_present(pipeline_output, "Experiment")

})

test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Measurement data: Test that all column classes are expected
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

  ## Experiment data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Experiment")

})

test_that("ID columns match the expected format for the pipeline", {

  ## femaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^ASK_[:upper:]{1}[:digit:]{6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^ASK_[:upper:]{1}[:digit:]{6}$")

  ## individualID format in Capture data is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^ASK_[:upper:]{1}[:digit:]{6}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^ASK_[:upper:]{1}[:digit:]{6}$")

})

test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID")

  ## captureID has only unique values
  test_unique_values(pipeline_output, "captureID")

  ## individualID has only unique values
  test_unique_values(pipeline_output, "individualID")

  ## measurementID has only unique values
  test_unique_values(pipeline_output, "measurementID")

  ## locationID has only unique values
  test_unique_values(pipeline_output, "locationID")

  ## treatmentID has only unique values
  test_unique_values(pipeline_output, "treatmentID")

})

test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture") # TODO: Check with data owner for captureMonth, captureDay

  ## Individual
  test_NA_columns(pipeline_output, "Individual") # TODO: Check with data owner for ringMonth, ringDay

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement")

  ## Location
  test_NA_columns(pipeline_output, "Location")

  ## Experiment
  test_NA_columns(pipeline_output, "Experiment")

})

test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture")

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Measurement
  test_category_columns(pipeline_output, "Measurement")

  ## Location
  test_category_columns(pipeline_output, "Location")

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

})
