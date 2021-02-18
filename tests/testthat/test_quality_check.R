context("Test that quality_check functions as expected...")

test_that("All quality check summary items are returned...", {

  expect_true(exists("CheckList", where = dummy_check))
  expect_true(exists("NumberChecks", where = dummy_check))
  expect_true(exists("NumberWarnings", where = dummy_check))
  expect_true(exists("NumberErrors", where = dummy_check))
  expect_true(exists("ElapsedTime", where = dummy_check))
  expect_true(exists("R_data", where = dummy_check))

})

test_that("CheckList returns an expected outcome...", {

  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(dummy_check$CheckList)))
  expect_equal(dummy_check$CheckList[1,]$CheckID, "B2")
  expect_equal(dummy_check$CheckList[1,]$CheckDescription, "Compare clutch and brood sizes")
  expect_equal(dummy_check$CheckList[1,]$Warning, TRUE)
  expect_equal(dummy_check$CheckList[1,]$Error, TRUE)

})

test_that("NumberChecks returns an expected outcome...", {

  expect_equal(class(dummy_check$NumberChecks), "integer")
  expect_equal(dummy_check$NumberChecks, nrow(dummy_check$CheckList))

})

test_that("NumberWarnings returns an expected outcome...", {

  expect_equal(class(dummy_check$NumberWarnings), "integer")
  expect_lte(dummy_check$NumberWarnings, dummy_check$NumberChecks)
  expect_equal(dummy_check$NumberWarnings, sum(dplyr::pull(dummy_check$CheckList[, "Warning"])))

})

test_that("NumberErrors returns an expected outcome...", {

  expect_equal(class(dummy_check$NumberErrors), "integer")
  expect_lte(dummy_check$NumberErrors, dummy_check$NumberChecks)
  expect_equal(dummy_check$NumberErrors, sum(dplyr::pull(dummy_check$CheckList[, "Error"])))

})

test_that("ElapsedTime returns an expected outcome...", {

  expect_equal(class(dummy_check$ElapsedTime), "difftime")

})

test_that("R_data returns an expected outcome...", {

  expect_length(dummy_check$R_data, 4)
  expect_equal(names(dummy_check$R_data), c("Brood_data", "Capture_data", "Individual_data", "Location_data"))
  expect_gt(length(dummy_check$R_data$Brood_data), 0)
  expect_gt(length(dummy_check$R_data$Capture_data), 0)
  expect_gt(length(dummy_check$R_data$Individual_data), 0)
  expect_gt(length(dummy_check$R_data$Location_data), 0)

})

test_that("Output reports are not created...", {

  expect_false(file.exists("output-report.md"))
  expect_false(file.exists("output-report.pdf"))
  expect_false(file.exists("output-report.html"))

})

test_that("Single checks function as expected...", {

  # Test check B2
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B2")$Warning, c(NA, NA, NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B2")$Error, c(NA, TRUE, NA, NA))

  # Test check B3
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B3")$Warning, c(NA, NA, NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B3")$Error, c(NA, TRUE, NA, NA))

  # Test check B4
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B4")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B4")$Error, c(NA, TRUE))

  # Test check B5
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B5")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B5")$Error, c(NA, TRUE))

  # Test check B6a
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6a")$Warning[c(1, 152, 153, 154)], c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6a")$Error[c(1, 152, 153, 154)], c(NA, NA, TRUE, TRUE))

  # Test check B6b
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6b")$Warning[c(1, 152, 153, 154)], c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6b")$Error[c(1, 152, 153, 154)], c(NA, NA, TRUE, TRUE))

  # Test check B6c
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6c")$Warning[c(1, 152, 153, 154)], c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6c")$Error[c(1, 152, 153, 154)], c(NA, NA, TRUE, TRUE))

  # Test check B6d
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6d")$Warning[c(1, 152, 153, 154, 155)], c(NA, TRUE, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B6d")$Error[c(1, 152, 153, 154, 155)], c(NA, NA, NA, TRUE, TRUE))

  # Test check B7
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B7")$Warning, c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B7")$Error, c(NA, NA, TRUE))

  # Test check B8
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B8")$Warning, c(NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B8")$Error, c(NA, TRUE, TRUE))

  # Test check B9
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B9")$Warning, c(NA, NA, NA, NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B9")$Error, c(NA, NA, NA, NA, NA, TRUE))

  # Test check B10
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B10")$Warning, c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B10")$Error, c(NA, NA, TRUE))

  # Test check B11
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B11")$Warning, c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B11")$Error, c(NA, NA, TRUE))

  # Test check B12
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B12")$Warning, c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B12")$Error, c(NA, NA, TRUE))

  # Test check B13
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B13")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "B13")$Error, c(NA, TRUE))

  # Test check C2a
  # adults
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2a_adults")$Warning[c(1, 152, 153)],
               c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2a_adults")$Error[c(1, 152, 153)],
               c(NA, NA, TRUE))

  # chicks
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2a_chicks")$Warning[1:9],
               c(NA, TRUE, NA, NA, TRUE, NA, NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2a_chicks")$Error[1:9],
               c(NA, NA, TRUE, NA, NA, TRUE, NA, NA, TRUE))

  # Test check C2b
  # adults
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2b_adults")$Warning[c(1, 152, 153)],
               c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2b_adults")$Error[c(1, 152, 153)],
               c(NA, NA, TRUE))

  # chicks
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2b_chicks")$Warning[c(1, 152, 153)],
               c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C2b_chicks")$Error[c(1, 152, 153)],
               c(NA, NA, TRUE))

  # Test check C3
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C3")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C3")$Error, c(NA, TRUE))

  # Test check C4
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C4")$Warning, c(NA, NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C4")$Error, c(NA, NA, NA))

  # Test check C5
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C5")$Warning, c(NA, NA, NA, NA, TRUE, NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, CheckID == "C5")$Error, c(NA, NA, NA, NA, NA, NA, TRUE, NA))

  # Test check I2
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I2")$Warning, c(NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I2")$Error, c(NA, TRUE, TRUE))

  # Test check I3
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I3")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I3")$Error, c(NA, TRUE))

  # Test check I4
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I4")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I4")$Error, c(NA, TRUE))

  # Test check I5
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I5")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I5")$Error, c(NA, TRUE))

  # Test check I6
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I6")$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, CheckID == "I6")$Error, c(NA, TRUE))

})

test_that("Approved-listing procedure functions as expected...", {

  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "Approved list")$Warning, NA)
  expect_equal(subset(dummy_check$R_data$Brood_data, CheckID == "Approved list")$Error, NA)

})
