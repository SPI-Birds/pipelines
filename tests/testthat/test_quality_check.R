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

  # Test 1: B2
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 1:4)$Warning, c(NA, NA, NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 1:4)$Error, c(NA, TRUE, NA, NA))

  # Test 2: B3
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 5:8)$Warning, c(NA, NA, NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 5:8)$Error, c(NA, TRUE, NA, NA))

  # Test 3: B4
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 9:10)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 9:10)$Error, c(NA, TRUE))

  # Test 4: B5
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 11:12)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 11:12)$Error, c(NA, TRUE))

  # Test 5: B6a
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 13:16)$Warning, c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 13:16)$Error, c(NA, NA, TRUE, TRUE))

  # Test 6: B6b
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 17:20)$Warning, c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 17:20)$Error, c(NA, NA, TRUE, TRUE))

  # Test 7: B6c
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 21:24)$Warning, c(NA, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 21:24)$Error, c(NA, NA, TRUE, TRUE))

  # Test 8: B7
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 25:26)$Warning, c(NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 25:26)$Error, c(NA, NA))

  # Test 9: B8
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 27:29)$Warning, c(NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 27:29)$Error, c(NA, NA, TRUE))

  # Test 10: B9
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 30:32)$Warning, c(NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 30:32)$Error, c(NA, TRUE, TRUE))

  # Test 11: B10
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 33:38)$Warning, c(NA, NA, NA, NA, NA, NA))
  expect_equal(subset(dummy_check$R_data$Brood_data, Row %in% 33:38)$Error, c(NA, NA, NA, NA, NA, TRUE))

  # Test 12: C2a
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 1:12)$Warning,
               c(NA, TRUE, NA, NA, TRUE, NA, NA, TRUE, NA, NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 1:12)$Error,
               c(NA, NA, TRUE, NA, NA, TRUE, NA, NA, TRUE, NA, NA, TRUE))

  # Test 13: C2b
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 13:18)$Warning, c(NA, TRUE, NA, NA, TRUE, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 13:18)$Error, c(NA, NA, TRUE, NA, NA, TRUE))

  # Test 14: C3
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 19:20)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Capture_data, Row %in% 19:20)$Error, c(NA, TRUE))

  # Test 15: I2
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 9:13)$Warning, c(NA, TRUE, TRUE, NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 9:13)$Error, c(NA, NA, NA, TRUE, TRUE))

  # Test 16: I3
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 14:15)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 14:15)$Error, c(NA, TRUE))

  # Test 17: I4
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 16:17)$Warning, c(NA, TRUE))
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 16:17)$Error, c(NA, NA))

  # Test 18: I5
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 18:19)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 18:19)$Error, c(NA, TRUE))

  # Test 19: I6
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 20:21)$Warning, c(NA, NA))
  expect_equal(subset(dummy_check$R_data$Individual_data, Row %in% 20:21)$Error, c(NA, TRUE))
})

test_that("Approved-listing procedure functions as expected...", {

  expect_equal(subset(dummy_check$R_data$Brood_data, Row == 0)$Warning, NA)
  expect_equal(subset(dummy_check$R_data$Brood_data, Row == 0)$Error, NA)

})
