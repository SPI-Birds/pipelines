context("Test that quality_check functions as expected...")

test_that("All quality check summary items are returned...", {

  expect_true(exists("CheckList", where = dummy_check))
  expect_true(exists("NumberChecks", where = dummy_check))
  expect_true(exists("NumberWarnings", where = dummy_check))
  expect_true(exists("NumberErrors", where = dummy_check))
  expect_true(exists("ElapsedTime", where = dummy_check))

})

test_that("CheckList returns an expected outcome...", {

  expect_true(all(c("tbl_df", "tbl", "data.frame")) %in% class(dummy_check$CheckList))
  expect_equal(dummy_check$CheckList[1,]$CheckID, "B1")
  expect_equal(dummy_check$CheckList[1,]$CheckDescription, "Brood data format")
  expect_equal(dummy_check$CheckList[1,]$Warning, "TRUE")
  expect_equal(dummy_check$CheckList[1,]$Error, "FALSE")

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

test_that("Output reports are not created...", {

  # expect_false(file.exists(system.file("tests/testthat/output-report.md", package = "SPIbirds")))
  # expect_false(file.exists(system.file("tests/testthat/output-report.pdf", package = "SPIbirds")))
  # expect_false(file.exists(system.file("tests/testthat/output-report.html", package = "SPIbirds")))

  expect_false(file.exists("output-report.md"))
  expect_false(file.exists("output-report.pdf"))
  expect_false(file.exists("output-report.html"))

})
