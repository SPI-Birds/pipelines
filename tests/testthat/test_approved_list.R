context("Test that approved_list functions as expected...")

test_that("Approved_list exists in data folder...", {

  expect_true(file.exists(here::here("./data/approved_list.rda")))

})

test_that("Approved_list contains all 4 dataframes...", {

  expect_true(exists("Brood_approved_list", approved_list))
  expect_true(exists("Capture_approved_list", approved_list))
  expect_true(exists("Individual_approved_list", approved_list))
  expect_true(exists("Location_approved_list", approved_list))

})

test_that("Approved_list dataframes contain the expected columns...", {

  expect_true(all(purrr::map_chr(approved_list, ~{colnames(.x)[1]}) == "PopID"))
  expect_equal(colnames(approved_list$Brood_approved_list)[2], "BroodID")
  expect_equal(colnames(approved_list$Capture_approved_list)[2], "CaptureID")
  expect_equal(colnames(approved_list$Individual_approved_list)[2], "IndvID")
  expect_equal(colnames(approved_list$Location_approved_list)[2], "LocationID")
  expect_true(all(purrr::map_chr(approved_list, ~{colnames(.x)[3]}) == "CheckID"))

})
