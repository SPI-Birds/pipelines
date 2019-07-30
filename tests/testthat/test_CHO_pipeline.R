context("Run data quality check on Santo Stefano Quisquina pipeline output")

test_that("CHO pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "CHO"))

})

test_that("CHO outputs all files...", {

  expect_true(file.exists("Brood_data_CHO.csv"))
  expect_true(file.exists("Capture_data_CHO.csv"))
  expect_true(file.exists("Individual_data_CHO.csv"))
  expect_true(file.exists("Location_data_CHO.csv"))

})

test_that("CHO individual data has no errors...", {

  check <- check_format_individual(utils::read.csv("Individual_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("CHO brood data has no errors...", {

  #Check that the format of the data is correct
  check <- check_format_individual(utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that clutch size < brood size
  check <- compare_clutch_brood(utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that brood size < fledglings
  check <- compare_brood_fledglings(utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that laying date < hatch date
  check <- compare_laying_hatching(utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that hatch date < fledge date
  check <- compare_hatching_fledging(utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("Check for impossible values in CHO brood data...", {

  #Check that the format of the data is correct
  brood_data <- utils::read.csv("Brood_data_CHO.csv", stringsAsFactors = FALSE) %>%
    split(f = as.factor(.$Species))

  purrr::pwalk(.l = list(brood_data),
               .f = ~{

                 check <- check_values_brood(Brood_data = ..1, species = unique(..1$Species))

                 expect_false(check$check_list$Error)

               })

})

## THIS NEEDS TO BE FIXED AS THE check_values_capture() function has missing info.
# test_that("Check for impossible values in CHO capture data...", {
#
#   #Check that the format of the data is correct
#   capture_data <- utils::read.csv("Capture_data_CHO.csv", stringsAsFactors = FALSE) %>%
#     split(f = as.factor(.$Species))
#
#   purrr::pwalk(.l = list(capture_data),
#                .f = ~{
#
#                  check <- check_values_capture(Capture_data = ..1, species = unique(..1$Species))
#
#                  expect_false(check$check_list$Error)
#
#                })
#
# })

test_that("CHO capture data has no errors...", {

  check <- check_format_individual(utils::read.csv("Capture_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("CHO location data has no errors...", {

  check <- check_format_individual(utils::read.csv("Location_data_CHO.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})
