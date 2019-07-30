context("Run data quality check on Santo Stefano Quisquina pipeline output")

test_that("VEL pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "VEL"))

})

test_that("VEL outputs all files...", {

  expect_true(file.exists("Brood_data_VEL.csv"))
  expect_true(file.exists("Capture_data_VEL.csv"))
  expect_true(file.exists("Individual_data_VEL.csv"))
  expect_true(file.exists("Location_data_VEL.csv"))

})

test_that("VEL individual data has no errors...", {

  check <- check_format_individual(utils::read.csv("Individual_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("VEL brood data has no errors...", {

  #Check that the format of the data is correct
  check <- check_format_individual(utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that clutch size < brood size
  check <- compare_clutch_brood(utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that brood size < fledglings
  check <- compare_brood_fledglings(utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that laying date < hatch date
  check <- compare_laying_hatching(utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

  #Check that hatch date < fledge date
  check <- compare_hatching_fledging(utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("Check for impossible values in VEL brood data...", {

  #Check that the format of the data is correct
  brood_data <- utils::read.csv("Brood_data_VEL.csv", stringsAsFactors = FALSE) %>%
    split(f = as.factor(.$Species))

  purrr::pwalk(.l = list(brood_data),
               .f = ~{

                 check <- check_values_brood(Brood_data = ..1, species = unique(..1$Species))

                 expect_false(check$check_list$Error)

               })

})

## THIS NEEDS TO BE FIXED AS THE check_values_capture() function has missing info.
# test_that("Check for impossible values in VEL capture data...", {
#
#   #Check that the format of the data is correct
#   capture_data <- utils::read.csv("Capture_data_VEL.csv", stringsAsFactors = FALSE) %>%
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

test_that("VEL capture data has no errors...", {

  check <- check_format_individual(utils::read.csv("Capture_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})

test_that("VEL location data has no errors...", {

  check <- check_format_individual(utils::read.csv("Location_data_VEL.csv", stringsAsFactors = FALSE))

  expect_false(check$check_list$Error)

})
