context("Run data quality check on Bandon Valley pipeline output")

test_that("BAN outputs all files...", {

  expect_true(exists("Brood_data", where = pipeline_output$BAN))
  expect_true(exists("Capture_data", where = pipeline_output$BAN))
  expect_true(exists("Individual_data", where = pipeline_output$BAN))
  expect_true(exists("Location_data", where = pipeline_output$BAN))

})

test_that("BAN individual data has no errors...", {

  check <- check_format_individual(pipeline_output$BAN$Individual_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

})

test_that("BAN brood data has no errors...", {

  #Check that the format of the data is correct
  check <- check_format_individual(pipeline_output$BAN$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

  #Check that clutch size < brood size
  check <- compare_clutch_brood(pipeline_output$BAN$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

  #Check that brood size < fledglings
  check <- compare_brood_fledglings(pipeline_output$BAN$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

  #Check that laying date < hatch date
  check <- compare_laying_hatching(pipeline_output$BAN$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

  #Check that hatch date < fledge date
  check <- compare_hatching_fledging(pipeline_output$BAN$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

})

test_that("Check for impossible values in BAN brood data...", {

  #Check that the format of the data is correct
  brood_data <- pipeline_output$BAN$Brood_data %>%
    split(f = as.factor(.$Species))

  purrr::pwalk(.l = list(brood_data),
               .f = ~{

                 check <- check_values_brood(Brood_data = ..1, species = unique(..1$Species))

                 expect_false(check$check_list$Error)

                 if(check$check_list$Error == TRUE){

                   purrr::pwalk(.l = check$error_output, .f = print)

                 }

               })

})

## THIS NEEDS TO BE FIXED AS THE check_values_capture() function has missing info.
# test_that("Check for impossible values in BAN capture data...", {
#
#   #Check that the format of the data is correct
#   capture_data <- utils::read.csv("Capture_data_BAN.csv", stringsAsFactors = FALSE) %>%
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

test_that("BAN capture data has no errors...", {

  check <- check_format_individual(pipeline_output$BAN$Capture_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

})

test_that("BAN location data has no errors...", {

  check <- check_format_individual(pipeline_output$BAN$Location_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    purrr::pwalk(.l = check$error_output, .f = print)

  }

})
