#### Tests for calc_age function

library(dplyr)

#### Old dummy data from the calc_age function
# set.seed(666)
# bird_data <-
#   tibble::tibble(IndvID = LETTERS[sample(1:26, 100, replace = TRUE)],
#                  SampleYear = sample(2012:2016, 100, replace = TRUE),
#                  CaptureDate = as.Date(paste(SampleYear, 3, 31, sep = "-"), format = "%Y-%m-%d")
#                                + sample(1:30, 100, replace = TRUE),
#                  Age_obsv = sample(c(1L, 4L), 100, replace = TRUE)) %>%
#   mutate(Age_calculated = calc_age(data = ., ID = IndvID, Age = Age_obsv,
#                                    Date = CaptureDate, Year = SampleYear))


#### Create dummy dataset of Capture data and run the calc_age function
set.seed(42)
Capture_data <-
  tibble::tibble(
    #### include some individuals without ID
    IndvID = sample(c(paste0("A", 1:25), NA), replace = TRUE, size = 100),
    Species = "PARMAJ") %>%
  dplyr::mutate(SampleYear = sample(2011:2018, 100, replace = TRUE),
                CaptureDate = as.Date(paste(SampleYear, 3, 31, sep = "-"), format = "%Y-%m-%d")
                + sample(1:30, 100, replace = TRUE)) %>%
  dplyr::arrange(IndvID, CaptureDate) %>%
  group_by(IndvID) %>%
  #### assign to some randomly selected IDs code 1 as first Age_obsv, others 4
  dplyr::mutate(rowid_group = row_number(),
                Age_obsv = case_when(IndvID %in% c("A12", "A30", "A21", "A23",
                                                   "A6", "A8", "A10", "A17",
                                                   "A20", "A2", "A18", "A5") &
                                       rowid_group == 1 ~ 1L,
                                     TRUE ~ 4L)) %>%
  #### trying with code 6 inseted of 4
  # dplyr::mutate(rowid_group = row_number(),
  #               Age_obsv = case_when(IndvID %in% c("A12", "A30", "A21", "A23",
  #                                                  "A6", "A8", "A10", "A17",
  #                                                  "A20", "A2", "A18", "A5") &
  #                                      rowid_group == 1 ~ 1L,
  #                                    TRUE ~ 6L)) %>%
  ungroup() %>%
  #### replace some age with NA
  dplyr::mutate(Age_obsv = replace(Age_obsv, row_number(Age_obsv) %in% sample(1:100, 20, replace = FALSE), NA))


#### Run the calc_age function
Capture_data <-
  Capture_data %>%
  dplyr::mutate(Age_calculated = calc_age(data = ., ID = IndvID, Age = Age_obsv,
                                          Date = CaptureDate, Year = SampleYear)) %>%
  #### calculate the diff years, just to check
  dplyr::arrange(IndvID, SampleYear, CaptureDate) %>%
  dplyr::group_by(IndvID) %>%
  dplyr::mutate(FirstYear = as.integer(first(SampleYear))) %>%
  dplyr::mutate(yr_diff   = as.integer(SampleYear) - FirstYear) %>%
  ungroup()


#### Tests
library(testthat)

context("Test the calc_age function")

test_that("The output is integer...", {

  expect_true(is.integer(Capture_data$Age_calculated))

  })

test_that("The output is correctly calculated...", {

  expect_equal(filter(Capture_data, IndvID == "A1") %>% pull(Age_calculated) %>% nth(2), 8)
  expect_gt(filter(Capture_data, IndvID == "A10") %>% pull(Age_calculated) %>% last(), 10)
  expect_equal(filter(Capture_data, IndvID == "A9") %>% pull(Age_calculated) %>% last(), 6)

})




