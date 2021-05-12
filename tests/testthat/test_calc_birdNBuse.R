context("Test that calc_birdNBuse() functions as expected...")

dummy_Brood_data <- tibble::tibble(
  BroodID = rep(c(1:5), 2),
  PopID = rep(c('XXX', 'YYY'), each = 5),
  LocationID = LETTERS[c(1,2,1,1,2,2,1,2,1,2)],
  BreedingSeason = c(2001, 2001, 2002, 2003, 2003,1962, 1963, 1963, 1965, 1965))

test_NBdata <- calc_birdNBuse(dummy_Brood_data)

test_that("All output columns are as expected...", {
  expect_equal(test_NBdata$PopID, rep(c('XXX', 'YYY'), each = 2))
  expect_equal(test_NBdata$LocationID, rep(c('A', 'B'), 2))
  expect_equal(test_NBdata$StartSeason, c(2001L, 2001L, 1963L, 1962L))
  expect_equal(test_NBdata$EndSeason, c(2003L, 2003L, 1965L, 1965L))
})
