context("Test that calc_clutchtype() functions as expected...")

set.seed(666)
dat <- tibble::tibble(PopID = "TEST", Species = "PARMAJ",
                      FemaleID = sample(LETTERS[1:7], size = 100, replace = TRUE),
                      NumberFledged = rpois(n = 100, lambda = 1),
                      NumberFledged_observed = NumberFledged,
                      #Create 100 fake broods
                      BreedingSeason = sample(c(seq(2000, 2012, 1)), size = 100, replace = TRUE),
                      LayDate = as.Date(paste(BreedingSeason,
                                              sample(c(4, 5, 6), size = 100, replace = TRUE),
                                              sample(seq(1, 31, 1), size = 100, replace = TRUE), sep = "-"),
                                        format = "%Y-%m-%d"),
                      LayDate_observed = LayDate)

dat$clutch_type_1_0 <- calc_clutchtype(data = dat, na.rm = FALSE, protocol_version = "1.0")
dat$clutch_type_1_1 <- calc_clutchtype(data = dat, na.rm = FALSE, protocol_version = "1.1")

## Change NA to "NA" in order to compare in tests
dat <- dat %>%
  tidyr::replace_na(list(clutch_type_1_0 = "NA", clutch_type_1_1 = "NA"))

test_that("Different protocol versions return same values...", {

  expect_true(all(dat$clutch_type_1_0 == dat$clutch_type_1_1))

})

