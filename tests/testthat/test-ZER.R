zer_data_dir <- Sys.getenv("ZER_DATA_DIR", unset = "")
testthat::skip_if(
  !nzchar(zer_data_dir) ||
    !file.exists(file.path(zer_data_dir, "ZER_PrimaryData_new.xlsx")),
  "Set ZER_DATA_DIR to the folder with ZER_PrimaryData_new.xlsx"
)

zer_output <- format_ZER(db = zer_data_dir)

testthat::test_that("ZER uses the new breeding workbook", {
  brood <- zer_output$Brood_data
  testthat::expect_identical(zer_output$protocol_version, "2.0.0")
  testthat::expect_equal(nrow(brood), 3074L)
  testthat::expect_true(all(brood$siteID == "ZER"))
  testthat::expect_true(all(brood$speciesID == "Turdus merula"))
  testthat::expect_false(anyDuplicated(brood$broodID) > 0L)
  testthat::expect_true(all(brood$locationID %in% zer_output$Location_data$locationID))

  first <- brood[brood$broodID == "ZER-1997-2", ]
  testthat::expect_equal(c(first$observedLayYear,
                           first$observedLayMonth,
                           first$observedLayDay), c(1997L, 4L, 8L))
  testthat::expect_equal(first$observedClutchSize, 4L)
  testthat::expect_identical(first$siteID, "ZER")
  testthat::expect_true(all(is.na(brood$observedClutchType)))
})

testthat::test_that("source errors and repeated row numbers are handled", {
  brood <- zer_output$Brood_data
  wrong_year <- brood[brood$broodID == "ZER-2014-2543", ]
  testthat::expect_equal(nrow(wrong_year), 1L)
  testthat::expect_true(is.na(wrong_year$observedLayYear))
  testthat::expect_equal(wrong_year$observedClutchSize, 5L)
  testthat::expect_true(all(c("ZER-2005-1198", "ZER-2005-1198-1") %in% brood$broodID))
  testthat::expect_true(all(c("ZER-2018-3268", "ZER-2018-3268-1") %in% brood$broodID))
})

testthat::test_that("no capture or ringing events are invented", {
  testthat::expect_equal(nrow(zer_output$Capture_data), 0L)
  testthat::expect_equal(nrow(zer_output$Individual_data), 0L)
  testthat::expect_equal(nrow(zer_output$Measurement_data), 0L)
  testthat::expect_equal(nrow(zer_output$Experiment_data), 0L)
  testthat::expect_true(all(zer_output$Location_data$locationType == "nest"))
  testthat::expect_true(any(!is.na(zer_output$Brood_data$maleID)))
  testthat::expect_true(any(!is.na(zer_output$Brood_data$femaleID)))
  testthat::expect_error(format_ZER(db = zer_data_dir, pop = "KOW"), "only Zeromski")
})

test_protocol_compliance(zer_output)
