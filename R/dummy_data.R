#' Create quality check dummy data
#'
#' @description Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}. In each dataframe, rows are created to test single checks from \code{\link{quality_check}}. A CheckID column is added to each dataframe to mark which rows serve to test each check. See a detailed description of the dummy data in \code{\link{dummy_data}}.
#'
#' @param overwrite Overwrite existing file. Default: TRUE
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_data}{Dummy brood data.}
#' \item{Capture_data}{Dummy capture data.}
#' \item{Individual_data}{Dummy individual data.}
#' \item{Location_data}{Dummy location data.}
#'
#' @export

create_dummy_data <- function(overwrite=TRUE) {

  # Set seed for dummy data drawn from distributions
  set.seed(74)

  # Create skeletons for each pipeline data frame
  # Brood data
  Brood_data <- tibble::tibble(
    Row = NA_integer_,
    BroodID = NA_character_,
    PopID = NA_character_,
    BreedingSeason = NA_integer_,
    Species = NA_character_,
    Plot = NA_character_,
    LocationID = NA_character_,
    FemaleID = NA_character_,
    MaleID = NA_character_,
    ClutchType_observed  = NA_character_,
    ClutchType_calculated  = NA_character_,
    LayDate_observed = as.Date(NA_character_),
    LayDate_min = as.Date(NA_character_),
    LayDate_max = as.Date(NA_character_),
    ClutchSize_observed = NA_integer_,
    ClutchSize_min = NA_integer_,
    ClutchSize_max = NA_integer_,
    HatchDate_observed = as.Date(NA_character_),
    HatchDate_min = as.Date(NA_character_),
    HatchDate_max = as.Date(NA_character_),
    BroodSize_observed = NA_integer_,
    BroodSize_min = NA_integer_,
    BroodSize_max = NA_integer_,
    FledgeDate_observed = as.Date(NA_character_),
    FledgeDate_min = as.Date(NA_character_),
    FledgeDate_max = as.Date(NA_character_),
    NumberFledged_observed = NA_integer_,
    NumberFledged_min = NA_integer_,
    NumberFledged_max = NA_integer_,
    AvgEggMass = NA_real_,
    NumberEggs = NA_integer_,
    AvgChickMass = NA_real_,
    NumberChicksMass = NA_integer_,
    AvgTarsus = NA_real_,
    AvgChicksTarsus = NA_integer_,
    OriginalTarsusMethod = NA_character_,
    ExperimentID = NA_character_,
    CheckID = NA_character_
  )


  # Capture data
  Capture_data <- tibble::tibble(
    Row = NA_integer_,
    IndvID = NA_character_,
    Species = NA_character_,
    Sex_observed = NA_character_,
    BreedingSeason = NA_integer_,
    CaptureDate = as.Date(NA_character_),
    CaptureTime = NA_character_,
    ObserverID = NA_character_,
    LocationID = NA_character_,
    CaptureAlive = NA,
    ReleaseAlive = NA,
    CapturePopID = NA_character_,
    CapturePlot = NA_character_,
    ReleasePopID = NA_character_,
    ReleasePlot = NA_character_,
    Mass = NA_real_,
    Tarsus = NA_real_,
    OriginalTarsusMethod = NA_character_,
    WingLength = NA_real_,
    Age_observed = NA_real_,
    Age_calculated = NA_integer_,
    ChickAge = NA_integer_,
    ExperimentID = NA_character_,
    CheckID = NA_character_
  )


  # Individual data
  Individual_data <- tibble::tibble(
    Row = NA_integer_,
    IndvID = NA_character_,
    Species = NA_character_,
    PopID = NA_character_,
    BroodIDLaid = NA_character_,
    BroodIDFledged = NA_character_,
    RingSeason = NA_integer_,
    RingAge = NA_character_,
    Sex_calculated = NA_character_,
    Sex_genetic = NA_character_,
    CheckID = NA_character_
  )


  # Location data
  Location_data <- tibble::tibble(
    Row = NA_integer_,
    LocationID = NA_character_,
    NestboxID = NA_character_,
    LocationType = NA_character_,
    PopID = NA_character_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    StartSeason = NA_integer_,
    EndSeason = NA_integer_,
    HabitatType = NA_character_,
    CheckID = NA_character_
  )


  # Add rows in which single checks can be validated
  # For each check we add rows with biologically probable values and
  # rows with biologically improbable values that violate the check

  # B2: Comparing clutch and brood sizes ####
  B2_rows <- Brood_data %>%
    dplyr::mutate( # Probable, non-manipulated brood
      ClutchSize_observed = as.integer(8),
      BroodSize_observed = as.integer(7)) %>%
    dplyr::add_row( # Impossible, non-manipulated brood (error)
      ClutchSize_observed = as.integer(7),
      BroodSize_observed = as.integer(8)) %>%
    dplyr::add_row( # Probable, manipulated brood
      ClutchSize_observed = as.integer(8),
      BroodSize_observed = as.integer(7),
      ExperimentID = "COHORT") %>%
    dplyr::add_row( # Improbable, manipulated brood (warning)
      ClutchSize_observed = as.integer(7),
      BroodSize_observed = as.integer(8),
      ExperimentID = "COHORT") %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq_len(n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B2"
    )

  # B3: Comparing brood sizes and fledgling numbers ####
  B3_rows <- Brood_data %>%
    dplyr::mutate( # Probable, non-manipulated brood
      BroodSize_observed = as.integer(6),
      NumberFledged_observed = as.integer(5)) %>%
    dplyr::add_row( # Impossible, non-manipulated brood (error)
      BroodSize_observed = as.integer(5),
      NumberFledged_observed = as.integer(6)) %>%
    dplyr::add_row( # Probable, manipulated brood
      BroodSize_observed = as.integer(6),
      NumberFledged_observed = as.integer(5),
      ExperimentID = "COHORT") %>%
    dplyr::add_row( # Improbable, manipulated brood (warning)
      BroodSize_observed = as.integer(5),
      NumberFledged_observed = as.integer(6),
      ExperimentID = "COHORT") %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B2_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B3"
    )

  # B4: Comparing laying and hatching dates ####
  B4_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      LayDate_observed = as.Date("2020-04-04"),
      HatchDate_observed = as.Date("2020-04-18")
    ) %>%
    dplyr::add_row( # Impossible (error)
      LayDate_observed = as.Date("2020-04-18"),
      HatchDate_observed = as.Date("2020-04-04")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B3_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B4"
    )

  # B5: Comparing hatching and fledging dates ####
  B5_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      HatchDate_observed = as.Date("2020-04-18"),
      FledgeDate_observed = as.Date("2020-05-01")
    ) %>%
    dplyr::add_row( # Impossible (error)
      HatchDate_observed = as.Date("2020-05-01"),
      FledgeDate_observed = as.Date("2020-04-18")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B4_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B5"
    )

  # B6a: Checking clutch size values against reference values ####
  B6a_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      ClutchSize_observed = 10) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      ClutchSize_observed = round(rnorm(150, 9, 2))) %>%
    dplyr::add_row( # Improbable (warning)
      ClutchSize_observed = 30) %>%
    dplyr::add_row( # Impossible (error)
      ClutchSize_observed = -10) %>%
    dplyr::add_row( # Impossible (error)
      ClutchSize_observed = 100) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B5_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B6a"
    )

  # B6b: Checking brood size values against reference values ####
  B6b_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      BroodSize_observed = 9) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      BroodSize_observed = round(rnorm(150, 8, 2))) %>%
    dplyr::add_row( # Improbable (warning)
      BroodSize_observed = 30) %>%
    dplyr::add_row( # Impossible (error)
      BroodSize_observed = -10) %>%
    dplyr::add_row( # Impossible (error)
      BroodSize_observed = 100) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B6a_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B6b"
    )

  # B6c: Checking fledgling number values against reference values ####
  B6c_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      NumberFledged_observed = 9) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      NumberFledged_observed = round(rnorm(150, 8, 2))) %>%
    dplyr::add_row( # Improbable (warning)
      NumberFledged_observed = 30) %>%
    dplyr::add_row( # Impossible (error)
      NumberFledged_observed = -10) %>%
    dplyr::add_row( # Impossible (error)
      NumberFledged_observed = 100) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B6b_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B6c"
    )

  # B6d: Checking laying date values against reference values ####
  B6d_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      LayDate_observed = as.Date("2020-05-01")) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      LayDate_observed = sample(seq(as.Date("2020-04-01"), as.Date("2020-05-31"), by = "day"), 150, replace = TRUE)) %>%
    dplyr::add_row( # Improbable, too early (warning)
      LayDate_observed = as.Date("2020-03-01")) %>%
    dplyr::add_row( # Improbable, too late (warning)
      LayDate_observed = as.Date("2020-08-01")) %>%
    dplyr::add_row( # Impossible, year earlier (error)
      LayDate_observed = as.Date("2019-05-01")) %>%
    dplyr::add_row( # Impossible, year later (error)
      LayDate_observed = as.Date("2021-05-01")) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B6c_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B6d"
    )

  # B7: Comparing brood size and number of chicks captured ####
  B7_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      BroodSize_observed = as.integer(1)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodSize_observed = as.integer(2)
      ) %>%
    dplyr::add_row( # Impossible (error)
      BroodSize_observed = as.integer(1)
      ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B6d_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B7"
    )

  B7_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      BroodIDLaid = unique(B7_brood_rows$BroodID)[1]
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = unique(B7_brood_rows$BroodID)[2]
    ) %>%
    dplyr::add_row( # Impossible (error)
      BroodIDLaid = c(unique(B7_brood_rows$BroodID)[3], unique(B7_brood_rows$BroodID)[3])
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq_len(n()),
      IndvID = paste0("B", Row),
      CheckID = "B7"
    )

  B7_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = B7_indv_rows$IndvID,
      CapturePopID = B7_indv_rows$PopID,
      BreedingSeason = B7_indv_rows$RingSeason,
      Species = B7_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq_len(n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "B7"
    )

  # B8: Check that BroodIDs are unique ####
  B8_rows <- Brood_data %>%
    dplyr::mutate( # Unique
      Row = max(B7_brood_rows$Row) + 1,
      BroodID = paste0("AAA-2020-", Row)
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = max(B7_brood_rows$Row) + 2,
      BroodID = paste0("AAA-2020-", Row) # Same ID
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = max(B7_brood_rows$Row) + 3,
      BroodID = paste0("AAA-2020-", Row - 1) # Same ID
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B8"
    )

  # B9: Check that clutch type order is correct ####
  B9_rows <- Brood_data %>%
    dplyr::mutate( # Probable (first - second)
      ClutchType_calculated = "first"
    ) %>%
    dplyr::add_row(
      ClutchType_calculated = "second"
    ) %>%
    dplyr::add_row( # Probable (replacement - second)
      ClutchType_calculated = "replacement"
    ) %>%
    dplyr::add_row(
      ClutchType_calculated = "second"
    ) %>%
    dplyr::add_row( # Impossible (second - first)
      ClutchType_calculated = "second"
    ) %>%
    dplyr::add_row(
      ClutchType_calculated = "first"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq(max(B8_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", c(min(Row), min(Row), min(Row) + 1, min(Row) + 1, min(Row) + 2, min(Row) + 2)),
      CheckID = "B9"
    )

  # B10: Comparing parent species ####
  B10_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B9_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = max(B9_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B10"
    )

  B10_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      IndvID = B10_brood_rows$FemaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      IndvID = B10_brood_rows$MaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B10_brood_rows$FemaleID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B10_brood_rows$MaleID[2],
      Species = "CYACAE"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Row = seq(max(B7_indv_rows$Row) + 1, length.out = n()),
      RingAge = "adult",
      CheckID = "B10"
    )

  B10_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = B10_indv_rows$IndvID,
      CapturePopID = B10_indv_rows$PopID,
      BreedingSeason = B10_indv_rows$RingSeason,
      Species = B10_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(B7_capture_rows$Row) + 1, length.out = n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "B10"
    )

  # C2a: Checking mass values against reference values ####
  # - adults
  C2a_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Mass = as.integer(18)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Mass = rnorm(150, 16, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Mass = as.integer(30)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Mass = as.integer(150)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(B10_capture_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      Age_calculated = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C2a_adults"
    )

  # - chicks

  # First create dummy data from logistic model + noise
  chick_dummy_mass <- tibble::tibble(
    ID = as.character(1:(30*17)),
    ChickAge = rep(1:30, each = 17),
  ) %>%
    dplyr::group_by(ID) %>%
    dplyr::group_modify(~{

      a <- rnorm(1, mean = 25, sd = 7.5)
      b <- rnorm(1, mean = 5, sd = 1.5)
      c <- rnorm(1, mean = 0.15, sd = 0.04)

      .x %>%
        dplyr:: mutate(Mass =  a / (1 + b * (exp(-c * .x$ChickAge))))

    })

  # Plot dummy data + logistic growth curve
  # logistic_model <- nls(Mass ~ a / (1 + b * (exp(-c * ChickAge))), data = chick_dummy_mass,
  #                       start = list(a = 25, b = 5, c = 0.15), trace = TRUE)
  #
  # newdata <- data.frame(ChickAge = seq(0, max(chick_dummy_mass$ChickAge), by = 1))
  # logistic_pred <- tibble::tibble(fit = predict(logistic_model, newdata = newdata),
  #                                 x = newdata$ChickAge) %>%
  #   dplyr::mutate(upper = fit + summary(logistic_model)$sigma * qnorm(0.99, 0, 1),
  #                 lower = fit + summary(logistic_model)$sigma * qnorm(0.01, 0, 1))
  #
  # ggplot2::ggplot() +
  #   ggplot2::geom_jitter(data = chick_dummy_mass, ggplot2::aes(x = ChickAge, y = Mass), shape = 21, alpha = 0.4, width = 0.2) +
  #   ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = x, y = lower), colour = "darkred", lty = 2) +
  #   ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = x, y = upper), colour = "darkred", lty = 2) +
  #   ggplot2::geom_line(data = logistic_pred, ggplot2::aes(x = x, y = fit), size = 1, colour = "darkred") +
  #   ggplot2::theme_classic()

  C2a_chick_rows <- Capture_data %>%
    # ChickAge known
    dplyr::mutate( # Probable
      Mass = as.integer(12),
      ChickAge = 12,
      Age_calculated = 1
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Mass = as.integer(40),
      ChickAge = 12,
      Age_calculated = 1
    ) %>%
    dplyr::add_row( # Impossible (error)
      Mass = as.integer(150),
      ChickAge = 12,
      Age_calculated = 1
    ) %>%
    # ChickAge unknown, age_calculated == 3 (ChickAge becomes maximum ChickAge in population)
    dplyr::add_row( # Probable
      Mass = as.integer(20),
      Age_calculated = 3
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Mass = as.integer(45),
      Age_calculated = 3
    ) %>%
    dplyr::add_row( # Impossible (error)
      Mass = as.integer(200),
      Age_calculated = 3
    ) %>%
    # ChickAge unknown, age_calculated == 1 (ChickAge becomes 14)
    dplyr::add_row( # Probable
      Mass = as.integer(15),
      Age_calculated = 1
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Mass = as.integer(40),
      Age_calculated = 1
    ) %>%
    dplyr::add_row( # Impossible (error)
      Mass = as.integer(150),
      Age_calculated = 1
    ) %>%
    dplyr::add_row( # Add 510 rows so that reference values can be created
      Mass = chick_dummy_mass$Mass,
      ChickAge = chick_dummy_mass$ChickAge,
      Age_calculated = 1
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C2a_adult_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C2a_chicks"
    )

  # C2b: Checking tarsus values against reference values ####
  # - adults
  C2b_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Tarsus = as.integer(20)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Tarsus = rnorm(150, 19, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Tarsus = as.integer(45)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Tarsus = as.integer(175)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C2a_chick_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      Age_calculated = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C2b_adults"
    )

  # - chicks
  C2b_chick_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Tarsus = as.integer(17)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Tarsus = rnorm(150, 16, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Tarsus = as.integer(40)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Tarsus = as.integer(175)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C2b_adult_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      Age_calculated = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C2b_chicks"
    )

  # C3: Checking chick age values ####
  C3_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      ChickAge = 15
    ) %>%
    dplyr::add_row( # Impossible (error)
      ChickAge = -2
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C2b_chick_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      Age_calculated = 3,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-02",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C3"
    )

  # I2: Checking unique individual IDs ####
  I2_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      PopID = "AAA",
      ) %>%
    dplyr::add_row( # Improbable (warning)
      PopID = as.character(c("AAA", "BBB")),
      ) %>%
    dplyr::add_row( # Impossible (error)
      PopID = as.character(c("AAA", "AAA")),
      ) %>%
    dplyr::mutate(
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "M",
      Row = seq(max(B10_indv_rows$Row) + 1, length.out = n()),
      RingAge = "adult",
      BroodIDLaid = paste(PopID, RingSeason, Row, "I2", sep = "-"),
      BroodIDFledged = BroodIDLaid,
      IndvID = paste0("I2_", c(min(Row), min(Row) + 1, min(Row) + 1, min(Row) + 2, min(Row) + 2)),
      CheckID = "I2"
    )

  I2_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = I2_indv_rows$IndvID,
      CapturePopID = I2_indv_rows$PopID,
      BreedingSeason = I2_indv_rows$RingSeason,
      Species = I2_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(C3_rows$Row) + 1, length.out = n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "I2"
    )

  # I3: Checking that chicks have BroodIDs ####
  I3_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = max(I2_indv_rows$Row) + 1,
      BroodIDLaid = "AAA-2020-14",
      BroodIDFledged = "AAA-2020-14"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = max(I2_indv_rows$Row) + 2
      ) %>%
    dplyr::mutate(
      IndvID = paste0("I3_", Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      RingAge = "chick",
      Species = "PARMAJ",
      Sex_calculated = "F",
      CheckID = "I3",
    )

  I3_capture_rows <- Capture_data %>%
    dplyr::add_row(
      IndvID = I3_indv_rows$IndvID, # 1: probable, 2: impossible (error)
      CapturePopID = I3_indv_rows$PopID,
      BreedingSeason = I3_indv_rows$RingSeason,
      Species = I3_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(I2_capture_rows$Row) + 1, length.out = n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, "NB", "001", sep="_"),
      CheckID = "I3"
    )

  I3_location_rows <- Location_data %>%
    dplyr::mutate(
      Row = as.integer(1),
      PopID = "AAA",
      LocationType = "NB",
      LocationID = paste(PopID, LocationType, "001", sep="_"),
      NestboxID = paste(PopID, LocationType, "001", sep="_"),
      StartSeason = 2019,
      CheckID = "I3"
    )

  # I4: Checking that individuals have no conflicting sex ####
  I4_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Sex_calculated  = "F"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Sex_calculated  = "C"
    ) %>%
    dplyr::mutate(
      Row = seq(max(I3_indv_rows$Row) + 1, length.out = n()),
      IndvID = paste0("I4_", Row),
      RingAge = "adult",
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      CheckID = "I4"
    )

  I4_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = I4_indv_rows$IndvID,
      CapturePopID = I4_indv_rows$PopID,
      BreedingSeason = I4_indv_rows$RingSeason,
      Species = I4_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(I3_capture_rows$Row) + 1, length.out = n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "I4"
    )

  # I5: Checking that individuals have no conflicting species ####
  I5_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Species = "CCCCCC"
    ) %>%
    dplyr::mutate(
      Row = seq(max(I4_indv_rows$Row) + 1, length.out = n()),
      IndvID = paste0("I", Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Sex_calculated = "M",
      CheckID = "I5"
    )

  I5_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = I5_indv_rows$IndvID,
      CapturePopID = I5_indv_rows$PopID,
      BreedingSeason = I5_indv_rows$RingSeason,
      Species = I5_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(I4_capture_rows$Row) + 1, length.out = n()),
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "I5"
    )

  # I6: Checking that individuals in Individual_data also appear in Capture_data ####
  I6_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = max(I5_indv_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Impossible (missing from Capture_data)
      Row = max(I5_indv_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      IndvID = paste0("I", Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "M",
      CheckID = "I6"
    )

  I6_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = max(I5_capture_rows$Row) + 1,
      IndvID = I6_indv_rows$IndvID[1],
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "I6"
    )

  # C4: Checking that adults caught on nest are listed are the parents ####
  C4_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      LocationID = "AAA_NB_002"
    ) %>%
    dplyr::add_row( # Probable (not caught on nest)
      LocationID = "AAA_MN_001"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      LocationID = "AAA_NB_003"
    ) %>%
    dplyr::mutate(
      Age_calculated = 5,
      Sex_observed = "F",
      Row = seq(max(I6_capture_rows$Row) + 1, length.out = n()),
      IndvID = paste0("I", Row),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-08-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      CheckID = "C4"
    )

  C4_location_rows <- Location_data %>%
    dplyr::mutate( # Probable
      LocationID = "AAA_NB_002",
      LocationType = "NB",
      NestboxID = LocationID
    ) %>%
    dplyr::add_row( # Probable (not caught on nest)
      LocationID = "AAA_MN_001",
      LocationType = "MN"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      LocationID = "AAA_NB_003",
      LocationType = "NB",
      NestboxID = LocationID
    ) %>%
    dplyr::mutate(
      Row = seq(max(I3_location_rows$Row) + 1, length.out = n()),
      PopID = "AAA",
      StartSeason = 2019,
      EndSeason = NA,
      CheckID = "C4"
    )

  C4_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      FemaleID = C4_capture_rows$IndvID[1],
      LocationID = "AAA_NB_002"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      FemaleID = "I01",
      LocationID = "AAA_NB_003"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = 2020,
      Species = "PARMAJ",
      Row = seq(max(B10_brood_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      CheckID = "C4"
    )

  # B11: Comparing species of brood and of parents ####
  B11_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Species = "CYACAE"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Row = seq(max(C4_brood_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "B11"
    )

  B11_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      IndvID = B11_brood_rows$FemaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      IndvID = B11_brood_rows$MaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B11_brood_rows$FemaleID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B11_brood_rows$MaleID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::mutate(
      Row = seq(max(I6_indv_rows$Row) + 1, length.out = n()),
      PopID = "AAA",
      RingAge = "adult",
      CheckID = "B11"
    )

  # B12: Comparing species of brood and of chicks ####
  B12_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Species = "CYACAE"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Row = seq(max(B11_brood_rows$Row) + 1, length.out = n()),
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      CheckID = "B12"
    )

  B12_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      BroodIDLaid = B12_brood_rows$BroodID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      BroodIDLaid = B12_brood_rows$BroodID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = B12_brood_rows$BroodID[2],
      Species = "CYACAE"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = B12_brood_rows$BroodID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::mutate(
      Row = seq(max(B11_indv_rows$Row) + 1, length.out = n()),
      IndvID = paste0("C", Row),
      PopID = "AAA",
      RingAge = "chick",
      BroodIDFledged = BroodIDLaid,
      CheckID = "B12"
    )

  # B13: Checking sex of parents ####
  B13_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B12_brood_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = max(B12_brood_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("K", Row),
      MaleID = paste0("L", Row),
      CheckID = "B13"
    )

  B13_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      IndvID = B13_brood_rows$FemaleID[1],
      Sex_calculated = "F"
    ) %>%
    dplyr::add_row( # Probable
      IndvID = B13_brood_rows$MaleID[1],
      Sex_calculated = "M"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B13_brood_rows$FemaleID[2],
      Sex_calculated = "F"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B13_brood_rows$MaleID[2],
      Sex_calculated = "F"
    ) %>%
    dplyr::mutate(
      Row = seq(max(B12_indv_rows$Row) + 1, length.out = n()),
      PopID = "AAA",
      Species = "PARMAJ",
      RingAge = "adult",
      CheckID = "B13"
    )

  # Approved_list: make sure that our approve-listing procedure works ####
  # We create a record that violates check B4, but should NOT result in TRUE in Warning & Error columns
  al_rows <- Brood_data %>%
    dplyr::mutate(
      Row = as.integer(0),
      LayDate_observed = as.Date("2020-06-24"),
      HatchDate_observed = as.Date("2020-06-11")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row),
      CheckID = "Approved list"
    )

  # Combine single check rows per dataframe
  Brood_data <- dplyr::bind_rows(al_rows, B2_rows, B3_rows, B4_rows, B5_rows, B6a_rows, B6b_rows,
                                 B6c_rows, B6d_rows, B7_brood_rows, B8_rows, B9_rows, B10_brood_rows,
                                 C4_brood_rows, B11_brood_rows, B12_brood_rows, B13_brood_rows)
  Capture_data <- dplyr::bind_rows(B7_capture_rows, B10_capture_rows, C2a_adult_rows, C2a_chick_rows,
                                   C2b_adult_rows, C2b_chick_rows,C3_rows, I2_capture_rows,
                                   I3_capture_rows, I4_capture_rows, I5_capture_rows, I6_capture_rows, C4_capture_rows)
  Individual_data <- dplyr::bind_rows(B7_indv_rows, B10_indv_rows, I2_indv_rows, I3_indv_rows, I4_indv_rows,
                                      I5_indv_rows, I6_indv_rows, B11_indv_rows, B12_indv_rows, B13_indv_rows)
  Location_data <- dplyr::bind_rows(I3_location_rows, C4_location_rows)

  # Check whether row numbers are unique
  if(any(duplicated(Brood_data$Row), duplicated(Capture_data$Row), duplicated(Individual_data$Row), duplicated(Location_data$Row))) {

    stop("One or more dummy dataframes have duplicated Row IDs. Make them unique.")

  }

  # Check whether all CheckIDs are filled in
  if(any(is.na(Brood_data$CheckID), is.na(Capture_data$CheckID), is.na(Individual_data$CheckID), is.na(Location_data$CheckID))) {

    stop("One or more dummy dataframes have rows in which the CheckID is not given. Provide the CheckID to those rows.")

  }

  # Combine in list
  dummy_data <- list(Brood_data = Brood_data,
                     Capture_data = Capture_data,
                     Individual_data = Individual_data,
                     Location_data = Location_data)

  usethis::use_data(dummy_data, overwrite=overwrite)

}
