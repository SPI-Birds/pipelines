#' Create quality check dummy data
#'
#' @description Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}. In each dataframe, rows are created to test single checks from \code{\link{quality_check}}. A CheckID column is added to each dataframe to mark which rows serve to test each check. See a detailed description of the dummy data in \code{\link{dummy_data}}.
#'
#' @return
#' List of 4 dataframes:
#' \item{Brood_data}{Dummy brood data.}
#' \item{Capture_data}{Dummy capture data.}
#' \item{Individual_data}{Dummy individual data.}
#' \item{Location_data}{Dummy location data.}
#'
#' @export

create_dummy_data <- function() {

  # Set seed for dummy data drawn from distributions
  set.seed(74)

  # Create skeletons for each pipeline data frame
  # Brood data
  Brood_data <- brood_data_template[1,] %>%
    dplyr::mutate(dplyr::across(.cols = where(is.character), .fns = ~ NA_character_),
                  dplyr::across(.cols = where(is.integer), .fns = ~ NA_integer_),
                  dplyr::across(.cols = where(~ class(.x) == "Date"), .fns = ~ as.Date(NA_character_)),
                  Row = NA_integer_) %>%
    dplyr::select(Row, dplyr::everything())


  # Capture data
  Capture_data <- capture_data_template[1,] %>%
    dplyr::mutate(dplyr::across(.cols = where(is.character), .fns = ~ NA_character_),
                  dplyr::across(.cols = where(is.integer), .fns = ~ NA_integer_),
                  dplyr::across(.cols = where(~ class(.x) == "Date"), .fns = ~ as.Date(NA_character_)),
                  Row = NA_integer_) %>%
    dplyr::select(Row, dplyr::everything())


  # Individual data
  Individual_data <- individual_data_template[1,] %>%
    dplyr::mutate(dplyr::across(.cols = where(is.character), .fns = ~ NA_character_),
                  dplyr::across(.cols = where(is.integer), .fns = ~ NA_integer_),
                  Row = NA_integer_) %>%
    dplyr::select(Row, dplyr::everything())


  # Location data
  Location_data <- location_data_template[1,] %>%
    dplyr::mutate(dplyr::across(.cols = where(is.character), .fns = ~ NA_character_),
                  dplyr::across(.cols = where(is.integer), .fns = ~ NA_integer_),
                  dplyr::across(.cols = where(is.numeric), .fns = ~ NA_real_),
                  Row = NA_integer_) %>%
    dplyr::select(Row, dplyr::everything())


  # Add rows in which single checks can be validated
  # For each check we add rows with biologically probable values and
  # rows with biologically improbable values that violate the check

  # B1: Comparing clutch and brood sizes ####
  B1_rows <- Brood_data %>%
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
      Row = seq_len(dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B1"
    )

  # B2: Comparing brood sizes and fledgling numbers ####
  B2_rows <- Brood_data %>%
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
      Row = seq(max(B1_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B2"
    )

  # B3: Comparing laying and hatching dates ####
  B3_rows <- Brood_data %>%
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
      Row = seq(max(B2_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B3"
    )

  # B4: Comparing hatching and fledging dates ####
  B4_rows <- Brood_data %>%
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
      Row = seq(max(B3_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B4"
    )

  # B5a: Checking clutch size values against reference values ####
  B5a_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      ClutchSize_observed = 10) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      ClutchSize_observed = round(stats::rnorm(150, 9, 2))) %>%
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
      Row = seq(max(B4_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B5a"
    )

  # B5b: Checking brood size values against reference values ####
  B5b_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      BroodSize_observed = 9) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      BroodSize_observed = round(stats::rnorm(150, 8, 2))) %>%
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
      Row = seq(max(B5a_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B5b"
    )

  # B5c: Checking fledgling number values against reference values ####
  B5c_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      NumberFledged_observed = 9) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      NumberFledged_observed = round(stats::rnorm(150, 8, 2))) %>%
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
      Row = seq(max(B5b_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B5c"
    )

  # B5d: Checking laying date values against reference values ####
  B5d_rows <- Brood_data %>%
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
      Row = seq(max(B5c_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B5d"
    )

  # B6: Comparing brood size and number of chicks in Individual_data ####
  B6_brood_rows <- Brood_data %>%
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
      Row = seq(max(B5d_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B6"
    )

  B6_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      BroodIDLaid = unique(B6_brood_rows$BroodID)[1]
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = unique(B6_brood_rows$BroodID)[2]
    ) %>%
    dplyr::add_row( # Impossible (error)
      BroodIDLaid = c(unique(B6_brood_rows$BroodID)[3], unique(B6_brood_rows$BroodID)[3])
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Row = seq_len(dplyr::n()),
      IndvID = paste0("B", .data$Row),
      CheckID = "B6"
    )

  B6_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = B6_indv_rows$IndvID,
      CapturePopID = B6_indv_rows$PopID,
      BreedingSeason = B6_indv_rows$RingSeason,
      Species = B6_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(.data$BreedingSeason, "06", "01", sep = "-"),
      Row = seq_len(dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = .data$CaptureID,
      CheckID = "B6"
    )

  # B7: Check that BroodIDs are unique ####
  B7_rows <- Brood_data %>%
    dplyr::mutate( # Unique
      Row = max(B6_brood_rows$Row) + 1,
      BroodID = paste0("AAA-2020-", .data$Row)
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = max(B6_brood_rows$Row) + 2,
      BroodID = paste0("AAA-2020-", .data$Row) # Same ID
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = max(B6_brood_rows$Row) + 3,
      BroodID = paste0("AAA-2020-", .data$Row - 1) # Same ID
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CheckID = "B7"
    )

  # B8: Check that clutch type order is correct ####
  B8_rows <- Brood_data %>%
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
      Row = seq(max(B7_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      FemaleID = paste0("F", c(min(.data$Row), min(.data$Row), min(.data$Row) + 1, min(.data$Row) + 1, min(.data$Row) + 2, min(.data$Row) + 2)),
      CheckID = "B8"
    )

  B8_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = B8_rows$FemaleID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B8_rows$FemaleID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B6_indv_rows$Row) + 1, length.out = dplyr::n()),
      Species = "PARMAJ",
      PopID = "AAA",
      RingSeason = as.integer(2019),
      CheckID = "B8"
    )

  B8_capture_rows <- Capture_data %>%
    dplyr::mutate(
      IndvID = B8_rows$FemaleID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B8_rows$FemaleID[-1]
    ) %>%
    dplyr::mutate(
      Sex_observed = "F",
      Species = "PARMAJ",
      Row = seq(max(B6_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-04",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B8"
    )

  # B9: Comparing parent species ####
  B9_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B8_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = max(B8_rows$Row) + 2
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = max(B8_rows$Row) + 3
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      FemaleID = paste0("F", .data$Row),
      MaleID = paste0("M", .data$Row),
      CheckID = "B9"
    )

  B9_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      IndvID = B9_brood_rows$FemaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      IndvID = B9_brood_rows$MaleID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B9_brood_rows$FemaleID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = B9_brood_rows$MaleID[2],
      Species = "CYACAE"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B9_brood_rows$FemaleID[3],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B9_brood_rows$MaleID[3],
      Species = "POEPAL"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Row = seq(max(B8_indv_rows$Row) + 1, length.out = dplyr::n()),
      RingAge = "adult",
      CheckID = "B9"
    )

  B9_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = B9_indv_rows$IndvID,
      CapturePopID = B9_indv_rows$PopID,
      BreedingSeason = B9_indv_rows$RingSeason,
      Species = B9_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(.data$BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(B8_capture_rows$Row) + 1, length.out = dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = .data$CaptureID,
      CheckID = "B9"
    )

  # C1a: Checking mass values against reference values ####
  # - adults
  C1a_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Mass = as.integer(18)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Mass = stats::rnorm(150, 16, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Mass = as.integer(30)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Mass = as.integer(150)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(B9_capture_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("C", .data$Row),
      Age_calculated = 5,
      Age_observed = .data$Age_calculated,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C1a_adults"
    )

  # - chicks

  # First create dummy data from logistic model + noise
  chick_dummy_mass <- tibble::tibble(
    ID = as.character(1:(30*17)),
    ChickAge = rep(1:30, each = 17),
  ) %>%
    dplyr::group_by(ID) %>%
    dplyr::group_modify(~{

      a <- stats::rnorm(1, mean = 25, sd = 7.5)
      b <- stats::rnorm(1, mean = 5, sd = 1.5)
      c <- stats::rnorm(1, mean = 0.15, sd = 0.04)

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

  C1a_chick_rows <- Capture_data %>%
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
      Row = seq(max(C1a_adult_rows$Row) + 1, length.out = dplyr::n()),
      Age_observed = .data$Age_calculated,
      IndvID = paste0("C", .data$Row),
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C1a_chicks"
    )

  # C1b: Checking tarsus values against reference values ####
  # - adults
  C1b_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Tarsus = as.integer(20)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Tarsus = stats::rnorm(150, 19, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Tarsus = as.integer(45)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Tarsus = as.integer(175)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C1a_chick_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("C", .data$Row),
      Age_calculated = 5,
      Age_observed = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C1b_adults"
    )

  # - chicks
  C1b_chick_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Tarsus = as.integer(17)
    ) %>%
    dplyr::add_row( # Add 150 rows so that reference values can be calculated
      Tarsus = stats::rnorm(150, 16, 3)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Tarsus = as.integer(40)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Tarsus = as.integer(175)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C1b_adult_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("C", .data$Row),
      Age_calculated = 1,
      Age_observed = 1,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C1b_chicks"
    )

  C1_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = C1a_adult_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = c(C1a_adult_rows$IndvID[-1],
                 C1a_chick_rows$IndvID,
                 C1b_adult_rows$IndvID,
                 C1b_chick_rows$IndvID)
    ) %>%
    dplyr::mutate(
      Row = seq(max(B9_indv_rows$Row) + 1, length.out = dplyr::n()),
      Species = "PARMAJ",
      PopID = "AAA",
      CheckID = "C1"
    )

  # C2: Checking chick age values ####
  C2_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      ChickAge = 15
    ) %>%
    dplyr::add_row( # Impossible (error)
      ChickAge = -2
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      Row = seq(max(C1b_chick_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("C", .data$Row),
      Age_calculated = 3,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-02",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C2"
    )

  C2_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = C2_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = C2_rows$IndvID[2]
    ) %>%
    dplyr::mutate(
      Row = seq(max(C1_indv_rows$Row) + 1, length.out = dplyr::n()),
      Species = "PARMAJ",
      PopID = "AAA",
      CheckID = "C2"
    )

  # I1: Checking unique individual IDs ####
  I1_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      PopID = "AAA",
      ) %>%
    dplyr::add_row( # Impossible (error)
      PopID = as.character(c("AAA", "AAA")),
      ) %>%
    dplyr::mutate(
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "M",
      Row = seq(max(C2_indv_rows$Row) + 1, length.out = dplyr::n()),
      RingAge = "adult",
      BroodIDLaid = paste(.data$PopID, .data$RingSeason, .data$Row, "I2", sep = "-"),
      BroodIDFledged = .data$BroodIDLaid,
      IndvID = paste0("I1_", c(min(.data$Row), min(.data$Row) + 1, min(.data$Row) + 1)),
      CheckID = "I1"
    )

  I1_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = I1_indv_rows$IndvID,
      CapturePopID = I1_indv_rows$PopID,
      BreedingSeason = I1_indv_rows$RingSeason,
      Species = I1_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(.data$BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(C2_rows$Row) + 1, length.out = dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = .data$CaptureID,
      CheckID = "I1"
    )

  # I2: Checking that chicks have BroodIDs ####
  I2_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = max(I1_indv_rows$Row) + 1,
      BroodIDLaid = "AAA-2020-14",
      BroodIDFledged = "AAA-2020-14"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = max(I1_indv_rows$Row) + 2
      ) %>%
    dplyr::mutate(
      IndvID = paste0("I2_", .data$Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      RingAge = "chick",
      Species = "PARMAJ",
      Sex_calculated = "F",
      CheckID = "I2",
    )

  I2_capture_rows <- Capture_data %>%
    dplyr::add_row(
      IndvID = I2_indv_rows$IndvID, # 1: probable, 2: impossible (error)
      CapturePopID = I2_indv_rows$PopID,
      BreedingSeason = I2_indv_rows$RingSeason,
      Species = I2_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(.data$BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(I1_capture_rows$Row) + 1, length.out = dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = .data$CaptureID,
      Age_observed = 1,
      Age_calculated = 1,
      CheckID = "I2"
    )

  I2_location_rows <- Location_data %>%
    dplyr::mutate(
      LocationID = unique(I2_capture_rows$LocationID)[1]
    ) %>%
    dplyr::add_row(
      LocationID = unique(I2_capture_rows$LocationID)[2]
    ) %>%
    dplyr::mutate(
      Row = seq_len(dplyr::n()),
      PopID = "AAA",
      LocationType = "NB",
      NestboxID = .data$LocationID,
      StartSeason = 2019,
      EndSeason = NA,
      CheckID = "I2"
    )

  # I3: Checking that individuals have no conflicting sex ####
  I3_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Sex_calculated  = "F"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Sex_calculated  = "C"
    ) %>%
    dplyr::mutate(
      Row = seq(max(I2_indv_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("I3_", .data$Row),
      RingAge = "adult",
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      CheckID = "I3"
    )

  I3_capture_rows <- Capture_data %>% # All individuals in Individual_data should be part of Capture_data
    dplyr::add_row(
      IndvID = I3_indv_rows$IndvID,
      CapturePopID = I3_indv_rows$PopID,
      BreedingSeason = I3_indv_rows$RingSeason,
      Species = I3_indv_rows$Species
    ) %>%
    tidyr::drop_na(IndvID) %>%
    dplyr::mutate(
      CaptureDate = paste(.data$BreedingSeason, "06", "01", sep = "-"),
      Row = seq(max(I2_capture_rows$Row) + 1, length.out = dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = .data$CaptureID,
      CheckID = "I3"
    )

  # I4: Checking that individuals have no conflicting species ####
  I4_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Species = "CCCCCC"
    ) %>%
    dplyr::mutate(
      Row = seq(max(I3_indv_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("I", .data$Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Sex_calculated = "M",
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
      Row = seq(max(I3_capture_rows$Row) + 1, length.out = dplyr::n()),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep="_"),
      LocationID = .data$CaptureID,
      CheckID = "I4"
    )

  # I5: Checking that individuals in Individual_data also appear in Capture_data ####
  I5_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = max(I4_indv_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Impossible (missing from Capture_data)
      Row = max(I4_indv_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      IndvID = paste0("I", .data$Row),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "M",
      CheckID = "I5"
    )

  I5_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = max(I4_capture_rows$Row) + 1,
      IndvID = I5_indv_rows$IndvID[1],
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "I5"
    )

  # C3: Checking that adults caught on nest are listed are the parents ####
  C3_capture_rows <- Capture_data %>%
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
      Row = seq(max(I5_capture_rows$Row) + 2, length.out = dplyr::n()), # +2 because I5's second dummy record should be missing
      IndvID = paste0("I", .data$Row),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C3"
    )

  C3_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = C3_capture_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = C3_capture_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(I5_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      Species = "PARMAJ",
      Sex_calculated = "F",
      RingSeason = as.integer(2019),
      CheckID = "C3"
    )

  C3_location_rows <- Location_data %>%
    dplyr::mutate( # Probable
      LocationID = "AAA_NB_002",
      LocationType = "NB",
      NestboxID = .data$LocationID
    ) %>%
    dplyr::add_row( # Probable (not caught on nest)
      LocationID = "AAA_MN_001",
      LocationType = "MN"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      LocationID = "AAA_NB_003",
      LocationType = "NB",
      NestboxID = .data$LocationID
    ) %>%
    dplyr::mutate(
      Row = seq(max(I2_location_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      StartSeason = 2019,
      EndSeason = NA,
      CheckID = "C3"
    )

  C3_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      FemaleID = C3_capture_rows$IndvID[1],
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
      Row = seq(max(B9_brood_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "C3",
      LayDate_observed = as.Date("2020-05-31"),
      FledgeDate_observed = as.Date("2020-07-01")
    )

  # B10: Comparing species of brood and of parents ####
  B10_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Species = "CYACAE"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Species = "POEPAL"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Row = seq(max(C3_brood_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      FemaleID = paste0("F", .data$Row),
      MaleID = paste0("M", .data$Row),
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
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B10_brood_rows$FemaleID[3],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B10_brood_rows$MaleID[3],
      Species = "PARMAJ"
    ) %>%
    dplyr::mutate(
      Row = seq(max(C3_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      RingAge = "adult",
      CheckID = "B10"
    )

  B10_capture_rows <- Capture_data %>%
    dplyr::mutate(
      IndvID = B10_indv_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B10_indv_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(C3_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-04",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B10"
    )

  # B11: Comparing species of brood and of chicks ####
  B11_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Species = "CYACAE"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Species = "PARMAJ"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Row = seq(max(B10_brood_rows$Row) + 1, length.out = dplyr::n()),
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B11"
    )

  B11_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      BroodIDLaid = B11_brood_rows$BroodID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      BroodIDLaid = B11_brood_rows$BroodID[1],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = B11_brood_rows$BroodID[2],
      Species = "CYACAE"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      BroodIDLaid = B11_brood_rows$BroodID[2],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      BroodIDLaid = B11_brood_rows$BroodID[3],
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      BroodIDLaid = B11_brood_rows$BroodID[3],
      Species = "POEPAL"
    ) %>%
    dplyr::mutate(
      Row = seq(max(B10_indv_rows$Row) + 1, length.out = dplyr::n()),
      IndvID = paste0("C", .data$Row),
      PopID = "AAA",
      RingAge = "chick",
      BroodIDFledged = .data$BroodIDLaid,
      CheckID = "B11"
    )

  B11_capture_rows <- Capture_data %>%
    dplyr::mutate(
      IndvID = B11_indv_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B11_indv_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B10_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-04",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B11"
    )

  # B12: Checking sex of mothers ####
  B12_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B11_brood_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = max(B11_brood_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      FemaleID = paste0("K", .data$Row),
      MaleID = paste0("L", .data$Row),
      CheckID = "B12"
    )

  B12_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      IndvID = B12_brood_rows$FemaleID[1],
      Sex_calculated = "F"
    ) %>%
    dplyr::add_row( # Probable
      IndvID = B12_brood_rows$MaleID[1],
      Sex_calculated = "M"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B12_brood_rows$FemaleID[2],
      Sex_calculated = "M"
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = B12_brood_rows$MaleID[2],
      Sex_calculated = "M"
    ) %>%
    dplyr::mutate(
      Row = seq(max(B11_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      Species = "PARMAJ",
      RingAge = "adult",
      CheckID = "B12"
    )

  B12_capture_rows <- Capture_data %>%
    dplyr::mutate(
      IndvID = B12_indv_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B12_indv_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B11_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-04",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B12"
    )

  # B13: Checking sex of fathers ####
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
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      FemaleID = paste0("K", .data$Row),
      MaleID = paste0("L", .data$Row),
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
      Row = seq(max(B12_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      Species = "PARMAJ",
      RingAge = "adult",
      CheckID = "B13"
    )

  B13_capture_rows <- Capture_data %>%
    dplyr::mutate(
      IndvID = B13_indv_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = B13_indv_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B12_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-04",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B13"
    )

  # C4: Checking that the age of subsequent captures is ordered correctly ####
  C4_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable (different year)
      IndvID = paste0("I", max(B13_capture_rows$Row) + 1),
      Age_observed = 5,
      CaptureDate = "2019-05-01"
    ) %>%
    dplyr::add_row( # Probable (different year)
      IndvID = paste0("I", max(B13_capture_rows$Row) + 1),
      Age_observed = 7,
      CaptureDate = "2020-05-01"
    ) %>%
    dplyr::add_row( # Probable (same year)
      IndvID = rep(paste0("I", max(B13_capture_rows$Row) + 2), 2),
      Age_observed = c(5, 5),
      CaptureDate = c("2020-05-01", "2020-05-17")
    ) %>%
    dplyr::add_row( # Improbable (warning)
      IndvID = rep(paste0("I", max(B13_capture_rows$Row) + 3), 2),
      Age_observed = c(7, 6),
      CaptureDate = c("2019-05-01", "2020-05-01")
    ) %>%
    dplyr::add_row( # Impossible (error)
      IndvID = rep(paste0("I", max(B13_capture_rows$Row) + 4), 2),
      Age_observed = c(5, 1),
      CaptureDate = c("2019-05-01", "2020-05-01")
    ) %>%
    dplyr::mutate(
      Sex_observed = "F",
      Row = seq(max(B13_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(stringr::str_sub(.data$CaptureDate, 1, 4)),
      Species = "PARMAJ",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C4"
    )

  C4_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = unique(C4_capture_rows$IndvID)[1]
    ) %>%
    dplyr::add_row(
      IndvID = unique(C4_capture_rows$IndvID)[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B13_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      RingSeason = as.integer(2018),
      Species = "PARMAJ",
      Sex_calculated = "F",
      CheckID = "C4"
    )

  # L1: Checking capture location coordinates ####
  L1_rows <- Location_data %>%
    dplyr::mutate( # Probable
      Longitude = 10.33485,
      Latitude = 63.43735
    ) %>%
    dplyr::add_row( # Probable
      Longitude = stats::rnorm(n = 100, mean = 10.33485, sd = 0.001),
      Latitude = stats::rnorm(n = 100, mean = 63.43735, sd = 0.001)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Longitude = 10.29036,
      Latitude = 63.17768
    ) %>%
    dplyr::mutate(
      Row = seq(max(C3_location_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      StartSeason = 2019,
      EndSeason = NA,
      LocationType = "NB",
      LocationID = paste0("AAA_NB_", .data$Row),
      NestboxID = .data$LocationID,
      CheckID = "L1"
    )

  L1_capture_rows <- Capture_data %>%
    dplyr::mutate(
      LocationID = L1_rows$LocationID[1]
    ) %>%
    dplyr::add_row(
      LocationID = L1_rows$LocationID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(C4_capture_rows$Row) + 1, length.out = dplyr::n()),
      Age_observed = 5,
      IndvID = paste0("I", .data$Row),
      CapturePopID = "AAA",
      Species = "PARMAJ",
      CaptureDate = "2020-05-15",
      BreedingSeason = as.integer(stringr::str_sub(.data$CaptureDate, 1, 4)),
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      Sex_observed = "F",
      CheckID = "L1"
    )

  L1_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = L1_capture_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = L1_capture_rows$IndvID[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(C4_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      Species = "PARMAJ",
      Sex_calculated = "F",
      RingSeason = as.integer(2020),
      CheckID = "L1"
    )

  # C5: Checking that individuals in Capture_data also appear in Individual_data ####
  C5_capture_rows <- Capture_data %>%
    dplyr::mutate( # Present in Individual_data
      Row = max(L1_capture_rows$Row) + 1,
    ) %>%
    dplyr::add_row( # Missing from Individual_data
      Row = max(L1_capture_rows$Row) + 2,
    ) %>%
    dplyr::mutate( # Probable
      IndvID = paste0("I", Row),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "C5"
    )

  C5_indv_rows <- Individual_data %>%
    dplyr::mutate(
      Row = max(L1_indv_rows$Row) + 1,
      IndvID = paste0("I", max(L1_capture_rows$Row) + 1),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "F",
      CheckID = "C5"
    )

  # B14: Checking that parents appear in Capture_data ####
  B14_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B13_brood_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Mother missing from Capture_data
      Row = max(B13_brood_rows$Row) + 2
    ) %>%
    dplyr::add_row( # Father missing from Capture_data
      Row = max(B13_brood_rows$Row) + 3
    ) %>%
    dplyr::add_row( # Both parents missing from Capture_data
      Row = max(B13_brood_rows$Row) + 4
    ) %>%
    dplyr::mutate(
      FemaleID = paste0("K", Row),
      MaleID = paste0("L", Row),
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "CYACAE",
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "B14"
    )

  B14_capture_rows <- Capture_data %>%
    dplyr::mutate( # Female of probable brood
      IndvID = B14_brood_rows$FemaleID[1],
      Sex_observed = "F"
    ) %>%
    dplyr::add_row( # Male of probable brood
      IndvID = B14_brood_rows$MaleID[1],
      Sex_observed = "M"
    ) %>%
    dplyr::add_row( # Male of brood with missing mother
      IndvID = B14_brood_rows$MaleID[2],
      Sex_observed = "M"
    ) %>%
    dplyr::add_row( # Female of brood with missing father
      IndvID = B14_brood_rows$FemaleID[3],
      Sex_observed = "F"
    ) %>%
    dplyr::mutate(
      Row = seq(max(C5_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-03",
      Species = "CYACAE",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B14"
    )

  B14_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = B14_capture_rows$IndvID[1],
      Sex_calculated = B14_capture_rows$Sex_observed[1]
    ) %>%
    dplyr::add_row(
      IndvID = B14_capture_rows$IndvID[-1],
      Sex_calculated = B14_capture_rows$Sex_observed[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(C5_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "CYACAE",
      CheckID = "B14"
    )

  # B15: Checking that nest locations appear in Location_data ####
  B15_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = max(B14_brood_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Location missing from Location_data
      Row = max(B14_brood_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      FemaleID = paste0("K", Row),
      MaleID = paste0("L", Row),
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "CYACAE",
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      LocationID = paste(.data$PopID, "NB", .data$Row, sep = "_"),
      CheckID = "B15"
    )

  B15_capture_rows <- Capture_data %>% # Add all parents to Capture_data, otherwise flagged by B14
    dplyr::mutate(
      IndvID = B15_brood_rows$FemaleID[1],
      Sex_observed = "F"
    ) %>%
    dplyr::add_row(
      IndvID = B15_brood_rows$MaleID[1],
      Sex_observed = "M"
    ) %>%
    dplyr::add_row(
      IndvID = B15_brood_rows$FemaleID[2],
      Sex_observed = "F"
    ) %>%
    dplyr::add_row(
      IndvID = B15_brood_rows$MaleID[2],
      Sex_observed = "M"
    ) %>%
    dplyr::mutate(
      Row = seq(max(B14_capture_rows$Row) + 1, length.out = dplyr::n()),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      CaptureDate = "2020-07-03",
      Species = "CYACAE",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      CheckID = "B15"
    )

  B15_indv_rows <- Individual_data %>%
    dplyr::mutate(
      IndvID = B15_capture_rows$IndvID[1],
      Sex_calculated = B15_capture_rows$Sex_observed[1]
    ) %>%
    dplyr::add_row(
      IndvID = B15_capture_rows$IndvID[-1],
      Sex_calculated = B15_capture_rows$Sex_observed[-1]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B14_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "CYACAE",
      CheckID = "B15"
    )

  B15_location_rows <- Location_data %>%
    dplyr::mutate( # Probable
      Row = max(L1_rows$Row) + 1,
      PopID = "AAA",
      StartSeason = as.integer(2019),
      EndSeason = as.integer(2021),
      LocationType = "NB",
      LocationID = B15_brood_rows$LocationID[1],
      NestboxID = .data$LocationID,
      CheckID = "B15"
    )

  # C6: Checking that capture locations appear in Location_data ####
  C6_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = max(B15_capture_rows$Row) + 1
    ) %>%
    dplyr::add_row( # Missing location from Location_data
      Row = max(B15_capture_rows$Row) + 2
    ) %>%
    dplyr::mutate(
      IndvID = paste0("I", Row),
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(.data$CapturePopID, .data$IndvID, .data$CaptureDate, sep = "_"),
      LocationID = paste(.data$CapturePopID, "NB", .data$Row, sep = "_"),
      CheckID = "C6"
    )

  C6_indv_rows <- Individual_data %>% # Add captures to Individual_data, otherwise flagged by C5
    dplyr::mutate(
      IndvID = C6_capture_rows$IndvID[1]
    ) %>%
    dplyr::add_row(
      IndvID = C6_capture_rows$IndvID[2]
    ) %>%
    dplyr::mutate(
      Row = seq(max(B15_indv_rows$Row) + 1, length.out = dplyr::n()),
      PopID = "AAA",
      RingSeason = as.integer(2020),
      Species = "PARMAJ",
      Sex_calculated = "F",
      CheckID = "C6"
    )

  C6_location_rows <- Location_data %>%
    dplyr::mutate(
      LocationID = C6_capture_rows$LocationID[1],
      Row = max(B15_location_rows$Row) + 1,
      PopID = "AAA",
      StartSeason = as.integer(2019),
      EndSeason = as.integer(2021),
      LocationType = "NB",
      NestboxID = .data$LocationID,
      CheckID = "C6"
    )

  # Approved_list: make sure that our approve-listing procedure works ####
  # We create a record that violates check B3, but should NOT result in TRUE in Warning & Error columns
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
      BroodID = paste(.data$PopID, .data$BreedingSeason, .data$Row, sep = "-"),
      CheckID = "Approved list"
    )

  # Combine single check rows per dataframe
  Brood_data <- dplyr::bind_rows(al_rows, B1_rows, B2_rows, B3_rows, B4_rows, B5a_rows, B5b_rows,
                                 B5c_rows, B5d_rows, B6_brood_rows, B7_rows, B8_rows, B9_brood_rows,
                                 C3_brood_rows, B10_brood_rows, B11_brood_rows, B12_brood_rows, B13_brood_rows,
                                 B14_brood_rows, B15_brood_rows) %>%
    dplyr::arrange(.data$Row)

  Capture_data <- dplyr::bind_rows(B6_capture_rows, B8_capture_rows, B9_capture_rows,
                                   B11_capture_rows, B12_capture_rows, B13_capture_rows, C1a_adult_rows, C1a_chick_rows,
                                   C1b_adult_rows, C1b_chick_rows, C2_rows, I1_capture_rows,
                                   I2_capture_rows, I3_capture_rows, I4_capture_rows, I5_capture_rows,
                                   C3_capture_rows, B10_capture_rows,C4_capture_rows, L1_capture_rows,
                                   C5_capture_rows, B14_capture_rows, B15_capture_rows, C6_capture_rows) %>%
    dplyr::arrange(.data$Row)

  Individual_data <- dplyr::bind_rows(B6_indv_rows, B8_indv_rows, B9_indv_rows, C1_indv_rows, C2_indv_rows,
                                      I1_indv_rows, I2_indv_rows, I3_indv_rows, I4_indv_rows, I5_indv_rows,
                                      C3_indv_rows, B10_indv_rows, B11_indv_rows, B12_indv_rows, B13_indv_rows,
                                      C4_indv_rows, L1_indv_rows, C5_indv_rows, B14_indv_rows,
                                      B15_indv_rows, C6_indv_rows) %>%
    dplyr::arrange(.data$Row)

  Location_data <- dplyr::bind_rows(I2_location_rows, C3_location_rows, L1_rows, B15_location_rows,
                                    C6_location_rows) %>%
    dplyr::arrange(.data$Row)

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

  return(dummy_data)

}
