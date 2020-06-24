#' Create quality check dummy data
#'
#' @description Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}.
#' In each data frame, rows are specifically created to test single checks from \code{\link{quality_check}}.
#' See a detailed description of which rows correspond to which checks in \code{\link{dummy_data}}.
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

  # Create skeletons for each pipeline data frame
  # Brood data
  tibble::tibble(
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
    LayDate = as.Date(NA_character_),
    LayDateError = NA_real_,
    ClutchSize = NA_integer_,
    ClutchSizeError = NA_real_,
    HatchDate = as.Date(NA_character_),
    HatchDateError = NA_real_,
    BroodSize = NA_integer_,
    BroodSizeError = NA_real_,
    FledgeDate = as.Date(NA_character_),
    FledgeDateError = NA_real_,
    NumberFledged = NA_integer_,
    NumberFledgedError = NA_real_,
    AvgEggMass = NA_real_,
    NumberEggs = NA_integer_,
    AvgChickMass = NA_real_,
    NumberChicksMass = NA_integer_,
    AvgTarsus = NA_real_,
    AvgChicksTarsus = NA_integer_,
    OriginalTarsusMethod = NA_character_,
    ExperimentID = NA_character_
  ) ->
    Brood_data


  # Capture data
  tibble::tibble(
    Row = NA_integer_,
    IndvID = NA_character_,
    Species = NA_character_,
    BreedingSeason = NA_integer_,
    CaptureDate = as.Date(NA_character_),
    CaptureTime = NA_character_,
    ObserverID = NA_character_,
    LocationID = NA_character_,
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
    ChickAge = NA_integer_
  ) ->
    Capture_data


  # Individual data
  tibble::tibble(
    Row = NA_integer_,
    IndvID = NA_character_,
    Species = NA_character_,
    PopID = NA_character_,
    BroodIDLaid = NA_character_,
    BroodIDFledged = NA_character_,
    RingSeason = NA_integer_,
    RingAge = NA_character_,
    Sex = NA_character_
  ) ->
    Individual_data


  # Location data
  tibble::tibble(
    Row = NA_integer_,
    LocationID = NA_character_,
    NestboxID = NA_character_,
    LocationType = NA_character_,
    PopID = NA_character_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    StartSeason = NA_integer_,
    EndSeason = NA_integer_,
    Habitat = NA_character_
  ) ->
    Location_data


  # Add rows in which single checks can be validated
  # For each check we add rows with biologically probable values and
  # rows with biologically improbable values that violate the check

  # B2: Comparing clutch and brood sizes
  B2_rows <- Brood_data %>%
    dplyr::mutate( # Probable, non-manipulated brood
      Row = as.integer(1),
      ClutchSize = as.integer(8),
      BroodSize = as.integer(7)) %>%
    dplyr::add_row( # Impossible, non-manipulated brood (error)
      Row = as.integer(2),
      ClutchSize = as.integer(7),
      BroodSize = as.integer(8)) %>%
    dplyr::add_row( # Probable, manipulated brood
      Row = as.integer(3),
      ClutchSize = as.integer(8),
      BroodSize = as.integer(7),
      ExperimentID = "COHORT") %>%
    dplyr::add_row( # Improbable, manipulated brood (warning)
      Row = as.integer(4),
      ClutchSize = as.integer(7),
      BroodSize = as.integer(8),
      ExperimentID = "COHORT") %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B3: Comparing brood sizes and fledgling numbers
  B3_rows <- Brood_data %>%
    dplyr::mutate( # Probable, non-manipulated brood
      Row = as.integer(5),
      BroodSize = as.integer(6),
      NumberFledged = as.integer(5)) %>%
    dplyr::add_row( # Impossible, non-manipulated brood (error)
      Row = as.integer(6),
      BroodSize = as.integer(5),
      NumberFledged = as.integer(6)) %>%
    dplyr::add_row( # Probable, manipulated brood
      Row = as.integer(7),
      BroodSize = as.integer(6),
      NumberFledged = as.integer(5),
      ExperimentID = "COHORT") %>%
    dplyr::add_row( # Improbable, manipulated brood (warning)
      Row = as.integer(8),
      BroodSize = as.integer(5),
      NumberFledged = as.integer(6),
      ExperimentID = "COHORT") %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B4: Comparing laying and hatching dates
  B4_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(9),
      LayDate = as.Date("2020-04-04"),
      HatchDate = as.Date("2020-04-18")
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(10),
      LayDate = as.Date("2020-04-18"),
      HatchDate = as.Date("2020-04-04")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B5: Comparing hatching and fledging dates
  B5_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(11),
      HatchDate = as.Date("2020-04-18"),
      FledgeDate = as.Date("2020-05-01")
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(12),
      HatchDate = as.Date("2020-05-01"),
      FledgeDate = as.Date("2020-04-18")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B6a: Checking clutch size values against reference values
  B6a_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(13),
      ClutchSize = 10) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(14),
      ClutchSize = 20) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(15),
      ClutchSize = -10) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(16),
      ClutchSize = 88) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B6b: Checking brood size values against reference values
  B6b_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(17),
      BroodSize = 10) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(18),
      BroodSize = 20) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(19),
      BroodSize = -10) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(20),
      BroodSize = 88) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B6c: Checking fledgling number values against reference values
  B6c_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(21),
      NumberFledged = 10) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(22),
      NumberFledged = 20) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(23),
      NumberFledged = -10) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(24),
      NumberFledged = 88) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # B7: Checking parent species
  B7_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(25)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(26)
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  B7_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(1),
      IndvID = "F25",
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Probable
      Row = as.integer(2),
      IndvID = "M25",
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(3),
      IndvID = "F26",
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(4),
      IndvID = "M26",
      Species = "CYACAE"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      RingAge = "adult"
    )

  # B8: Comparing brood size and number of chicks captured
  B8_brood_rows <- Brood_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(27),
      BroodSize = as.integer(1)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(28),
      BroodSize = as.integer(2)
      ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(29),
      BroodSize = as.integer(1)
      ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  B8_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(5),
      BroodIDLaid = "AAA-2020-27"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(6),
      BroodIDLaid = "AAA-2020-28"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(7:8),
      BroodIDLaid = "AAA-2020-29"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      Species = "PARMAJ",
      IndvID = paste0("C", Row)
    )

  # B9: Check that BroodIDs are unique
  B9_rows <- Brood_data %>%
    dplyr::mutate( # Unique
      Row = as.integer(30),
      BroodID = "AAA-2020-30"
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = as.integer(31),
      BroodID = "AAA-2020-31"
    ) %>%
    dplyr::add_row( # Duplicated (error)
      Row = as.integer(32),
      BroodID = "AAA-2020-31"
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )


  # C2a: Checking mass values against reference values
  # - adults
  C2a_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(1),
      Mass = as.integer(20)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(2),
      Mass = as.integer(30)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(3),
      Mass = as.integer(100)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      IndvID = paste0("A", Row),
      Age_calculated = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_")
    )

  # - chicks
  C2a_chick_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(4),
      Mass = as.integer(20)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(5),
      Mass = as.integer(30)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(6),
      Mass = as.integer(100)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      IndvID = paste0("C", Row),
      Age_calculated = 3,
      ChickAge = 15,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_")
    )


  # C2b: Checking tarsus values against reference values
  # - adults
  C2b_adult_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(7),
      Tarsus = as.integer(20)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(8),
      Tarsus = as.integer(40)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(9),
      Tarsus = as.integer(100)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      IndvID = paste0("A", Row),
      Age_calculated = 5,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-05-02",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_")
    )

  # - chicks
  C2b_chick_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(10),
      Tarsus = as.integer(20)
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(11),
      Tarsus = as.integer(40)
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(12),
      Tarsus = as.integer(100)
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      IndvID = paste0("C", Row),
      Age_calculated = 3,
      ChickAge = 15,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-02",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_")
    )

  # C3: Checking chick age values
  C3_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(13),
      ChickAge = 15
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(14),
      ChickAge = -2
    ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      IndvID = paste0("C", Row),
      Age_calculated = 3,
      Species = "PARMAJ",
      BreedingSeason = 2020,
      CaptureDate = "2020-06-02",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_")
    )

  # I2: Checking unique individual IDs
  I2_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(9),
      PopID = "AAA",
      IndvID = paste0("I", Row)
      ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(c(10, 11)),
      PopID = as.character(c("AAA", "BBB")),
      IndvID = as.character(c("I10", "I10"))
      ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(c(12, 13)),
      PopID = as.character(c("AAA", "AAA")),
      IndvID = as.character(c("I12", "I12"))
      ) %>%
    dplyr::mutate(
      Species = "PARMAJ"
    )

  # I3: Checking that chicks have BroodIDs
  I3_indv_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(14),
      BroodIDLaid = "AAA-2020-14",
      BroodIDFledged = "AAA-2020-14"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(15)
      ) %>%
    dplyr::mutate(
      IndvID = paste0("C", Row),
      PopID = "AAA",
      RingAge = "chick",
      Species = "PARMAJ"
    )

  I3_capture_rows <- Capture_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(15),
      IndvID = "C14"
      ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(16),
      IndvID = "C15"
      ) %>%
    dplyr::mutate(
      CapturePopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      CaptureDate = "2020-06-01",
      CaptureID = paste(CapturePopID, IndvID, CaptureDate, sep="_"),
      LocationID = paste(CapturePopID, "NB", 1, sep="_")
    )

  I3_location_rows <- Location_data %>%
    dplyr::mutate(
      Row = as.integer(1),
      PopID = "AAA",
      LocationType = "NB",
      LocationID = paste(PopID, LocationType, 1, sep="_"),
      NestboxID = paste(PopID, LocationType, 1, sep="_")
    )

  # I4: Checking that individuals have no conflicting sex
  I4_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(16),
      Sex = "F"
    ) %>%
    dplyr::add_row( # Improbable (warning)
      Row = as.integer(17),
      Sex = "C"
    ) %>%
    dplyr::mutate(
      IndvID = paste0("I", Row),
      PopID = "AAA",
      Species = "PARMAJ"
    )

  # I5: Checking that individuals have no conflicting species
  I5_rows <- Individual_data %>%
    dplyr::mutate( # Probable
      Row = as.integer(18),
      Species = "PARMAJ"
    ) %>%
    dplyr::add_row( # Impossible (error)
      Row = as.integer(19),
      Species = "CONFLICTED"
    ) %>%
    dplyr::mutate(
      IndvID = paste0("I", Row),
      PopID = "AAA"
    )

  # Approved_list: make sure that our approvelisting procedure works
  # We create a record that violates check B4, but should NOT result in TRUE in Warning & Error columns
  al_rows <- Brood_data %>%
    dplyr::mutate(
      Row = as.integer(0),
      LayDate = as.Date("2020-06-24"),
      HatchDate = as.Date("2020-06-11")
    ) %>%
    dplyr::mutate(
      PopID = "AAA",
      BreedingSeason = as.integer(2020),
      Species = "PARMAJ",
      BroodID = paste(PopID, BreedingSeason, Row, sep="-"),
      FemaleID = paste0("F", Row),
      MaleID = paste0("M", Row)
    )

  # Combine single check rows per dataframe
  Brood_data <- dplyr::bind_rows(B2_rows, B3_rows, B4_rows, B5_rows, B6a_rows, B6b_rows, B6c_rows, B7_brood_rows, B8_brood_rows, B9_rows, al_rows)
  Capture_data <- dplyr::bind_rows(C2a_adult_rows, C2a_chick_rows, C2b_adult_rows, C2b_chick_rows, C3_rows, I3_capture_rows)
  Individual_data <- dplyr::bind_rows(B7_indv_rows, B8_indv_rows, I2_rows, I3_indv_rows, I4_rows, I5_rows)
  Location_data <- dplyr::bind_rows(I3_location_rows)

  # Combine in list
  dummy_data <- list(Brood_data = Brood_data,
                     Capture_data = Capture_data,
                     Individual_data = Individual_data,
                     Location_data = Location_data)

  usethis::use_data(dummy_data, overwrite=overwrite)
}
