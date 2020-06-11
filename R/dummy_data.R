#' Create quality check dummy data
#'
#' @description Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}.
#' In each data frame, rows are specifically created to test single checks from \code{\link{quality_check}}.
#' See a detailed description of which rows correspond to which checks in \code{\link{dummy_data}}.
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


  # Add rows in which single checks are violated
  # The code is ordered by the time at which the checks are made

  # B2: Comparing clutch and brood sizes
  # - Non-manipulated brood
  Brood_data %>%
    dplyr::mutate(
      Row = as.integer(1),
      ClutchSize = as.integer(7),
      BroodSize = as.integer(8),
    ) ->
    Brood_data

  # - Manipulated brood
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(2),
      ClutchSize = as.integer(7),
      BroodSize = as.integer(8),
      ExperimentID = "COHORT"
    ) ->
    Brood_data

  # B3: Comparing brood sizes and fledgling numbers
  # - Non-manipulated brood
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(3),
      BroodSize = as.integer(5),
      NumberFledged = as.integer(6)
    ) ->
    Brood_data

  # - Manipulated brood
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(4),
      BroodSize = as.integer(5),
      NumberFledged = as.integer(6),
      ExperimentID = "COHORT"
    ) ->
    Brood_data

  # B4: Comparing laying and hatching dates
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(5),
      LayDate = as.Date(as.character("2019-04-04")),
      HatchDate = as.Date("2019-04-01")
    ) ->
    Brood_data

  # B5: Comparing hatching and fledging dates
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(6),
      HatchDate = as.Date("2019-04-04"),
      FledgeDate = as.Date("2019-04-01")
    ) ->
    Brood_data


  # B6: Checking brood variable values against reference values
  # - Unusual values
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(7:14),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      ClutchSize = as.integer(c(18, 18, 14, 14, 14, 14, 14, 14)),
      BroodSize = as.integer(c(16, 16, 16, 16, 16, 16, 16, 16)),
      NumberFledged = as.integer(c(16, 16, 16, 16, 16, 16, 16, 16))
    ) ->
    Brood_data

  # - Impossible values
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(15:22),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      ClutchSize = as.integer(c(25, 25, 25, 25, 25, 25, 25, 25)),
      BroodSize = as.integer(c(22, 22, 22, 22, 22, 22, 22, 22)),
      NumberFledged = as.integer(c(22, 22, 22, 22, 22, 22, 22, 22))
    ) ->
    Brood_data

  # C2: Checking capture variable values against reference values
  # - Unusual values adults
  Capture_data %>%
    tibble::add_row(
      Row = as.integer(1:8),
      Age_calculated = as.integer(5, 5, 5, 5, 5, 5, 5, 5),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      Mass = as.integer(c(7, 7, 7, 7, 7, 7, 7, 7)),
      Tarsus = as.integer(c(14, 14, 14, 14, 14, 14, 14, 14)),
    )  %>%
    dplyr::slice(-1L) ->
    Capture_data

  # - Improbable values chicks
  Capture_data %>%
    tibble::add_row(
      Row = as.integer(9:16),
      Age_calculated = as.integer(3, 3, 3, 3, 3, 3, 3, 3),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      Mass = as.integer(c(7, 7, 7, 7, 7, 7, 7, 7)),
      Tarsus = as.integer(c(14, 14, 14, 14, 14, 14, 14, 14)),
    ) ->
    Capture_data

  # - Unusual values adults
  Capture_data %>%
    tibble::add_row(
      Row = as.integer(17:24),
      Age_calculated = as.integer(5, 5, 5, 5, 5, 5, 5, 5),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      Mass = as.integer(c(3, 3, 3, 3, 3, 3, 3, 3)),
      Tarsus = as.integer(c(30, 30, 30, 30, 30, 30, 30, 30)),
    )   ->
    Capture_data

  # - Impossbile values chicks
  Capture_data %>%
    tibble::add_row(
      Row = as.integer(25:32),
      Age_calculated = as.integer(3, 3, 3, 3, 3, 3, 3, 3),
      Species = c("PARMAJ", "CYACAE", "FICHYP", "SITEUR", "PERATE", "PASMON", "FICALB", "POEPAL"),
      Mass = as.integer(c(3, 3, 3, 3, 3, 3, 3, 3)),
      Tarsus = as.integer(c(30, 30, 30, 30, 30, 30, 30, 30)),
    )  ->
    Capture_data

  # I2: Checking unique individual IDs
  Individual_data %>%
    tibble::add_row(
      Row = as.integer(1:4),
      IndvID = as.character(c("A0001", "A0001", "A0002", "A0002")),
      PopID = as.character(c("AAA", "AAA", "AAA", "BBB"))
    )  %>%
    dplyr::slice(-1L) ->
    Individual_data

  # I3: Checking that chicks have BroodIDs
  Individual_data %>%
    tibble::add_row(
      Row = as.integer(5),
      IndvID = as.character("B0001"),
      PopID = as.character("BBB"),
      RingAge = as.character("chick"),
    )  ->
    Individual_data

  Capture_data %>%
    tibble::add_row(
      Row = as.integer(33),
      IndvID = as.character("B0001"),
      CaptureDate = as.Date("2019-04-01"),
      CapturePopID = as.character("BBB"),
      LocationID = as.character("BBB_001")
    )   ->
    Capture_data

  Location_data %>%
    tibble::add_row(
      Row = as.integer(1),
      LocationID = as.character("BBB_001"),
      LocationType = as.character("NB"),
      PopID = as.character("BBB")
    )  %>%
    dplyr::slice(-1L) ->
    Location_data

  # I4: Checking that individuals have no conflicting sex
  Individual_data %>%
    tibble::add_row(
      Row = as.integer(6),
      Sex = as.character("C"),
    )  ->
    Individual_data

  # B7: Checking parent species
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(23),
      FemaleID = as.character("F0001"),
      MaleID = as.character("M0001")
    ) ->
    Brood_data

  Individual_data %>%
    tibble::add_row(
      Row = as.integer(7:8),
      IndvID = as.character(c("F0001", "M0001")),
      Species = as.character(c("PARMAJ", "CYACAE"))
    )  ->
    Individual_data

  # B8: Comparing brood size and number of chicks captured
  Individual_data %>%
    tibble::add_row(
      Row = as.integer(9:12),
      IndvID = as.character(c("C0001", "C0002", "C0003", "C0004")),
      BroodIDLaid = as.character(c("2019_CCC001", "2019_CCC001", "2019_CCC002", "2019_CCC002"))
    ) ->
    Individual_data

  Brood_data %>%
    tibble::add_row(
      Row = as.integer(24:25),
      BroodID = as.character(c("2019_CCC001", "2019_CCC002")),
      BroodSize = as.integer(c(3, 1))
    ) ->
    Brood_data

  # B9: Check that BroodIDs are unique
  Brood_data %>%
    tibble::add_row(
      Row = as.integer(26:27),
      BroodID = as.character(c("2019_DDD001", "2019_DDD001")),
      PopID = as.character(c("DDD", "DDD"))
    ) ->
    Brood_data

  # Combine in list
  dummy_data <- list(Brood_data = Brood_data,
                     Capture_data = Capture_data,
                     Individual_data = Individual_data,
                     Location_data = Location_data)

  return(dummy_data)
}
