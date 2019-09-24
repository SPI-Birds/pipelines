#' Quality check dummy data
#'
#' Create dummy pipeline output (\code{dummy_data}) to be tested in \code{\link{quality_check}}. In each data frame, rows are specifically created to violate single checks from \code{\link{quality_check}. See a detailed description of which rows correspond to which checks in \sQuote{Details}.
#'
#' \strong{Brood data}:
#' \enumerate{
#'   \item Row 1 represents a non-manipulated brood that violates \sQuote{Brood check 2: Comparing clutch and brood sizes} (see \code{\link{compare_clutch_brood}}).
#'   \item Row 2 represents a manipulated brood that violates \sQuote{Brood check 2: Comparing clutch and brood sizes} (see \code{\link{compare_clutch_brood}}).
#'   \item Row 3 represents a non-manipulated brood that violates \sQuote{Brood check 3: Comparing brood sizes and fledgling numbers} (see \code{\link{compare_brood_fledglings}}).
#'   \item Row 4 represents a manipulated brood that violates \sQuote{Brood check 3: Comparing brood sizes and fledgling numbers} (see \code{\link{compare_brood_fledglings}}).
#'   \item Row 5 represents a brood that violates \sQuote{Brood check 4: Comparing laying and hatching dates} (see \code{\link{compare_laying_hatching}}).
#' }
#'
#' \strong{Capture data}:
#'
#' \strong{Individual data}:
#'
#' \strong{Location data}:
#'
#' @format
#' List of 4 dataframes.
#' \describe{
#'   \item{Brood_data}{Dummy brood data.}
#'   \item{Capture_data}{Dummy capture data.}
#'   \item{Individual_data}{Dummy individual data.}
#'   \item{Location_data}{Dummy location data.}
#' }
#'
#' @name dummy_data

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
  LayingDate = as.Date(NA_character_),
  LayingDateError = NA_real_,
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
# Brood check 2: Comparing clutch and brood sizes

# Non-manipulated brood
Brood_data %>%
  dplyr::mutate(
    Row = as.integer(1),
    ClutchSize = as.integer(7),
    BroodSize = as.integer(8),
  ) ->
  Brood_data

# Manipulated brood
Brood_data %>%
  tibble::add_row(
    Row = as.integer(2),
    ClutchSize = as.integer(7),
    BroodSize = as.integer(8),
    ExperimentID = "COHORT"
  ) ->
  Brood_data


# Brood check 3: Comparing brood sizes and fledgling numbers

# Non-manipulated brood
Brood_data %>%
  tibble::add_row(
    Row = as.integer(3),
    BroodSize = as.integer(5),
    NumberFledged = as.integer(6)
  ) ->
  Brood_data


# Manipulated brood
Brood_data %>%
  tibble::add_row(
    Row = as.integer(4),
    BroodSize = as.integer(5),
    NumberFledged = as.integer(6),
    ExperimentID = "COHORT"
  ) ->
  Brood_data

# Brood check 4: Comparing laying and hatching dates

Brood_data %>%
  tibble::add_row(
    Row = as.integer(5),
    LayingDate = as.Date(as.character("2019-04-04")),
    HatchDate = as.Date("2019-04-01")
  ) ->
  Brood_data

# Brood check 5: Comparing hatching and fledging dates

Brood_data %>%
  tibble::add_row(
    Row = as.integer(6),
    HatchDate = as.Date("2019-04-04"),
    FledgeDate = as.Date("2019-04-01")
  ) ->
  Brood_data


# Combine in list
dummy_data <- list(Brood_data = Brood_data,
                   Capture_data = Capture_data,
                   Individual_data = Individual_data,
                   Location_data = Location_data)

save(dummy_data,
     file = paste0(utils::choose.dir(), "\\dummy_data.rda"))
