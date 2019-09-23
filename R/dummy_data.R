#' Quality check dummy data
#'
#' Create dummy pipeline output (\code{dummy_data}) to be tested in \code\link{quality_check}}.
#' Each row in \code{dummy_data} corresponds to a single check in \code\link{quality_check}}.


# Create skeletons for each pipeline dataframe
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

# Combine in list
dummy_data <- list(Brood_data = Brood_data,
                   Capture_data = Capture_data,
                   Individual_data = Individual_data,
                   Location_data = Location_data)

# Add row that violates
