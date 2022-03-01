# Data data templates
# Create templates of the data tables that are part of SPI-Birds standard format. These are used to ensure that the output of each pipeline conforms to the standard format, in terms of variable names and variable types/classes. For each version of the standard format, templates can be generated.

# Data template for standard format version 1.0
v1.0 <- tibble::lst(

  ## Individual data
  Individual_data = tibble::tibble(
    IndvID = NA_character_,
    Species = NA_character_,
    PopID = NA_character_,
    BroodIDLaid = NA_character_,
    BroodIDFledged = NA_character_,
    RingSeason = NA_real_,
    RingAge = NA_character_,
    Sex = NA_character_
  ),

  ## Brood data
  Brood_data = tibble::tibble(
    BroodID = NA_character_,
    PopID = NA_character_,
    BreedingSeason = NA_real_,
    Species = NA_character_,
    Plot = NA_character_,
    LocationID = NA_character_,
    FemaleID = NA_character_,
    MaleID = NA_character_,
    ClutchType_observed = NA_character_,
    ClutchType_calculated = NA_character_,
    LayingDate = as.Date(NA_character_),
    LayingDateError = NA_integer_,
    ClutchSize = NA_integer_,
    ClutchSizeError = NA_integer_,
    HatchDate = as.Date(NA_character_),
    HatchDateError = NA_integer_,
    BroodSize = NA_integer_,
    BroodSizeError = NA_integer_,
    FledgeDate = as.Date(NA_character_),
    FledgeDateError = NA_integer_,
    NumberFledged = NA_integer_,
    NumberFledgedError = NA_integer_,
    AvgEggMass = NA_real_,
    NumberEggs = NA_integer_,
    AvgChickMass = NA_real_,
    NumberChicksMass = NA_integer_,
    AvgTarsus = NA_real_,
    NumberChicksTarsus = NA_integer_,
    OriginalTarsusMethod = NA_character_,
    ExperimentID = NA_character_
  ),

  ## Capture data
  Capture_data = tibble::tibble(
    IndvID = NA_character_,
    Species = NA_character_,
    BreedingSeason = NA_real_,
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
    Age_calculated = NA_real_,
    ChickAge = NA_real_
  ),

  ## Location data
  Location_data = tibble::tibble(
    LocationID = NA_character_,
    NestboxID = NA_character_,
    LocationType = NA_character_,
    PopID = NA_character_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    StartSeason = NA_real_,
    EndSeason = NA_real_,
    Habitat = NA_character_
  )

)

# Data template for standard format version 1.1
v1.1 <- tibble::lst(

  ## Individual data
  Individual_data = tibble::tibble(
    IndvID = NA_character_,
    Species = NA_character_,
    PopID = NA_character_,
    BroodIDLaid = NA_character_,
    BroodIDFledged = NA_character_,
    RingSeason = NA_integer_,
    RingAge = NA_character_,
    Sex_calculated = NA_character_,
    Sex_genetic = NA_character_
  ),

  ## Brood data
  Brood_data = tibble::tibble(
    BroodID = NA_character_,
    PopID = NA_character_,
    BreedingSeason = NA_integer_,
    Species = NA_character_,
    Plot = NA_character_,
    LocationID = NA_character_,
    FemaleID = NA_character_,
    MaleID = NA_character_,
    ClutchType_observed = NA_character_,
    ClutchType_calculated = NA_character_,
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
    NumberChicksTarsus = NA_integer_,
    OriginalTarsusMethod = NA_character_,
    ExperimentID = NA_character_
  ),

  ## Capture data
  Capture_data = tibble::tibble(
    CaptureID = NA_character_,
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
    Age_calculated = NA_real_,
    ChickAge = NA_real_,
    ExperimentID = NA_character_
  ),

  ## Location data
  Location_data = tibble::tibble(
    LocationID = NA_character_,
    NestboxID = NA_character_,
    LocationType = NA_character_,
    PopID = NA_character_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    StartSeason = NA_real_,
    EndSeason = NA_real_,
    HabitatType = NA_character_
  )

)

# Combine data templates of different versions
data_templates <- tibble::lst(v1.0, v1.1)

# Save templates as internal data, i.e., sysdata.rda
usethis::use_data(data_templates, internal = TRUE, overwrite = TRUE, compress = "xz")
