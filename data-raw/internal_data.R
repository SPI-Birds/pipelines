#### Internal data objects ####

# Create tables and lists that are intended to be used internally.

# 1. Data templates ####
# Create templates of the data tables that are part of SPI-Birds standard format. These are used to ensure that the output of each pipeline conforms to the standard format, in terms of variable names and variable types/classes. For each version of the standard format, templates are available.

# Data template for standard format version 1.0
# Source: https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf
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
# Source: https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf
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


# Data template for standard format version 2.0
# Source: https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf
v2.0 <- tibble::lst(

  ## Individual data
  Individual_data = tibble::tibble(
    row = NA_integer_,
    individualID = NA_character_,
    speciesID = NA_character_,
    siteID = NA_character_,
    broodIDLaid = NA_character_,
    broodIDFledged = NA_character_,
    ringYear = NA_integer_,
    ringMonth = NA_integer_,
    ringDay = NA_integer_,
    ringStage = NA_character_,
    ringSiteID = NA_character_,
    geneticSex = NA_character_,
    rowWarning = NA,
    rowError = NA,
  ),

  ## Brood data
  Brood_data = tibble::tibble(
    row = NA_integer_,
    broodID = NA_character_,
    siteID = NA_character_,
    speciesID = NA_character_,
    plotID = NA_character_,
    locationID = NA_character_,
    femaleID = NA_character_,
    maleID = NA_character_,
    observedClutchType = NA_character_,
    observedLayYear = NA_integer_,
    observedLayMonth = NA_integer_,
    observedLayDay = NA_integer_,
    minimumLayYear = NA_integer_,
    minimumLayMonth = NA_integer_,
    minimumLayDay = NA_integer_,
    maximumLayYear = NA_integer_,
    maximumLayMonth = NA_integer_,
    maximumLayDay = NA_integer_,
    observedClutchSize = NA_integer_,
    minimumClutchSize = NA_integer_,
    maximumClutchSize = NA_real_,
    observedHatchYear = NA_integer_,
    observedHatchMonth = NA_integer_,
    observedHatchDay = NA_integer_,
    minimumHatchYear = NA_integer_,
    minimumHatchMonth = NA_integer_,
    minimumHatchDay = NA_integer_,
    maximumHatchYear = NA_integer_,
    maximumHatchMonth = NA_integer_,
    maximumHatchDay = NA_integer_,
    observedBroodSize = NA_integer_,
    minimumBroodSize = NA_integer_,
    maximumBroodSize = NA_real_,
    observedFledgeYear = NA_integer_,
    observedFledgeMonth = NA_integer_,
    observedFledgeDay = NA_integer_,
    minimumFledgeYear = NA_integer_,
    minimumFledgeMonth = NA_integer_,
    minimumFledgeDay = NA_integer_,
    maximumFledgeYear = NA_integer_,
    maximumFledgeMonth = NA_integer_,
    maximumFledgeDay = NA_integer_,
    observedNumberFledged = NA_integer_,
    minimumNumberFledged = NA_integer_,
    maximumNumberFledged = NA_real_,
    treatmentID = NA_character_,
    rowWarning = NA,
    rowError = NA
  ),

  ## Capture data
  Capture_data = tibble::tibble(
    row = NA_integer_,
    captureID = NA_character_,
    individualID = NA_character_,
    captureTagID = NA_character_,
    releaseTagID = NA_character_,
    speciesID = NA_character_,
    observedSex = NA_character_,
    captureYear = NA_integer_,
    captureMonth = NA_integer_,
    captureDay = NA_integer_,
    captureTime = NA_character_,
    recordedBy = NA_character_,
    locationID = NA_character_,
    capturePhysical = NA,
    captureAlive = NA,
    releaseAlive = NA,
    captureSiteID = NA_character_,
    capturePlotID = NA_character_,
    releaseSiteID = NA_character_,
    releasePlotID = NA_character_,
    chickAge = NA_integer_,
    treatmentID = NA_character_,
    rowWarning = NA,
    rowError = NA
  ),

  ## Location data
  Location_data = tibble::tibble(
    row = NA_integer_,
    locationID = NA_character_,
    locationType = NA_character_,
    siteID = NA_character_,
    decimalLatitude = NA_real_,
    decimalLongitude = NA_real_,
    startYear = NA_integer_,
    endYear = NA_integer_,
    habitatID = NA_character_,
    rowWarning = NA,
    rowError = NA
  ),

  ## Measurement data
  Measurement_data = tibble::tibble(
    row = NA_integer_,
    measurementID = NA_character_,
    recordID = NA_character_,
    siteID = NA_character_,
    measurementSubject = NA_character_,
    measurementType = NA_character_,
    measurementValue = NA_real_,
    measurementAccuracy = NA_real_,
    measurementUnit = NA_character_,
    measurementDeterminedYear = NA_integer_,
    measurementDeterminedMonth = NA_integer_,
    measurementDeterminedDay = NA_integer_,
    recordedBy = NA_character_,
    measurementMethod = NA_character_,
    rowWarning = NA,
    rowError = NA
  ),

  ## Experiment data
  Experiment_data = tibble::tibble(
    row = NA_integer_,
    treatmentID = NA_character_,
    experimentID = NA_character_,
    siteID = NA_character_,
    experimentType = NA_character_,
    experimentStartYear = NA_integer_,
    experimentStartMonth = NA_integer_,
    experimentStartDay = NA_integer_,
    experimentStartTime = NA_character_,
    experimentEndYear = NA_integer_,
    experimentEndMonth = NA_integer_,
    experimentEndDay = NA_integer_,
    experimentEndTime = NA_character_,
    experimentStage = NA_character_,
    treatmentDetails = NA_character_,
    recordedBy = NA_character_,
    reference = NA_character_,
    rowWarning = NA,
    rowError = NA
  )

)

# Combine data templates of different versions in a list
data_templates <- tibble::lst(v1.0, v1.1, v2.0)


# 2. Optional variables generated by standard utility functions ####
utility_variables <- tibble::lst(

  # Individual data
  Individual_data = tibble::tibble(calculatedSex = NA_character_),

  # Brood data
  Brood_data = tibble::tibble(breedingSeason = NA_character_,
                              calculatedClutchType = NA_character_,
                              nestAttemptNumber = NA_integer_),

  # Capture data
  Capture_data = tibble::tibble(exactAge = NA_integer_,
                                minimumAge = NA_integer_)

)

# 3. Variable lists for standard format testing ####
# Create a list of variables that are key and cannot have missing values (i.e., NA), and a list of variables that can only contain a limited number of categories (e.g., "M", "F" or "C" in Sex_observed). These lists are intended to be used in pipeline tests (see R/test_general_format.R) that ensure pipeline robustness and consistency.
# Note that row, rowWarning and rowError cannot have missing values but are not part of the key variables list because these columns are filled during the standard quality check procedure, which is done after the pipeline is tested.

# Create list of key variables
key_variables <- tibble::lst(

  # Individual data
  Individual_data = c("IndvID", "Species", "PopID", "RingSeason", # v1.0 & v1.1 variables
                      "individualID", "speciesID", "siteID", "ringYear", # v2.0 variables
                      "ringMonth", "ringDay", "ringSiteID"),

  # Brood data
  Brood_data = c("BroodID", "PopID", "BreedingSeason", "Species", # v1.0 & v1.1 variables
                 "broodID", "siteID", "speciesID"), # v2.0 variables

  # Capture data
  Capture_data = c("IndvID", "Species", "BreedingSeason", "CaptureDate", "CapturePopID", # v1.0 variables
                   "CaptureID", "CaptureAlive", "ReleaseAlive", # v1.1 variables
                   "captureID", "individualID", "releaseTagID", "speciesID", # v2.0 variables
                   "captureYear", "captureMonth", "captureDay", "capturePhysical", "captureAlive", "releaseAlive",
                   "captureSiteID"),

  # Location data
  Location_data = c("LocationID", "LocationType", "PopID", # v1.0 & v1.1 variables
                    "locationID", "locationType", "siteID"), # v2.0 variables

  # Measurement data
  Measurement_data = c("measurementID", "recordID", "siteID", "measurementSubject", # v2.0 variables
                       "measurementType", "measurementValue", "measurementUnit", "measurementDeterminedYear",
                       "measurementDeterminedMonth", "measurementDeterminedDay"),

  # Experiment data
  Experiment_data = c("experimentID", "treatmentID", "experimentID", "siteID", "experimentType", # v2.0 variables
                      "experimentStartYear", "experimentEndYear", "treatmentDetails")

)

# Create list of categorical variables and their possible categories
categorical_variables <- tibble::lst(

  # Individual data
  Individual_data = tibble::lst(
    Species = c(species_codes$speciesID, "FICHIB", "CCCCCC"), # v1.0 & v1.1
    PopID = site_codes$siteID, # v1.0 & v1.1
    RingAge = c("chick", "adult", NA), # v1.0 & v1.1
    Sex = c("F", "M", "C", NA), # v1.0
    Sex_calculated = c("F", "M", "C", NA), # v1.1
    Sex_genetic = c("F", "M", "C", NA), # v1.1
    speciesID = c(species_codes$speciesID, "FICHIB", "CCCCCC"), # v2.0
    siteID = site_codes$siteID, # v2.0
    ringStage = c("chick", "subadult", "adult", NA), # v2.0
    ringSiteID = site_codes$siteID, # v2.0
    geneticSex = c("F", "M", "C", NA) # v2.0
  ),

  # Brood data
  Brood_data = tibble::lst(
    Species = species_codes$speciesID, # v1.0 & v1.1
    PopID = site_codes$siteID, # v1.0 & v1.1
    ClutchType_observed = c("first", "second", "replacement", NA), # v1.0 & v1.1
    ClutchType_calculated = c("first", "second", "replacement", NA), # v1.0 & v1.1
    OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA), # v1.0 & v1.1
    ExperimentID = c("PHENOLOGY", "COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA), # v1.0 & v1.1
    speciesID = c(species_codes$speciesID, "FICHIB"), # v2.0
    siteID = site_codes$siteID, # v2.0
    observedClutchType = c("first", "second", "replacement", NA) # v2.0
  ),

  # Capture data
  Capture_data = tibble::lst(
    Species = species_codes$speciesID, # v1.0 & v1.1
    CapturePopID = site_codes$siteID, # v1.0 & v1.1
    ReleasePopID = c(site_codes$siteID, NA), # v1.0 & v1.1
    OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA), # v1.0 & v1.1
    ExperimentID = c("PHENOLOGY", "COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA), # v1.1
    Sex_observed = c("F", "M", NA), # v1.1
    speciesID = c(species_codes$speciesID, "FICHIB"), # v2.0
    observedSex = c("F", "M", NA), # v2.0
    captureSiteID = site_codes$siteID, # v2.0
    releaseSiteID = c(site_codes$siteID, NA) # v2.0
  ),

  # Location data
  Location_data = tibble::lst(
    LocationType = c("NB", "MN"), # v1.0 & v1.1
    PopID = site_codes$siteID, # v1.0 & v1.1
    Habitat = c("deciduous", "evergreen", "mixed", NA), # v1.0
    HabitatType = c("deciduous", "evergreen", "mixed", "urban", NA), # v1.1
    locationType = c("nest", "capture", "observation"), # v2.0
    siteID = site_codes$siteID, # v2.0
    habitatID = habitat_codes$habitatID # v2.0
  ),

  # Measurement data
  Measurement_data = tibble::lst( # v2.0
    siteID = site_codes$siteID,
    measurementSubject = c("capture", "location")
    ##FIXME: Maybe add measurementType?
  ),

  # Experiment data
  Experiment_data = tibble::lst( # v2.0
    siteID = site_codes$siteID
    ##FIXME: Maybe add experimentType?
  )

)

# 4. Save ####
# Save objects as internal data in R/sysdata.rda
usethis::use_data(data_templates, key_variables, utility_variables, categorical_variables,
                  internal = TRUE, overwrite = TRUE, compress = "xz")
