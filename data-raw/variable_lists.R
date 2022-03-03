# Variable lists for standard format testing
# Create a list of variables that are key and cannot have missing values (i.e., NA), and a list of variables that can only contain a limited number of categories (e.g., "M", "F" or "C" in Sex_observed). These lists are intended to be used in pipeline tests (see R/test_general_format.R) that ensure pipeline robustness and consistency.

# Create list of key variables
key_variables <- tibble::lst(

  # Individual data
  Individual_data = c("IndvID", "Species", "PopID", "RingSeason", # v1.0 & v1.1 variables
                      "individualID", "speciesID", "siteID", "ringYear", "ringSiteID"),  # v1.2 variables

  # Brood data
  Brood_data = c("BroodID", "PopID", "BreedingSeason", "Species", # v1.0 & v1.1 variables
                 "broodID", "siteID", "speciesID"), # v1.2 variables

  # Capture data
  Capture_data = c("IndvID", "Species", "BreedingSeason", "CaptureDate", "CapturePopID", # v1.0 variables
                   "CaptureID", "CaptureAlive", "ReleaseAlive", # v1.1 variables
                   "captureID", "individualID", "releaseRingNumber", "speciesID", "captureYear", "captureMonth", "captureDay", # v1.2 variables
                   "captureSiteID"),

  # Location data
  Location_data = c("LocationID", "NestboxID", "LocationType", "PopID", # v1.0 & v1.1 variables
                    "locationID", "locationType", "siteID"), # v1.2 variables

  # Measurement data
  Measurement_data = c("measurementID", "measurementOnID", "siteID", "measurementType", "measurementValue", # v1.2 variables
                       "measurementUnit", "measurementDeterminedYear"),

  # Experiment data
  Experiment_data = c("experimentID", "treatmentID", "treatmentType", "treatmentStage", "treatmentDetails") # v1.2 variables

)

# Create list of categorical variables and their possible categories
categorical_variables <- tibble::lst(

  # Individual data
  Individual_data = tibble::lst(
    Species = species_codes$speciesID, # v1.0 & v1.1
    PopID = pop_codes$PopID, # v1.0 & v1.1
    RingAge = c("chick", "adult", NA), # v1.0 & v1.1
    Sex = c("F", "M", "C", NA), # v1.0
    Sex_calculated = c("F", "M", "C", NA), # v1.1
    Sex_genetic = c("F", "M", "C", NA), # v1.1
    speciesID = species_codes$speciesID, # v1.2
    siteID = pop_codes$PopID, # v1.2
    ringStage = .data$RingAge, # v1.2
    sexGenetic = c("F", "M", "C", NA) # v1.2
  ),

  # Brood data
  Brood_data = tibble::lst(
    Species = species_codes$speciesID, # v1.0 & v1.1
    PopID = pop_codes$PopID, # v1.0 & v1.1
    ClutchType_observed = c("first", "second", "replacement", NA), # v1.0 & v1.1
    ClutchType_calculated = c("first", "second", "replacement", NA), # v1.0 & v1.1
    OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA), # v1.0 & v1.1
    ExperimentID = c("PHENOLOGY", "COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA), # v1.0 & v1.1
    speciesID = unique(species_codes$speciesID), # v1.2
    siteID = pop_codes$PopID, # v1.2
    observedClutchType = c("first", "second", "replacement", NA) # v1.2
  ),

  # Capture data
  Capture_data = tibble::lst(
    Species = species_codes$speciesID, # v1.0 & v1.1
    CapturePopID = pop_codes$PopID, # v1.0 & v1.1
    ReleasePopID = c(pop_codes$PopID, NA), # v1.0 & v1.1
    OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA), # v1.0 & v1.1
    ExperimentID = c("PHENOLOGY", "COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA), # v1.1
    speciesID = species_codes$speciesID, # v1.2
    captureSiteID = pop_codes$PopID, # v1.2
    releaseSiteID = c(pop_codes$PopID, NA) # v1.2
  ),

  # Location data
  Location_data = tibble::lst(
    LocationType = c("NB", "MN", NA), # v1.0 & v1.1
    Habitat = c("deciduous", "evergreen", "mixed", NA), # v1.0
    HabitatType = c("deciduous", "evergreen", "mixed", "urban", NA), # v1.1
    # FIXME: add location type categories when v1.2 is finalised
    #locationType = c(),
    habitatType = c("deciduous", "evergreen", "mixed", "urban", NA), # v1.2
  )

  # Measurement data
  # FIXME: add measurement type categories when v1.2 is finalised
  # Measurement_data = tibble::lst( # v1.2
  #   measurementType = c()
  # )

  # Experiment data
  # FIXME: add treatment type categories when v1.2 is finalised
  # Experiment_data = tibble::lst( # v1.2
  #   treatmentType = c()
  # )

)

# Save lists as internal data in R/sysdata.rda
usethis::use_data(key_variables, categorical_variables, internal = TRUE, overwrite = TRUE, compress = "xz")
