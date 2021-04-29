## Capture data template
capture_data_template <- tibble(CaptureID = NA_character_, ## Missing data not allowed
                                IndvID = NA_character_, ## Missing data not allowed
                                Species = NA_character_, ## Missing data not allowed
                                Sex_observed = NA_character_,
                                BreedingSeason = NA_integer_, ## Missing data not allowed
                                CaptureDate = as.Date(NA), ## Missing data not allowed
                                CaptureTime = NA_character_,
                                ObserverID = NA_character_,
                                LocationID = NA_character_,
                                CaptureAlive = NA,
                                ReleaseAlive = NA,
                                CapturePopID = NA_character_, ## Missing data not allowed
                                CapturePlot = NA_character_,
                                ReleasePopID = NA_character_, ## Missing data only allowed if ReleaseAlive is False
                                ReleasePlot = NA_character_, ## Missing data only allowed if ReleaseAlive is False
                                Mass = NA_real_,
                                Tarsus = NA_real_,
                                OriginalTarsusMethod = NA_character_,
                                WingLength = NA_real_,
                                Age_observed = NA_integer_,
                                Age_calculated = NA_integer_,
                                ChickAge = NA_integer_,
                                ExperimentID = NA_character_)

## Save
save(capture_data_template, file = "./data/Capture_data_template.rda")

########################################################################
########################################################################

## Individual data template
individual_data_template <- tibble(IndvID = NA_character_, ## Missing data not allowed
                                Species = NA_character_, ## Missing data not allowed
                                PopID = NA_character_, ## Missing data not allowed
                                BroodIDLaid = NA_character_,
                                BroodIDFledged = NA_character_,
                                RingSeason = NA_integer_,
                                RingAge = NA_character_,
                                Sex_calculated = NA_character_,
                                Sex_genetic = NA_character_)

## Save
save(individual_data_template, file = "./data/Individual_data_template.Rda")

########################################################################
########################################################################

## Brood data template
brood_data_template <- tibble(BroodID = NA_character_, ## Missing data not allowed
                              PopID = NA_character_, ## Missing data not allowed
                              BreedingSeason = NA_integer_, ## Missing data not allowed
                              Species = NA_character_, ## Missing data not allowed
                              Plot = NA_character_,
                              LocationID = NA_character_,
                              FemaleID = NA_character_,
                              MaleID = NA_character_,
                              ClutchType_observed = NA_character_,
                              ClutchType_calculated = NA_character_,
                              LayDate_observed = as.Date(NA),
                              LayDate_min = as.Date(NA),
                              LayDate_max = as.Date(NA),
                              ClutchSize_observed = NA_integer_,
                              ClutchSize_min = NA_integer_,
                              ClutchSize_max = NA_integer_,
                              HatchDate_observed = as.Date(NA),
                              HatchDate_min = as.Date(NA),
                              HatchDate_max = as.Date(NA),
                              BroodSize_observed = NA_integer_,
                              BroodSize_min = NA_integer_,
                              BroodSize_max = NA_integer_,
                              FledgeDate_observed = as.Date(NA),
                              FledgeDate_min = as.Date(NA),
                              FledgeDate_max = as.Date(NA),
                              NumberFledged_observed= NA_integer_,
                              NumberFledged_min = NA_integer_,
                              NumberFledged_max = NA_integer_,
                              AvgEggMass = NA_real_,
                              NumberEggs = NA_integer_,
                              AvgChickMass = NA_real_,
                              NumberChicksMass = NA_integer_,
                              AvgTarsus = NA_real_,
                              NumberChicksTarsus = NA_integer_,
                              OriginalTarsusMethod = NA_character_,
                              ExperimentID = NA_character_)

## Save
save(brood_data_template, file = "./data/Brood_data_template.Rda")

########################################################################
########################################################################

## Location
location_data_template <- tibble(LocationID = NA_character_, ## Missing data not allowed
                                 NestboxID = NA_character_, ## Missing data  allowed  for some species
                                 LocationType = NA_character_, ## Missing data not allowed
                                 PopID = NA_character_, ## Missing data not allowed
                                 Latitude = NA_real_,
                                 Longitude = NA_real_,
                                 StartSeason = NA_integer_,
                                 EndSeason = NA_integer_,
                                 HabitatType = NA_character_)

## Save
save(location_data_template, file = "./data/Location_data_template.Rda")
