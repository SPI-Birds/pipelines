# Generate rough meta-data for Montpellier.
#
# Code used to get rough estimate of MON meta-data.
# This is not intended to be
# run, just to record how meta data were
# extracted previously.

# #Estimate MON meta-data from primary data
# MON <- run_pipelines(PopID = c("MUR", "PIR", "ROU", "MON", "MTV"), output_type = "R")
#
# #Determine approximate location for each population.
# #Just take the lat/long of the first nestbox in each pop
# MON$Location_data %>%
#   dplyr::filter(LocationType == "NB") %>%
#   dplyr::group_by(PopID) %>%
#   dplyr::summarise(Lat = first(Latitude), Long = first(Longitude))
#
# #Determine the max number of nest box locations is each population
# MON$Location_data %>%
#   dplyr::filter(LocationType == "NB") %>%
#   dplyr::group_by(PopID) %>%
#   dplyr::summarise(n = length(unique(LocationID)))
#
# #Determine first and last year of data collection from capture data
# MON$Capture_data %>%
#   dplyr::group_by(CapturePopID) %>%
#   dplyr::summarise(startyear = min(BreedingSeason),
#                    endyear = max(BreedingSeason))
#
# #Check for missing years
# MON$Capture_data %>%
#   dplyr::filter(CapturePopID == "MTV") %>%
#   dplyr::group_by(BreedingSeason) %>%
#   summarise(n = n()) %>%
#   print(n = Inf)
#
# #Check for experiments
# MON$Brood_data %>%
#   pull(ExperimentID) %>%
#   any()
#
# #Determine species caught
# MON$Capture_data %>%
#   dplyr::group_by(CapturePopID, Species) %>%
#   dplyr::summarise(n = n())
#
# #Check for winter ringing
# MON$Capture_data %>%
#   dplyr::group_by(CapturePopID) %>%
#   dplyr::summarise(winter_ring = any(lubridate::month(CaptureDate) %in% c(12)))
#
# #Determine area for all populations
# Nestbox_data <- MON$Location_data %>%
#   dplyr::filter(LocationType == "NB" & !is.na(Latitude)) %>%
#   sp::SpatialPointsDataFrame(data = ., coords = .[, c("Longitude", "Latitude")], proj4string = sp::CRS("+init=epsg:4326")) %>%
#   sp::spTransform(CRSobj = sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
#
# #Split by population
# pop_split <- split(Nestbox_data, as.factor(Nestbox_data$PopID))
#
# #For each population.
# #Determine the convex hull of all nestbox locations and determine the area
# purrr::map_df(.x = pop_split, .f = function(data){
#
#   study_site_polygon <- rgeos::gConvexHull(data)
#
#   area <- rgeos::gArea(study_site_polygon)
#
#   return(tibble::tibble(PopID = first(data$PopID), Area = area))
#
# })
#
# purrr::walk(.x = pop_split, function(data){
#
#   rgdal::writeOGR(data, dsn = ".", layer = paste(first(data$PopID), "points", sep = "_"),
#                   driver = "ESRI Shapefile")
#
# })
