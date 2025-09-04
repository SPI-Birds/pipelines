#'Construct standard format for data from Hochstadt, Germany.
#'
#'A pipeline to produce the standard format for the hole nesting bird
#'populations in Hochstadt, Germany administered by Max Plank Institute
#'for Ornithology, Seewiesen (Michaela Hau).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{AvgEggMass:} Clutch mass is recorded in many cases; however, these measurements are
#'taken on or after the start of incubation. As egg mass can change throughout the period of
#'incubation we have not used these data.
#'
#'\strong{Plot:} There is no distinct plot information in Hochstadt. Plot is left blank.
#'
#'\strong{LocationID:} Captures can occur on nest boxes (e.g., with trap, while brooding, at feeder)
#'or with mistnets. The location of all non-nestbox trapping is located next to a known nestbox.
#'Therefore, we simply use nestbox as the LocationID for all captures, even when the capture didn't
#'occur within the nestbox itself. Therefore, Location data only includes location information
#'for nestbox sites.
#'
#'\strong{Age_observed:} All captures listed as 'nestling' are given a EURING code of 1 (i.e. unable to fly).
#'Captures listed as 'adult' can be either '1st year' or 'adult'. We treat '1st year' as birds known to
#'be in their first reproductive season (i.e. 2nd year of life; EURING 5) while 'adult' are birds known to be after
#'hatched before this season, but exact age unknown (i.e. EURING 4). Some cases are listed as '1st year?'.
#'These are treated the same as '1st year'.
#'
#'\strong{ChickAge:} Chick age is sometimes stored with uncertainty (e.g. 14/15). In all these cases we
#'take the lower age.
#'
#'\strong{ExperimentID:} Manipulation of individuals is recorded with each capture. This includes
#'hormonal injections and attaching backpacks. We treat any brood as having been experimented on
#'if any type of manipulation was recorded on any individual associated with a given brood.
#'\strong{Note:} at this moment, ExperimentID is simply recorded as TRUE/FALSE while we try to
#'categorise all experiments.
#'
#'@inheritParams pipeline_params
#'
#'@return 4 data tables in the standard format (version 1.1.1). When `output_type = "R"`, a list of 4 data frames corresponding to the 4 standard data tables and 1 character vector indicating the protocol version on which the pipeline is based. When `output_type = "csv"`, 4 .csv files corresponding to the 4 standard data tables and 1 text file indicating the protocol version on which the pipeline is based.
#'@export
#' 
format_HOC_1.1 <- function(db = choose_directory(),
                      species = NULL,
                      pop = NULL,
                      path = ".",
                    	output_type = "R"){

	## BOILERPLATE												

	# The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

  # Force choose_directory() if used
	force(db)

  start_time <- Sys.time()

  message("Importing primary data...")

	# Determine species and population codes for filtering
  if(is.null(species)){

    species_filter <- NULL

  } else {

    species_filter <- species

  }

	# Otherwise, use the specified pop filter
  if(is.null(pop)){

    pop_filter <- NULL

  } else {

    pop_filter <- pop

  }

	## Set options
  options(dplyr.summarise.inform = FALSE)

	## RUNNING THE FUNCTION TO RETRIEVE THE DATA

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_HOC(db = db)

	# CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_HOC(db = db)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_HOC(db = db)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_HOC(db = db)

	## FUNCTIONS

	#' Create brood data table for Hochstadt.
	#'
	#' @param db Location of primary data from Hochstadt.
	#'
	#' @return A data frame with Brood data

	create_brood_HOC <- function(db){

		#We read everything in as text and convert it afterwards
		#Even though some columns (e.g. date) work well, they may be broken with newer data.
		#Using text and converting manually should be more robust to data changes
		#They include egg mass, but this is always after incubation so it is not included (we only take egg weight before incubation)
		Brood_data <- readxl::read_excel(path = paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Nests_ID", na = c("", "na"),
																		col_types = "text") %>%
			janitor::clean_names() %>%
			dplyr::mutate(BroodID = .data$unique_nest_id,
										PopID = "HOC",
										BreedingSeason = .data$year,
										Species = "PARMAJ",
										Plot = NA_character_,
										LocationID = paste0("H", .data$nestbox_no),
										FemaleID = .data$social_female_bird_id,
										MaleID = .data$social_male_bird_id,
										ClutchType_observed = .data$clutch_no,
										ClutchType_calculated = NA_character_,
										BreedingSeason = as.integer(.data$year),
										
										# We need this column because otherwise the function calc_clutchtype will not run
										LayingDate = janitor::excel_numeric_to_date(as.numeric(.data$x1st_egg_lay_date)),

										LayingDate_observed = janitor::excel_numeric_to_date(as.numeric(.data$x1st_egg_lay_date)),
										# lay_date_error is zero in the original data, so we do not assign min and max lay dates
										LayingDate_min = NA_character_,
										LayingDate_max = NA_character_,
										
										ClutchSize_observed = as.integer(.data$clutch_size),
										# clutch_size_error is zero in the original data, so we do not assign min and max lay dates
										LayingDate_min = NA_character_,
										ClutchSize_min = NA_character_,
										ClutchSize_max = NA_character_,
										
										HatchDate_observed = janitor::excel_numeric_to_date(as.numeric(.data$hatch_date)),
										# hatch_date_error is zero in the original data, so we do not assign min and max hatch dates
										HatchDate_min = NA_character_,
										HatchDate_max = NA_character_,
										HatchDateError = as.numeric(.data$hatch_date_error),
										
										BroodSize_observed = as.integer(.data$hatch_number),
										# hatch_number_error is zero in the original data, so we do not assign min and max lay dates
										BroodSize_min = NA_integer_,
										BroodSize_max = NA_integer_,
										
										FledgeDate = janitor::excel_numeric_to_date(as.numeric(.data$fledge_date)),
										# fledge_number_error is zero in the original data, so we do not assign min and max lay dates
										FledgeDate_min = NA_integer_,
										FledgeDate_max = NA_integer_,

										NumberFledged = as.integer(.data$fledge_number),
										# fledge_date_error is zero in the original data, so we do not assign min and max lay dates
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
										
										) %>%
			dplyr::arrange("BreedingSeason", "FemaleID", "LayingDate_observed") %>%
			dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%

			# Remove the temporary LayingDate column needed to calculate the clutchtype column
			dplyr::select(-LayingDate) %>%

			# Select the correct columns that we want to keep
			# Order should be correct
			dplyr::select("BroodID":"ExperimentID")

		return(Brood_data)

	}

	#' Create capture data table for Hochstadt.
	#'
	#' @param db Location of primary data from Hochstadt.
	#'
	#' @return A data frame with Capture data

	create_capture_HOC <- function(db){

		# We want to have sex available, sex can be found in another sheet, load that sheet
		bird_id_sex_observed <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Bird_ID", na = c("", "na"),
																			col_types = "text") %>%
																			dplyr::select(RingNumber, Sex_observed)

		# Keep only the unique records, as some are duplicates
		bird_id_sex_observed_unique <- bird_id_sex_observed %>%
  		distinct(RingNumber, .keep_all = TRUE)

		Capture_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Capture ID", na = c("", "na"),
			col_types = "text")

		# Join the dataframes, so we have sex_observed available in our capture data
		Capture_data <- Capture_data %>%
  		left_join(bird_id_sex_observed_unique, by = c("BirdID" = "RingNumber"))																	

		Capture_data %>%
			janitor::clean_names() %>%
			dplyr::mutate(
										CaptureID = .data$capture_id,
										IndvID = .data$bird_id,
										# Is BroodID needed here?
										BroodID = .data$nest_id,
										Species = "PARMAJ",
										# We take sex from another sheet
										Sex_observed = .data$sex_observed,
										BreedingSeason = year(janitor::excel_numeric_to_date(as.numeric(Capture_data$Date))),
										CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date)),										
										CaptureTime = paste0(stringr::str_pad(string = (as.numeric(.data$time_capture) * (24*60)) %/% 60,
																													width = 2, pad = "0"),
																				":", stringr::str_pad(string = round((as.numeric(.data$time_capture) * (24*60)) %% 60),
																															width = 2, pad = "0")),
										ObserverID = .data$measures_taken_by,
										LocationID = purrr::map_chr(.x = .data$nest_location,
																								.f = ~{
																									if(is.na(..1)){
																										return(NA_character_)
																									} else {
																										boxnumber <- stats::na.omit(dplyr::na_if(stringr::str_split_1(..1, "[^0-9]+"), ""))
																										return(paste0("H", boxnumber))

																									}

																								}),
										# We do know causes of dead, but, not at which capture event the bird died
										# thus, we can not ascertain if a bird was captured alive and released alive
										CaptureAlive = NA_character_,
										ReleaseAlive = NA_character_,
										CapturePopID = "HOC",
										CapturePlot = NA_character_,
										ReleasePopID = "HOC",
										ReleasePlot = NA_character_,

										Mass = as.numeric(.data$mass_g),
										
										Tarsus = as.numeric(.data$tarsus_length_mm),
										OriginalTarsusMethod = "Alternative",
										
										WingLength = as.numeric(.data$wing_length_mm),

										) %>%
			
			dplyr::bind_cols(., purrr::pmap_df(.l = list(age_exact = .$age_exact,
																									age_simple = .$age_simple,
																									BreedingSeason = .$BreedingSeason),

																				function(age_exact, age_simple, BreedingSeason){

																				if(age_simple == "nestling"){

																					if(age_exact == "nestling" | is.na(age_exact)){

																						return(tibble::tibble(Age_observed = 1L, ChickAge = NA_integer_))

																					} else {

																						return(tibble::tibble(Age_observed = 1L,
																																	ChickAge = as.integer(stringr::str_split(age_exact, pattern = "/")[[1]][1])))

																					}

																				} else {

																					return(tibble::tibble(Age_observed = dplyr::case_when(grepl("ADULT", toupper(age_exact)) ~ 4L,
																																																grepl("1ST YEAR", toupper(age_exact)) & BreedingSeason >= 2019 ~ 5L,
																																																grepl("1ST YEAR", toupper(age_exact)) & BreedingSeason < 2019 ~ 4L),
																																																ChickAge = NA_integer_))

																				}

																			})) %>%
			dplyr::rowwise() %>%
			dplyr::mutate(ExperimentID = any(c(.data$physical_manipulation_present_at_time_of_catching,
																				.data$physical_manipulation_present_at_time_of_release,
																				.data$physiological_manipulation) %in% "manipulated")) %>%
			dplyr::ungroup()

		Death_captures <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "DeadBirds ID", na = c("", "na"),
																				col_types = "text") %>%
			janitor::clean_names() %>%
			dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(.data$date_found)),
										IndvID = .data$ringnumber) %>%
			#Find cases where that individual was not recorded captured on that date
			dplyr::left_join(Capture_data %>%
												dplyr::mutate(in_capt = TRUE) %>%
												dplyr::select("CaptureDate", "IndvID", "in_capt"),
											by = c("CaptureDate", "IndvID")) %>%
			dplyr::filter(is.na(.data$in_capt)) %>%
			dplyr::mutate(Species = "PARMAJ",
										BreedingSeason = lubridate::year(.data$CaptureDate),
										CaptureTime = NA_character_,
										ObserverID = NA_character_,
										LocationID = NA_character_,
										CapturePopID = "HOC",
										CapturePlot = NA_character_,
										ReleasePopID = "HOC",
										ReleasePlot = NA_character_,
										Mass = NA_real_,
										Tarsus = NA_real_,
										OriginalTarsusMethod = NA_character_,
										WingLength = NA_real_,
										Age_observed = dplyr::case_when(.data$age == "adult" ~ 4L,
																										.data$age == "nestling" ~ 1L),
										ChickAge = NA_integer_,
										BroodID = NA_character_,
										ExperimentID = NA,
										FoundDead = TRUE,
										capture_method = NA_character_)

		Capture_data_combined <- dplyr::bind_rows(Capture_data, Death_captures) %>%
			dplyr::arrange(.data$IndvID, .data$BreedingSeason, .data$CaptureDate, .data$CaptureTime) %>%
			calc_age(ID = .data$IndvID, Age = .data$Age_observed,
							Date = .data$CaptureDate, Year = .data$BreedingSeason)  %>%
			dplyr::select("IndvID", "Species", "BreedingSeason", "CaptureDate", "CaptureTime",
										"ObserverID", "LocationID", "CapturePopID", "CapturePlot", "ReleasePopID", "ReleasePlot",
										"Mass", "Tarsus", "OriginalTarsusMethod", "WingLength", "Age_observed",
										"Age_calculated", "ChickAge", "FoundDead", "BroodID", "ExperimentID", "capture_method")

		return(Capture_data_combined)

	}

	#' Create individual data table for Hochstadt.
	#'
	#' @param db Location of individual data from Hochstadt.
	#'
	#' @return A data frame with Individual data

	create_individual_HOC <- function(db){

		#Technically, they already have individual data in a separate table
		#However, we will check this in comparison to capture data
		Individual_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Bird_ID", na = c("", "na"),
																			col_types = "text") %>%
			janitor::clean_names() %>%
			dplyr::mutate(IndvID = .data$ring_number,
										Species = "PARMAJ",
										Sex = dplyr::case_when(.data$sex == "female" ~ "F",
																					.data$sex == "male" ~ "M"),
										PopID = "HOC",
										RingSeason = lubridate::year(janitor::excel_numeric_to_date(as.numeric(.data$date_ringed))),
										RingAge = dplyr::case_when(.data$age_simple == "adult" ~ "adult",
																							.data$age_simple == "nestling" ~ "chick"),
										BroodIDLaid = .data$nest_of_origin_id,
										BroodIDFledged = .data$rearing_nest_id) %>%
			dplyr::select("IndvID", "Species", "PopID", "BroodIDLaid", "BroodIDFledged",
										"RingSeason", "RingAge", "Sex")

		return(Individual_data)

	}

	#' Create location data table for Hochstadt.
	#'
	#' @param db Location of location data from Hochstadt.
	#'
	#' @return A data frame with Location data

	create_location_HOC <- function(db){

		Location_data <- readxl::read_excel(paste0(db, "/HOC_PrimaryData.xlsx"), sheet = "Location Data", na = c("", "na"),
																				col_types = "text") %>%
			janitor::clean_names() %>%
			dplyr::mutate(LocationID = paste0("H", .data$nestbox_number),
										NestboxID = .data$LocationID,
										LocationType = "NB",
										PopID = "HOC",
										Latitude = as.numeric(.data$latitude),
										Longitude = as.numeric(.data$longitude),
										StartSeason = 2014L,
										EndSeason = NA_integer_,
										Habitat = "mixed")

		return(Location_data)

	}

}
