#' Construct standard format for data from Glimmen, the Netherlands
#'
#' A pipeline to produce the standard format for the study site at
#' Glimmen, the Netherlands, administred by Simon Verhulst (University of Groningen).
#'
#' This pipeline is built using SPI-Birds' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf}{standard format v2.0.0}.
#'
#' This section provides details on data management choices that are unique to these data.
#'
#' \strong{speciesID}: Species are not explicitly recorded, but we assume that all individuals are successfully identified as Eurasian jackdaws.
#'
#' \strong{observedClutchSize}: Broods with less than 0 eggs are excluded. Check with data owner.
#'
#' \strong{Capture data, Individual data}: Only individual-level data on breeding individuals is recorded.
#'
#' \strong{captureYear}: captureYear is assumed to be equal to year of breeding. captureMonth and captureDay are unknown, but could possibly be deduced from observedLayMonth and observedLayDay. Check with data owner.
#'
#' \strong{exactAge, minimumAge}: As a result of captureMonth and captureDay being unknown, age cannot be determined using the standard utility function \code{\link{calc_age}}.
#'
#' \strong{captureTagID, releaseTagID}: Check with data owner for information on ring numbers (or other tags).
#'
#' \strong{captureAlive, releaseAlive}: All individuals are assumed to be captured and released alive.
#'
#' \strong{capturePhysical}: All captures are assumed to be physical captures.
#'
#' \strong{tagStage}: All individuals are assumed to be tagged as 'adults'. Check with data owner.
#'
#' \strong{tagYear}: All individuals are assumed to be tagged in the first year the appear in the dataset. Check with data owner.
#'
#' \strong{tagSiteID}: All individuals are assumed to be tagged at Glimmen. Check with data owner.
#'
#' \strong{Location data}: No location information is available, resulting in an empty Location data table.
#'
#' \strong{Measurement data}: No measurements were taken, resulting in an empty Measurement data table.
#'
#' \strong{Experiment data}: No experiments were conducted, resulting in an empty Experiment data table.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format (v2.0.0).
#'
#' @export
#'

format_GLI <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)) {

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the developer & user
  start_time <- Sys.time()

  # Load data
  message("Importing primary data...")

  all_data <- readxl::read_excel(path = paste0(db, "/GLI_PrimaryData.xlsx")) %>%
    # Convert all column names to snake case
    janitor::clean_names() %>%
    dplyr::mutate(siteID = "GLI",
                  # Ensure unique plotIDs; add siteID as prefix
                  # TODO: Check whether 'colony' is interpreted correctly
                  plotID = paste0("GLI_", .data$colony)) %>%
    # We assume that all records are successfully identified as Eurasian jackdaws
    dplyr::mutate(speciesID = species_codes$speciesID[species_codes$speciesCode == "10015"]) %>%
    # Convert dates
    dplyr::mutate(dplyr::across(.cols = c("ld", "hd"),
                                .fns = ~{

                                  as.Date(.x)

                                })) %>%
    # Filter species
    dplyr::filter(.data$speciesID %in% {species})


  # BROOD DATA
  message("Compiling brood data...")

  Brood_data <- create_brood_GLI(data = all_data,
                                 optional_variables = optional_variables)


  # CAPTURE DATA
  message("Compiling capture data...")

  Capture_data <- create_capture_GLI(brood_data = Brood_data,
                                     optional_variables = optional_variables)


  # INDIVIDUAL DATA
  message("Compiling individual data...")

  Individual_data <- create_individual_GLI(capture_data = Capture_data,
                                           optional_variables = optional_variables)


  # LOCATION DATA

  # NB: There is no location information so we create an empty data table
  Location_data <- data_templates$v2.0$Location_data[0,]


  # MEASUREMENT DATA

  # NB: There is no measurement information so we create an empty data table
  Measurement_data <- data_templates$v2.0$Measurement_data[0,]


  # EXPERIMENT DATA

  # NB: There is no experiment information so we create an empty data table
  Experiment_data <- data_templates$v2.0$Experiment_data[0,]


  # WRANGLE DATA FOR EXPORT

  Brood_data <- Brood_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Brood_data[1, !(names(data_templates$v2.0$Brood_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Brood_data), dplyr::contains(names(utility_variables$Brood_data),
                                                                         ignore.case = FALSE))

  Capture_data <- Capture_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Capture_data[1, !(names(data_templates$v2.0$Capture_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Capture_data), dplyr::contains(names(utility_variables$Capture_data),
                                                                           ignore.case = FALSE))

  Individual_data <- Individual_data %>%
    # Add row ID
    dplyr::mutate(row = 1:n()) %>%
    # Add missing columns
    dplyr::bind_cols(data_templates$v2.0$Individual_data[1, !(names(data_templates$v2.0$Individual_data) %in% names(.))]) %>%
    # Keep only columns that are in the standard format or in the list of optional variables
    dplyr::select(names(data_templates$v2.0$Individual_data), dplyr::contains(names(utility_variables$Individual_data),
                                                                              ignore.case = FALSE))

  # TIME

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))


  # OUTPUT

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_GLI.csv"), row.names = FALSE)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_GLI.csv"), row.names = FALSE)

    utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_GLI.csv"), row.names = FALSE)

    utils::write.csv(x = Measurement_data, file = paste0(path, "\\Measurement_data_GLI.csv"), row.names = FALSE)

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_GLI.csv"), row.names = FALSE)

    utils::write.csv(x = Experiment_data, file = paste0(path, "\\Experiment_data_GLI.csv"), row.names = FALSE)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Measurement_data = Measurement_data,
                Location_data = Location_data,
                Experiment_data = Experiment_data))

  }

}

#' Create brood data table for Glimmen, the Netherlands.
#'
#' Create brood data table in the standard format (v2.0.0) for data from Glimmen, the Netherlands.
#'
#' @param data Data frame. Primary data from Glimmen, the Netherlands.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_brood_GLI <- function(data,
                             optional_variables) {

  broods <- data %>%
    dplyr::rename(broodID = "jd_nest",
                  femaleID = "fem_id",
                  maleID = "male_id",
                  observedLayDate = "ld",
                  observedClutchSize = "cs",
                  observedHatchDate = "hd") %>%
    # Remove broods with observedClutchSize < 0
    # TODO: Check with data owner
    dplyr::filter(.data$observedClutchSize >= 0 | is.na(.data$observedClutchSize)) %>%
    # femaleID & maleID are unique numbers, except when negative then:
    # -1: unringed individual
    # -2: ringed but unidentified individual
    # -3: unknown
    # As these codes are not unique, we cannot use them to identify individuals,
    # so in these cases we set femaleID/maleID to NA.
    # When femaleID/maleID is non-NA add site prefix (GLI) to ensure that IDs are unique across SPI-Birds' study sites
    dplyr::mutate(dplyr::across(.cols = c("femaleID", "maleID"),
                                .fns = ~{

                                  dplyr::case_when(.x < 0 ~ NA_character_,
                                                   TRUE ~ paste0("GLI_", .x))

                                }),
                  observedLayYear = dplyr::case_when(is.na(.data$observedLayDate) ~ as.integer(.data$year),
                                                     TRUE ~ as.integer(lubridate::year(.data$observedLayDate))),
                  observedLayMonth = as.integer(lubridate::month(.data$observedLayDate)),
                  observedLayDay = as.integer(lubridate::day(.data$observedLayDate)),
                  observedHatchYear = dplyr::case_when(is.na(.data$observedHatchDate) ~ as.integer(.data$year),
                                                       TRUE ~ as.integer(lubridate::year(.data$observedHatchDate))),
                  observedHatchMonth = as.integer(lubridate::month(.data$observedHatchDate)),
                  observedHatchDay = as.integer(lubridate::day(.data$observedHatchDate)))

  # Add optional variables
  output <- broods %>%
    {if("breedingSeason" %in% optional_variables) calc_season(data = ., season = .data$year) else .} %>%
    # calculatedClutchType cannot be provided as number of fledglings are not recorded
    #{if("calculatedClutchType" %in% optional_variables) calc_clutchtype(data = ., na.rm = FALSE, protocol_version = "2.0") else .} %>%
    {if("nestAttemptNumber" %in% optional_variables) calc_nestattempt(data = ., season = .data$breedingSeason) else .}

  return(output)

}

#' Create capture data table for Glimmen, the Netherlands.
#'
#' Create capture data table in the standard format (v2.0.0) for data from Glimmen, the Netherlands.
#'
#' @param brood_data Data frame. Output from \code{\link{create_brood_GLI}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_capture_GLI <- function(brood_data,
                               optional_variables) {

  captures <- brood_data %>%
    # Pivot femaleID & maleID into rows
    tidyr::pivot_longer(cols = c("femaleID", "maleID"),
                        names_to = "sex",
                        values_to = "individualID") %>%
    # Remove unknown individuals
    dplyr::filter(!is.na(.data$individualID)) %>%
    # Little to no capture information available for individuals
    # For each individual we assume that captureYear == year of breeding
    dplyr::rename(captureYear = "year",
                  captureSiteID = "siteID",
                  capturePlotID = "plotID") %>%
    dplyr::mutate(observedSex = dplyr::case_when(grepl(pattern = "^f", x = .data$sex) ~ "F",
                                                 grepl(pattern = "^m", x = .data$sex) ~ "M"),
                  age = "adult",
                  releaseSiteID = .data$captureSiteID,
                  releasePlotID = .data$capturePlotID,
                  # TODO: Individuals are assumed to be captured alive, without replacing rings
                  captureAlive = TRUE,
                  releaseAlive = TRUE,
                  capturePhysical = TRUE,
                  captureMonth = NA_integer_,
                  captureDay = NA_integer_,
                  chickAge = NA_integer_) %>%
    # Arrange chronologically per individual
    dplyr::arrange(.data$individualID, .data$captureYear, .data$observedLayMonth, .data$observedLayDay) %>%
    # TODO: Check with data owner for tag IDs (e.g., ring numbers)
    # Create captureID
    dplyr::group_by(.data$individualID) %>%
    dplyr::mutate(captureID = paste(.data$individualID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup()

  # Add optional variables
  output <- captures %>%
    {if("exactAge" %in% optional_variables | "minimumAge" %in% optional_variables) calc_age(data = .,
                                                                                            Age = .data$age,
                                                                                            Year = .data$captureYear,
                                                                                            protocol_version = "2.0") %>%
        dplyr::select(dplyr::contains(c(names(captures), optional_variables))) else .}

  return(output)

}

#' Create individual data table for Glimmen, the Netherlands.
#'
#' Create individual data table in the standard format (v2.0.0) for data from Glimmen, the Netherlands.
#'
#' @param capture_data Data frame. Output from \code{\link{create_capture_GLI}}.
#' @param optional_variables A character vector of names of optional variables (generated by standard utility functions) to be included in the pipeline output.
#'
#' @return A data frame.
#'

create_individual_GLI <- function(capture_data,
                                  optional_variables) {

  # Create a list of individuals from capture_data
  individuals <- capture_data %>%
    # arrange data for each individual chronologically
    dplyr::arrange(.data$individualID, .data$captureYear) %>%
    # For every individual...
    dplyr::group_by(.data$individualID) %>%
    # ... determine stage/timing/site at first capture
    dplyr::summarise(tagStage = dplyr::first(.data$age),
                     tagYear = dplyr::first(.data$captureYear),
                     tagSiteID = dplyr::first(.data$captureSiteID),
                     speciesID = dplyr::first(.data$speciesID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(siteID = "GLI")

  # Add optional variables
  output <- individuals %>%
    {if("calculatedSex" %in% optional_variables) calc_sex(individual_data = .,
                                                          capture_data = capture_data) else .}

}

#----------------------#
#TODO: Does Colony correspond with plotID?
#TODO: Is there location information (locationIDs and geographic coordinates)?
#TODO: Are there any measurements on breeding individuals or chicks?
#TODO: What does Status  mean?
#TODO: How to interpret broods with LD and CS == 0?
#TODO: How to interpret broods with LD == NA and CS == 0
#TODO: How to interpret broods with CS < 0?
#TODO: Ask for ring numbers
#TODO: Ask for ring dates
#TODO: Are chicks ringed?
#TODO: Verify captureAlive, releaseAlive, capturePhysical
#TODO: Verify tagStage, tagYear, tagSiteID
#TODO: Verify conflicted sex individuals (6 cases)
