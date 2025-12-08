#' Construct standard format for data from Warsaw, Poland
#'
#' A pipeline to produce the standard format for the nest box population in Warsaw,
#' Poland, administered by Marta Szulkin.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' \strong{Species}: Only PARMAJ and CYACAE are entered in the Capture and Individual tables.
#'
#' \strong{IndvID}: IndvID codes of the form '19XX' (two numbers + XX) indicate a chick that died before fledging.
#' The first two numbers give the year in which the chick died. There is one adult band
#' that also includes 'XX' so this should not be used to filter out these records. The
#' regular expression "^[:digit:]{2}XX" along with stringr::str_detect can be used to
#' identify and filter out these records.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 4 .csv files or 4 data frames in the standard format.
#' @export

format_WRS <- function(
    db = choose_directory(),
    path = ".",
    species = NULL,
    pop = NULL,
    output_type = "R") {
    # Force choose_directory() if used
    force(db)

    start_time <- Sys.time()

    message("Importing primary data...")

    # Force user to select directory
    force(db)

    # Determine species and population codes for filtering
    if (is.null(species)) {
        species_filter <- NULL
    } else {
        species_filter <- species
    }

    if (is.null(pop)) {
        pop_filter <- NULL
    } else {
        pop_filter <- pop
    }

    start_time <- Sys.time()

    # Set options
    options(dplyr.summarise.inform = FALSE)

    # Read in primary data from nest sheet
    # Note: some columns do not have a header name
    nest_data <- readxl::read_xlsx(
        path = paste0(db, "/WRS_PrimaryData.xlsx"),
        guess_max = 5000,
        sheet = "Nests",
        col_types = "text"
    ) %>%
        janitor::clean_names() %>%
        janitor::remove_empty(which = "rows") %>%
        # Reformat and rename columns
        dplyr::mutate(
            BreedingSeason = as.integer(.data$year),
            LocationID = as.character(.data$nestbox_id),
            Plot = as.character(.data$site),
            LayDate_observed = suppressWarnings(
                as.Date(
                    as.numeric(.data$lp),
                    origin = as.Date(
                        paste0(.data$year, "-03-31")
                    )
                )
            ),
            HatchDate_observed = suppressWarnings(
                as.Date(
                    as.numeric(.data$hd),
                    origin = as.Date(
                        paste0(.data$year, "-03-31")
                    )
                )
            ),
            ClutchSize_observed = suppressWarnings(
                as.integer(.data$cs)
            ),
            BroodSize_observed = suppressWarnings(
                as.integer(.data$nr_hatched)
            ),
            NumberFledged_observed = suppressWarnings(
                as.integer(.data$nr_fledged)
            ),
            Latitude = as.numeric(.data$lat),
            Longitude = as.numeric(.data$long)
        ) %>%
        # Recode column information
        dplyr::mutate(
            dplyr::across(where(is.character), ~ dplyr::na_if(., "NA")),
            PopID = "WRS",
            # TODO: Current version does not have more species then listed below
            Species = dplyr::case_when(
                .data$species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
                .data$species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
                .data$species == "FC" ~ species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                .data$species == "NUT" ~ species_codes[species_codes$speciesEURINGCode == 14790, ]$Species,
                .data$species == "CT" ~ species_codes[species_codes$speciesEURINGCode == 14610, ]$Species,
                .data$species == "RS" ~ species_codes[species_codes$speciesEURINGCode == 11220, ]$Species,
                .data$species == "SP" ~ species_codes[species_codes$speciesEURINGCode == 15980, ]$Species,
                TRUE ~ NA_character_
            ),
            # TODO: Check about camera and experiments
            # Current version does not seem to have any experiments included
            ExperimentID = NA_character_,
            # TODO: current version does not have weighted eggs
            NumberEggs = NA_integer_,
            AvgEggMass = NA_real_
            # End mutate
        ) %>%
        ## Arrange
        dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%
        ## Select variables of interest
        dplyr::select(
            BreedingSeason,
            PopID,
            Plot,
            LocationID,
            Species,
            LayDate_observed,
            HatchDate_observed,
            ClutchSize_observed,
            BroodSize_observed,
            NumberFledged_observed,
            NumberEggs,
            AvgEggMass,
            ExperimentID,
            Latitude,
            Longitude,
            unique_breeding_event
        )

    ## Read in primary data from chicks
    chick_data <- suppressWarnings(readxl::read_xlsx(
        path = paste0(db, "/WRS_PrimaryData.xlsx"),
        guess = 5000,
        sheet = "Chicks",
        .name_repair = "minimal"
    )) %>%
        janitor::clean_names() %>%
        janitor::remove_empty(which = "rows") %>%
        ## Rename variables
        ## TODO: Check about capture dates for dead chicks
        dplyr::rename(
            BreedingSeason = .data$year,
            Plot = .data$site,
            LocationID = .data$nestbox_id,
            IndvID = .data$ring_id,
            Tarsus = .data$tarsus_d15
        ) %>%
        dplyr::left_join(nest_data[, c("unique_breeding_event", "HatchDate_observed")],
            by = "unique_breeding_event"
        ) %>%
        # Handling different date formats in Excel
        # If chicks die before banding, the CaptureDate is set to the last day it was handled.
        # TODO: check why we now only have weight on day 15, and not other days
        dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "NA")),
            CaptureDate = suppressWarnings(dplyr::case_when(
                grepl("-|/", .data$d15_date) ~ lubridate::dmy(.data$d15_date, quiet = TRUE),
                !is.na(janitor::excel_numeric_to_date(as.numeric(.data$d15_date))) ~ janitor::excel_numeric_to_date(as.numeric(.data$d15_date)),
                !is.na(.data$weight_d15) ~ .data$HatchDate_observed + 15L,
                !is.na(.data$HatchDate_observed) ~ .data$HatchDate_observed,
                TRUE ~ lubridate::NA_Date_
            ))
        ) %>%
        ## Adjust variables
        ## TODO: Check about 'dead chick' ID codes
        dplyr::mutate(
            PopID = "WRS",
            BreedingSeason = as.integer(.data$BreedingSeason),
            ReleaseAlive = dplyr::case_when(
                .data$chick_exp != 0 | .data$chick_pred != 0 | .data$fledged == 0 ~ FALSE,
                TRUE ~ TRUE
            ),
            Tarsus = suppressWarnings(as.numeric(.data$tarsus)),
            Mass = suppressWarnings(round(as.numeric(dplyr::case_when(
                !is.na(.data$weight_d15) ~ .data$weight_d15,
                !is.na(.data$weight_d10) ~ .data$weight_d10,
                !is.na(.data$weight_d5) ~ .data$weight_d5,
                !is.na(.data$weight_d2) ~ .data$weight_d2,
                TRUE ~ NA_character_
            )), 3)),
            ChickAge = dplyr::case_when(
                !is.na(.data$weight_d15) ~ 15L,
                !is.na(.data$weight_d10) ~ 10L,
                !is.na(.data$weight_d5) ~ 5L,
                !is.na(.data$weight_d2) ~ 2L,
                TRUE ~ NA_integer_
            ),
            Species = dplyr::case_when(
                .data$species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
                .data$species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
                .data$species == "FC" ~ species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                .data$species == "NUT" ~ species_codes[species_codes$speciesEURINGCode == 14790, ]$Species,
                .data$species == "CT" ~ species_codes[species_codes$speciesEURINGCode == 14610, ]$Species,
                .data$species == "RS" ~ species_codes[species_codes$speciesEURINGCode == 11220, ]$Species,
                .data$species == "SP" ~ species_codes[species_codes$speciesEURINGCode == 15980, ]$Species,
                TRUE ~ NA_character_
            )
        ) %>%
        dplyr::select(
            .data$BreedingSeason,
            .data$PopID,
            .data$Plot,
            .data$LocationID,
            .data$Species,
            .data$IndvID,
            .data$CaptureDate,
            .data$Mass,
            .data$Tarsus,
            .data$ChickAge,
            .data$ReleaseAlive,
            .data$unique_breeding_event
        )

    ## Read in primary data from adults
    ## TODO: Ask about dates with 'BIB'
    adult_data <- suppressWarnings(readxl::read_xlsx(
        path = paste0(db, "/WRS_PrimaryData.xlsx"),
        sheet = "Adults",
        col_types = "text"
    )) %>%
        janitor::clean_names() %>%
        janitor::remove_empty(which = "rows") %>%
        dplyr::rename(
            BreedingSeason = year,
            Plot = site,
            LocationID = nestbox_id,
            IndvID = ring_id,
            ObserverID = obs,
            Sex_observed = sex,
            Age_observed = age,
            Mass = weight,
            WingLength = wing_length,
            Tarsus = tarsus
        ) %>%
        dplyr::mutate(CaptureDate = suppressWarnings(dplyr::case_when(
            grepl("/", .data$date) ~ lubridate::dmy(.data$date, quiet = TRUE),
            TRUE ~ janitor::excel_numeric_to_date(as.numeric(.data$date))
        ))) %>%
        dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "NA")),
            PopID = "WRS",
            BreedingSeason = as.integer(.data$breeding_season),
            dplyr::across(c(.data$Mass, .data$wing_length, .data$tarsus), ~ suppressWarnings(as.numeric(.x))),
            CaptureTime = suppressWarnings(case_when(
                grepl(":", .data$hour) ~ as.character(.data$hour),
                TRUE ~ format(as.POSIXct(Sys.Date() + as.numeric(.data$hour)), "%H:%M", tz = "UTC")
            )),
            Species = dplyr::case_when(
                .data$species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
                .data$species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
                .data$species == "FC" ~ species_codes[species_codes$speciesEURINGCode == 13490, ]$Species,
                .data$species == "NUT" ~ species_codes[species_codes$speciesEURINGCode == 14790, ]$Species,
                .data$species == "CT" ~ species_codes[species_codes$speciesEURINGCode == 14610, ]$Species,
                .data$species == "RS" ~ species_codes[species_codes$speciesEURINGCode == 11220, ]$Species,
                .data$species == "SP" ~ species_codes[species_codes$speciesEURINGCode == 15980, ]$Species,
                TRUE ~ NA_character_
            ),
            ReleaseAlive = dplyr::case_when(
                .data$adult_exp == 1 ~ FALSE,
                TRUE ~ TRUE
            ),
            Age_observed = dplyr::case_when(
                .data$Age_observed == 2 ~ 5L,
                toupper(.data$Age_observed) == "PO2" ~ 6L,
                TRUE ~ as.integer(.data$Age_observed)
            ),
            ExperimentID = dplyr::case_when(
                !is.na(.data$aggress) ~ "OTHER",
                TRUE ~ NA_character_
            ),
            dplyr::across(where(is.character), ~ dplyr::na_if(., "NA"))
        ) %>%
        dplyr::select(
            BreedingSeason,
            PopID,
            Plot,
            LocationID,
            Species,
            IndvID,
            CaptureDate,
            CaptureTime,
            Sex_observed,
            Age_observed,
            Mass,
            WingLength,
            Tarsus,
            ReleaseAlive,
            ObserverID,
            ExperimentID,
            unique_breeding_event
        )

    #### BROOD DATA
    message("Compiling brood information...")
    Brood_data_temp <- create_brood_WRS(nest_data, chick_data, adult_data)

    #### CAPTURE DATA
    message("Compiling capture information...")
    Capture_data_temp <- create_capture_WRS(chick_data, adult_data)

    #### INDIVIDUAL DATA
    message("Compiling individual information...")
    Individual_data_temp <- create_individual_WRS(Capture_data_temp, Brood_data_temp)

    #### LOCATION DATA
    message("Compiling location information...")
    Location_data_temp <- create_location_WRS(nest_data)

    time <- difftime(Sys.time(), start_time, units = "sec")

    message(paste0("All tables generated in ", round(time, 2), " seconds"))


    #### PROCESSING FINAL DATA TO EXPORT

    ## Brood data
    Brood_data <- Brood_data_temp %>%
        ## Keep only necessary columns
        dplyr::select(dplyr::contains(names(brood_data_template))) %>%
        ## Add missing columns
        dplyr::bind_cols(brood_data_template[, !(names(brood_data_template) %in% names(.))]) %>%
        ## Reorder columns
        dplyr::select(names(brood_data_template))

    # ## Check column classes
    # purrr::map_df(brood_data_template, class) == purrr::map_df(Brood_data, class)


    ## Capture data
    Capture_data <- Capture_data_temp %>%
        ## Keep only necessary columns
        dplyr::select(dplyr::contains(names(capture_data_template))) %>%
        ## Add missing columns
        dplyr::bind_cols(capture_data_template[, !(names(capture_data_template) %in% names(.))]) %>%
        ## Reorder columns
        dplyr::select(names(capture_data_template))

    # ## Check column classes
    # purrr::map_df(capture_data_template, class) == purrr::map_df(Capture_data, class)


    ## Individual data
    Individual_data <- Individual_data_temp %>%
        ## Keep only necessary columns
        dplyr::select(dplyr::contains(names(individual_data_template))) %>%
        ## Add missing columns
        dplyr::bind_cols(individual_data_template[, !(names(individual_data_template) %in% names(.))]) %>%
        ## Reorder columns
        dplyr::select(names(individual_data_template))

    # ## Check column classes
    # purrr::map_df(individual_data_template, class) == purrr::map_df(Individual_data, class)

    ## Location data
    Location_data <- Location_data_temp %>%
        ## Keep only necessary columns
        dplyr::select(dplyr::contains(names(location_data_template))) %>%
        ## Add missing columns
        dplyr::bind_cols(location_data_template[, !(names(location_data_template) %in% names(.))]) %>%
        ## Reorder columns
        dplyr::select(names(location_data_template))

    # ## Check column classes
    # purrr::map_df(location_data_template, class) == purrr::map_df(Location_data, class)



    ## Filter to keep only desired Species if specified for Brood, Capture, and Individual tables
    if (!is.null(species_filter)) {
        Brood_data <- Brood_data %>%
            dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

        Capture_data <- Capture_data %>%
            dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))

        Individual_data <- Individual_data %>%
            dplyr::filter(.data$Species %in% species_filter & !(is.na(.data$Species)))
    }

    ## Filter to keep only desired Pops if specified for Brood, Capture, Individual, and Location tables
    if (!is.null(pop_filter)) {
        nest_data <- nest_data %>%
            dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

        Capture_data <- Capture_data %>%
            dplyr::filter(.data$CapturePopID %in% pop_filter & !(is.na(.data$CapturePopID)))

        Individual_data <- Individual_data %>%
            dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))

        Location_data <- Location_data %>%
            dplyr::filter(.data$PopID %in% pop_filter & !(is.na(.data$PopID)))
    }

    #### EXPORT DATA

    if (output_type == "csv") {
        message("Saving .csv files...")

        utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_WRS.csv"), row.names = F)

        utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_WRS.csv"), row.names = F)

        utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_WRS.csv"), row.names = F)

        utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_WRS.csv"), row.names = F)

        invisible(NULL)
    }

    if (output_type == "R") {
        message("Returning R objects...")

        return(list(
            Brood_data = Brood_data,
            Capture_data = Capture_data,
            Individual_data = Individual_data,
            Location_data = Location_data
        ))
    }
}

#### --------------------------------------------------------------------------~
#### FUNCTIONS
#### --------------------------------------------------------------------------~


#' Create brood data table in Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @param chick_data Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_brood_WRS <- function(nest_data, chick_data, adult_data) {
    ## Combine primary data
    ## TODO: Check on tarsus method
    ## TODO: Check on clutch type observed
    Brood_data_temp <- nest_data %>%
        ## Keep only records with sufficient information
        dplyr::filter(!is.na(.data$unique_breeding_event) & !is.na(.data$Species)) %>%
        dplyr::left_join(
            adult_data %>%
                dplyr::select(
                    .data$unique_breeding_event,
                    .data$Sex_observed,
                    .data$IndvID
                ) %>%
                stats::na.omit() %>%
                ## A few cases where the same individuals were caught multiple times for a single breeding event
                ## Keeping only distinct records by breeding event and sex
                ## TODO: Check about whether this is robust
                dplyr::distinct(.data$unique_breeding_event, .data$Sex_observed, .keep_all = T) %>%
                tidyr::pivot_wider(
                    id_cols = .data$unique_breeding_event,
                    values_from = .data$IndvID,
                    names_from = .data$Sex_observed
                ) %>%
                dplyr::rename(
                    FemaleID = "F",
                    MaleID = "M"
                ),
            by = "unique_breeding_event"
        ) %>%
        dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%
        ## Create BroodID
        dplyr::mutate(BroodID = paste(.data$Plot, 1:dplyr::n(), sep = "-")) %>%
        dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE)) %>%
        ## Reorder columns
        dplyr::select(dplyr::any_of(names(brood_data_template)), dplyr::everything())

    return(Brood_data_temp)
}

#' Create capture data table for Warsaw, Poland.
#'
#' @param chick_data, Data frame of chick ringing records from Warsaw, Poland.
#'
#' @param adult_data, Data frame of adult ringing records from Warsaw, Poland.
#'
#' @return A data frame.

create_capture_WRS <- function(chick_data, adult_data) {
    ## All chicks with IndvIDs containing 'XX' died before fledging
    ## TODO: Check on dropping these, they currently don't have a CaptureDate
    Capture_data_temp <- adult_data %>%
        dplyr::mutate(
            RingAge_temp = "adult",
            CaptureAlive = TRUE
        ) %>%
        ## Bind chick data
        dplyr::bind_rows(chick_data %>%
            dplyr::mutate(
                RingAge_temp = "chick",
                CaptureAlive = dplyr::case_when(
                    grepl("XX", .data$IndvID) & is.na(.data$Mass) ~ FALSE,
                    TRUE ~ TRUE
                ),
                Age_observed = 1L
            )) %>%
        ## Create new columns
        dplyr::mutate(
            CapturePopID = .data$PopID,
            ReleasePopID = .data$PopID,
            CapturePlot = .data$Plot,
            ReleasePlot = .data$Plot
        ) %>%
        ## Arrange
        dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
        ## Calculate age
        dplyr::group_by(.data$IndvID) %>%
        calc_age(
            ID = .data$IndvID,
            Age = .data$Age_observed,
            Date = .data$CaptureDate,
            Year = .data$BreedingSeason
        ) %>%
        ## Create CaptureID
        ## Arrange
        dplyr::arrange(.data$BreedingSeason, .data$IndvID, .data$CaptureDate) %>%
        dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%
        ## Reorder columns
        dplyr::select(dplyr::any_of(names(capture_data_template)), dplyr::everything())


    return(Capture_data_temp)
}

#' Create individual table for Warsaw, Poland.
#'
#' @param Capture_data_temp Capture data output from Warsaw, Poland
#'
#' @param Brood_data_temp Brood data output from Warsaw, Poland
#'
#' @return A data frame.

create_individual_WRS <- function(Capture_data_temp, Brood_data_temp) {
    ## Create individual data
    Individual_data_temp <- Capture_data_temp %>%
        #### Format and create new data columns
        dplyr::group_by(.data$IndvID, .data$CapturePopID) %>%
        dplyr::mutate(PopID = .data$CapturePopID) %>%
        dplyr::group_by(.data$IndvID) %>%
        dplyr::mutate(RingSeason = min(.data$BreedingSeason, na.rm = TRUE)) %>%
        ## Arrange
        dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
        ## Determine individual info
        dplyr::mutate(
            Sex_calculated = purrr::map_chr(
                .x = list(unique(stats::na.omit(.data$Sex_observed))),
                .f = ~ {
                    if (length(..1) == 0) {
                        return(NA_character_)
                    } else if (length(..1) == 1) {
                        return(..1)
                    } else {
                        return("C")
                    }
                }
            ),
            Sex_genetic = NA_character_,
            Species = purrr::map_chr(
                .x = list(unique(stats::na.omit(.data$Species))),
                .f = ~ {
                    if (length(..1) == 0) {
                        return(NA_character_)
                    } else if (length(..1) == 1) {
                        return(..1)
                    } else {
                        return("CCCCCC")
                    }
                }
            ),
            RingAge = purrr::pmap_chr(
                .l = list(dplyr::first(.data$Age_observed)),
                .f = ~ {
                    if (is.na(..1)) {
                        return("adult")
                    } else if (..1 <= 3L) {
                        return("chick")
                    } else if (..1 > 3L) {
                        return("adult")
                    }
                }
            )
        ) %>%
        ## Join Brood data for Individuals banded as chicks
        dplyr::mutate(brood_record = dplyr::case_when(
            .data$RingAge == "chick" &
                .data$RingSeason == .data$BreedingSeason &
                !is.na(.data$LocationID) ~ "yes",
            TRUE ~ NA_character_
        )) %>%
        ## Only join BroodID to chick records
        dplyr::left_join(
            Brood_data_temp %>%
                dplyr::mutate(brood_record = "yes") %>%
                dplyr::select(
                    .data$brood_record,
                    .data$unique_breeding_event,
                    .data$BroodID
                ),
            by = c("brood_record", "unique_breeding_event")
        ) %>%
        ## Add BroodID information
        ## Only one unique (non NA) BroodID per individual
        dplyr::group_by(.data$IndvID) %>%
        dplyr::mutate(
            BroodIDLaid = purrr::map_chr(
                .x = list(unique(stats::na.omit(.data$BroodID))),
                .f = ~ {
                    if (length(..1) != 1) {
                        return(NA_character_)
                    } else if (length(..1) == 1) {
                        return(..1)
                    }
                }
            ),
            BroodIDFledged = .data$BroodIDLaid
        ) %>%
        ## Keep distinct records by PopID and InvdID
        dplyr::distinct(.data$PopID, .data$IndvID, .keep_all = TRUE) %>%
        ## Arrange
        dplyr::arrange(.data$CaptureID) %>%
        dplyr::ungroup() %>%
        ## Reorder columns
        dplyr::select(dplyr::any_of(names(individual_data_template)), dplyr::everything())


    return(Individual_data_temp)
}


#' Create location data table for Warsaw, Poland.
#'
#' @param nest_data Data frame of nest data from Warsaw, Poland.
#'
#' @return A data frame.

create_location_WRS <- function(nest_data) {
    ## Build location data based on nest data
    Location_data_temp <- nest_data %>%
        ## Need to first remove trailing 0s from Lat/Lon
        dplyr::mutate(
            Latitude = sub("^0+", "", .data$Latitude),
            Longitude = sub("^0+", "", .data$Longitude)
        ) %>%
        ## Summarize information for each nest box
        dplyr::group_by(.data$PopID, .data$LocationID) %>%
        dplyr::summarise(
            NestboxID = .data$LocationID,
            LocationType = "NB",
            StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
            EndSeason = NA_integer_,

            ## Keep lat/lon with the most digits for each box
            Latitude = as.numeric(.data$Latitude[which.max(nchar(.data$Latitude))]),
            Longitude = as.numeric(.data$Longitude[which.max(nchar(.data$Longitude))]),

            ## TODO: Match vegetation type with plots based on data owner input
            HabitatType = dplyr::case_when(
                .data$Plot == "CMZ" ~ "urban",
                .data$Plot == "KPN" ~ "urban",
                .data$Plot == "POL" ~ "urban",
                .data$Plot == "LOL" ~ "urban",
                .data$Plot == "MUR" ~ "urban",
                .data$Plot == "OLO" ~ "urban",
                .data$Plot == "PAL" ~ "urban",
                .data$Plot == "UNI" ~ "urban",
                .data$Plot == "BIB" ~ "urban"
            )
        ) %>%
        ## Keep distinct records
        dplyr::distinct(.data$PopID, .data$LocationID, .keep_all = TRUE) %>%
        dplyr::ungroup()

    return(Location_data_temp)
}
