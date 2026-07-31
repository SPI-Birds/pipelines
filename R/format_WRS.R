#' Construct standard format for data from Warsaw, Poland
#'
#' A pipeline to produce the standard format for the nest box population in Warsaw,
#' Poland, administered by Marta Szulkin.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard format please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' \strong{Species}: Only PARMAJ and CYACAE are available.
#'
#' \strong{IndvID}: IndvID codes have 7 characters and start with letter "K".
#' Chicks and adults with no ring/no ID were removed from the dataset
#' FIXME There are 3 adult rings with an extra character - asking data custodian about it
#'
#' \strong{CaptureDate}: For chicks, CaptureDate is inferred based on D15Date column.
#' Sometimes, this column is empty or NA. CaptureDate is thus inferred from Hd (hatching date).
#'
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

  # The version of the standard protocol on which this pipeline is based
  protocol_version <- "1.1.0"

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
    sheet = "nests",
    col_types = "text"
  ) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    # Reformat and rename columns
    dplyr::mutate(
      BreedingSeason = as.integer(.data$Year),
      LocationID = as.character(.data$NestboxId),
      Plot = as.character(.data$Site),
      LayDate_observed = suppressWarnings(
        as.Date(
          as.numeric(.data$LAyDAte),
          origin = as.Date(
            paste0(.data$Year, "-03-31")
          )
        )
      ),
      HatchDate_observed = suppressWarnings(
        as.Date(
          as.numeric(.data$Hd),
          origin = as.Date(
            paste0(.data$Year, "-03-31")
          )
        )
      ),
      ClutchSize_observed = suppressWarnings(
        as.integer(.data$Cs)
      ),
      BroodSize_observed = suppressWarnings(
        as.integer(.data$NrHAtched)
      ),
      NumberFledged_observed = suppressWarnings(
        as.integer(.data$NrFledged)
      ),
      Latitude = round(as.numeric(.data$Lat), digits = 5),
      Longitude = round(as.numeric(.data$Long), digits = 5)
    ) %>%
    # Recode column information
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(., "NA")),
      PopID = "WRS",
      Species = dplyr::case_when(
        .data$Species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
        .data$Species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
        .default = NA_character_
      ),
      # TODO: Check about camera and experiments
      # Current version does not seem to have any experiments included
      ExperimentID = NA_character_,
      NumberEggs = dplyr::case_when(!is.na(.data$EggMAssTot) ~ as.integer(.data$NrEggsWeighed),
                                    TRUE ~ NA_integer_),
      AvgEggMass = dplyr::case_when(!is.na(.data$EggMAssTot) ~ suppressWarnings(as.numeric(.data$EggMAssTot)),
                                    TRUE ~ NA_real_),
      # End mutate
    ) %>%
    ## Arrange
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID, as.Date(.data$LayDate_observed, format = "%Y-%m-%d")) %>%
    ## Create a variable to distinguish unique breeding events (some errors detcted in UniqueBreedingEvent)
    dplyr::group_by(.data$Year, .data$LocationID) %>%
    dplyr::mutate(broodID2 = paste(.data$Year, .data$LocationID, 1:dplyr::n(), sep = "_")) %>%
    dplyr::ungroup() %>%
    ## Select variables of interest
    dplyr::select(
      "BreedingSeason",
      "PopID",
      "Plot",
      "LocationID",
      "Species",
      "LayDate_observed",
      "HatchDate_observed",
      "ClutchSize_observed",
      "BroodSize_observed",
      "NumberFledged_observed",
      "NumberEggs",
      "AvgEggMass",
      "ExperimentID",
      "Latitude",
      "Longitude",
      "UniqueBreedingEvent",
      "broodID2"
    )

  ## Read in primary data from chicks

  chick_data_temp <- suppressWarnings(readxl::read_xlsx(
    path = paste0(db, "/WRS_PrimaryData.xlsx"),
    guess = 5000,
    sheet = "chicks",
    .name_repair = "minimal"
  )) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                                ~ dplyr::na_if(., "NA"))) %>%
    dplyr::filter(!is.na(.data$D15Date) & .data$D15Date != "") %>% # Drop records without a d15 date
    dplyr::rename(
      BreedingSeason = "Year",
      Plot = "Site",
      LocationID = "NestboxId",
      IndvID = "RingId",
    ) %>%
    # Handling different date formats in Excel
    # If chicks die before banding, the CaptureDate is set to the last day it was handled.
    dplyr::mutate(HatchDate_observed = suppressWarnings(as.Date(as.numeric(.data$Hd),
                                                                origin = as.Date(paste0(.data$BreedingSeason, "-03-31")))),
                  CaptureDate = suppressWarnings(dplyr::case_when(
                    grepl("-|/", .data$D15Date) ~ lubridate::mdy(.data$D15Date, quiet = TRUE),
                    !is.na(janitor::excel_numeric_to_date(as.numeric(.data$D15Date))) ~ lubridate::ydm(janitor::excel_numeric_to_date(as.numeric(.data$D15Date)), quiet = TRUE),
                    !is.na(.data$WeightD15) ~ .data$HatchDate_observed + 14L,
                    !is.na(.data$HatchDate_observed) ~ .data$HatchDate_observed,
                    .default = as.Date(NA_character_)
                  )
                  )) %>%
    dplyr::filter(!is.na(.data$CaptureDate)) %>%
    ## Adjust variables
    dplyr::mutate(
      PopID = "WRS",
      BreedingSeason = as.integer(.data$BreedingSeason),
      CaptureAlive = TRUE,
      ReleaseAlive =  TRUE,
      Tarsus = suppressWarnings(as.numeric(.data$TarsusD15)),
      Mass = suppressWarnings(round(as.numeric(dplyr::case_when(
        !is.na(.data$WeightD15) ~ .data$WeightD15,
        .default = NA_character_
      )), 3)),
      ChickAge = dplyr::case_when(
        !is.na(.data$WeightD15) ~ 15L,
        .default = NA_integer_
      ),
      Species = dplyr::case_when(
        .data$Species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
        .data$Species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
        .default = NA_character_
      )
    )

  ## Create new rows for every chick that did not fledge
  chick_dead <- chick_data_temp %>%
    dplyr::filter(.data$Fledged == "0" & !is.na(.data$D15Date)) %>%
    dplyr::mutate(CaptureDate = as.Date(.data$CaptureDate, format = "%Y-%m-%d") + lubridate::days(10), # considering they were found dead when checking fledging event (~10 days after banding chicks)
                  CaptureAlive = FALSE,
                  ReleaseAlive = FALSE)

  ## Bind new dataframe to existed one (on chicks)
  chick_data <- dplyr::bind_rows(chick_data_temp, chick_dead)  %>%
    dplyr::filter(!is.na(.data$IndvID) & stringr::str_detect(.data$IndvID, "^K")) %>% #discard individuals with no ID
    dplyr::select(
      "BreedingSeason",
      "PopID",
      "Plot",
      "LocationID",
      "Species",
      "IndvID",
      "CaptureDate",
      "Mass",
      "Tarsus",
      "ChickAge",
      "CaptureAlive",
      "ReleaseAlive",
      "UniqueBreedingEvent")

  ## Read in primary data from adults
  #TODO: ask data custodian about ObserverID
  adult_data <- suppressWarnings(readxl::read_xlsx(
    path = paste0(db, "/WRS_PrimaryData.xlsx"),
    sheet = "parents",
    col_types = "text"
  )) %>%
    janitor::clean_names(case = "upper_camel") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::filter(!toupper(.data$RingId) %in% c("NORING", "NOTRINGED")) %>% # Drop rows without a ring
    dplyr::rename(
      BreedingSeason = "Year",
      Plot = "Site",
      LocationID = "NestboxId",
      IndvID = "RingId",
      Sex_observed = "Sex",
      Age_observed = "Age",
      Mass = "Weight"
    ) %>%
    dplyr::mutate(
      #Clean multiple formats for dates in 3 steps
      #Step 1 - convert every value as a date with format YYYY-MM-DD
      Date_temp1 = suppressWarnings(dplyr::case_when(
        grepl("[-/]", .data$Date) ~ lubridate::mdy(.data$Date, quiet = TRUE),
        grepl("[.]", .data$Date) ~ lubridate::dmy(.data$Date, quiet = TRUE),
        .default = suppressWarnings(janitor::excel_numeric_to_date(as.numeric(.data$Date)))
      )) %>% as.Date(),
      #Step 2 - detect spurious dates (Excel numeric format where month and day were automatically and falsely swapped by Excel)
      Date_temp2 = dplyr::case_when(grepl("[-/.]", .data$Date) ~ .data$Date_temp1,
                                    TRUE ~ lubridate::ydm(.data$Date_temp1, quiet = TRUE)) %>% as.Date(),
      #Step 3 - reassign correct dates
      CaptureDate = dplyr::case_when(is.na(.data$Date_temp2) ~ .data$Date_temp1,
                                     TRUE ~ .data$Date_temp2),
      dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(., "NA")),
      PopID = "WRS",
      BreedingSeason = as.integer(.data$BreedingSeason),
      dplyr::across(c("Mass", "WingLength", "Tarsus"), ~ suppressWarnings(as.numeric(.x))),
      CaptureTime = suppressWarnings(dplyr::case_when(
        grepl(":", .data$Hour) ~ as.character(.data$Hour),
        .default = format(as.POSIXct(Sys.Date() + as.numeric(.data$Hour)), "%H:%M", tz = "UTC")
      )),
      Species = dplyr::case_when(
        .data$Species == "GT" ~ species_codes[species_codes$speciesEURINGCode == 14640, ]$Species,
        .data$Species == "BT" ~ species_codes[species_codes$speciesEURINGCode == 14620, ]$Species,
        .default = NA_character_
      ),
      ReleaseAlive = TRUE,
      Age_observed = dplyr::case_when(
        .data$Age_observed == 2 ~ 5L,
        toupper(.data$Age_observed) == "PO2" ~ 6L,
        .default = suppressWarnings(as.integer(.data$Age_observed))
      ),
      ExperimentID = NA_character_,
      dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(., "NA"))
    ) %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::select(
      "BreedingSeason",
      "PopID",
      "Plot",
      "LocationID",
      "Species",
      "IndvID",
      "CaptureDate",
      "CaptureTime",
      "Sex_observed",
      "Age_observed",
      "Mass",
      "WingLength",
      "Tarsus",
      "ReleaseAlive",
      "ExperimentID",
      "UniqueBreedingEvent"
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
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Brood_data))) %>%
    ## Add missing columns
    dplyr::bind_cols(data_templates[["v1.1.0"]]$Brood_data[0, !(names(data_templates[["v1.1.0"]]$Brood_data) %in% names(.))] %>%
                       tibble::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Brood_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(dplyr::if_all(
      c("BroodID", "PopID", "BreedingSeason", "Species"), ~ !is.na(.)
    ))

  # ## Check column classes
  # purrr::map_df(data_templates$1.1.0$Brood_data, class) == purrr::map_df(Brood_data, class)


  ## Capture data
  Capture_data <- Capture_data_temp %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Capture_data))) %>%
    ## Add missing columns
    dplyr::bind_cols(data_templates[["v1.1.0"]]$Capture_data[0, !(names(data_templates[["v1.1.0"]]$Capture_data) %in% names(.))] %>%
                       tibble::add_row()) %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Capture_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(
      dplyr::if_all(c(
        "CaptureID",
        "CapturePopID",
        "BreedingSeason",
        "IndvID",
        "Species",
        "CaptureDate"
      ), ~ !is.na(.))
    )


  # ## Check column classes
  # purrr::map_df(data_templates[["1.1.0"]]$Capture_data, class) == purrr::map_df(Capture_data, class)


  ## Individual data
  Individual_data <- Individual_data_temp %>%
    ## Keep only necessary columns
    dplyr::select(dplyr::contains(names(data_templates[["v1.1.0"]]$Individual_data))) %>%
    ## Add missing template columns
    {
      missing_cols <- setdiff(names(data_templates[["v1.1.0"]]$Individual_data), names(.))
      dplyr::mutate(., !!!stats::setNames(rep(list(NA), length(missing_cols)), missing_cols))
    } %>%
    ## Reorder columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Individual_data)) %>%
    dplyr::ungroup() %>%
    ## Remove any NAs from critical columns
    dplyr::filter(dplyr::if_all(
      c("PopID", "IndvID", "Species", "RingSeason"), ~ !is.na(.)
    ))


  # ## Check column classes
  # purrr::map_df(data_templates[["1.1.0"]]$Individual_data, class) == purrr::map_df(Individual_data, class)

  ## Location data
  Location_data <- Location_data_temp %>%
    ## Keep only template columns that exist
    dplyr::select(dplyr::any_of(names(data_templates[["v1.1.0"]]$Location_data))) %>%
    ## Add missing template columns
    {
      missing_cols <- setdiff(names(data_templates[["v1.1.0"]]$Location_data), names(.))
      dplyr::mutate(., !!!stats::setNames(rep(list(NA), length(missing_cols)), missing_cols))
    } %>%
    ## Reorder and keep only template columns
    dplyr::select(names(data_templates[["v1.1.0"]]$Location_data)) %>%
    dplyr::ungroup()




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
      Location_data = Location_data,
      protocol_version = protocol_version
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
    dplyr::filter(!is.na(.data$UniqueBreedingEvent) & !is.na(.data$Species)) %>%
    dplyr::left_join(
      adult_data %>%
        dplyr::select(
          "UniqueBreedingEvent",
          "Sex_observed",
          "IndvID"
        ) %>%
        stats::na.omit() %>%
        ## A few cases where the same individuals were caught multiple times for a single breeding event
        ## Keeping only distinct records by breeding event and sex
        dplyr::distinct(.data$UniqueBreedingEvent, .data$Sex_observed, .keep_all = T) %>%
        tidyr::pivot_wider(
          id_cols = "UniqueBreedingEvent",
          values_from = "IndvID",
          names_from = "Sex_observed"
        ) %>%
        dplyr::rename(
          FemaleID = "F",
          MaleID = "M"
        ),
      by = c("broodID2" = "UniqueBreedingEvent")
    ) %>%
    dplyr::arrange(.data$PopID, .data$BreedingSeason, .data$Plot, .data$LocationID) %>%
    ## Create BroodID
    dplyr::mutate(BroodID = paste(.data$Plot, 1:dplyr::n(), sep = "-")) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., protocol_version = "1.1", na.rm = FALSE)) %>%
    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Brood_data)), dplyr::everything())

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
    dplyr::arrange(.data$BreedingSeason, .data$IndvID, as.Date(.data$CaptureDate, format = "%Y-%m-%d")) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::mutate(CaptureID = paste(.data$IndvID, dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup() %>%
    ## Reorder columns
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Capture_data)), dplyr::everything())


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
      .default = NA_character_
    )) %>%
    ## Only join BroodID to chick records
    dplyr::left_join(
      Brood_data_temp %>%
        dplyr::mutate(brood_record = "yes") %>%
        dplyr::select(
          "brood_record",
          "broodID2",
          "BroodID"
        ),
      by = c("brood_record", c("UniqueBreedingEvent" = "broodID2"))
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
    dplyr::select(dplyr::any_of(names(data_templates[["1.1.0"]]$Individual_data)), dplyr::everything())


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
    dplyr::reframe(
      NestboxID = .data$LocationID,
      LocationType = "NB",
      StartSeason = min(.data$BreedingSeason, na.rm = TRUE), #TODO Check with data custodian if there is a better estimation
      EndSeason = as.integer(2025), #TODO Ensure with data custodian

      ## Keep lat/lon with the most digits for each box
      Latitude = as.numeric(.data$Latitude[which.max(nchar(.data$Latitude))]),
      Longitude = as.numeric(.data$Longitude[which.max(nchar(.data$Longitude))]),

      ## TODO: Match vegetation type with plots based on data owner input
      HabitatType = dplyr::case_when(
        .data$Plot == "CMZ" ~ "urban",
        .data$Plot == "KPN" ~ "evergreen",
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
