#' Construct standard format for data from Zeromski Park and Kowale, Poland.
#'
#' A pipeline to produce the standard format for the Common Blackbird population
#' in Szczecin, Poland (sites: Zeromski Park and Kowale), administered by the
#' University of Szczecin.
#'
#' This section provides details on data management choices that are unique to
#' this data. For a general description of the standard protocol please see
#' \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.1.0.pdf}{here}.
#'
#' \strong{Species}: Only Common Blackbird (\emph{Turdus merula}, TURMER) is
#' present in this dataset.
#'
#' \strong{Sites}: Data covers two sites in Szczecin, Poland: Zeromski Park
#' (PopID \code{"ZER"}) and Kowale (PopID \code{"KOW"}). Use the \code{pop}
#' argument to restrict output to one site; if \code{pop} is \code{NULL} (the
#' default) both sites are included.
#'
#' \strong{Age}: The data distinguishes three biological age classes:
#' \code{chick} (ringed in the nest), \code{subadult} (first calendar year), and
#' \code{adult} (second calendar year or older). The source also contains the
#' label \code{pubadult}, which is treated as adult.
#'
#' \strong{ClutchType_observed}: The source column named \code{ClutchType}
#' contains the brood number within a breeding season (1--7), not the protocol's
#' clutch categories. In particular, a later brood number does not distinguish a
#' replacement clutch from a true second clutch. \code{ClutchType_observed} is
#' therefore set to \code{NA} for all records. \code{ClutchType_calculated} is
#' derived from laying dates and female identity using
#' \code{\link{calc_clutchtype}}.
#'
#' \strong{FledgeDate}: A systematic bulk-entry error in the supplied source
#' data sets \code{FledgeYear} to 2018 for many broods laid before 2018.
#' \code{FledgeDate_observed} (and its min/max) is set to \code{NA} for those
#' records. Dates are also removed whenever \code{FledgeYear} is more than one
#' year after \code{LayYear}.
#'
#' \strong{CaptureAlive, ReleaseAlive}: No mortality information is recorded.
#' Both are set to \code{TRUE} for all captures.
#'
#' \strong{LocationType}: Birds were captured by mist net at all locations.
#' The exception being chicks, which were handled in the nests.
#' \code{LocationType} is set to \code{"MN"} for alllocation records.
#'
#' \strong{Measurements}: Mass (g), tarsus length (mm), and wing length (mm) are
#' stored in a separate long-format sheet and joined to captures by
#' \code{captureID}. Some values use comma decimal notation (e.g. \code{35,5});
#' these are converted to standard decimal notation before coercion to numeric.
#'
#' \strong{LocationID}: The \code{locationID} field in the source data is used
#' directly as the SPI-Birds \code{LocationID}, representing the exact brood
#' location (i.e. nest site, e.g. \code{"1998-1"}) rather than a plot. Location
#' coordinates are only available at site level; all nest locations within a site
#' therefore share the same coordinates. The
#' within-site coordinate differences are negligible. The capture sheet's
#' \code{locationID} values are used as \code{LocationID} in
#' \code{Capture_data}; adults were mist-netted at these localities, while chicks
#' were handled at the nests.
#'
#' \strong{Missing species in captures}: 81 capture records have no
#' \code{speciesID} in the source data. As this is a single-species study, these
#' are assumed to be Common Blackbird and filled with \code{"TURMER"}.
#'
#' \strong{NumberFledged}: The source data records 0 fledglings for failed
#' broods. The protocol requires \code{NumberFledged_observed >= 1} or
#' \code{NA}; values of 0 are therefore converted to \code{NA}.
#'
#' \strong{Broods without LayYear}: Brood records with no \code{LayYear} (and
#' therefore no \code{BreedingSeason}) are excluded from \code{Brood_data}.
#'
#' \strong{BroodIDFledged}: No fledging brood information is recorded
#' separately. \code{BroodIDFledged} is assumed equal to \code{BroodIDLaid}
#' on the basis that blackbirds do not move between broods before fledging.
#'
#' \strong{Individuals not in individual sheet}: Some \code{IndvID} values
#' present in the capture data have no corresponding record in the individual
#' data sheet. For these, \code{Species}, \code{PopID}, and \code{RingSeason} are
#' derived from the earliest capture record; \code{RingAge} defaults to
#' \code{"adult"}.
#'
#' @inheritParams pipeline_params
#'
#' @return 4 data tables in the standard format (version 1.1.0). When
#'  \code{output_type = "R"}, a list of 4 data frames corresponding to the 4
#'  standard data tables and 1 character vector indicating the protocol version
#'  on which the pipeline is based. When \code{output_type = "csv"}, 4 .csv
#'  files corresponding to the 4 standard data tables and 1 text file indicating
#'  the protocol version on which the pipeline is based.
#' @export

format_ZER <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R") {
  protocol_version <- "1.1.0"

  force(db)

  if (is.null(species)) {
    species_filter <- species_codes$Species
  } else {
    species_filter <- species
  }

  pop_filter <- if (is.null(pop)) c("KOW", "ZER") else toupper(pop)

  start_time <- Sys.time()

  message("Importing primary data...")

  xl_path <- file.path(db, "ZER_PrimaryData.xlsx")

  brood_raw <- suppressWarnings(
    readxl::read_excel(
      xl_path,
      sheet = "Brood Blackbird",
      col_types = c(
        "text", "text", "text", "text", "text", "text", "text",
        "numeric", # ClutchType
        "numeric", "numeric", "numeric", # LayYear/Month/Day
        "numeric", # ClutchSize
        "numeric", "numeric", "numeric", # HatchYear/Month/Day
        "numeric", # BroodSize
        "numeric", "numeric", "numeric", # FledgeYear/Month/Day
        "numeric"
      )
    ) # NumberFledged
  )

  capture_raw <- suppressWarnings(
    readxl::read_excel(
      xl_path,
      sheet = "Capture Blackbird",
      col_types = c(
        "text", "text", "text", "text", # captureID, individualID, speciesID, Sex
        "numeric", "numeric", "numeric", # tagYear, tag1onth, tagDay
        "text", "text", "text", # siteID, plotID, locationID
        "numeric"
      )
    ) # chickAge
  )

  individual_raw <- readxl::read_excel(
    xl_path,
    sheet = "Ind Data Blackbird",
    range = readxl::cell_cols("A:I"),
    col_types = c(
      "text", "text", "text", "text",
      "text", "text", "text",
      "text", "text"
    )
  )

  measurement_raw <- suppressWarnings(
    readxl::read_excel(
      xl_path,
      sheet = "Measurement Blackbird",
      col_types = c(
        "text", "text", "text", # measurementID, captureID, siteID
        "text", "text", "text", # measurementType, measurementValue, measurementUnit
        "numeric", "numeric", "numeric"
      )
    ) # measurementYear/Month/Day
  )

  location_raw <- readxl::read_excel(xl_path, sheet = "Location Blackbirds")

  message("Compiling brood data...")
  Brood_data <- create_brood_ZER(brood_raw, species_filter, pop_filter, protocol_version)

  message("Compiling capture data...")
  Capture_data <- create_capture_ZER(capture_raw, measurement_raw, species_filter, pop_filter, protocol_version)

  message("Compiling individual data...")
  Individual_data <- create_individual_ZER(Capture_data, individual_raw, pop_filter, protocol_version)

  message("Compiling location data...")
  Location_data <- create_location_ZER(brood_raw, capture_raw, location_raw, pop_filter, protocol_version)

  time <- difftime(Sys.time(), start_time, units = "sec")
  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if (output_type == "csv") {
    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = file.path(path, "Brood_data_ZER.csv"), row.names = FALSE)
    utils::write.csv(x = Capture_data, file = file.path(path, "Capture_data_ZER.csv"), row.names = FALSE)
    utils::write.csv(x = Individual_data, file = file.path(path, "Individual_data_ZER.csv"), row.names = FALSE)
    utils::write.csv(x = Location_data, file = file.path(path, "Location_data_ZER.csv"), row.names = FALSE)
    utils::write.table(
      x = protocol_version, file = file.path(path, "protocol_version_ZER.txt"),
      quote = FALSE, row.names = FALSE, col.names = FALSE
    )

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

# Normalise siteID: uppercase and strip Polish diacritics (e.g. Ż -> Z)
normalise_site_ZER <- function(x) {
  toupper(iconv(trimws(x), to = "ASCII//TRANSLIT"))
}

#' Create brood data table for Zeromski Park / Kowale, Poland.
#'
#' @param data Data frame. Raw brood sheet from ZER_PrimaryData.xlsx.
#' @param species_filter Character vector. Species codes to include.
#' @param pop_filter Character vector. Site codes to include (\code{"KOW"}, \code{"ZER"}).
#' @param protocol_version Character string. Protocol version.
#'
#' @return A data frame.

create_brood_ZER <- function(data, species_filter, pop_filter, protocol_version) {
  Brood_data <- data %>%
    dplyr::mutate(
      siteID    = normalise_site_ZER(.data$siteID),
      speciesID = trimws(.data$speciesID)
    ) %>%
    dplyr::filter(
      .data$siteID %in% pop_filter,
      .data$speciesID %in% species_filter,
      !is.na(.data$LayYear)
    ) %>%
    dplyr::mutate(
      BroodID = .data$broodID,
      PopID = .data$siteID,
      BreedingSeason = as.integer(.data$LayYear),
      Species = .data$speciesID,
      Plot = NA_character_,
      LocationID = .data$locationID,
      FemaleID = dplyr::na_if(trimws(.data$femaleID), "NA"),
      MaleID = dplyr::na_if(trimws(.data$maleID), "NA"),
      # Raw ClutchType is brood number within season, not first/second/replacement
      ClutchType_observed = NA_character_,
      LayDate_observed = as.Date(
        paste(.data$LayYear, .data$LayMonth, .data$LayDay, sep = "-"),
        format = "%Y-%m-%d"
      ),
      ClutchSize_observed = as.integer(.data$ClutchSize),
      HatchDate_observed = as.Date(
        paste(.data$HatchYear, .data$HatchMonth, .data$HatchDay, sep = "-"),
        format = "%Y-%m-%d"
      ),
      BroodSize_observed = as.integer(.data$BroodSize),
      # Remove the confirmed 2018 bulk-entry error
      FledgeDate_observed = dplyr::case_when(
        is.na(.data$FledgeYear) |
          (.data$FledgeYear == 2018 & .data$LayYear < 2018) |
          (.data$FledgeYear - .data$LayYear) > 1 ~ as.Date(NA_character_),
        TRUE ~ as.Date(
          paste(.data$FledgeYear, .data$FledgeMonth, .data$FledgeDay, sep = "-"),
          format = "%Y-%m-%d"
        )
      ),
      # 0 fledglings = failed brood; protocol represents this as NA (min value is 1)
      NumberFledged_observed = dplyr::na_if(as.integer(.data$NumberFledged), 0L)
    ) %>%
    dplyr::mutate(
      ClutchType_calculated  = calc_clutchtype(., na.rm = FALSE, protocol_version = "1.1"),
      LayDate_min            = .data$LayDate_observed,
      LayDate_max            = .data$LayDate_observed,
      ClutchSize_min         = .data$ClutchSize_observed,
      ClutchSize_max         = .data$ClutchSize_observed,
      HatchDate_min          = .data$HatchDate_observed,
      HatchDate_max          = .data$HatchDate_observed,
      BroodSize_min          = .data$BroodSize_observed,
      BroodSize_max          = .data$BroodSize_observed,
      FledgeDate_min         = .data$FledgeDate_observed,
      FledgeDate_max         = .data$FledgeDate_observed,
      NumberFledged_min      = .data$NumberFledged_observed,
      NumberFledged_max      = .data$NumberFledged_observed,
      AvgEggMass             = NA_real_,
      NumberEggs             = NA_integer_,
      AvgChickMass           = NA_real_,
      NumberChicksMass       = NA_integer_,
      AvgTarsus              = NA_real_,
      NumberChicksTarsus     = NA_integer_,
      OriginalTarsusMethod   = NA_character_,
      ExperimentID           = NA_character_
    ) %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Brood_data[
        1, !(names(data_templates[[paste0("v", protocol_version)]]$Brood_data) %in% names(.))
      ]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Brood_data))

  return(Brood_data)
}

#' Create capture data table for Zeromski Park / Kowale, Poland.
#'
#' @param capture_data Data frame. Raw capture sheet from ZER_PrimaryData.xlsx.
#' @param measurement_data Data frame. Raw measurement sheet from ZER_PrimaryData.xlsx.
#' @param species_filter Character vector. Species codes to include.
#' @param pop_filter Character vector. Site codes to include.
#' @param protocol_version Character string. Protocol version.
#'
#' @return A data frame.

create_capture_ZER <- function(capture_data, measurement_data, species_filter, pop_filter, protocol_version) {
  # Pivot measurements from long to wide, handling comma decimal notation
  measurements_wide <- measurement_data %>%
    dplyr::mutate(
      measurementValue = as.numeric(gsub(",", ".", .data$measurementValue))
    ) %>%
    dplyr::select("captureID", "measurementType", "measurementValue") %>%
    tidyr::pivot_wider(
      names_from  = "measurementType",
      values_from = "measurementValue",
      values_fn   = mean
    ) %>%
    dplyr::rename(
      Tarsus     = "tarsus length",
      WingLength = "wing length",
      Mass       = "mass"
    )

  Capture_data <- capture_data %>%
    dplyr::mutate(
      siteID    = normalise_site_ZER(.data$siteID),
      # Fill NA speciesID: this is a blackbird-only study
      speciesID = dplyr::coalesce(trimws(.data$speciesID), "TURMER")
    ) %>%
    dplyr::filter(
      .data$siteID %in% pop_filter,
      .data$speciesID %in% species_filter
    ) %>%
    dplyr::left_join(measurements_wide, by = "captureID") %>%
    dplyr::mutate(
      CaptureID = .data$captureID,
      IndvID = .data$individualID,
      Species = .data$speciesID,
      Sex_observed = dplyr::na_if(trimws(.data$Sex), "NA"),
      BreedingSeason = as.integer(.data$tagYear),
      CaptureDate = as.Date(
        paste(.data$tagYear, .data$tag1onth, .data$tagDay, sep = "-"),
        format = "%Y-%m-%d"
      ),
      CaptureTime = NA_character_,
      ObserverID = NA_character_,
      LocationID = .data$locationID,
      CaptureAlive = TRUE,
      ReleaseAlive = TRUE,
      CapturePopID = .data$siteID,
      CapturePlot = .data$plotID,
      ReleasePopID = .data$siteID,
      ReleasePlot = .data$plotID,
      OriginalTarsusMethod = dplyr::case_when(
        !is.na(.data$Tarsus) ~ "Standard",
        TRUE ~ NA_character_
      ),
      # chick if chickAge recorded; otherwise age unknown at this capture
      Age_observed = dplyr::case_when(
        !is.na(.data$chickAge) ~ 1L,
        TRUE ~ NA_integer_
      ),
      ChickAge = dplyr::case_when(
        !is.na(.data$chickAge) ~ as.integer(.data$chickAge),
        TRUE ~ NA_integer_
      ),
      ExperimentID = NA_character_
    ) %>%
    calc_age(
      ID = IndvID, Age = Age_observed,
      Date = CaptureDate, Year = BreedingSeason
    ) %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Capture_data[
        1, !(names(data_templates[[paste0("v", protocol_version)]]$Capture_data) %in% names(.))
      ]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Capture_data))

  return(Capture_data)
}

#' Create individual data table for Zeromski Park / Kowale, Poland.
#'
#' @param Capture_data Data frame. Output from \code{\link{create_capture_ZER}}.
#' @param individual_data Data frame. Raw individual sheet from ZER_PrimaryData.xlsx.
#' @param pop_filter Character vector. Site codes to include.
#' @param protocol_version Character string. Protocol version.
#'
#' @return A data frame.

create_individual_ZER <- function(Capture_data, individual_data, pop_filter, protocol_version) {
  ind_clean <- individual_data %>%
    dplyr::mutate(
      siteID = normalise_site_ZER(.data$siteID),
      BroodIDLaid = dplyr::na_if(trimws(.data$broodIDLaid), "NA"),
      tagStage = trimws(tolower(.data$tagStage)),
      RingSeason = as.integer(.data$tagYear),
      Species = trimws(.data$speciesID),
      PopID = .data$siteID,
      RingAge = dplyr::case_when(
        .data$tagStage == "chick" ~ "chick",
        .data$tagStage %in% c("adult", "subadult", "pubadult") ~ "adult",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(.data$siteID %in% pop_filter) %>%
    dplyr::select("individualID", "Species", "PopID", "BroodIDLaid", "RingSeason", "RingAge")

  Individual_data <- Capture_data %>%
    dplyr::filter(!is.na(.data$IndvID)) %>%
    dplyr::arrange(.data$IndvID, .data$CaptureDate) %>%
    dplyr::group_by(.data$IndvID) %>%
    dplyr::summarise(
      # Derive key columns directly from captures as reliable fallback
      Species_cap = dplyr::first(stats::na.omit(.data$Species)),
      PopID_cap = dplyr::first(.data$CapturePopID),
      RingSeason_cap = dplyr::first(.data$BreedingSeason),
      RingAge_cap = dplyr::case_when(
        dplyr::first(.data$Age_observed) == 1L ~ "chick",
        TRUE ~ "adult"
      ),
      Sex_calculated = purrr::map_chr(
        .x = list(unique(.data$Sex_observed)),
        .f = ~ {
          if (all(c("F", "M") %in% ..1)) {
            return("C")
          } else if ("F" %in% ..1) {
            return("F")
          } else if ("M" %in% ..1) {
            return("M")
          } else {
            return(NA_character_)
          }
        }
      ),
      .groups = "drop"
    ) %>%
    dplyr::left_join(ind_clean, by = c("IndvID" = "individualID")) %>%
    dplyr::distinct(.data$IndvID, .keep_all = TRUE) %>%
    dplyr::mutate(
      # Coalesce: prefer individual_raw values; fall back to capture-derived values
      Species = dplyr::coalesce(.data$Species, .data$Species_cap),
      PopID = dplyr::coalesce(.data$PopID, .data$PopID_cap),
      RingSeason = dplyr::coalesce(.data$RingSeason, .data$RingSeason_cap),
      RingAge = dplyr::coalesce(.data$RingAge, .data$RingAge_cap),
      BroodIDFledged = .data$BroodIDLaid,
      Sex_genetic = NA_character_
    ) %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Individual_data[
        1, !(names(data_templates[[paste0("v", protocol_version)]]$Individual_data) %in% names(.))
      ]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Individual_data))

  return(Individual_data)
}

#' Create location data table for Zeromski Park / Kowale, Poland.
#'
#' @param brood_data Data frame. Raw brood sheet from ZER_PrimaryData.xlsx.
#' @param capture_data Data frame. Raw capture sheet from ZER_PrimaryData.xlsx.
#' @param location_data Data frame. Raw location sheet from ZER_PrimaryData.xlsx.
#' @param pop_filter Character vector. Site codes to include.
#' @param protocol_version Character string. Protocol version.
#'
#' @return A data frame.

create_location_ZER <- function(brood_data, capture_data, location_data, pop_filter, protocol_version) {
  # Site-level coordinates lookup
  site_coords <- location_data %>%
    dplyr::mutate(siteID = toupper(trimws(.data$siteID))) %>%
    dplyr::select("siteID", "decimalLatitude", "decimalLongitude")

  brood_locs <- brood_data %>%
    dplyr::mutate(siteID = normalise_site_ZER(.data$siteID)) %>%
    dplyr::filter(.data$siteID %in% pop_filter, !is.na(.data$locationID)) %>%
    dplyr::select("locationID", "siteID")

  capture_locs <- capture_data %>%
    dplyr::mutate(siteID = normalise_site_ZER(.data$siteID)) %>%
    dplyr::filter(.data$siteID %in% pop_filter, !is.na(.data$locationID)) %>%
    dplyr::select("locationID", "siteID")

  Location_data <- dplyr::bind_rows(brood_locs, capture_locs) %>%
    dplyr::distinct(.data$locationID, .keep_all = TRUE) %>%
    dplyr::left_join(site_coords, by = "siteID") %>%
    dplyr::mutate(
      LocationID   = .data$locationID,
      NestboxID    = NA_character_,
      LocationType = "MN",
      PopID        = .data$siteID,
      Latitude     = .data$decimalLatitude,
      Longitude    = .data$decimalLongitude,
      StartSeason  = NA_integer_,
      EndSeason    = NA_integer_,
      HabitatType  = NA_character_
    ) %>%
    dplyr::bind_cols(
      data_templates[[paste0("v", protocol_version)]]$Location_data[
        1, !(names(data_templates[[paste0("v", protocol_version)]]$Location_data) %in% names(.))
      ]
    ) %>%
    dplyr::select(names(data_templates[[paste0("v", protocol_version)]]$Location_data))

  return(Location_data)
}
