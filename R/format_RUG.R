#' Format the Groningen pied flycatcher data
#'
#' Reads the two SPI tables from the Access file supplied by the data owner
#' Nest coordinates are not available in that file
#'
#' @inheritParams pipeline_params
#' @return Four protocol 1.1.0 tables and the protocol version.
#' @export
format_RUG <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R") {
  force(db)
  protocol_version <- "1.1.0"
  if (!is.null(pop) && !"RUG" %in% pop) {
    return(NULL)
  }

  dsn <- file.path(db, "RUG_PrimaryData.accdb")
  if (!file.exists(dsn)) stop("RUG_PrimaryData.accdb was not found in ", db)
  table_dir <- tempfile("RUG_tables_")
  dir.create(table_dir)
  on.exit(unlink(table_dir, recursive = TRUE), add = TRUE)
  tables <- c("SPI breeding data", "SPI catch data")
  export_access_db(dsn, tables, table_dir)

  breeding <- utils::read.csv(file.path(table_dir, "SPI breeding data.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  catches <- utils::read.csv(file.path(table_dir, "SPI catch data.csv"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  species_code <- species_codes$Species[species_codes$speciesEURINGCode == 13490]
  if (length(species_code) != 1L) stop("Pied flycatcher species code was not found")

  breeding <- breeding[breeding$ST == 2, , drop = FALSE]
  catches <- catches[catches$S == 2, , drop = FALSE]
  if (!is.null(species) && !species_code %in% species) {
    breeding <- breeding[0, , drop = FALSE]
    catches <- catches[0, , drop = FALSE]
  }

  Brood_data <- create_brood_RUG(breeding, species_code)
  capture_work <- create_capture_RUG(catches, breeding, Brood_data, species_code)
  Individual_data <- create_individual_RUG(capture_work)
  Location_data <- create_location_RUG(Brood_data, capture_work)

  Capture_data <- rug_standardize(capture_work, "Capture_data")
  Brood_data <- rug_standardize(Brood_data, "Brood_data")
  Individual_data <- rug_standardize(Individual_data, "Individual_data")
  Location_data <- rug_standardize(Location_data, "Location_data")

  if (output_type == "csv") {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    for (name in c("Brood_data", "Capture_data", "Individual_data", "Location_data")) {
      utils::write.csv(get(name), file.path(path, paste0(name, "_RUG.csv")),
        row.names = FALSE
      )
    }
    writeLines(protocol_version, file.path(path, "protocol_version_RUG.txt"))
    return(invisible(NULL))
  }
  if (output_type != "R") stop("output_type must be 'R' or 'csv'")
  list(
    Brood_data = Brood_data, Capture_data = Capture_data,
    Individual_data = Individual_data, Location_data = Location_data,
    protocol_version = protocol_version
  )
}

rug_box <- function(x) {
  x <- round(as.numeric(x), 1)
  ifelse(is.na(x), NA_character_,
    ifelse(x == round(x), as.character(round(x)),
      format(x, trim = TRUE, nsmall = 1, scientific = FALSE)
    )
  )
}

rug_location <- function(plot, box) {
  ifelse(is.na(plot) | is.na(box), NA_character_,
    paste("RUG", plot, box, sep = "-")
  )
}

rug_id <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "00000000", "0")] <- NA_character_
  x
}

rug_count <- function(x) {
  x <- as.integer(x)
  x[is.na(x) | x < 0L | x >= 98L] <- NA_integer_
  x
}

rug_day <- function(x, year) {
  day <- as.numeric(x)
  year <- as.integer(year)
  valid <- !is.na(day) & day > -99 & day < 500 & !is.na(year)
  out <- as.Date(rep(NA_character_, length(day)))
  out[valid] <- as.Date(paste0(year[valid], "-03-31")) + day[valid]
  out
}

rug_standardize <- function(data, table) {
  template <- data_templates$v1.1.0[[table]]
  for (name in setdiff(names(template), names(data))) {
    data[[name]] <- rep(template[[name]][1], nrow(data))
  }
  tibble::as_tibble(data[names(template)])
}

# Build the brood table.
create_brood_RUG <- function(breeding, species_code) {
  n <- nrow(breeding)
  year <- as.integer(breeding$JA)
  plot <- rug_box(breeding$Terreinnummer)
  box <- rug_box(breeding$Nestkastnummer)
  lay <- rug_day(breeding$LBG, year)
  hatch <- rug_day(breeding$GBD, year)
  hatch[!is.na(breeding$GBD) & breeding$GBD <= 0] <- as.Date(NA)
  uncertainty <- as.integer(breeding$AL)
  clutch <- rug_count(breeding$LG)
  brood <- rug_count(breeding$GB)
  fledged <- rug_count(breeding$UIT)
  maybe_hatched <- rug_count(breeding$AG)
  maybe_fledged <- rug_count(breeding$AU)

  # The guide says dates above 500 are estimates, without a stated interval.
  lay_max <- lay
  lay_max[!is.na(uncertainty) & uncertainty > 0] <-
    lay[!is.na(uncertainty) & uncertainty > 0] +
    uncertainty[!is.na(uncertainty) & uncertainty > 0]
  lay_max[is.na(uncertainty) | uncertainty < 0] <- as.Date(NA)

  type <- dplyr::case_when(
    breeding$LE == 0 ~ "first",
    breeding$LE %in% c(1, 4, 6, 8) ~ "replacement",
    breeding$LE %in% c(2, 3, 7) ~ "second",
    TRUE ~ NA_character_
  )
  out <- tibble::tibble(
    BroodID = paste0("RUG-", breeding$BroedselID), PopID = rep("RUG", n),
    BreedingSeason = year, Species = rep(species_code, n), Plot = plot,
    LocationID = rug_location(plot, box), FemaleID = rug_id(breeding$VROUW),
    MaleID = rug_id(breeding$MAN), ClutchType_observed = type,
    LayDate_observed = lay, LayDate_min = lay, LayDate_max = lay_max,
    ClutchSize_observed = clutch, ClutchSize_min = clutch, ClutchSize_max = clutch,
    HatchDate_observed = hatch, HatchDate_min = hatch, HatchDate_max = hatch,
    BroodSize_observed = brood, BroodSize_min = brood,
    BroodSize_max = ifelse(!is.na(brood) & !is.na(maybe_hatched),
      brood + maybe_hatched, NA_integer_
    ),
    NumberFledged_observed = fledged, NumberFledged_min = fledged,
    NumberFledged_max = ifelse(!is.na(fledged) & !is.na(maybe_fledged),
      fledged + maybe_fledged, NA_integer_
    )
  )
  # UVD is described as an estimate even when a day is entered.
  out$FledgeDate_observed <- as.Date(NA)
  out$FledgeDate_min <- as.Date(NA)
  out$FledgeDate_max <- as.Date(NA)
  for (name in c("ClutchSize_max", "BroodSize_max", "NumberFledged_max")) {
    out[[name]][!is.na(out[[name]]) & out[[name]] == 0L] <- NA_integer_
  }

  if (n) {
    out <- dplyr::arrange(out, .data$BreedingSeason, .data$LayDate_min)
    out$ClutchType_calculated <- calc_clutchtype(
      data = out, protocol_version = "1.1", na.rm = FALSE
    )
  } else {
    out$ClutchType_calculated <- character(0)
  }
  out$ClutchType_calculated <- as.character(out$ClutchType_calculated)
  out
}

# Build the capture table.
create_capture_RUG <- function(catches, breeding, broods, species_code) {
  n <- nrow(catches)
  year <- as.integer(catches$JR)
  plot <- rug_box(catches$Terreinnummer)
  box <- rug_box(catches$Plaatsnummer)
  location <- rug_location(plot, box)
  date <- as.Date(sprintf(
    "%04d-%02d-%02d", year,
    as.integer(catches$MND), as.integer(catches$DAG)
  ))
  indv <- rug_id(catches$RNR)
  sex <- dplyr::case_when(
    catches$SW == 1 ~ "F", catches$SW == 2 ~ "M",
    TRUE ~ NA_character_
  )
  age <- dplyr::case_when(
    catches$LTW == 1 ~ 1L, catches$LTW == 3 ~ 3L,
    catches$LTW %in% c(2, 4) ~ 4L,
    catches$LTW == 5 ~ 5L, catches$LTW == 6 ~ 6L,
    TRUE ~ NA_integer_
  )
  mass <- as.numeric(catches$GEW) / 100
  mass[mass <= 0] <- NA_real_
  wing <- as.numeric(catches$LVL2) / 10
  wing[wing <= 0] <- NA_real_
  t1 <- as.numeric(catches$TARS1) / 10
  t2 <- as.numeric(catches$TARS2) / 10
  t1[t1 <= 0] <- NA_real_
  t2[t2 <= 0] <- NA_real_
  tarsus <- ifelse(!is.na(t1) & !is.na(t2), (t1 + t2) / 2,
    ifelse(!is.na(t1), t1, t2)
  )
  time <- as.integer(catches$TYD)
  time[time < 0 | time > 2359 | time %% 100 > 59] <- NA_integer_
  time <- ifelse(is.na(time), NA_character_, sprintf("%02d:%02d", time %/% 100, time %% 100))
  alive <- !(catches$DL %in% c(1, 2) |
    (!is.na(catches$Lijk) & catches$Lijk == 1))
  release <- alive & !(catches$DL %in% c(4, 5, 7, 8, 10, 11, 12, 14, 20))
  moved <- catches$MANIP %in% c(4, 6, 7, 8, 10, 11)
  release_plot <- rug_box(catches$`Plot release`)
  release_plot[is.na(release_plot) & !moved] <- plot[is.na(release_plot) & !moved]

  out <- tibble::tibble(
    source_id = catches$ID, IndvID = indv, Species = rep(species_code, n),
    Sex_observed = sex, Sex_genetic = dplyr::case_when(
      catches$SP == 1 ~ "F", catches$SP == 2 ~ "M", TRUE ~ NA_character_
    ),
    BreedingSeason = year, CaptureDate = date, CaptureTime = time,
    ObserverID = as.character(catches$SYS), LocationID = location,
    CaptureAlive = alive, ReleaseAlive = release,
    CapturePopID = rep("RUG", n), CapturePlot = plot,
    ReleasePopID = ifelse(moved, NA_character_, "RUG"),
    ReleasePlot = release_plot,
    Mass = mass, Tarsus = tarsus,
    OriginalTarsusMethod = ifelse(is.na(tarsus), NA_character_, "Alternative"),
    WingLength = wing, Age_observed = age,
    ChickAge = as.integer(catches$pullusage),
    source_brood = as.numeric(catches$lautestID)
  )
  out$ChickAge[out$ChickAge < 0] <- NA_integer_
  out <- out[!is.na(out$IndvID) & !is.na(out$CaptureDate), , drop = FALSE]

  # Use a box and season only when they identify one brood.
  brood_key <- paste(broods$BreedingSeason, broods$LocationID)
  catch_key <- paste(out$BreedingSeason, out$LocationID)
  unique_key <- !duplicated(brood_key) & !duplicated(brood_key, fromLast = TRUE)
  idx <- match(catch_key, brood_key)
  idx[!is.na(idx) & !unique_key[idx]] <- NA_integer_
  out$BroodID <- broods$BroodID[idx]

  # For repeated boxes, the original record number can resolve a chick's brood.
  ambiguous <- which(is.na(out$BroodID) & out$Age_observed == 1L)
  if (length(ambiguous)) {
    key <- paste(
      breeding$JA, rug_location(
        rug_box(breeding$Terreinnummer),
        rug_box(breeding$Nestkastnummer)
      ),
      breeding$IDlautest
    )
    unique_id <- !duplicated(key) & !duplicated(key, fromLast = TRUE)
    candidate <- match(paste(
      out$BreedingSeason[ambiguous], out$LocationID[ambiguous],
      out$source_brood[ambiguous]
    ), key)
    candidate[!is.na(candidate) & !unique_id[candidate]] <- NA_integer_
    source_to_brood <- match(paste0("RUG-", breeding$BroedselID[candidate]), broods$BroodID)
    out$BroodID[ambiguous] <- broods$BroodID[source_to_brood]
  }
  out$BroodID[out$Age_observed != 1L | is.na(out$Age_observed)] <- NA_character_
  out$FosterBroodID <- out$BroodID
  # HER and the chick movement codes point to a different natal nest.
  moved_chick <- !is.na(catches$HER) & catches$HER != 0 &
    abs(as.numeric(catches$HER) - as.numeric(catches$Plaatsnummer)) > 0.01
  moved_chick <- moved_chick | catches$MANIP %in% c(3, 38, 40)
  moved_idx <- match(catches$ID[moved_chick], out$source_id, nomatch = 0L)
  out$BroodID[moved_idx[moved_idx > 0L]] <- NA_character_
  out <- out |>
    dplyr::arrange(.data$IndvID, .data$CaptureDate, .data$source_id) |>
    dplyr::group_by(.data$IndvID) |>
    dplyr::mutate(CaptureID = paste0(.data$IndvID, "_", dplyr::row_number())) |>
    dplyr::ungroup()
  if (nrow(out)) {
    out <- calc_age(out,
      ID = .data$IndvID, Age = .data$Age_observed,
      Date = .data$CaptureDate, Year = .data$BreedingSeason,
      showpb = FALSE
    )
  }
  out
}

# Build the individual table.
create_individual_RUG <- function(captures) {
  if (!nrow(captures)) {
    return(rug_standardize(tibble::tibble(), "Individual_data"))
  }
  captures |>
    dplyr::group_by(.data$IndvID) |>
    dplyr::summarise(
      Species = dplyr::first(.data$Species), PopID = "RUG",
      BroodIDLaid = {
        x <- unique(stats::na.omit(.data$BroodID))
        if (length(x) == 1L) x else NA_character_
      },
      BroodIDFledged = {
        x <- unique(stats::na.omit(.data$FosterBroodID))
        if (length(x) == 1L) x else NA_character_
      },
      RingSeason = min(.data$BreedingSeason),
      RingAge = if (any(.data$Age_observed == 1L, na.rm = TRUE)) "chick" else "adult",
      Sex_calculated = {
        x <- unique(stats::na.omit(.data$Sex_observed))
        if (length(x) == 1L) x else if (length(x) > 1L) "C" else NA_character_
      },
      Sex_genetic = {
        x <- unique(stats::na.omit(.data$Sex_genetic))
        if (length(x) == 1L) x else if (length(x) > 1L) "C" else NA_character_
      },
      .groups = "drop"
    )
}

# Build the location table.
create_location_RUG <- function(broods, captures) {
  sites <- dplyr::bind_rows(
    dplyr::select(broods, .data$LocationID, .data$BreedingSeason),
    dplyr::select(captures, .data$LocationID, .data$BreedingSeason)
  )
  sites <- dplyr::filter(sites, !is.na(.data$LocationID))
  if (!nrow(sites)) {
    return(rug_standardize(tibble::tibble(), "Location_data"))
  }
  sites |>
    dplyr::group_by(.data$LocationID) |>
    dplyr::summarise(
      StartSeason = min(.data$BreedingSeason, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      NestboxID = sub("^RUG-[^-]+-", "", .data$LocationID),
      LocationType = "NB", PopID = "RUG"
    )
}
