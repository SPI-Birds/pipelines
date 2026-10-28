#' Format the Zeromski Park blackbird breeding records
#'
#' Reads `ZER_PrimaryData_new.xlsx`, sheet `data base`. This workbook contains
#' breeding histories for Zeromski Park (ZER), but no capture history. Nest
#' construction records without a valid first-egg date or a positive egg,
#' nestling, or fledgling count are omitted from Brood_data. This excludes
#' 1,233 rows, some of which may need review after date corrections. The
#' source's row number is used for brood IDs; three duplicated row numbers
#' receive a suffix to keep IDs unique.
#'
#' The first-egg dates for 101 records labelled 2014 have year 2023. These
#' dates are omitted pending confirmation from the data owner. Other dates
#' outside their stated breeding year are treated the same way. A brood
#' sequence number cannot distinguish second and replacement clutches, so
#' observedClutchType is left unknown.
#'
#' The workbook contains adult ring numbers, but no ringing dates, capture
#' events, chick identities, or dated measurements. Parent ring numbers are
#' retained in Brood_data; Individual_data, Capture_data, and Measurement_data
#' are empty because the necessary tagging and event details are absent. No
#' Kowale (KOW) records are present in this source.
#'
#' @param db Directory containing `ZER_PrimaryData_new.xlsx`.
#' @param path Directory for CSV output.
#' @param species Species to include. Accepts `"TURMER"` or `"Turdus merula"`.
#' @param pop Population to include. Only `"ZER"` is available.
#' @param output_type Either `"R"` or `"csv"`.
#'
#' @return Six protocol 2.0.0 tables and a protocol version when
#'   `output_type = "R"`; otherwise, writes CSV files and a version text file.
#' @export
format_ZER <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R") {
  if (!is.null(pop) && !all(toupper(pop) == "ZER")) {
    stop("ZER_PrimaryData_new.xlsx contains only Zeromski Park (ZER).")
  }
  if (!output_type %in% c("R", "csv")) {
    stop("output_type must be 'R' or 'csv'.")
  }

  source_file <- file.path(db, "ZER_PrimaryData_new.xlsx")
  if (!file.exists(source_file)) {
    stop("Missing ZER_PrimaryData_new.xlsx in ", db)
  }
  raw <- readxl::read_excel(
    source_file,
    sheet = "data base",
    range = readxl::cell_cols("A:AP"),
    col_types = "text",
    .name_repair = "minimal"
  )
  required <- c("1", "year", "nest number", "male ring number",
                "female ring number", "first egg", "egg number",
                "nestlings", "fledgling number")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("Missing columns in ZER breeding sheet: ", paste(missing, collapse = ", "))
  }

  protocol_version <- "2.0.0"
  tables <- data_templates[[paste0("v", protocol_version)]]
  output <- lapply(tables, function(template) template[0, , drop = FALSE])
  output$protocol_version <- protocol_version

  if (is.null(species) || any(species %in% c("TURMER", "Turdus merula"))) {
    source <- prepare_breeding_ZER(raw)
    output$Brood_data <- create_brood_ZER(source, tables$Brood_data)
    output$Location_data <- create_location_ZER(source, tables$Location_data)
  }

  if (output_type == "csv") {
    for (name in names(tables)) {
      utils::write.csv(output[[name]], file.path(path, paste0(name, "_ZER.csv")),
                       row.names = FALSE)
    }
    writeLines(protocol_version, file.path(path, "protocol_version_ZER.txt"))
    return(invisible(NULL))
  }
  output
}

# Excel stores dates as serial numbers even when read as text.
parse_date_ZER <- function(x) {
  serial <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA_character_, length(x)))
  is_serial <- !is.na(serial) & serial > 10000 & serial < 100000
  result[is_serial] <- as.Date(serial[is_serial], origin = "1899-12-30")
  is_text <- !is_serial & !is.na(x)
  result[is_text] <- suppressWarnings(as.Date(substr(x[is_text], 1, 10),
                                              format = "%Y-%m-%d"))
  result
}

parse_count_ZER <- function(x) {
  value <- suppressWarnings(as.numeric(trimws(x)))
  value[!is.na(value) & (value < 0 | value != floor(value))] <- NA_real_
  as.integer(value)
}

ring_ZER <- function(x) {
  value <- toupper(trimws(x))
  value[is.na(value) | !grepl("^[A-Z]{2}[0-9]{4,6}$", value)] <- NA_character_
  value
}

prepare_breeding_ZER <- function(raw) {
  year <- parse_count_ZER(raw[["year"]])
  lay_date <- parse_date_ZER(raw[["first egg"]])
  lay_date[!is.na(lay_date) & as.integer(format(lay_date, "%Y")) != year] <- as.Date(NA)
  clutch_size <- parse_count_ZER(raw[["egg number"]])
  brood_size <- parse_count_ZER(raw[["nestlings"]])
  fledged <- parse_count_ZER(raw[["fledgling number"]])
  brood_size[!is.na(brood_size) & brood_size == 0L] <- NA_integer_

  # A built nest is not necessarily a breeding attempt.
  keep <- !is.na(year) &
    (!is.na(lay_date) | (!is.na(clutch_size) & clutch_size > 0) |
     (!is.na(brood_size) & brood_size > 0) |
     (!is.na(fledged) & fledged > 0))
  source <- tibble::tibble(
    source_id = trimws(raw[["1"]][keep]),
    year = year[keep],
    nest = trimws(raw[["nest number"]][keep]),
    male = ring_ZER(raw[["male ring number"]][keep]),
    female = ring_ZER(raw[["female ring number"]][keep]),
    lay_date = lay_date[keep],
    clutch_size = clutch_size[keep],
    brood_size = brood_size[keep],
    fledged = fledged[keep],
    plant = trimws(raw[["plant name"]][keep]),
    placement = trimws(raw[["nest placement"]][keep]),
    height = trimws(raw[["hight"]][keep])
  )
  source$source_id[is.na(source$source_id) | source$source_id == ""] <-
    paste0("row", which(keep)[is.na(source$source_id) | source$source_id == ""])
  source$brood_id <- make.unique(
    paste("ZER", source$year, source$source_id, sep = "-"), sep = "-"
  )
  valid_nest <- !is.na(source$nest) & source$nest != "" &
    !source$nest %in% c("?", "brak")
  source$location_id <- source$brood_id
  source$location_id[valid_nest] <- paste(
    "ZER", source$year[valid_nest], "nest", source$nest[valid_nest], sep = "-"
  )
  source
}

create_brood_ZER <- function(source, template) {
  result <- template[rep(1L, nrow(source)), , drop = FALSE]
  result$row <- seq_len(nrow(source))
  result$broodID <- source$brood_id
  result$speciesID <- "Turdus merula"
  result$studyID <- "SZCZ"
  result$siteID <- "ZER"
  result$locationID <- source$location_id
  result$femaleID <- source$female
  result$maleID <- source$male
  result$observedLayYear <- as.integer(format(source$lay_date, "%Y"))
  result$observedLayMonth <- as.integer(format(source$lay_date, "%m"))
  result$observedLayDay <- as.integer(format(source$lay_date, "%d"))
  result$minimumLayYear <- result$maximumLayYear <- result$observedLayYear
  result$minimumLayMonth <- result$maximumLayMonth <- result$observedLayMonth
  result$minimumLayDay <- result$maximumLayDay <- result$observedLayDay
  result$observedClutchSize <- source$clutch_size
  result$minimumClutchSize <- source$clutch_size
  result$maximumClutchSize <- as.numeric(source$clutch_size)
  result$observedBroodSize <- source$brood_size
  result$minimumBroodSize <- source$brood_size
  result$maximumBroodSize <- as.numeric(source$brood_size)
  result$observedNumberFledged <- source$fledged
  result$minimumNumberFledged <- source$fledged
  result$maximumNumberFledged <- as.numeric(source$fledged)
  result
}

create_location_ZER <- function(source, template) {
  locations <- source[!duplicated(source$location_id), , drop = FALSE]
  result <- template[rep(1L, nrow(locations)), , drop = FALSE]
  result$row <- seq_len(nrow(locations))
  result$locationID <- locations$location_id
  result$locationType <- "nest"
  result$locationDetails <- vapply(seq_len(nrow(locations)), function(i) {
    fields <- c(plant = locations$plant[i],
                placement = locations$placement[i],
                height = locations$height[i])
    fields <- fields[!is.na(fields) & fields != ""]
    if (!length(fields)) return(NA_character_)
    paste(paste(names(fields), fields, sep = ": "), collapse = "; ")
  }, character(1))
  result$studyID <- "SZCZ"
  result$siteID <- "ZER"
  result$startYear <- locations$year
  result$endYear <- locations$year
  result
}
