#' Format the La Hiruela and Montesclaros data
#'
#' Work in progress.
#'
#' @inheritParams pipeline_params
#'
#' @return The four protocol 1.1.0 tables once the mappings are settled.
format_LAH <- function(db = choose_directory(),
                       path = ".",
                       species = NULL,
                       pop = NULL,
                       output_type = "R") {
  source <- read_primary_LAH(db)
  protocol_version <- "1.1.0"

  Brood_data <- create_brood_LAH(source, species, pop, protocol_version)
  Capture_data <- create_capture_LAH(source, species, pop, protocol_version)
  Individual_data <- create_individual_LAH(source, Capture_data, pop, protocol_version)
  Location_data <- create_location_LAH(source, pop, protocol_version)

  output <- list(
    Brood_data = Brood_data,
    Capture_data = Capture_data,
    Individual_data = Individual_data,
    Location_data = Location_data,
    protocol_version = protocol_version
  )
  if (output_type == "R") {
    return(output)
  }
  if (output_type != "csv") stop("output_type must be 'R' or 'csv'.")

  for (table in c("Brood_data", "Capture_data", "Individual_data", "Location_data")) {
    utils::write.csv(output[[table]], file.path(path, paste0(table, "_LAH.csv")),
      row.names = FALSE
    )
  }
  writeLines(protocol_version, file.path(path, "protocol_version_LAH.txt"))
  invisible(NULL)
}

read_primary_LAH <- function(db) {
  file <- file.path(db, "LAH_PrimaryData.xlsx")
  if (!file.exists(file)) stop("Missing LAH_PrimaryData.xlsx in ", db)

  chicks <- readxl::read_excel(file, sheet = "1987-2018", col_types = "text")
  required <- c(
    "HABITAT", "YEAR", "BOX", "BOX_year", "fledglingRing",
    "ID", "SIRE", "DAM", "LayDate_April", "Clutch_Size",
    "FledglingNumber"
  )
  missing <- setdiff(required, names(chicks))
  if (length(missing)) {
    stop("Missing LAH columns: ", paste(missing, collapse = ", "))
  }

  # Check for conflicting nest fields before taking one row per nest.
  nest_fields <- c(
    "YEAR", "HABITAT", "BOX", "SIRE", "DAM", "LayDate_April",
    "Clutch_Size", "FledglingNumber"
  )
  conflicts <- chicks %>%
    dplyr::group_by(.data$BOX_year) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(nest_fields), ~ dplyr::n_distinct(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(nest_fields), ~ .x > 1L))
  nests <- chicks %>%
    dplyr::distinct(.data$BOX_year, .keep_all = TRUE)
  occupancy <- readxl::read_excel(
    file,
    sheet = "OCCUPANCY", skip = 1, .name_repair = "minimal"
  )

  list(
    chicks = chicks, nests = nests, occupancy = occupancy,
    nest_conflicts = conflicts
  )
}

create_brood_LAH <- function(source, species, pop, protocol_version) {
  # HABITAT 1/2/3, the date offset, and zero-fledgling rows need confirmation.
  stop("Brood mapping pending; see notes/notes/pipelines.LAH.md.")
}

create_capture_LAH <- function(source, species, pop, protocol_version) {
  # The workbook does not give a capture date for each bird.
  stop("Capture mapping pending; see notes/notes/pipelines.LAH.md.")
}

create_individual_LAH <- function(source, Capture_data, pop, protocol_version) {
  # Confirm how fledglingRing and ID identify the same bird.
  stop("Individual mapping pending; see notes/notes/pipelines.LAH.md.")
}

create_location_LAH <- function(source, pop, protocol_version) {
  # BOX can be reused over years; HABITAT 3 also needs a site assignment.
  stop("Location mapping pending; see notes/notes/pipelines.LAH.md.")
}
