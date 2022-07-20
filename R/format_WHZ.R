#' Construct standard format for data from Westerholz, Germany
#'
#' A pipeline to produce the standard format for the nest box population in Westerholz,
#' Germany, administered by Bart Kempenaers, Max Planck Institute for Biological Intelligence, Seewiesen, Germany.
#'
#' This section provides details on data management choices that are unique to this data. For a general description of the standard format please see \href{https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v1.2.0.pdf}{here}.
#'
#' @inheritParams pipeline_params
#'
#' @return Generates either 6 .csv files or 6 data frames in the standard format.
#'
#' @export
#'

format_WHZ <- function(db = choose_directory(),
                       species = NULL,
                       site = NULL,
                       optional_variables = NULL,
                       path = ".",
                       output_type = "R") {

  # Force choose_directory() if used
  force(db)

  # Assign species for filtering
  if(is.null(species)){

    species <- species_codes$speciesID

  }

  # If all optional variables are requested, retrieve all names
  if(!is.null(optional_variables) & "all" %in% optional_variables) optional_variables <- names(unlist(unname(utility_variables)))

  # Record start time to provide processing time to the user
  start_time <- Sys.time()

  message("Importing primary data...")

  # Create a temporary in-memory RSQLite database to execute SQL queries on
  connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

  # Clean SQL files
  queries <- paste0(db, "\\",
                    c("BREEDING", "NESTS", "ADULTS", "CHICKS",
                      "SEX", "BOX_geoCoordinates"),
                    ".sql") %>%
    purrr::map(.f = ~ clean_query_WHZ(.x)) %>%
    unlist()

  # Execute SQL queries on database
  purrr::walk(.x = queries,
              .f = ~ DBI::dbExecute(connection, statement = .x))



}

#' Clean syntax in SQL files
#'
#' WHZ primary data are stored in a series of SQL files, which contain redundant lines and syntax that is not correctly interpreted when reading into R. To execute the SQL statements, we first clean the SQL syntax.
#'
#' @param path Location of SQL file.
#'
#' @return A list of characters with one element for each SQl statement.
#'
#' @example
#' \dontrun{
#' clean_query_WHZ("~/Adults.SQL")
#' }
#'

clean_query_WHZ <- function(path){

  # Read lines from SQL file, and clean SQL query
  queries <- readLines(path) %>%
    # Remove metadata, comments
    stringr::str_remove_all("--.*$") %>%
    stringr::str_remove_all("/\\*.*?\\*/;") %>%
    stringr::str_remove_all("\\\\'") %>%
    stringr::str_remove_all("COMMENT.*(?=\\,)") %>%
    stringr::str_remove_all("unsigned")


  clean_queries <- purrr::map(.x = queries,
                              .f = ~{

                                # Set primary key
                                dplyr::case_when(stringr::str_detect(.x, "NOT NULL AUTO_INCREMENT") ~ paste0("`", stringr::str_extract(.x, pattern = "(?<=[`]).*(?=[`])"), "`",
                                                                                                             " INTEGER PRIMARY KEY AUTOINCREMENT,"),
                                                 stringr::str_detect(.x, "NOT NULL DEFAULT 0") ~ paste0("`", stringr::str_extract(.x, pattern = "(?<=[`]).*(?=[`])"), "`",
                                                                                                             " INTEGER PRIMARY KEY,"),
                                                 stringr::str_detect(.x, "PRIMARY KEY") ~ "",
                                                 # Remove additional keys
                                                 stringr::str_detect(.x, "KEY") ~ "",
                                                 # Remove table lock statements
                                                 stringr::str_detect(.x, "LOCK") ~ "",
                                                 stringr::str_detect(.x, "^\\)") ~ ");",
                                                 stringr::str_detect(.x, "--[^\r\n]*") ~ "",
                                                 stringr::str_detect(.x, "/\\*.*?\\*/") ~ "",
                                                 TRUE ~ .x
                                )

                              }) %>%
    purrr::discard(~ .x == "")

  # Split by statement
  output <- purrr::map(.x = split(clean_queries, cumsum(stringr::str_detect(clean_queries, "^[:upper:]+"))),
                       .f = ~{

                         unlist(.x) %>%
                           stringr::str_c(collapse = " ") %>%
                           # Remove line breaks, tabs, etc.
                           stringr::str_replace_all("[\r\n\t\f\v]", " ") %>%
                           # Remove redundant white space
                           stringr::str_replace_all(" +", " ") %>%
                           # Remove redundant commas
                           stringr::str_replace_all(", +(?=\\))", " ")

                       })

  return(output)

}
