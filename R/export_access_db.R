#' Export selected tables from Access database to csv files through Jackcess
#'
#' Connect to MS Access database via Jackcess (a Java library) to export a selected tables as a  csv files.
#'
#' Jackcess is a Java library for reading from and writing to MS Access databases.
#' Previously, we used ODBC (Open Database Connectivity) to connect to MS Access databases,
#' but this does not work on MacOS and Linux as straightforwardly as it does on Windows.
#'
#' @param dsn Character. File path to MS Access database source name.
#' @param table Character. Names of tables to export.
#' @param output_dir Character. File path to directory where the new files will be created.
#' @param header Logical. Include the column names in the header? Default: \code{TRUE}.
#' @param delim Character. The column delimiter in the output file. Default: ",".
#' @param quote Character. The quote character in the output file. Default: '"'.
#' @param filter a Jackcess ExportFilter object to filter columns or rows in the export. Default: \code{rjackcess::SimpleExportFilter()$INSTANCE}, which is the full, unfiltered instance (table).
#'
#' @returns csv files of selected tables from Access database
#'
#' @importFrom rJava .jnew .jchar
#' @importFrom rjackcess SimpleExportFilter Database
#' @export

export_access_db <- function(dsn,
                             table,
                             output_dir,
                             header = TRUE,
                             delim = ",",
                             quote = '"',
                             filter = rjackcess::SimpleExportFilter()$INSTANCE) {

  if(!grepl("\\\\|\\/", output_dir)) {

    stop("`output_dir` should be a character vector giving the path to a directory")

  }

  # Initialise Jackcess object to interact with MS Access database
  db <- rjackcess::Database(dsn)

  # Create directory where new files will be created
  if(!dir.exists(output_dir)) {

    dir.create(output_dir, recursive = TRUE)

  }

  # Retrieve non-exported ExportUtilBuilder from rjackcess
  ExportUtilBuilder <- utils::getFromNamespace("ExportUtilBuilder", "rjackcess")

  purrr::walk(.x = table,
              .f = ~{

                out_file <- paste0(output_dir, "/", .x, ".csv")

                # Call the Jackcess exportFile utility class to export table .x to csv
                ExportUtilBuilder()$exportFile(db,
                                               .x,
                                               rJava::.jnew("java/io/File", path.expand(out_file)),
                                               header,
                                               delim,
                                               rJava::.jchar(quote),
                                               filter)

              })


  # Close connection to database
  db$close()

  invisible(NULL)

}
