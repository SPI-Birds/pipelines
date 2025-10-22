#' Export Access Database Tables to CSV using Python AccessParser
#'
#' This function uses Python's AccessParser library via reticulate to export
#' MS Access database tables to CSV files. This provides cross-platform
#' compatibility for reading Access databases.
#'
#' @param dsn Character. File path to MS Access database source name (.mdb or .accdb file).
#' @param table Character vector. Names of tables to export. Can be a single table name or vector of table names.
#' @param output_dir Character. File path to directory where the new CSV files will be created. Directory will be created if it doesn't exist.
#' @param header Logical. Include the column names in the header row? Default: TRUE.
#' @param delim Character. The column delimiter in the output CSV file. Default: "," (comma).
#' @param quote Character. The quote character in the output CSV file. Default: '"' (double quote).
#' @param python_script Character. Path to the Python script containing export_access_db function. Default: "test_files/export_access_db.py".
#'
#' @return Invisible NULL. Creates CSV files of selected tables from Access database in the specified output directory.
#'
#' @details
#' This function requires the \code{reticulate} package and Python with the \code{access_parser} library installed.
#'
#' The function will:
#' \itemize{
#'   \item Connect to the specified Access database
#'   \item Extract data from the specified table(s)
#'   \item Create the output directory if it doesn't exist
#'   \item Export each table as a separate CSV file named \code{tablename.csv}
#' }
#'
#' @examples
#' \dontrun{
#' # Export single table
#' export_access_db_wrapper(
#'     dsn = "AMM_Ammersee_Germany/AMM_PrimaryData.accdb",
#'     table = "Broods",
#'     output_dir = "exported_data"
#' )
#'
#' # Export multiple tables
#' export_access_db_wrapper(
#'     dsn = "AMM_Ammersee_Germany/AMM_PrimaryData.accdb",
#'     table = c("Broods", "Catches", "Chicks"),
#'     output_dir = "exported_data"
#' )
#'
#' # Export with custom CSV formatting
#' export_access_db_wrapper(
#'     dsn = "database.accdb",
#'     table = "MyTable",
#'     output_dir = "output",
#'     header = TRUE,
#'     delim = ";",
#'     quote = "'"
#' )
#' }
#'
#' @seealso
#' \url{https://github.com/mccartney/access_parser} for AccessParser documentation
#'
#' @importFrom reticulate source_python
#' @export
export_access_db_python <- function(dsn,
                                    table,
                                    output_dir,
                                    header = TRUE,
                                    delim = ",",
                                    quote = '"',
                                    python_script = system.file("extdata", "export_access_db.py", package = "pipelines")) {
    # Check if reticulate is installed
    if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("Package 'reticulate' is required but not installed.\nPlease install it with: install.packages('reticulate')")
    }

    # Load reticulate
    library(reticulate)

    # Check if Python script exists
    if (!file.exists(python_script)) {
        stop(paste("Python script not found:", python_script))
    }

    # Source the Python function
    tryCatch(
        {
            reticulate::source_python(python_script)
            message("Python function loaded successfully")
        },
        error = function(e) {
            stop(paste("Error sourcing Python script:", e$message))
        }
    )

    # Call the Python function
    tryCatch(
        {
            export_access_db(
                dsn = dsn,
                table = table,
                output_dir = output_dir,
                header = header,
                delim = delim,
                quote = quote
            )

            message("Tables exported successfully using Python AccessParser")
            message(paste("Output directory:", output_dir))
        },
        error = function(e) {
            stop(paste("Error in Python function:", e$message))
        }
    )

    invisible(NULL)
}
