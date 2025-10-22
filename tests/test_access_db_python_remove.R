# File to run both export_access_db_python and export_access_db_rjackcess
# this is to validate the output

# REMOVE THIS FILE IN PRODUCTION

# First, load some usefull libraries:
library(here)
library(dplyr)

# Set the working directory
if (basename(here()) != "pipelines") {
    setwd(paste0(here(), "/pipelines"))
}

remove.packages("pipelines")
devtools::clean_dll() # Clean any compiled code
devtools::load_all()
devtools::install()

# Load and install the "pipelines" package fresh
devtools::load_all()
devtools::install()

# Generate/update all documentation
devtools::document()

# Then reload the package
devtools::load_all()

# Assume we are in the pipelines repo/folder
# Get the full path
dir <- here()

if (dir.exists(paste(dir, "/AMM_Ammersee_Germany", sep = ""))) {
    print("Directory exists!")
    dir <- paste0(dir, "/AMM_Ammersee_Germany")
} else {
    print("Directory does not exist.")
}

dsn <- paste0(dir, "/AMM_PrimaryData.accdb")

python_output_dir <- file.path(dir, "exported_tables", "python")
rjackcess_output_dir <- file.path(dir, "exported_tables", "rjackcess")

# Create directories if they don't exist
dir.create(python_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rjackcess_output_dir, recursive = TRUE, showWarnings = FALSE)

export_access_db_python(
    dsn = dsn,
    table = c("Broods", "Catches", "Chicks"),
    output_dir = python_output_dir
)

export_access_db_rjackcess(
    dsn = dsn,
    table = c("Broods", "Catches", "Chicks"),
    output_dir = rjackcess_output_dir
)

# Compare the output files

# Load the exported .csv files back and compare columns
python_broods <- read.csv(file.path(python_output_dir, "Broods.csv"))
rjackcess_broods <- read.csv(file.path(rjackcess_output_dir, "Broods.csv"))
cat("Broods columns match?: ", setequal(colnames(python_broods), colnames(rjackcess_broods)), "\n")

python_catches <- read.csv(file.path(python_output_dir, "Catches.csv"))
rjackcess_catches <- read.csv(file.path(rjackcess_output_dir, "Catches.csv"))
cat("Catches columns match?: ", setequal(colnames(python_catches), colnames(rjackcess_catches)), "\n")

python_chicks <- read.csv(file.path(python_output_dir, "Chicks.csv"))
rjackcess_chicks <- read.csv(file.path(rjackcess_output_dir, "Chicks.csv"))
cat("Chicks columns match?: ", setequal(colnames(python_chicks), colnames(rjackcess_chicks)), "\n")

# Compare the functions
amm_python <- format_AMM_python(
    db = dir,
    species = NULL,
    pop = NULL,
    path = ".", # Not needed, since we save to an R object
    output_type = "R"
)

amm_rjackcess <- format_AMM_rjackcess(
    db = dir,
    species = NULL,
    pop = NULL,
    path = ".", # Not needed, since we save to an R object
    output_type = "R"
)
