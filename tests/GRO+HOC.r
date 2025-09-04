# Test script
# Some code to run both the HOC and GRO pipelines
# I can then compare the output of a 1.0 vs a 1.1 pipeline

# First, load some useful libraries:
library(here)
library(dplyr)

# Load and install the "pipelines" package
devtools::load_all()
devtools::install()

# Assume we are in the pipelines repo/folder
# Get the full path
dir <- here()

# Strip the full path of "pipelines"
dir <- dirname(dir)

print("Before format_HOC")
print(getwd())
HOC_data <- format_HOC(
    db = paste(dir, "/HOC_Hochstadt_Germany", sep = ""),
    species = NULL,
    pop = NULL,
    path = ".",
    output_type = "R"
)
print("After format_HOC")

# Run the GRO pipeline
GRO_data <- format_GRO(
    db = paste(dir, "/GRO_GroblaNiepolomice_Poland", sep = ""),
    species = NULL,
    pop = NULL,
    path = ".", # Not needed, since we save to an R object
    output_type = "R"
)

# Let's see what kind of data each list contains
names(GRO_data)
names(HOC_data)

# Compare "Brood_data" in 1.0 vs "Brood_data" in 1.1
identical(names(HOC_data$Brood_data), names(GRO_data$Brood_data))
# They are not the same, what are the differences?

# Lets see what 1.1 has that 1.0 does not have
setdiff(names(GRO_data$Brood_data), names(HOC_data$Brood_data))

# ...and vice versa
setdiff(names(HOC_data$Brood_data), names(GRO_data$Brood_data))
# So, we did get more conservative with our variables
# Not an exact date, but what is observed, or ranges of those

# I wanna see how the old variables map to the new ones:
# Dump it in a nice json format while we are at it :)
# Make it table agnostic and put it in a function

# Function to map old variables to new variables
map_old_to_new <- function(old_vars, new_vars) {
  lapply(old_vars, function(old) {
    # Remove "Error" suffix to get base name
    base <- sub("Error$", "", old)
    # Find all new variables that start with the base
    new <- grep(paste0("^", base), new_vars, value = TRUE)
    # Return a list with old and new
    list(old = old, new = new)
  })
}

mapping <- map_old_to_new(names(HOC_data$Brood_data), names(GRO_data$Brood_data))

# Filter everything that is identical out of it
filtered_mapping <- Filter(function(x) {
  !(length(x$new) == 1 && x$old == x$new)
}, mapping)

filtered_mapping %>% View()