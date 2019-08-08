
# SPI-Birds pipeline: Introduction

Welcome to the SPI-Birds pipeline package. This README will give you an introduction on how to use the package for creating hole-nesting bird data following the [SPI-Birds standard format](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf).

## Storing the data

Population data should be stored in a standard format for use with the SPI-Birds pipelines.

* The names of data file(s) for each population should not be changed. When updating with data from new years, make sure the name of the new data is the same as the old data.
* Data from each population should have its own folder. The folder name should include the three letter code associated with the population (e.g. HAR for Harjavalta). See the standard protocol for the full list of population codes.
* All populations folders should be stored in the same location.

## Create data in the standard format

The main function you will use is `run_pipelines()`. This function has 4 arguments:

* path: The location of the folder where all population data is stored (see storage protocol above).
* PopID: The population code(s) for the populations where you want to extract data.
* Species: The species code(s) for the species you want to extract data for (e.g. PARMAJ for great tit). See the standard protocol for all species codes.
* combined: Should the outputs of the pipelines be combined into a single .csv file or create multiple .csv files?

For example, the below code will format great tit data from Harjavalta (Finland) and Choupal (Portugal) and output combined .csv files in the location where the data are stored.

```r
run_pipelines(path = "C:\\all_data", PopID = c("HAR", "CHO"), Species = "PARMAJ", combined = TRUE)
```

**Note:** If you select a PopID/Species combination that does not exist, this population will be skipped.

## Run a single pipeline

If you want to just run one single pipeline, you can use the `format_XXX()` functions. Each *data owner* has a corresponding function. In most cases the code for the data owner is the same as that of the population.

```r
#Run the Harjavalta pipeline
format_HAR()
```

For cases where a data owner administers multiple populations, the code of the data owner is used instead. The argument pop is used to specify which population(s) is required. Codes of data owners can be seen in the `pop_names` object that comes with the package.

```r
#Run the NIOO pipeline and return data from Hoge Veluwe
format_NIOO(pop = "HOG")
```

You can use the argument `output_type` to output the data in R. This can be useful for troubleshooting or for the data quality checks.

```r
#Run the NIOO pipeline and return an R object with data from Hoge Veluwe
format_NIOO(pop = "HOG", output_type = "R")
```

## Detail pipeline assumptions

Every pipeline has its own help document. In this document, we explicitly describe all assumptions and decisions that were made in the pipeline.

```r
#Find assumptions build into the NIOO pipeline
?format_NIOO
```
