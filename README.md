
# SPI-Birds pipeline: Introduction

Welcome to the SPI-Birds pipeline package. This README will give you an introduction on how to use the package for creating hole-nesting bird data following the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf).

## Setup your computer to run pipelines

Pipelines for some populations require additional software and software drivers. These setup instructions describe how to install the required software for a Windows 10 64-bit operating system (on which the pipelines were built). The setup procedure should be similar for other Windows 64-bit systems, but will differ for non-Windows operating systems. If you are unsure which version of Windows is running on your computer, check 'System Type' in the 'System Information'. To run the pipelines for all populations a users system must have:

- Microsoft Access Driver (/*.mdb, /*.accdb)
- Python 3
- Python libraries *pandas* and *pypxlib*

### Windows 10 64-bit

#### Microsoft Access Driver

Firstly, you must check that you are running a 64-bit version of R. Open an R session and see whether you have 64-bit or 32-bit installation.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/R_version.jpg)

If you do not have a 64-bit version you will need to install one [here](https://www.r-project.org/).

In the taskbar, search for 'ODBC'. There will be two version (32-bit and 64-bit) ***select the 64-bit version***. This will open the 'ODBC Data Source Administrator (64-bit)' window.

Click 'Add' to install a new driver on your system.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/ODBC_driver.jpg)

Select 'Microsoft Access Driver (/*.mdb, /*.accdb)' and click finish. In the next window, you ***must*** add a 'Data Source Name'. Leave everything else blank.  

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/Add_name.jpg)

You can check if this driver is installed and recognised by R using the function `odbcListDrivers()` in the `odbc` package. Note that you will need to open a new session of R before the driver will appear.

#### Python 3

To install Python, we recommend using the [Anaconda distribution](https://www.anaconda.com/distribution/). ***Make sure to download the 3.X version of Python***. The Anaconda distribution comes with some libraries (including *pandas*) pre-loaded.

Once installed, open the 'Anaconda prompt' and type:

`pip install pypxlib`

This will install the *pypxlib* library on your system.

Restart your computer before running the pipelines.

### Mac

Work in progress..

## Storing the data

Primary data is stored following a standard naming approach.

* The names of data file(s) for each population should not be changed. When updating with data from new years, make sure the name of the new data is the same as the old data.
* Data from each population should have its own folder. The folder name should include the three letter code associated with the population (e.g. HAR for Harjavalta). See the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf) for the full list of population codes.
* All populations folders should be stored in the same location.

## Create data in the standard format

Each year when primary data are updated all pipelines will be re-run for all populations. This is done using the function `run_pipelines()`. This function has 4 arguments:

* path: The location of the folder where all population data is stored (see 'Storing the data' above).
* PopID: The population code(s) for the populations where you want to run pipelines.
* Species: The species code(s) for the species you want to use (e.g. PARMAJ for great tit). See the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf) for all species codes.
* output_type: Should the data be output in R or as separate .csv files

For example, the below code will format great tit data from Harjavalta (Finland) and Choupal (Portugal) and output .csv files in the location where the data are stored.

```r
run_pipelines(path = "C:\\all_data", PopID = c("HAR", "CHO"), Species = "PARMAJ", output_type = "csv")
```

**Note:** If you select a PopID/Species combination that does not exist, this population will be skipped.

In most cases, we will want to run all pipelines together. In this case, the arguments *PopID* and *Species* can be ignored.

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
