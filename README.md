
# SPI-Birds pipeline: Introduction (for the general user)

Welcome to the SPI-Birds pipeline package. This README will give you an introduction on how to load the package, how to find out details about each pipeline, and how to use the package for creating hole-nesting bird data following the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf).

## Load the pipeline package

The pipeline package can be installed in R using the following code from the `devtools` package:

```
devtools::install_github("SPI-Birds/pipelines")
library(pipelines)
```

This will install all pipelines and quality check code on your computer and load the code into your session of R. Individual pipelines are build as separate functions for each data owner and are given the name `format_X()` where *X* is the letter code for the data owner. The codes for different data owners and corresponding populations are described below. *Note* in cases where a data owner administers one population, the unique 3 letter population ID code and the data owner code are identical.

| PopID         | Name                   | Country     |Data owner  |
| ------------- |:----------------------:| -----------:| ----------:|
| CHO           | Choupal                | Portugal    |CHO         |
| HAR           | Harjavalta             | Finland     |HAR         |
| BAN           | Bandon Valley          | Ireland     |BAN         |
| VEL           | Velky Kosir            | Czechia     |VEL         |
| HOG           | Hoge Veluwe            | Netherlands |NIOO        |
| OOS           | Oosterhout             | Netherlands |NIOO        |
| VLI           | Vlieland               | Netherlands |NIOO        |
| BUU           | Buunderkamp            | Netherlands |NIOO        |
| LIE           | Liesbos                | Netherlands |NIOO        |
| WAR           | Warnsborn              | Netherlands |NIOO        |
| WES           | Westerheide            | Netherlands |NIOO        |
| SSQ           | Santo Stefano Quisquina| Italy       |SSQ         |
| BOS           | Boshoek                | Belgium     |UAN         |
| PEE           | Peerdsbos              | Belgium     |UAN         |
| WYT           | Wytham Woods           | UK          |WYT         |
| ROU           | Rouviere               | France      |MON         |
| MON           | Montpellier City       | France      |MON         |
| MTV           | Mont Ventoux           | France      |MON         |
| MUR           | Muro                   | France      |MON         |
| PIR           | Pirio                  | France      |MON         |
| KEV           | Kevo                   | Finland     |KEV         |

## Read about a pipeline

To process each set of primary data into the structure described in the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf) it is often necessary to make assumptions about how each variable is intepreted. All assumptions made during the pipeline process are described in the help documentation for a given function. This can be accessed using the `?` in R. For example, to read about the assumptions made when processing data from the NIOO, you can use the code:

```
?format_NIOO
```

## Run the pipelines for yourself

Each set of primary data is in a slightly different format. Therefore, to run all pipelines successfully, your system will require additional software and drivers (in addition to R).

### Setup your computer to run pipelines

Pipelines for some populations require additional software and software drivers. These setup instructions describe how to install the required software for a Windows 10 64-bit operating system (on which the pipelines were built). The setup procedure should be similar for other Windows 64-bit systems, but will differ for non-Windows operating systems. If you are unsure which version of Windows is running on your computer, check 'System Type' in 'System Information'. To run the pipelines for all populations a users system must have:

- Microsoft Access Driver (/*.mdb, /*.accdb)
- Python 3
- Python libraries *pandas* and *pypxlib*

---

#### Windows 10 64-bit

##### Microsoft Access Driver

Firstly, you must check that you are running a 64-bit version of R. Open an R session and see whether you have 64-bit or 32-bit installation.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/R_version.jpg)

If you do not have a 64-bit version you will need to install one [here](https://www.r-project.org/).

---

Once you have a 64-bit version of R, search for 'ODBC' in the Windows taskbar. There will be two version (32-bit and 64-bit) ***select the 64-bit version***. This will open the 'ODBC Data Source Administrator (64-bit)' window.

In the new window check for 'Microsoft Access Driver'. If this already exists you can skip to the Python stage.

If 'Microsoft Access Driver' does not exist click 'Add' to install a new driver on your system.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/ODBC_driver.jpg)

---

Select 'Microsoft Access Driver (/*.mdb, /*.accdb)' and click finish.

***If 'Microsoft Access Driver (/*.mdb, /*.accdb)' does not appear, you will need to download the 64-bit driver [here](https://www.microsoft.com/en-US/download/details.aspx?id=13255)***

In the next window, you ***must*** add a 'Data Source Name'. Everything else can be left blank.

![](https://github.com/LiamDBailey/SPIbirds/blob/master/inst/extdata/README_imgs/Add_name.jpg)

Check if this driver is installed and recognised by R using the function `odbcListDrivers()` in the `odbc` package. Note that you will need to open a new session of R before the driver will appear.

##### Python 3

To install Python, we recommend using the [Anaconda distribution](https://www.anaconda.com/distribution/). ***Make sure to download the 3.X version of Python***. The Anaconda distribution comes with some libraries (including *pandas*) pre-loaded.

Once installed, open the 'Anaconda prompt' and type:

`pip install pypxlib`

This will install the *pypxlib* library on your system.

Restart your computer before running the pipelines.

#### Mac

Work in progress..

#### Troubleshooting

If you are still unable to run the pipelines following these setup instructions try these troubleshooting techniques:

- Restart your computer before running pipelines to ensure R recognises the newly installed software and drivers.

- Download the newest version of R [here](https://www.r-project.org/).

- Update all R packages.

---

### Primary data naming conventions

All pipelines assume the primary data is stored using a standard naming protocol: `X_PrimaryData_Y`. Where *X* is the data owner code (described above) and *Y* is additional information used to distinguish between multiple primary data files. The exact naming convention of primary data for each pipeline is described in the help. *Note:* All primary data files for a given pipeline should be stored in a single folder.

### Running the pipelines

Once your computer is set up and primary data follow the correct naming protocol you can run the pipeline function. R will ask you to select the folder where the primary data are stored. You can decide on the output create by the pipeline using the argument `output_type`, which can be either "csv" (as separate .csv files, the default) or "R" (as an R object).

```
format_NIOO(output_type = "R")
```

# SPI-Birds pipeline: Introduction (for the SPI-Birds team)

This provides additional information for the SPI-Birds team, which isn't relevant for the general user.

## Primary data storage

All primary data are stored within a single folder on the NIOO SPI-Birds computer. Primary data is stored following the standard naming approach described above `X_PrimaryData_Y`. In addition, you should ensure that:

* The names of primary data file(s) for each population should not be changed. When updating with data from new years, make sure the name of the new data is the same as the old data following the naming protocol described above.
* Data from each population should have its own folder. The folder name should include the letter code associated with the data owner (e.g. HAR for Harjavalta, NIOO for NIOO).
* All populations folders should be stored in the same location.

## Create data in the standard format

Each year when primary data are updated all pipelines will be re-run for all populations. This is done using the function `run_pipelines()`. This function has 4 arguments:

* path: The location of the folder where all population data is stored. Can be left blank and R will prompt you to find the folder.
* PopID: The population code(s) for the populations where you want to run pipelines.
* Species: The species code(s) for the species you want to use (e.g. PARMAJ for great tit). See the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf) for all species codes.
* output_type: Should the data be output in R or as separate .csv files

For example, the below code will format great tit data from Harjavalta (Finland) and Choupal (Portugal) and output .csv files in the location where the data are stored.

```r
run_pipelines(path = "C:\\all_data", PopID = c("HAR", "CHO"), Species = "PARMAJ", output_type = "csv")
```

**Note:** If you select a PopID/Species combination that does not exist, this population will be skipped.

In most cases, we will want to run all pipelines together. In this case, the arguments *PopID* and *Species* can be ignored.
