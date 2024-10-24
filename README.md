<h1 style="font-weight:normal" align="center">
  &nbsp;SPI-Birds Network and Database: Pipelines&nbsp;
</h1>

<div align="center">

&nbsp;&nbsp;&nbsp;
<a href="https://spibirds.org"><img border="0" alt="Blog" src="https://assets.dryicons.com/uploads/icon/svg/4926/home.svg" width="40" height="40"></a>&nbsp;&nbsp;&nbsp;
<a href="mailto:spibirds@nioo.knaw.nl"><img border="0" alt="Email" src="https://assets.dryicons.com/uploads/icon/svg/8007/c804652c-fae4-43d7-b539-187d6a408254.svg" width="40" height="40"></a>&nbsp;&nbsp;&nbsp;
<a href="https://twitter.com/spibirds"><img border="0" alt="Twitter" src="https://assets.dryicons.com/uploads/icon/svg/8385/c23f7ffc-ca8d-4246-8978-ce9f6d5bcc99.svg" width="40" height="40"></a>
&nbsp;&nbsp;&nbsp;

</div>

<h3 style="font-weight:normal" align="center">
  &nbsp;Welcome to the pipeline repository for the SPI-Birds Network and Database. Follow the links above to visit our website, contact us via e-mail or Twitter. This README contains all the details required to work with the pipelines, including workflow guidelines for developers.&nbsp;
</h3>

---

<details>
<summary>Table of Contents (general user)</summary>

<!-- toc -->
[Load the pipeline package](#load)  

[Pipeline documentation](#docs)

[Run the pipelines for yourself](#run) 
<!-- tocstop -->

</details>
<details>

<summary>Table of Contents (developers guidelines)</summary>

<!-- toc -->
[Data storage conventions](#storage)

[Naming conventions](#naming)

[Recommended workflow](#workflow)

[Data requests](#requests)

[Archiving](#archiving) 

[Quality check](#quality_check)
<!-- tocstop -->

</details>

---

# SPI-Birds pipeline: Introduction (for the general user)

Welcome to the SPI-Birds pipeline package. This section of the README will give you an introduction on how to load the package, how to find out details about each pipeline, and how to use the package for creating bird data following the [SPI-Birds community data standard](https://github.com/SPI-Birds/documentation/blob/master/standard_protocol/SPI_Birds_Protocol_v2.0.0.pdf) and generating [standard quality checks](https://github.com/SPI-Birds/documentation/blob/master/quality_check/SPI-Birds_quality-check-protocol_v1.1.1.pdf).

<a name="load"/>

## Load the pipeline package

The pipeline package can be installed in R using the following code with the package `remotes`:

```
remotes::install_github("SPI-Birds/pipelines")
library(pipelines)
```

This will install all pipelines and quality check code on your computer and attach our `pipeline` package into your session of R. Individual pipelines are build as separate functions for each data owner (where one data owner can administer multiple populations). Each function is given the name `format_X()` where *X* is the letter code for the data owner. The codes for different data owners and corresponding populations are described in the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v2.0.0.pdf). *Note* in cases where a data owner administers one population, the unique 3 letter population ID code and the data owner code are identical.

<a name="docs"/>

## Pipeline documentation

To process each set of primary data into the structure described in the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v2.0.0.pdf) it is often necessary to make assumptions about how each variable is interpreted. All assumptions made during the pipeline process are described in the help documentation for a given function. This can be accessed using the `?` in R. For example, to read about the assumptions made when processing data from the NIOO, you can use the code:

```
?format_NIOO
```

<a name="run"/>

## Run the pipelines for yourself

Each set of primary data is in a slightly different format. Therefore, to run all pipelines successfully, your system will require additional software and libraries (in addition to R).

### Setup your computer to run pipelines

Pipelines for some populations require additional software and drivers. Setup instructions describe how to install the required software for both a Windows 10 64-bit operating system and Mac OSX. The setup procedure should be similar for other Windows 64-bit systems. If you are unsure which version of Windows is running on your computer, check 'System Type' in 'System Information'. To run the pipelines for all populations a users system must have:

- Java
- Python 3
- Python libraries *pandas* and *pypxlib*

---

#### Windows 10 64-bit

##### Java

Please make sure you install Java that matches your R architecture, so make sure to use the 64-bit version of Java when you use the 64-bit version of R.
`rJava`, the package that allows us to use Java from R, determines the Java location from the registry, so make sure you use the official [Oracle installer](https://www.java.com/en/) so that your Java installation can be found. 

##### Python 3

To install Python, we recommend using the [Anaconda distribution](https://www.anaconda.com/distribution/). ***Make sure to download the 3.X version of Python***. The Anaconda distribution comes with some libraries (including *pandas*) pre-loaded.

Once installed, open the 'Anaconda prompt' and type:

`pip install pypxlib`

This will install the *pypxlib* library on your system.

Restart your computer before running the pipelines.

##### MikTex

To generate the pdf quality check report on Windows you will need to have installed [`MikTex`](https://miktex.org/). If MikTex is not installed, only the html version of the quality check report can be created.

An alternative LaTeX distribution that works well in R is [`TinyTeX`](https://yihui.org/tinytex/).

#### Mac

##### Java

Modern MacOS versions no longer supply Java, so it must be downloaded from third parties. The most commonly used distributions are [adoptium.net](https://adoptium.net/) and [Azul Zulu](https://www.azul.com/downloads/).

When installing from a zip or tar ball, put your Java installation in `/Library/Java/JavaVirtualMachines`. For example, if installing Zulu, unpack/move it such that it results in `/Library/Java/JavaVirtualMachines/zulu-11.jdk`.

`rJava`, the package that allows us to use Java from R, will try to automatically detect the Java location and load it dynamically. You can also check the version selected by your settings via `/usr/libexec/java_home` in the Terminal.

##### Python 3 for Mac

The following notes detail how to set up the python environment on MacOS, including necessary libraries:

* Install Anaconda 3.X (this was last tested with 3.8)

* Check your default python version by opening terminal and typing:`python3 --version`(This should return Python 3.X.Y)

* Check that `pip` is available by typing `pip3 --version` in the terminal

* Update `pip` by typing `python3 -m pip install --user --upgrade pip` in the terminal
    (You have to use the `--user` argument as permission may be denied otherwise)

* Open RStudio and load the reticulate package: `library(reticulate)`

* Check which version of python reticulate is linked to: `py_config()`
    (Required python libraries need to be installed into this virtual environment)

* Install `pandas` library from within R: `py_install("pandas")`

* Install `pypxlib` library from within R: `py_install("pypxlib", pip = TRUE)`
    (Since this is an external library hosted on GitHub, you need to specify installation via pip)

* Check that both libraries are now available:
```
reticulate::py_module_available("pandas")
reticulate::py_module_available("pypxlib")
```
(Both commands should return TRUE)

With this setup, python should be good to go for extracting paradox databases.
(Note that when you install Anaconda, the r-reticulate environment should already be present. If that is not the case, you may have to first generate the environment and link it to RStudio).

##### Pdf compilation on Mac

At present, the `pipelines` package does not create pdf outputs when run on a Mac. 
This is work in progress and will be changed in the future.

#### Troubleshooting

If you are still unable to run the pipelines following these setup instructions try these troubleshooting techniques:

- Restart your computer before running pipelines to ensure R recognises the newly installed software and drivers.

- If R on Windows does not recognise Python's *pandas* module, try installing it using ```reticulate::py_install("pandas")```.

- Download the newest version of R [here](https://www.r-project.org/).

- Update all R packages.

---

<a name="run"/>

### Running the pipelines

Once your computer is set up and primary data follow the correct naming protocol you can run the pipeline function. R will ask you to select the folder where the primary data are stored. You can decide on the output create by the pipeline using the argument `output_type`, which can be either "csv" (as separate .csv files, the default) or "R" (as an R object).

```
format_NIOO(output_type = "R")
```

If you want to run multiple pipelines at once, you can use the `run_pipelines()` function instead.

# Developer guidelines

<a name="storage"/>

## Data storage conventions

### The N drive data folder

All files relevant to SPI-Birds are stored in the N drive on the NIOO server (`N:\Dep.AnE\SPI_Birds\data`). This `data` folder contains separate folders for every data owner in the format `X_Name_Country`, where *X* is the data owner code, *Name* is the name of the data owner, and *Country* is the country where the data owner is based. For example, the NIOO data are stored in the folder:

```
NIOO_NetherlandsInstituteOfEcology_Netherlands
```

### Data owner folders

The folder for each data owner will contain all relevant information for all populations administered by the data owner. This will usually include:

- Primary data
- Meta data
- Archive meta data
- The archive folder

The naming convention of each of these files is described [below](#naming).

### The .standard_format folder

In addition to folders for each data owner, the `data` folder contains the most recent output of all pipelines in the standard format, including an archiving folder. When a data request is made, this version of the standard format can be filtered to meet a given data request (see Data requests below). This is more efficient than re-running pipelines for each data request.

<a name="naming"/>

## Naming conventions

All files used to run pipelines and store data should follow the standard naming convention.

### Primary data

Primary data should be named with the format `X_PrimaryData_Y`. Where *X* is the data owner code (described above) and *Y* is additional information used to distinguish between multiple primary data files. For example, the a data owner `ABC` may have separate primary data files for great and blue tits. These files might then be named:

```
ABC_PrimaryData_Greattit.csv
ABC_PrimaryData_Bluetit.csv
```

### Meta data

All data owners should also provide meta-data about their population(s) in an .xslx file with the format `X_MetaData.xlsx`, where *X* is the data owner code.

### Archive meta data

The folder of each data owner will also include an archive meta data .txt file (the archiving process is explained in more detail below). This file will be in the format `X_ArchiveMetaData.txt`, where *X* is the data owner code.

### Additional files

The data owner may provide other files (e.g. field work protocols, relevant papers). The possible types of files here is unrestricted, to the naming convention must be more flexible. Files can contain any information and be of any file type; however all files should start with the data owner code. For example, the field protocol for data owner `ABC` may be stored as:

```
ABC_FieldProtocol.docx
```

### Pipelines

Code of all pipelines is stored in the /R folder of the pipelines repository. Every pipeline file should follow the naming convention `format_X.R`, where *X* is the data owner code. More details on the structure of pipeline code can be found [below](#workflow).

<a name="workflow"/>

## Recommended developer workflow

Below we describe the workflow that any developer should follow when building a new pipeline.

### Before starting

- Contact the data owner and let them know you have started to work on their data. At this point, it is usually helpful to ask about any changes or additions that may have been made to the primary data since it was first included in the SPI-Birds database.

- Update the SPI-Birds Google Sheet and list the pipeline as 'in progress'.

### Create a new branch

- Pull the newest version of the master branch (`git pull`).

- Create a new branch from the master where you will build your pipeline (`git checkout -b new_branch_name`). Make sure the branch name is clear and concise.

- As you work, you should stage (`git add format_X.R`) and commit (`git commit -m 'commit header' -m 'commit details'`) your work regularly.

*Note* Commits should ideally be distinct blocks of changes with a concise header and detailed description. See some commit best practices [here](https://r-pkgs.org/git.html#commit-best-practices).

- To make commits more easily readable/searchable you should include an emoji at the *start* of each commit message following [these dev guidelines](https://gitmoji.dev/). For example, if you find some typos in the code your commit would be ':pencil2: Fix typo in format_XXX() function'.

- When you have finished working for a day, push your branch to the remote (`git push -u origin new_branch_name` the first time; `git push` afterwards).

### Build the pipeline

- In your new branch, create the new file `format_X.R` in the /R folder, where X is the data owner code.

- This file should contain one parent function (`format_X()`) and at least 4 internal functions for each of the four tables in the standard format:
    - `create_brood_X()`
    - `create_capture_X()`
    - `create_individual_X()`
    - `create_location_X()`
    
- `format_X()` should always take 4 standard arguments:
    * path: The location of the folder where primary data are stored. Can be left blank and R will prompt you to find the folder.
    * PopID: The population code(s) for the populations where you want to run pipelines. Relevant for data owners that administer multiple populations.
    * Species: The species code(s) for the species you want to use (e.g. PARMAJ for great tit). See the [SPI-Birds standard protocol](https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v2.0.0.pdf) for all species codes.
    * output_type: Should the data be output in R or as separate .csv files
    
- These arguments are all documented under pipeline_params in the `zzz.R` file within /R.

- Every function should be documented using `roxygen2`. The 'Details' section of the documentation should be used to record any decisions that were made during the pipeline construction that may be relevant for the data owner, users, or other developers.

- Once a pipeline is finished, add information about the pipeline to `pop_codes.csv` and `pop_species_combos.csv` in the /inst/extdata folder.

- If your pipeline works with a new species, also include this species in `species_codes.csv` in the /inst/extdata folder.
    
*Note:* We recommend you look at other pipelines as a guide.

### Create unit tests

Every pipeline should have a set of unit tests in the /test/testthat folder using the `testthat` package.

- The unit testing code for each pipeline should be stored in a file `test-XXX.R`, where XXX is the data owner code. The file should start with an option to skip if the data path is missing. It should then run the corresponding `format_XXX()` function for the pipeline, followed by the required tests. Unit tests should ensure that primary data has been properly converted to the standard format. This will usually involve comparing the values for a range of different individuals in the standard format (e.g. different species, different sex) to those that would be expected from the primary data. In other words, these tests require some *manual* inspection of the primary data to determine the correct output expected for each individual.

- Each pipeline should undergo five sets of unit tests:
    - Test standard format structure. Have the four tables Brood_data, Capture_data, Individual_data, and Location_data been created.
    - Test brood data.
    - Test capture data.
    - Test individual data.
    - Test location data.
    
- See examples from completed pipelines to better understand the structure of unit testing code.
    
### `test_pipeline()`

Once you have finished the pipeline and written relevant unit tests you should make sure these tests pass.

- Firstly, run unit tests just for your new pipeline. In the console type `test_pipeline(filter = "XXX")`, where XXX is the data owner code of your new pipeline.

- Once your pipeline passes the relevant tests next run the unit tests for *all* existing pipelines by removing the filter argument: `test_pipeline()`. This can be time consuming and a bit annoying, but it is important to regularly test all pipelines in case old code has broken due to e.g. package updates.

- If one or more tests fail you can fix them and trouble shoot using the filter argument as shown above. To test more than one pipeline simultaneously use `test_pipeline(filter = "XXX|YYY")`, where XXX and YYY are two different data owner codes.

### `devtools::check()`

Once your branch is passing all unit tests you should next check the package structure. This will more broadly check things like the documentation, check for unreadable characters, ensure all the code can be loaded. This will *not* re-run the pipeline unit tests, which are skipped at this stage.

- You can check the code using `devtools::check()` or Ctrl/Cmd + Shift + E to run the checks in the build window.

- Currently, the output of `devtools::check()` should include 2 known notes:

```
Imports includes 27 non-default packages.
Importing from so many packages makes the package vulnerable to any of
them becoming unavailable.  Move as many as possible to Suggests and
use conditionally.
```

Package dependencies are discussed in more detail below.

- *Any other ERRORS, WARNINGS, or NOTES must be fixed before continuing! Pull requests will not be accepted otherwise.*

#### Tips for passing `devtools::check()`

##### "no visible binding for global variable"

This will often occur when working with `dplyr` code. All references to columns in a data frame should be prefixed by `.data$`.

##### "no visible global function"

All functions except those in the `base` package should have the package namespace explicitly stated (e.g. `stats::na.omit`).

##### "Undocumented arguments in documentation object 'XXX'"

The function XXX includes an argument that is not documented with `@param` in the roxygen2 documentation code. Check for spelling mistakes!

##### "Documented arguments not in \usage in documentation object 'XXX'"

The function XXX includes documentation for an argument in `@param` in the roxygen2 documentation code that does not appear in the function. Check for spelling mistakes!

##### "Found the following file with non-ASCII characters"

Packages can only include [ASCII characters](https://theasciicode.com.ar/). You can check the character types being used in a line of code with `stringi::stri_enc_mark()`. For example:

```
#Will return ASCII
stringi::stri_enc_mark("ABC")

#Will return UTF-8
stringi::stri_enc_mark("是")
```

Watch out for cases where slanted quotation marks are used (‘’) instead of straight ones ('')! Slanted quotation marks can often be introduced when copying text from outside R, but they are **NOT ASCII**.

If a non-ASCII character must be used, it can be encoded with unicode `\uxxxx`.

### Create a pull request

Once your pipeline is stable and passes all tests and checks it should be reviewed by other developers.

- Push your finished pipeline to the remote (`git push`).

- Visit the [pipelines repository](https://github.com/SPI-Birds/pipelines) and open a pull request.

- Request a reviewer. It is also good to let the person know directly so they don't miss it.

*Note* One key aspect of the code review should also be to test the pipelines on both Mac OSX and Windows.

*Note* The pull request should not be merged until after the data owner confirmation.

- Once the pipeline is stable it can be updated to 'finished' in the Google Sheet.

### Data owner confirmation

The code review should ensure that there are no major bug or oversights. At this point, we can contact the data owner to discuss the pipeline.

- Explain all decisions that were made in the pipeline.

- Confirm any columns/data that were unclear/uncertain.

- Ask about any other data that appear to be missing (e.g. capture data, nest box coordinates).

- At this point, some changes may be needed to incorporate data owner input. If changes are made to the pipeline code it's important that unit tests and checks are run on the code again.

- Record data owner approval in the Google Sheet.

### Merge and quality check

- Once a pipeline has approval from both the data owner and code reviewer the pull request can be merged.

- At this point the working branch can be deleted from the remote and local.

*Note* Remember to pull the newest version of the master branch at this point, it will include the new pipeline.

- Run `quality_check()` on the standard format output from the pipeline. Send the quality check report and the standard format data to the data owner to help them improve data integrity. See more details on the quality check below.

- Contact Antica to update the populations as 'available' on the website.

### Update and archive

- Every time a new pipeline is finished (or an old pipeline updated) we should update and archive the .standard_format folder on the N drive. More about archiving below.

<a name="requests"/>

## Data requests

- A data request will specify the PopIDs and Species of interest. We can return the relevant data in the standard format by running `subset_datarqst()` on the most recent version of the standard format in the .standard_format folder.

- You can choose to include or exclude individuals where the species is uncertain using the `include_conflicting` argument (FALSE by default).

- Run `quality_check()` on the subset of the standard format.

- Provide the user with the subset of the standard format and the quality check report.

<a name="archiving"/>

## Archiving

### Archiving a new population

1. Create a new folder in N:\Dep.AnE\SPI_Birds\data. It should follow the syntax `<OWNERID>_<PopName>_<Country>`
2. Rename files.
    - Primary data should follow the syntax `<OWNERID>_PrimaryData`. If there are multiple primary data files provide different suffixes to differentiate them (e.g. `<OWNERID>_PrimaryData_GTData`
    - Population meta-data should follow the syntax `<OWNERID>_MetaData`
    - All other data that is not meta-data or primary data can be named in any way, but should always start with `<OWNERID>_`
3. Create the initial archive. The below code will generate a `ArchiveMetaData.txt` file and generate an archive folder for the new population. **Important**: Make sure you specify that this is the initial archive with `initial = TRUE`.
```
archive(data_folder = "N:\Dep.AnE\SPI_Birds\data", OwnerID = <OWNERID>, new_data_date = <DATE WHEN DATA WERE RECEIVED>, initial = TRUE)
```

### Archiving updated data

1. Rename new files to match existing data files (i.e. with the syntax `<OWNERID>_PrimaryData`). **Important**: Files should have the **exact** same name, otherwise the pipelines may break. If you do need to use new file names (and rework the pipeline) you will be given a prompt to continue.
2. Decide if we are dealing with a 'minor' update (e.g. fix typos) or a 'major' update (e.g. new year of data).
2. Run archiving code:
```
archive(data_folder = "N:\Dep.AnE\SPI_Birds\data", OwnerID = <OWNERID>, update_type = <"major"/"minor">,
        new_data_path = <LOCATION OF NEW FILES. Consider using choose.files()>,
        new_data_date = <DATE WHEN DATA WERE RECEIVED>, initial = FALSE)
```

### Archiving standard format data

THIS IS STILL DONE MANUALLY AND NEEDS TO BE UPDATED. EVERY TIME A NEW PIPELINE IS FINISHED WE SHOULD ADD THE NEWEST VERSION OF THE STANDARD FORMAT IN .standard_format AND ALSO IN A NEW FOLDER .standard_format/archive/<YYYY_MM_DD>.

<a name="quality_check"/>

## Quality check
Note: the quality check is built for pipelines tailored to version 1.0.0 and 1.1.0 of the standard format. Updating the quality checks to match pipelines tailored to version 2.0.0 of the standard format is in progress.

### Creating checks
The `quality_check()` function is a wrapper function that combines 4 dataset-specific wrapper functions:
- `brood_check()`
- `capture_check()`
- `individual_check()`
- `location_check()`

Each of the dataset-specific functions contains a series of individual quality check functions. These individual quality check functions should be named ‘check_’ or ‘compare_’ followed by a short description of the check and come with a CheckID (e.g. B2 is the second individual check within the `brood_check()` wrapper).

All individual checks should function on rows and flag records as ‘warning’ (unlikely values) or ‘potential error’ (impossible values). 

### Approve-listing
Approve-listed records (i.e. flagged records that are subsequently verified by data owners) should not be flagged by the checks.

If the data owner verifies any records flagged by the quality check (i.e. classifies them as legitimate values) add them to `brood_approved_list.csv`, `capture_approved_list.csv`, `individual_approved_list.csv` or `location_approved_list.csv`.

### Running quality check
The quality check is run on data in the standard format using `quality_check()`. 

The output of the quality check includes:
- A summary table of which checks resulted in warnings and potential errors
- The pipeline output, where each table of the standard format includes two additional columns (Warning and Error) marking the records that resulted in warnings and potential errors
- A report (in html and/or pdf) with a description of all checks and a list of all warnings and potential errors that have been flagged in the pipeline output.

### Troubleshooting

If you have any issues with running the quality check, try these troubleshooting tips:
- Often pipelines make use of several grouping structures (inserted by `dplyr::group_by()` or `dplyr::rowwise()`). Removing these groups (by `dplyr::ungroup()` or `dplyr::summarise(..., .groups = "drop")`) reduces the run time of the quality check considerably.
- If you have trouble creating the pdf, try setting the LaTeX engine to LuaLaTeX (i.e. `quality_check(..., latex_engine = "lualatex")`).
