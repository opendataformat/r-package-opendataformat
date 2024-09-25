# opendataformat 

## Overview

The `opendataformat` package is specifically designed to facilitate the seamless utilization of the Open Data Format. 
It offers functionality to import data from the Open Data Format into an R data frame, as well as export data from an R data frame to the Open Data Format. 
Additionally, you can easily access comprehensive information about the dataset and variables using either the RStudio Viewer or your web browser. 
This user-friendly approach ensures convenient exploration and utilization of dataset information within your preferred environment.

For more comprehensive insights into the Open Data Format specification, please visit: [Open Data Format Specification](https://git.soep.de/opendata/specification). 
This resource provides detailed documentation and profiles illustrating the storage locations of attributes within the Open Data Format, as well as within the native formats to which they will be converted.

Additionally, you will have access to a practical example of [real data in the Open Data Format](https://git.soep.de/opendata/open-data-package).

Learn more about the Open Data Format in R in `vignette("opendataformat")`.

## Installation

``` r

# opendataformat package from Zenodo:
install.packages(
  "https://zenodo.org/records/13683314/files/opendataformat_1.2.1.tar.gz",
  repos = NULL, method = "libcurl")


# Alternatively you can install the development version from Gitlab:
devtools::install_git(
  "https://github.com/opendataformat/r-package-opendataformat.git")



```

## Getting started

``` r
library("opendataformat")
```

The opendataformat package consists of five main functions:

- `read_odf()` to read an Open Data Format file in R. This function takes an input parameter, which is the path to the Open Data Format ZIP file, and points to an R object for further processing.

- `docu_odf()` to explore the dataset information. You can set the whole dataset `df` or an selected variable `df$var` as input and you will get an HTML page, displayed either in the RStudio viewer or the Web Browser, with metadata on the respective data level. 

- `setLanguage_odf()` changes the "active" language of a dataset. The metadata for this language is by default displayed with `docu_odf()`.

- `write_odf()` to write the R Dataframe to an Open Data Format ZIP file. By specifying the dataframe input and providing the output directory path the function will generate a ZIP file containing the dataset as "data.csv" and "metadata.xml".

- `odf_labels()` to retrieve labels and other metadata from an odf-data.frame-object.

### Multilingual Datasets

When working with a multilingual dataset, the `opendataformatr` package provides the option to specify the language you want to work with for the main functions: `read_odf()`, `docu_odf()`, `write_odf()`, and `odf_labels()`.
 
You can achieve this by using the `languages` argument and setting it to either 
`all` to include all languages, `current` (or `default`) to use the currently activated language, or by specifying the language code such as `de` for German or `en` for English. 
This allows you to easily select the desired language for your dataset operations.
The language codes are defined by the [ISO 639-1](https://de.wikipedia.org/wiki/Liste_der_ISO-639-1-Codes).
Note that for the function `odf_labels()` you can specify only one language, therefore the `language` argument only takes single languages as input.


## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on https://github.com/thartl-diw/r-package-opendataformat/issues. 

