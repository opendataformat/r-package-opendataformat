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
# At this point you can install the development version from Gitlab:
# install.packages("devtools")
devtools::install_git("https://git.soep.de/thartl/r-package-new.git")



```
### If the package is published at CRAN, you can use the common way of installation
## install.packages("opendataformat")

## Getting started

``` r
library("opendataformat")
```

The opendataformat package consists of four main functions:

- `read_opendf()` to read an Open Data Format file in R. This function takes an input parameter, which is the path to the Open Data Format ZIP file, and points to an R object for further processing.

- `docu_opendf()` to explore the dataset information. You can set the whole dataset `df` or an selected variable `df$var` as input and you will get an HTML page, displayed either in the RStudio viewer or the Web Browser, with metadata on the respective data level. 

- `setLanguage_opendf()` changes the "active" language of a dataset. The metadata for this language is by default displayed with `docu_opendf()`.

- `write_opendf()` to write the R Dataframe to an Open Data Format ZIP file. By specifying the dataframe input and providing the output directory path the function will generate a ZIP file containing the dataset as "data.csv" and "metadata.xml".

### Multilingual Datasets

When working with a multilingual dataset, the `opendataformatr` package provides the option to specify the language you want to work with for the main functions: `read_opendf()`, `docu_opendf()`, `write_opendf()`
 
You can achieve this by using the `languages` argument and setting it to either 
`all` to include all languages, `default` to use the default language, or by specifying the language code such as `de` for German or `en` for English. 
This allows you to easily select the desired language for your dataset operations.
The language codes are defined by the [ISO 639-1](https://de.wikipedia.org/wiki/Liste_der_ISO-639-1-Codes).


## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [gitlab](https://git.soep.de/opendata/r-package/-/issues). 

