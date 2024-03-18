#' @title Conversion filter for data that is specified as Open Data Format.
#'
#' @description Data stored in an Open Data Format can be converted using the
#' convert_opendf() function. Several conversion steps are possible:
#'
#' * xml2csv: converts the external Open Data Format (XML and CSV files)
#' to the internal Open Data Format (CSV files)
#' * csvr2: converts the internal Open Data Format to an R dataframe
#' * xml2r: converts the external Open Data Format to an R dataframe
#' * r2csv: converts an R data frame to the internal Open Data Format
#' * csv2xml: converts the internal Open Data Format to the external Open Data Format
#' * r2xml: converts an R data frame to the external Open Data Format
#'
#' @import reticulate
#'
#' @param format Specifies the conversion step, e.g. \code{format = "xml2csv"}.
#'
#' @param input Input data, e. g.
#'
#' * path to ZIP file that contains data that is stored in the external Open Data Format,
#' * path to directory that contains data that is stored in the internal Open Data Format,
#' * or name of R data frame, e. g. df
#'
#' @param output Output data, e. g.
#'
#' * path to ZIP file that will contain data that is stored in the external Open Data Format,
#' * path to directory that will contain data that is stored in the internal Open Data Format
#'
#' @param languages
#' For some conversion steps (xml2csv, xml2r, r2csv, r2xml), you can select the language in which the descriptions
#' and labels of the data will be converted.
#'
#' * By default all available language variants are converted (\code{languages = "all"}).
#' * You can also choose to convert only the default language (\code{languages = "default"}),
#' * or you can select the language by language code, e.g. \code{languages = "en"}.
#'
#' @param variables
#' For some conversion steps (r2csv, csv2xml, r2xml), you can choose,
#' if all dataset variables should be converted.
#'
#' @param export_data
#' For some conversion steps (csv2xml, r2xml), you can choose,
#' if you want to export the file that holds the data (data.csv).
#'
#' @return Converted data and metadata that are specified within
#' "Open Data Format" concept.
#'
#' @seealso
#' More information about the Open Data Format specification and
#' data examples are available here:
#' [https://git.soep.de/opendata/](https://git.soep.de/opendata/)
#'
#' @examples
#'
#' # xml2csv: converts the external Open Data Format (XML and CSV files)
#' # to the internal Open Data Format (CSV files)
#' ## get path to example data from the opendataformat package (data.zip)
#' path <- system.file("extdata", "data.zip", package="opendataformat")
#' ## convert
#' \dontrun{
#' convert_opendf(
#'   format = "xml2csv",
#'   input = path,
#'   output = "mydir",
#'   languages = "all"
#' )
#' }
#'
#' # csvr2: converts the internal Open Data Format to an R dataframe
#' ## get path to example data from the opendataformat package
#' ## (data_csv directory containing data.csv, dataset.csv, variables.csv, categories.csv)
#' path <- system.file("extdata", "data_csv", package="opendataformat")
#' ## convert
#' \dontrun{
#' df <- convert_opendf(
#'   format = "csv2r",
#'   input = path
#' )
#' }
#'
#' # xml2r: converts the external Open Data Format to an R dataframe
#' ## get path to example data from the opendataformat package (data.zip)
#' path <- system.file("extdata", "data.zip", package="opendataformat")
#' ## convert
#' \dontrun{
#' df <- convert_opendf(
#'   format = "xml2r",
#'   input = path,
#'   languages = "en"
#' )
#' }
#'
#' # r2csv: converts an R data frame to the internal Open Data Format
#' ## get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#' ## convert
#' \dontrun{
#' convert_opendf(
#'   format = "r2csv",
#'   input = df,
#'   output = "mydir",
#'   languages = "default",
#'   export_data = "yes"
#' )
#' }
#'
#'
#' # csv2xml: converts the internal Open Data Format to the external Open Data Format
#' ## get path to example data from the opendataformat package
#' ## (data_csv directory containing data.csv, dataset.csv, variables.csv, categories.csv)
#' path <- system.file("extdata", "data_csv", package="opendataformat")
#' ## convert
#' \dontrun{
#' convert_opendf(
#'   format = "csv2xml",
#'   input = "mydir",
#'   output = "myzip",
#'   export_data = "no"
#' )
#' }
#'
#' # r2xml: converts an R data frame to the external Open Data Format
#' ## get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#' ## convert
#' \dontrun{
#' convert_opendf(
#'   format = "r2xml",
#'   input = df,
#'   output = "myzip",
#'   languages = "en",
#'   variables = "no",
#'   export_data = "no"
#' )
#' }
#'
#' @export
convert_opendf <- function(format,
                        input,
                        output,
                        languages,
                        variables = "yes",
                        export_data) {
  if (format == "xml2csv") {
    languages <- languages
    py$input_zip <- reticulate::r_to_py(input)
    py$output_dir <- reticulate::r_to_py(output)
    py$languages <- reticulate::r_to_py(languages)
    reticulate::py_run_file(
      system.file(
        "python",
        "xml2csv.py",
        package = "opendataformat"
        )
      )
    message(
      paste0(
        "Your CSV files are stored within the directory:\n",
        output,"."
      )
    )
    }
  if (format == "csv2r") {
    return(csv2r(input))
  }
  # read_opendf()
  if (format == "xml2r") {
    unlink(paste0(tempdir(), "/*"))
    languages <- languages
    py$input_zip <- reticulate::r_to_py(input)
    py$output_dir <- reticulate::r_to_py(tempdir())
    py$languages <- reticulate::r_to_py(languages)
    reticulate::py_run_file(
      system.file(
        "python",
        "xml2csv.py",
        package = "opendataformat"
        )
      )
    message("Your CSV files are stored within the directory:")
    message(tempdir())
    return(csv2r(tempdir())) # input
  }
  if (format == "r2csv") {
    return(r2csv(input,
                 output,
                 languages,
                 variables,
                 export_data))
  }
  if (format == "csv2xml") {
    py$input_dir <- reticulate::r_to_py(input)
    py$output_dir <- reticulate::r_to_py(output)
    py$export_data <- reticulate::r_to_py(export_data)
    py$variables_arg <- reticulate::r_to_py(variables)
    reticulate::py_run_file(
      system.file(
        "python",
        "csv2xml.py",
        package = "opendataformat"
        )
      )
    cat(
      paste0(
        "The XML file metadata.xml and the CSV file data.csv
        are saved within the directory '",output,"',
        as well as within the ZIP file '",output,".zip."
        )
      )
  }
  # write_opendf()
  if (format == "r2xml") {
    unlink(paste0(tempdir(), "/*"))
    r2csv(input, tempdir(), languages = languages, variables, export_data)
    py$input_dir <- reticulate::r_to_py(tempdir()) # input
    py$output_dir <- reticulate::r_to_py(output)
    py$export_data <- reticulate::r_to_py(export_data)
    py$variables_arg <- reticulate::r_to_py(variables)
    reticulate::py_run_file(
      system.file(
        "python",
        "csv2xml.py",
        package = "opendataformat"
        )
      )
    if (export_data == "yes") {
      message(
        paste0(
          "The XML file metadata.xml and the CSV file data.csv
          are saved within the directory '",output,"',
          as well as within the ZIP file '",output,".zip."
          )
        )
    }
  }
}
