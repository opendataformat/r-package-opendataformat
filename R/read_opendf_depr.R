#' @title Read data specified as Open Data Format.
#'
#' @description Import data from the Open Data Format to an R data frame.
#'This function is depreciated, please use read_opendf().
#'
#' @import reticulate
#'
#' @param file Path to ZIP file containing data and metadata specified
#' as Open Data Format.
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be imported.
#'
#' * By default all available language variants are imported (\code{languages = "all"}).
#' * You can also choose to import only the default language (\code{languages = "default"}),
#' * or you can select the language by language code, e.g. \code{languages = "en"}.
#'
#' @return R dataframe with attributes including dataset and variable information.
#'
#' @export
#' @examples
#' # get path to example data from the opendataformat package (data.zip)
#' path <- system.file("extdata", "data.zip", package="opendataformat")
#' path
#'
#' # read example data specified as Open Data Format from ZIP file
#' df <- read_opendf_depr(input = path)
#' df
#' attributes(df)
#' attributes(df$bap87)
#'
#' # read example data with language selection
#' df <- read_opendf_depr(input = path, languages = "de")
#' attributes(df$bap87)
#'
#' @export
read_opendf_depr <- function(file, languages = "all") {
  opendataformat::convert_opendf(
    format = "xml2r",
    file,
    languages = languages
  )
}

