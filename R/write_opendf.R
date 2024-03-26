#' @title Write R data frame to the Open Data Format.
#'
#' @description Export data from an R data frame to a ZIP file that stores
#' the data as Open Data Format.
#'
#' @import reticulate
#'
#' @param x R data frame (df) to be writtem.
#'
#' @param file Path to ZIP file or name of zip file to save the opendf-dataset in the working directory.
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be exported
#'
#' * By default all available language variants are exported
#' (\code{languages = "all"}).
#' * You can also choose to export only the default language
#' (\code{languages = "default"}),
#' * Or only the current language
#' (\code{languages = "current"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#' 
#'
#' @param variables
#' If you are exporting the entire dataset, you can choose whether or not to
#' export all available metadata (labels and descriptions of the dataset and the variables).
#'
#' * By default, all metadata is exported: information describing the dataset
#' itself, as well as information describing all variables in
#' the dataset (\code{variables = "yes"}).
#' * If you set \code{variables = "no"}, only the information describing the
#' dataset is exported.
#'
#' @param export_data
#' Choose, if you want to export the file that holds the
#' data (data.csv).
#'
#' * By default the data and metadata are exported (\code{export_data = "yes"}).
#' * To export only metadata and no data, select \code{export_data = "no"}
#'
#' @return ZIP file and unzipped directory containing the data as CSV file and
#' the metadata as XML file (DDI Codebook 2.5.).
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#' df
#'
#' # write R data frame with attributes to the file my_data.zip specified
#' # as Open Data Format.
#' \dontrun{
#' write_opendf(input = df, output = "my_data")
#' }
#'
#' # write variable from R data frame with attributes to the file my_data.zip specified
#' # as Open Data Format.
#' \dontrun{
#' write_opendf(input = df$bap87, output = "my_data")
#' }
#'
#' # write R data frame with attributes to the file my_data.zip
#' # with selected language.
#' \dontrun{
#' write_opendf(input = df, output = "my_data", languages = "en")
#' }
#'
#' # write R data frame with attributes to the file my_data.zip but only
#' # metadata, no data.
#' \dontrun{
#' write_opendf(input = df, output = "my_data", export_data = "no")
#' }
#'
#'
#' @export
write_opendf <- function(x,
                        file,
                        languages = "all",
                        variables = "yes",
                        export_data = "yes") {
  #if no default labels and descriptions (labels and descriptions without language tag) are available, 
  # return an warning and run write_opendf for the active language
  if (languages=="default"){
    if (!("default" %in% unlist(attributes(x)["languages"]))){
      message(paste0("Metadata saved in language: ",unlist(attributes(x)["lang"])))
      languages=unlist(attributes(x)["lang"])
    }else{
      message("Metadata saved in language default without language tag")
    }
  }
  #set language to current language, if current language is indicated
  if (languages=="current"){
    languages=unlist(attributes(x)["lang"])
  }
  unlink(paste0(tempdir(), "/*"))
  #remove .zip from file name
  file<-gsub(".zip","", file)
  opendataformat::convert_opendf(
    format = "r2xml",
    x,
    file,
    languages,
    variables,
    export_data)
}
