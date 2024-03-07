#' @title Get documentation from R data frame.
#'
#' @description
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @import reticulate
#'
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be displayed
#'
#' * By default the language that is set to default is displayed
#' (\code{languages = "default"}).
#' * You can choose to view all available language variants by selecting
#' (\code{languages = "all"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#'
#' @param style
#' You can choose between three display formats:
#'
#' * By default the documentation is displayed
#' as an HTML page (\code{style = "html"}).
#' * To view the documentation within the R console, select \code{style = "print"}
#' * If you want to take a look at both styles, use \code{style = "all"}.
#'
#' @param variables
#' You can choose whether or not to display all the available metadata
#' (labels and descriptions of the data set and the variables).
#'
#' * By default, information describing the dataset is displayed (\code{variables = "no"}).
#' * Selecting \code{variables = "yes"} will also display information about
#' the variables included in the dataset.
#'
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#'
#' \dontrun{
#' # view documentation about the dataset
#' docu_opendf(df)
#'
#' # view information from a selected variable
#' docu_opendf(df$bap87)
#'
#' # view dataset and variable information
#' docu_opendf(df, variables = "yes")
#'
#' # view information for all available languages
#' docu_opendf(df, languages = "all")
#'
#' # print information to the R console
#' docu_opendf(df$bap87, style = "print")
#' }
#'
#'
#' @export
docu_opendf <- function(
  input,
  languages = "active",
  variables = "no",
  style = "html") {
  unlink(paste0(tempdir(), "/*"))
  if (style == "html" | style == "all") {
    if(languages=="active"){
      languages=attr(input, "lang", exact=T)
    }
    #if no language default exists, the active language is used
    if(languages=="default"){
      if(!("default" %in% unlist(strsplit(attr(input, "languages", exact=T), " ")))){
        languages=attr(input, "lang", exact=T)
      }
    }
    suppressMessages(make_docu(input, languages, variables, style))
    # R html viewer
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(paste0(tempdir(),"/docu.html"))
    } else {
      utils::browseURL(paste0(tempdir(),"/docu.html"))
    }
  }
  if (style == "print") {
    suppressMessages(make_docu(input, languages, variables, style))
  }
}
#' @noRd
make_docu <- function(
  input,
  languages,
  variables,
  style) {
  # set temp_dir to R temporary directory
  temp_dir = tempdir()
  # re-use r2csv function from opendataformat package
  opendataformat::r2csv(input, output = temp_dir, languages, variables, export_data = "no")
  # path to temporary output dir > python environment
  py$temp_dir <- reticulate::r_to_py(temp_dir)
  # style argument > python environment
  py$style <- reticulate::r_to_py(style)
  # system path to python module from opendataformat package > python environment
  py$path <- system.file("python", package = "opendataformat")
  # source python script to import python package open_df and run main function
  reticulate::py_run_file(
    system.file(
      "python",
      "docu_opendf.py",
      package = "opendataformat"
    )
  )
}
