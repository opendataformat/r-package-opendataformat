#' @title RData to CSV
#'
#' @description The r2csv function cssonverts a R data frame to the CSV open data format.
#'
#' @import utils
#'
#' @param input R dataframe object.
#'
#' @param output path to directory for saving the CSV files.
#'
#' @param languages select language option: "all" = all available languages, "default" = language that is set
#'  to default, or language code for exmaple "de" or "en".
#'
#' @param variables Choose, if all dataset variables should be converted.
#'
#' @param export_data choose whether the dataset CSV should be copied to the output directory ("yes") or not ("no").
#'
#' @return CSV files holding the data and the metadata.
#'
#' @export
r2csv <- function(
  input,
  output,
  languages,
  variables,
  export_data) {
  #if no default labels and descriptions (labels and descriptions without language tag) are available, 
  # return an warning and run write_opendf for the active language
  if (languages=="default"){
    if (!("default" %in% attributes(input)[["languages"]])){
      message(paste0("Metadata saved in language: ",attributes(input)[["lang"]]))
      languages=attributes(input)[["lang"]]
    }else{
      message("Metadata saved in language default without language tag")
    }
  }
  #remove labels (duplicate of current/active language labels)
  attr(input, "label") <- NULL
  for (var in names(input)){
    attr(input[[var]], "label") <- NULL
  }
  #assign labels and description of default language to "label" and "description" and remove duplicate
  if ("default" %in% attr(input, "languages")){
    attr(input, "label") <- attr(input, "label_default")
    attr(input, "label_default")<-NULL
    attr(input, "description") <- attr(input, "description_default")
    attr(input, "description_default")<-NULL
    for (var in names(input)){
      attr(input[[var]], "label") <- attr(input[[var]], "label_default")
      attr(input[[var]], "label_default")<-NULL
      attr(input[[var]], "description") <- attr(input[[var]], "description_default")
      attr(input[[var]], "description_default")<-NULL
      attr(input[[var]], "labels") <- attr(input[[var]], "labels_default")
      attr(input[[var]], "labels_default")<-NULL
    }
  }
  # export_data
  if (export_data == "yes") {
    df <- make_data(input)
    # check if output dir is present
    ifelse(!dir.exists(output),
           dir.create(output),
           FALSE)
    write.csv(
      df,
      file = paste0(output, "/data.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8",
      na = "",
      quote = TRUE
    )
  }
  # languages = all
  if (languages == "all") {
    get_csv_all(input, output, variables)
    message("Your CSV files are stored within the directory:")
    message(  output)
  }
  # languages = default
  if (languages == "default") {
    get_csv_default(input, output, variables)
    message("Your CSV files are stored within the directory:")
    message(  output)
  }
  # languages = code
  if (paste0("label_", languages) %in% names(attributes(input)) == TRUE) {
    get_csv_lang(input, output, variables, languages)
    message("Your CSV files are stored within the directory:")
    message(  output)
  }
  # no valid language selection
  if (languages != "all" &
      languages != "default" &
      paste0("label_", languages) %in% names(attributes(input)) == FALSE) {
    # get valid language codes
    lang <- c()
    for (item in names(attributes(input))) {
      if (startsWith(item, "label_") == TRUE) {
        lang <- append(lang, item)
      }
    }
    lang <- lang[!is.na(lang)]
    lang <- gsub("label_", "\\1", lang)
    # message with possibly valid selection options
    message("Your language selection is not valid.")
    message("Try using:")
    message("  'all'")
    message("  'default'")
    for (code in lang) {
      message(paste0("  '",code,"'"))
    }
  }
}
#' @noRd
make_data <- function(input) {
  # input = tibble
  if ("tbl_df" %in% class(input) == TRUE | "tbl" %in% class(input) == TRUE) {
    # convert data: classes "haven_labelled and numeric to string
    input_converted <- input
    for (var in attributes(input_converted)$names) {
      if ("haven_labelled" %in% class(input_converted[[var]]) == TRUE) {
        input_converted[[var]] <- as.character(unclass(input_converted[[var]]))
      }
      if ("numeric" %in% class(input_converted[[var]]) == TRUE) {
        input_converted[[var]] <- as.character(unclass(input_converted[[var]]))
      }
    }
    df <- input_converted
  } else {
    df <- input
  }
  # input = haven labelled
  if ("haven_labelled" %in% class(input) == TRUE) {
    df <- as.character(unclass(input))
  }
  # input = numeric
  if ("numeric" %in% class(input) == TRUE) {
    df <- as.character(unclass(input))
  }
  return(df)
}
#' @noRd
write_odf_csv <- function(dataframe, name, output){
  ifelse(!dir.exists(output),
         dir.create(output),
         FALSE)
  write.csv(
    dataframe,
    file = paste0(output, name),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}
