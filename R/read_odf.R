#' @title Read data specified as Open Data Format.
#'
#' @description Import data from the Open Data Format to an R data frame.
#'
#' @import xml2
#' @import data.table
#' @import tibble
#' @importFrom zip zip_list unzip
#'
#'
#' @param file
#' the name of the file which the data are to be read from.
#' By default all available language variants are imported
#' (\code{languages = "all"}).
#'
#' @param languages
#' integer: the maximum number of rows to read in. Negative and other invalid
#' values are ignored.
#'
#' @param nrows
#' Maximum number of lines to read.
#'
#' @param skip
#' Select the number of rows to be skipped (without the column names).
#'
#' @param select
#' 	A vector of column names or numbers to keep, drop the rest. In all forms of
#' 	select, order that the columns are specified determines the order of the
#' 	columns in the result.
#' 	
#' @param na.strings
#' 	A character vector of strings which are to be interpreted as NA values. By
#' 	default, ",," for columns of all types, including type character is read as
#' 	NA for consistency. ,"", is unambiguous and read as an empty string. To
#' 	read ,NA, as NA, set na.strings="NA". To read ,, as blank string "", set
#' 	na.strings=NULL. When they occur in the file, the strings in na.strings
#' 	should not appear quoted since that is how the string literal ,"NA", is
#' 	distinguished from NA, for example, when na.strings="NA".
#' 	
#'
#'
#' @return R dataframe with attributes including dataset and variable
#' information.
#'
#' @export
#' @examples
#' # get path to example data from the opendataformat package (data.zip)
#' path  <-  system.file("extdata", "data.zip", package = "opendataformat")
#' path
#'
#' # read example data specified as Open Data Format from ZIP file
#' df  <-  read_odf(file = path)
#' attributes(df)
#' attributes(df$bap87)
#'
#' # read example data with language selection
#' df  <-  read_odf(file = path, languages = "de")
#' attributes(df$bap87)
#'
#' @export
read_odf  <-  function(file,
                       languages = "all",
                       nrows = Inf,
                       skip = 0,
                       select = NULL,
                       na.strings = getOption("datatable.na.strings", "NA")) {
  is_integer <- function(x) {
    x == round(x)
  }
  
  if (!is_integer(nrows) | nrows < 0){
    stop("nrows must be a positive integer")
  }
  if (!is_integer(skip) | skip < 0){
    stop("skip must be a positive integer")
  }

  #Normalize path from from relative to absolute
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  # replace \\\\ with // to avert errors in data.table::fread(...)
  file <- gsub("\\\\\\\\", "//", file)
  
  if (!file.exists(file)) {
    stop("Error: The specified zip file does not exist.")
  }
  # Step 2: List contents of the zip file
  zip_filelist <- zip::zip_list(file)$filename
  if (!("data.csv" %in% zip_filelist) | !("metadata.xml" %in% zip_filelist)){
    stop("Error: Expected data.csv and metadata.xml in specified zip.")
  }
  
  # load the data csv "data.csv"
  # Unzip the file to a temporary location
  zip::unzip(file, files = "data.csv", exdir = tempdir(), overwrite = TRUE)
  if (skip  !=  0) {
    if (is.null(select)) {
      cnames <- names(data.table::fread(file.path(tempdir(), "data.csv"),
                                        skip = 0, nrows = 1,
                                        na.strings = na.strings))
      data  <-  data.table::fread(file.path(tempdir(), "data.csv"),
                                  skip = skip + 1, nrows = nrows,
                                  col.names = cnames, na.strings = na.strings)
    } else {
      cnames <- names(data.table::fread(file.path(tempdir(), "data.csv"),
                                        skip = 0, nrows = 0,
                                        na.strings = na.strings))
      if (class(select) %in% c("numeric", "integer")) {
        cindex <- select
      } else {
        cindex <- which(cnames %in% select)
      }
      data  <-  data.table::fread(file.path(tempdir(), "data.csv"),
                                  select = cindex, header = FALSE,
                                  skip = skip + 1,
                                  nrows = nrows, col.names = cnames[cindex],
                                  na.strings = na.strings)
    }
  } else {
    data  <-  data.table::fread(file.path(tempdir(), "data.csv"),
                                select = select, skip = skip, nrows = nrows,
                                na.strings = na.strings)
  }
  data <- as_tibble(data)
  #attr(data, "spec") <- NULL
  
  
  #read xml from zipped folder
  metadata <- read_xml(x = unz(file, "metadata.xml"), encoding = "UTF-8")
  
  #Extract study name
  study_metadata <- xml_children(metadata)[grep("<stdyDscr>",
                                                xml_children(metadata))]
  study_metadata <- xml_children(study_metadata)[grep("<citation>",
                                                      xml_children(
                                                        study_metadata))]
  study_metadata <- xml_children(study_metadata)[grep("<titlStmt>",
                                                      xml_children(
                                                        study_metadata))]
  study_metadata <- xml_children(study_metadata)[grep("<titl>",
                                                      xml_children(
                                                        study_metadata))]
  if (length(xml_text(study_metadata)) > 0) {
    study <- xml_text(study_metadata)
  }  else {
    study <- ""
  }
  
  attr(data, "study") <- study
  
  #Extract dataset description
  dataset_metadata <- xml_children(metadata)[grep("<fileDscr>",
                                                  xml_children(metadata))]
  dataset_descrsub <- xml_children(
    xml_children(dataset_metadata)[
      xml_name(xml_children(dataset_metadata)) == "fileTxt"])
  
  #get dataset name study
  filename  <-  xml_text(dataset_descrsub[
    xml_name(dataset_descrsub) == "fileName"])
  attr(data, "name") <- filename
  #get dataset descriptions
  dataset_descriptions <- dataset_descrsub[
    xml_name(dataset_descrsub) == "fileCont"]
  for (description in dataset_descriptions){
    if (languages == "all" ||
        xml_attr(description, attr = "lang") %in% languages) {
      if (length(xml_text(description)) > 0)
        attr(data, paste0("description_",
                          xml_attr(description, attr = "lang"))) <-
          xml_text(description)
      else attr(data, paste0("description_",
                             xml_attr(description, attr = "lang"))) <- ""
    }
  }
  #get dataset labels
  dataset_labels <- xml_children(xml_children(
    dataset_descrsub[xml_name(dataset_descrsub) == "fileCitation"]))
  for (label in dataset_labels){
    if (languages[1] == "all" ||
        xml_attr(label, attr = "lang") %in% languages) {
      if (length(xml_text(label)) > 0)
        attr(data, paste0("label_", xml_attr(label, attr = "lang"))) <-
          xml_text(label)
      else attr(data, paste0("label_", xml_attr(label, attr = "lang")))  <- ""
    }
  }
  #Get dataset url
  url_node <- xml_children(xml_children(dataset_metadata)[
    xml_name(xml_children(dataset_metadata)) == "notes"])
  if (length(xml_attr(url_node, attr = "URI")) > 0) {
    attr(data, "url") <- xml_attr(url_node, attr = "URI")
  } else {
    attr(data, "url") <- ""
  }
  
  
  
  #Extract variable description
  variable_metadata <- xml_children(
    xml_children(metadata)[grep("<dataDscr>", xml_children(metadata))])
  #Loop over MEtadata for each variable to extract
  for (var in variable_metadata){
    varname <- xml_attr(var, attr = "name")
    if (varname %in% colnames(data)) {
      #Get variable name
      attr(data[[varname]], "name") <- xml_attr(var, attr = "name")
      #Get variable labels
      for (label in xml_children(var)[xml_name(xml_children(var)) == "labl"]) {
        if (languages[1] == "all" || xml_attr(
          label, attr = "lang") %in% languages) {
          if (length(xml_text(label)) > 0)
            attr(data[[varname]], paste0(
              "label_", xml_attr(label, attr = "lang"))) <- xml_text(label)
          else attr(data[[varname]], paste0(
            "label_", xml_attr(label, attr = "lang"))) <- ""
        }
      }
      
      #Get variable descriptions
      for (descr in xml_children(var)[xml_name(xml_children(var)) == "txt"]) {
        if (languages[1] == "all" ||
            xml_attr(descr, attr = "lang") %in% languages) {
          if (length(xml_text(descr)) > 0)
            attr(data[[varname]], paste0(
              "description_", xml_attr(descr, attr = "lang"))) <-
              xml_text(descr)
          else attr(data[[varname]], paste0(
            "description_", xml_attr(descr, attr = "lang"))) <- ""
        }
      }
      
      #Get variable type
      type <- xml_attr(xml_children(var)[
        xml_name(xml_children(var)) == "varFormat"], attr = "type")
      if (length(type > 0)) {
        attr(data[[varname]], "type") <- type
      } else {
        attr(data[[varname]], "type") <- ""
      }
      
      #Get variable url
      url <- xml_attr(xml_children(xml_children(var)[
        xml_name(xml_children(var)) == "notes"]), attr = "URI")
      if (length(url) > 0) attr(data[[varname]], "url") <-
        url else attr(data[[varname]], "url") <- ""
      
      #Get variable value labels
      varlabel_nodes <- xml_children(var)[
        xml_name(xml_children(var)) == "catgry"]
      if (length(varlabel_nodes) > 0) {
        varlabels <- list()
        for (varlabel in varlabel_nodes) {
          if (length(varlabels) == 0) {
            varlabels$values <- xml_text(xml_children(varlabel)[
              xml_name(xml_children(varlabel)) == "catValu"])
            labels <- xml_children(varlabel)[
              xml_name(xml_children(varlabel)) == "labl"]
            for (label in labels){
              if (languages[1] == "all" ||
                  xml_attr(label, attr = "lang") %in% languages)
                varlabels[paste0("labels_", xml_attr(label, attr = "lang"))] <-
                  xml_text(label)
            }
          } else {
            varlabels$values <- c(varlabels$values,
                                  xml_text(xml_children(varlabel)[
                                    xml_name(xml_children(varlabel)) ==
                                      "catValu"]))
            labels <- xml_children(varlabel)[
              xml_name(xml_children(varlabel)) == "labl"]
            for (label in labels) {
              if (languages[1] == "all" ||
                  xml_attr(label, attr = "lang") %in% languages)
                varlabels[[paste0(
                  "labels_", xml_attr(label, attr = "lang"))]] <-
                  c(varlabels[paste0("labels_",
                                     xml_attr(label, attr = "lang"))][[1]],
                    xml_text(label))
            }
          }
        }
        #label values as numeric only if all values are numeric
        if (is.numeric(varlabels[names(varlabels) == "values"][[1]])){
          values <- as.numeric(varlabels[names(varlabels) == "values"][[1]])
        } else {
          values <- as.character(varlabels[names(varlabels) == "values"][[1]])
        }
        if (all(as.character(as.numeric(values)) == values)){
          values <- as.numeric(values)
        }
        
        for (i in seq(1, length(varlabels))) {
          if (names(varlabels)[[i]] != "values") {
            names(values) <- varlabels[[i]]
            attr(data[[varname]], names(varlabels)[i]) <- values
          }
        }
      }
    }
  }
  
  #Assign language and active language attributes
  lang_attr <- names(attributes(data))[c(grep("label", names(attributes(data))),
                                         grep("description",
                                              names(attributes(data))))]
  langs <- unique(unlist(lapply(lang_attr,
                                function(x) strsplit(x, "_")[[1]][[2]])))
  attr(data, "languages") <- langs
  if ("en" %in% langs) lang <- "en" else lang <- langs[1]
  attr(data, "lang") <- lang
  for (var in names(data)) {
    attr(data[[var]], "languages") <- langs
    attr(data[[var]], "lang") <- lang
  }
  
  # add label in active language for haven
  attr(data, "label") <- attr(data, paste0("label_", lang))
  for (var in names(data)) {
    attr(data[[var]], "label") <- attr(data[[var]], paste0("label_", lang))
  }
  
  #add odf class
  attr(data, "class") <- c("odf_tbl", attr(data, "class"))
  
  return(data)
}
