#' @title Write R data frame to the Open Data Format.
#'
#' @description Export data from an R data frame to a ZIP file that stores
#' the data as Open Data Format.
#'
#' @import zip
#' @import xml2
#' @import magrittr
#' @import data.table
#'
#' @param x R data frame (df) to be writtem.
#'
#' @param file Path to ZIP file or name of zip file to save the odf-dataset
#' in the working directory.
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be
#' exported
#' * By default all available language variants are exported
#' (\code{languages = "all"}).
#' * You can also choose to export only the default language
#' (\code{languages = "default"}),
#' * Or only the current language
#' (\code{languages = "current"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#'
#' @param compression_level
#' A number between 1 and 9. 9 compresses best, but it also takes the longest.
#'
#'
#'
#' @param export_data
#' Choose, if you want to export the file that holds the
#' data (data.csv).Default is TRUE.
#'
#' * By default the data and metadata are exported (\code{export_data = "yes"}).
#' * To export only metadata and no data, select \code{export_data = "no"}
#'
#' @param verbose Display more messages.
#'
#' @return ZIP file and unzipped directory containing the data as CSV file and
#' the metadata as XML file (DDI Codebook 2.5.).
#'
#' @examples
#' # get example data from the opendataformat package
#' df  <-  get(data("data_odf"))
#'
#' # write R data frame with attributes to the file my_data.zip specified
#' # as Open Data Format.
#' write_odf(x = df, paste0(tempdir(), "/my_data.zip"))
#' 
#' # write R data frame with attributes to the file my_data.zip
#' # with selected language.
#' write_odf(x = df,  paste0(tempdir(), "/my_data.zip"), languages = "en")
#'
#' # write R data frame with attributes to the file my_data.zip but only
#' # metadata, no data.
#' write_odf(x = df,  file = paste0(tempdir(), "/my_data.zip"), export_data = "no")
#'
#'
#' @export
write_odf  <-  function(x,
                        file,
                        languages = "all",
                        export_data = TRUE,
                        verbose = TRUE,
                        compression_level = 5) {
  # Normalize path from from relative to absolute
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  # replace \\\\ with // to avert errors in data.table(...)
  file <- gsub("\\\\\\\\", "//", file)

  if (!grepl(".zip", file)) {
    file <- paste0(file, ".zip")
  }

  #Remove label attributes for haven in active language
  if (!is.null(attr(x, "label"))) {
    attr(x, "label") <- NULL
  }
  for (var in names(x)){
    if (!is.null(attr(x[, var], "label"))) {
      attr(x[, var], "label") <- NULL
    }
  }

  # Return an error, if the language argument is not valid
  if (languages[1] != "all" && !all(languages %in% attr(x, "languages")))
    stop("languages not valid")
  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
  folder_url <- gsub(".zip", "", file)
  folder_url <- gsub("\\\\", "/", folder_url)
  folder_name <- strsplit(folder_url, "/")[[1]][length(strsplit(folder_url,
                                                                "/")[[1]])]
  root_dir <- paste0(strsplit(folder_url, "/")[[1]][-length(
    strsplit(folder_url, "/")[[1]])], "/", collapse = "")
  if (root_dir == "/") {
    root_dir <- getwd()
    file <- paste0(getwd(), "/", file)
  }
  if (dir.exists(root_dir) == FALSE &&
      dir.exists(paste0("/", root_dir)) == FALSE &&
      dir.exists(paste0("//", root_dir)) == FALSE) {
    stop("File path not found")
  }

  dir.create(paste0(tempdir(), "/", folder_name), showWarnings = FALSE)

  if (export_data  == TRUE) data.table::fwrite(
    x = x, file = paste0(tempdir(), "/", folder_name, "/data.csv"),
    quote = TRUE,  na = "", encoding = "UTF-8")

  # Create xml root node with codeBook attributes
  metadata <- xml_new_root(.value = "codeBook")
  
  # Create codebook attributes
  xml_attr(metadata, attr = "xmlns:xsi") <- "http://www.w3.org/2001/XMLSchema-instance"
  xml_attr(metadata, attr = "xmlns") <- "ddi:codebook:2_5"
  xml_attr(metadata, attr = "xsi:schemaLocation") <- "ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  xml_attr(metadata, attr = "version") <- "2.5"
  
  # Create mandatory study description metadata
  stdyDscr <- xml_add_child(metadata, "stdyDscr")
  citation <- xml_add_child(stdyDscr, "citation")
  titlStmt <- xml_add_child(citation, "titlStmt")
  xml_add_child(titlStmt, "titl", attr(x, "study"))
  
  # Create dataset/file metadata
  fileDscr <- xml_add_child(metadata, "fileDscr")
  fileTxt <- xml_add_child(fileDscr, "fileTxt")
  xml_add_child(fileTxt, "fileName", attr(x, "name"))
  
  # Add dataset labels
  fileCitation <- xml_add_child(fileTxt, "fileCitation")
  fileTitlStmt <- xml_add_child(fileCitation, "titlStmt")
  
  if (length(names(attributes(x))[grepl("label", names(attributes(x)))]) > 0) {
    first_lang <- FALSE
    for (labl in names(attributes(x))[grepl("label", names(attributes(x)))]) {
      lang <- strsplit(labl, "_")[[1]][2]
      if (languages[1] == "all" || lang %in% languages) {
        if (first_lang == FALSE) {
          if (lang == "NA") {
            xml_add_child(fileTitlStmt, "titl", attr(x, labl))
          } else {
            xml_add_child(fileTitlStmt, "titl", attr(x, labl), "xml:lang" = lang)
          }
          first_lang <- TRUE
        } else {
          if (lang == "NA") {
            xml_add_child(fileTitlStmt, "parTitl", attr(x, labl))
          } else {
            xml_add_child(fileTitlStmt, "parTitl", attr(x, labl), "xml:lang" = lang)
          }
        }
      }
    }
  }
  
  
  # Add dataset descriptions
  if (length(names(attributes(x))[grepl("description", names(attributes(x)))]) > 0) {
    for (descr in names(attributes(x))[grepl("description", names(attributes(x)))]) {
      lang <- strsplit(descr, "_")[[1]][2]
      if (languages[1] == "all" || lang %in% languages) {
        if (lang == "NA") {
          xml_add_child(fileTxt, "fileCont", attr(x, descr))
        } else {
          xml_add_child(fileTxt, "fileCont", attr(x, descr), "xml:lang" = lang)
        }
      }
    }
  }
  
  # Create dataset URL
  url <- attr(x, "url")
  if (is.null(url)) url <- ""
  notes <- xml_add_child(fileDscr, "notes")
  xml_add_child(notes, "ExtLink", "URI" = url)
  
  # Add data (variable) metadata
  dataDscr <- xml_add_child(metadata, "dataDscr")
  
  # Add metadata for each variable
  for (var in names(x)) {
    varNode <- xml_add_child(dataDscr, "var", "name" = var)
    
    # Add variable labels
    if (length(names(attributes(x[, var]))[grepl("label", names(attributes(x[, var])))]) > 0) {
      for (labl in names(attributes(x[, var]))[grepl("label", names(attributes(x[, var])))]) {
        if (!grepl("labels", labl)) {
          lang <- strsplit(labl, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            if (lang == "NA") {
              xml_add_child(varNode, "labl", attr(x[, var], labl))
            } else {
              xml_add_child(varNode, "labl", attr(x[, var], labl), "xml:lang" = lang)
            }
          }
        }
      }
    }
    
    # Add variable descriptions
    if (length(names(attributes(x[, var]))[grepl("description", names(attributes(x[, var])))]) > 0) {
      for (descr in names(attributes(x[, var]))[grepl("description", names(attributes(x[, var])))]) {
        if (!grepl("labels", descr)) {
          lang <- strsplit(descr, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            if (lang == "NA") {
              xml_add_child(varNode, "txt", attr(x[, var], descr))
            } else {
              xml_add_child(varNode, "txt", attr(x[, var], descr), "xml:lang" = lang)
            }
          }
        }
      }
    }
    
    # Add value labels
    if (length(names(attributes(x[, var]))[grepl("labels", names(attributes(x[, var])))]) > 0) {
      labels <- names(attributes(x[, var]))[grepl("labels", names(attributes(x[, var])))]
      values <- unique(unlist(lapply(labels, function(lab) attr(x[, var], lab))))
      
      for (val in values) {
        catgryNode <- xml_add_child(varNode, "catgry")
        xml_add_child(catgryNode, "catValu", val)
        
        for (labl in labels) {
          lang <- strsplit(labl, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            labl_new <- names(attr(x[, var], labl))[attr(x[, var], labl) == val]
            if (!is.na(labl_new)) {
              if (lang == "NA") {
                xml_add_child(catgryNode, "labl", labl_new)
              } else {
                xml_add_child(catgryNode, "labl", labl_new, "xml:lang" = lang)
              }
            }
          }
        }
      }
    }
    
    # Add variable type
    type <- attr(x[, var], "type")
    if (is.null(type)) type <- class(x[, var])
    xml_add_child(varNode, "varFormat", "type" = type)
    
    # Add URL
    url <- attr(x[, var], "url")
    if (is.null(url)) url <- ""
    notesVar <- xml_add_child(varNode, "notes")
    xml_add_child(notesVar, "ExtLink", "URI" = url)
  }
  
  
  write_xml(metadata, paste0(tempdir(), "/", folder_name, "/metadata.xml"))
  

  #Zip directory
  old_wd <- getwd()
  setwd(paste0(tempdir(), "/", folder_name))
  on.exit(setwd(old_wd))
  if (export_data == TRUE) {
    zip::zip(zipfile = file, files = c("data.csv", "metadata.xml"),
             compression_level = compression_level)
  } else {
    zip::zip(zipfile = file, files = c("metadata.xml"),
             compression_level = compression_level)
  }

  setwd(old_wd)

  #check if write_odf was successful
  if (file.exists(file) && verbose == TRUE) {
    print(
      paste0(
        "Dataset successfully written to '", file, "'"
      )
    )
  } else {
    if (!file.exists(file)) stop("Datasaet was not written successfully")
  }
}
