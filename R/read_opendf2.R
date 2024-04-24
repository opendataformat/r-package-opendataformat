#' @title Read data specified as Open Data Format.
#'
#' @description Import data from the Open Data Format to an R data frame.
#'
#' @import utils
#' @import xml2
#' 
#' 
#' @param file
#' the name of the file which the data are to be read from.
#' By default all available language variants are imported (\code{languages = "all"}).
#' 
#' @param languages
#' integer: the maximum number of rows to read in. Negative and other invalid values are ignored.
#' 
#' @param nrows
#' Select the number of rows to be skipped.
#' 
#' @param skip
#' integer: the number of lines of the data file to skip before beginning to read data.
#' 
#' @param check.names
#' logical. If TRUE then the names of the variables in the data frame are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by make.names) so that they are, and also to ensure that there are no duplicates.
#' 
#' @param colClasses
#' Possible values are NA (the default, when type.convert is used), "NULL" (when the column is skipped), one of the atomic vector classes (logical, integer, numeric, complex, character, raw), or "factor", "Date" or "POSIXct". Otherwise there needs to be an as method (from package methods) for conversion from "character" to the specified formal class.
#' Note that colClasses is specified per column (not per variable) and so includes the column of row names (if any).
#'
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
#' df <- read_opendf2(file = path)
#' df
#' attributes(df)
#' attributes(df$bap87)
#'
#' # read example data with language selection
#' df <- read_opendf(file = path, languages = "de")
#' attributes(df$bap87)
#'
#' @export
read_opendf2 <- function(file,
                         languages = "all",
                         nrows = -1,
                         skip = 0,
                         check.names=T,
                         colClasses=NA) {
  #file="H:/Testdaten/testdata.zip"
  files<-as.character(unzip(file, list = TRUE)$Name)
  # load the data csv "data.csv"
  data <- read.csv(unz(file, "data.csv"), header = TRUE,
                   sep = ",", skip=skip, nrows=nrows, check.names=check.names, colClasses=colClasses)
  vars<-names(data)
  #library("XML")
  #install.packages("xml2")
  library(xml2)
  metadata<-read_xml(x = unz(file, "metadata.xml"))
  #metadata <- xmlParse(file = unz(file, "metadata.xml"))
  #xml_find_all(metadata, ".//fileDscr")
  
  
  #xml_name(metadata)
  
  #Extract dataset description
  dataset_metadata<-xml_children(metadata)[1]
  dataset_attr<-list()
  dataset_descrsub<-xml_children(xml_children(dataset_metadata)[xml_name(xml_children(dataset_metadata))=="fileTxt"])
  
  #get dataset name
  dataset_attr$name<-xml_text(dataset_descrsub[xml_name(dataset_descrsub)=="fileName"])
  #get dataset descriptions
  dataset_descriptions<-dataset_descrsub[xml_name(dataset_descrsub)=="fileCont"]
  for (description in dataset_descriptions){
    if (languages == "all" | xml_attr(description, attr="lang") %in% languages){
      if (length(xml_text(description))>0) dataset_attr[paste0("description_", xml_attr(description, attr="lang"))]<-xml_text(description) else dataset_attr[paste0("description_", xml_attr(description, attr="lang"))]<-""
    }
  }
  #get dataset labels
  dataset_labels<-xml_children(xml_children(dataset_descrsub[xml_name(dataset_descrsub)=="fileCitation"]))
  for (label in dataset_labels){
    if (languages == "all" | xml_attr(label, attr="lang") %in% languages){
      if (length(xml_text(label))>0) dataset_attr[paste0("label_", xml_attr(label, attr="lang"))]<-xml_text(label) else dataset_attr[paste0("label_", xml_attr(label, attr="lang"))]<-""
    }
  }
  #Get dataset url
  url_node<-xml_children(xml_children(dataset_metadata)[xml_name(xml_children(dataset_metadata))=="notes"])
  dataset_attr$url<-xml_attr(url_node, attr="URI")
  
  
  
  #Extract variable description
  variable_metadata<-xml_children(xml_children(metadata)[2])[xml_attr(xml_children(xml_children(metadata)[2]), attr="name") %in% vars]
  variable_attr<-list()
  #Loop over MEtadata for each variable to extract
  for (var in variable_metadata){
    var_attr<-list()
    
    #Get variable name
    var_attr$name=xml_attr(var, attr="name")
    
    #Get variable labels
    label_nodes<-xml_children(var)[xml_name(xml_children(var))=="labl"]
    for (label in label_nodes){
      if (languages == "all" | xml_attr(label, attr="lang") %in% languages){
        if (length(xml_text(label))>0) var_attr[paste0("label_", xml_attr(label, attr="lang"))]<-xml_text(label) else var_attr[paste0("label_", xml_attr(label, attr="lang"))]<-""
      }
    }
    
    #Get variable descriptions
    descr_nodes<-xml_children(var)[xml_name(xml_children(var))=="txt"]
    for (descr in descr_nodes){
      if (languages == "all" | xml_attr(descr, attr="lang") %in% languages){
        if (length(xml_text(descr))>0) var_attr[paste0("description_", xml_attr(descr, attr="lang"))]<-xml_text(descr) else var_attr[paste0("description_", xml_attr(descr, attr="lang"))]<-""
      }
    }
    
    #Get variable url
    url_node<-xml_children(xml_children(var)[xml_name(xml_children(var))=="notes"])
    url<-xml_attr(url_node, attr="URI")
    if (length(url)>0) var_attr$url<-url else var_attr$url<-""
    
    #Get variable type
    type_node<-xml_children(var)[xml_name(xml_children(var))=="varFormat"]
    type<-xml_attr(type_node, attr="type")
    if (length(type>0))var_attr$type<-type else var_attr$type<-""
    
    #Get variable value labels
    varlabel_nodes<-xml_children(var)[xml_name(xml_children(var))=="catgry"]
    if(length(varlabel_nodes)>0){
      varlabels<-list()
      for (varlabel in varlabel_nodes){
        if (length(varlabels)==0){
          varlabels$values<-xml_text(xml_children(varlabel)[xml_name(xml_children(varlabel))=="catValu"])
          labels<-xml_children(varlabel)[xml_name(xml_children(varlabel))=="labl"]
          for (label in labels){
            if (languages == "all" | xml_attr(label, attr="lang") %in% languages) varlabels[paste0("labels_", xml_attr(label, attr="lang"))]<-xml_text(label)
          }
        } else {
          varlabels$values<-c(varlabels$values, xml_text(xml_children(varlabel)[xml_name(xml_children(varlabel))=="catValu"]))
          labels<-xml_children(varlabel)[xml_name(xml_children(varlabel))=="labl"]
          for (label in labels){
            if (languages == "all" | xml_attr(label, attr="lang") %in% languages) varlabels[[paste0("labels_", xml_attr(label, attr="lang"))]]<-c(varlabels[paste0("labels_", xml_attr(label, attr="lang"))][[1]], xml_text(label))
          }
        }
      }
      values=as.numeric(varlabels[names(varlabels)=="values"][[1]])
      for (i in 1:length(varlabels)){
        if (names(varlabels)[[i]]!="values"){
          names(values)<-varlabels[[i]]
          var_attr[[names(varlabels)[i]]]<-values
        }
      }
    }
    variable_attr[[var_attr$name]]<-var_attr
  }
  
  
  #Assign metadata to attributes
  #for dataset
  for (i in 1:length(dataset_attr)){
    attr(data, names(dataset_attr)[i])<-dataset_attr[[i]]
  }
  #for variables
  for (var in 1:length(names(variable_attr))){
    variablename<-names(variable_attr)[var]
    for (i in 1:length(variable_attr[[var]])){
      attr(data[,variablename], names(variable_attr[[var]])[i])<-variable_attr[[var]][[i]]
    }
  }

  #Assign language and activa language attributes
  lang_attr<-names(attributes(data))[c(grep("label", names(attributes(data))),grep("description", names(attributes(data))))]
  langs<-unique(unlist(lapply(lang_attr, function(x) strsplit(x, "_")[[1]][[2]])))
  #strsplit(names(attributes(data))[c(grep("label", names(attributes(data))),grep("description", names(attributes(data))))], "_")
  attr(data, "languages")<-langs
  if("en" %in% langs) lang<- "en" else lang<-langs[1]
  attr(data, "lang")<-lang
  for (var in names(data)){
    attr(data[,var], "languages")<- langs
    attr(data[,var], "lang")<- lang
  }
  attr(data, "class")<-c(attr(data, "class"), "opendf")
  return(data)
}
