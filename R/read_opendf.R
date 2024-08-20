#' @title Read data specified as Open Data Format.
#'
#' @description Import data from the Open Data Format to an R data frame.
#'
#' @import xml2
#' @import zip
#' @import data.table
#' 
#' 
#' @param file
#' the name of the file which the data are to be read from.
#' By default all available language variants are imported (\code{languages = "all"}).
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
#' @param variables
#' Columns to include in the results. You can use the same mini-language as 
#' dplyr::select() to refer to the columns by name. Use c() to use more than 
#' one selection expression. 
#' Although this usage is less common, col_select also accepts a numeric column 
#' index. See ?tidyselect::language for full details on the selection language.
#'
#'
#' @return R dataframe with attributes including dataset and variable 
#' information.
#'
#' @export
#' @examples
#' # get path to example data from the opendataformat package (data.zip)
#' path <- system.file("extdata", "data.zip", package="opendataformat")
#' path
#'
#' # read example data specified as Open Data Format from ZIP file
#' df <- read_opendf(file = path)
#' df
#' attributes(df)
#' attributes(df$bap87)
#'
#' # read example data with language selection
#' df <- read_opendf(file = path, languages = "de")
#' attributes(df$bap87)
#'
#' @export
read_opendf <- function(file,
                        languages = "all",
                        nrows = Inf,
                        skip = 0,
                        variables=NULL) {
  #Normalize path from from relative to absolute
  file<-normalizePath(file, winslash = "/", mustWork = FALSE)
  # replace \\\\ with // to avert errors in data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv')
  file<-gsub("\\\\\\\\", "//", file)
  
  #read xml from zipped folder
  metadata<-read_xml(x = unz(file, "metadata.xml"))

  #Extract study name
  study_metadata<-xml_children(metadata)[grep("<stdyDscr>", xml_children(metadata))]
  study_metadata<-xml_children(study_metadata)[grep("<citation>", xml_children(study_metadata))]
  study_metadata<-xml_children(study_metadata)[grep("<titlStmt>", xml_children(study_metadata))]
  study_metadata<-xml_children(study_metadata)[grep("<titl>", xml_children(study_metadata))]
  if (length(xml_text(study_metadata))>0) study<-xml_text(study_metadata) else study=""
  
  #Extract dataset description
  dataset_metadata<-xml_children(metadata)[grep("<fileDscr>", xml_children(metadata))]
  dataset_attr<-list()
  dataset_descrsub<-xml_children(xml_children(dataset_metadata)[xml_name(xml_children(dataset_metadata))=="fileTxt"])
  
  #get dataset name and combine with study
  filename <- xml_text(dataset_descrsub[xml_name(dataset_descrsub)=="fileName"])
  if (study != filename & study !="") filename <- paste0(study, ": " , filename)
  dataset_attr$name<-filename
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
  if (length(xml_attr(url_node, attr="URI"))>0){
    dataset_attr$url<-xml_attr(url_node, attr="URI")
  } else {
    dataset_attr$url<-""
  }
  
  
  
  #Extract variable description
  variable_metadata<-xml_children(xml_children(metadata)[grep("<dataDscr>", xml_children(metadata))])
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
    
    #Get variable type
    type_node<-xml_children(var)[xml_name(xml_children(var))=="varFormat"]
    type<-xml_attr(type_node, attr="type")
    if (length(type>0))var_attr$type<-type else var_attr$type<-""
    
    
    #Get variable url
    url_node<-xml_children(xml_children(var)[xml_name(xml_children(var))=="notes"])
    url<-xml_attr(url_node, attr="URI")
    if (length(url)>0) var_attr$url<-url else var_attr$url<-""
    
    
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
      #label values as numeric only if all values are numeric
      if (all.equal(as.character(as.numeric(varlabels[names(varlabels)=="values"][[1]])), as.character(varlabels[names(varlabels)=="values"][[1]]))){
        values <- as.numeric(varlabels[names(varlabels)=="values"][[1]])
      } else {
        values <- as.character(varlabels[names(varlabels)=="values"][[1]])
      }
      for (i in 1:length(varlabels)){
        if (names(varlabels)[[i]]!="values"){
          names(values)<-varlabels[[i]]
          var_attr[[names(varlabels)[i]]]<-values
        }
      }
    }
    variable_attr[[var_attr$name]]<-var_attr
  }
  

  # load the data csv "data.csv"
  # Unzip the file to a temporary location
  zip::unzip(file, files = "data.csv", exdir = tempdir(), overwrite = TRUE)
  options(warn = -1)
  if (skip != 0){
    if (is.null(variables)){
      cnames<-names(data.table::fread(file.path(tempdir(), "data.csv"), skip=0, nrows=1))
      data <- data.table::fread(file.path(tempdir(), "data.csv"), skip=skip+1, nrows=nrows, col.names=cnames)
      #data<-data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv'), skip=skip+1, nrows=nrows, col.names=cnames)
      
    } else {
      cnames<-names(data.table::fread(file.path(tempdir(), "data.csv"), skip=0, nrows=0))
      if (class(variables)!="numeric"){
        cindex<-which(cnames %in% variables)
      } else {
        cindex<-variables
      }
      data <- data.table::fread(file.path(tempdir(), "data.csv"), select=cindex, header=F, skip=skip+1, nrows=nrows, col.names=cnames[cindex])
      #data<-data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv'), select=cindex, header=F, skip=skip+1, nrows=nrows, col.names=cnames[cindex])
    }
  } else {
    if (is.null(variables)){
      
      # Read the data using fread
      data <- data.table::fread(file.path(tempdir(), "data.csv"), skip=skip, nrows=nrows)
      #data<-data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv'))
    } else {
      data <- data.table::fread(file.path(tempdir(), "data.csv"), select=variables, skip=skip, nrows=nrows)
      #data<-data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv'), select=variables, skip=skip, nrows=nrows)
    }
  }
  options(warn=0)
  data<-as.data.frame(data)
  attr(data, "spec")<-NULL
  
  #Assign metadata to attributes
  #for dataset
  for (i in 1:length(dataset_attr)){
    attr(data, names(dataset_attr)[i])<-dataset_attr[[i]]
  }
  #for variables
  for (var in 1:length(names(variable_attr))){
    variablename<-names(variable_attr)[var]
    if (variablename %in% names(data)){
      for (i in 1:length(variable_attr[[var]])){
        attr(data[,variablename],as.character(names(variable_attr[[var]])[i]))<-variable_attr[[var]][[i]]
      }
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
  
   #add label in active language for haven
  attr(data, "label")<-attr(data, paste0("label_", lang))
  for (var in names(data)){
    attr(data[,var], "label")<-attr(data[,var], paste0("label_", lang))
  }
  
  #add opendf class
  attr(data, "class")<-c(attr(data, "class"), "opendf")
  
  return(data)
}
