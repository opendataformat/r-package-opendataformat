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
#' @param file Path to ZIP file or name of zip file to save the opendf-dataset 
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
#' df <- get(data("data_opendf"))
#' df
#'
#' # write R data frame with attributes to the file my_data.zip specified
#' # as Open Data Format.
#' \dontrun{
#' write_opendf(x = df, file = "my_data.zip")
#' }
#'
#' # write variable from R data frame with attributes to the file my_data.zip 
#' # specified as Open Data Format.
#' \dontrun{
#' write_opendf(x = df$bap87,  file = "my_data.zip")
#' }
#'
#' # write R data frame with attributes to the file my_data.zip
#' # with selected language.
#' \dontrun{
#' write_opendf(x = df,  file = "my_data.zip", languages = "en")
#' }
#'
#' # write R data frame with attributes to the file my_data.zip but only
#' # metadata, no data.
#' \dontrun{
#' write_opendf(x = df,  file = "my_data.zip", export_data = "no")
#' }
#'
#'
#' @export
write_opendf <- function(x,
                         file,
                         languages = "all",
                         export_data = TRUE, 
                         verbose = T,
                         compression_level = 5) {
  # Normalize path from from relative to absolute
  file<-normalizePath(file, winslash = "/", mustWork = FALSE)
  # replace \\\\ with // to avert errors in data.table::fread(cmd = paste0('unzip -p "',file, '" data.csv')
  file<-gsub("\\\\\\\\", "//", file)
  
  if(!grepl(".zip", file)){
    file<-paste0(file, ".zip")
  }
  
  #Remove label attributes for haven
  if(!is.null(attr(x,"label"))){
    attr(x,"label")<-NULL
  }
  for (var in names(x)){
    if(!is.null(attr(x[,var],"label"))){
      attr(x[,var],"label")<-NULL
    }
  }
  
  #if no default labels and descriptions (labels and descriptions without language tag) are available, 
  # return an warning and run write_opendf for the active language
  if (languages!="all" & !all(languages %in% attr(x, "languages"))) stop("languages not valid")
  unlink(paste0(tempdir(),"/*"), recursive=T)
  folder_url<-gsub(".zip", "",file)
  folder_url<-gsub("\\\\", "/", folder_url)
  folder_name<-strsplit(folder_url, "/")[[1]][length(strsplit(folder_url, "/")[[1]])]
  root_dir<-paste0(strsplit(folder_url, "/")[[1]][-length(strsplit(folder_url, "/")[[1]])], "/", collapse="")
  if (root_dir=="/") {
    root_dir<-getwd()
    file=paste0(getwd(),"/", file)
    }
  if (dir.exists(root_dir)==FALSE & dir.exists(paste0("/",root_dir))==FALSE & dir.exists(paste0("//",root_dir))==FALSE){
    stop("File path not found")
  }
  
  dir.create(paste0(tempdir(), "/", folder_name),  showWarnings = F)

  if (export_data == T) data.table::fwrite(x = x, file = paste0(tempdir(), "/", folder_name, "/data.csv"), quote = T,  na = "",encoding = "UTF-8")

  
  
  #create xml root node with codeBook attributes
  metadata<-xml_new_root(.value="codeBook")
  #create codebook attributes
  xml_attr(metadata, attr="xmlns:xsi")<-"http://www.w3.org/2001/XMLSchema-instance"
  xml_attr(metadata, attr="xmlns")<-"ddi:codebook:2_5"
  xml_attr(metadata, attr="xsi:schemaLocation")<-"ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  xml_attr(metadata, attr="version")<-"2.5"
  metadata %>% 
    {
      #create mandatory study description metadata
      xml_add_child(., "stdyDscr") %>%
        {
          xml_add_child(., "citation") %>% 
            {
              xml_add_child(.,"titlStmt") %>%
                {
                  xml_add_child(.,"titl", attr(x, "name"))
                }
            }
        }
      #create dataset/file metadata
      xml_add_child(., "fileDscr") %>%
        {
          xml_add_child(., "fileTxt") %>% 
            {
              xml_add_child(.,"fileName", attr(x, "name"))
              
              #add dataset labels
              xml_add_child(.,"fileCitation") %>%
                xml_add_child(.,"titlStmt") %>%
                {
                  if(length(names(attributes(x))[grepl("label", names(attributes(x)))])>0){
                    first_lang=F
                    for (labl in names(attributes(x))[grepl("label", names(attributes(x)))]){
                      lang<-strsplit(labl, "_")[[1]][2]
                      if (languages=="all" | lang %in% languages){
                        if (first_lang==F) {
                          if (lang=="NA") xml_add_child(.,"titl", attr(x, labl)) else xml_add_child(.,"titl", attr(x, labl), "xml:lang"=lang)
                          first_lang=T
                        } else {
                          if (lang=="NA") xml_add_child(.,"parTitl", attr(x, labl)) else xml_add_child(.,"parTitl", attr(x, labl), "xml:lang"=lang)
                        } 
                      }
                    }
                  }
                }
              
              #add dataset descriptions
              if(length(names(attributes(x))[grepl("description", names(attributes(x)))])>0){
                for (descr in names(attributes(x))[grepl("description", names(attributes(x)))]){
                  lang<-strsplit(descr, "_")[[1]][2]
                  if (languages=="all" | lang %in% languages){
                    if (lang=="NA") xml_add_child(.,"fileCont", attr(x, descr)) else xml_add_child(.,"fileCont", attr(x, descr), "xml:lang"=lang)
                  }
                }
              }
              
            }
          #create dataset url
          url<-attr(x, "url")
          if (is.null(url)) url<-""
          (xml_add_child(., "notes") %>% xml_add_child("ExtLink", "URI"=url)) 
        }
      #add data (variable) metadata
      xml_add_child(., "dataDscr") %>%
        {
          #add metadata for each variable
          for (var in names(x)){
            #variable name
            xml_add_child(., "var", "name"=var) %>%
              {
                #add variable labels
                if(length(names(attributes(x[,var]))[grepl("label", names(attributes(x[,var])))])>0){
                  for (labl in names(attributes(x[,var]))[grepl("label", names(attributes(x[,var])))]){
                    if (!grepl("labels", labl)) {
                      lang<-strsplit(labl, "_")[[1]][2]
                      if (languages=="all" | lang %in% languages){
                        if (lang=="NA") xml_add_child(.,"labl", attr(x[,var], labl)) else xml_add_child(.,"labl", attr(x[,var], labl), "xml:lang"=lang)
                      }
                    }
                  }
                }
                
                #add variable descriptions
                if(length(names(attributes(x[,var]))[grepl("description", names(attributes(x[,var])))])>0){
                  for (descr in names(attributes(x[,var]))[grepl("description", names(attributes(x[,var])))]){
                    if (!grepl("labels", descr)) {
                      lang<-strsplit(descr, "_")[[1]][2]
                      if (languages=="all" | lang %in% languages){
                        if (lang=="NA") xml_add_child(.,"txt", attr(x[,var], descr)) else xml_add_child(.,"txt", attr(x[,var], descr), "xml:lang"=lang)
                      }
                    }
                  }
                }
                
                
                #add value labels
                if(length(names(attributes(x[,var]))[grepl("labels", names(attributes(x[,var])))])>0){
                  labels<-names(attributes(x[,var]))[grepl("labels", names(attributes(x[,var])))]
                  values<-attr(x[,var], labels[1])
                  for (val in values){
                    xml_add_child(., "catgry") %>% 
                      {
                        xml_add_child(., "catValu", val)
                        for (labl in labels){
                          lang<-strsplit(labl, "_")[[1]][2]
                          if (languages=="all" | lang %in% languages){
                            labl_new<-names(attr(x[,var], labl))[attr(x[,var], labl)==val]
                            if(!is.na(labl_new)) {
                              if (lang=="NA") xml_add_child(.,"labl", labl_new) else xml_add_child(.,"labl", labl_new, "xml:lang"=lang)
                            }
                          }
                        }
                      }
                  }
                }
                
                #add variable type
                type<-attr(x[,var], "type")
                if (is.null(type)) type <-class(x[,var])
                xml_add_child(.,"varFormat", "type"=type)
                
                #add url
                url<-attr(x[,var], "url")
                if (is.null(url)) url<-""
                xml_add_child(., "notes") %>% xml_add_child("ExtLink", "URI"=url)
                
              }
          }
        }
    }
  #write metadata.xml
  write_xml(metadata, paste0(tempdir(), "/", folder_name, "/metadata.xml"))
  
  #Zip directory
  old_wd<-getwd()
  setwd(paste0(tempdir(), "/",folder_name))
  if (export_data==T) {
    zip::zip(zipfile=file, files=c("data.csv", "metadata.xml"), compression_level = compression_level)
  } else {
    zip::zip(zipfile=file, files=c("metadata.xml"), compression_level = compression_level)
  }
  setwd(old_wd)

  
  #check if write_opendf was successful
  if (file.exists(file) & verbose==T){
    print(
      paste0(
        "Dataset successfully written to '",file,"'"
      )
    )
  } else {
    if (!file.exists(file)) stop("Datasaet was not written successfully")
  }
}
