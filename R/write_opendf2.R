#' @title Write R data frame to the Open Data Format.
#'
#' @description Export data from an R data frame to a ZIP file that stores
#' the data as Open Data Format.
#'
#' @import utils
#' @import xml2
#' @import magrittr
#' @import readr
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
#' @param export_data
#' Choose, if you want to export the file that holds the
#' data (data.csv).Default is  TRUE.
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
write_opendf2 <- function(x,
                         file,
                         languages = "all",
                         export_data=TRUE) {
  #if no default labels and descriptions (labels and descriptions without language tag) are available, 
  # return an warning and run write_opendf for the active language
  if (languages!="all" & !all(languages %in% attr(x, "languages"))) stop("languages not valid")
  unlink(paste0(tempdir(),"/*"), recursive=T)
  folder_url<-gsub(".zip", "",file)
  folder_url<-gsub("\\\\", "/", folder_url)
  folder_name<-strsplit(folder_url, "/")[[1]][length(strsplit(folder_url, "/")[[1]])]
  root_dir<-paste0(strsplit(folder_url, "/")[[1]][-length(strsplit(folder_url, "/")[[1]])], "/", collapse="/")
  if (dir.exists(root_dir)==FALSE & dir.exists(paste0("/",root_dir))==FALSE & dir.exists(paste0("//",root_dir))==FALSE){
    stop("File path not found")
  }
  
  dir.create(paste0(tempdir(), "/", folder_name),  showWarnings = F)

  if (export_data==T) readr::write_csv(x=x, file=paste0(tempdir(), "/", folder_name, "/data.csv"), na = "", progress=F)

  
  
  #create xml root node with codeBook attributes
  metadata<-xml_new_root(.value="codeBook")
  #create codebook attributes
  xml_attr(metadata, attr="xmlns:xsi")<-"http://www.w3.org/2001/XMLSchema-instance"
  xml_attr(metadata, attr="xmlns")<-"ddi:codebook:2_5"
  xml_attr(metadata, attr="xsi:schemaLocation")<-"ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  xml_attr(metadata, attr="version")<-"2.5"
  metadata %>% 
    {
      #create dataset/file metadata
      xml_add_child(., "fileDscr") %>%
        {
          xml_add_child(., "fileTxt") %>% 
            {
              xml_add_child(.,"fileName", attr(x, "name"))
              #add dataset descriptions
              for (descr in names(attributes(x))[grepl("description", names(attributes(x)))]){
                lang<-strsplit(descr, "_")[[1]][2]
                if (languages=="all" | lang %in% languages){
                  if (lang=="NA") xml_add_child(.,"fileCont", attr(x, descr)) else xml_add_child(.,"fileCont", attr(x, descr), "xml:lang"=lang)
                }
              }
              #add dataset labels
              xml_add_child(.,"fileCitation") %>%
                {
                  for (labl in names(attributes(x))[grepl("label", names(attributes(x)))]){
                    lang<-strsplit(labl, "_")[[1]][2]
                    if (languages=="all" | lang %in% languages){
                      if (lang=="NA") xml_add_child(.,"titl", attr(x, labl)) else xml_add_child(.,"titl", attr(x, labl), "xml:lang"=lang)
                    }
                  }
                }
            }
          #create dataset url
          (xml_add_child(., "notes") %>% xml_add_child("ExtLink", "URI"=attr(x, "url"))) 
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
                for (labl in names(attributes(x[,var]))[grepl("label", names(attributes(x[,var])))]){
                  if (!grepl("labels", labl)) {
                    lang<-strsplit(labl, "_")[[1]][2]
                    if (languages=="all" | lang %in% languages){
                      if (lang=="NA") xml_add_child(.,"labl", attr(x[,var], labl)) else xml_add_child(.,"labl", attr(x[,var], labl), "xml:lang"=lang)
                    }
                  }
                }
                #add variable descriptions
                for (descr in names(attributes(x[,var]))[grepl("description", names(attributes(x[,var])))]){
                  if (!grepl("labels", descr)) {
                    lang<-strsplit(descr, "_")[[1]][2]
                    if (languages=="all" | lang %in% languages){
                      if (lang=="NA") xml_add_child(.,"txt", attr(x[,var], descr)) else xml_add_child(.,"txt", attr(x[,var], descr), "xml:lang"=lang)
                    }
                  }
                }
                #add url
                xml_add_child(., "notes") %>% xml_add_child("ExtLink", "URI"=attr(x[,var], "url"))
                #add variable type
                xml_add_child(.,"varFormat", "type"=attr(x[,var], "type"))
                labels<-names(attributes(x[,var]))[grepl("labels", names(attributes(x[,var])))]
                values<-attr(x[,var], labels[2])
                for (val in values){
                  xml_add_child(., "catgry") %>% 
                    {
                      xml_add_child(., "catValu", val)
                      for (labl in labels){
                        lang<-strsplit(labl, "_")[[1]][2]
                        if (languages=="all" | lang %in% languages){
                          labl_new<-names(attr(x[,var], labl))[attr(x[,var], labl)==val]
                          if(is.na(labl_new)) labl_new<-""
                          if (lang=="NA") xml_add_child(.,"labl", labl_new) else xml_add_child(.,"labl", labl_new, "xml:lang"=lang)
                        }
                      }
                    }
                }
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
    utils::zip(zipfile=gsub("/", "\\\\", file),c("data.csv", "metadata.xml"), flags="-q")
  } else {
    utils::zip(zipfile=gsub("/", "\\\\", file),c("metadata.xml"), flags="-q")
  }
  setwd(old_wd)
  
  #check if write_opendf was successful
  if (file.exists(file)){
    print(
      paste0(
        "Dataset successfully written to '",file,"'"
      )
    )
  }
}
