#' @title Get documentation from R data frame.
#'
#' @description
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @import cli
#'
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be displayed
#'
#' * By default the language that is set to current is displayed
#' (\code{languages = "current"}).
#' * The default-option chooses either the default language(if labels and descriptions without a language tag exist)
#' * Otherwise the current language is displayed.
#' (\code{languages = "default"}).
#' * You can choose to view all available language variants by selecting
#' (\code{languages = "all"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#'
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#'

#' # view documentation about the dataset in the language that is currently set
#' \dontrun{
#' docu2_opendf(df)
#' }
#'
#' # view information from a selected variable in language "en"
#' \dontrun{
#' docu2_opendf(df$bap87, languages = "en")
#' }
#'
#' # view dataset information for all available languages
#' \dontrun{
#' docu2_opendf(df, languages = "all")
#' }
#'
#'
#' @export
 
docu2_opendf<-function(input, languages="current"){
  
  #check whether input is dataset or variable
  if ("data.frame" %in% class(input)){
    input_type<-"dataset"
  } else {
    input_type="variable"
    if (class(input)=="NULL") stop(paste0(variable," not found"))
  }
  
  #assign languages and currentlanguage
  input_languages<-unlist(strsplit(attr(input, "languages"), " "))
  input_lang<-unlist(attr(input, "lang"))
  
  #Check if languages argument is valid
  if (!(languages %in% c(input_languages, "current", "default", "all"))) stop("Invalid argument language.")
  
  
  #if languages is set to default, but no default language exists, the current language is used
  if(languages=="default"){
    if(!("default"%in%input_languages)){
      languages=attr(input, "lang")
    } else {
      #Language default is saved without language tag.
      languages<-""
    }
  }
  #if languages is set to current, the current language is used
  if(languages=="current"){
    languages=attr(input, "lang")
  }
  #transform "de" to "_de"
  if (languages!="" & languages!="all") languages=paste0("_", languages)
  #if languages is set to all, all languages of the dataset ate assigned to languages
  if (languages=="all"){
    languages<-input_languages
    languages=paste0("_", languages)
  }
  #get dataset/variable name
  name=attr(input, "name")
  #get label and description for every language
  label=list()
  description=list()
  for (l in languages){
    label[l]=attr(input, paste0("label",l))
    description[l]=attr(input, paste0("description",l))
  }
  
  #get url
  url=attr(input, "url")
  if (url!=""){
    interactive_url<- cli::style_hyperlink(
      text = url,
      url = url
    )
  } else {
    interactive_url<-url
  }
  
  #get value labels for each language
  if(input_type=="variable"){
    valuelabels=list()
    type=attr(input, "type")
    for (l in languages){
      labels=attr(input, paste0("labels",l))
      labels_names=names(attr(input, paste0("labels",l)))
      valuelabels[l]<-paste0("    ",labels, ": ", labels_names, "\n", collapse="")
    }
  }
  
  #######  format output ######
  #name and url
  printing_output<-c(
    paste0(input_type, ":\n"),
    paste0("    ", name, "\n")
  )
  
  #label
  for (l in languages){
    printing_output<-c(
      paste0(printing_output),
      paste0("Label ", gsub("_","",l), ":\n"),
      paste0("    ", label[[which(names(label)==l)]], "\n")
    )
  }
  
  #languages
  for (l in languages){
    printing_output<-c(
      paste0(printing_output),
      paste0("Description ", gsub("_","",l), ":\n"),
      paste0("    ", description[[which(names(description)==l)]], "\n")
    )
  }
  
  #Value Labels
  if (input_type=="variable"){
    for (l in languages){
      if (valuelabels[[which(names(valuelabels)==l)]] != "    : \n"){
        printing_output<-c(
          paste0(printing_output),
          paste0("Value Labels ", gsub("_","",l), ":\n"),
          paste0(valuelabels[[which(names(valuelabels)==l)]])
        )
      }
    }
    #Type
    printing_output<-c(
      paste0(printing_output),
      paste0("type:\n"),
      paste0("    ", type, "\n")
    )
  }
  
  #languages
  if (input_type=="dataset"){
    printing_output<-c(
      paste0(printing_output),
      paste0("languages:\n"),
      paste0("    ", paste0(input_languages,collapse = " "), " (active: ", input_lang, ")", "\n")
    )
  }
  
  #url
  printing_output<-c(
    paste0(printing_output),
    paste0("url:\n"),
    paste0("    ", interactive_url, "\n")
  )
  #print meta data in console
  for (i in 1:length(printing_output)){
    cat(printing_output[i])
  }
}
