#' @title Get variable labels or other metadata from a data frame in opendataformat .
#'
#' @description
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @importFrom cli style_hyperlink
#' @importFrom crayon underline
#' @importFrom crayon bold
#' 
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param language Select the language in which the labels of the variables will be displayed. 
#' If no language is selected, the current/active language of the data frame will be used.
#' 
#' @param valuelabels You can choose to display the value labels instead of the variable label,
#' if the input is a variable.
#' 
#' @param retrieve You can choose to display another attribute(meta data) instead of the variable label(s).
#' Possible options are "description", "url" or "type". If valuelabels=TRUE, this argument is ignored.
#' 
#' * By default the language that is set to current is displayed
#' (\code{languages = "current"}).
#' * You can select the language by language code, e.g.
#' \code{languages = "en"}.
#' 
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#'

#' # view the variable labels for all variables in english
#' \dontrun{
#' opendf_labels(input=df, languages="en", valuelabels=F)
#' }
#'
#' # view the value labels for variable bap87 in english
#' \dontrun{
#' opendf_labels(input=df$bap87, languages="en", valuelabels=F)
#' }
#'
#' @export

opendf_labels<-function(input,
                        language="current",
                        valuelabels=F,
                        retrieve="labels") {
  if (language=="current"){
    lang<-attr(input, "lang")
  } else {
    lang<-language
  }
  if (!(lang %in% attr(input, "languages"))){
    stop("Input for language invalid.")
  }
  
  #check spelling of retrieve-parameter
  if (retrieve=="label"){
    retrieve <- "labels"
  }
  if (retrieve=="description" | retrieve=="descriptions"){
    retrieve <- paste0("description_", lang)
  }
  if (retrieve == "urls"){
    retrieve <- "url"
  }
  if (retrieve == "types"){
    retrieve <- "type"
  }
  if (retrieve=="valuelabels" | retrieve=="valuelabel"){
    valuelabels <- T
    retrieve <- "labels"
  }
  
  #check if retrieve is valid
  if (!(retrieve %in% c("labels", "type", paste0("description_", lang ), "url"))){
    stop(paste0("Function input retrieve ", output, " is not valid"))
  }
  
  output<-c()
  output_names<-c()
  if ("data.frame" %in% class(input)){
    output_names<-colnames(input)
    for (var in colnames(input)){
      if (retrieve=="labels"){
        output<-c(output,attr(input[,var], paste0("label_", lang)))
      } else {
        output<-c(output,attr(input[,var], retrieve))
      }
      
    }
    names(output)<-output_names
  } else {
    if (valuelabels==F){
      output_names<-attr(input, "name")
      if (retrieve=="labels"){
        output<-attr(input, paste0("label_", lang))
      } else {
        output<-attr(input, retrieve)
      }
      names(output)<-output_names
    } else {
      output<-attr(input, paste0("labels_", lang))
    }
  }
  return(output)
}
  