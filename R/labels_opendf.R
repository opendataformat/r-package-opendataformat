#' @title Get variable labels or other metadata from a data frame in opendataformat.
#'
#' @description
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param language Select the language in which the labels of the variables 
#' will be displayed. If no language is selected, the current/active language 
#' of the data frame will be used.
#' \itemize{
#'   \item By default the language that is set to current is displayed (\code{language = "current"}).
#'   \item You can select the language by language code, e.g. \code{language = "en"}.
#' }
#'
#' @param valuelabels You can choose to display the value labels instead of 
#' the variable label, if the input is a variable by setting \code{valuelabels=TRUE} 
#' or \code{valuelabels="yes"}. If the input is a dataset, this argument is 
#' ignored.
#'
#' @param retrieve You can choose to display another attribute/metadata instead 
#' of the variable label(s). Possible options are "description", "url", "type" 
#' or "languages".If \code{valuelabels=TRUE} or \code{valuelabels="yes"} and 
#' the input is a variable, this argument is ignored.
#'
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#' # view the variable labels for all variables in English
#' labels_opendf(input = df, language = "en", valuelabels = FALSE)
#'
#' # view the value labels for variable bap87 in English
#' labels_opendf(input = df$bap87, language = "en", valuelabels = FALSE)
#'
#' # view the description for variable bap87 in English
#' labels_opendf(input = df$bap87, language = "en", retrieve = "description")
#'
#' @export

labels_opendf<-function(input,
                        language="current",
                        valuelabels=F,
                        retrieve="labels") {
  if (language=="current"){
    lang<-attr(input, "lang")
  } else {
    lang<-language
  }
  if (length(lang)>1) stop("Input for language invalid. Please specify only one language.")
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
  if (retrieve=="valuelabels" | retrieve=="valuelabel" | retrieve=="value labels" | retrieve=="value label"){
    valuelabels <- T
    retrieve <- "labels"
  }
  
  #check if retrieve is valid
  if (!(retrieve %in% c("labels", "type", paste0("description_", lang ), "url", "languages"))){
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
    if (valuelabels==F | valuelabels=="no"){
      output_names<-attr(input, "name")
      if (retrieve=="labels"){
        output<-attr(input, paste0("label_", lang))
      } else {
        output<-attr(input, retrieve)
      }
      names(output)<-output_names
    } else {
      if ( valuelabels==T | valuelabels=="yes" ) output<-attr(input, paste0("labels_", lang)) else stop("Function input valuelabels not valid")
    }
  }
  return(output)
}
  