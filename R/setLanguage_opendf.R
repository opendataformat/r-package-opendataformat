#' @title Change language of dataframe metadata
#'
#' @description
#' Changes the active language of a dataframe with metadata for the docu_opendf function.
#'
#' @param dataframe R data frame (df) enriched with metadata in the opendf-format.
#'
#' @param languages
#' Select the language to which you want to switch the metadata.
#'
#' @return Dataframe
#'
#' @examples
#' # get example data from the opendataformat package
#' \dontrun{
#' df <- get(data("data_opendf"))
#' }
#'
#' # Switch dataset df to language "en" 
#' \dontrun{
#' df <- setLanguage_opendf(df, languages = "en")
#' }
#' 
#' # Display dataset information for dataset df. Metadata for language "en" is displayed.
#' \dontrun{
#' docu2_opendf(df)
#' }
#'
#' @export

setLanguage_opendf<-function(dataframe, language){
  df_languages<- attr(dataframe, "languages")
  #check if language is available for the dataframe
  if (language %in% df_languages){
    attr(dataframe, "lang")<-language
    attributes(dataframe)[["label"]]<-attr(dataframe, paste0("label_", language))
    for (var in names(var)){
      attr(dataframe[[var]], "lang")<-language
      attr(dataframe[[var]], "label")<-attributes(dataframe)[[paste0("label_", language)]]
    }
  } else {
    stop(paste0("Language '", language, "' not available for the dataset."))
  }
  return(dataframe)
}