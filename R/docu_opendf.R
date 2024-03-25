#' @title Get documentation from R data frame.
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
#' @param languages Select the language in which the descriptions and labels of the data will be displayed
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
#' @param style Selects where the output should be displayed (console ore viewer).
#' * By default the metadata information is displayed in both the console and the viewer.
#' (\code{style = "both"}).
#' (\code{style = "all"}).
#' @param style Selects where the output should be displayed (console ore viewer). Default is "both"
#' * You can choose to display the code only in the console
#' (\code{style = "console"}).
#' (\code{style = "print"}).
#' * You can choose to display the code only in the viewer
#' (\code{style = "viewer"}).
#' (\code{style = "html"}).
#' 
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_opendf"))
#'

#' # view documentation about the dataset in the language that is currently set
#' \dontrun{
#' docu_opendf(df)
#' }
#'
#' # view information from a selected variable in language "en"
#' \dontrun{
#' docu_opendf(df$bap87, languages = "en")
#' }
#'
#' # view dataset information for all available languages
#' \dontrun{
#' docu_opendf(df, languages = "all")
#' }
#' 
#' # print information to the R console
#' \dontrun{
#' docu_opendf(df$bap87, style = "print")
#' }
#' 
#' # print information to the R viewer
#' \dontrun{
#' docu_opendf(df$bap87, style = "viewer")
#' }
#'
#' @export
 
docu_opendf<-function(input, languages="current", style="both"){

  
  if (("data.frame" %in% class(input) & !("opendf" %in% class(input)))| (!("lang" %in% names(attributes(input))) & !("languages" %in% names(attributes(input)))) ){
    stop("Input is not a dataframe or variable in the opendf-format.")
  }
  
  #check whether input is dataset or variable
  if ("data.frame" %in% class(input)){
    input_type<-"Dataset"
  } else {
    input_type="Variable"
    if (class(input)=="NULL") stop("Input not found")
  }
  
  #assign languages and currentlanguage
  input_languages<-attr(input, "languages")
  input_lang<-unlist(attr(input, "lang"))
  
  #Check if languages argument is valid
  if (!(languages %in% c(input_languages, "current", "default", "all"))) stop("Your language selection is not valid.")
  
  
  #if languages is set to default, but no default language exists, the current language is used
  if(languages=="default"){
    if(!("default"%in%input_languages)){
      languages=attr(input, "lang")
    }
  }
  #if languages is set to current, the current language is used
  if(languages=="current"){
    languages=attr(input, "lang")
  }
  #transform "de" to "_de"
  if (languages!="all") languages=paste0("_", languages)
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
  if (url!="" & exists("style_hyperlink")){
    interactive_url<- cli::style_hyperlink(
      text = url,
      url = url
    )
  } else {
    interactive_url<-url
  }
  
  #get value labels for each language
  if(input_type=="Variable"){
    valuelabels=list()
    valuelabels_html=list()
    type=attr(input, "type")
    for (l in languages){
      labels=attr(input, paste0("labels",l))
      labels_names=names(attr(input, paste0("labels",l)))
      valuelabels[l]<-paste0("    ",labels, ": ", labels_names, "\n", collapse="")
      valuelabels_html[l]<-paste0("<tr><td>&#160;&#160;&#160;&#160;",labels, "</td><td>&#160;&#160;&#160;&#160;", labels_names, "</td></tr>", collapse="")
    }
  }
  
  #######  format output ######
  #name and url
  printing_output<-c(
    crayon::underline(crayon::bold(paste0(input_type, ":\n"))),
    paste0("    ", name, "\n")
  )
  html_output<-paste0(
    "<html><body>",
    "<h2>", input_type, ": ",  name,"</h2>"
    )

  #label
  for (l in languages){
    printing_output<-c(
      paste0(printing_output),
      crayon::bold(paste0("Label (", gsub("_","",l), "):\n")),
      paste0("    ", label[[l]], "\n")
    )
    html_output<-paste0(
      html_output,
      "<h3>Label (", gsub("_","",l), "):","</h3>", "<p>", label[[l]], "</p>"
    )
  }
  
  #Description
  for (l in languages){
    printing_output<-c(
      paste0(printing_output),
      crayon::bold(paste0("Description (", gsub("_","",l), "):\n")),
      paste0("    ", description[[l]], "\n")
    )
    html_output<-paste0(
      html_output,
      "<h3>Description (", gsub("_","",l), "):","</h3>", "<p>", description[[l]], "</p>"
    )
  }
  
  #Value Labels
  if (input_type=="Variable"){
    for (l in languages){
      if (valuelabels[[which(names(valuelabels)==l)]] != "    : \n"){
        printing_output<-c(
          paste0(printing_output),
          crayon::bold(paste0("Value Labels ", gsub("_","",l), ":\n")),
          paste0(valuelabels[[which(names(valuelabels)==l)]])
        )
        html_output<-paste0(
          html_output,
          "<h3>Value Labels (", gsub("_","",l), "):",
          "<table><tr><th>Values</th><th>Labels</th></tr>", 
          paste0(valuelabels_html[[which(names(valuelabels_html)==l)]]),
          "</table>"
        )
      }
    }
    #Type
    printing_output<-c(
      paste0(printing_output),
      crayon::bold("type:\n"),
      paste0("    ", type, "\n")
    )
    html_output<-paste0(
      html_output,
      "<h3>type:","</h3>", type
    )
  }
  
  #languages
  if (input_type=="Dataset"){
    printing_output<-c(
      paste0(printing_output),
      crayon::bold("languages:\n"),
      paste0("    ", paste0(input_languages,collapse = " "), " (active: ", input_lang, ")", "\n")
    )
    html_output<-paste0(
      html_output,
      "<h3>languages:","</h3>", paste0("<p>", paste0(input_languages,collapse = " "), " (active: ", input_lang, ")", "</p>")
    )
  }
  
  #url
  printing_output<-c(
    paste0(printing_output),
    crayon::bold("url:\n"),
    paste0("    ", interactive_url, "\n")
  )
  html_output<-paste0(
    html_output,
    "<h3>url:","</h3>", paste0("<p><a href='",url,"'>",url,"</a></p>")
  )
  
  html_output<-paste0(html_output,"</html></body>")
  #print meta data in console
  if (style %in% c("both", "all", "print", "console")){
    for (i in 1:length(printing_output)){
      cat(printing_output[i])
    }
  }
  #print meta data in viewer
  if (style %in% c("both", "all", "html", "viewer")){
    #create html tempfile and write html output
    tempDir <- tempdir()
    htmlFile <- file.path(tempDir, "docu.html")
    viewer <- getOption("viewer")
    writeLines(html_output
               , htmlFile)
    viewer(htmlFile)
  }
}
