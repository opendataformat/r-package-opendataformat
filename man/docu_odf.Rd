% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docu_odf.R
\name{docu_odf}
\alias{docu_odf}
\title{Get documentation from R data frame.}
\usage{
docu_odf(
  input,
  languages = "current",
  style = "viewer",
  replace_missing_language = FALSE,
  variables = "yes"
)
}
\arguments{
\item{input}{R data frame (df) or variable from an R data frame (df$var).}

\item{languages}{Select the language in which the descriptions and labels of
the data will be displayed.
\itemize{
\item By default the language that is set to current is displayed
(\code{languages = "current"}).
\item The default-option chooses either the default language(if labels and
\item descriptions without a language tag exist)Otherwise the current language
\item is displayed.
(\code{languages = "default"}).
\item You can choose to view all available language variants by selecting
(\code{languages = "all"}),
\item or you can select the language by language code, e.g.
\code{languages = "en"}.
}}

\item{style}{Selects where the output should be displayed (console ore
viewer).By default the metadata information is displayed in the viewer if the
viewer is available.
(\code{style = "console"})
(\code{style = "print"})
\itemize{
\item You can choose to display the code in both the console and the viewer
(\code{style = "both"})
(\code{style = "all"})
\item You can choose to display the code only in the viewer
(\code{style = "viewer"})
(\code{style = "html"})
}}

\item{replace_missing_language}{If only one language is specified in
languages and replace_missing_language is set to TRUE. In case of a
missing label or description, the default or english label/description
is displayed additionally (if one of these is available).}

\item{variables}{Indicate whether a list with all the variables should be
displayed with the dataset metadata.
If the input is a variable/column, the variables-argument will be ignored.
Set (\code{variables = "yes"}) to display the list of variables.}
}
\value{
Documentation.
}
\description{
Get access to information about the dataset
and variables via the R-Studio Viewer or the web browser.
}
\examples{
# get example data from the opendataformat package
df <- get(data("data_odf"))

# view documentation about the dataset in the language that is currently set
docu_odf(df)

# view information from a selected variable in language "en"
docu_odf(df$bap87, languages = "en")

# view dataset information for all available languages
docu_odf(df, languages = "all")

# print information to the R console
docu_odf(df$bap87, style = "print")

# print information to the R viewer
docu_odf(df$bap87, style = "viewer")

# Since the label for language de is missing, in this case the
# english label will be displayed additionally.
attributes(df$bap87)["label_de"] <- ""
docu_odf(df$bap87, languages = "de", replace_missing_language = TRUE)

}
