## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results = 'hide', messages=FALSE, warning = FALSE-----------------
devtools::install_git("https://git.soep.de/opendata/r-package-opendataformat.git",  
                      quiet = TRUE)
library(opendataformat)

## ----read_opendf--------------------------------------------------------------
path <- system.file("extdata", "data.zip", package="opendataformat")
df <- read_opendf(file = path)
#df <- read_opendf(file = "../my_data.zip")

## ----view df, comment = ""----------------------------------------------------
df

## ----view df haven, comment = ""----------------------------------------------
#library(haven)
#View(df)

## ----read_opendf language, eval=FALSE-----------------------------------------
#  df_en <- read_opendf(file = path, languages="en")

## ----read_opendf language all, eval=FALSE-------------------------------------
#  df_en <- read_opendf(file = path, languages="all")

## ----read_opendf language list, eval=FALSE------------------------------------
#  df_en <- read_opendf(file = path, languages=c("en", "de"))

## ----read_opendf all inputs, eval=FALSE---------------------------------------
#  df<-read_opendf(file, languages = "all", nrows = Inf, skip = 0, variables = NULL)

## ----docu_opendf, eval = FALSE------------------------------------------------
#  docu_opendf(df)

## ----docu_opendf print--------------------------------------------------------
docu_opendf(df, style = "print")

## ----docu_opendf variables, comment=""----------------------------------------
docu_opendf(df, variables="yes", style="print")

## ----docu_opendf selected variable default------------------------------------
docu_opendf(df$bap9001, style = "print")

## ----docu_opendf languages all, eval = FALSE----------------------------------
#  docu_opendf(df$bap9001, style = "print", variables="yes", languages = "all")

## ----docu_opendf languages code print-----------------------------------------
docu_opendf(df$bap9001, style = "print", languages = "de")

## ----docu_opendf languages code all data, eval = FALSE------------------------
#  docu_opendf(df, style = "print", variables="yes", languages = "de")

## ----docu own style, eval = TRUE, comment = ""--------------------------------
for (i in names(df)) {
  cat(
    paste0(attributes(df[[i]])$name, ": ", attributes(df[[i]])$label_de, "\n")
  )
}

## ----labels_opendf1, comment = ""---------------------------------------------
labels_opendf(df)

## ----labels_opendf2, comment = ""---------------------------------------------
labels_opendf(df$bap87, valuelabels=T)

## ----docu_opendf setLanguage2, eval = FALSE-----------------------------------
#  df<-setLanguage_opendf(df, language="de")
#  
#  docu_opendf(df$bap9001, style = "print")

## ----display languages--------------------------------------------------------
attributes(df)$languages
attr(df, "languages")

## ----dataset attributes, comment = ""-----------------------------------------
attributes(df)

## ----variable attributes, comment = ""----------------------------------------
attributes(df$bap87)

## ----variable label attributes, comment = ""----------------------------------
attributes(df$bap87)$label_de

## ----variable label attributes2, eval = FALSE, comment = ""-------------------
#  attr(df$bap87, "label_de")

## ----attributes deletion, comment = ""----------------------------------------
attributes(df$bap87)$description_de<-NULL
attributes(df$bap87)$description_de

## ----labels_opendf3, comment = ""---------------------------------------------
labels_opendf(df)

## ----labels_opendf4, comment = ""---------------------------------------------
labels_opendf(df$bap96)

## ----labels_opendf5, eval = FALSE, comment = ""-------------------------------
#  labels_opendf(df, language="en")

## ----labels_opendf6, eval = FALSE, comment = ""-------------------------------
#  df<-setLanguage_opendf(df, language="en")
#  labels_opendf(df)

## ----labels_opendf valuelabels, comment = ""----------------------------------
labels_opendf(df$bap9001, valuelabels=T)


## ----labels_opendf valuelabels2, eval=FALSE, comment = ""---------------------
#  labels_opendf(df$bap9001, retrieve="valuelabels")
#  

## ----labels_opendf valuelabels names, comment = ""----------------------------
names(labels_opendf(df$bap9001, valuelabels=T))

## ----labels_opendf descriptions, comment = ""---------------------------------
labels_opendf(df, retrieve="description")

## ----labels_opendf url, eval = FALSE, comment = ""----------------------------
#  labels_opendf(df, retrieve="url")

## ----labels_opendf type, eval = FALSE, comment = ""---------------------------
#  labels_opendf(df, retrieve="type")

## ----write_opendf, comment = "", eval = FALSE---------------------------------
#  write_opendf(
#    x = df[,1:4],
#    file = "../df_1_4.zip"
#  )
#  
#  #or :
#  df_14<-df[,1:4]
#  write_opendf(
#    x = df[,1:4],
#    file = "df_1_4.zip"
#  )

## ----write_opendf metadata, comment = "", eval = FALSE------------------------
#  write_opendf(
#    x=df,
#    file = "../df_metadata.zip",
#    export_data = FALSE
#  )

## ----write_opendf english, comment = "", eval = FALSE-------------------------
#  write_opendf(
#    x=df,
#    file = "../df_en.zip",
#    languages = "en"
#  )

## ----write_opendf english german, comment = "", eval = FALSE------------------
#  write_opendf(
#    x=df,
#    file = "../df_en_de.zip",
#    languages = c("en","de")
#  )

## ----table, comment = ""------------------------------------------------------
table(df$bap87, useNA = "ifany")

## ----table attributes labels, comment = ""------------------------------------
attributes(df$bap87)$labels_en

## ----table attributes labels_de, comment = ""---------------------------------
attributes(df$bap87)$labels_de

## ----table factor, comment = ""-----------------------------------------------
table(
  factor(
    df$bap87, labels = names(attributes(df$bap87)$labels_en
                             )
    )
  )

## ----table factor labels_opendf, eval=F, comment = ""-------------------------
#  table(
#    factor(
#      df$bap87, labels = names(labels_opendf(df$bap87, valuelabels=T))
#      )
#    )

## ----table factor german, comment = ""----------------------------------------
table(
  factor(
    df$bap87,
      labels=names(attributes(df$bap87)$labels_de)
    )
  )

## ----table factor german labels_opendf, comment = ""--------------------------
table(
  factor(
    df$bap87,
      labels=names(labels_opendf(df$bap87, valuelabels=T, language="de"))
    )
  )

## ----copy data, comment = ""--------------------------------------------------
bap87_rec <- df$bap87

## ----check metadata, comment = ""---------------------------------------------
attributes(bap87_rec)

## ----NA, comment = ""---------------------------------------------------------
for (row in 1:length(bap87_rec)) {
  if (!is.na(bap87_rec[row]) && bap87_rec[row] <= -1) {
    bap87_rec[row] <- NA
  }
}

table(bap87_rec, useNA = "ifany")

## ----check metadata again, comment = ""---------------------------------------
attributes(bap87_rec)$labels_en

## ----copy metadata to recoded var, comment = ""-------------------------------
attributes(bap87_rec)$labels_en <- unname(attributes(df$bap87)$labels_en)[3:7] # values
names(attributes(bap87_rec)$labels_en) <- names(attributes(df$bap87)$labels_en)[3:7] # labels

attributes(bap87_rec)$labels_en

## ----remove language versions of labels, comment = ""-------------------------
attributes(bap87_rec)$labels_de <- unname(attributes(df$bap87)$labels_de)[3:7] # values
names(attributes(bap87_rec)$labels_de) <- names(attributes(df$bap87)$labels_de)[3:7] # labels



## ----change variable name of recoded var, comment = ""------------------------
attributes(bap87_rec)$name <- "bap87_rec"
attributes(bap87_rec)$name

## ----frequency table of recoded var, comment = ""-----------------------------
table(
  factor(
    bap87_rec,
      labels=names(attributes(bap87_rec)$labels_en)
    )
  )

## ----barplot, comment = "", fig.height = 3, fig.width = 5, fig.align = "center"----
barplot(
  table(
  factor(
    bap87_rec,
      labels=names(attributes(bap87_rec)$labels_en)
    )
  ),
  main = attributes(bap87_rec)$description_en, # title
  xlab = paste0(
    attributes(bap87_rec)$name,": ", attributes(bap87_rec)$label), # label 
  sub = attributes(bap87_rec)$url, # subtitle
  cex.main = 0.9, cex.names = 0.7, cex.sub = 0.8, cex.axis = 0.6, cex.lab = 0.7 # font sizes
)

## ----barplot german, comment = "", fig.height = 3, fig.width = 5, fig.align = "center"----
barplot(
  table(
  factor(
    bap87_rec,
      labels=names(attributes(bap87_rec)$labels_de)
    )
  ),
  main = attributes(bap87_rec)$description_de, # title
  xlab = paste0(
    attributes(bap87_rec)$name,": ", attributes(bap87_rec)$label_de), # label 
  sub = attributes(bap87_rec)$url, # subtitle
  cex.main = 0.7, cex.names = 0.5, cex.sub = 0.8, cex.axis = 0.7, cex.lab = 0.7 # font sizes
)

