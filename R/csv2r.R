#' @title CSV to RData
#'
#' @description Main Functions that are used to convert the internal Open Data Format
#' to an R data frame.
#'
#' @import utils
#'
#' @param input Path to the directory holding the
#' metadata CSV files and the data CSV file.
#'
#' @return Metadata enriched R data frame.
#'
#' @noRd
csv2r <- function(input) {
  data <- load_csv(input, "data.csv")
  data <- dataset_attributes(data, input)
  data <- variables_attributes(data, input)
  data <- categories_attributes(data, input)
  attr(data, "class") <- c(class(data), "opendf")
  
  if("languages" %in% names(attributes(data))){
    #set first language as active language
    attributes(data)["lang"]<-attributes(data)[["languages"]][1]
    #assign label of active (current) language to "label"
    attributes(data)[["label"]]<-attr(data, paste0("label_", attr(data, "lang")))
    for(var in 1:ncol(data)){
      #assign language attributes also to all variables
      attr(data[[var]], "languages")<-attributes(data)[["languages"]]
      attr(data[[var]], "lang")<-attributes(data)[["lang"]]
      #assign label of active (current) language to "label"
      attr(data[[var]], "label")<-attributes(data[[var]])[[paste0("label_", attr(data[[var]], "lang"))]]
    }
  } else {
    message("No labels or descriptions available for dataset and variables.")
  }

  
  return(data)
}
#' @noRd
load_csv <- function(input, filename) {
  if (file.exists(paste0(input, "/", filename)) == TRUE) {
    data <- read.csv(
      paste0(input, "/", filename),
      fileEncoding = "UTF-8"
    )
    #replace NAs in description, label, url and type with ""
    for (col in 1:ncol(data)){
      if (grepl("label", names(data)[col]) | grepl("description",names(data)[col]) | grepl("url",names(data)[col]) | grepl("type",names(data)[col])){
        if(anyNA(data[,col])) data[which(is.na(data[,col])),col]<-""
      }
    }
  return(data)
  }
  if (file.exists(paste0(input, "/", filename)) == FALSE) {
   stop(paste0("Your input directory does not contain the file '", filename, "'."))
  }
}
#' @noRd
get_lang_csv <- function(entity, attribute) {
  column_names <- names(entity)
  lang <- c()
  for (i in 1:length(column_names)) {
    if (startsWith(column_names[i], attribute) == TRUE) {
      lang[i] <- column_names[i]
    }
  }
  lang <- lang[!is.na(lang)]
  lang <- gsub(attribute, "\\1", lang)
  return(lang)
}
#' @noRd
dataset_attributes <- function(dataframe, input) {
  dataset <- load_csv(input, "dataset.csv")
  metadata_without_lang_exists=F
  # name
  attributes(dataframe)[["name"]] <- enc2utf8(dataset[["dataset"]])
  # label
  if ('label' %in% names(dataset) == TRUE) {
    metadata_without_lang_exists=T
    attributes(dataframe)[["languages"]]<-enc2utf8("default")
    attributes(dataframe)[["label_default"]] <-
      enc2utf8(
        dataset[["label"]]
      )
  }
  if ("TRUE" %in% startsWith(names(dataset), "label_") == TRUE) {
    for (i in get_lang_csv(dataset, "label_")) {
      #add each language to characteristic languages
      if ("languages" %in% names(attributes(dataframe))){
        attributes(dataframe)[["languages"]]<-c(
          attributes(dataframe)[["languages"]], enc2utf8(i)
        )
      } else {
        attributes(dataframe)[["languages"]]<-enc2utf8(i)
      }
      
      attributes(dataframe)[paste0("label_", i)] <-
        enc2utf8(
          dataset[[
            paste0(
              "label_",
              i
              )
            ]]
        )
      }
  }
  # description
  if ('description' %in% names(dataset) == TRUE) {
    metadata_without_lang_exists=T
    attributes(dataframe)[["description_default"]] <-
      enc2utf8(
        dataset[["description"]]
      )
    if (!("languages" %in% names(attributes(dataframe)))){
      attributes(dataframe)[["languages"]]<-enc2utf8("default")
    }
    if (!("default" %in% attr(dataframe, "languages"))){
      attributes(dataframe)[["languages"]]<-c(
        attributes(dataframe)[["languages"]], enc2utf8("default")
      )
    }
  }
  if ("TRUE" %in% startsWith(names(dataset), "description_") == TRUE) {
    for (i in get_lang_csv(dataset, "description_")) {
      attributes(dataframe)[paste0("description_", i)] <-
        enc2utf8(
          dataset[[
            paste0(
              "description_",
              i
              )
            ]]
        )
      #add languages to attribute lang, if they don't exist yet
      if (!("languages" %in% names(attributes(dataframe)))){
        attributes(dataframe)[["languages"]]<-enc2utf8(i)
      }
      if (!(i %in% attr(dataframe, "languages"))){
        attributes(dataframe)[["languages"]]<-c(
          attributes(dataframe)[["languages"]], enc2utf8(i)
        )
      }
    }
  }
  # url
  if ('url' %in% names(dataset) == TRUE) {
    attributes(dataframe)["url"] <- enc2utf8(dataset[["url"]])
  }
  if (metadata_without_lang_exists==T) message("Some dataset metadata has no language tag. Metadata assigned to language default.")
  return(dataframe)
}
#' @noRd
variables_attributes <- function(dataframe, input) {
  variables <- load_csv(input, "variables.csv")
  #check for new languages
  langs_var<-get_lang_csv(dataframe, "label_")
  langs_var2<-get_lang_csv(dataframe, "description_")
  if (!is.null(langs_var) | !is.null(langs_var2)){
    for (lang in c(langs_var, langs_var2)){
      if (!("languages" %in% names(attributes(dataframe)))){
        attributes(dataframe)[["languages"]]<-enc2utf8(lang)
      }
      if (!(lang %in% attr(dataframe, "languages"))){
        attributes(dataframe)[["languages"]]<-c(
          attributes(dataframe)[["languages"]], enc2utf8(lang)
        )
      }
    }
  }
  if (("label" %in% names(variables) | "description" %in% names(variables)) & !("default" %in%  attributes(dataframe)[["languages"]])){
    if ("languages" %in% names(attributes(dataframe))){
      attributes(dataframe)[["languages"]]<-c(
        attributes(dataframe)[["languages"]], enc2utf8("default")
      )
    } else {
      attributes(dataframe)[["languages"]]<-enc2utf8("default")
    }
  }
  
  #If variable from variables.csv is not in dataset, we display a warning
  metadata_without_variable_exists=F
  metadata_without_lang_exists=F
  for (var in variables$variable) {
    if(var %in% names(dataframe)){
      attributes(dataframe[[var]])$name <-
        enc2utf8(
          variables["variable"][variables["variable"] == var, ]
        )
      # label
      if ('label' %in% names(variables) == TRUE) {
        metadata_without_lang_exists=T
        attributes(dataframe[[var]])["label_default"] <-
          enc2utf8(
            variables["label"][variables["variable"] == var, ]
          )
      }
      if ('TRUE' %in% startsWith(names(variables), "label_") == TRUE) {
        for (i in get_lang_csv(variables, "label_")) {
          attributes(dataframe[[var]])[paste0("label_", i)] <-
            enc2utf8(
              variables[
                paste0(
                  "label_",
                  i
                )
              ][variables["variable"] == var, ])
        }
      }
      # description
      if ('description' %in% names(variables) == TRUE) {
        metadata_without_lang_exists=T
        attributes(dataframe[[var]])["description_default"] <-
          enc2utf8(
            variables["description"][variables["variable"] == var, ]
          )
      }
      if ("TRUE" %in% startsWith(names(variables), "description_") == TRUE) {
        for (i in get_lang_csv(variables, "description_")) {
          attributes(dataframe[[var]])[paste0("description_", i)] <-
            enc2utf8(
              variables[
                paste0(
                  "description_",
                  i
                )
              ][variables["variable"] == var, ]
            )
        }
      }
      # type
      if ('type' %in% names(variables) == TRUE) {
        attributes(dataframe[[var]])$type <-
          enc2utf8(variables["type"][variables["variable"] == var, ])
      }
      # url
      if ('url' %in% names(variables) == TRUE) {
        attributes(dataframe[[var]])$url <-
          enc2utf8(variables["url"][variables["variable"] == var, ])
      }
    } else {
      metadata_without_variable_exists=T
      warning(paste0("Metadata for ",var, " not assigned: variable not in the dataset."))
    }
  }
  if (metadata_without_lang_exists==T)message("Some variable metadata has no language tag. Metadata assigned to language default.")
  if (metadata_without_variable_exists==T) message("Some variable metadata could not be assigned: variable(s) not in the dataset. For further details, see warnings()")
  return(dataframe)
}
#' @noRd
categories_attributes <- function(dataframe, input) {
  #variables <- load_csv(input, "variables.csv")
  categories <- load_csv(input, "categories.csv")
  valuelabels_without_variable_exists=F
  valuelabels_without_lang_exist=F
  for (var in unique(categories$variable)) {
    if(var %in% names(dataframe)){
      # label
      if ('label' %in% names(categories) == TRUE) {
        valuelabels_without_lang_exist=T
        attributes(dataframe[[var]])$labels_default <-
          categories["value"][categories["variable"] == var, ]
        names(attributes(dataframe[[var]])$labels_default) <-
          enc2utf8(
            categories["label"][categories["variable"] == var, ]
          )
      }
      if ('TRUE' %in% startsWith(names(categories), "label_") == TRUE) {
        for (i in get_lang_csv(categories, "label_")) {
          attributes(dataframe[[var]])[[paste0("labels_", i)]] <-
            categories["value"][categories["variable"] == var, ]
          names(attributes(dataframe[[var]])[[paste0("labels_", i)]]) <-
            enc2utf8(
              categories[
                paste0(
                  "label_",
                  i
                )
              ][categories["variable"] == var, ])
        }
      }
    } else {
      valuelabels_without_variable_exists=T
      warning(paste0("Value Labels for ", var, " not assigned: variable not in the dataset."))
    }
  }
  if (valuelabels_without_lang_exist==T)message ("Value labels without language tag in the metadata. They are assigned to language default.")
  if (valuelabels_without_variable_exists==T) message("Some value labels could not be assigned: variable(s) not in the dataset. For further details, see warnings()")
  return(dataframe)
}
