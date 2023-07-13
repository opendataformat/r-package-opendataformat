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
  return(data)
}
#' @noRd
load_csv <- function(input, filename) {
  if (file.exists(paste0(input, "/", filename)) == TRUE) {
    data <- read.csv(
      paste0(input, "/", filename),
      fileEncoding = "UTF-8"
    )
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
  # name
  attributes(dataframe)[["name"]] <- enc2utf8(dataset[["dataset"]])
  # label
  if ('label' %in% names(dataset) == TRUE) {
    attributes(dataframe)[["label"]] <-
      enc2utf8(
        dataset[["label"]]
      )
  }
  if ('label' %in% names(dataset) == FALSE &
      "TRUE" %in% startsWith(names(dataset), "label_") == TRUE) {
    attributes(dataframe)[["label"]] <-
      enc2utf8(
        dataset[[
          paste0(
            "label_",
            get_lang_csv(dataset, "label_")[1]
          )
        ]]
      )
  }
  if ("TRUE" %in% startsWith(names(dataset), "label_") == TRUE) {
    for (i in get_lang_csv(dataset, "label_")) {
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
    attributes(dataframe)[["description"]] <-
      enc2utf8(
        dataset[["description"]]
      )
  }
  if ('description' %in% names(dataset) == FALSE &
      "TRUE" %in% startsWith(names(dataset), "description_") == TRUE) {
    attributes(dataframe)["description"] <-
      enc2utf8(
        dataset[[
          paste0(
            "description_",
            get_lang_csv(dataset, "description_")[1]
            )
          ]]
        )
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
    }
  }
  # url
  if ('url' %in% names(dataset) == TRUE) {
    attributes(dataframe)["url"] <- enc2utf8(dataset[["url"]])
  }

  return(dataframe)
}
#' @noRd
variables_attributes <- function(dataframe, input) {
  variables <- load_csv(input, "variables.csv")
  for (var in variables$variable) {
    # name
    attributes(dataframe[[var]])$name <-
      enc2utf8(
        variables["variable"][variables["variable"] == var, ]
        )
    # label
    if ('label' %in% names(variables) == TRUE) {
      attributes(dataframe[[var]])["label"] <-
        enc2utf8(
          variables["label"][variables["variable"] == var, ]
        )
    }
    if ('label' %in% names(variables) == FALSE &
        'TRUE' %in% startsWith(names(variables), "label_") == TRUE) {
      attributes(dataframe[[var]])["label"] <-
        enc2utf8(
          variables[
            paste0(
              "label_",
              get_lang_csv(variables, "label_")[1]
              )
            ][variables["variable"] == var, ]
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
      attributes(dataframe[[var]])["description"] <-
        enc2utf8(
          variables["description"][variables["variable"] == var, ]
        )
    }
    if ("TRUE" %in% startsWith(names(variables), "description_") == TRUE) {
      for (i in get_lang_csv(variables, "description_")) {
        attributes(dataframe[[var]])["description"] <-
          enc2utf8(
            variables[
              paste0(
                "description_",
                get_lang_csv(variables, "description_")[1]
                )
              ][variables["variable"] == var, ]
            )
      }
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
  }
  return(dataframe)
}
#' @noRd
categories_attributes <- function(dataframe, input) {
  variables <- load_csv(input, "variables.csv")
  categories <- load_csv(input, "categories.csv")
  for (var in variables$variable) {
    # value
    attributes(dataframe[[var]])$labels <-
      categories["value"][categories["variable"] == var, ]
    # label
    if ('label' %in% names(variables) == TRUE) {
      names(attributes(dataframe[[var]])$labels) <-
        enc2utf8(
          categories["label"][categories["variable"] == var, ]
        )
    }
    if ('label' %in% names(variables) == FALSE &
        'TRUE' %in% startsWith(names(variables), "label_") == TRUE) {
      names(attributes(dataframe[[var]])$labels) <-
        enc2utf8(
          categories[
            paste0(
              "label_",
              get_lang_csv(categories, "label_")[1]
            )
          ][categories["variable"] == var, ]
        )
    }
    if ('TRUE' %in% startsWith(names(variables), "label_") == TRUE) {
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
  }
  return(dataframe)
}
