#' @title RData to CSV for selected language
#'
#' @description Functions that are used to convert an R data frame
#' to the internal Open Data Format (CSV files) if the language is selected
#' by language code.
#'
#' @import utils
#'
#' @param input R data frame object.
#'
#' @param output Path to directory for saving the CSV files.
#'
#' @param languages The language is set to the selected language code, e.g. "de" or "en".
#'
#' @param variables Choose "yes" if all data set variables should be
#' converted or "no" if not.
#'
#' @param export_data Choose "yes" if the data set CSV should be copied
#' to the output directory or "no" if not.
#'
#' @return CSV files holding the data and the metadata for the selected language.
#'
# --------------------
# dataset.csv with DATA FRAME INPUT
# --------------------
#' @noRd
get_dataset_lang <- function(data, languages) {
        header <- NULL
        metadata <- NULL
        # - data set name
        if ("name" %in% names(attributes(data))) {
            header <- "dataset"
            metadata <- attributes(data)$name

        } else {
            header <- "dataset"
            metadata <- "dataset name"
        }
        # - data set label
        if (paste0("label_", languages) %in% names(attributes(data))) {
          header <- append(header, paste0("label_", languages))
          metadata <- append(metadata, attributes(data)[[paste0("label_", languages)]])
        }
        # - data set description
        if (paste0("description_", languages) %in% names(attributes(data))) {
          header <- append(header, paste0("description_", languages))
          metadata <- append(metadata, attributes(data)[[paste0("description_", languages)]])
        }
        # - url
        if ("url" %in% names(attributes(data))) {
            header <- append(header, "url")
            metadata <- append(metadata, attributes(data)$url)
        }
        # - make data frame for data set information
        dataset_df <- data.frame()
        dataset_df <- rbind(dataset_df, metadata)
        names(dataset_df) <- header
        return(dataset_df)
}
# --------------------
# variables.csv with DATA FRAME INPUT
# --------------------
#' variables header
#' @noRd
var_header_data_lang <- function(data, languages) {
    var_header <- c()
    # - get variable attributes from all variables in data set
    for (var in attributes(data)$names) {
        var_header <- append(var_header,
                                  names(attributes(data[[var]])))
    }
    # - keep unique variable attributes
    var_header <- unique(var_header)
    # - change name to variable
    if ("name" %in% var_header) {
        var_header <- gsub("name", "variable", var_header)
    } else {
        var_header <- append("variable", var_header)
    }
    # - drop labels attributes (categories.csv) and
    # - not selected label and description
    var_header_reduced <- c()
    for (item in var_header) {
        if (startsWith(item, "labels") == FALSE &&
            item != "label" &&
            item != "description" &&
            startsWith(item, "label_") == FALSE &&
            startsWith(item, "description_") == FALSE
            ) {
            var_header_reduced <- append(var_header_reduced, item)
        }
        if (item == paste0("label_", languages)) {
          var_header_reduced <- append(var_header_reduced, item)
        }
        if (item == paste0("description_", languages)) {
          var_header_reduced <- append(var_header_reduced, item)
        }
    }
    return(var_header_reduced)
}
#' variables matrix
#' @noRd
var_matrix_lang <- function(data, languages) {
    # - make matrix
    var_mat <- matrix(
        nrow = length(attributes(data)$names), # num variables
        ncol = length(var_header_data_lang(data, languages)), # num header items
        dimnames = list(
            attributes(data)$names, # variable names from data
            var_header_data_lang(data, languages) # header items
            )
        )
    return(var_mat)
}
#'  making column "variable" from "name" attribute
#' @noRd
var_variable_column_lang <- function(data) {
    column <- NULL
    var_id <- 1
    # - iterate through all variables
    for (var in attributes(data)$names) {
        # -- if variable has "name" attribute
        if ("name" %in% names(attributes(data[[var]]))) {
            # --- if "name" attribute is empty
            if (attributes(data[[var]])$name == "") {
                column <- append(
                    column, as.character(paste0("var_id_",var_id)))
                message(paste0(
                    "Attention! Variable name is missing. var_id_",var_id,
                    " is placeholder."))
            } else { # if "name" attribute is not empty
                column <- append(
                    column, attributes(data[[var]])$name)
            }
        } else { # if variable has no "name" attribute
            column <- append(
                column, var)
        } #  end if variable has no "name" attribute
            var_id <- var_id + 1
        } #  end iteration through variables
    return(column)
}
#' making variables data frame
#' @noRd
var_df_lang <- function(data, languages) {
    # - make data frame from matrix
    var_df <- data.frame(var_matrix_lang(data, languages), row.names = NULL)
    # - if column "variable" exists
    if ("variable" %in% var_header_data_lang(data, languages) == TRUE) {
        var_df[["variable"]] <- var_variable_column_lang(data)
    }
    # - iterate through header to fill each column with metadata
    for (item in var_header_data_lang(data, languages)) {
        if (item != "variable") {
            column <- NULL
            for (var in attributes(data)$names) {
                # -- if variable has attribute
                if (item %in% names(attributes(data[[var]]))) {
                    # --- if attribute is empty
                    if (as.character(attributes(data[[var]])[[item]])[1] == "") {
                        column <- append(column, "")
                    } else { # if attribute is not empty
                        column <- append(
                          column,
                          as.character(attributes(data[[var]])[[item]])[1]
                          )
                    }
                } else { # if variable has no attribute
                    column <- append(column, "")
                } #  end if variable has no attribute
            } #  end iteration through variables
            var_df[[item]] <- column
        }
    }
    return(var_df)
}
# --------------------
# variables.csv with VARIABLE INPUT
# --------------------
#' variables header
#' @noRd
var_header_var_lang <- function(variable, languages) {
    var_header <- c()
    # - get variable attributes from selected variable
    var_header <- append(var_header, names(attributes(variable)))
    # - change name to variable
    if ("name" %in% var_header) {
        var_header <- gsub("name", "variable", var_header)
    } else {
        var_header <- append("variable", var_header)
        message("Attention! Variable name is missing.")
    }
    # - drop labels attributes (categories.csv) and
    # - not selected language attributes
    var_header_reduced <- c()
    for (item in var_header) {
      if (startsWith(item, "labels") == FALSE &&
        item != "label" &&
        item != "description" &&
        startsWith(item, "label_") == FALSE &&
        startsWith(item, "description_") == FALSE) {
          var_header_reduced <- append(var_header_reduced, item)
          }
      if (item == paste0("label_", languages)) {
        var_header_reduced <- append(var_header_reduced, item)
        }
      if (item == paste0("description_", languages)) {
        var_header_reduced <- append(var_header_reduced, item)
        }
      }
    return(var_header_reduced)
}
#' variables matrix
#' @noRd
var_matrix_var_lang <- function(variable, languages) {
    # - make matrix
    var_mat <- matrix(
        nrow = 1, # num variables
        ncol = length(var_header_var_lang(variable, languages)), # num header items
        dimnames = list(
            attributes(variable)$name, # variable name
            var_header_var_lang(variable, languages) # header items
        )
    )
    return(var_mat)
}
#' making column "variable" from "name" attribute (get metadata)
#' @noRd
var_variable_column_var_lang <- function(variable) {
    column <- NULL
    # - if variable has "name" attribute
    if ("name" %in% names(attributes(variable))) {
        # -- if "name" attribute is empty
        if (attributes(variable)$name == "") {
                column <- as.character("var_id")
                message(
                    "Attention! Variable name is missing. var_id
                    is placeholder.")
        } else { # if "name" attribute is not empty
                column <- attributes(variable)$name
            }
        } else { # if variable has no "name" attribute
            column <- as.character("var_id")
            message(
                "Attention! Variable name is missing. var_id is placeholder.")
            } #  end if variable has no "name" attribute

    return(column)
}
#' making variables data frame
#' @noRd
var_df_var_lang <- function(variable, languages) {
    # - make data frame from matrix
    var_df <- data.frame(var_matrix_var_lang(variable, languages), row.names = NULL)
    # - if column "variable exists
    if ("variable" %in% var_header_var_lang(variable, languages) == TRUE) {
        var_df[["variable"]] <- var_variable_column_var_lang(variable)
    }
    # - iterate through header to fill each column with metadata
    for (item in var_header_var_lang(variable, languages)) {
        if (item != "variable") {
            column <- NULL
            # -- if variable has attribute
            if (item %in% names(attributes(variable))) {
                # --- if attribute is empty
                if (attributes(variable)[[item]][1] == "") {
                    column <- append(column, "")
                } else { # if attribute is not empty
                    column <- append(column, attributes(variable)[[item]])
                }
            } else { # if variable has no attribute
                column <- append(column, "")
            } #  end if variable has no attribute
        var_df[[item]] <- column
    }
}
    return(var_df)
}
# --------------------
# categories.csv with DATA FRAME INPUT
# --------------------
#'  -- categories header
#' @noRd
cat_header_data_lang <- function(data, languages) {
    cat_header <- NULL
    # - get all attributes from all variables in data set
    for (var in attributes(data)$names) {
        cat_header <- append(cat_header,
                             names(attributes(data[[var]])))
    }
    # - reduce to unique attributes
    cat_header <- unique(cat_header)
    # - append mandatory attributes for categorical variables
    cat_header_reduced <- c("variable", "value")
    for (item in cat_header) {
        if (item == paste0("labels_", languages)) {
            cat_header_reduced <- append(cat_header_reduced, item)
        }
    }
    return(cat_header_reduced)
}
#' making column "variable"
#' @noRd
cat_variable_column_data_lang <- function(data, languages) {
    var_names <- NULL
    var_id <- 1
    for (var in attributes(data)$names) {
        # - is variable categorical?
        if (paste0("labels_", languages) %in% names(attributes(data[[var]]))) {
            # -- no variable name > generate placeholder
            if (is.null(attributes(data[[var]])$name) || attributes(data[[var]])$name == "") {
                var_names <- append(
                    var_names,
                    rep(var,
                        length(attributes(data[[var]])$labels)))
                    #
                    # rep(paste0("var_id_", var_id),
                    #     length(attributes(data[[var]])[[paste0("labels_", languages)]])))
            } else { # variable name is available
                var_names <- append(
                    var_names,
                    rep(attributes(data[[var]])$name,
                        length(attributes(data[[var]])[[paste0("labels_", languages)]])))
            }
        }
        var_id <- var_id + 1
    }
    return(var_names)
}
#' categories matrix
#' @noRd
cat_matrix_data_lang <- function(data, languages) {
    # - get number of categories over all variables of data set
    nrow = 0
    for (var in attributes(data)$names) {
        # -- check if variable is categorical
        if (paste0("labels_", languages) %in% names(attributes(data[[var]]))) {
            nrow <- nrow + as.integer(
              length(attributes(data[[var]])[[paste0("labels_", languages)]]))
        }
    }
    # - make matrix
    cat_mat <- matrix(
        nrow = nrow, # num of catgeories for all variables
        ncol = length(cat_header_data_lang(data, languages)), # num header items
        dimnames = list(
            cat_variable_column_data_lang(data, languages), # variable name
            cat_header_data_lang(data, languages) # header items
        )
    )
    return(cat_mat)
}
#' making column "value"
#' @noRd
cat_values_column_data_lang <- function(data, languages) {
  values <- NULL
  for (var in attributes(data)$names) {
    # - is variable categorical?
    if (paste0("labels_", languages) %in% names(attributes(data[[var]])) == TRUE) {
      # -- is labels attribute empty?
      if (is.null(names(attributes(data[[var]])[[
        paste0("labels_", languages)
      ]])) == TRUE) {
        values <- append(values, "")
      } else {
        values <- append(values, as.character(unname(
          attributes(data[[var]])[[paste0("labels_", languages)]]))
        )
      }
    }
  }
  return(as.character(values))
}
#' making columns "labels"
#' @noRd
cat_labels_column_data_lang <- function(data, languages) {
  labels <- NULL
  for (var in attributes(data)$names) {
    # - is variable categorical? Does labels attribute exist?
    if (paste0("labels_", languages) %in% names(attributes(data[[var]])) == TRUE) {
      # -- is labels attribute empty?
      if (is.null(names(attributes(data[[var]])[[
        paste0("labels_", languages)
        ]])) == TRUE) {
        labels <- append(labels, "")
      } else {
        labels <- append(labels, names(attributes(data[[var]])[[
          paste0("labels_", languages)
          ]]))
      }
    }
  }
  return(labels)
}
#' making categories data frame
#' @noRd
cat_df_data_lang <- function(data, languages) {
  # - make data frame from matrix
  cat_df <- data.frame(cat_matrix_data_lang(data, languages), row.names = NULL)
  # - append columns
  for (item in cat_header_data_lang(data, languages)) {
    # -- column "variable"
    if (item == "variable") {
      cat_df[["variable"]] <- cat_variable_column_data_lang(data, languages)
    }
    # -- column "value
    if (item == "value") {
      cat_df[["value"]] <- cat_values_column_data_lang(data, languages)
    }
    # -- columns "labels", "labels_de", ...
    if (startsWith(item, "labels") == TRUE) {
      cat_df[[paste0("labels_", languages)]] <- cat_labels_column_data_lang(data, languages)
    }
  }
  return(cat_df)
}
# --------------------
# categories.csv with VARIABLE INPUT
# --------------------
#' categories header
#' @noRd
cat_header_var_lang <- function(variable, languages) {
  cat_header <- NULL
  # - get all attributes from selected variables
  cat_header <- append(cat_header, names(attributes(variable)))
  # - append mandatory attributes for categorical variable
  cat_header_reduced <- c("variable", "value")
  for (item in cat_header) {
    if (item == paste0("labels_", languages)) {
      cat_header_reduced <- append(cat_header_reduced, item)
    }
  }
  return(cat_header_reduced)
}
#' making column "variable"
#' @noRd
cat_variable_column_var_lang <- function(variable, languages) {
  var_names <- NULL
  # - is variable categorical?
  if (paste0("labels_", languages) %in% names(attributes(variable))) {
    # -- no variable name > generate placeholder
    if (is.null(attributes(variable)$name) || attributes(variable)$name == "") {
      var_names <- append(
        var_names,
        rep("var_id",
        length(attributes(variable)[[paste0("labels_", languages)]])))
      } else { # variable name is available
        var_names <- append(
          var_names,
          rep(attributes(variable)$name,
              length(attributes(variable)[[paste0("labels_", languages)]])))
    }
  }
  return(var_names)
}
#' categories matrix
#' @noRd
cat_matrix_var_lang <- function(variable, languages) {
  # - get number of categories from variable
  nrow = 0
  # - check if variable is categorical
  if (paste0("labels_", languages) %in% names(attributes(variable))) {
    nrow <- nrow + as.integer(length(attributes(variable)[[
      paste0("labels_", languages)]]))
    # -- make matrix
    cat_mat <- matrix(
      nrow = nrow, # num of categories for variable
      ncol = length(cat_header_var_lang(variable, languages)), # num header items
      dimnames = list(
        cat_variable_column_var_lang(variable, languages), # variable name
        cat_header_var_lang(variable, languages) # header items
      )
    )
    return(cat_mat)
    } else {
      message("Your selected variable is not categorical.
              Variable labels and values are not available.")
    }
}
#' making column "value"
#' @noRd
cat_values_column_var_lang <- function(variable, languages) {
  values <- NULL
  # - is variable categorical?
  if (paste0("labels_", languages) %in% names(attributes(variable))) {
    values <- append(values, as.character(unname(attributes(variable)[[
      paste0("labels_", languages)]])))
    return(as.character(values))
  }
}
#' making columns "labels"
#' @noRd
cat_labels_column_var_lang <- function(variable, languages) {
  labels <- NULL
  # - is variable categorical? Does labels attribute exist?
  if (paste0("labels_", languages) %in% names(attributes(variable)) == TRUE) {
    # -- is labels attribute empty?
    if (is.null(names(attributes(variable)[[
      paste0("labels_", languages)
      ]])) == TRUE) {
      labels <- append(labels, "")
    } else {
      labels <- append(labels, names(attributes(variable)[[
        paste0("labels_", languages)
      ]]))
    }
    return(labels)
  }
}
#' making categories data frame
#' @noRd
cat_df_var_lang <- function(variable, languages) {
  # - make data frame from matrix
  cat_df <- data.frame(cat_matrix_var_lang(variable, languages),
                       row.names = NULL)
  # - append columns
  for (item in cat_header_var_lang(variable, languages)) {
    # -- column "variable"
    if (item == "variable") {
      cat_df[["variable"]] <- cat_variable_column_var_lang(variable, languages)
    }
    # -- column "value
    if (item == "value") {
      cat_df[["value"]] <- cat_values_column_var_lang(variable, languages)
    }
    # -- columns "labels", "labels_de", ...
    if (item == paste0("labels_", languages)) {
      cat_df[[item]] <- cat_labels_column_var_lang(variable, languages)
    }
  }
  return(cat_df)
}
# --------------------
# MAKE CSV FILES
# --------------------
#' @noRd
get_csv_lang <- function(input, output, variables, languages) {
  # - data frame input
  if ("data.frame" %in% class(input) == TRUE) {
    if (variables == "no") {
      write_odf_csv(
        get_dataset_lang(input, languages),
        "/dataset.csv",
        output)
    }
    if (variables == "yes") {
      write_odf_csv(
        get_dataset_lang(input, languages),
        "/dataset.csv",
        output)
      write_odf_csv(
        var_df_lang(input, languages),
        "/variables.csv",
        output)
       write_odf_csv(
         cat_df_data_lang(input, languages),
         "/categories.csv",
         output)
      }
  }
  # - variable input
  else {
    write_odf_csv(
      var_df_var_lang(input, languages),
      "/variables.csv",
      output)
    if (length(cat_df_var_default(input)) > 0) {
       write_odf_csv(
         cat_df_var_lang(input, languages),
         "/categories.csv",
         output)
    }
  }
}
