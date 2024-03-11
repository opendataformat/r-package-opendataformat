#' @title RData to CSV for all languages
#'
#' @description Functions that are used to convert an R data frame
#' to the internal Open Data Format (CSV files) if "all" languages are selected.
#'
#' @import utils
#'
#' @param input R data frame.
#'
#' @param output Path to directory for saving the CSV files.
#'
#' @param languages The language option is set to "all".
#'
#' @param variables Choose "yes" if all data set variables should be
#' converted or "no" if not.
#'
#' @param export_data Choose "yes" if the data set CSV should be copied
#' to the output directory or "no" if not.
#'
#' @return CSV files holding the data and the metadata for all languages.
#'
# --------------------
# dataset.csv with DATA FRAME INPUT
# --------------------
#' @noRd
get_dataset_all <- function(data) {
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
        label_list <- c()
        for (attr in names(attributes(data))) {
            if (startsWith(attr, "label") == TRUE) {
                label_list <- append(label_list, attr)
            }
        }
        header <- append(header, label_list)
        for (lab in label_list) {
            metadata <- append(metadata, attributes(data)[lab])
        }
        # - data set description
        descr <- c()
        for (attr in names(attributes(data))) {
            if (startsWith(attr, "description") == TRUE) {
                descr <- append(descr, attr)
            }
        }
        header <- append(header, descr)
        for (des in descr) {
            metadata <- append(metadata, attributes(data)[des])
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
var_header_data <- function(data) {
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
    # - drop labels attributes (categories.csv)
    var_header_reduced <- c()
    for (item in var_header) {
        if (startsWith(item, "labels") == FALSE) {
            var_header_reduced <- append(var_header_reduced, item)
        }
    }
    #remove language attributes
    if ("languages" %in% var_header_reduced  | "lang" %in% var_header_reduced){
      var_header_reduced<-var_header_reduced[-which(var_header_reduced %in% c("languages", "lang"))]
    }
    return(var_header_reduced)
}
#' variables matrix
#' @noRd
var_matrix <- function(data) {
    # - make matrix
    var_mat <- matrix(
        nrow = length(attributes(data)$names), # num variables
        ncol = length(var_header_data(data)), # num header items
        dimnames = list(
            attributes(data)$names, # variable names from data
            var_header_data(data) # header items
            )
        )
    return(var_mat)
}
#'  making column "variable" from "name" attribute
#' @noRd
var_variable_column <- function(data) {
    column <- NULL
    var_id <- 1
    # - iterate through all variables
    for (var in attributes(data)$names) {
        # - if variable has "name" attribute
        if ("name" %in% names(attributes(data[[var]]))) {
            # - if "name" attribute is empty
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
            column <- append(column, var)
        } # end if variable has no "name" attribute
            var_id <- var_id + 1
        } # end iteration through variables
    return(column)
}
#' making variables dataframe
#' @noRd
var_df <- function(data) {
    # - make data frame from matrix
    var_df <- data.frame(var_matrix(data), row.names = NULL)
    # - if column "variable" exists
    if ("variable" %in% var_header_data(data) == TRUE) {
        var_df[["variable"]] <- var_variable_column(data)
    }
    # - iterate through header to fill each column with metadata
    for (item in var_header_data(data)) {
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
var_header_var <- function(variable) {
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
    # - drop labels attributes (categories.csv)
    var_header_reduced <- c()
    for (item in var_header) {
        if (startsWith(item, "labels") == FALSE) {
            var_header_reduced <- append(var_header_reduced, item)
        }
    }
    return(var_header_reduced)
}
#' variables matrix
#' @noRd
var_matrix_var <- function(variable) {
    # - make matrix
    var_mat <- matrix(
        nrow = 1, # num variables
        ncol = length(var_header_var(variable)), # num header items
        dimnames = list(
            attributes(variable)$name, # variable name
            var_header_var(variable) # header items
        )
    )
    return(var_mat)
}
#' making column "variable" from "name" attribute (get metadata)
#' @noRd
var_variable_column_var <- function(variable) {
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
var_df_var <- function(variable) {
    # - make data frame from matrix
    var_df <- data.frame(var_matrix_var(variable), row.names = NULL)
    # - if column "variable exists
    if ("variable" %in% var_header_var(variable) == TRUE) {
        var_df[["variable"]] <- var_variable_column_var(variable)
    }
    # - iterate through header to fill each column with metadata
    for (item in var_header_var(variable)) {
        if (item != "variable") {
            column <- NULL
            # -- if variable has attribute
            if (item %in% names(attributes(variable))) {
                # --- if attribute is empty
                if (attributes(variable)[[item]][1] == "") {
                    column <- append(column, "")
                } else { # if attribute is not empty
                    column <- append(column, attributes(variable)[[item]][1])
                }
            } else { # if variable has no attribute
                column <- append(column, "")
            } #  end if variable has no attribute
        var_df[[item]] <- column
        }
    }
    #remove lang and languages attributes
    if ("languages" %in% names(var_df) | "lang" %in% names(var_df)){
      var_df<-var_df[-which(names(var_df) %in% c("languages", "lang"))]
    }
    return(var_df)
}
# --------------------
# categories.csv with DATA FRAME INPUT
# --------------------
#' categories header
#' @noRd
cat_header_data <- function(data) {
    cat_header <- NULL
    # - get all attributes from all variables in dataset
    for (var in attributes(data)$names) {
        cat_header <- append(cat_header,
                             names(attributes(data[[var]])))
    }
    # - reduce to unique attributes
    cat_header <- unique(cat_header)
    # - append mandatory attributes for categorical variables
    cat_header_reduced <- c("variable", "value")
    for (item in cat_header) {
        if (startsWith(item, "labels") == TRUE) {
            cat_header_reduced <- append(cat_header_reduced, item)
        }
    }
    return(cat_header_reduced)
}
#' making column "variable"
#' @noRd
cat_variable_column_data <- function(data) {
    var_names <- NULL
    var_id <- 1
    #retrieve active language
    lang=attr(data, "lang")
    if (lang=="default") lang="" else lang=paste0("_", lang)
    for (var in attributes(data)$names) {
        # - is variable categorical?
        if (paste0("labels", lang) %in% names(attributes(data[[var]])) | "labels" %in% names(attributes(data[[var]]))) {
            # - no variable name > generate placeholder
            if (is.null(attributes(data[[var]])$name) || attributes(data[[var]])$name == "") {
                var_names <- append(
                    var_names,
                    rep(var,
                        length(attributes(data[[var]])[paste0("labels", lang)])))
                    # rep(paste0("var_id_", var_id),
                    #     length(attributes(data[[var]])$labels)))
            } else { # variable name is available
                var_names <- append(
                    var_names,
                    rep(attributes(data[[var]])$name,
                        length(attributes(data[[var]])[[paste0("labels", lang)]])))
            }
        }
        var_id <- var_id + 1
    }
    return(var_names)
}
#' categories matrix
#' @noRd
cat_matrix_data <- function(data) {
    # - get number of categories over all variables of data set
    nrow = 0
    lang=attr(data, "lang")
    if (lang=="default") lang="" else lang=paste0("_", lang)
    for (var in attributes(data)$names) {
        # -- check if variable is categorical
        if (paste0("labels", lang) %in% names(attributes(data[[var]])) | "labels" %in% names(attributes(data[[var]]))) {
            nrow <- nrow + as.integer(length(attributes(data[[var]])[[paste0("labels", lang)]]))
        }
    }
    # - make matrix
    cat_mat <- matrix(
        nrow = nrow, # num of catgeories for all variables
        ncol = length(cat_header_data(data)), # num header items
        dimnames = list(
            cat_variable_column_data(data), # variable name
            cat_header_data(data) # header items
        )
    )
    return(cat_mat)
}
#' making column "value"
#' @noRd
cat_values_column_data <- function(data) {
  values <- NULL
  lang=attr(data, "lang")
  if (lang=="default") lang="" else lang=paste0("_", lang)
  for (var in attributes(data)$names) {
    # - is variable categorical?
    if (paste0("labels", lang) %in% names(attributes(data[[var]]))) {
      values <- append(values, as.character(unname(attributes(data[[var]])[[paste0("labels", lang)]])))
    }
  }
  return(as.character(values))
}
#' making columns "labels"
#' @noRd
cat_labels_column_data <- function(data, item) {
  labels <- NULL
  for (var in attributes(data)$names) {
    # - is variable categorical? Does labels attribute exist?
    lang=attr(data, "lang")
    if (lang=="default") lang="" else lang=paste0("_", lang)
    if (paste0("labels", lang) %in% names(attributes(data[[var]])) == TRUE) {
      # - is labels attribute empty?
      if (is.null(names(attributes(data[[var]])[[paste0("labels", lang)]])) == TRUE) {
        labels <- append(labels, "")
      } else {
        labels <- append(labels, names(attributes(data[[var]])[[item]]))
      }
    }
  }
  return(labels)
}
#' making categories data frame
#' @noRd
cat_df_data <- function(data) {
  # - make data frame from matrix
  cat_df <- data.frame(cat_matrix_data(data), row.names = NULL)
  # - append columns
  for (item in cat_header_data(data)) {
    # -- column "variable"
    if (item == "variable") {
      cat_df[["variable"]] <- cat_variable_column_data(data)
    }
    # -- column "value
    if (item == "value") {
      cat_df[["value"]] <- cat_values_column_data(data)
    }
    # -- columns "labels", "labels_de", ...
    if (startsWith(item, "labels") == TRUE) {
      cat_df[[item]] <- cat_labels_column_data(data, item)
    }
  }
  return(cat_df)
}
# --------------------
# categories.csv with VARIABLE INPUT
# --------------------
#' categories header
#' @noRd
cat_header_var <- function(variable) {
  cat_header <- NULL
  # - get all attributes from selected variables
  cat_header <- append(cat_header, names(attributes(variable)))
  # - append mandatory attributes for categorical variable
  cat_header_reduced <- c("variable", "value")
  for (item in cat_header) {
    if (startsWith(item, "labels") == TRUE) {
      cat_header_reduced <- append(cat_header_reduced, item)
    }
  }
  return(cat_header_reduced)
}
#' making column "variable"
#' @noRd
cat_variable_column_var <- function(variable) {
  var_names <- NULL
  lang=attr(data, "lang")
  if (lang=="default") lang="" else lang=paste0("_", lang)
  # - is variable categorical?
  if (paste0("labels", lang) %in% names(attributes(variable))) {
    # -- no variable name > generate placeholder
    if (is.null(attributes(variable)$name) || attributes(variable)$name == "") {
      var_names <- append(
        var_names,
        rep("var_id",
        length(attributes(variable)[[paste0("labels", lang)]])))
      } else { # variable name is available
        var_names <- append(
          var_names,
          rep(attributes(variable)$name,
              length(attributes(variable)[[paste0("labels", lang)]])))
    }
  }
  return(var_names)
}
#' categories matrix
#' @noRd
cat_matrix_var <- function(variable) {
  # - get number of categories from variable
  nrow = 0
  lang=attr(data, "lang")
  if (lang=="default") lang="" else lang=paste0("_", lang)
  # - check if variable is categorical
  if (paste0("labels", lang) %in% names(attributes(variable))) {
    nrow <- nrow + as.integer(length(attributes(variable)[[paste0("labels", lang)]]))
    # -- make matrix
    cat_mat <- matrix(
      nrow = nrow, # num of catgeories for variable
      ncol = length(cat_header_var(variable)), # num header items
      dimnames = list(
        cat_variable_column_var(variable), # variable name
        cat_header_var(variable) # header items
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
cat_values_column_var <- function(variable) {
  values <- NULL
  lang=attr(data, "lang")
  if (lang=="default") lang="" else lang=paste0("_", lang)
  # - is variable categorical?
  if (paste0("labels", lang) %in% names(attributes(variable))) {
    values <- append(values, as.character(unname(attributes(variable)[[paste0("labels", lang)]])))
    return(as.character(values))
  }
}
#' making columns "labels"
#' @noRd
cat_labels_column_var <- function(variable, item) {
  labels <- NULL
  lang=attr(data, "lang")
  if (item=="labels") lang="" else lang=paste0("_",lang)
  # - is variable categorical? Does labels attribute exist?
  if (paste0("labels",lang) %in% names(attributes(variable)) == TRUE) {
    # -- is labels attribute empty?
    if (is.null(names(attributes(variable)[[paste0("labels",lang)]])) == TRUE) {
      labels <- append(labels, "")
    } else {
      labels <- append(labels, names(attributes(variable)[[item]]))
    }
    return(labels)
  }
}
#' making categories data frame
#' @noRd
cat_df_var <- function(variable) {
  # - make data frame from matrix
  cat_df <- data.frame(cat_matrix_var(variable), row.names = NULL)
  # - append columns
  for (item in cat_header_var(variable)) {
    # -- column "variable"
    if (item == "variable") {
      cat_df[["variable"]] <- cat_variable_column_var(variable)
    }
    # -- column "value
    if (item == "value") {
      cat_df[["value"]] <- cat_values_column_var(variable)
    }
    # -- columns "labels", "labels_de", ...
    if (startsWith(item, "labels") == TRUE) {
      cat_df[[item]] <- cat_labels_column_var(variable, item)
    }
  }
  return(cat_df)
}
# --------------------
# MAKE CSV FILES
# --------------------
#' @noRd
get_csv_all <- function(input, output, variables) {
  # - data frame input
  if ("data.frame" %in% class(input) == TRUE) {
    if (variables == "no") {
      write_odf_csv(
        get_dataset_all(input),
        "/dataset.csv",
        output)
    }
    if (variables == "yes") {
      write_odf_csv(
        get_dataset_all(input),
        "/dataset.csv",
        output)
      write_odf_csv(
        var_df(input),
        "/variables.csv",
        output)
      write_odf_csv(
        cat_df_data(input),
        "/categories.csv",
        output)
      }
  }
  # - variable input
  else {
    write_odf_csv(
      var_df_var(input),
      "/variables.csv",
      output)
    if (length(cat_df_var(input)) > 0 ) {
      write_odf_csv(
        cat_df_var(input),
        "/categories.csv",
        output)
    }
  }
}
