#' get_dataset_default: data set input - make data set.csv header and metadata
test_that("get_dataset_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test column 'dataset'
  expect_equal(get_dataset_default(df)[["dataset"]], "bap")
  # --- test column 'label'
  expect_equal(get_dataset_default(df)[["label"]], "Data from individual questionnaires 2010")
  # --- test_column 'description'
  expect_equal(get_dataset_default(df)[["description"]], "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all.\" This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  # --- test_column 'url'
  expect_equal(get_dataset_default(df)[["url"]], "https://paneldata.org/soep-core/data/bap")
  # manipulate data: empty label
  attributes(df)$label <- ""
  # --- test column 'label'
  expect_equal(get_dataset_default(df)[["label"]], "")
  # manipulate data: no label
  attributes(df)$label <- NULL
  # --- test column 'label'
  expect_equal(get_dataset_default(df)[["label"]], NULL)
  # print(get_dataset_default(df))
})
#' var_header_data_default: dataset input - make variables.csv header as list of
#' unique attributes from all variables in dataset without attributes
#' that include language code
test_that("var_header_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_header_data_default(df), c(
    "variable",
    "label",
    "description",
    "type",
    "url")
    )
  # -- manipulate data: no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(sort(var_header_data_default(df)), sort(c(
    "variable",
    "label",
    "description",
    "type",
    "url"))
  )
  # -- manipulate data: no url
  attributes(df$bap87)$url <- NULL
  # --- test
  expect_equal(sort(var_header_data_default(df)), sort(c(
    "variable",
    "label",
    "description",
    "type",
    "url"))
  )
})
#' var_matrix_default: datset input - number of rows and number of columns
test_that("var_matrix_default", {
  # get data
  df <- get(load("testdata/data_odf.RData"))
  # test number of columns
  expect_equal(ncol(var_matrix_default(df)), 5)
  # test number of rows
  expect_equal(nrow(var_matrix_default(df)), 7)
})
#' var_variable_column_default: dataset input - get metadata
#' for the column 'variable' from the attribute 'name'
test_that("var_variable_column_default", {
  # all variable have "name" attribbute and "name" atribute is not empty
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  #  -- test
  expect_equal(var_variable_column_default(df),
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # one variable has no "name" attribute and anothe has an empty name attribute
  # -- manipulate data
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # -- test
  expect_equal(var_variable_column_default(df_no_name),
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
#' var_df_default: dataset input - variable name in variables data frame
test_that("var_df_default_name", {
  # -- get data: all variables have variable names
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_default(df)[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # -- manipulate data: no variable has variable name
  df_no_name <- df
  for (var in attributes(df_no_name)$names) {
    attributes(df_no_name[[var]])$name <- NULL
  }
  # --- test
  expect_equal(var_df_default(df_no_name)[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # -- manipulate data: one variable has no name, another has an empty name
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # --- test
  expect_equal(var_df_default(df_no_name)[["variable"]],
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
#' var_df_default: dataset input - variable label in variables data frame
test_that("var_df_default_label", {
  # -- get data: all variables have a label
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_default(df)[["label"]],
               c("Current Health",
                 "hours of sleep, normal workday",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height",
                 "Firstname"))
  # -- manipulate data: one variable has no label, another has an empty label
  df_no_label <- df
  attributes(df_no_label$bap87)$label <- NULL
  attributes(df_no_label$bap9201)$label <- ""
  # --- test
  expect_equal(var_df_default(df_no_label)[["label"]],
               c("",
                 "",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height",
                 "Firstname"))
  # -- manipulate data: no variable has a label
  df_no_label <- df
  for (var in attributes(df_no_label)$names) {
    attributes(df_no_label[[var]])$label <- NULL
  }
  # -- test
  expect_equal(colnames(var_df_default(df_no_label)),
               c("variable",
                 "description",
                 "type",
                 "url" )
               )

})
#' var_header_var_default: variable input - get header for variables.csv
test_that("var_header_var_var_default", {
  # -- get data: variable has all attributes
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_header_var_default(df$bap87),
               c("variable",
                 "label",
                 "description",
                 "type",
                 "url" )
               )
  # -- manipulate data: variable has no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(var_header_var_default(df$bap87),
               c("variable",
                 "label",
                 "description",
                 "type",
                 "url" )
               )
  # -- manipulate data: variable has no label
  attributes(df$bap87)$label <- NULL
  # -- test
  expect_equal(var_header_var_default(df$bap87),
               c("variable",
                 "description",
                 "type",
                 "url" )
               )

})
#' var_matrix_var_default: variable input - matrix for variables data frame
test_that("var_matrix_var_default", {
  # -- get data: variable has all attributes
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(ncol(var_matrix_var_default(df$bap87)), 5)
  expect_equal(nrow(var_matrix_var_default(df$bap87)), 1)
  # -- manipulate data: variable has no name, label and url
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$label <- NULL
  attributes(df$bap87)$url <- NULL
  # --- test
  expect_equal(ncol(var_matrix_var_default(df$bap87)), 3)
  expect_equal(nrow(var_matrix_var_default(df$bap87)), 1)
})
#' var_variable_column_var_default: variable input - get metadata
#' for 'variable' column
test_that("var_variable_column_var_default", {
  # -- get data: variable has 'name' attribute
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_variable_column_var_default(df$bap87), "bap87")
  # -- manipulate data: variable 'name' attribute exists but is empty
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(var_variable_column_var_default(df$bap87), "var_id")
  # -- manipulate data: variable has no 'name' attribute
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(var_variable_column_var_default(df$bap87), "var_id")
})
#' var_df_var_default: variable input - making variables data frame
test_that("var_df_var_default", {
  # -- get data: variable has all attribute
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_var_default(df$bap87)[["variable"]],
               "bap87")
  expect_equal(var_df_var_default(df$bap87)[["label"]],
               "Current Health")
  expect_equal(var_df_var_default(df$bap87)[["label_en"]],
               NULL)
  expect_equal(var_df_var_default(df$bap87)[["description"]],
  "Question: How would you describe your current health?")
  expect_equal(var_df_var_default(df$bap87)[["description_de"]],
               NULL)
  expect_equal(var_df_var_default(df$bap87)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap87")
  expect_equal(var_df_var_default(df$bap87)[["type"]],
               "numeric")
  # -- manipulate data: variable has no variable name,
  # no label and an empty description
  attributes(df$bap9002)$name <- NULL
  attributes(df$bap9002)$label <- NULL
  attributes(df$bap9002)$description <- ""
  # --- test
  expect_equal(var_df_var_default(df$bap9002)[["variable"]],
               "var_id")
  expect_equal(var_df_var_default(df$bap9002)[["label"]],
               NULL)
  expect_equal(var_df_var_default(df$bap9002)[["description"]],
               "")
  expect_equal(var_df_var_default(df$bap9002)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap9002")
  expect_equal(var_df_var_default(df$bap9002)[["type"]],
               "numeric")
})
#' cat_header_var_default: data set input - get column header
#' for categories.csv
test_that("cat_header_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_header_data_default(df), c("variable", "value", "labels"))
})
#' cat_variable_column_data_default: data set input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_variable_column_data_default(df), c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
    )
  # -- manipulate data: no variable name
  df_no_name <- df
  attributes(df_no_name$bap9001)$name <- ""
  attributes(df_no_name$bap96)$name <- NULL
  expect_equal(cat_variable_column_data_default(df_no_name), c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" ))
  # -- manipulate data: empty labels
  df_empty_labels <- df
  attributes(df_empty_labels$bap9001)$labels <- ""
  expect_equal(length(cat_variable_column_data_default(df_empty_labels)), 28)
  # -- manipulate data: no labels
  df_no_labels <- df
  attributes(df_no_labels$bap9001)$labels <- NULL
  expect_equal(length(cat_variable_column_data_default(df_no_labels)), 27)
})
#' cat_matrix_data_default: data set input - matrix for categories.csv
test_that("cat_matrix_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_data_default(df)), 34) # nrow
  expect_equal(ncol(cat_matrix_data_default(df)), 3) # ncol
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels <- NULL
  # --- test
  expect_equal(nrow(cat_matrix_data_default(df)), 32) # nrow
  expect_equal(ncol(cat_matrix_data_default(df)), 3) # ncol
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels <- ""
  # --- test
  expect_equal(nrow(cat_matrix_data_default(df)), 26) # nrow
  expect_equal(ncol(cat_matrix_data_default(df)), 3) # ncol
})
#' cat_values_column_data_default: data set input - get column 'values'
#' for categories.csv
test_that("cat_values_column_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_data_default(df), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1", "1",  "2",
    "3",  "4",  "5",  "-2", "-1", "1",  "2",  "3",  "4",  "5", "-2", "-1", "1",
    "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels <- NULL
  # --- test
  expect_equal(length(cat_values_column_data_default(df)), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels <- ""
  # --- test
  expect_equal(length(cat_values_column_data_default(df)), 26)
})
#' cat_labels_column_data_default: data set input - get column 'labels'
#' for categories.csv
test_that("cat_labels_column_data_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_labels_column_data_default(df, "labels"), c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels <- NULL
  # --- test
  expect_equal(length(cat_labels_column_data_default(df, "labels")), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels <- ""
  # --- test
  expect_equal(length(cat_labels_column_data_default(df, "labels")), 26)
})
#' cat_df_data_default: data set input - make categories data frame
test_that("cat_df_data_default",{
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_data_default(df)[["labels_de"]], NULL)
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  expect_equal(cat_df_data_default(df)[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_default(df)[["labels"]], c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
  # -- manipulate data: empty labels and empty variable name
  attributes(df$bap87)$labels <- ""
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_df_data_default(df)[["labels"]], c(
    "", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
  expect_equal(cat_df_data_default(df)[["variable"]], c(
    "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  # -- manipulate data: no variable name
  df <- get(load("testdata/data_odf.RData"))
  attributes(df$bap87)$name <- NULL
  expect_equal(cat_df_data_default(df)[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_default(df)[["labels"]], c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
  # -- manipulate data: no variable name and no labels
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$labels <- NULL
  expect_equal(cat_df_data_default(df)[["variable"]], c(
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_default(df)[["labels"]], c(
    "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
})
#' cat_header_var_default: variable input - get column header for categories.csv
test_that("cat_header_var_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_header_var_default(df$bap87), c(
    "variable", "value", "labels")
  )
  # --- manipulate data: empty variable names
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_header_var_default(df$bap87), c(
    "variable", "value", "labels")
  )
  # --- manipulate data: no variable names
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_header_var_default(df$bap87), c(
    "variable", "value", "labels")
  )
  # --- manipulate data: empty variable labels
  attributes(df$bap87)$labels <- ""
  # --- test
  expect_equal(cat_header_var_default(df$bap87), c(
    "variable", "value", "labels")
  )
  # --- manipulate data: no variable labels
  attributes(df$bap87)$labels <- NULL
  # --- test
  expect_equal(cat_header_var_default(df$bap87), c(
    "variable", "value")
  )
})
#' cat_variable_column_var_default: variable input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_var_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_variable_column_var_default(df$bap9201), c(
    "bap9201", "bap9201")
  )
  # --- manipulate data: empty variable name
  attributes(df$bap9201)$name <- ""
  # --- test
  expect_equal(cat_variable_column_var_default(df$bap9201), c(
    "var_id", "var_id")
  )
  # --- manipulate data: no variable name
  attributes(df$bap9201)$name <- NULL
  # --- test
  expect_equal(cat_variable_column_var_default(df$bap9201), c(
    "var_id", "var_id")
  )
})
#' cat_matrix_var_default: variable input - matrix for categories.csv
test_that("cat_matrix_var_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_var_default(df$bap96)), 2)
  expect_equal(ncol(cat_matrix_var_default(df$bap96)), 3)
  # -- manipulate data: empty labels
  attributes(df$bap96)$labels <- ""
  # --- test
  expect_equal(nrow(cat_matrix_var_default(df$bap96)), 1)
  expect_equal(ncol(cat_matrix_var_default(df$bap96)), 3)
  # -- manipulate data: no labels
  attributes(df$bap96)$labels <- NULL
  # --- test
  expect_equal(nrow(cat_matrix_var_default(df$bap96)), NULL)
  expect_equal(ncol(cat_matrix_var_default(df$bap96)), NULL)
})
#' cat_values_column_var_default: variable input - get column 'values'
test_that("cat_values_column_var_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_var_default(df$bap87), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5"))
  # -- manipulate data: empty labels
  attributes(df$bap87)$labels <- ""
  # --- test
  expect_equal(cat_values_column_var_default(df$bap87), "")
  # -- manipulate data: no labels
  attributes(df$bap87)$labels <- NULL
  # --- test
  expect_equal(cat_values_column_var_default(df$bap87), NULL)
})
#' cat_labels_column_var_default: variable input - get column 'labels'
test_that("cat_labels_column_var_default", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test
    expect_equal(cat_labels_column_var_default(df$bap87, "labels"), c(
      "Does not apply", "No Answer",  "Very good",  "Good", "Satisfactory",
      "Poor", "Bad")
    )
    # -- manipulate data: empty labels
    attributes(df$bap87)$labels <- ""
    # --- test
    expect_equal(cat_labels_column_var_default(df$bap87, "labels"), "")
    # -- manipulate data: no labels
    attributes(df$bap87)$labels <- NULL
    # --- test
    expect_equal(cat_labels_column_var_default(df$bap87, "labels"), NULL)
})
#' cat_df_var_default: variable input - get categories data frame
test_that("cat_df_var_default", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_var_default(df$bap87)[["labels_de"]], NULL)
  expect_equal(cat_df_var_default(df$bap87)[["labels"]], c(
    "Does not apply", "No Answer",  "Very good",  "Good", "Satisfactory",
    "Poor", "Bad")
    )
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_df_var_default(df$bap87)[["variable"]], rep("var_id", 7)
  )
  # -- manipulate data: no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_df_var_default(df$bap87)[["variable"]], rep("var_id", 7)
  )
  # manipulate data: empty label
  attributes(df$bap96)$labels <- ""
  expect_equal(cat_df_var_default(df$bap96)[["labels"]], "")
  # manipulate data: no label
  attributes(df$bap96)$labels <- NULL
  expect_equal(cat_df_var_default(df$bap96)[["labels"]], NULL)
  attributes(df$bap87)$labels <- NULL
  expect_equal(cat_df_var_default(df$bap87)[["values"]], NULL)
})
#' get_csv_default: data set input - making CSV files from generated data frames
test_that("get_csv_default_data", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test:  A: variables = "no
    get_csv_default(df, tempdir(), variables = "no")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_false(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_false(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # --- test: B: variables = "yes"
    get_csv_default(df, tempdir(), variables = "yes")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  })
#' get_csv_default: variable input - making CSV files from generated data frames
test_that("get_csv_default_variable", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test: A: variables = "no"
    get_csv_default(df$bap87, tempdir(), variables = "no")
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # -- test: B: variables = "yes"
    get_csv_default(df$bap87, tempdir(), variables = "yes")
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  })

# testthat::test_file("tests/testthat/test-r2csv_default.R")
# data <- get(load("tests/testthat/testdata/data_odf.RData"))
