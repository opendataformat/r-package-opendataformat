#' get_dataset_lang: data set input - make dataset.csv header and metadata
test_that("get_dataset_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test column 'dataset'
  expect_equal(get_dataset_lang(df, "de")[["dataset"]], "bap")
  # --- test column 'label_de'
  expect_equal(get_dataset_lang(df, "en")[["label_en"]], "Data from individual questionnaires 2010")
  # --- test_column 'description_en'
  expect_equal(get_dataset_lang(df, "en")[["description_en"]], "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  # --- test_column 'url'
  expect_equal(get_dataset_lang(df, "en")[["url"]], "https://paneldata.org/soep-core/data/bap")
  # manipulate data: empty label_de
  attributes(df)$label_de <- ""
  # --- test column 'label'
  expect_equal(get_dataset_lang(df, "de")[["label_de"]], "")
  # manipulate data: no label-de
  attributes(df)$label_de <- NULL
  # --- test column 'label'
  expect_equal(get_dataset_lang(df, "de")[["label_de"]], NULL)
  # print(get_dataset_lang(df, "de"))
  # print(get_dataset_lang(df, "en"))
})
#' var_header_data_lang: dataset input - make variables.csv header as list of
#' unique attributes from all variables in data set with attributes
#' that include the selected language code
test_that("var_header_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_header_data_lang(df, "de"), c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url")
    )
  expect_equal(var_header_data_lang(df, "en"), c(
    "variable",
    "label_en",
    "description_en",
    "type",
    "url")
  )
  expect_equal(var_header_data_lang(df, "it"), c(
    "variable",
    "type",
    "url")
  )
  # -- manipulate data: no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(sort(var_header_data_lang(df, "de")), sort(c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"))
  )
  # -- manipulate data: no url
  attributes(df$bap87)$url <- NULL
  # --- test
  expect_equal(sort(var_header_data_lang(df, "de")), sort(c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"))
  )
})
#' var_matrix_lang: datset input - number of rows and
#' number of columns
test_that("var_matrix_lang", {
  # get data
  df <- get(load("testdata/data_odf.RData"))
  # test number of columns
  expect_equal(ncol(var_matrix_lang(df, "de")), 5)
  # test number of rows
  expect_equal(nrow(var_matrix_lang(df, "de")), 7)
})
#' var_variable_column_lang: data set input - get metadata
#' for the column 'variable' from the attribute 'name'
test_that("var_variable_column_lang", {
  # all variable have "name" attribbute and "name" atribute is not empty
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  #  -- test
  expect_equal(var_variable_column_lang(df),
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # one variable has no "name" attribute and anothe has an empty name attribute
  # -- manipulate data
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # -- test
  expect_equal(var_variable_column_lang(df_no_name),
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
#' var_df_lang: data set input - variable name in variables data frame
test_that("var_df_lang_name", {
  # -- get data: all variables have variable names
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_lang(df, "de")[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # -- manipulate data: no variable has variable name
  df_no_name <- df
  for (var in attributes(df_no_name)$names) {
    attributes(df_no_name[[var]])$name <- NULL
  }
  # --- test
  expect_equal(var_df_lang(df_no_name, "de")[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # -- manipulate data: one variable has no name, another has an empty name
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # --- test
  expect_equal(var_df_lang(df_no_name, "de")[["variable"]],
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
# ' var_df_lang: data set input - variable label in variables data frame
test_that("var_df_lang_label", {
  # -- get data: all variables have a label
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_lang(df, "en")[["label_en"]],
               c("Current Health",
                 "hours of sleep, normal workday",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height",
                 "Firstname")
               )
  expect_equal(colnames(var_df_lang(df, "de")), c("variable",
                                                 "label_de",
                                                 "description_de",
                                                 "type",
                                                 "url")
               )
  # -- manipulate data: one variable has no label, another has an empty label
  df_no_label <- df
  attributes(df_no_label$bap87)$label_en <- NULL
  attributes(df_no_label$bap9201)$label_en <- ""
  # --- test
  expect_equal(var_df_lang(df_no_label, "en")[["label_en"]],
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
    attributes(df_no_label[[var]])$label_de <- NULL
  }
  # -- test
  expect_equal(colnames(var_df_lang(df_no_label, "de")),
               c("variable",
                 "description_de",
                 "type",
                 "url" )
               )
})
#' var_header_var_lang: variable input - get header for variables.csv
test_that("var_header_var_var_lang", {
  # -- get data: variable has all attributes
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_header_var_lang(df$bap87, "de"),
               c("variable",
                 "label_de",
                 "description_de",
                 "type",
                 "url" )
               )
  # -- manipulate data: variable has no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(var_header_var_lang(df$bap87, "de"),
               c("variable",
                 "label_de",
                 "description_de",
                 "type",
                 "url" )
               )
  # -- manipulate data: variable has no label
  attributes(df$bap87)$label_de <- NULL
  # -- test
  expect_equal(var_header_var_lang(df$bap87, "de"),
               c("variable",
                 "description_de",
                 "type",
                 "url" )
               )
})
#' var_variable_column_var_lang: variable input - get metadata
#' for 'variable' column
test_that("var_variable_column_var_lang", {
  # -- get data: variable has 'name' attribute
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_variable_column_var_lang(df$bap87), "bap87")
  # -- manipulate data: variable 'name' attribute exists but is empty
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(var_variable_column_var_lang(df$bap87), "var_id")
  # -- manipulate data: variable has no 'name' attribute
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(var_variable_column_var_lang(df$bap87), "var_id")
})
#' var_matrix_var_lang: variable input - matrix for variables data frame
test_that("var_matrix_var_lang", {
# -- get data: variable has all attributes
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(ncol(var_matrix_var_lang(df$bap87, "de")), 5)
  # expect_equal(nrow(var_matrix_var_lang(df$bap87)), 1)
  # -- manipulate data: variable has no name, label and url
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$label_de <- NULL
  attributes(df$bap87)$url <- NULL
  # # --- test
  expect_equal(ncol(var_matrix_var_lang(df$bap87, "de")), 3)
  expect_equal(nrow(var_matrix_var_lang(df$bap87, "de")), 1)
})
#' var_df_var_lang: variable input - making variables data frame
test_that("var_df_var_lang", {
  # -- get data: variable has all attribute
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(var_df_var_lang(df$bap87, "de")[["variable"]],
               "bap87")
  expect_equal(var_df_var_lang(df$bap87, "en")[["label_en"]],
               "Current Health")
  expect_equal(var_df_var_lang(df$bap87, "de")[["label_en"]],
               NULL)
  expect_equal(var_df_var_lang(df$bap87, "en")[["description_en"]],
  "Question: How would you describe your current health?")
  expect_equal(var_df_var_lang(df$bap87, "en")[["description_de"]],
               NULL)
  expect_equal(var_df_var_lang(df$bap87, "de")[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap87")
  expect_equal(var_df_var_lang(df$bap87, "de")[["type"]],
               "numeric")
  # -- manipulate data: variable has no variable name,
  # no label and an empty description
  attributes(df$bap9002)$name <- NULL
  attributes(df$bap9002)$label_de <- NULL
  attributes(df$bap9002)$description_de <- ""
  # --- test
  expect_equal(var_df_var_lang(df$bap9002, "de")[["variable"]],
               "var_id")
  expect_equal(var_df_var_lang(df$bap9002, "de")[["label_de"]],
               NULL)
  expect_equal(var_df_var_lang(df$bap9002, "de")[["description_de"]],
               "")
  expect_equal(var_df_var_lang(df$bap9002, "en")[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap9002")
  expect_equal(var_df_var_lang(df$bap9002, "en")[["type"]],
               "numeric")
})
#' cat_header_var_lang: dataset input - get column header for categories.csv
test_that("cat_header_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_header_data_lang(df, "de"), c("variable", "value", "labels_de"))
  expect_equal(cat_header_data_lang(df, "en"), c("variable", "value", "labels_en"))
  expect_equal(cat_header_data_lang(df, "it"), c("variable", "value"))
})
#' cat_variable_column_data_lang: dataset input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_variable_column_data_lang(df, "de"), c(
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
  expect_equal(cat_variable_column_data_lang(df_no_name, "de"), c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" ))
  # -- manipulate data: empty labels
  df_empty_labels <- df
  attributes(df_empty_labels$bap9001)$labels_de <- ""
  expect_equal(length(cat_variable_column_data_lang(df_empty_labels, "de")), 28)
  # -- manipulate data: no labels
  df_no_labels <- df
  attributes(df_no_labels$bap9001)$labels_de <- NULL
  expect_equal(length(cat_variable_column_data_lang(df_no_labels, "de")), 27)
})
#' cat_matrix_data_lang: dataset input - matrix for categories.csv
test_that("cat_matrix_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_data_lang(df, "de")), 34) # nrow
  expect_equal(ncol(cat_matrix_data_lang(df, "de")), 3) # ncol
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  # # --- test
  expect_equal(nrow(cat_matrix_data_lang(df, "de")), 32) # nrow
  expect_equal(ncol(cat_matrix_data_lang(df, "de")), 3) # ncol
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(nrow(cat_matrix_data_lang(df, "de")), 26) # nrow
  expect_equal(ncol(cat_matrix_data_lang(df, "de")), 3) # ncol
  expect_equal(colnames(cat_matrix_data_lang(df, "de")), c("variable",
                                                           "value",
                                                           "labels_de"))
})
#' cat_values_column_data_lang: dataset input - get column 'values'
#' for categories.csv
test_that("cat_values_column_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_data_lang(df, "de"), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1", "1",  "2",
    "3",  "4",  "5",  "-2", "-1", "1",  "2",  "3",  "4",  "5", "-2", "-1", "1",
    "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  # --- test
  expect_equal(length(cat_values_column_data_lang(df, "de")), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(length(cat_values_column_data_lang(df, "de")), 26)
})
#' cat_labels_column_data_lang: data set input - get column 'labels'
#' for categroies.csv
test_that("cat_labels_column_data_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_labels_column_data_lang(df, "en"), c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  # --- test
  expect_equal(length(cat_labels_column_data_lang(df, "de")), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(length(cat_labels_column_data_lang(df, "de")), 26)
})
#' cat_df_data_lang: data set input - make categories data frame
test_that("cat_df_data_lang",{
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_data_lang(df, "de")[1:10, "labels_de"], c(
    "trifft nicht zu",
    "keine Angabe",
    "Sehr gut",
    "Gut",
    "Zufriedenstellend",
    "Weniger gut",
    "Schlecht",
    "trifft nicht zu",
    "keine Angabe",
    "trifft nicht zu")
    )
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  expect_equal(cat_df_data_lang(df, "de")[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_lang(df, "en")[["labels_en"]], c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
  # -- manipulate data: empty labels and empty variable name
  attributes(df$bap87)$labels_en <- ""
  attributes(df$bap87)$name <- ""
# --- test
  # -- manipulate data: no variable name
  df <- get(load("testdata/data_odf.RData"))
  attributes(df$bap87)$name <- NULL
  expect_equal(cat_df_data_lang(df, "de")[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_lang(df, "en")[["labels_en"]], c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
  # -- manipulate data: no variable name and no labels
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$labels_en <- NULL
  expect_equal(cat_df_data_lang(df, "en")[["variable"]], c(
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data_lang(df, "en")[["labels_en"]], c(
    "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
  )
})
#' cat_header_var_lang: variable input - get column header for categories.csv
test_that("cat_header_var_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_header_var_lang(df$bap87, "de"), c(
    "variable", "value", "labels_de")
  )
  # --- manipulate data: empty variable names
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_header_var_lang(df$bap87, "de"), c(
    "variable", "value", "labels_de")
  )
  # --- manipulate data: no variable names
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_header_var_lang(df$bap87, "de"), c(
    "variable", "value", "labels_de")
  )
  # --- manipulate data: empty variable labels
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(cat_header_var_lang(df$bap87, "en"), c(
    "variable", "value", "labels_en")
  )
  # --- manipulate data: no variable labels
  attributes(df$bap87)$labels_de <- NULL
  # --- test
  expect_equal(cat_header_var_lang(df$bap87, "de"), c(
    "variable", "value")
  )
})
#' cat_variable_column_var_lang: variable input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_var_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_variable_column_var_lang(df$bap9201, "de"), c(
    "bap9201", "bap9201")
  )
  # --- manipulate data: empty variable name
  attributes(df$bap9201)$name <- ""
  # --- test
  expect_equal(cat_variable_column_var_lang(df$bap9201, "de"), c(
    "var_id", "var_id")
  )
  # --- manipulate data: no variable name
  attributes(df$bap9201)$name <- NULL
  # --- test
  expect_equal(cat_variable_column_var_lang(df$bap9201, "de"), c(
    "var_id", "var_id")
  )
})
#' cat_matrix_var_lang: variable input - matrix for categories.csv
test_that("cat_matrix_var_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_var_lang(df$bap96, "de")), 2)
  expect_equal(ncol(cat_matrix_var_lang(df$bap96, "de")), 3)
  # -- manipulate data: empty labels
  attributes(df$bap96)$labels_de <- ""
  # --- test
  expect_equal(nrow(cat_matrix_var_lang(df$bap96, "de")), 1)
  expect_equal(ncol(cat_matrix_var_lang(df$bap96, "de")), 3)
  # -- manipulate data: no labels
  attributes(df$bap96)$labels_en <- NULL
  # --- test
  expect_equal(nrow(cat_matrix_var_lang(df$bap96, "en")), NULL)
  expect_equal(ncol(cat_matrix_var_lang(df$bap96, "en")), NULL)
})
#' cat_values_column_var_lang:: variable input - get column 'values'
test_that("cat_values_column_var_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_var_lang(df$bap87, "de"), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5"))
  # -- manipulate data: empty labels
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(cat_values_column_var_lang(df$bap87, "de"), "")
  # -- manipulate data: no labels
  attributes(df$bap87)$labels_de <- NULL
  # --- test
  expect_equal(cat_values_column_var_lang(df$bap87, "de"), NULL)
})
#' cat_labels_column_var_lang: variable input - get column 'labels'
test_that("cat_labels_column_var_lang:", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test
    expect_equal(cat_labels_column_var_lang(df$bap87, "en"), c(
      "Does not apply", "No Answer",  "Very good",  "Good", "Satisfactory",
      "Poor", "Bad")
    )
    # -- manipulate data: empty labels
    attributes(df$bap87)$labels_de <- ""
    # --- test
    expect_equal(cat_labels_column_var_lang(df$bap87, "de"), "")
    # -- manipulate data: no labels
    attributes(df$bap87)$labels_de <- NULL
    # --- test
    expect_equal(cat_labels_column_var_lang(df$bap87, "de"), NULL)
})
#' cat_df_var_lang: variable input - get categories dataframe
test_that("cat_df_var_lang", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_var_lang(df$bap87, "en")[["labels_de"]], NULL)
  expect_equal(cat_df_var_lang(df$bap87, "en")[["labels_en"]], c(
    "Does not apply", "No Answer",  "Very good",  "Good", "Satisfactory",
    "Poor", "Bad")
    )
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_df_var_lang(df$bap87, "de")[["variable"]], rep("var_id", 7)
  )
  # -- manipulate data: no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_df_var_lang(df$bap87, "de")[["variable"]], rep("var_id", 7)
  )
  # manipulate data: empty label
  attributes(df$bap96)$labels_de <- ""
  expect_equal(cat_df_var_lang(df$bap96, "de")[["labels_de"]], "")
  # manipulate data: no label
  attributes(df$bap96)$labels_de <- NULL
  expect_equal(cat_df_var_lang(df$bap96, "de")[["labels_de"]], NULL)
  attributes(df$bap87)$labels_en <- NULL
  expect_equal(cat_df_var_lang(df$bap87, "en")[["values"]], NULL)
})
#' get_csv_lang: data set input - making CSV files from generated data frames
test_that("get_csv_lang_data", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test:  A: variables = "no"
    get_csv_lang(df, tempdir(), variables = "no", languages = "de")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_false(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_false(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # --- test: B: variables = "yes"
    get_csv_lang(df, tempdir(), variables = "yes", languages = "de")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # --- test:  C: variables = "no" AND languages = "en"
    get_csv_lang(df, tempdir(), variables = "no", languages = "en")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_false(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_false(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  }
)
#' get_csv_lang: variable input - making CSV files from generated data frames
test_that("get_csv_lang_variable", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test: A: variables = "no"
    get_csv_lang(df$bap87, tempdir(), variables = "no", languages = "de")
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # -- test: B: variables = "yes"
    get_csv_lang(df$bap87, tempdir(), variables = "yes", languages = "de")
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # -- test: C: variables = "yes" AND languages = "en"
    get_csv_lang(df$bap87, tempdir(), variables = "yes", languages = "en")
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  })

# testthat::test_file("tests/testthat/test-r2csv_lang.R")
# covr::package_coverage()

