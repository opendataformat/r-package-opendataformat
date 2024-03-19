#' get_dataset_all: data set input - make dataset.csv header and metadata
test_that("get_dataset_all", {
  # get data
  df <- get(load("testdata/data_odf.RData"))
  # test column 'dataset'
  expect_equal(get_dataset_all(df)[["dataset"]], "bap")
  # test column 'label'
  expect_equal(get_dataset_all(df)[["label"]], "Data from individual questionnaires 2010")
  # test column 'label_en'
  expect_equal(get_dataset_all(df)[["label_en"]], "Data from individual questionnaires 2010")
  # test column 'label_de'
  expect_equal(get_dataset_all(df)[["label_de"]], "Daten vom Personenfragebogen 2010")
  # test_column 'description'
  expect_equal(get_dataset_all(df)[["description"]], NULL)
  # test_column 'description_en'
  expect_equal(get_dataset_all(df)[["description_en"]], "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  # test_column 'description_de'
  expect_equal(get_dataset_all(df)[["description_de"]], "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  # test_column 'url'
  expect_equal(get_dataset_all(df)[["url"]], "https://paneldata.org/soep-core/data/bap")
  })
#' var_header: data set input - make variables.csv header as list of
#' unique attributes from all variables in data set
test_that("var_header", {
  # expected values
  expected <- c(
    "variable",
    "label",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url")

  # manipulate variable attributes
  df <- get(load("testdata/data_odf.RData"))
  attributes(df$bap87)$url <- NULL
  # actual values
  actual <- var_header_data(df)
  # Test that attribute lists are equal
  expect_equal(sort(actual), sort(expected))
})
#' var_matrix: data set input - number of rows and number of columns
test_that("var_matrix", {
  # get data
  df <- get(load("testdata/data_odf.RData"))
  # test number of columns
  expect_equal(ncol(var_matrix(df)), 8)
  # test number of rows
  expect_equal(nrow(var_matrix(df)), 7)
})
#' var_variable_column: data set input - get metadata for the column 'variable'
#' from the attribute 'name'
test_that("var_variable_column", {
  # all variable have "name" attribute and "name" attribute is not empty
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  #  -- test
  expect_equal(var_variable_column(df),
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # one variable has no "name" attribute and another has an empty name attribute
  # -- manipulate data
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # -- test
  expect_equal(var_variable_column(df_no_name),
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
#' var_df: data set input - variable name in variables data frame
test_that("var_df_name", {
  # all variables have variable names
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(var_df(df)[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # no variable has variable name
  # -- manipulate data
  df_no_name <- df
  for (var in attributes(df_no_name)$names) {
    attributes(df_no_name[[var]])$name <- NULL
  }
  # -- test
  expect_equal(var_df(df_no_name)[["variable"]],
               c("bap87", "bap9201", "bap9001", "bap9002",
                 "bap9003", "bap96", "name"))
  # one variable has no name, another has an empty name
  # -- manipulate data
  df_no_name <- df
  attributes(df_no_name$bap87)$name <- NULL
  attributes(df_no_name$bap9002)$name <- ""
  # -- test
  expect_equal(var_df(df_no_name)[["variable"]],
               c("bap87", "bap9201", "bap9001", "var_id_4",
                 "bap9003", "bap96", "name"))
})
#' var_df: data set input - variable label in variables data frame
test_that("var_df_label", {
  # all variables have a label
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(var_df(df)[["label_en"]],
               c("Current Health",
                 "hours of sleep, normal workday",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height",
                 "Firstname"))
  # one variable has no label, another has an empty label
  # -- manipulate data
  df_no_label <- df
  attributes(df_no_label$bap87)$label <- NULL
  attributes(df_no_label$bap9201)$label <- ""
  # -- test
  expect_equal(var_df(df_no_label)[["label_en"]],
               c("Current Health",
                 "hours of sleep, normal workday",
                 "Pressed For Time Last 4 Weeks",
                 "Run-down, Melancholy Last 4 Weeks",
                 "Well-balanced Last 4 Weeks",
                 "Height",
                 "Firstname"))
  # no variable has a label
  # -- manipulate data
  df_no_label <- df
  for (var in attributes(df_no_label)$names) {
    attributes(df_no_label[[var]])$label <- NULL
  }
  # -- test
  expect_equal(colnames(var_df(df_no_label)),
               c("variable", "label_en", "label_de",
                 "description_en", "description_de", "type", "url" ))

})
#' var_header_var: variable input - get header for variables.csv
test_that("var_header_var", {
  # variable has all attributes
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(var_header_var(df$bap87),
               c("variable", "label_en", "label_de",
                 "description_en", "description_de", "type", "url", "label"  ))
  # variable has no variable name
  # -- manipulate data
  attributes(df$bap87)$name <- NULL
  # -- test
  expect_equal(var_header_var(df$bap87),
               c("variable", "label_en", "label_de",
                 "description_en", "description_de", "type", "url", "label"  ))
  # variable has no label
  # -- manipulate data
  attributes(df$bap87)$label <- NULL
  # -- test
  expect_equal(var_header_var(df$bap87),
               c("variable", "label_en", "label_de",
                 "description_en", "description_de", "type", "url" ))

})
#' var_matrix_var: variable input - matrix for variables data frame
test_that("var_matrix_var", {
  # variable has all attributes
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(ncol(var_matrix_var(df$bap87)), 8)
  expect_equal(nrow(var_matrix_var(df$bap87)), 1)
  # variable has no name, label and url
  # -- manipulate data
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$label <- NULL
  attributes(df$bap87)$url <- NULL
  # -- test
  expect_equal(ncol(var_matrix_var(df$bap87)), 6)
  expect_equal(nrow(var_matrix_var(df$bap87)), 1)
})
#' var_variable_column_var: variable input - get metadata for 'variable' column
test_that("var_variable_column_var", {
  # variable has 'name' attribute
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(var_variable_column_var(df$bap87), "bap87")
  # variable 'name' attribute exists but is empty
  # -- manipulate data
  attributes(df$bap87)$name <- ""
  # -- test
  expect_equal(var_variable_column_var(df$bap87), "var_id")
  # variable has no 'name' attribute
  # -- manipulate data
  attributes(df$bap87)$name <- NULL
  # -- test
  expect_equal(var_variable_column_var(df$bap87), "var_id")
})
#' var_df_var: variable input - making variables data frame
test_that("var_df_var", {
  # variable has all attribute
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(var_df_var(df$bap87)[["variable"]],
               "bap87")
  expect_equal(var_df_var(df$bap87)[["label"]],
               "Current Health")
  expect_equal(var_df_var(df$bap87)[["label_en"]],
               "Current Health")
  expect_equal(var_df_var(df$bap87)[["label_de"]],
  "Gesundheitszustand gegenwärtig")
  expect_equal(var_df_var(df$bap87)[["description_de"]],
  "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(var_df_var(df$bap87)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap87")
  expect_equal(var_df_var(df$bap87)[["type"]],
               "numeric")
  # variable has no variable name, no label and an empty german description
  # -- manipulate data
  attributes(df$bap9002)$name <- NULL
  attributes(df$bap9002)$label <- NULL
  attributes(df$bap9002)$description_de <- ""
  # -- test
  expect_equal(var_df_var(df$bap9002)[["variable"]],
               "var_id")
  expect_equal(var_df_var(df$bap9002)[["label"]],
               NULL)
  expect_equal(var_df_var(df$bap9002)[["label_en"]],
               "Run-down, Melancholy Last 4 Weeks")
  expect_equal(var_df_var(df$bap9002)[["label_de"]],
               "Niedergeschlagen letzten 4 Wochen")
  expect_equal(var_df_var(df$bap9002)[["description_de"]],
               "")
  expect_equal(var_df_var(df$bap9002)[["url"]],
               "https://paneldata.org/soep-core/data/bap/bap9002")
  expect_equal(var_df_var(df$bap9002)[["type"]],
               "numeric")
})
#' cat_header_var: data set input - get column header for categories.csv
test_that("cat_header_data", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_header_data(df), c("variable", "value", "labels_en", "labels_de"))
})
#' cat_variable_column_data: data set input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_data", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  expect_equal(cat_variable_column_data(df), c(
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
  expect_equal(cat_variable_column_data(df_no_name), c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name"  ))
  # -- manipulate data: empty labels
  df_empty_labels <- df
  attributes(df_empty_labels$bap9001)$labels <- ""
  expect_equal(length(cat_variable_column_data(df_empty_labels)), 34)
  # -- manipulate data: no labels
  df_no_labels <- df
  attributes(df_no_labels$bap9001)$labels <- NULL
  expect_equal(length(cat_variable_column_data(df_no_labels)), 34)
})
#' cat_matrix_data: data set input - matrix for categories.csv
test_that("cat_matrix_data", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_data(df)), 34) # nrow
  expect_equal(ncol(cat_matrix_data(df)), 4) # ncol
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  attributes(df$bap96)$labels_en <- NULL
  # --- test
  expect_equal(nrow(cat_matrix_data(df)), 32) # nrow
  expect_equal(ncol(cat_matrix_data(df)), 4) # ncol
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_en <- ""
  attributes(df$bap87)$labels_de <- ""
  # --- test
  expect_equal(nrow(cat_matrix_data(df)), 26) # nrow
  expect_equal(ncol(cat_matrix_data(df)), 4) # ncol
})
#' cat_values_column_data: data set input - get column 'values'
#' for categories.csv
test_that("cat_values_column_data", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_data(df), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1", "1",  "2",
    "3",  "4",  "5",  "-2", "-1", "1",  "2",  "3",  "4",  "5", "-2", "-1", "1",
    "2",  "3",  "4",  "5",  "-2", "-1", "-2", "-1")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  attributes(df$bap96)$labels_en <- NULL
  # --- test
  expect_equal(length(cat_values_column_data(df)), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_de <- ""
  attributes(df$bap87)$labels_en <- ""
  # --- test
  expect_equal(length(cat_values_column_data(df)), 26)
})
#' cat_labels_column_data: data set input - get column 'labels'
#' for categories.csv
test_that("cat_labels_column_data", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_labels_column_data(df, "labels_en"), c(
    "Does not apply", "No Answer", "Very good", "Good", "Satisfactory", "Poor",
    "Bad", "Does not apply", "No Answer", "Does not apply", "No Answer",
    "Always", "Often", "Sometimes", "Almost Never", "Never",  "Does not apply",
    "No Answer", "Always",  "Often",  "Sometimes",  "Almost Never", "Never",
    "Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never",
    "Never", "Does not apply", "No Answer", "Does not apply", "No Answer")
    )
  # -- manipulate data: one variable is not categorical
  attributes(df$bap96)$labels_de <- NULL
  attributes(df$bap96)$labels_en <- NULL
  # --- test
  expect_equal(length(cat_labels_column_data(df, "labels_de")), 32)
  # -- manipulate data: another variable is categorical but labels are empty
  attributes(df$bap87)$labels_de <- ""
  attributes(df$bap87)$labels_en <- ""
  # --- test
  expect_equal(length(cat_labels_column_data(df, "labels_de")), 26)
})
#' cat_df_data: data set input - make categories data frame
test_that("cat_df_data",{
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_data(df)[["labels_de"]], c(
    "trifft nicht zu",  "keine Angabe", "Sehr gut", "Gut", "Zufriedenstellend",
    "Weniger gut", "Schlecht",  "trifft nicht zu",  "keine Angabe", "trifft nicht zu",
    "keine Angabe", "Immer",  "Oft",  "Manchmal", "Fast nie",
    "Nie",  "trifft nicht zu",  "keine Angabe", "Immer",  "Oft",
    "Manchmal", "Fast nie", "Nie",  "trifft nicht zu",  "keine Angabe",
    "Immer",  "Oft",  "Manchmal", "Fast nie", "Nie",
    "trifft nicht zu", "keine Angabe",  "trifft nicht zu",  "keine Angabe")
    )
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  expect_equal(cat_df_data(df)[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data(df)[["labels_de"]], c(
    "trifft nicht zu",  "keine Angabe", "Sehr gut", "Gut", "Zufriedenstellend",
    "Weniger gut", "Schlecht",  "trifft nicht zu",  "keine Angabe", "trifft nicht zu",
    "keine Angabe", "Immer",  "Oft",  "Manchmal", "Fast nie",
    "Nie",  "trifft nicht zu",  "keine Angabe", "Immer",  "Oft",
    "Manchmal", "Fast nie", "Nie",  "trifft nicht zu",  "keine Angabe",
    "Immer",  "Oft",  "Manchmal", "Fast nie", "Nie",
    "trifft nicht zu", "keine Angabe",  "trifft nicht zu",  "keine Angabe")
  )
  # -- manipulate data: empty labels and empty variable name
  attributes(df$bap87)$labels_en <- ""
  attributes(df$bap87)$labels_de <- ""
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_df_data(df)[["labels_de"]], c(
    "", "trifft nicht zu",  "keine Angabe", "trifft nicht zu",
    "keine Angabe", "Immer",  "Oft",  "Manchmal", "Fast nie",
    "Nie",  "trifft nicht zu",  "keine Angabe", "Immer",  "Oft",
    "Manchmal", "Fast nie", "Nie",  "trifft nicht zu",  "keine Angabe",
    "Immer",  "Oft",  "Manchmal", "Fast nie", "Nie",
    "trifft nicht zu", "keine Angabe",  "trifft nicht zu",  "keine Angabe")
  )
  expect_equal(cat_df_data(df)[["variable"]], c(
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
  expect_equal(cat_df_data(df)[["variable"]], c(
    "bap87", "bap87", "bap87", "bap87", "bap87", "bap87", "bap87",
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data(df)[["labels_de"]], c(
    "trifft nicht zu",  "keine Angabe", "Sehr gut", "Gut", "Zufriedenstellend",
    "Weniger gut", "Schlecht",  "trifft nicht zu",  "keine Angabe", "trifft nicht zu",
    "keine Angabe", "Immer",  "Oft",  "Manchmal", "Fast nie",
    "Nie",  "trifft nicht zu",  "keine Angabe", "Immer",  "Oft",
    "Manchmal", "Fast nie", "Nie",  "trifft nicht zu",  "keine Angabe",
    "Immer",  "Oft",  "Manchmal", "Fast nie", "Nie",
    "trifft nicht zu", "keine Angabe",  "trifft nicht zu",  "keine Angabe")
  )
  # -- manipulate data: no variable name and no labels
  attributes(df$bap87)$name <- NULL
  attributes(df$bap87)$labels_de <- NULL
  attributes(df$bap87)$labels_en <- NULL
  expect_equal(cat_df_data(df)[["variable"]], c(
    "bap9201", "bap9201",
    "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001", "bap9001",
    "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002", "bap9002",
    "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003", "bap9003",
    "bap96",  "bap96",
    "name", "name" )
  )
  expect_equal(cat_df_data(df)[["labels_de"]], c(
    "trifft nicht zu",  "keine Angabe", "trifft nicht zu",
    "keine Angabe", "Immer",  "Oft",  "Manchmal", "Fast nie",
    "Nie",  "trifft nicht zu",  "keine Angabe", "Immer",  "Oft",
    "Manchmal", "Fast nie", "Nie",  "trifft nicht zu",  "keine Angabe",
    "Immer",  "Oft",  "Manchmal", "Fast nie", "Nie",
    "trifft nicht zu", "keine Angabe",  "trifft nicht zu",  "keine Angabe")
  )
})
#' cat_header_var: variable input - get column header for categories.csv
test_that("cat_header_var", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_header_var(df$bap87), c(
    "variable", "value", "labels_en", "labels_de")
  )
  # --- manipulate data: empty variable names
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_header_var(df$bap87), c(
    "variable", "value", "labels_en", "labels_de")
  )
  # --- manipulate data: no variable names
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_header_var(df$bap87), c(
    "variable", "value", "labels_en", "labels_de")
  )
  # --- manipulate data: empty variable labels
  attributes(df$bap87)$labels_en <- ""
  # --- test
  expect_equal(cat_header_var(df$bap87), c(
    "variable", "value", "labels_en", "labels_de")
  )
  # --- manipulate data: no variable labels
  attributes(df$bap87)$labels_de <- NULL
  print(attributes(df$bap87)$labels_de)
  # --- test
  expect_equal(cat_header_var(df$bap87), c(
    "variable", "value", "labels_en")
  )
})
#' cat_variable_column_var: variable input - get column 'variable'
#' for categories.csv
test_that("cat_variable_column_var", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_variable_column_var(df$bap9201), c(
    "bap9201", "bap9201")
  )
  # --- manipulate data: empty variable name
  attributes(df$bap9201)$name <- ""
  # --- test
  expect_equal(cat_variable_column_var(df$bap9201), c(
    "var_id", "var_id")
  )
  # --- manipulate data: no variable name
  attributes(df$bap9201)$name <- NULL
  # --- test
  expect_equal(cat_variable_column_var(df$bap9201), c(
    "var_id", "var_id")
  )
})
#' cat_matrix_var: variable input - matrix for categories.csv
test_that("cat_matrix_var", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(nrow(cat_matrix_var(df$bap96)), 2)
  expect_equal(ncol(cat_matrix_var(df$bap96)), 4)
  # -- manipulate data: empty labels
  attributes(df$bap96)$labels_de <- ""
  attributes(df$bap96)$labels_en <- ""
  # --- test
  expect_equal(nrow(cat_matrix_var(df$bap96)), 1)
  expect_equal(ncol(cat_matrix_var(df$bap96)), 4)
  # -- manipulate data: no labels
  attributes(df$bap96)$labels_de <- NULL
  attributes(df$bap96)$labels_en <- NULL
  # --- test
  expect_equal(nrow(cat_matrix_var(df$bap96)), NULL)
  expect_equal(ncol(cat_matrix_var(df$bap96)), NULL)
})
#' cat_values_column_var: variable input - get column 'values'
test_that("cat_values_column_var", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_values_column_var(df$bap87), c(
    "-2", "-1", "1",  "2",  "3",  "4",  "5"))
  # -- manipulate data: empty labels
  attributes(df$bap87)$labels_de <- ""
  attributes(df$bap87)$labels_en <- ""
  # --- test
  expect_equal(cat_values_column_var(df$bap87), "")
  # -- manipulate data: no labels
  attributes(df$bap87)$labels_de <- NULL
  attributes(df$bap87)$labels_en <- NULL
  # --- test
  expect_equal(cat_values_column_var(df$bap87), NULL)
})
#' cat_labels_column_var: variable input - get column 'labels' or
#' 'labels_de', 'labels_en
test_that("cat_labels_column_var'", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test
    expect_equal(cat_labels_column_var(df$bap87, "labels_en"), c(
      "Does not apply", "No Answer",  "Very good",  "Good", "Satisfactory",
      "Poor", "Bad")
    )
    expect_equal(cat_labels_column_var(df$bap87, "labels_de"), c(
      "trifft nicht zu",  "keine Angabe", "Sehr gut", "Gut",
      "Zufriedenstellend", "Weniger gut",  "Schlecht" )
    )
    # -- manipulate data: empty labels
    attributes(df$bap87)$labels_de <- ""
    attributes(df$bap87)$labels_en <- ""
    # --- test
    expect_equal(cat_labels_column_var(df$bap87, "labels_de"), "")
    # -- manipulate data: no labels
    attributes(df$bap87)$labels_en <- NULL
    # --- test
    expect_equal(cat_labels_column_var(df$bap87, "labels_en"), NULL)

})
#' cat_df_var: variable input - get categories data frame
test_that("cat_df_var", {
  # -- get data
  df <- get(load("testdata/data_odf.RData"))
  # --- test
  expect_equal(cat_df_var(df$bap87)[["labels_de"]], c(
    "trifft nicht zu",  "keine Angabe", "Sehr gut", "Gut",
    "Zufriedenstellend", "Weniger gut",  "Schlecht" )
  )
  # -- manipulate data: empty variable name
  attributes(df$bap87)$name <- ""
  # --- test
  expect_equal(cat_df_var(df$bap87)[["variable"]], rep("var_id", 7)
  )
  # -- manipulate data: no variable name
  attributes(df$bap87)$name <- NULL
  # --- test
  expect_equal(cat_df_var(df$bap87)[["variable"]], rep("var_id", 7)
  )
  # manipulate data: empty label
  attributes(df$bap96)$labels_de <- ""
  expect_equal(cat_df_var(df$bap96)[["labels_de"]], c("",""))
  # manipulate data: no label
  attributes(df$bap96)$labels <- NULL
  expect_equal(cat_df_var(df$bap96)[["labels"]], NULL)
  attributes(df$bap87)$labels <- NULL
  expect_equal(cat_df_var(df$bap87)[["labels"]], NULL)
})
#' get_csv_all: data set input - making CSV files from generated data frames
test_that("get_csv_all_data", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # --- test: A: variables = "no
    unlink(paste0(tempdir(),"/*"))
    get_csv_all(df, tempdir(), variables = "no")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_false(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_false(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # --- test: B: variables = "yes"
    get_csv_all(df, tempdir(), variables = "yes")
    expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  })
#' get_csv_all: variable input - making CSV files from generated data frames
test_that("get_csv_all_variable", {
    # -- get data
    df <- get(load("testdata/data_odf.RData"))
    # A: variables = "no"
    get_csv_all(df$bap87, tempdir(), variables = "no")
    # -- test
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
    # B: variables = "yes"
    get_csv_all(df$bap87, tempdir(), variables = "yes")
    # -- test
    expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
    expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
    expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
    unlink(paste0(tempdir(),"/*"))
  })

# testthat::test_file("tests/testthat/test-r2csv_all.R")
