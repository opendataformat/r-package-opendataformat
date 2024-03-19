#' load_csv
#' - no available file name
test_that("load_csv_error", {
  expect_error(load_csv("testdata/csv", "mydata.csv"))
})
#' - data.csv: check class
test_that("load_csv_data_class", {
 expect_equal(class(load_csv("testdata/csv", "data.csv")), "data.frame")
})
#' - data.csv: check variable values
test_that("load_csv_data_values", {
  expect_equal(load_csv("testdata/csv", "data.csv")[["bap87"]][1:3], c(4, 3, NA))
})
#' - data.csv: check variable names of data frame
test_that("load_csv_data_varnames", {
  expect_equal(names(load_csv("testdata/csv", "data.csv")), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name")
    )
})
#'- dataset_csv: check class
test_that("load_csv_dataset_class",{
  expect_equal(class(load_csv("testdata/csv", "dataset.csv")), "data.frame")
})
#' - dataset_csv: check column names
test_that("load_csv_dataset_columns", {
  expect_equal(names(load_csv("testdata/csv", "dataset.csv")), c(
    "dataset",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "url")
    )
})
#' - dataset.csv: check content
test_that("load_csv_dataset_content",{
  expect_equal(load_csv("testdata/csv", "dataset.csv")$dataset, "bap")
  expect_equal(load_csv("testdata/csv", "dataset.csv")$label_de,
               "Daten vom Personenfragebogen 2010")
  expect_equal(load_csv("testdata/csv", "dataset.csv")$url,
               "https://paneldata.org/soep-core/data/bap")
})
#'- variables.csv: check class
test_that("load_csv_variables_class",{
  expect_equal(class(load_csv("testdata/csv", "variables.csv")), "data.frame")
})
#' - variables.csv: check column names
test_that("load_csv_variables_columns", {
  expect_equal(names(load_csv("testdata/csv", "variables.csv")), c(
    "variable",
    "label_en",
    "label_de",
    "type",
    "description_en",
    "description_de",
    "url")
  )
})
#' - variables.csv: check content
test_that("load_csv_variables_content",{
  expect_equal(load_csv("testdata/csv", "variables.csv")$variable, c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name")
  )
  expect_equal(load_csv("testdata/csv", "variables.csv")[["label_de"]][1:3], c(
    "Gesundheitszustand gegenwärtig ",
    "Stunden Schlaf, normaler Werktag ",
    "Eile, Zeitdruck letzten 4 Wochen ")
  )
  expect_equal(load_csv("testdata/csv", "variables.csv")[["type"]][4:6], c(
    "numeric",
    "numeric",
    "numeric")
  )
})
#' - categories_csv: check class
test_that("load_csv_categories_class",{
  expect_equal(class(load_csv("testdata/csv", "categories.csv")), "data.frame")
})
#' - categories.csv: check column names
test_that("load_csv_categories_columns", {
  expect_equal(names(load_csv("testdata/csv", "categories.csv")), c(
    "variable",
    "value",
    "label_en",
    "label_de")
  )
})
#' - categories.csv: check content
test_that("load_csv_categories_content",{
  expect_equal(load_csv("testdata/csv", "categories.csv")$variable, c(
    rep("bap87", 7),
    rep("bap9201", 2),
    rep("bap9001", 7),
    rep("bap9002", 7),
    rep("bap9003", 7),
    rep("bap96", 2),
    rep("name", 2))
  )
  expect_equal(load_csv("testdata/csv", "categories.csv")$value, c(
    subset(c(-2:5), c(-2:5)!=0),
    subset(c(-2:-1), c(-2:-1)!=0),
    subset(c(-2:5), c(-2:5)!=0),
    subset(c(-2:5), c(-2:5)!=0),
    subset(c(-2:5), c(-2:5)!=0),
    subset(c(-2:-1), c(-2:-1)!=0),
    subset(c(-2:-1), c(-2:-1)!=0))
  )
  zufrieden <- c("trifft nicht zu",
             "keine Angabe",
             "Sehr gut",
             "Gut",
             "Zufriedenstellend",
             "Weniger gut",
             "Schlecht")
  immer <- c("trifft nicht zu",
             "keine Angabe",
             "Immer",
             "Oft",
             "Manchmal",
             "Fast nie",
             "Nie")
  expect_equal(load_csv("testdata/csv", "categories.csv")$label_de, c(
    zufrieden,
    zufrieden[1:2],
    immer,
    immer,
    immer,
    zufrieden[1:2],
    zufrieden[1:2])
  )
})
#' get_lang_csv
test_that("get_lang_csv", {
  expect_equal(get_lang_csv(load_csv("testdata/csv", "variables.csv"),
                     "label_"), c("en", "de"))
  expect_equal(get_lang_csv(load_csv("testdata/csv", "variables.csv"),
                     "description_"), c("en", "de"))
  expect_equal(get_lang_csv(load_csv("testdata/csv", "dataset.csv"),
                     "label_"), c("en", "de"))
  expect_equal(get_lang_csv(load_csv("testdata/csv", "categories.csv"),
                     "label_"), c("en", "de"))
  expect_equal(get_lang_csv(load_csv("testdata/csv", "categories.csv"),
                     "description_"), character())
  expect_equal(get_lang_csv(load_csv("testdata/csv", "categories.csv"),
                     "myattribute_"), character())

})
#' dataset_attributes
test_that("dataset_attributes", {
  # - make data
  df <- load_csv("testdata/csv", "data.csv")
  df <- dataset_attributes(df, "testdata/csv")
  # - tests
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, NULL)
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
})
#' dataset_attributes: additional label column, no url and no description_en
test_that("dataset_attributes_manipulated", {
  # - make data
  df <- load_csv("testdata/csv_manipulated", "data.csv")
  df <- dataset_attributes(df, "testdata/csv_manipulated")
  # - dataset columns
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  # dataset attributes
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, NULL)
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$description_en, NULL)
  expect_equal(attributes(df)$url, NULL)
  # no additional variable attributes expected
  expect_equal(attributes(df$bap87), NULL)
})
#' variables_attributes
test_that("variable_attributes", {
  # - make data
  df <- load_csv("testdata/csv", "data.csv")
  df <- variables_attributes(df, "testdata/csv")
  # - dataset columns
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  # - variable attributes
  expect_equal(attributes(df$bap87)$name, "bap87")
  expect_equal(attributes(df$bap87)$label, NULL)
  expect_equal(attributes(df$bap87)$label_de, "Gesundheitszustand gegenwärtig ")
  expect_equal(attributes(df$bap87)$label_en, "Current Health")
  expect_equal(attributes(df$bap87)$description_de, "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(attributes(df$bap87)$description_en, "Question: How would you describe your current health?")
  expect_equal(attributes(df$bap87)$type, "numeric")
  expect_equal(attributes(df$bap87)$url, "https://paneldata.org/soep-core/data/bap/bap87")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(attributes(df$name)$url, "")
  # - no labels expected (categories)
  expect_equal(attributes(df$bap87)$labels, NULL)
})
#' variables_attributes: additional label column, no url and no description_en
test_that("variable_attributes_manipulated", {
  # - make data
  df <- load_csv("testdata/csv_manipulated", "data.csv")
  df <- variables_attributes(df, "testdata/csv_manipulated")
  # - dataset columns
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  # - variable attributes
  expect_equal(attributes(df$bap87)$name, "bap87")
  expect_equal(attributes(df$bap87)$label, NULL)
  expect_equal(attributes(df$bap87)$label_de, "Gesundheitszustand gegenwärtig ")
  expect_equal(attributes(df$bap87)$label_en, "Current Health")
  expect_equal(attributes(df$bap87)$description_de, "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(attributes(df$bap87)$type, "numeric")
  expect_equal(attributes(df$bap87)$url, NULL)
  expect_equal(attributes(df$bap96)$url, NULL)
  expect_equal(attributes(df$name)$url, NULL)
  # - no labels expected (categories)
  expect_equal(attributes(df$bap87)$labels, NULL)
})
#' categories_attributes
test_that("categories_attributes", {
  # - make data
  df <- load_csv("testdata/csv", "data.csv")
  df <- categories_attributes(df, "testdata/csv")
  # - dataset columns
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  # - categories attributes
  expect_equal(names(attributes(df$bap87)$labels), NULL)
  expect_equal(names(attributes(df$bap87)$labels_en), c(
    "Does not apply",
    "No Answer",
    "Very good",
    "Good",
    "Satisfactory",
    "Poor",
    "Bad"
  ))
  expect_equal(as.character(unname(attributes(df$bap87)$labels_en)), c(
    "-2",
    "-1",
    "1",
    "2",
    "3",
    "4",
    "5"
  ))
  expect_equal(names(attributes(df$name)$labels_en), c(
    "Does not apply",
    "No Answer"
  ))
  expect_equal(as.character(unname(attributes(df$name)$labels_en)), c(
    "-2",
    "-1"))
  expect_equal(as.character(unname(attributes(df$name)$labels_de)), c(
    "-2",
    "-1"))
  expect_equal(names(attributes(df$name)$labels_de), c(
    "trifft nicht zu",
    "keine Angabe"
  ))
})
#' csv2r: main function
test_that("csv2r", {
  # - make data with all attributes
  df <-  csv2r("testdata/csv")
  # - dataset columns
  expect_equal(names(df), c(
    "bap87",
    "bap9201",
    "bap9001",
    "bap9002",
    "bap9003",
    "bap96",
    "name"))
  # - dataset attributes
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  # - variable attributes
  expect_equal(attributes(df$bap87)$name, "bap87")
  expect_equal(attributes(df$bap87)$label, "Current Health")
  expect_equal(attributes(df$bap87)$label_de, "Gesundheitszustand gegenwärtig ")
  expect_equal(attributes(df$bap87)$label_en, "Current Health")
  expect_equal(attributes(df$bap87)$description_de, "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?")
  expect_equal(attributes(df$bap87)$description_en, "Question: How would you describe your current health?")
  expect_equal(attributes(df$bap87)$type, "numeric")
  expect_equal(attributes(df$bap87)$url, "https://paneldata.org/soep-core/data/bap/bap87")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(attributes(df$name)$url, "")
  # - categories attributes
  expect_equal(names(attributes(df$bap87)$labels_en), c(
    "Does not apply",
    "No Answer",
    "Very good",
    "Good",
    "Satisfactory",
    "Poor",
    "Bad"
  ))
  expect_equal(as.character(unname(attributes(df$bap87)$labels_en)), c(
    "-2",
    "-1",
    "1",
    "2",
    "3",
    "4",
    "5"
  ))
})
# testthat::test_file("tests/testthat/test-csv2r.R")
# covr::package_coverage()
