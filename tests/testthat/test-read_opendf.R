#' read_opendf: all languages
test_that("read_opendf_all", {
  df <- read_opendf(
    input = "testdata/data.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "url",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels",
    "labels_en",
    "labels_de"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                         "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, "data.frame")
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, "Body size")
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels), c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_en),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_en), c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_de), c("trifft nicht zu", "keine Angabe"))
})
#' read_opendf: default languages
test_that("read_opendf_default", {
  df <- read_opendf(
    input = "testdata/data.zip",
    languages = "default"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "url",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "type",
    "url",
    "labels"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, "data.frame")
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),c(-2,-1))
})
#' read_opendf: "de" languages
test_that("read_opendf_de", {
  df <- read_opendf(
    input = "testdata/data.zip",
    languages = "de"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "label",
    "label_de",
    "description",
    "description_de",
    "url",
    "class"
  ))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label",
    "label_de",
    "description",
    "description_de",
    "type",
    "url",
    "labels",
    "labels_de"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$description, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, "data.frame")
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels), c("trifft nicht zu", "keine Angabe"))
  expect_equal(names(attributes(df$bap96)$labels_de), c("trifft nicht zu", "keine Angabe"))
})

# testthat::test_file("tests/testthat/test-read_opendf.R")
# covr::package_coverage()
