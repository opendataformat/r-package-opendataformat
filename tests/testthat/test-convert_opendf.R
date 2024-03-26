# convert_opendf_r2csv:
test_that("convert_opendf_r2csv", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- data set input
  # --- test 1:
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- test 2:
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "no" # CHANGED
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv"))) # CHANGED
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- test 3:
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "no", # CHANGED
    export_data = "yes"
  )
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_false(file.exists(paste0(tempdir(),"/variables.csv"))) # CHANGED
  expect_false(file.exists(paste0(tempdir(),"/categories.csv"))) # CHANGED
  unlink(paste0(tempdir(),"/*"))
  # -- variable input
  # --- test 1:
  convert_opendf(
    format = "r2csv",
    input = df$bap87, # CHANGED
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv"))) # CHANGED
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- test 2:
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "no" # CHANGED
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv"))) # CHANGED
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv"))) # CHANGED
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- test 3:
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "all",
    variables = "no", # CHANGED
    export_data = "yes"
  )
  expect_true(file.exists(paste0(tempdir(),"/data.csv"))) # CHANGED
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
})
#' convert_opendf_r2csv_lang: test language options
test_that("convert_opendf_r2csv_lang", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test "all":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label[1], NULL)
  expect_equal(df_variables$label_en[1], "Current Health")
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label[1], NULL)
  expect_equal(df_variables$label_en[1], "Current Health")
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # -- test "default":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "description_en",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_en[1], "Current Health")
  expect_equal(df_variables$label_de[1], NULL)
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "description_en",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_en, "Current Health")
  expect_equal(df_variables$label_de, NULL)
  unlink(paste0(tempdir(),"/*"))
  # -- test "de":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "de",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  expect_equal(df_variables[["label"]], NULL)
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "de",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables[["label_de"]], "Gesundheitszustand gegenwärtig")
  expect_equal(df_variables[["label"]], NULL)
  unlink(paste0(tempdir(),"/*"))
  # -- test messages: everything okay
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "de",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "all",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "default",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  # -- test message: no valid language
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "",
      variables = "yes",
      export_data = "yes"
    ),
    "Your language ")
})
#' convert_opendf_csv2r
test_that("convert_opendf_csv2r", {
  df <- convert_opendf(
    format = "csv2r",
    input = "testdata/csv"
  )
  # - dataset column names
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
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  # - variable attributes
  expect_equal(attributes(df$bap87)$name, "bap87")
  expect_equal(attributes(df$bap87)$label_de, "Gesundheitszustand gegenwärtig ")
  expect_equal(attributes(df$bap87)$label_en, "Current Health")
  expect_equal(attributes(df$bap87)$description, NULL)
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
#' convert_opendf_xml2csv: all language
test_that("convert_opendf_xml2csv_all", {
  # make data
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data.zip",
    output = tempdir(),
    languages = "all"
  )
  # check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # get csv files and check column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "url"
  ))
})
#' convert_opendf_r2csv_lang: test language options
test_that("convert_opendf_r2csv_lang", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  df_with_default<- get(load("testdata/data_odf_with_default.RData"))
    # -- test "all":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_en[1], "Current Health")
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_en, "Current Health")
  expect_equal(df_variables$label_de, "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # -- test "default":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df_with_default,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "type",
    "url",
    "label",
    "description"
  ))
  expect_equal(df_variables$label[1], "Current Health")
  expect_equal(df_variables$label_de[1], NULL)
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df_with_default$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "type",
    "url",
    "label",
    "description"
  ))
  expect_equal(df_variables$label, "Current Health")
  expect_equal(df_variables$label_de, NULL)
  unlink(paste0(tempdir(),"/*"))
  # -- test "de":
  # --- data set input
  convert_opendf(
    format = "r2csv",
    input = df,
    output = tempdir(),
    languages = "de",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  expect_equal(df_variables[["label"]], NULL)
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  convert_opendf(
    format = "r2csv",
    input = df$bap87,
    output = tempdir(),
    languages = "de",
    variables = "yes",
    export_data = "yes"
  )
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables[["label_de"]], "Gesundheitszustand gegenwärtig")
  expect_equal(df_variables[["label"]], NULL)
  unlink(paste0(tempdir(),"/*"))
  # -- test messages: everything okay
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "de",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "all",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "default",
      variables = "yes",
      export_data = "yes"
    ),
    "Your CSV files are stored within the directory:")
  # -- test message: no valid language
  expect_message(
    convert_opendf(
      format = "r2csv",
      input = df,
      output = tempdir(),
      languages = "",
      variables = "yes",
      export_data = "yes"
    ),
    "Your language ")
})
#' convert_opendf_csv2r
test_that("convert_opendf_csv2r", {
  df <- convert_opendf(
    format = "csv2r",
    input = "testdata/csv"
  )
  # - dataset column names
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
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf ")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  # - variable attributes
  expect_equal(attributes(df$bap87)$name, "bap87")
  expect_equal(attributes(df$bap87)$label_de, "Gesundheitszustand gegenwärtig ")
  expect_equal(attributes(df$bap87)$label_en, "Current Health")
  expect_equal(attributes(df$bap87)$description, NULL)
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
#' convert_opendf_xml2csv_standard_xml: all languages
test_that("convert_opendf_xml2csv_all_standard_xml", {
  # - xml without default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data.zip",
    output = tempdir(),
    languages = "all"
  )
  # -- check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # -- dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "url"
  ))
  # -- dataset.csv content
  expect_equal(dataset$dataset, "bap")
  expect_equal(dataset$label_en, "Data from individual questionnaires 2010")
  expect_equal(dataset$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(dataset$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$url, "https://paneldata.org/soep-core/data/bap")
  # -- variables.csv column names
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "label_en",
    "label_de",
    "type",
    "description_en",
    "description_de",
    "url"
  ))
  # -- variables.csv content
  expect_equal(variables$variable, c("bap87","bap9201","bap9001","bap9002",
                                     "bap9003","bap96","name"))
  expect_equal(variables$label_en, c(
    "Current Health",
    "hours of sleep, normal workday",
    "Pressed For Time Last 4 Weeks",
    "Run-down, Melancholy Last 4 Weeks",
    "Well-balanced Last 4 Weeks",
    "Height","Firstname"))
  expect_equal(variables$label_de, c(
    "Gesundheitszustand gegenwärtig",
    "Stunden Schlaf, normaler Werktag",
    "Eile, Zeitdruck letzten 4 Wochen",
    "Niedergeschlagen letzten 4 Wochen",
    "Ausgeglichen letzten 4 Wochen",
    "Körpergröße",
    "Vorname"))
  expect_equal(variables$type, c("numeric","numeric","numeric","numeric",
                                 "numeric","numeric","character"))
  expect_equal(variables$description_en, c(
    "Question: How would you describe your current health?",
    "Sleep hours per weekday",
    "Frequency of feeling time pressure in the past 4 weeks",
    "Frequency of feeling a sad and depressed state",
    "Frequency of feeling balance",
    "Body size",
    "Firstname"))
  expect_equal(variables$description_de, c(
    "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
    "Schlafstunden pro Wochentag",
    "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen",
    "Häufigkeit der Niedergeschlagenheit",
    "Häufigkeit der Ausgeglichenheit",
    "Körpergröße",
    "Vorname"))
  expect_equal(variables$url, c(
    "https://paneldata.org/soep-core/data/bap/bap87",
    "https://paneldata.org/soep-core/data/bap/bap9201",
    "https://paneldata.org/soep-core/data/bap/bap9001",
    "https://paneldata.org/soep-core/data/bap/bap9002",
    "https://paneldata.org/soep-core/data/bap/bap9003",
    "https://paneldata.org/soep-core/data/bap/bap96",
    "" ))
  # -- categories.csv column names
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c("variable","value","label_en","label_de"))
  # -- categories.csv content
  expect_equal(categories$variable, c(
    rep("bap87", 7),
    rep("bap9201", 2),
    rep("bap9001", 7),
    rep("bap9002", 7),
    rep("bap9003", 7),
    rep("bap96", 2),
    rep("name", 2)
    ))
  expect_equal(categories$value, c(
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1)
  ))
  expect_equal(categories$label_en[1:2], c("Does not apply","No Answer"))
  expect_equal(categories$label_de[1:2], c("trifft nicht zu","keine Angabe"))

  expect_equal(categories$label_en[8:10], c("Does not apply","No Answer","Does not apply"))
  expect_equal(categories$label_de[8:10], c("trifft nicht zu","keine Angabe","trifft nicht zu"))
})
#' convert_opendf_xml2csv_default_xml: all languages
test_that("convert_opendf_xml2csv_all_default_xml", {
  # - xml with default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data_with_default.zip",
    output = tempdir(),
    languages = "all"
  )
  # -- check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # -- dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "url"
  ))
  # -- dataset.csv content
  expect_equal(dataset$dataset, "bap")
  expect_equal(dataset$label, "Data from individual questionnaires 2010")
  expect_equal(dataset$label_en, "Data from individual questionnaires 2010")
  expect_equal(dataset$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(dataset$description, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$url, "https://paneldata.org/soep-core/data/bap")
  # -- variables.csv column names
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "label",
    "label_en",
    "label_de",
    "type",
    "description",
    "description_en",
    "description_de",
    "url")
    )
  # -- variables.csv content
  expect_equal(variables$variable, c("bap87","bap9201","bap9001","bap9002",
                                     "bap9003","bap96","name"))
  expect_equal(variables$label[1:2], c(
    "Current Health",
    ""
    ))
  expect_equal(variables$label_en[1:2], c(
    "Current Health",
    "hours of sleep, normal workday"
  ))
  expect_equal(variables$description[1:2], c(
    "Question: How would you describe your current health?",
    "")
    )
  expect_equal(variables$description_de[1:2], c(
    "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
    "Schlafstunden pro Wochentag"
  ))
  expect_equal(variables$type[4:6], c("numeric", "numeric", "numeric"))
  expect_equal(variables$url[3:5], c(
    "https://paneldata.org/soep-core/data/bap/bap9001",
    "https://paneldata.org/soep-core/data/bap/bap9002",
    "https://paneldata.org/soep-core/data/bap/bap9003"
  ))
  # categories.csv column name
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c(
    "variable",
    "value",
    "label",
    "label_en",
    "label_de"
    ))
  # categories.csv content
  expect_equal(categories$variable,  c(
    rep("bap87", 7),
    rep("bap9201", 2),
    rep("bap9001", 7),
    rep("bap9002", 7),
    rep("bap9003", 7),
    rep("bap96", 2),
    rep("name", 2)
  ))
  expect_equal(categories$value, c(
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1)
  ))
  expect_equal(categories$label[1:8], c(
    "Does not apply",
    "No Answer",
    "Very good",
    "Good",
    "Satisfactory",
    "",
    "",
    ""
  ))
  expect_equal(categories$label_de[1:8], c(
    "trifft nicht zu",
    "keine Angabe",
    "Sehr gut",
    "Gut",
    "Zufriedenstellend",
    "Weniger gut",
    "Schlecht",
    "trifft nicht zu"
  ))
  expect_equal(categories$label_en[1:8], c(
    "Does not apply",
    "No Answer",
    "Very good",
    "Good",
    "Satisfactory",
    "Poor",
    "Bad",
    "Does not apply"
  ))
})
#' convert_opendf_xml2csv_standard_xml: default language
test_that("convert_opendf_xml2csv_default_standard_xml", {
  # xml without default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data.zip",
    output = tempdir(),
    languages = "default"
  )
  # check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "url"
  ))
  # -- dataset.csv content
  expect_equal(dataset$dataset, "bap")
  expect_equal(dataset$url, "https://paneldata.org/soep-core/data/bap")
  # -- variables.csv column names
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "type",
    "url"
  ))
  # -- variables.csv content
  expect_equal(variables$variable, c("bap87","bap9201","bap9001","bap9002",
                                     "bap9003","bap96","name"))
  expect_equal(variables$label[1:2], NULL)
  expect_equal(variables$label_en[1:2], NULL)
  expect_equal(variables$description[1:2], NULL)
  expect_equal(variables$description_de[1:2], NULL)
  expect_equal(variables$type[4:6], c("numeric", "numeric", "numeric"))
  expect_equal(variables$url[3:5], c(
    "https://paneldata.org/soep-core/data/bap/bap9001",
    "https://paneldata.org/soep-core/data/bap/bap9002",
    "https://paneldata.org/soep-core/data/bap/bap9003"
  ))
  # categories.csv column name
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c(
    "variable",
    "value"
  ))
  expect_equal(categories$value, c(
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1)
  ))
  expect_equal(categories$label[1:8], NULL)
  expect_equal(categories$label_de[1:8], NULL)
  expect_equal(categories$label_en[1:8], NULL)

})
#' convert_opendf_xml2csv_default_xml: default language
test_that("convert_opendf_xml2csv_default_xml_default", {
  # xml with default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data_with_default.zip",
    output = tempdir(),
    languages = "default"
  )
  # check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label",
    "description",
    "url"
  ))
  # -- dataset.csv content
  expect_equal(dataset$dataset, "bap")
  expect_equal(dataset$label, "Data from individual questionnaires 2010")
  expect_equal(dataset$label_de, NULL)
  expect_equal(dataset$description, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$description_de, NULL)
  expect_equal(dataset$url, "https://paneldata.org/soep-core/data/bap")
  # -- variables.csv column names
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "label",
    "type",
    "description",
    "url"
  ))
  # -- variables.csv content
  expect_equal(variables$variable, c("bap87","bap9201","bap9001","bap9002",
                                     "bap9003","bap96","name"))
  expect_equal(variables$label[1:2], c(
    "Current Health",
    ""
  ))
  expect_equal(variables$label_en[1:2], NULL)
  expect_equal(variables$description[1:2], c(
    "Question: How would you describe your current health?",
    "")
  )
  expect_equal(variables$description_de[1:2], NULL)
  expect_equal(variables$type[4:6], c("numeric", "numeric", "numeric"))
  expect_equal(variables$url[3:5], c(
    "https://paneldata.org/soep-core/data/bap/bap9001",
    "https://paneldata.org/soep-core/data/bap/bap9002",
    "https://paneldata.org/soep-core/data/bap/bap9003"
  ))
  # -- categories.csv column name
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c(
    "variable",
    "value",
    "label"
  ))
  # -- categories content
  expect_equal(categories$variable,  c(
    rep("bap87", 7),
    rep("bap9201", 2),
    rep("bap9001", 7),
    rep("bap9002", 7),
    rep("bap9003", 7),
    rep("bap96", 2),
    rep("name", 2)
  ))
  expect_equal(categories$value, c(
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1)
  ))
  expect_equal(categories$label[1:8], c(
    "Does not apply",
    "No Answer",
    "Very good",
    "Good",
    "Satisfactory",
    "",
    "",
    ""
  ))
  expect_equal(categories$label_de[1:8], NULL)
  expect_equal(categories$label_en[1:8], NULL)
})
#' convert_opendf_xml2csv_standard_xml: language selected
test_that("convert_opendf_xml2csv_lang_standard_xml", {
  # xml without default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data.zip",
    output = tempdir(),
    languages = "de"
  )
  # check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label_de",
    "description_de",
    "url"
  ))
  # -- dataset.csv content
  expect_equal(dataset$dataset, "bap")
  expect_equal(dataset$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(dataset$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(dataset$url, "https://paneldata.org/soep-core/data/bap")
  # -- variables.csv column names
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "label_de",
    "type",
    "description_de",
    "url"
  ))
  # -- variables.csv content
  expect_equal(variables$variable, c("bap87","bap9201","bap9001","bap9002",
                                     "bap9003","bap96","name"))
  expect_equal(variables$label_en, NULL)
  expect_equal(variables$label_de, c(
    "Gesundheitszustand gegenwärtig",
    "Stunden Schlaf, normaler Werktag",
    "Eile, Zeitdruck letzten 4 Wochen",
    "Niedergeschlagen letzten 4 Wochen",
    "Ausgeglichen letzten 4 Wochen",
    "Körpergröße",
    "Vorname"))
  expect_equal(variables$type, c("numeric","numeric","numeric","numeric",
                                 "numeric","numeric","character"))
  expect_equal(variables$description_en, NULL)
  expect_equal(variables$description_de, c(
    "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
    "Schlafstunden pro Wochentag",
    "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen",
    "Häufigkeit der Niedergeschlagenheit",
    "Häufigkeit der Ausgeglichenheit",
    "Körpergröße",
    "Vorname"))
  expect_equal(variables$url, c(
    "https://paneldata.org/soep-core/data/bap/bap87",
    "https://paneldata.org/soep-core/data/bap/bap9201",
    "https://paneldata.org/soep-core/data/bap/bap9001",
    "https://paneldata.org/soep-core/data/bap/bap9002",
    "https://paneldata.org/soep-core/data/bap/bap9003",
    "https://paneldata.org/soep-core/data/bap/bap96",
    "" ))
  # -- categories.csv column names
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c("variable","value","label_de"))
  # -- categories.csv content
  expect_equal(categories$variable, c(
    rep("bap87", 7),
    rep("bap9201", 2),
    rep("bap9001", 7),
    rep("bap9002", 7),
    rep("bap9003", 7),
    rep("bap96", 2),
    rep("name", 2)
  ))
  expect_equal(categories$value, c(
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1,1,2,3,4,5),
    c(-2,-1),
    c(-2,-1)
  ))
  expect_equal(categories$label_en[1:2], NULL)
  expect_equal(categories$label_de[1:2], c("trifft nicht zu","keine Angabe"))

  expect_equal(categories$label_en[8:10], NULL)
  expect_equal(categories$label_de[8:10], c("trifft nicht zu","keine Angabe","trifft nicht zu"))
})
#' convert_opendf_xml2csv_default_xml: language selected
test_that("convert_opendf_xml2csv_lang_default_xml", {
  # xml with default
  convert_opendf(
    format = "xml2csv",
    input = "testdata/data_with_default.zip",
    output = tempdir(),
    languages = "de"
  )
  # check if files are generates
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  # dataset.csv column names
  dataset <- read.csv(paste0(tempdir(),"/dataset.csv"), header = TRUE, sep = ",")
  expect_equal(names(dataset), c(
    "dataset",
    "label_de",
    "description_de",
    "url"
  ))
  variables <- read.csv(paste0(tempdir(),"/variables.csv"), header = TRUE, sep = ",")
  expect_equal(names(variables), c(
    "variable",
    "label_de",
    "type",
    "description_de",
    "url"
  ))
  categories <- read.csv(paste0(tempdir(),"/categories.csv"), header = TRUE, sep = ",")
  expect_equal(names(categories), c(
    "variable",
    "value",
    "label_de"
  ))
})
#' convert_opendf_xml2r_standard_xml: all languages
test_that("convert_opendf_xml2_all_standard_xml", {
  # xml without default
  df <- convert_opendf(
    format = "xml2r",
    input = "testdata/data.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "languages",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "url",
    "lang",
    "label",
    "class"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "label_de",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_en), c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_de), c("trifft nicht zu", "keine Angabe"))
})
#' convert_opendf_xml2r_default_xml: all languages
test_that("convert_opendf_xml2_all_default_xml", {
  df <- convert_opendf(
    format = "xml2r",
    input = "testdata/data_with_default.zip",
    languages = "all"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "languages",
    "label_default",
    "label_en",
    "label_de",
    "description_default",
    "description_en",
    "description_de",
    "url",
    "lang",
    "label",
    "class"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, "Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de, "Die Daten wurden im Rahmen der Studie SOEP-Core mittels des Fragebogens „Leben in Deutschland – Befragung 2010 zur sozialen Lage - Personenfragebogen für alle“ erhoben. Dieser Fragebogen richtet sich an die einzelnen Personen im Haushalt. Eine Ansicht des Erhebungsinstrumentes finden Sie hier: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_default",
    "label_en",
    "label_de",
    "description_default",
    "description_en",
    "description_de",
    "type",
    "url",
    "labels_default",
    "labels_en",
    "labels_de",
    "languages",
    "lang",
    "label"
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$description_de, "Körpergröße")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels), NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_en), c("Does not apply", "No Answer"))
  expect_equal(unname(attributes(df$bap96)$labels_de),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_de), c("trifft nicht zu", "keine Angabe"))
})
#' convert_opendf_xml2r_standard_xml: default languages
test_that("convert_opendf_xml2_default_default_xml", {
  df <- convert_opendf(
    format = "xml2r",
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
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, NULL)
  expect_equal(attributes(df)$description, NULL)
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "type",
    "url"
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, NULL)
  expect_equal(attributes(df$bap96)$description, NULL)
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),NULL)
  expect_equal(names(attributes(df$bap96)$labels), NULL)
})
#' convert_opendf_xml2r_default_xml: default languages
test_that("convert_opendf_xml2_default_standard_xml", {
  # xml without default
  df <- convert_opendf(
    format = "xml2r",
    input = "testdata/data_with_default.zip",
    languages = "default"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "languages",
    "label_default",
    "description_default",
    "url",
    "lang",
    "label",
    "class"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label_default, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description_default, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$languages, "default")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_default",
    "description_default",
    "type",
    "url",
    "labels_default",
    "languages",
    "lang",
    "label"
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label_default, "Height")
  expect_equal(attributes(df$bap96)$description_default, "Body size")
  expect_equal(attributes(df$bap96)$languages, "default")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels),c(-2,-1))
})
#' convert_opendf_xml2r_standard_xml: language selection
test_that("convert_opendf_xml2_lang_standard_xml", {
  # xml without default
  df <- convert_opendf(
    format = "xml2r",
    input = "testdata/data.zip",
    languages = "en"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "languages",
    "label_en",
    "description_en",
    "url",
    "lang",
    "label",
    "class"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "description_en",
    "type",
    "url",
    "labels_en",
    "languages",
    "lang",
    "label"
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels_de),NULL)
  expect_equal(unname(attributes(df$bap96)$labels_en),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_de), NULL)
  expect_equal(names(attributes(df$bap96)$labels_en), c("Does not apply", "No Answer"))
})
#' convert_opendf_xml2r_default_xml: language selection
test_that("convert_opendf_xml2_lang_default_xml", {
  df <- convert_opendf(
    format = "xml2r",
    input = "testdata/data_with_default.zip",
    languages = "en"
  )
  # - dataset attributes
  expect_equal(names(attributes(df)),c(
    "names",
    "row.names",
    "name",
    "languages",
    "label_en",
    "description_en",
    "url",
    "lang",
    "label",
    "class"
  ))
  # - dataset content
  expect_equal(attributes(df)$names, c("bap87","bap9201","bap9001","bap9002",
                                       "bap9003","bap96","name"))
  expect_equal(attributes(df)$name, "bap")
  expect_equal(attributes(df)$label, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_en, "Data from individual questionnaires 2010")
  expect_equal(attributes(df)$label_de, NULL)
  expect_equal(attributes(df)$description_en, "The data were collected as part of the SOEP-Core study using the questionnaire \"Living in Germany - Survey 2010 on the social situation - Personal questionnaire for all. This questionnaire is addressed to the individual persons in the household. A view of the survey instrument can be found here: https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.369781.de/soepfrabo_personen_2010.pdf")
  expect_equal(attributes(df)$description_de, NULL)
  expect_equal(attributes(df)$url, "https://paneldata.org/soep-core/data/bap")
  expect_equal(attributes(df)$class, c("data.frame", "opendf"))
  # - variable attributes
  expect_equal(names(attributes(df$bap87)), c(
    "name",
    "label_en",
    "description_en",
    "type",
    "url",
    "labels_en",
    "languages",
    "lang",
    "label" 
  ))
  # - variables content
  expect_equal(attributes(df$bap96)$name, "bap96")
  expect_equal(attributes(df$bap96)$label_de, NULL)
  expect_equal(attributes(df$bap96)$label_en, "Height")
  expect_equal(attributes(df$bap96)$label, "Height")
  expect_equal(attributes(df$bap96)$description_en, "Body size")
  expect_equal(attributes(df$bap96)$description_de, NULL)
  expect_equal(attributes(df$bap96)$url, "https://paneldata.org/soep-core/data/bap/bap96")
  expect_equal(unname(attributes(df$bap96)$labels_en),c(-2,-1))
  expect_equal(names(attributes(df$bap96)$labels_en), c("Does not apply", "No Answer"))
  expect_equal(names(attributes(df$bap96)$labels_de), NULL)
})
# convert_opendf_r2xml: test export_data,
# export_data = yes, variables = yes, languages = all
test_that("convert_opendf_r2xml_export_data", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # - make xml and data
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml and no data
   convert_opendf(
     format = "r2xml",
     input = df,
     output = paste0(tempdir(),"/MY_XML"),
     languages = "all",
     variables = "yes",
     export_data = "no" # changed
   )
   # -- test if file exists
   expect_false(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
   expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
   expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
   unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' convert_opendf_r2xml: test variables,
#' export_data = yes, variables = yes, languages = all
test_that("convert_opendf_r2xml_variables", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # - make xml with variables = yes
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with variables = no
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "no", # changed
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' convert_opendf_r2xml: test languages,
#' export_data = yes, variables = yes, languages = all
test_that("convert_opendf_r2xml_languages", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # - make xml with languages = default
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with languages = de
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "de", # changed
    variables = "yes",
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with languages = notvalid
  convert_opendf(
    format = "r2xml",
    input = df,
    output = paste0(tempdir(),"/MY_XML"),
    languages = "not", # changed
    variables = "yes",
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# convert_opendf_csv2xml: test export_data,
# export_data = yes, variables = yes, languages = all
test_that("convert_opendf_csv2xml_export_data", {
  # - make xml and data
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml and no data
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML2"),
    languages = "all",
    variables = "yes",
    export_data = "no" # changed
  )
  # -- test if file exists
  unzip(paste0(tempdir(),"/MY_XML2.zip"),exdir=paste0(tempdir(),"/MY_XML2"))
  expect_false(file.exists(paste0(tempdir(),"/MY_XML2/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML2/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML2.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# convert_opendf_csv2xml: test variables,
# export_data = yes, variables = yes, languages = all
test_that("convert_opendf_csv2xml_variables", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # - make xml with variables = yes
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "yes",
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with variables = no
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "all",
    variables = "no", # changed
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# convert_opendf_csv2xml: test languages,
# export_data = yes, variables = yes, languages = all
test_that("convert_opendf_csv2xml_languages", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # - make xml with languages = default
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with languages = de
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "de", # changed
    variables = "yes",
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # - make xml with languages = notvalid
  convert_opendf(
    format = "csv2xml",
    input = "testdata/csv",
    output = paste0(tempdir(),"/MY_XML"),
    languages = "not", # changed
    variables = "yes",
    export_data = "yes"
  )
  unzip(paste0(tempdir(),"/MY_XML.zip"),exdir=paste0(tempdir(),"/MY_XML"))
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# testthat::test_file("tests/testthat/test-convert_opendf.R")
# covr::package_coverage()
