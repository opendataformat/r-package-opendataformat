#' make_data: checks if data frame class is tibble and if so,
#' numeric values are converted to strings and then the dataframe is written
#' as CSV file
test_that("make_data", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test if class is data.frame
  expect_equal(class(df), "data.frame")
  expect_equal(class(make_data(df)$bap96), "numeric")
  expect_equal(class(make_data(df)$bap87), "integer")

  # # - manipulate data: make data frame tibble
  # library("tidyverse")
  # df_tib <- as_tibble(df)
  # # -- test
  # expect_equal(class(df_tib$bap96), "numeric")
  # # - manipulate data: make variable from class "haven labelled"
  # library("haven")
  # df_tib$bap87 <- labelled(
  #   df_tib$bap87, c(
  #     "Does not apply" = -2,
  #     "No Answer" = -1,
  #     "very good" = 1,
  #     "Good" = 2,
  #     "Satisfactory" = 3,
  #     "Poor" = 4,
  #     "Bad" = 5 )
  # )
  # expect_equal(class(df_tib$bap87)[1], "haven_labelled")
  # # - test conversion from class "numeric" to "character"
  # expect_equal(class(make_data(df_tib)$bap96), "character")
  # # - test conversion from class "haven_labelled" to "character"
  # expect_equal(class(make_data(df_tib)$bap87), "character")
})
#' write_odf_csv: checks if output dir exists, and writes rdata to CSV in dir
test_that("write_odf_csv", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test
  write_odf_csv(df, "/MY_CSV.csv", tempdir())
  expect_true(file.exists(paste0(tempdir(),"/MY_CSV.csv")))
  unlink(paste0(tempdir(),"/*"))
})
#' r2csv: test argument: export_data = "yes" | "no"
test_that("r2csv_export_data", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test: export_data = "yes"
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
    )
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "yes"
  )
  expect_true(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv"))) # FALSE
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # -- test: export_data = "no"
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))

})
#' r2csv: test argument: variables = "yes" | "no"
test_that("r2csv_variables", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # -- test: variables = "yes"
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # -- test: variables = "no"
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "no",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_false(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_false(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "no", #  is ignored due to variable input
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # -- test: variables = "yes"
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  expect_true(file.exists(paste0(tempdir(),"/categories.csv")))
  unlink(paste0(tempdir(),"/*"))
})
#' r2csv: test argument: languages = "all"
test_that("r2csv_languages_all",{
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "all",
    variables = "yes",
    export_data = "no"
  )
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_dataset <- read.csv(file = paste0(tempdir(),"/dataset.csv"))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_dataset)$names, c(
    "dataset",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "url"
  ))
  expect_equal(df_dataset$label,"Data from individual questionnaires 2010")
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "all",
    variables = "no",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label",
    "label_en",
    "label_de",
    "description",
    "description_en",
    "description_de",
    "type",
    "url")
    )
  unlink(paste0(tempdir(),"/*"))
  expect_equal(df_variables$label_de, "Gesundheitszustand gegenwärtig")
  expect_equal(df_variables$label_en, "Current Health")
})
#' r2csv: test argument: languages = "default"
test_that("r2csv_languages_default",{
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "default",
    variables = "yes",
    export_data = "no"
  )
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_dataset <- read.csv(file = paste0(tempdir(),"/dataset.csv"))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_dataset)$names, c(
    "dataset",
    "label",
    "description",
    "url"
  ))
  expect_equal(df_dataset$label,"Data from individual questionnaires 2010")
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label",
    "description",
    "type",
    "url"
  ))
  expect_equal(df_variables$label[1], "Current Health")
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "default",
    variables = "no",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label",
    "description",
    "type",
    "url")
  )
  unlink(paste0(tempdir(),"/*"))
  expect_equal(df_variables$label_en, NULL)
  expect_equal(df_variables$label, "Current Health")
  unlink(paste0(tempdir(),"/*"))
})
#' r2csv: test argument: languages = "de" | "en"
test_that("r2csv_languages_code",{
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # --- data set input
  r2csv(
    input = df,
    output = tempdir(),
    languages = "de",
    variables = "yes",
    export_data = "no"
  )
  expect_true(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_dataset <- read.csv(file = paste0(tempdir(),"/dataset.csv"))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_dataset)$names, c(
    "dataset",
    "label_de",
    "description_de",
    "url"
  ))
  expect_equal(df_dataset$label_de,"Daten vom Personenfragebogen 2010")
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_de",
    "description_de",
    "type",
    "url"
  ))
  expect_equal(df_variables$label_de[1], "Gesundheitszustand gegenwärtig")
  unlink(paste0(tempdir(),"/*"))
  # --- variable input
  r2csv(
    input = df$bap87,
    output = tempdir(),
    languages = "en",
    variables = "no",
    export_data = "no"
  )
  expect_false(file.exists(paste0(tempdir(),"/dataset.csv")))
  expect_true(file.exists(paste0(tempdir(),"/variables.csv")))
  df_variables <- read.csv(file = paste0(tempdir(),"/variables.csv"))
  expect_equal(attributes(df_variables)$names, c(
    "variable",
    "label_en",
    "description_en",
    "type",
    "url")
  )
  expect_equal(df_variables$label_de, NULL)
  expect_equal(df_variables$label_en, "Current Health")
  unlink(paste0(tempdir(),"/*"))
})
#' r2csv: test argument: languages = no valid argument chosen
test_that("r2csv_languages_notvalid",{
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # --- data set input
  expect_message(
    r2csv(
      input = df,
      output = tempdir(),
      languages = "",
      variables = "no",
      export_data = "no"
    ), "Your language")
  expect_message(
    r2csv(
      input = df,
      output = tempdir(),
      languages = "deepkkwjehfd",
      variables = "no",
      export_data = "no"
    ), "Your language")
  expect_message(
    r2csv(
      input = df,
      output = tempdir(),
      languages = "de",
      variables = "no",
      export_data = "no"
    ), "Your CSV files are stored")
  # --- variable input
  expect_message(
    r2csv(
      input = df$bap87,
      output = tempdir(),
      languages = "",
      variables = "no",
      export_data = "no"
    ), "Your language")
  expect_message(
    r2csv(
      input = df$bap87,
      output = tempdir(),
      languages = "deepkkwjehfd",
      variables = "no",
      export_data = "no"
    ), "Your language")
  expect_message(
    r2csv(
      input = df$bap87,
      output = tempdir(),
      languages = "de",
      variables = "no",
      export_data = "no"
    ), "Your CSV files are stored")
})

# testthat::test_file("tests/testthat/test-r2csv.R")
# covr::package_coverage()
