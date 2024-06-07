#' docu_opendf: test if documentation is generated: 'html'
test_that("docu_opendf", {
  #' - get data
  df <- get(load("testdata/data_odf.RData"))
  #' - test
  #' -- style = "html"
  #' --- input = df
  #' ---- languages = "all"
  #' ----- variables = "no"
  docu_opendf(
    input = df,
    languages = "all",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df,
    languages = "all",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "default"
  #' ----- variables = "no"
  docu_opendf(
    input = df,
    languages = "default",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df,
    languages = "default",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "de"
  #' ----- variables = "no"
  docu_opendf(
    input = df,
    languages = "de",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df,
    languages = "de",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: empty language selection
  expect_error(
    docu_opendf(
      input = df$test,
      style = "html"),
    "Input is not a dataframe or variable in the opendf-format."
    )
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: invalid language selection
  expect_error(
    docu_opendf(
      input = df,
      languages = "wdsqadf",
      style = "html"),
    "Your language selection is not valid."
  )
  unlink(paste0(tempdir(),"/*"))
  #' --- input = df$bap87
  #' ---- languages = "all"
  #' ----- variables = "no"
  docu_opendf(
    input = df$bap87,
    languages = "all",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "all",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "default"
  #' ----- variables = "no"
  docu_opendf(
    input = df$bap87,
    languages = "default",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "default",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "de"
  #' ----- variables = "no"
  docu_opendf(
    input = df$bap87,
    languages = "de",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "de",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: empty language selection
  expect_error(
    docu_opendf(
      input = df,
      languages = "",
      style = "html"),
    "Your language selection is not valid."
  )
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: invalid language selection
  expect_error(
    docu_opendf(
      input = df,
      languages = "",
      style = "html"),
    "Your language selection is not valid."
  )
  unlink(paste0(tempdir(),"/*"))
})

# testthat::test_file("tests/testthat/test-docu_opendf.R")
# covr::package_coverage()
