#' docu_opendf: test if documentation is generated: 'html'
test_that("make_docu_html", {
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
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "default"
  #' ----- variables = "no"
  docu_opendf(
    input = df,
    languages = "default",
    variables = "no",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df,
    languages = "default",
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "de"
  #' ----- variables = "no"
  docu_opendf(
    input = df,
    languages = "de",
    variables = "no",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df,
    languages = "de",
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: empty language selection
  expect_message(
    docu_opendf(
      input = df,
      languages = "",
      variables = "yes",
      style = "html"),
    "Your language selection is not valid."
    )
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: invalid language selection
  expect_message(
    docu_opendf(
      input = df,
      languages = "wdsqadf",
      variables = "yes",
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
    variables = "no",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "all",
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "default"
  #' ----- variables = "no"
  docu_opendf(
    input = df$bap87,
    languages = "default",
    variables = "no",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "default",
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ---- languages = "de"
  #' ----- variables = "no"
  docu_opendf(
    input = df$bap87,
    languages = "de",
    variables = "no",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ----- variables = "yes"
  docu_opendf(
    input = df$bap87,
    languages = "de",
    variables = "yes",
    style = "html")
  expect_true(file.exists(paste0(tempdir(),"/docu.html")))
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: empty language selection
  expect_message(
    docu_opendf(
      input = df,
      languages = "",
      variables = "yes",
      style = "html"),
    "Your language selection is not valid."
  )
  unlink(paste0(tempdir(),"/*"))
  #' ------ error message: invalid language selection
  expect_message(
    docu_opendf(
      input = df,
      languages = "",
      variables = "yes",
      style = "html"),
    "Your language selection is not valid."
  )
  unlink(paste0(tempdir(),"/*"))
})

# testthat::test_file("tests/testthat/test-docu_opendf.R")
# covr::package_coverage()
