#' write_opendf: default setting: languages = "all", variables = "yes", export_data = "yes"
test_that("write_opendf_default_setting", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML")
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' write_opendf test export_data argument
test_that("write_opendf_export_data", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with export_datat = yes
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    export_data = "yes"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # make xml and data with export_datat = no
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    export_data = "no" # changed
  )
  # -- test if file exists
  expect_false(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' write_opendf test variables argument
test_that("write_opendf_variables", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with variables = no
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    variables = "no"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' write_opendf test languages argument
test_that("write_opendf_languages", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with languages = default
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "default"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # make xml and data with languages = de
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "de"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  # make xml and data with languages = notvalid
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "notvalid"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# testthat::test_file("tests/testthat/test-write_opendf.R")
# covr::package_coverage()

