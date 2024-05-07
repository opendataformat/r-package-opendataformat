#' write_opendf2: default setting: languages = "all", variable_metadata = "yes", export_data = "yes"
test_that("write_opendf2_default_setting", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML")
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
#' write_opendf2 test export_data argument
test_that("write_opendf2_export_data", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with export_datat = yes
  write_opendf2(
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
  write_opendf2(
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

#' write_opendf2 test languages argument
test_that("write_opendf2_languages", {
  # - get data
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with languages = default
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "en"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  df<-read_opendf2(paste0(tempdir(),"/MY_XML.zip"),
                   languages = "all",
                   nrows = 0)
  expect_equal(attr(df, "languages"), "en")
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with languages = de
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "de"
  )
  # -- test if file exists
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_true(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  df<-read_opendf2(paste0(tempdir(),"/MY_XML.zip"),
               languages = "all",
               nrows = 0)
  expect_equal(attr(df, "languages"), "de")
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
  df <- get(load("testdata/data_odf.RData"))
  # make xml and data with languages = notvalid
  expect_error(write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML"),
    languages = "notvalid"
  ) , "languages not valid")
  # -- test if file exists
  expect_false(file.exists(paste0(tempdir(),"/MY_XML/data.csv")))
  expect_false(file.exists(paste0(tempdir(),"/MY_XML/metadata.xml")))
  expect_false(file.exists(paste0(tempdir(),"/MY_XML.zip")))
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
# testthat::test_file("tests/testthat/test-write_opendf2.R")
# covr::package_coverage()

