
#' write_odf and read_odf: default setting
test_that("read_write_odf_default_setting", {
  # - get data
  df <- read_odf(
    file = "testdata/data.zip",
    languages = "all"
  )
  # make xml and data
  write_odf(
    x = df,
    file = paste0(tempdir(), "/MY_XML.zip")
  )

  df_copy <- read_odf(paste0(tempdir(), "/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df, df_copy))

  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})

#' write_odf and read_odf: with languages="en"
test_that("read_write_odf_language_de", {
  # - get data
  df <- read_odf(
    file = "testdata/data.zip",
    languages = "de"
  )
  # make xml and data
  write_odf(
    x = df,
    file = paste0(tempdir(), "/MY_XML.zip")
  )

  df_copy <- read_odf(file = paste0(tempdir(), "/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df, df_copy))

  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})


#' write_odf and read_odf: with default language
test_that("read_write_odf_with_default_language", {
  # - get data
  df <- read_odf(
    file = "testdata/data_with_default.zip",
    languages = "all"
  )
  # make xml and data
  write_odf(
    x = df,
    file = paste0(tempdir(), "/MY_XML.zip")
  )

  df_copy <- read_odf(file = paste0(tempdir(), "/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df, df_copy))

  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})

#' write_odf and read_odf: with extreme values: large numbers, small numbers 
#' and long strings
test_that("read_write_odf_with_extreme values", {
  # - get data
  df1 <- read_odf("testdata/data_special_values.zip")
  
  
  write_odf(x = df1, 
            file = paste0(tempdir(), "/df_with_extrem_values.zip")
            )
  
  df2 <- read_odf(paste0(tempdir(), "/df_with_extrem_values.zip"))
  
  
  expect_true(all.equal(df1, df2))
  expect_equal(sum(df1$large_vals), -216868392221803)
  expect_equal(sum(df2$large_vals), -216868392221803)
  expect_equal(sum(df1$floats), 3.9537558972732)
  expect_equal(sum(df2$floats), 3.9537558972732)
  expect_equal(sum(df1$large_vals),sum(df2$large_vals))
  expect_equal(sum(df1$floats), sum(df2$floats))
  expect_equal(df1$long_strings, df2$long_strings)
  
  # read data.csv as text and check if they are identical
  # Open a connection to the CSV files inside the ZIPs and read it
  con <- unz("testdata/data_special_values.zip", "data.csv")
  csv_lines1 <- readLines(con)
  close(con)
  con <- unz(paste0(tempdir(), "/df_with_extrem_values.zip"), "data.csv")
  csv_lines2 <- readLines(con)
  close(con)
  
  #check if CSV-files are identical
  expect_equal(csv_lines1, csv_lines2)
  
  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})

#' write_odf and read_odf: with auto
test_that("read_write_odf_with_auto", {
  # - get data
  library(ISLR)
  data(Auto)
  # make xml and data
  write_odf(
    x = Auto,
    file = paste0(tempdir(), "/MY_XML.zip")
  )

  auto_copy <- read_odf(file = paste0(tempdir(), "/MY_XML.zip"))
  # -- test if objects are equal
  write_odf(
    x = auto_copy,
    file = paste0(tempdir(), "/MY_XML2.zip")
  )

  auto_copy2 <- read_odf(file = paste0(tempdir(), "/MY_XML2.zip"))

  expect_true(all.equal(auto_copy, auto_copy2))

  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
})
