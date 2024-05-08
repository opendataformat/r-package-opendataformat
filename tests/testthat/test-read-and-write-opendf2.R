#' write_opendf2 and read_opendf2: default setting
test_that("read_write_opendf2_default_setting", {
  # - get data
  df <- read_opendf2(
    file = "testdata/data.zip",
    languages = "all"
  )
  # make xml and data
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf2(paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))

  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})

#' write_opendf2 and read_opendf2: with languages="en"
test_that("read_write_opendf2_language_de", {
  # - get data
  df <- read_opendf2(
    file = "testdata/data.zip",
    languages = "de"
  )
  # make xml and data
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf2(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))

  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})


#' write_opendf2 and read_opendf2: with default language
test_that("read_write_opendf2_with_default_language", {
  # - get data
  df <- read_opendf2(
    file = "testdata/data_with_default.zip",
    languages = "all"
  )
  # make xml and data
  write_opendf2(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf2(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))
  
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})

#' write_opendf2 and read_opendf2: with auto
test_that("read_write_opendf2_with_default_language", {
  # - get data
  library(ISLR)
  data(Auto)
  # make xml and data
  write_opendf2(
    x = Auto,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  Auto_copy<-read_opendf2(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  write_opendf2(
    x = Auto_copy,
    file = paste0(tempdir(),"/MY_XML2.zip")
  )
  
  Auto_copy2<-read_opendf2(file=paste0(tempdir(),"/MY_XML2.zip"))
  
  expect_true(all.equal(Auto_copy,Auto_copy2))
  
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})
