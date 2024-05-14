
#' write_opendf and read_opendf: default setting
test_that("read_write_opendf_default_setting", {
  # - get data
  df <- read_opendf(
    file = "testdata/data.zip",
    languages = "all"
  )
  # make xml and data
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf(paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))

  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})

#' write_opendf and read_opendf: with languages="en"
test_that("read_write_opendf_language_de", {
  # - get data
  df <- read_opendf(
    file = "testdata/data.zip",
    languages = "de"
  )
  # make xml and data
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))

  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})


#' write_opendf and read_opendf: with default language
test_that("read_write_opendf_with_default_language", {
  # - get data
  df <- read_opendf(
    file = "testdata/data_with_default.zip",
    languages = "all"
  )
  # make xml and data
  write_opendf(
    x = df,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  df_copy<-read_opendf(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  expect_true(all.equal(df,df_copy))
  
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})

#' write_opendf and read_opendf: with auto
test_that("read_write_opendf_with_default_language", {
  # - get data

  data(Auto)
  # make xml and data
  write_opendf(
    x = Auto,
    file = paste0(tempdir(),"/MY_XML.zip")
  )
  
  Auto_copy<-read_opendf(file=paste0(tempdir(),"/MY_XML.zip"))
  # -- test if objects are equal
  write_opendf(
    x = Auto_copy,
    file = paste0(tempdir(),"/MY_XML2.zip")
  )
  
  Auto_copy2<-read_opendf(file=paste0(tempdir(),"/MY_XML2.zip"))
  
  expect_true(all.equal(Auto_copy,Auto_copy2))
  
  unlink(paste0(tempdir(),"/*"), recursive = TRUE)
})

