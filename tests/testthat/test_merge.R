


#test merging
test_that("merge.opendf", {
  #' - get data
  df1 <- get(load("testdata/data_odf.RData"))[,1:4]
  df2 <- get(load("testdata/data_odf.RData"))[,4:7]
  df3 <- get(load("testdata/data_odf.RData"))[,5:7]
  df4 <- get(load("testdata/data_odf.RData"))[,2:4]
  df5 <- get(load("testdata/data_odf.RData"))[,3:5]
  df6 <- get(load("testdata/data_odf.RData"))[,6:7]
  df7 <- get(load("testdata/data_odf.RData"))
  
  
  df1$id = 1:20
  df2$id = 1:20
  df3$id = c(1:10, 1:10)
  df4$id = c(1:10, 1:10)
  df5$id = c(1:10, 1:10)
  df6$id = c(1:10, 1:10)
  df7$id = 6:25
  df1$id2 = 16:35
  df2$id2 = 16:35
  df3$id2 = c(1:10, 1:10)
  df4$id2 = c(11:20, 1:10)
  df5$id2 = c(1:10, 11:20)
  df6$id2 = c(1:10, 1:10)
  df7$id2 = 6:25
  
  
  df_out<-merge
  expect_equal("T","T")
}












)

