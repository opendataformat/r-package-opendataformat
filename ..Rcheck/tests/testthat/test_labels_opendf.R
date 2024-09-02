
#test for data frame input
test_that("docu_odf", {
  #' - get data
  df <- get(load("testdata/data_odf.RData"))

  
  expect_equal(as.character(labels_odf(df)), c("Current Health", "hours of sleep, normal workday", "Pressed For Time Last 4 Weeks", 
                                    "Run-down, Melancholy Last 4 Weeks","Well-balanced Last 4 Weeks","Height","Firstname"))
  expect_equal(names(labels_odf(df)), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  #set language="en"
  expect_equal(as.character(labels_odf(df, language="en")), c("Current Health", "hours of sleep, normal workday", "Pressed For Time Last 4 Weeks", 
                                                  "Run-down, Melancholy Last 4 Weeks","Well-balanced Last 4 Weeks","Height","Firstname"))
  expect_equal(names(labels_odf(df, language="en")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  #set language="de"
  expect_equal(as.character(labels_odf(df, language="de")), c("Gesundheitszustand gegenwärtig", "Stunden Schlaf, normaler Werktag", 
                                                                 "Eile, Zeitdruck letzten 4 Wochen", "Niedergeschlagen letzten 4 Wochen",
                                                                 "Ausgeglichen letzten 4 Wochen", "Körpergröße", "Vorname"))
  expect_equal(names(labels_odf(df, language="de")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  #set valuelabels=T
  expect_equal(as.character(labels_odf(df, language="de", valuelabels = T)), c("Gesundheitszustand gegenwärtig", "Stunden Schlaf, normaler Werktag", 
                                                                 "Eile, Zeitdruck letzten 4 Wochen", "Niedergeschlagen letzten 4 Wochen",
                                                                 "Ausgeglichen letzten 4 Wochen", "Körpergröße", "Vorname"))
  expect_equal(names(labels_odf(df, language="de", valuelabels = T)), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))

  
  #set valuelabels=T
  expect_equal(as.character(labels_odf(df, language="de", valuelabels = T, retrieve="description")), 
               c(
                 "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?",
                 "Schlafstunden pro Wochentag" ,
                 "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen",
                 "Häufigkeit der Niedergeschlagenheit",
                 "Häufigkeit der Ausgeglichenheit",
                 "Körpergröße",
                 "Vorname")
               )
  expect_equal(names(labels_odf(df, language="de", valuelabels = T, retrieve="url")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  
  #set retrieve to "description"
  expect_equal(as.character(labels_odf(df, language="de", retrieve="description")), 
               c(
                 "Frage: Wie würden Sie Ihren gegenwärtigen Gesundheitszustand beschreiben?", "Schlafstunden pro Wochentag",
                 "Häufigkeit des Gefühls von Zeitdruck in den letzten 4 Wochen","Häufigkeit der Niedergeschlagenheit", 
                 "Häufigkeit der Ausgeglichenheit", "Körpergröße", "Vorname")
               )
  expect_equal(names(labels_odf(df, language="de", retrieve="description")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  
  #set retrieve to "url"
  expect_equal(as.character(labels_odf(df, language="de", retrieve="url")), 
               c("https://paneldata.org/soep-core/data/bap/bap87", "https://paneldata.org/soep-core/data/bap/bap9201",
                 "https://paneldata.org/soep-core/data/bap/bap9001", "https://paneldata.org/soep-core/data/bap/bap9002", 
                 "https://paneldata.org/soep-core/data/bap/bap9003", "https://paneldata.org/soep-core/data/bap/bap96",""
               ))
  expect_equal(names(labels_odf(df, language="de", retrieve="url")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  #set retrieve to "type"
  expect_equal(as.character(labels_odf(df, language="de", retrieve="type")), 
               c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character")
               )
  expect_equal(names(labels_odf(df, language="de", retrieve="type")), c("bap87", "bap9201", "bap9001", "bap9002", "bap9003", "bap96", "name"))
  
  
  
  
})


#test for variable input
test_that("docu_odf", {
  #' - get data
  df <- get(load("testdata/data_odf.RData"))
  
  
  expect_equal(as.character(labels_odf(df$bap87)), "Current Health")
  expect_equal(names(labels_odf(df$bap87)), "bap87")
  #change language
  expect_equal(as.character(labels_odf(df$bap87, language="en")), "Current Health")
  expect_equal(names(labels_odf(df$bap87, language="en")), "bap87")
  
  expect_equal(as.character(labels_odf(df$bap9001, language="de")), "Eile, Zeitdruck letzten 4 Wochen")
  expect_equal(names(labels_odf(df$bap9001, language="de")), "bap9001")
  
  #set valuelabels=T
  expect_equal(as.character(labels_odf(df$bap9001)), "Pressed For Time Last 4 Weeks")
  expect_equal(names(labels_odf(df$bap9001)), "bap9001")
  
  
  #set language="en"
  expect_equal(as.character(labels_odf(df$bap9201, language="en")), "hours of sleep, normal workday")
  expect_equal(names(labels_odf(df$bap9201, language="en")), "bap9201")
  #set language="de"
  expect_equal(as.character(labels_odf(df$bap9201, language="de")), "Stunden Schlaf, normaler Werktag")
  expect_equal(names(labels_odf(df$bap9201, language="de")), "bap9201")
  
  #set valuelabels=T
  expect_equal(as.numeric(labels_odf(df$bap9001, language="de", valuelabels = T)), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="de", valuelabels = T)), c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal", "Fast nie", "Nie"))

  expect_equal(as.numeric(labels_odf(df$bap9001, language="en", valuelabels = T)), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="en", valuelabels = T)), c("Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never", "Never"))
  
  #set valuelabels=T
  expect_equal(as.numeric(labels_odf(df$bap9001, language="de", retrieve="valuelabels")), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="de", retrieve="valuelabels")), c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal", "Fast nie", "Nie"))
  
  expect_equal(as.numeric(labels_odf(df$bap9001, language="en", retrieve="valuelabels")), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="en", retrieve="valuelabels")), c("Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never", "Never"))
  
  
  #set valuelabels=T
  expect_equal(as.numeric(labels_odf(df$bap9001, language="de", valuelabels = T, retrieve="url")), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="de", valuelabels = T, retrieve="url")), c("trifft nicht zu", "keine Angabe", "Immer", "Oft", "Manchmal", "Fast nie", "Nie"))

  expect_equal(as.numeric(labels_odf(df$bap9001, language="en", valuelabels = T, retrieve="description")), c(-2, -1, 1, 2, 3, 4, 5))
  expect_equal(names(labels_odf(df$bap9001, language="en", valuelabels = T, retrieve="description")), c("Does not apply", "No Answer", "Always", "Often", "Sometimes", "Almost Never", "Never"))
  
  
  #set retrieve to "url"
  expect_equal(as.character(labels_odf(df$bap9001, language="de",  retrieve="url")), "https://paneldata.org/soep-core/data/bap/bap9001")
  expect_equal(names(labels_odf(df$bap9001, language="de", retrieve="url")), "bap9001")
  
  
  #set retrieve to "description"
  expect_equal(as.character(labels_odf(df$bap9001, language="en", retrieve="description")), "Frequency of feeling time pressure in the past 4 weeks")
  expect_equal(names(labels_odf(df$bap9001, language="en", retrieve="description")), "bap9001")
  
  #set retrieve to "type"
  expect_equal(as.character(labels_odf(df$bap9001, language="en", retrieve="type")), "numeric")
  expect_equal(names(labels_odf(df$bap9001, language="en", retrieve="type")), "bap9001")

  
})

