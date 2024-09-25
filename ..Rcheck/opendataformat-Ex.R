pkgname <- "opendataformat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "opendataformat-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('opendataformat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("docu_odf")
### * docu_odf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: docu_odf
### Title: Get documentation from R data frame.
### Aliases: docu_odf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_odf"))

# view documentation about the dataset in the language that is currently set
## Not run: 
##D docu_odf(df)
## End(Not run)

# view information from a selected variable in language "en"
## Not run: 
##D docu_odf(df$bap87, languages = "en")
## End(Not run)

# view dataset information for all available languages
## Not run: 
##D docu_odf(df, languages = "all")
## End(Not run)

# print information to the R console
## Not run: 
##D docu_odf(df$bap87, style = "print")
## End(Not run)

# print information to the R viewer
## Not run: 
##D docu_odf(df$bap87, style = "viewer")
## End(Not run)

# Since the label for language de is missing, in this case the 
# english label will be displayed additionally.
## Not run: 
##D attributes(df$bap87)["label_de"]<-""
##D docu_odf(df$bap87, languages="de", style = "console", replace_missing=T)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("docu_odf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("labels_odf")
### * labels_odf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: labels_odf
### Title: Get variable labels or other metadata from a data frame in
###   opendataformat.
### Aliases: labels_odf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_odf"))
# view the variable labels for all variables in English
labels_odf(input = df, language = "en", valuelabels = FALSE)

# view the value labels for variable bap87 in English
labels_odf(input = df$bap87, language = "en", valuelabels = FALSE)

# view the description for variable bap87 in English
labels_odf(input = df$bap87, language = "en", retrieve = "description")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("labels_odf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_odf")
### * read_odf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_odf
### Title: Read data specified as Open Data Format.
### Aliases: read_odf

### ** Examples

# get path to example data from the opendataformat package (data.zip)
path <- system.file("extdata", "data.zip", package="opendataformat")
path

# read example data specified as Open Data Format from ZIP file
df <- read_odf(file = path)
df
attributes(df)
attributes(df$bap87)

# read example data with language selection
df <- read_odf(file = path, languages = "de")
attributes(df$bap87)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_odf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setlanguage_odf")
### * setlanguage_odf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setlanguage_odf
### Title: Change language of dataframe metadata
### Aliases: setlanguage_odf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_odf"))

# Switch dataset df to language "en" 
df <- setlanguage_odf(df, language = "en")

# Display dataset information for dataset df in language "en"
docu_odf(df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setlanguage_odf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_odf")
### * write_odf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_odf
### Title: Write R data frame to the Open Data Format.
### Aliases: write_odf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_odf"))
df

# write R data frame with attributes to the file my_data.zip specified
# as Open Data Format.
## Not run: 
##D write_odf(x = df, file = "my_data.zip")
## End(Not run)

# write variable from R data frame with attributes to the file my_data.zip 
# specified as Open Data Format.
## Not run: 
##D write_odf(x = df$bap87,  file = "my_data.zip")
## End(Not run)

# write R data frame with attributes to the file my_data.zip
# with selected language.
## Not run: 
##D write_odf(x = df,  file = "my_data.zip", languages = "en")
## End(Not run)

# write R data frame with attributes to the file my_data.zip but only
# metadata, no data.
## Not run: 
##D write_odf(x = df,  file = "my_data.zip", export_data = "no")
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_odf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
