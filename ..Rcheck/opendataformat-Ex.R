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
nameEx("docu_opendf")
### * docu_opendf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: docu_opendf
### Title: Get documentation from R data frame.
### Aliases: docu_opendf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_opendf"))

# view documentation about the dataset in the language that is currently set
## Not run: 
##D docu_opendf(df)
## End(Not run)

# view information from a selected variable in language "en"
## Not run: 
##D docu_opendf(df$bap87, languages = "en")
## End(Not run)

# view dataset information for all available languages
## Not run: 
##D docu_opendf(df, languages = "all")
## End(Not run)

# print information to the R console
## Not run: 
##D docu_opendf(df$bap87, style = "print")
## End(Not run)

# print information to the R viewer
## Not run: 
##D docu_opendf(df$bap87, style = "viewer")
## End(Not run)

# Since the label for language de is missing, in this case the 
# english label will be displayed additionally.
## Not run: 
##D attributes(df$bap87)["label_de"]<-""
##D docu_opendf(df$bap87, languages="de", style = "console", replace_missing=T)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("docu_opendf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("labels_opendf")
### * labels_opendf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: labels_opendf
### Title: Get variable labels or other metadata from a data frame in
###   opendataformat.
### Aliases: labels_opendf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_opendf"))
# view the variable labels for all variables in English
labels_opendf(input = df, language = "en", valuelabels = FALSE)

# view the value labels for variable bap87 in English
labels_opendf(input = df$bap87, language = "en", valuelabels = FALSE)

# view the description for variable bap87 in English
labels_opendf(input = df$bap87, language = "en", retrieve = "description")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("labels_opendf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_opendf")
### * read_opendf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_opendf
### Title: Read data specified as Open Data Format.
### Aliases: read_opendf

### ** Examples

# get path to example data from the opendataformat package (data.zip)
path <- system.file("extdata", "data.zip", package="opendataformat")
path

# read example data specified as Open Data Format from ZIP file
df <- read_opendf(file = path)
df
attributes(df)
attributes(df$bap87)

# read example data with language selection
df <- read_opendf(file = path, languages = "de")
attributes(df$bap87)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_opendf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setLanguage_opendf")
### * setLanguage_opendf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setLanguage_opendf
### Title: Change language of dataframe metadata
### Aliases: setLanguage_opendf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_opendf"))

# Switch dataset df to language "en" 
df <- setLanguage_opendf(df, language = "en")

# Display dataset information for dataset df in language "en"
docu_opendf(df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setLanguage_opendf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_opendf")
### * write_opendf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_opendf
### Title: Write R data frame to the Open Data Format.
### Aliases: write_opendf

### ** Examples

# get example data from the opendataformat package
df <- get(data("data_opendf"))
df

# write R data frame with attributes to the file my_data.zip specified
# as Open Data Format.
## Not run: 
##D write_opendf(x = df, file = "my_data.zip")
## End(Not run)

# write variable from R data frame with attributes to the file my_data.zip 
# specified as Open Data Format.
## Not run: 
##D write_opendf(x = df$bap87,  file = "my_data.zip")
## End(Not run)

# write R data frame with attributes to the file my_data.zip
# with selected language.
## Not run: 
##D write_opendf(x = df,  file = "my_data.zip", languages = "en")
## End(Not run)

# write R data frame with attributes to the file my_data.zip but only
# metadata, no data.
## Not run: 
##D write_opendf(x = df,  file = "my_data.zip", export_data = "no")
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_opendf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
