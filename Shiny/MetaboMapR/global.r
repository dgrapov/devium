# avoid breaks in R-output print and show JSON packets transferred
# over websockets
options(width = 150, shiny.trace=TRUE)
# options(width = 150)

# #get Devium R-scripts for analysis functions
source("http://pastebin.com/raw.php?i=JVyTrYRD")

# options(repos = c("http://cran.rstudio.com/"))
libs <- c("tools","CTSgetR") #"RJSONIO", "shiny", "car", "AER", "Ecdat", "foreign", "tools", "ggplot2", 
	#"gridExtra", "reshape2", "plyr", "markdown", "R.utils", "psych", "rela", "arm", "xts")
available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
	install.packages(inst.libs, dependencies = TRUE)
	suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
}

# #load CTSgetR for translations
# install.packages("devtools")
# install.packages("RJSONIO")
# library(devtools);library(RJSONIO)#;library(RCurl) 
# install_github(repo = "CTSgetR", username = "dgrapov")
library(CTSgetR)

values <- reactiveValues()
TCA.kegg <- c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
# TCA.CID<-CTSgetR(TCA.kegg, from="KEGG", to = "PubChem CID")
TCA.CID <- c("[]","51", "440649","[]", "439161",   "1110",    "972",      "1198",     "970",      "6302",     "222656",   "643757",  "19782904", "1060",     "440568"  ,"[]", "21883788" ,"[]","1005" )
values[["Citric Acid Cycle"]] <-data.frame(KEGG = TCA.kegg, CID = TCA.CID )
# values[["Citric Acid Cycle"]] <-"Citric_Acid_Cycle"
datasets<-"Citric Acid Cycle"
values$clipboard<-""