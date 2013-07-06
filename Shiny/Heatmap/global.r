options(shiny.trace = TRUE)
# shinyRun(`server.R` = quote({}), `ui.R` = quote({})))

#check for and/or install dependencies
need<-c("RCurl","pheatmap","datasets")
for(i in 1:length(need)){
if(require(need[i], character.only = TRUE)==FALSE){install.packages(need[i]);library(need[i], character.only = TRUE)} else { library(need[i],character.only = TRUE)}
}


#load devium repo from from github
source("http://pastebin.com/raw.php?i=JVyTrYRD")
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") # for dendrogram


# accessory fxns
#convert vector to named list
namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}
	
# use to see datasets
# code from http://stackoverflow.com/questions/12575098/to-see-all-the-content-not-just-objects-in-a-package-in-r
lsp <- function (package, all.names = FALSE, pattern) {
    package <- deparse(substitute(package))
    ls(pos = paste("package", package, sep = ":"), all.names = all.names, 
        pattern = pattern)
}

# limit datasets to class == data.frame
set<-lsp(datasets)
set.class<-sapply(seq(set), function(i){ class(get(set[i]))})
data.options<-set[set.class=="data.frame"] #|set.class=="matrix"

#select colors for heatmap options
color.opts<-c("red","orange","yellow","green","blue","violet","purple","white","black","gray")












