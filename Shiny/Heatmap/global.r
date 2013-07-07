

options(shiny.trace = TRUE)

#check for and/or install dependencies
need<-c("RCurl","pheatmap","datasets","WGCNA") # some more should auto load...
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

#fix numeric rownames (sometimes no effect try other methods inside)
# need to append numeric rownames with X (and avoid error causing characters in names) to stay consistent when matrix transposed
rdy.t<-function(obj){
  list<-dimnames(obj)
  names<-lapply(seq(list), function(i){
    tmp<-check.fix.names(fixlc(list[[i]]),ok.chars=c(".","_"))
    test<-!is.na(as.numeric(tmp))
    paste(ifelse(test,"X",""),tmp,sep="")           
  })
  out<-as.matrix(obj)
  dimnames(out)<-names
  return(data.frame(out))
}



# limit datasets to class == data.frame
data.options<-function(){
  set<-lsp(datasets)
  set.class<-sapply(seq(set), function(i){ class(get(set[i]))})
  set[set.class=="data.frame"] #|set.class=="matrix"
}

#select colors for heatmap options
color.opts<-function(){c("red","orange","yellow","green","blue","violet","purple","white","black","gray")}
