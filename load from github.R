source.git.hub<-function(url = "https://github.com/dgrapov/devium/tree/master/R")
{
  if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
  
  #get the names of all scripts to source
  obj<-getURL("https://github.com/dgrapov/devium/tree/master/R",ssl.verifypeer=FALSE)
  tmp<-strsplit(obj,'href=\"/')
  tmp2<-unlist(strsplit(as.character(unlist(tmp)),'class'))
  scripts<-gsub("/blob","",gsub('\" ',"",tmp2[grep("dgrapov/devium/blob/master/R/",tmp2)])) # fix formatting
  #add http for git hub
  scripts<-paste("https://raw.githubusercontent.com/",scripts,sep="")
  sapply(1:length(scripts),function(i)
  {
    text <- getURL(scripts[i],
                  ssl.verifypeer=FALSE)
    #problem 06/27/14 with '/r'?
    text<-gsub("\r","",text)
    tryCatch( eval( expr = parse( text = text),envir=.GlobalEnv),error=function(e){print(paste("can't load:",scripts[i]))})
  })
}

source.git.hub()
