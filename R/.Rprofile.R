print('Yo ho ho and a bottle of Rum')

source.local.dir<-function(wd){
	o.dir<-getwd()
	setwd(wd)
	files<-dir()[unique(c(agrep(".r",dir()),agrep(".R",dir())))]
	lapply(1:length(files),function(i) {tryCatch(source(files[i]),error=function(e){paste0("can't load-->",files[i])})
	})
	setwd(o.dir)
}

source.local.dir("/Users/dgrapov/Dropbox/Devium/devium/R")