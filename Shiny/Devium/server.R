sourceGitDirectory<-function(url="https://github.com/dgrapov/devium/tree/master/Shiny/Devium/tools/analysis", user="dgrapov", input, output){
	obj<-RCurl::getURL(url, ssl.verifypeer=FALSE)
	#can't use XML::xmlParse(contents) or xmlToList(contents)... so hack
	tmp<-strsplit(obj,'href=\"/')
	tmp2<-unlist(strsplit(as.character(unlist(tmp)),'class'))
	#look for .R scripts
	s1<-agrep("dgrapov",tmp2)
	s2<-agrep(".R/",tmp2)
	keep<-s1[s1%in%s2][-1] # the first entry is always wrong?
	tmp3<-tmp2[keep]
	
	scripts<-paste0("https://raw.github.com/",gsub('\" ',"",tmp3)) # fix formatting
	scripts<-gsub("blob/","",scripts)
	sapply(1:length(scripts),function(i)
		{
			tryCatch( eval( expr = parse( text = getURL(scripts[i],
						   ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",scripts[i]))})
		})
	
}

shinyServer(function(input, output) {

	# source base functions
	source('radyant.R', local = TRUE)
	
	# source enabled analysis tools
	# flist <- sourceDirectory('tools', recursive = TRUE) # local
	sourceGitDirectory(url="https://github.com/dgrapov/devium/tree/master/Shiny/Devium/tools/analysis", user="dgrapov", input=input, output=output) # for online mode
	sourceGitDirectory(url="https://github.com/dgrapov/devium/tree/master/Shiny/Devium/tools/data", user="dgrapov" , input=input, output=output)

	# the 'grand' analysis ui-element caller
	output$ui_analysis <- renderUI({
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

	
	# the 'grand' data ui-element caller - only for transform for now
	# not used yet - should all tools be separated out?
	# output$ui_transform <- renderUI(function() {
 	#  	if(input$tool != "dataview") return()
	#   get(paste('ui_',input$datatabs, sep=""))()
	# })
})
