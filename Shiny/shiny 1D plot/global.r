#put global one time run out here
library(datasets)
library(ggplot2) # for data

#helper function (converts vector to named list)
namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}

iris.ui<-function(){
	wellPanel(
		selectInput("group","Group", "Loading...")
	)
}

# mtcars.ui<-function(){
	# wellPanel(
		# selectInput("variable","Variable", "Loading...")
	# )
# }