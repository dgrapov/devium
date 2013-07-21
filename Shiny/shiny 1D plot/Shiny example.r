
# run at  above scripts file path
library(shiny)
library(ggplot2)

# run local
app.dir<-"C:/Users/D/Dropbox/shiny" # filepath for app
runApp(app.dir) # yehaw

#run from gist
library(shiny)
runGist('5792778')

rpel<-function (string, envir = .GlobalEnv) 
	{
		eval(parse(text = string), envir = envir)
	}