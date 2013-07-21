setwd("C:/Users/D/Desktop/shiny PCA")


# run at  above scripts file path
library(shiny)
library(ggplot2)

# run local
app.dir<-getwd() # filepath for app
runApp(app.dir) # yehaw

#
library(shiny)
runGist("5846650")