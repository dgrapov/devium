
start.data<-rdy.t(mtcars)
input<-list()


input$data<-"start.data"
input$type<-"none"
input$linkage <- "ward"
input$distance <-"euclidean"
input$names<-T
input$border<-T
heatmap.color<-c( "red","yellow","green")


start.data<-rdy.t(iris)
input$groups<-"Species" 
input$dimention<-"rows"

input$groups<-"X1" 
input$dimention<-"columns"

start.data<-rdy.t(mtcars)

input$groups<-"Volvo.142E" 
input$dimention<-"columns"

input$groups<-"mpg" 
input$dimention<-"rows"