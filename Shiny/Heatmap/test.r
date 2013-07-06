setwd("C:/Users/D/Dropbox/shiny/Heatmap")


# run at  above scripts file path
library(shiny)


# run local
app.dir<-getwd() # filepath for app
runApp(app.dir) # yehaw

#
library(shiny)
runGist("")

#main fxn
devium.heatmap(data, 
match.dim=1,
type = "none",
alpha = 0.05,
show.names= T,
border.color= F,
class.factor=as.data.frame(factor),
class.color=class.color,heatmap.color=c("blue","red","white"),
cluster.method = "ward", 
distance.method = "minkowski")

input<-list()
input$data<-"iris"
input$groups<-"Species"
input$dimention<-"columns"
input$type<-"none"
input$linkage <- "ward"
input$distance <-"euclidean"
input$dimention<-"columns"
input$groups<-"1"

#heirachichally cluster	the data
distance<-dist(mtcars) # euclidean
clusters<-hclust(distance, method = "ward")
plot(clusters)


#circular dendrogram showing cluster numbers
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg="#EFEFEF")
 x ,             # an hclust object to draw
  k        = 2,   # the number of groups
  col.up   = "black",
  col.down = rainbow(k),
  lty.up   = 2,
  lty.down = 1,
  lwd.up   = 1,
  lwd.down = 2,
  type     = c("rectangle","triangle"),
  knot.pos = c("mean","bary","left","right","random"),
  criteria,
  fact.sup,
  show.labels=TRUE,
  only.tree=FALSE,
  main     = paste("Colored Dendrogram (",k," groups)"),
  boxes    = TRUE,
  members,

A2Rplot(clusters, k=7, boxes = FALSE,col.up = "gray50", col.down = rainbow(7),main="")