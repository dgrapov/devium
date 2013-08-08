#debugging
# output<-input<-list()
# input$datasets<-"mtcars"
# input$hc_vars<-colnames(input$datasets)
# input$hc_group_vars<-"am"
# input$hc_transformation<-"none"
# input$hc_method<-"none"
# input$hc_dist_method<-"none"
# input$low_col<-input$mid_col<-input$high_col<-"gray"
# match.dim

#functions used
#----------------------------------
#load devium repo from from github
# source("http://pastebin.com/raw.php?i=JVyTrYRD")
# load code of A2R function
# # source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") # for dendrogram

#select colors for heatmap options
color.opts<-function(){c("red","orange","yellow","green","blue","violet","purple","white","black","gray")}

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

# HCA UI
#--------------------------------------
# variable selection - hclustering
output$hc_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE)
})

# annotation variable selection - hclustering
output$hc_group_vars <- renderUI({

	tmp.data <- getdata()[,input$hc_vars,drop=FALSE]
	if(is.null(tmp.data)) {return()}
	
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    #tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
    
    #remove factors else could convert to numerics
	# should go into annotation section
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
    tmp.data<-tmp.data[,!fct.id, drop=F]
    
    if(input$dimention=="1"){
      tmp.data<-data.frame(t(data.frame(tmp.data)))
    } 

	# if(input$dimention=="1"){
		# group.opts<-colnames(tmp.data)
    # } else {
		group.opts<-rownames(tmp.data)
    # }
	
  vars <-group.opts
  if(is.null(vars)) return()
  selectInput(inputId = "hc_group_vars", label = "Variables:", choices = c("none",vars), selected ="none", multiple = TRUE)
})

#number of clusters
# #condition slider max on dim[2]				
output$hc_clusters <- renderUI({
   
    tmp.data <- getdata()[,input$hc_vars,drop=FALSE]
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    #tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
    
    #remove factors else could convert to numerics
	# should go into annotation section
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
    tmp.data<-tmp.data[,!fct.id, drop=F]
    
    if(input$dimention=="1"){
      tmp.data<-data.frame(t(data.frame(tmp.data)))
    } 
  
    ymax <-ncol(tmp.data) # 
    nclust<-tryCatch(input$HCA_groups, error= function(e) {NULL})
    # avoiding errors in stats
    if(is.null(nclust)){nclust<-3}
    if(nclust < 2) {nclust<-3}
    if(nclust>ymax) {nclust<-ymax-1}
    
    sliderInput(inputId = "hc_nrClus","Number of Clusters",min= 3, max = ymax, value = nclust, step = 1)
  })
 
#method options
hc_transformation<-list("none" = "none", "Z-scale"= "z.scale", "Spearmans correlation" = "spearman", "Pearsons correlation" = "pearson","Biweight correlation" = "biweight")
hc_method <-  c("none","ward", "single", "complete", "average", "mcquitty", "median" , "centroid")#list("Ward's" = "ward", "Single" = "single", "Complete" = "complete", "Average" = "average", "McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")
hc_dist_method <-  c("none","euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski")#c("sq.euclidian", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

ui_hclustering <- function() {
  wellPanel(
	h3("Data"),
    uiOutput("hc_vars"), # data variables
	tags$style(type='text/css', "#hc_vars { height: 200px; padding-bottom: 35px;}"),
	radioButtons("dimention","Dimension:", list("rows" = 1,"columns" = 2), selected = "columns"), # flipping data rows/columns
    selectInput("hc_transformation", label = "Transformation:", choices = hc_transformation, selected = hc_transformation[1], multiple = FALSE),
	h3("Clustering"),
	selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
    selectInput("hc_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE),
    uiOutput("hc_clusters"),
	# selectInput("hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("hc_saveclus", "Save cluster membership"),
	h3("Annotation"),
	uiOutput("hc_group_vars"),
	checkboxInput("names","Names",FALSE),
    checkboxInput("border","Border",FALSE),
    h4("Colors"),
    selectInput("low_col","low", color.opts(), selected = "green" ),
    selectInput("mid_col","mid", color.opts(), selected = "black"),
    selectInput("high_col","high", color.opts(),selected = "red")
  )
}
  
# #caption
# output$caption<-renderText({c(input$hc_group_vars)})
  
  # #make heatmap
  plot.hclustering <- function(result){ 
   
    #plot heatmap 
    devium.heatmap(
      data 			      	= result$data, 
      match.dim 		 	= result$match.dim,
      type 			     	= result$type,
      class.factor 	  		= result$class.factor,
      class.color 	  		= NULL,
      heatmap.color	 	 	= result$heatmap.color,
      cluster.method 		= result$cluster.method, 
      distance.method 		= result$distance.method,
      font.size 		  	= 12,
      border.color	  		= result$border.color,
      show.names 		  	= result$show.names)
  }#)
   
  # ##dendrogram		
  # output$dendrogram <- renderPlot({	
    
    # if(!input$linkage=="none"||!input$distance=="none"){
      
      # #data
      # tmp.data<-data.frame(t(ui.opts$data())) 
    
      
      # #clustering
      # distance<-dist(tmp.data, method = input$distance ) # euclidean
      # clusters<-hclust(distance, method = input$linkage)
      

      # #number of clusters
      # nclust<-input$HCA.groups
  
      # # function from http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R
      # A2Rplot(clusters, k=nclust, boxes = FALSE,col.up = "gray50", col.down = rainbow(nclust), main="")
	 # }
# })



# #----------output-----------------------
summary.hclustering <- function(result) {
	result
}

# place all reactives inside here the result is sent to plots or summaries
hclustering<-reactive({ # clustering output
	tmp<-list() # collect args to pass to plot
	#dimension
	tmp$match.dim<- as.numeric(input$dimention)
	
	#data
    tmp.data <-getdata()[,input$hc_vars,drop=FALSE]
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
	tmp.vars<-tmp.data # save annotation
    tmp.data<-tmp.data[,!fct.id, drop=F]
    
    # if(input$dimention=="1"){
      # tmp.data<-data.frame(t(data.frame(tmp.data)))
    # } 
    tmp$data<-tmp.data
	
	#top row(s) annotation
	if(input$hc_group_vars=="none"){
			class.factor<-NULL 
		} else {
			if(tmp$match.dim==1){# mind f$ck for flipping
				class.factor<-as.data.frame(tmp.vars[,colnames(tmp.vars)%in%input$hc_group_vars,drop=F]) 
			} else {
				class.factor<-t(as.data.frame(tmp.vars[rownames(tmp.vars)%in%input$hc_group_vars,,drop=F])[,!fct.id,drop=F])
			}
	}
	tmp$class.factor<-class.factor
	tmp$type <-input$type
    tmp$show.names <-input$names
    tmp$border.color <-ifelse(input$border==TRUE,"gray40",FALSE)
	if(input$hc_meth == "none"|input$hc_dist == "none"){
		tmp$cluster.method<- "none"
		tmp$distance.method<-"none"
    } else {
		tmp$cluster.method <- input$hc_meth
		tmp$distance.method <- input$hc_dist
    }
	tmp$type<-input$hc_transformation
	
	tmp$heatmap.color<-c(input$low_col,input$mid_col,input$high_col)
	tmp
})

# plot.hclustering<-function(result){
	# plot(1)	
# }

# # plot.hclustering <- function(result) {

	# # # use ggdendro when it gets back on cran
	# # par(mfrow = c(2,1))
	# # plot(result, main = "Dendrogram")
	# # height = rev(result$height[result$height > 0])
	# # nr_of_clusters = 1:length(height)
	# # plot(nr_of_clusters,height, xlab = "Nr of clusters", ylab = "Height", type = 'b')
# # }



# hclustering <- reactive({ # doing the clustering
	# if(is.null(input$hc_vars)) return("Please select one or more variables")
	# if(input$hc_dist =="none" | input$hc_meth =="none") return("Please select clustering options")
	
	# dat <- getdata()[,input$hc_vars]
	# # if(input$hc_dist == "sq.euclidian") {
		# # dist.data <- dist(dat, method = "euclidean")^2
	# # } else {
	# dist.data <- dist(dat, method = input$hc_dist)
	# # }
	# hclust(d = dist.data, method= input$hc_meth)
# })

observe({ # adding cluster membership to the data
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		changedata(as.factor(clusmem), paste("hclus",input$hc_nrClus,sep=""))
	})
})





#---------------------------------------
#---------------------------------------

# # variable selection - hclustering
# output$hc_vars <- renderUI({
  # vars <- varnames()
  # if(is.null(vars)) return()
  # selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
# })

# hc_method <- list("Ward's" = "ward", "Single" = "single", "Complete" = "complete", "Average" = "average", 
	# "McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")

# hc_dist_method <- c("sq.euclidian", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

# ui_hclustering <- function() {
  # wellPanel(
    # uiOutput("hc_vars"), 
    # selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
    # selectInput("hc_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE),
    # selectInput("hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    # actionButton("hc_saveclus", "Save cluster membership")
  # )
# }

# summary.hclustering <- function(result) {
	# result
# }

# plot.hclustering <- function(result) {

	# # use ggdendro when it gets back on cran
	# par(mfrow = c(2,1))
	# plot(result, main = "Dendrogram")
	# height = rev(result$height[result$height > 0])
	# nr_of_clusters = 1:length(height)
	# plot(nr_of_clusters,height, xlab = "Nr of clusters", ylab = "Height", type = 'b')
# }

# hclustering <- reactive({ # doing the clustering
	# if(is.null(input$hc_vars)) return("Please select one or more variables")

	# dat <- getdata()[,input$hc_vars]
	# if(input$hc_dist == "sq.euclidian") {
		# dist.data <- dist(dat, method = "euclidean")^2
	# } else {
		# dist.data <- dist(dat, method = input$hc_dist)
	# }
	# hclust(d = dist.data, method= input$hc_meth)
# })

# observe({ # adding cluster membership to the data
	# if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	# isolate({
		# clusmem <- cutree(hclustering(), k = input$hc_nrClus)
		# changedata(as.factor(clusmem), paste("hclus",input$hc_nrClus,sep=""))
	# })
# })


# K-means objects
#--------------------------
output$km_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), 
	  checkboxInput(inputId = "km_hcinit", label = "Initial centers from HC", value = TRUE),
  	conditionalPanel(condition = "input.km_hcinit == true",
  		wellPanel(
	  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
  			selectInput("km_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE)
  		)
  	),
  	conditionalPanel(condition = "input.km_hcinit == false", 
	    numericInput("km_seed", "Set random seed:", 1234, min = 0)
	  ),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}

summary.kmeansClustering <- function(result) {
	result$cluster = NULL
	result
}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$km_vars, drop = FALSE]
	# gg.xlim <- quantile(as.vector(as.matrix(dat)),probs = c(.01,.99))
	dat$clusvar <- as.factor(result$cluster)

		plots <- list()
		for(var in input$km_vars) {
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + xlim(gg.xlim[1],gg.xlim[2]) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
		}
		print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

hinitclustering <- reactive({
	if(is.null(input$km_vars)) return("Please select one or more variables")
	dat <- getdata()[,input$km_vars]
	if(input$km_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$km_dist)
	}
	hclust(d = dist.data, method= input$km_meth)
})

kmeansClustering <- reactive({
	if(is.null(input$km_vars)) return("Please select one or more variables")
	set.seed(input$km_seed)
	dat <- getdata()[,input$km_vars]
	# dat <- lapply(dat,as.numeric) 	# has strange effect
	if(input$km_hcinit) {
		clusmem <- cutree(hinitclustering(), k = input$km_nrClus)
		cluscenters <- as.matrix(aggregate(dat,list(clusmem),mean)[-1])
		kmeans(na.omit(object = data.frame(dat)), centers = cluscenters, iter.max = 500)
	} else {
		kmeans(na.omit(object = data.frame(dat)), centers = input$km_nrClus, nstart = 10, iter.max = 500)
	}
})

observe({
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		clusmem <- kmeansClustering()$cluster
		changedata(as.factor(clusmem), paste("kclus",input$km_nrClus,sep=""))
	})
})
