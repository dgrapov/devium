
#ideas https://github.com/mostly-harmless/radyant
#http://blog.snap.uaf.edu/2013/05/20/introducing-r-shiny-web-apps/

# old<-function(){
# shinyServer(function(input, output, session){
	# #update variable and group based on dataset
	# output$variable <- renderUI({ 
		# obj<-switch(input$dataset,
           # "iris" = iris,
           # "mtcars" = mtcars)	 
		# var.opts<-namel(colnames(obj))
		# selectInput("variable","Variable:", var.opts) # uddate UI 				 
		# }) 
		
	# output$group <- renderUI({ 
		# obj<-switch(input$dataset,
           # "iris" = iris,
           # "mtcars" = mtcars)	 
		# var.opts<-namel(colnames(obj))
		# selectInput("group","Groups:", var.opts) # uddate UI 				 
		# }) 
		
	# output$caption<-renderText({
		# switch(input$plot.type,
			# "boxplot" 	= 	"Boxplot",
			# "histogram" =	"Histogram",
			# "density" 	=	"Density plot",
			# "bar" 		=	"Bar graph")
		# })
			
	
	# output$plot <- renderUI({
		# plotOutput("p")
	# })
		
	# #plotting function using ggplot2
	# output$p <- renderPlot({

	# plot.obj<<-list() # not sure why input$X can not be used directly?
	# plot.obj$data<<-get(input$dataset) 
	# plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
	# plot.obj$group<<-with(plot.obj$data,get(input$group)) 
	
	# #dynamic plotting options
	# plot.type<-switch(input$plot.type,
			# "boxplot" 	= 	geom_boxplot(),
			# "histogram" =	geom_histogram(alpha=0.5,position="identity"),
			# "density" 	=	geom_density(alpha=.75),
			# "bar" 		=	geom_bar(position="dodge")
		# )
		
	# require(ggplot2)
	# #plotting theme
	# .theme<- theme(
				# axis.line = element_line(colour = 'gray', size = .75), 
				# panel.background = element_blank(),  
				# plot.background = element_blank()
				 # )	 
	# if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
		# p<-ggplot(plot.obj$data, 
				# aes(
					# x 		= plot.obj$group, 
					# y 		= plot.obj$variable,
					# fill 	= as.factor(plot.obj$group)
					# )
				# ) + plot.type
				
				# if(input$show.points==TRUE)
				# { 
					# p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
				# }
				
		# } else {
		
		# p<-ggplot(plot.obj$data, 
				# aes(
					# x 		= plot.obj$variable,
					# fill 	= as.factor(plot.obj$group),
					# group 	= as.factor(plot.obj$group),
					# #color 	= as.factor(plot.obj$group)
					# )
				# ) + plot.type
		# }
		
	 # p<-p+labs(
			# fill 	= input$group,
			# x 		= "",
			# y 		= input$variable
			# )  +
	# .theme
	# print(p)
	# })	
# })
# }

shinyServer(function(input, output, session) {
		
		.theme<- theme(
					axis.line = element_line(colour = 'gray', size = .75), 
					panel.background = element_blank(),  
					plot.background = element_blank()
				 )	
	
	
		#file info	
		output$filetable <- renderTable({
			if(is.null(input$files) ) { return() }  else { 
			tmp<-read.csv(input$files$datapath, header=T, stringsAsFactors =T)
			tmp<-tmp[,seq_along(1:ncol(tmp))<=10] # show max 10 columns and binf head tail calls
			rbind(head(tmp,10),tail(tmp,10))
				# input$files 
				
				} 
			
			})
		
		#confirm load
		output$caption<-renderText({
			if (!is.null(PCA.results())) {
					"Principal Components Analysis"
				} else {
					if(is.null(input$files)) { "Load Data" }  else { "Data Loaded"}
				}
			}) 
		
		
		#number of PCs
		output$PCs<-renderUI({
				if (is.null(input$files)) { return(NULL) }
				maxPCs<-ncol(input$files)
				numericInput("PCs", "Number of Principal Components", 
				2, min = 2, max = maxPCs)
			})
			
		PCA.results<-reactive({
				if (is.null(input$files)) { 
						return(NULL) 
					} else {
						# list(data=read.csv(input$files$datapath, header=T, stringsAsFactors =T),
						# data2=rnorm(10))
						
					# }
				#adapted from another devium
				pca.inputs<-list()
				start.data<<-read.csv(input$files$datapath, header=T, stringsAsFactors =T)
				pca.inputs$pca.data<-"start.data"
				pca.inputs$pca.algorithm<-input$method
				pca.inputs$pca.components<-input$PCs
				pca.inputs$pca.center<-input$center
				pca.inputs$pca.scaling<-input$scaling
				pca.inputs$pca.cv<-input$cv # currently not used
				devium.pca.calculate(pca.inputs,return="list",plot=F)
				}
			})	
			
		
			
		#make screeplot
		output$screeplot <- renderPlot({
					if (is.null(PCA.results())) { 
						return(NULL)
					} else {
						x<-PCA.results()
						x<-data.frame(x$pca.eigenvalues)
						
						# make.scree.plot(x)	
						make.scree.plot.bar(x)
					}
				})	
				
		# scores diagnostic plot
		output$scores <- renderPlot({
					if (is.null(PCA.results())) { 
						return(NULL)
					} else {
						tmp<-PCA.results()
						scores<-data.frame(tmp$pca.scores)
						if(nrow(tmp$pca.diagnostics)==nrow(scores))
								{
									if(any(tmp$pca.diagnostics$DmodX=="NaN")){tmp$pca.diagnostics$DmodX<-1}
									scores<-data.frame(leverage=tmp$pca.diagnostics$leverage, dmodx=tmp$pca.diagnostics$DmodX,scores)
								} else {
									scores<-data.frame(leverage=1, dmodx=1,scores)
								}
								
						p<-ggplot(scores,mapping = aes_string(x = names(scores)[3], y = names(scores)[4],color=names(scores)[1],size=names(scores)[2])) + 
						scale_size_continuous("DmodX", range = c(4, 10)) + 
						geom_point(alpha=0.75) +.theme
						print(p)
					}
				})	
		#loadings plot
		output$loadings <- renderPlot({
					if (is.null(PCA.results())) { 
						return(NULL)
					} else {
						tmp<-PCA.results()
						loadings<-data.frame(tmp$pca.loadings,names=rownames(tmp$pca.loadings))
						
						#plot
						p<-ggplot(loadings,mapping = aes_string(x = names(loadings)[1], y = names(loadings)[2], label = "names")) + 
						geom_text(size=4,alpha=0.75) +.theme
						print(p)
					}
				})	
				
})
