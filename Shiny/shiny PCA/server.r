

shinyServer(function(input, output, session) {
		
		.theme<- theme(
					axis.line = element_line(colour = 'gray', size = .75), 
					panel.background = element_blank(),  
					plot.background = element_blank()
				 )	
	
	
		#file info	
		output$filetable <- renderTable({
			if(is.null(input$files) ) { return() }  else { 
			tmp<-read.csv(input$files$datapath, header=T, stringsAsFactors =T,row.names=TRUE)
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
