
shinyServer(function(input, output, session) {
		
		observe({
		# output$groups <- renderUI({ 
			# get dim [2] names
			tmp.data<-get(input$data)
			tmp<-input$dimention
			if(tmp=="columns"){
				group.opts<-rownames(tmp.data)
			} else {
				group.opts<-colnames(tmp.data) 
			}
			# SelectInput("groups","Group", namel(group.opts))
			 updateSelectInput(session, "groups", choices = group.opts)
		}) 

		#summary
		output$summary <- renderPrint({
			tmp.data<-data.frame(get(input$data))
			match.dim<-ifelse(input$dimention=="rows",1,2)
			if(match.dim==2){tmp.data<-data.frame(t(data.frame(tmp.data)))}		
			str(tmp.data)
		})
		
		#caption
		output$caption<-renderText({c(input$groups)})
		#output$caption<-renderText({"Bring the Heatmap"})
	
		#make heatmap
		output$heatmap <- renderPlot({
		
				#condition inputs
				tmp.data<-get(input$data)
				tmp.data[is.na(tmp.data)]<-0 #make missing values < - 0 to avoid errors
				match.dim<-ifelse(input$dimention=="rows",1,2)
				type <-input$type
				show.names <-input$names
				border.color <-ifelse(input$border==TRUE,"gray40",FALSE)
				
				#get object for top annotation
				if(match.dim==1){
					class.factor<-as.data.frame(tmp.data[,colnames(tmp.data)%in%input$groups,drop=F])
				} else {
					# if rownames are numeric will be appended with an X
					tmp<-data.frame(t(data.frame(tmp.data)))
					if(match.dim==1){
						class.factor<-tmp[names%in%input$groups,,drop=F]
					} else {
						class.factor<-tmp[,names%in%input$groups,drop=F]
					}
					class.factor<- data.frame(matrix(fixlc(class.factor), ncol=1))
					tryCatch(colnames(class.factor)<-input$groups, error= function (e){}) # for legend
				} 
				
				if(is.null(input$groups)) class.factor<-NULL
				
				
				#remove factors from main heatmap matrix visualization 
				#alternative is to convert to numeric, but redundant with annotation row output
				fct.id<-sapply(seq(tmp.data), function(i){!is.numeric(tmp.data[,i])|is.integer(tmp.data[,i])})
				tmp.data<-tmp.data[,!fct.id, drop=F]
				if(match.dim==2){
					if(!is.null(input$groups)) {
						names<-colnames(class.factor)
						class.factor<-data.frame(as.matrix(fixlc(class.factor)[!fct.id]))
						colnames(class.factor)<-names
					}
				}
				
				#avoid clustering w/ partial args set (could auto set defaults for anything = "none")
				if(input$linkage=="none"||input$distance=="none"){
					cluster.method<- distance.method <-"none"
				} else {
					cluster.method 	<- input$linkage
					distance.method <- input$distance
				}
				
				#colors
				heatmap.color<-c(input$low.col,input$mid.col,input$high.col)
				
				
				devium.heatmap(
				data 			= tmp.data, 
				match.dim 		= match.dim,
				type 			= type,
				class.factor 	= class.factor,
				class.color 	= NULL,
				heatmap.color	= heatmap.color,
				cluster.method 	= cluster.method, 
				distance.method = distance.method,
				font.size 		= 12,
				border.color	= border.color,
				show.names 		= show.names)
				})	
		
		# #condition slider max on dim[2]				
		output$HCA.groups <- renderUI({
					#should have some reactive object to store these values instead of constantly calling
					tmp.data<-data.frame(get(input$data))
					#drop factors pre transpose
					fct.id<-sapply(seq(tmp.data), function(i){!is.numeric(tmp.data[,i])|is.integer(tmp.data[,i])})
					tmp.data<-tmp.data[,!fct.id, drop=F]
					match.dim<-ifelse(input$dimention=="rows",1,2)
					if(match.dim==2){tmp.data<-data.frame(t(data.frame(tmp.data)))}
					
					ymax <-nrow(tmp.data) # opposite of usual 
					nclust<-tryCatch(input$HCA.groups, error= function(e) {NULL})
					# avoiding errors in stats
					if(is.null(nclust)){nclust<-3}
					if(nclust < 2) {nclust<-3}
					if(nclust>ymax) {nclust<-ymax-1}
					
					sliderInput(inputId = "HCA.groups","Number of Clusters",min= 3, max = ymax, value = nclust, step = 1)
				  })
		
		##dendrogram		
		output$dendrogram <- renderPlot({	
			
			if(!input$linkage=="none"||!input$distance=="none"){
			
				tmp.data<-data.frame(get(input$data))
				rownames(tmp.data)<-paste("X",tryCatch(rownames(tmp.data), error= function(e) {1:nrow(tmp.data)}), sep="")
				tmp.data[is.na(tmp.data)]<-0 #make missing values < - 0 to avoid errors
				match.dim<-ifelse(input$dimention=="rows",1,2)
				match.dim<-ifelse(input$dimention=="rows",1,2)
				#drop factors pre transpose
				fct.id<-sapply(seq(tmp.data), function(i){!is.numeric(tmp.data[,i])|is.integer(tmp.data[,i])})
				tmp.data<-tmp.data[,!fct.id, drop=F]
				if(match.dim==2){tmp.data<-data.frame(t(data.frame(tmp.data)))}
				type <-input$type
				#remove factors
				
				
				#clustering
				distance<-dist(tmp.data, method = input$distance ) # euclidean
				clusters<-hclust(distance, method = input$linkage)
				
				# #number of clusters
				# if(nrow(tmp.data)<6) { nclust<-nrow(tmp.data) } else { nclust<-6 }
				
				# #update slider (seems no option to update max)
				# observe({
					# tmp.data<-data.frame(get(input$data))
					# updateSliderInput(session, "HCA.groups", max =(nrow(tmp.data)-1))
				# })
				
					nclust<-input$HCA.groups
					# #control
					# if(is.null(nclust)){nclust<-3}
					# if(nclust < 2) {nclust<-3}
					
					A2Rplot(clusters, k=nclust, boxes = FALSE,col.up = "gray50", col.down = rainbow(nclust), main="")

				
				#get selected clusters and add to annotation options
				# HCA.clusters<<--cutree(clusters, k=nclust)
				# #update groups
				
				
			}
			
			})		
})
