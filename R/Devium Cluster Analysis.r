
# Heirarchical cluster analysis (HCA)
#--------------------------------------

#create heatmap optionally using HCA
devium.heatmap<-function(data, class.factor=NULL, class.color=NULL, heatmap.color = NULL, border.color=NULL, match.dim=2, type=c("none", "spearman", "pearson","biweight"),
						cluster.method = c("none","ward", "single", "complete", "average", "mcquitty", "median" , "centroid"),
						distance.method = c("none","euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"),
						alpha = NULL,font.size = 12,show.names=F, ncolors = 100){
		check.get.packages("pheatmap")
		
		type<-match.arg(type)
		cluster.method<-match.arg(cluster.method)
		distance.method<-match.arg(distance.method)

	
		#prepare data object
		if(match.dim==1){tmp.data<-data.frame(t(data.frame(data)))} else { tmp.data<-data.frame(data)}
		
		# calculate correlations
		if(!type=="none"){
			 tmp<-devium.calculate.correlations(tmp.data,type=type)
			 tmp.data<-tmp$cor
			 tmp.data.pvalue<-tmp$p.value
			
			if(is.numeric(alpha)){ # make discrete
				tmp.data[tmp.data>0]<-1
				tmp.data[tmp.data<0]<--1
				tmp.data[tmp.data.pvalue>alpha]<-0
				ncolors<-3 # limit heat map to 3 colors
			}
		}
		
		if(!cluster.method=="none"){
			cluster_rows<-cluster_cols<-T
			# calculate distances
			if(!distance.method=="none"){
				if(type=="none"){
					if(match.dim==2){
						clustering_distance_cols<-dist(tmp.data, method= distance.method)
						clustering_distance_rows<-dist(data.frame(t(tmp.data)), method= distance.method)
					} else {
						clustering_distance_rows<-dist(tmp.data, method= distance.method)
						clustering_distance_cols<-dist(data.frame(t(tmp.data)), method= distance.method)
					}
				} else {
					tmp.data.dist<-dist(tmp.data, method= distance.method) # for correlations ignore sign focus on magnitude
					clustering_distance_cols<-clustering_distance_rows<-tmp.data.dist
				}
			} 
		} else {
			cluster_rows<-cluster_cols<-F
			cluster.method<-NULL			
		}
		
		# set colors for top of the heatmap annotation
		if(!is.null(class.factor)){
			annotation<- data.frame(sapply(class.factor,as.factor))
			tryCatch(rownames(annotation)<-colnames(tmp.data), error=function(e){cat("The supplied class.factor doesn't match the number of columns","\n")})
		} else {
			annotation<-NA
		}
		
		if(is.na(annotation)){
			annotation.color<-NA
		} else {
			if(is.null(class.color)){
				choices<-colors()[-agrep(c("gray","grey"),colors())]
				#test if factor is continuous or discrete
				fct.type<-sapply(1:ncol(annotation),function(i){
					obj<-as.factor(annotation[,i])
					nlevels(obj)>10
				})
				
				annotation.color<-lapply(1:ncol(class.factor), function(i){
					if(fct.type[i]){
						tmp<-choices[sample(1:length(choices),1)]
						tmp<-colorRampPalette(c("white",tmp))(nlevels(annotation[,i]))
						names(tmp)<-levels(annotation[,i])
					} else {
						tmp<-choices[sample(1:length(choices),nlevels(annotation[,i]))]
						names(tmp)<-levels(annotation[,i])
					}
					tmp
				})
				names(annotation.color)<-colnames(annotation)
			} else {
				annotation.color<- lapply(1:ncol(class.color), function(i){
				fct<-as.factor(annotation[,i])
					sapply(1:nlevels(fct), function(j){
						name<-levels(fct)[j]
						tmp<-fixlc(class.color[which(fct==name)[1], i])
						names(tmp)<-name
						tmp
					})
				})
				names(annotation.color)<-colnames(annotation)
			}
		}
			
		
		if(show.names==FALSE){
			show_rownames<-show_colnames <-F
		} else { 
			show_rownames<-show_colnames <-T
		}
		
		#set colors
		heat.col<-function(col=1,n){ # color for heat map
				# cat("Choices  \n","1 = orange-white-blue",
									# "\n", "2 = white-red \n","3 = white-black \n",
										# "4 = green-black-red \n",
										# "5 = yellow-red \n",
										# "6 = white-yellow-red \n",
										# "7 = white-yellow-orange-red \n")
 				col= switch(col,
				"1" = colorRampPalette(c("navy", "white", "orange"))(n),
				"2" = colorRampPalette(c( "white", "red"))(n),
				"3" = colorRampPalette(c( "white", "black"))(n),
				"4" = colorRampPalette(c( "green","black", "red"))(n),
				"5" = colorRampPalette(c( "yellow", "red"))(n),
				"6" = colorRampPalette(c( "white","yellow", "red"))(n),
				"7" = colorRampPalette(c( "white","yellow","orange", "red"))(n),
				"8" = colorRampPalette(c("navy", "white", "firebrick3"))(n))
				return(col)
			}
	
		if(is.null(heatmap.color)){ heat.color<-heat.col(8,ncolors) } else { heat.color<-colorRampPalette(heatmap.color)(ncolors)}
		
		#plot heat map
		pheatmap(tmp.data,col= heat.color,
		show_rownames = show_rownames,show_colnames = show_colnames, # show labels
		cluster_rows = cluster_rows, cluster_cols=cluster_cols, # cluster
		clustering_method=cluster.method,
		clustering_distance_rows = clustering_distance_rows,
		clustering_distance_cols = clustering_distance_cols,
		border_color = border.color,
		annotation = annotation, # annotation factor
		annotation_colors = annotation.color, # colors for each column of annotation factor
		annotation_legend= TRUE,
		fontsize = font.size)
}
