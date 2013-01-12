

#function to add transparency to colors
alpha.col<-function(color,alpha)
	{
		tmp <- col2rgb(color)/255 
		rgb(tmp[1,],tmp[2,],tmp[3,],alpha=alpha)
	}

#plot cluster representation of point group via connected edges to a group center
edge.group<-function(obj,color,lwd=1,lty=1)
	{
		#split objs and inputs for each group based on color (color, lwd, lty  are all mapped together)
		tmp.obj<-as.data.frame(obj)
		tmp.char<-as.data.frame(cbind(color,lwd,lty))
		fct<-as.factor(color)
		.obj<-split(tmp.obj,fct)
		.char<-split(tmp.char,fct)
		
		i<-1
		for(i in 1:nlevels(fct))
		{	
			#group center
			pts<-.obj[[i]]
			m<-colMeans(pts)
			
			#plotting
			segments(m[1],m[2],pts[,1],pts[,2],col=as.character(.char[[i]][,1]),
				lwd=as.numeric(as.character(.char[[i]][,2])),lty=as.numeric(as.character(.char[[i]][,3])))
		}
	}

#plot ellipse representation of point group 	
ellipse.group<-function(obj,color,lwd=1,lty=1,border="#00000050",ellipse.level=.95,show.polygon=TRUE, alpha=.5)
	{
		check.get.packages(c("ellipse","splancs"))
		
		#check color and add extra transparency
		color<-alpha.col(color,alpha)	
		#split objs and inputs for each group based on color (color, lwd, lty  are all mapped together)
		tmp.obj<-as.data.frame(obj)
		tmp.char<-as.data.frame(cbind(color,lwd,lty,border))
		fct<-as.factor(color)
		.obj<-split(tmp.obj,fct)
		.char<-split(tmp.char,fct)

		#calculate points for ellipse
		ellipse.var<-lapply(1:nlevels(fct),function(i)
			{
				tmp<-list()
				pts<-.obj[[i]]
				if(nrow(pts)<=2){pts<-matrix(c(NA,NA))}# avoid polygon error for 1D object
				m<-colMeans(pts)
				tmp$points<-tryCatch(ellipse(as.matrix(cov(pts)),centre=c(m[1],m[2]),level=ellipse.level),
					error=function(e){NA})
				tmp$color<-unique(as.character(.char[[i]][,1]))[1] # choose single value
				tmp$lwd<-unique(as.numeric(as.character(.char[[i]][,2])))[1]
				tmp$lty<-unique(as.numeric(as.character(.char[[i]][,3])))[1]
				tmp$border<-unique(as.character(.char[[i]][,4]))[1]
				tmp
			})
			
		# get size to plot smallest last
		ellipse.size<-sapply(1:length(ellipse.var),function(i)
			{
				tryCatch(areapl(ellipse.var[[i]]$points),error=function(e){NA})
			})
		
		plot.order<-order(ellipse.size,decreasing=TRUE)
		
		#plot
		for(i in 1:length(ellipse.var))
			{
				if(!is.na(ellipse.var[[plot.order[i]]]$points))
					{
						if(show.polygon==TRUE)
						{
							polygon(unlist(ellipse.var[[plot.order[i]]]$points[,1]),unlist(ellipse.var[[plot.order[i]]]$points[,2]),
								col=as.character(ellipse.var[[plot.order[i]]]$color),border = ellipse.var[[plot.order[i]]]$border,
								lwd=ellipse.var[[plot.order[i]]]$lwd,lty=ellipse.var[[plot.order[i]]]$lty)		
						}else{
							lines(unlist(ellipse.var[[plot.order[i]]]$points[,1]),unlist(ellipse.var[[plot.order[i]]]$points[,2]),
								col=as.character(ellipse.var[[plot.order[i]]]$color),border = ellipse.var[[plot.order[i]]],
								lwd=ellipse.var[[plot.order[i]]]$lwd,lty=ellipse.var[[plot.order[i]]]$lty)	
						}				
					}
			}
	}		
	
#plot polygon representation of point group
polygon.group<-function(obj,color,lwd=1,lty=1,border="#00000050",.level=.95,show.polygon=TRUE, alpha=.5)
	{
		check.get.packages("grDevices")#convex hull

		#check color and add extra transparency
		color<-alpha.col(color,alpha)	
		#split objs and inputs for each group based on color (color, lwd, lty  are all mapped together)
		tmp.obj<-as.data.frame(obj)
		tmp.char<-as.data.frame(cbind(color,lwd,lty,border))
		fct<-as.factor(color)
		.obj<-split(tmp.obj,fct)
		.char<-split(tmp.char,fct)

		#calculate points for 
		.var<-lapply(1:nlevels(fct),function(i)
			{
				tmp<-list()
				pts<-.obj[[i]]
				if(nrow(pts)<=2){pts<-NA} # avoid polygon error for 1D object
				tmp$points<-tryCatch(as.matrix(pts)[chull(as.matrix(pts)),],
					error=function(e){NA})
				tmp$color<-unique(as.character(.char[[i]][,1]))[1] # choose single value
				tmp$lwd<-unique(as.numeric(as.character(.char[[i]][,2])))[1]
				tmp$lty<-unique(as.numeric(as.character(.char[[i]][,3])))[1]
				tmp$border<-unique(as.character(.char[[i]][,4]))[1]
				tmp
			})
			
		# get size to plot smallest last
		.size<-sapply(1:length(.var),function(i)
			{
				tryCatch(areapl(.var[[i]]$points),error=function(e){NA})
			})
		
		plot.order<-order(.size,decreasing=TRUE)
		
		#plot
		for(i in 1:length(.var))
			{
				if(!is.na(.var[[plot.order[i]]]$points))
					{
						if(show.polygon==TRUE)
						{
							polygon(unlist(.var[[plot.order[i]]]$points[,1]),unlist(.var[[plot.order[i]]]$points[,2]),
								col=as.character(.var[[plot.order[i]]]$color),border = .var[[plot.order[i]]]$border,
								lwd=.var[[plot.order[i]]]$lwd,lty=.var[[plot.order[i]]]$lty)		
						}else{
							lines(unlist(.var[[plot.order[i]]]$points[,1]),unlist(.var[[plot.order[i]]]$points[,2]),
								col=as.character(.var[[plot.order[i]]]$color),border = .var[[plot.order[i]]],
								lwd=.var[[plot.order[i]]]$lwd,lty=.var[[plot.order[i]]]$lty)	
						}	
					}		
			}
	}		