

#function to add transparency to colors
alpha.col<-function(color,alpha)
	{
		#check to see if alpha already set
		
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

#interactively choose colors	
getcolors <- function(n){
	# from http://menugget.blogspot.com/2013/01/choosing-colors-visually-with-getcolors.html#more
	 N <- 6
	 
	 X <- seq(N^2)-0.5
	 Y <- seq(N)-0.5
	 Z <- matrix(0, nrow=length(X), ncol=length(Y))
	 
	 LEV <- seq(0,1,,N) 
	 R <- rep(LEV, each=N^2)
	 G <- rep(rep(LEV, each=N), N)
	 B <- rep(LEV, N^2)
	 
	 x11(width=6, height=6)
	 layout(matrix(1:2, nrow=2, ncol=1), widths=c(6), heights=c(1.5,4.5))
	 op <- par(mar=c(1,3,2,1))
	 
	 image(X,Y,Z, col=NA, xlab="", ylab="", xaxt="n", yaxt="n")
	 for(i in seq(Z)){
	  xs <- c(((i-1) %% N^2), ((i-1) %% N^2), ((i-1) %% N^2) + 1, ((i-1) %% N^2) + 1)
	  ys <- c(((i-1) %/% N^2), ((i-1) %/% N^2)+1, ((i-1) %/% N^2) + 1, ((i-1) %/% N^2))
	  polygon(xs, ys, col=rgb(R[i], G[i], B[i]), border=NA)
	 }
	 mtext(paste("Click on", n, "colors [please]"), side=3, line=0.5)
	 box()
	 
	 COLS <- NA*seq(n)
	 for(i in seq(n)){
	  coord <- locator(1)
	  red <- coord$y / N
	  green <- coord$x / N^2
	  blue <- (coord$x %% N) / N
	  #pos <- (round(coord$y-1) * N^2) + round(coord$x)
	  COLS[i] <- rgb(red, green, blue)
	 }
	 
	 par(mar=c(1,3,0,1))
	 pal <- colorRampPalette(c("black", "white"))
	 image(x=1:100, y=seq(n), z=matrix(rep(1:100,n), nrow=100, ncol=n), col=pal(100), xlab="", ylab="", xaxt="n", yaxt="n")
	 box()
	 for(i in seq(n)){
	  lines(x=c(1,100), y=c(i,i), col=COLS[i], lwd=4)
	 }
	 axis(2, at=seq(n))
	 
	 par(op)
	 
	 COLS
}

#add scale to plot
image.scale <- function(z, zlim, col = heat.colors(12),breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
  zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 poly <- vector(mode="list", length(col))
 for(i in seq(poly)){
  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
 }
 xaxt <- ifelse(horiz, "s", "n")
 yaxt <- ifelse(horiz, "n", "s")
 if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
 if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
 if(missing(xlim)) xlim=XLIM
 if(missing(ylim)) ylim=YLIM
 plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
 for(i in seq(poly)){
  if(horiz){
   polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
  }
  if(!horiz){
   polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
  }
 }
}

#map object to a color (save object to a legend "scatter.plot.legend" in envir = devium,, eventually make this defineable)
convert.to.color<-function(object,pallet="rainbow",alpha=.5,legend="color")
	{
		
		#function to add transparency to colors
		alpha.col<-function(color,alpha)
			{
				tmp <- col2rgb(color)/255 
				rgb(tmp[1,],tmp[2,],tmp[3,],alpha=alpha)
			}
		
		fct<-as.factor(unlist(object))
		out<-switch(pallet,
		rainbow	 	= 	rainbow(nlevels(fct),alpha=alpha)[fct],						
		heat 		= 	heat.colors(nlevels(fct),alpha=alpha)[fct],
		terrain 	= 	terrain.colors(nlevels(fct),alpha=alpha)[fct], 
		topo		= 	topo.colors(nlevels(fct),alpha=alpha)[fct],
		chromatic 	= 	cm.colors(nlevels(fct),alpha=alpha)[fct])
		
		#bind with factor for legend
		obj<-list(data.frame(factor = fct,color=out))
		names(obj)<-legend
		#save to legend 
		set.plot.legend(obj,name="scatter.plot.legend",env=devium)
		
		return(out)
	}