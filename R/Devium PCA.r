devium.pca.calculate<-function(pca.inputs=get("devium.pca.object",envir=devium),args.list=TRUE,return=NULL, plot=TRUE)
	{
		#port of imDEV source code optimized for GUI use
		#accepts list with the following arguments
		#pca.data<- character name of the data object (samples as rows)
		#pca.components numeric number of number of principal components
		#pca.algorithm see pcaMethods::pca for options
		#pca.center logical, mean center the data
		#pca.scaling character describing scaling method, see pcaMethods::prep for options
		
		#check for text or factor and add to subset
		tmp<-pca.inputs
		data.obj<-get(tmp$pca.data)
		data.obj<-data.obj[sapply(1:ncol(data.obj), function(i) {class(data.obj[,i])=="numeric"})]
		
		
		#adjust PCS if > than data
		PCs<-tmp$pca.components
		if(PCs> min(dim(data.obj))){PCs<-min(dim(data.obj))} # this should be done internally in the PCa fxn
		pca.results<-pcaMethods::pca(as.matrix(data.obj), method=tmp$pca.algorithm, nPcs=PCs, center=tmp$pca.center,scale=tmp$pca.scaling, seed=123)
		
		#results
		scores<-as.data.frame(pca.results@scores)
		loadings<-as.data.frame(pca.results@loadings)
		eigenvalues<-data.frame(eigenvalues=pca.results@R2)

		#add leverage and dmodX
		#bind between scores and loadings
		lev<-tryCatch(as.matrix(leverage(result)),error=function(e){"can not calculate"})
		dmodx<-tryCatch(as.matrix(DModX(result)),error=function(e){"can not calculate"})
		diagnostics<-tryCatch(data.frame(leverage=lev,DmodX=dmodx),error=function(e){data.frame(Error="not applicable")})

		#scree plot
		if(plot==TRUE){ make.scree.plot(eigenvalues) }
	
		
		#Determine placement of output for EXCEL
		if(!return=="list"){
			data.list<-list(eigenvalues,scores,diagnostics,loadings)
			list.names<-matrix(c("eigenvalues","scores","diagnostics","loadings"))
			start.row<-1;spacer<-1;start.col<-1
			direction<-"horizontal"

			#assign complete object to envir = devium
			assign("devium.pca.results",list(eigenvalues=eigenvalues, scores=scores, loadings=loadings, diagnostics=diagnostics,
			placement=list.placement.full(data.list,list.names,direction="horizontal",start.col,start.row,spacer)),envir=devium)
		}
		
		#get the name of the data
		if(return=="list"){
				return(list(pca.scores = scores, pca.loadings =  loadings,pca.eigenvalues = eigenvalues, pca.diagnostics = diagnostics))
			} else {
				name<-tmp$pca.data
				assign(paste(name,"pca.scores",sep="."),scores,envir=.GlobalEnv)
				assign(paste(name,"pca.diagnostics",sep="."),diagnostics,envir=.GlobalEnv)
				assign(paste(name,"pca.loadings",sep="."),loadings,envir=.GlobalEnv)
		}
	}


# generate a scree plot 
make.scree.plot<-function(eigenvalues)
	{
		pcaeigen<-eigenvalues
		#x11()
		par(mar=c(4,4,4,4.25))
		total<-sum(matrix(unlist(pcaeigen))*100)
		plot(c(1:nrow(pcaeigen)),matrix(unlist(pcaeigen))*100,type="l",main=paste("PCA Screeplot showing",round(total,0), "% explained variance"),
		lwd=2,xaxt="n",frame.plot=TRUE,xlab=paste("Principal components (n =",nrow(pcaeigen) ,")"),ylab="")#cbind(matrix(1:nrow(as.matrix(pcaeigen)))
		abline(v=seq(1,nrow(pcaeigen),by=1),lty=2,col="gray40")
		points(as.matrix(pcaeigen)*100,col="black",pch=21,bg="red",cex=2)
		mtext("% Explained Variance",side=2,line=3,col="red")
		abline(h=1,lty=2)
		total.var<-round(cumsum(matrix(unlist(pcaeigen)))*100,0)
		par(new=TRUE)
		plot(c(1:nrow(pcaeigen)),total.var,type="l",
		lwd=2,xaxt="n",yaxt="n",lty=1,ylab="",xlab="")
		abline(v=c(1:nrow(pcaeigen))[total.var>79][1],lty=2)
		points(total.var,col="black",pch=21,bg="blue",cex=2)
		axis(4)
		mtext("Total % explained variance",side=4,line=3,col="blue")
		axis(1,seq(1,nrow(pcaeigen),by=1))
		#---------------------------------------------------------------
	}

	
	test<-function(){
tmp<-list()
data(mtcars)

tmp$pca.algorithm<-"svd"
tmp$pca.components<-2
tmp$pca.center<-TRUE
tmp$pca.scaling<-"uv"
tmp$pca.data<-"mtcars"
pca.inputs<-tmp

output$PCA.results<-devium.pca.calculate(pca.inputs,return="list",plot=FALSE)

}
