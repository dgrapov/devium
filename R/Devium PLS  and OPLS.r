
#function to carry out PLS or orthogonal signal correction PLS (OSC-PLS), obsolete use make.OSC.PLS.model
OSC.correction<-function(pls.y,pls.data,comp=5,OSC.comp=4,validation = "LOO",progress=TRUE,cv.scale=FALSE,return.obj="stats",...){ 
	
	check.get.packages("pls")
	
	#initialize
	OSC.results<-list()
	OSC.results$data[[1]]<-pls.data
	OSC.results$y[[1]]<-pls.y # also add a place to store plsr options for record keeping
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = (OSC.comp+1), style = 3)}
	
	#need to iteratively fit models for each OSC
	for(i in 1:(OSC.comp+1)){ 
		
		data<-OSC.results$data[[i]]
		tmp.model<-plsr(OSC.results$y[[1]]~., data = data, ncomp = comp, validation = validation ,CV.scale=cv.scale,...)#
		ww<-tmp.model$loading.weights[,1]
		pp<-tmp.model$loadings[,1]
		w.ortho<- pp - crossprod(ww,pp)/crossprod(ww)*ww
		t.ortho<- as.matrix(pls.data) %*% w.ortho
		p.ortho<- crossprod(as.matrix(data),t.ortho)/ c(crossprod(t.ortho))
		Xcorr<- data - tcrossprod(t.ortho,p.ortho)
		
		#store results
		OSC.results$RMSEP[[i]]<-matrix(RMSEP(tmp.model)$val,ncol=2,byrow=TRUE) # multi Y RMSEP is bound by row 
		OSC.results$rmsep[[i]]<- RMSEP(tmp.model)$val[2,,comp+1]# CV adjusted rmsep for each y by column
		OSC.results$Q2[[i]]<-matrix(R2(tmp.model)$val,,byrow=TRUE)
		OSC.results$Xvar[[i]]<-drop(tmp.model$Xvar/tmp.model$Xtotvar)
		OSC.results$fitted.values[[i]]<-tmp.model$fitted.values
		OSC.results$scores[[i]]<-tmp.model$scores
		OSC.results$loadings[[i]]<-tmp.model$loadings
		OSC.results$loading.weights[[i]]<-tmp.model$loading.weights
		OSC.results$total.LVs[[i]]<-comp
		OSC.results$OSC.LVs[[i]]<-i-1 # account for first model not having any OSC LVs
		#initialize data for next round
		OSC.results$data[[i+1]]<-as.data.frame(Xcorr)
		
		#update timer
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		}
	
	if (progress == TRUE){close(pb)}
	if (return.obj=="model"){return(tmp.model)} else {	return(OSC.results)	}
}

#function to carry out PLS or orthogonal signal correction PLS (O-PLS) adapted from OSC.PLS adding predictions
make.OSC.PLS.model<-function(pls.y,pls.data,comp=5,OSC.comp=4,validation = "LOO",progress=TRUE,cv.scale=FALSE,return.obj="stats",train.test.index=NULL,...){ 
	
	check.get.packages("pls")
	
	#initialize
	OSC.results<-list()
	OSC.results$data[[1]]<-pls.data
	OSC.results$y[[1]]<-pls.y<-as.matrix(pls.y)
	if(!is.null(train.test.index)){ # objects fo predictions
			OSC.results$test.data[[1]]<-OSC.results$data[[1]][train.test.index=="test",]
			if(cv.scale==TRUE){
				OSC.results$test.y<-test.y<-as.matrix(OSC.results$y[[1]][train.test.index=="test",])
			} else {
				OSC.results$test.y<-test.y<-as.matrix(OSC.results$y[[1]][train.test.index=="test",])
			}		
			OSC.results$data[[1]]<-OSC.results$data[[1]][train.test.index=="train",]
			OSC.results$y[[1]]<-as.matrix(OSC.results$y[[1]][train.test.index=="train",])
	} 
	
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = (OSC.comp+1), style = 3)}
	
	#need to iteratively fit models for each OSC
	for(i in 1:(OSC.comp+1)){ 
			
		data<-OSC.results$data[[i]]
		tmp.model<-plsr(OSC.results$y[[1]]~., data = data, ncomp = comp, validation = validation ,scale=cv.scale,...)#
		ww<-tmp.model$loading.weights[,1]
		pp<-tmp.model$loadings[,1]
		w.ortho<- pp - crossprod(ww,pp)/crossprod(ww)*ww
		t.ortho<- as.matrix(data) %*% w.ortho
		p.ortho<- crossprod(as.matrix(data),t.ortho)/ c(crossprod(t.ortho))
		Xcorr<- data - tcrossprod(t.ortho,p.ortho)
		
		#prediction objects
		if(!is.null(train.test.index)){
			test.data<-OSC.results$test.data[[i]]
			predicted.mod<-	predict(tmp.model,newdata=test.data, ncomp=1:comp, comps=1:comp, type="response")
			OSC.results$predicted.Y[[i]]<-predicted.mod
			OSC.results$predicted.RMSEP[[i]]<-sapply(1:ncol(test.y), function(i){
				(sum((predicted.mod[,i]-test.y[,i])^2)/nrow(predicted.mod))^.5
			})
			t.tst<-as.matrix(test.data)%*%w.ortho
			p.tst <- crossprod(as.matrix(test.data), t.tst) / c(crossprod(t.tst))
			OSC.test.data <- as.matrix(test.data) - tcrossprod(t.tst, p.tst)
			OSC.results$test.data[[i+1]]<-OSC.test.data # for next round
		}
		
		#store results
		OSC.results$RMSEP[[i]]<-matrix(RMSEP(tmp.model)$val,ncol=2,byrow=TRUE) # multi Y RMSEP is bound by row 
		OSC.results$rmsep[[i]]<- RMSEP(tmp.model)$val[2,,comp+1]# CV adjusted rmsep for each y by column
		OSC.results$Q2[[i]]<-matrix(R2(tmp.model)$val,ncol=ncol(pls.y),byrow=TRUE)
		OSC.results$Xvar[[i]]<-drop(tmp.model$Xvar/tmp.model$Xtotvar)
		OSC.results$fitted.values[[i]]<-tmp.model$fitted.values
		OSC.results$scores[[i]]<-tmp.model$scores
		OSC.results$loadings[[i]]<-tmp.model$loadings
		OSC.results$loading.weights[[i]]<-tmp.model$loading.weights
		OSC.results$total.LVs[[i]]<-comp
		OSC.results$OSC.LVs[[i]]<-i-1 # account for first model not having any OSC LVs
		#initialize data for next round
		OSC.results$data[[i+1]]<-as.data.frame(Xcorr)
		
		#update timer
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		}
	
	if (progress == TRUE){close(pb)}
	if (return.obj=="model"){return(tmp.model)} else {	return(OSC.results)	}
}

#fit many OPLS models to overview optimal LV and OLV
optimize.OPLS<-function(max.LV=4,tolerance =0.01,pls.y,pls.data,validation = "LOO",method="oscorespls",cv.scale=T,...){

	#iterate and fit OSC models for each possible LV > 1
	out<-lapply(1:max.LV, function(i){
		mod<-OSC.correction(pls.y=pls.y,pls.data=pls.data,comp=i,OSC.comp=i,validation = validation,cv.scale=cv.scale,...)
		tmp<-data.frame(RMSEP=do.call("rbind",mod$RMSEP))
		tmp$LV<-i
		tmp$OLV<-rep(mod$OSC.LVs,each=(ncol(pls.y)*(i+1)))
		tmp$pls.y<-rep(1:ncol(pls.y), each=(i+1))
		#do not report partials
		get<-matrix(rep(0:i),nrow=nrow(tmp))
		tmp[get==max(get),]
	})
	obj<-do.call("rbind",out)
	
	#choose optimal combination of LV/OLV for all Ys
	choose.opt.OPLS.comp(obj=obj,pls.y=pls.y,tolerance=0.01)
}

#choose optimal model LV and OLV component number
choose.opt.OPLS.comp<-function(obj,pls.y,tolerance=0.01){
	

	tmp.list<-split(obj,obj$pls.y)
	
	results<-lapply(1:length(tmp.list), function(i){
		x<-tmp.list[[i]]
		RMSEP<-x[,1:2]
		comp<-x$LV
		ocomp<-x$OLV
		
		even<-1:ncol(RMSEP)%%2==0 # CV RMSEP currently assuming this was used in modeling
		tmp<-RMSEP[,even]# CV RMSEP
		is.min<-which.min(tmp)
		min.RMSEP<-tmp[is.min]
		#look for smaller model with in tolerance
		# not worse than this, accept smaller				
		delta<-tmp-min.RMSEP
		tmp.min<-which(delta<=tolerance)
		data.frame(x[c(tmp.min),], delta[tmp.min])

	})
	
	#choose smallest model within tolerance for both
	tmp<-do.call("rbind",results)
	x<-split(tmp$LV, tmp$pls.y )
	LV<-unlist(Reduce(intersect, x))
	x<-split(tmp$OLV, tmp$pls.y )
	OLV<-unlist(Reduce(intersect, x))
	
	#if there is an intersection
	if(length(LV)>0&length(OLV)>0){
		list(best=tmp[tmp$LV==min(LV)&tmp$OLV==min(OLV), ], LV=min(LV), OLV=min(OLV))
	} else {
		list(best=tmp, LV=tmp$LV[which.min(tmp$delta.tmp.min.)], OLV = tmp$OLV[which.min(tmp$delta.tmp.min.)])
	}		
}

#plot OSC results
plot.OSC.results<-function(obj,plot="RMSEP",groups=NULL){
	check.get.packages("ggplot2")
	#plot = one of: c("RMSEP","scores","loadings","delta.weights")
	#groups is a factor to show group visuyalization in scores plot
	switch(plot,
		RMSEP 			=  .local<-function(obj){
								#bind info and RMSEP
								comps<-obj$total.LVs
								ocomps<-obj$OSC.LVs
								plot.obj<-obj$RMSEP
								bound<-do.call("rbind",lapply(1:length(comps),function(i)
									{
										out<-as.data.frame(cbind(plot.obj[[i]][,1],c(0:comps[i]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
										colnames(out)<-c("RMSEP","component","model")
										out
									}))
								bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
								
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(),  
													plot.background = element_blank()
												 )
								#plot				 
								p<-ggplot(data=bound, aes(x=component, y=RMSEP,color=model)) + geom_line(size=1,alpha=.5) + geom_point(size=2)+.theme
								print(p)
							},
		scores 			=	.local<-function(obj){
								comps<-obj$total.LVs
								ocomps<-obj$OSC.LVs
								plot.obj<-obj$scores
								if(is.null(groups)){groups<-rep("gray",nrow(plot.obj))}
								bound<-do.call("rbind",lapply(1:length(comps),function(i)
									{
										out<-as.data.frame(cbind(plot.obj[[i]][,1:2],unlist(groups),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
										colnames(out)<-c("Comp1","Comp2","groups","model")
										out
									}))
								bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
								
								#calculate convex hull for polygons for each group
								data.obj <- split(bound, bound$model)
								tmp.obj <- lapply(1:length(data.obj), function(i){
									obj<-data.obj[[i]]
									s2<-split(obj,obj[,3])
									do.call(rbind,lapply(1:length(s2),function(j){
										tmp<-s2[[j]]
										tmp[chull(tmp[,1:2]),] 
										}))
								})
								chull.boundaries <- do.call("rbind", tmp.obj)
							
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(), 
													panel.border = element_rect(colour="gray",fill=NA),
													plot.background = element_blank()
												 )
												 
								#make plot
								p<-ggplot(data=bound, aes(x=Comp1, y=Comp2, group=groups,color=groups)) + #geom_density2d(aes(group=groups))+
								geom_hline(aes(yintercept=0),color="gray60",linetype="dashed")+geom_vline(aes(xintercept=0),color=I("gray60"),linetype=2)+facet_grid(. ~ model)
								p<-p+geom_polygon(data=chull.boundaries,aes(x=Comp1,y=Comp2,fill=groups),alpha=.5) +geom_point(size=2)+.theme
								print(p)
							},
		loadings 		= 	.local<-function(obj){ # will only plot first component for each model
							comps<-obj$total.LVs
							ocomps<-obj$OSC.LVs
							plot.obj<-obj$loadings
							bound<-do.call("rbind",lapply(1:length(comps),function(i)
								{
									out<-as.data.frame(cbind(plot.obj[[i]][,1:2],rownames(plot.obj[[i]]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
									colnames(out)<-c("Comp1","Comp2","variable","model")
									out
								}))
							bound[,1:2]<-as.numeric(as.matrix(bound[,1:2]))	
							
							#custom theme
							.theme<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												legend.position = "none",
												plot.background = element_blank()
											 )
							
							#make plot
							p<-ggplot(data=bound, aes(x=variable,y=Comp1, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
							facet_grid(. ~ model) +.theme
							print(p)
						},
		delta.weights 	= 	.local<-function(obj){ # will only plot first component for each model
							comps<-obj$total.LVs
							ocomps<-obj$OSC.LVs
							plot.obj<-obj$loading.weights
							bound<-do.call("rbind",lapply(2:(length(ocomps)),function(i)
								{
									out<-as.data.frame(cbind(plot.obj[[1]][,1]-plot.obj[[i]][,1],names(plot.obj[[i]][,1]),paste(comps[i]," LVs and ",ocomps[i]," OSC LVs",sep="")))
									colnames(out)<-c("delta_weight","variable","model")
									out
								}))
							bound[,1]<-signif(as.numeric(as.matrix(bound[,1])),3)	
							
							#theme
							.theme<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												legend.position = "none",
												plot.background = element_blank()
											 )
							#make plot
							p<-ggplot(data=bound, aes(x=variable,y=delta_weight, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
							facet_grid(. ~ model) +.theme
							print(p)
						}
						)				
	.local(obj)
	}

##OBSOLETE## see plot.PLS -plot results for a single model , for pls output..need to merge with opls
plot.PLS.results<-function(obj,plot="RMSEP",groups=data.frame(rep("NULL",nrow(obj$data))),comp1=1,comp2=2){
	require(ggplot2)
	#plot = one of: c("RMSEP","scores","loadings")
	#groups is a factor to show group visualization in scores plot
	
	switch(plot, # only looks right for single Y models!
		RMSEP 			=  .local<-function(obj,comp1,comp2){
								#bind info and RMSEP
								comps<-1:obj$ncomp
								obj$RMSEP<-as.data.frame(drop(RMSEP(obj))$val)
								plot.obj<-tryCatch(obj$RMSEP[2,] ,error= function(e){obj$RMSEP[1,]})# CV
								
								bound<-data.frame(component=comps,RMSEP=unlist(plot.obj[-1])) # not the intercept
								
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(),  
													plot.background = element_blank()
												 )
								#plot				 
								p<-ggplot(data=bound, aes(x=component, y=RMSEP)) + 
								geom_line(size=1,alpha=.5) + geom_point(size=2)+.theme +
								scale_x_continuous(breaks = c(min(comps):max(comps)))
								print(p)
							
								
							},
		scores 			=	.local<-function(obj,comp1,comp2){
								comps<-obj$ncomp
								plot.obj<-tryCatch(obj$scores[[1]][,c(comp1,comp2)],error=function(e){obj$scores[,c(comp1,comp2)]}) # not sure how to simply unclass and coerce to data.frame
								
								#format data
								out<-data.frame(plot.obj[,c(comp1,comp2)],join.columns(as.matrix(groups)))
								colnames(out)<-c("LV1","LV2","groups")
									
								out[,1:2]<-as.numeric(as.matrix(out[,1:2]))	
								
								#calculate convex hull for polygons for each group
								data.obj <- split(out, as.factor(unlist(groups)))
								tmp.obj <- lapply(1:length(data.obj), function(i){
									obj<-data.obj[[i]]
									s2<-split(obj,obj[,3])
									do.call(rbind,lapply(1:length(s2),function(j){
										tmp<-s2[[j]]
										tmp[chull(tmp[,1:2]),] 
										}))
								})
								chull.boundaries <- do.call("rbind", tmp.obj)
							
								#custom theme
								.theme<- theme(
													axis.line = element_line(colour = 'gray', size = .75), 
													panel.background = element_blank(), 
													panel.border = element_rect(colour="gray",fill=NA),
													plot.background = element_blank()
												 )
												 
								#make plot
								p<-ggplot(data=out, aes(x=LV1, y=LV2, group=groups,color=groups)) + 
								geom_hline(aes(yintercept=0),color="gray60",linetype="dashed")+ 
								geom_vline(aes(xintercept=0),color=I("gray60"),linetype=2) 
								p<-p+geom_polygon(data=chull.boundaries,aes(x=LV1,y=LV2,fill=groups),alpha=.5) +geom_point(size=2)+.theme
								print(p)
							},
							
		loadings 		= 	.local<-function(obj,comp1,comp2){ # will only plot first component for each model
							plot.obj<-cbind(unclass(unlist(obj$loadings)))
							weight<-unclass(unlist(obj$loading.weights[,ncol(obj$loading.weights)]))
							out<-data.frame(plot.obj,weight,parameter=rownames(obj$loadings))
							bound<-data.frame(melt(out))
						
							#
							#custom theme
							.theme<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												panel.border = element_rect(colour="gray",fill=NA),
												plot.background = element_blank(),
												legend.position = "none"
											 )
											 
							# add label color and sort on clusters
							#make plot
							p<-ggplot(data=bound, aes(x=parameter,y=value, fill=parameter)) + 
							geom_bar(stat = "identity") + coord_flip() + #geom_density2d(aes(group=groups))+
							facet_grid(. ~ variable) +.theme
							print(p)
						
						}
						)				
	.local(obj,comp1,comp2)
	}	
	
#recreating plots based on plot.PCA options with slight modifications (good example of a place to use oob, need to have helper function to switch top level inputs based on class and use generic plotter)
plot.PLS<-function(obj, plot = c("screeplot","scores","loadings","biplot"),xaxis=1,yaxis=2,size=3,groups=NULL, label=TRUE, legend.name =  NULL, font.size=5,group.bounds="ellipse"){
	require(ggplot2)
	#plot = one of: c("screeplot","scores","loadings","biplot","multi")
	#groups is a factor to show group visualization in scores plot
	
	
	
	local<-switch(plot, # only looks right for single Y models!
		RMSEP 			=  function(obj,...){
									
									.theme<- theme(
										axis.line = element_line(colour = 'gray', size = .75), 
										panel.background = element_blank(),  
										plot.background = element_blank()
									) 
									# RMSEP<-obj$RMSEP[length(obj$RMSEP)]][,ncol(obj$RMSEP[[1]])] # get for all LVs and optionally CV version
									# Q2<-obj$Q2[[length(obj$Q2)]][,ncol(obj$Q2[[1]])]
									# Xvar<-c(0,obj$Xvar[[length(obj$Xvar)]]) # 0 is for intercept only model
									
									RMSEP<-obj$RMSEP[,ncol(obj$RMSEP)] # get for all LVs and optionally CV version
									Q2<-obj$Q2[,ncol(obj$Q2)]
									Xvar<-c(0,obj$Xvar) # 
									
									LV<-paste0("LV ",1:length(RMSEP))
									tmp<-melt(data.frame(LV,RMSEP,Q2,Xvar))
									
									p<-ggplot(data=tmp ,aes(y=value,x=LV,fill=variable))+
									geom_bar(stat="identity",position=position_dodge())+.theme +ylab("value")+xlab("LV")
									print(p)
								
							},
		scores 			=	function(obj,color,size){
								comps<-obj$total.LVs[1]
								tmp.obj<-tryCatch(obj$scores[[comps]][,c(xaxis,yaxis)],error=function(e){obj$scores[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce to data.frame
					
								tmp<-data.frame(tmp.obj,id = rownames(tmp.obj))
								#plot 
								.theme2<- theme(
											axis.line = element_line(colour = 'gray', size = .75), 
											panel.background = element_blank(), 
											plot.background = element_blank(),
											legend.background=element_rect(fill='white'),
											legend.key = element_blank()
										 )
										 
								if(is.null(color)){
										tmp$color<-"gray"
									}else{
										tmp$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}
								
								points<-if(all(tmp$color=="gray")) { 
									geom_point(color="gray",size=size,alpha=.75,show_guide = FALSE) 
								} else { 
									geom_point(aes(color=color),size=size,alpha=.5)  
								}
								#labels
								tmp$lab.offset<-tmp[,2]-abs(range(tmp.obj[,2])[1]-range(tmp.obj[,2])[2])/50						
								labels<-if(label==TRUE){geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)} else { NULL }
								
								#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.2, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=.2, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj),tmp$color)# group visualization via 
									polygons<-if(is.null(color)){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.1, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=.1, show_guide = FALSE) 
										}
								}
								#making the actual plot 
								p<-ggplot(data=tmp,aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2])) + 
								geom_vline(xintercept = 0,linetype=2, size=.5, alpha=.5) + 
								geom_hline(yintercept = 0,linetype=2, size=.5, alpha=.5) +
								points +
								.theme2 + 
								labels +
								polygons +
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) 
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								print(p)
							},
		"loadings"		= function(obj,color,size){
							comps<-obj$total.LVs[1]
							tmp.obj<-tryCatch(obj$loadings[[comps]][,c(xaxis,yaxis)],error=function(e){obj$loadings[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce to data.frame
							tmp<-data.frame(tmp.obj,id = rownames(tmp.obj))
							#plot 
							.theme2<- theme(
										axis.line = element_line(colour = 'gray', size = .75), 
										panel.background = element_blank(), 
										plot.background = element_blank(),
										legend.background=element_rect(fill='white'),
										legend.key = element_blank()
									 )
							#check to make sure color length matches dim[1]
							if(is.null(color)){
									tmp$color<-"gray"
								}else{
									if(!length(color[,])==nrow(tmp)){tmp$color<-"gray"# reset if doesn't match
									} else { 
											tmp$color<-as.factor(color[,])
											if(is.null(legend.name)){legend.name<-colnames(color)}
									}
							}
							
							points<-if(all(tmp$color=="gray")) { 
								geom_point(color="gray",size=size,alpha=.75,show_guide = FALSE) 
							} else { 
								geom_point(aes(color=color),size=size,alpha=.5)  
							}
							#labels
							tmp$lab.offset<-tmp[,2]-abs(range(tmp.obj[,2])[1]-range(tmp.obj[,2])[2])/50						
							labels<-if(label==TRUE){geom_text(size=font.size,aes_string(x=colnames(tmp)[1], y="lab.offset",label="id"),color="black",show_guide = FALSE)} else { NULL }
							
							#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp$color)# group visualization via 
									polygons<-if(all(tmp$color=="gray")){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.2, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=.2, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj),tmp$color)# group visualization via 
									polygons<-if(all(tmp$color=="gray")){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.1, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=.1, show_guide = FALSE) 
										}
								}
							
							#making the actual plot 
							p<-ggplot(data=tmp,aes_string(x=colnames(tmp)[1], y=colnames(tmp)[2])) + 
							geom_vline(xintercept = 0,linetype=2, size=.5, alpha=.5) + 
							geom_hline(yintercept = 0,linetype=2, size=.5, alpha=.5) +
							points +
							.theme2 + 
							labels +
							polygons +
							scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
							scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) 
							if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
							print(p)
						},				
		"biplot"		= function(obj,color,size){
								comps<-obj$total.LVs[1]
								loadings<-tmp.loadings<-tryCatch(obj$loadings[[comps]][,c(xaxis,yaxis)],error=function(e){obj$loadings[,c(xaxis,yaxis)]}) # not sure how to simply unclass and coerce
								scores<-tmp.obj<-data.frame(tryCatch(obj$scores[[comps]][,c(xaxis,yaxis)],error=function(e){obj$scores[,c(xaxis,yaxis)]})) # not sure how to simply unclass and coerce to data.frame
								.theme2<- theme(
												axis.line = element_line(colour = 'gray', size = .75), 
												panel.background = element_blank(), 
												plot.background = element_blank()
											 )
								#based on https://groups.google.com/forum/#!topic/ggplot2/X-o2VXjDkQ8
								tmp.loadings[,1]<-rescale(loadings[,1], range(scores[,1]))
								tmp.loadings[,2]<-rescale(loadings[,2], range(scores[,2]))
								tmp.loadings<-data.frame(tmp.loadings,label=rownames(loadings))
								
								#using tmp.obj because no need for labels, and started badly need to rewrite
								#Adding Hoettellings T2 ellipse
								if(is.null(color)){
										tmp.obj$color<-"gray"
									}else{
										tmp.obj$color<-as.factor(color[,])
										if(is.null(legend.name)){legend.name<-colnames(color)}
								}	
								
							#group visualizations
								#Hoettellings T2 ellipse
								polygons<-NULL
								if(group.bounds=="ellipse"){		
									ell<-get.ellipse.coords(cbind(tmp.obj[,1],tmp.obj[,2]), group=tmp.obj$color)# group visualization via 
									polygons<-if(all(tmp.obj$color=="gray")){
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.2, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell$coords),aes(x=x,y=y, fill=group),linetype=2,alpha=.2, show_guide = FALSE) 
										}
								}
								
								if(group.bounds=="polygon"){
									ell<-get.polygon.coords(data.frame(tmp.obj),tmp$color)# group visualization via 
									polygons<-if(all(tmp.obj$color=="gray")){
											geom_polygon(data=data.frame(ell),aes(x=x,y=y), fill="gray", color="gray",linetype=2,alpha=.1, show_guide = FALSE) 
										} else {
											geom_polygon(data=data.frame(ell),aes(x=x,y=y, fill=group),linetype=2,alpha=.1, show_guide = FALSE) 
										}
								}
									
								points<-if(all(tmp.obj$color=="gray")) { 
									geom_point(data=data.frame(tmp.obj),aes_string(x=colnames(tmp.obj)[1], y=colnames(tmp.obj)[2]),color="gray",size=size,alpha=.75,show_guide = FALSE) 
								} else { 
									geom_point(data=data.frame(tmp.obj), aes_string(x=colnames(tmp.obj)[1], y=colnames(tmp.obj)[2],color="color"),size=size,alpha=.5)  
								}
								#plot
								p<-ggplot()+
								points +
								polygons+
								geom_segment(data=tmp.loadings, aes_string(x=0, y=0, xend=colnames(tmp.loadings)[1], yend=colnames(tmp.loadings)[2]), arrow=NULL, alpha=0.25)+
								geom_text(data=tmp.loadings, aes_string(x=colnames(tmp.loadings)[1], y=colnames(tmp.loadings)[2], label="label"), alpha=0.5, size=font.size)+
								scale_colour_discrete("Variety")+
								scale_x_continuous(paste(colnames(tmp)[1],sprintf("(%s%%)", round(obj$Xvar[xaxis],digits=2)*100),sep=" "))+
								scale_y_continuous(paste(colnames(tmp)[2],sprintf("(%s%%)", round(obj$Xvar[yaxis],digits=2)*100),sep=" ")) +
								.theme2
								if(!is.null(legend.name)) {p<-p+scale_colour_discrete(name = legend.name)}
								print(p)
							}
		)
							
		local(obj,color=groups,size)
	}		
	
#create PLS model
make.PLS.model<-function(y,data,pls.method="simpls",
		ncomp=2, CV="LOO",CV.segments=NULL,segment.type=NULL, CV.scale=FALSE, opt.comp=FALSE){
	#use opt.comp=TRUE to dynamically optimize the # of latent variables
	#minimum of 2 components 
	#need to switch based on CV specifications
	
	#make sure the number of supplied components won't error plsr
	if(ncomp>nrow(data)-1)
		{
			ncomp<-nrow(data)-1
			#cat("The number of components was changed to", ncomp, "to accommodate sample number","\n")
		}
	if(CV=="LOO")

	{	if(opt.comp==TRUE)
		{	
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
			new.comp<-c(2:ncomp)[which.max(R2(mod1)$val[-c(1:2)])]
			#cat("PCs were changed from",PCs,"to", new.comp)
			if(dim(as.data.frame(new.comp))[1]==0){new.comp<-2}
			mod1 <- plsr(y~ as.matrix(data), ncomp=new.comp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
		}else{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,scale=CV.scale)
		}
	}else{
		if(opt.comp==TRUE)
		{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
			new.comp<-c(2:ncomp)[which.max(R2(mod1)$val[-c(1:2)])]
			if(dim(as.data.frame(new.comp))[1]==0){new.comp<-2}
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
		}else{
			mod1 <- plsr(y~ as.matrix(data), ncomp=ncomp,
			data=as.data.frame(data) ,method=pls.method,validation=CV,segments=CV.segments,segment.type=segment.type,scale=CV.scale)
		}
	}

	mod1
	}
	
#extract OSC submodel from OSC results object
get.OSC.model<-function(obj,OSC.comp){
	#obj = results from OSC.correction()
	#OSC.comp = number of orthogonally corrected components
	
	
	index<-c(1:length(obj$OSC.LVs))
	id<-index[obj$OSC.LVs==OSC.comp]
	
	#extract and return
	out<-list()
	out$data<-obj$data[[id]]
	out$y<-obj$y
	out$RMSEP<-obj$RMSEP[[id]]
	out$Q2<-obj$Q2[[id]]
	out$Xvar<-obj$Xvar[[id]]
	out$scores<-obj$scores[[id]]
	out$loadings<-obj$loadings[[id]]
	out$loading.weights<-obj$loading.weights[[id]]
	out$total.LVs<-obj$total.LVs[[id]]
	out$OSC.LVs<-obj$OSC.LVs[[id]]
	return(out)
}
	
#calculate root mean squared error	
RMSE<-function(values,predictions){sqrt(sum((values-predictions)^2)/length(values))}

#model function
model.fxn<-function(data,inds,algorithm="pcr",y,ncomp=2,return="pred.error",...){
			mod<-do.call(algorithm,list(formula=y~.,data=data,ncomp=ncomp,subset=inds,...=...))
			switch(return,
			"pred.error" = .local<-function(){
								y-predict(mod, newdata=data,ncomp=ncomp,...)
							},
			"coef"		 = 	.local<-function(){
								c(coef(mod))
							}
					)
			.local()
		}

#bootstrap function
boot.fxn<-function(algorithm="pcr",data=tmp.data,y,ncomp=2,return="pred.error", R=499,...){
	library(boot);library(pls)
	boot(data,statistic=model.fxn,R=R,algorithm=algorithm,ncomp=ncomp,y=y,return=return,...=...)	
}

#generate boostrapped parameters for model (only works for single Y models)
boot.model<-function(algorithm="pcr",data=tmp.data,y,ncomp=2,return="pred.error",R=499,...){
	# function currently tailored to pls and tested with pcr and plsr
	# return can be one of c("pred.error","coef")
	# pred.error = out of bag (sample) error of prediction (RMSEP)
	# coef = bootstrapped coefficent weights 
	
	x<-boot.fxn(algorithm,data,y,ncomp,return,R,...)
	in.bag<-boot.array(x)
	out.bag<-in.bag==0
	switch(return,
	"pred.error" 	= .local<-function(){
							in.bag<-boot.array(x)
							oob.error<-mean((x$t^2)[in.bag==0])
							oob.error.sd<-sd((x$t^2)[in.bag==0])
							app.error<-MSEP(do.call(algorithm,list(formula=y~.,data=data,ncomp=ncomp,...=...)),ncomp=ncomp,intercept=FALSE)
							est.error<-sqrt(0.368*c(app.error$val) + 0.632 * oob.error)
							sd.error<-sqrt(0.368*c(app.error$val) + 0.632 * oob.error.sd)
							return(list(RMSEP=data.frame(bootstrapped.0.632_RMSEP = est.error,mean.error = mean(abs(x$t0)),sd.error=sd(abs(x$t0))),boot.obj = x))
						},
	"coef" 			= .local<-function(){ 
							
							return(list(coef=data.frame(bootstrapped.coef=x$t0,CI.95.percent=t(apply(x$t,2,quantile, c(0.025,.975)))),boot.obj = x))
						}
			)	
	.local()	
}

#function to evaluate performance at various thresholds based on bootstrapped results for full model for feature selection
feature.cut<-function(obj,type="quantile",cuts=seq(0.05,.95,.01),separate=FALSE, plot=TRUE, parallel=FALSE){
	# type can be one of c("number", "quantile")
	# cuts are used to split feature set for model evaluation 
	# for quantile give probabilities, and for number the number of top features (abs coef magnitude)
	# separate determines if positive and negative coefficients are tested together (FALSE) or seperately (TRUE)
	# if plot = TRUE the feature number to cut level relationship will be plotted
	# if parallel = TRUE will default to using snow (later add multicluster support)
	# results are a list of inices for features returned at the specified cut
	
	#work function
			switch(type,
				"quantile"	 = .local<-function(cut,separate){
									
									bound<-1-cut
									
									if(separate==TRUE){
										pos.cut<-quantile(obj[obj>0],prob=bound)
										neg.cut<- -quantile(abs(obj[obj<0]),bound)
										#need to take half for extraction from both pos and negative seperately
										pos.var<-data.frame(index=c(1:length(obj))[obj>=pos.cut],value=obj[obj>=pos.cut])[order(obj[obj>=pos.cut],decreasing=TRUE),]
										neg.var<-data.frame(index=c(1:length(obj))[obj<=neg.cut],value=obj[obj<=neg.cut])[order(obj[obj<=neg.cut],decreasing=FALSE),]
										
										#return the correct proportion of samples 
										#pos.n<-c(1:nrow(pos.var))[c(1:nrow(pos.var))>=quantile(c(1:nrow(pos.var)),bound)]
										#neg.n<-c(1:nrow(neg.var))[c(1:nrow(neg.var))>=quantile(c(1:nrow(pos.var)),bound)]
										c(pos.var[,1],neg.var[,1])
										
									} else {
										all.cut<-quantile(abs(obj),bound)
										c(1:length(obj))[abs(obj)>=all.cut]
									}
								},
				"number" 	= .local<-function(cut,separate){
		
									if(separate==TRUE){
										tmp<-obj
										c(c(1:length(tmp))[order(tmp,decreasing=TRUE)][1:ceiling(cut/2)],
										c(1:length(tmp))[order(tmp,decreasing=TRUE)][c(length(tmp)-floor(cut/2)):length(tmp)])
									} else {
										tmp<-obj
										c(1:length(tmp))[order(abs(tmp),decreasing=TRUE)][1:cut]
									}
								}
					)
					
			if (parallel == TRUE){
				check.get.packages(c("snow","doSNOW","foreach"))
				#start cluster
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				
				#
				out<-list()		
				out<-foreach(i=c(1:length(cuts))) %dopar% .local(cut=cuts[i],separate=separate)
				stopCluster(cl.tmp)	
			} else {
				out<-list()
				for(i in 1:length(cuts)){
					out[[i]]<-.local(cut=cuts[i],separate=separate)
				}
			}
			
			if(plot == TRUE){
				check.get.packages("ggplot2")
				val<-data.frame(cuts=cuts,features=sapply(out,length))
				
				#protect against fatal error due to ggplots2
				if(nrow(val)>1){
				p<-ggplot(data=val, aes(y=features,x=cuts,fill=features)) + geom_bar( stat="identity")
				print(p)
				}
			}
		return(out)
			
}

#function to bootstrap many models
multi.boot.model<-function(algorithm="pcr",data=data,y,
							feature.subset=NULL,ncomp=4,return="pred.error",R=10,
							parallel=FALSE,plot=TRUE,...){
		
		.local<-function(var.id,...){
			boot.model(algorithm=algorithm,data=data[,var.id],y=y,ncomp=ncomp,return=return,R=R,...)[[1]] # no boot obj returned
		}
		
		if (parallel == TRUE){
				check.get.packages(c("snow","doSNOW","foreach"))
				#start cluster
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				
				#
				out<-list()		
				out<-foreach(i=c(1:length(feature.subset)),.combine="rbind") %dopar% .local(var.id=feature.subset[[i]])
				stopCluster(cl.tmp)	
			} else {
				pb <- txtProgressBar(min = 0, max = length(feature.subset), style = 3)
				out<-do.call("rbind",lapply(1:length(feature.subset),
								function(i){
								setTxtProgressBar(pb, i)
								.local(var.id=feature.subset[[i]])})) #,...
				dimnames(out)<-list(c(1:length(feature.subset)),c("bootstrapped.0.632_RMSEP", "mean.error", "sd.error"))
				close(pb)
			}
			
			#output
			val<-data.frame(feature.set=c(1:nrow(out)),number.of.features=sapply(feature.subset,length),RMSEP_0.632=as.numeric(out[,1]), 
							mean.error = out[,2] )
							
			#calculate loess mins
			get.lo<-function(){
					lo <- loess(RMSEP_0.632 ~ number.of.features, data=val)
					which.min(predict(lo,new.data=val))}
			lo.min<-tryCatch(get.lo(),error=function(e){NULL})		
			
			out<-list()
			out$results<-val
			out$loess.min<-lo.min
			out$RMSEP_0.632.min<-val$number.of.features[which.min(val$RMSEP_0.632)]
			
			if(plot == TRUE){
				check.get.packages(c("ggplot2","reshape"))
				
				plot.obj<-data.frame(melt(val[,-c(1:2)]),features=rep(val$number.of.features,(ncol(val)-2)))
				print(plot.obj)
				#make plot
				p<-ggplot(data=plot.obj, aes(y=value, x=features,group=variable,color=variable, fill=variable)) + 
				xlab("number of features")  
				
				if(length(lo.min)>0){
					p<-p+stat_smooth(level = 0.95,size=.75,alpha=.15,legend=FALSE) + 
					geom_vline(xintercept = out$RMSEP_0.632.min,lty=2,col="red") +
					ggtitle(paste("minimum at ",out$RMSEP_0.632.min," features"))+ 
					geom_point(size=3,alpha=.75)  
				} else {
					p<-p+geom_point(size=3,alpha=.75)
				}
				print(p)
			}
			return(out)
}

#make bar plot for weights or loadings
feature.bar.plot<-function(feature.set,weights.set,extra.plot=NULL){
	#feature.set = index for selected features
	#all weights to plot as a bar graph
	#show.all determines if only selected or all features are ploted
		
		#Bargraphs
		#custom theme
		.theme<- theme(
							axis.line = element_line(colour = 'gray', size = .75), 
							panel.background = element_blank(), 
							#legend.position = "none",
							plot.background = element_blank()
						 )
		#plotting data
		bound<-data.frame(weights=weights.set,
						variable=c(1:length(weights.set)),
						show = c(1:length(weights.set))%in%feature.set)
		
		#cut offs
		cuts<-range(bound$weights[!bound$show])
		plot.title<- paste ("upper/lower bounds = ", signif(cuts[2],4), " / " ,signif(cuts[1],4))
		#plot.colors<-scale_fill_brewer(palette="Blues")
		
		#make plot of variable and weight
		p<-ggplot(data=bound, aes(x=variable,y=weights, fill=show)) +
		geom_bar(stat = "identity") + #geom_density2d(aes(group=groups))+
		.theme +geom_hline(yintercept = cuts,lty=2,col="red") +
		 labs(title = plot.title, fill= "Selected") #+
		#plot.colors
	
		
		# sorted weight
		sorted.bound<-bound[order(bound$weights),]
		sorted.bound$variable<-c(1:length(bound$weights))
		
		#theme
		.theme2<- theme(
			axis.line = element_line(colour = 'gray', size = .75), 
			panel.background = element_blank(), 
			legend.position = "none",
			plot.background = element_blank()
		 )		
						 
		p2<-ggplot(data=sorted.bound, aes(x=variable,y=weights, fill=show)) +
		geom_bar(stat = "identity") + xlab(" ") + #geom_density2d(aes(group=groups))+
		.theme2 + geom_hline(yintercept = cuts,lty=2,col="red")# +
		#plot.colors
		#print(p2)
		
		#print plots
		if(is.null(extra.plot)){
				multiplot (p,p2, plotlist=NULL, cols=1)		
			} else {
				multiplot (extra.plot,p,p2, plotlist=NULL, cols=1)	
			}
	}

#create S-plot for variable loadings and conduct significance testing woth FDR to determine optimal feature selection cut off
make.S.plot<-function(pls.data,pls.scores,pls.loadings, cut.off=0.05, FDR=TRUE,plot=TRUE,...){
	
	check.get.packages("ggplot2")
	
	#pls.data 	= data used for model
	#scores 	= scores for selected (1st) component
	#loadings 	= loadings for selected (1st) component
	#plot	 	= make S-Plot
	#cut.off 	= select optimal features based on a test of the significance of the correlation (pearsons) between variable and scores
	#FDR 		= use q-value as the cut off 
	#... 		= can specify correlation type = c("pearson","spearman","biweight")
	
	# calculate p(corr) or correlation between scores and the original variable
	cor.mat<-devium.calculate.correlations(cbind(pls.scores,pls.data),...) #
	corrs<-cor.mat$cor[-1,1]
	p.vals<-cor.mat$p.value[-1,1]
	
	#false discovery rate correction
	if(FDR==TRUE){
			#p.vals<-FDR.adjust(p.vals,type="pvalue",return.all=TRUE)$qval # 
			p.vals<-p.adjust(p.vals, method="BH")
		}
		
	#index to draw visualization	
	show<-p.vals
	show[]<-1
	if(is.numeric(cut.off)){
			show[p.vals>cut.off]<-0
		} 
	
	#make plot	
	plot.obj<-data.frame(pcorr=corrs,loadings=pls.loadings,value=p.vals, significant=as.logical(show))
	
	if(plot==TRUE){
		#theme
		.theme<- theme(
							axis.line = element_line(colour = 'gray', size = .75), 
							panel.background = element_blank(), 
							#legend.position = "none",
							plot.background = element_blank()
						 )
		
		#cut offs
		selected<-plot.obj$significant==1
		plot.title<- paste (sum(selected)," selected features or ",round(sum(selected)/length(pls.loadings)*100,0),"%",sep="")
		
		
		#make plot of variable and weight
		p<-ggplot(data=plot.obj, aes(x=loadings,y=pcorr, color=significant)) +
		geom_point(stat = "identity",alpha=.75) + #geom_density2d(aes(group=groups))+
		.theme + labs(title = plot.title, fill= "Selected") 
		print(p)
	} else { p<-"NULL"}
	
	return(list(plot=p,selected.data=pls.data[,plot.obj$significant==1],feature.info=plot.obj))
}

#feature select using a combination of analyte correlation to scores (S-plot) and feature weights
PLS.feature.select<-function(pls.data,pls.scores,pls.loadings,pls.weight,plot=TRUE,p.value=0.05, FDR=TRUE,
		cut.type="quantile",top=0.95,separate=TRUE,...){
		#combined args from
		#feature.cut() & make.S.plot()
		#cuts is a single value which is a propability for type = quantile or integer for number
		
		#first selection criteria based on magnitude of model weight
		weight.cut<-feature.cut(obj=pls.weight,type=cut.type,cuts=top,separate=separate,plot=FALSE)
		weight.cut.selected<-data.frame(selected.weights=rep(0,length(pls.weight)))
		weight.cut.selected[unlist(weight.cut),]<-1
		
		#second selection criteria based on variable correlation with scores
		cor.cut<-make.S.plot(pls.data=pls.data,pls.scores=pls.scores,pls.loadings=pls.loadings,cut.off=p.value, FDR=FDR,plot=FALSE,...)
		
		#combine and plot
		combo.cut<-data.frame(model.weight=pls.weight,weight.cut.selected, cor.cut$feature.info)
		combo.cut$combined.selection<-combo.cut$significant&combo.cut$selected.weights==1
		
		#create updated S-plot
		#cut offs
		plot.obj<-combo.cut
		selected<-plot.obj$combined.selection==1
		plot.title<- paste (sum(selected)," selected features or ",round(sum(selected)/length(pls.loadings)*100,0),"%",sep="")
		
		#theme
		.theme<- theme(
						axis.line = element_line(colour = 'gray', size = .75), 
						panel.background = element_blank(), 
						legend.position = "none",
						plot.background = element_blank()
					 )
	
		
		#make plot of variable and weight
		p<-ggplot(data=plot.obj, aes(x=loadings,y=pcorr, color=combined.selection)) +
		geom_point(stat = "identity",alpha=.75) + #geom_density2d(aes(group=groups))+
		.theme + labs(title = plot.title, fill= "Selected")
		
		
		#plot results
		feature.bar.plot(feature.set=c(1:length(combo.cut$model.weight))[combo.cut$combined.selection],weights.set=combo.cut$model.weight, extra.plot=p)
		
		#return results
		return(as.data.frame(combo.cut))
	}	

#permute PLS model
permute.PLS<-function(data,y,n=10,ncomp,...){# could be made parallel
	#permuted Y
	perm.y<-lapply(1:n,function(i)
			{
				apply(y,2,gtools::permute)
			})

	#generate permuted models		
	model<-lapply(1:n,function(i)
			{
				# cat("permuting model",i,"\n")
				model<-make.PLS.model(y=perm.y[[i]],data,ncomp=ncomp,...)
				#get stats
				q2<-R2(model)$val[,,ncomp+1]
				rx2<-drop(model$Xvar/model$Xtotvar)[ncomp]
				pred.val<-model$fitted.values[,,ncomp]
				rmsep<-pls::RMSEP(model)$val[2,,ncomp] # take CV adjuste RMSEP
				list(Q2=q2,RX2=rx2,RMSEP=rmsep)#,predicted=pred.val,actual=perm.y[[i]])
			})
	
	tmp<-matrix(unlist(do.call("rbind",model)),ncol=3) 
	colnames(tmp)<-c("Q2","Xvar","RMSEP")
	means<-apply(tmp,2,mean)
	sds<-apply(tmp,2,sd)
	summary<-paste(signif(means,4)," ± ", signif(sds,3))
	return(list(permuted.values=tmp, mean = means, standard.deviations = sds, summary = summary))
}	

#permute OSC-PLS model
permute.OSC.PLS<-function(data,y,n=10,ncomp,OSC.comp=1,train.test.index=NULL,...){ # should be made parallel
	
	
	#permuted Y
	perm.y<-lapply(1:n,function(i)
			{
				apply(y,2,gtools::permute)
			})

	#generate permuted models		
	model<-lapply(1:n,function(i)
			{
				# cat("permuting model",i,"\n")
				if(!is.null(train.test.index)) {tmp.train.test.index<-train.test.index[,i,drop=FALSE]} else {tmp.train.test.index<-train.test.index}
				model<-make.OSC.PLS.model(pls.y=as.matrix(perm.y[[i]]),pls.data=data,comp=ncomp,OSC.comp=OSC.comp,train.test.index=tmp.train.test.index,...) #,...
				#get stats
				q2<-model$Q2[[OSC.comp+1]][ncomp+1,,drop=FALSE]# cv adjusted 
				rx2<-model$Xvar[[OSC.comp+1]][ncomp]
				pred.val<-as.matrix(model$fitted.values[[OSC.comp+1]][,,ncomp])
				rmsep<-model$rmsep[[OSC.comp+1]]# take CV adjusted RMSEP (see true below RMSEP)
				if(!is.null(train.test.index)) {
					rmsep<-model$predicted.RMSEP 
				}
				list(Q2=q2,RX2=rx2,RMSEP=rmsep)#,predicted=pred.val,actual=perm.y[[i]])
			})
	
	tmp<-matrix(unlist(do.call("rbind",model)),ncol=3) 
	colnames(tmp)<-c("Q2","Xvar","RMSEP")
	means<-apply(tmp,2,mean)
	sds<-apply(tmp,2,sd)
	summary<-paste(signif(means,4),"±", signif(sds,3))
	return(list(permuted.values=tmp, mean = means, standard.deviations = sds, summary = summary))
}	

#statistical test to compare permuted distrubution to model performance
OSC.validate.model<-function(model, perm, train= NULL) {
#model must be object generated with OSC.correction, the stats for largest LV/OLV model will be used
#perm must be object generated with permute.OSC.PLS
# if train = NULL perform a one-sample t-test to test if model stat comes from permuted distibution
# else perform a two sample t-test to compare train/test to permuted stats

	if(is.null(train)){
		tmp<-c("Q2","Xvar","RMSEP")	
		p.vals<-do.call("cbind",sapply(1:length(tmp), function(i) {	
				val<-model[[tmp[i]]]
				val<-as.matrix(val[[length(val)]])
				val<-val[nrow(val),ncol(val)]
				per.val<-tryCatch(perm$permuted.values[,tmp[i]], error=function(e) {"not found"}) 
				data.frame(matrix(c(val,tryCatch(t.test(per.val,mu=val)$p.value, error=function(e) {1})),ncol=1)) #force error = insiginificant 
		}))
		
		#make output in table form
		res<-data.frame(rbind(signif(p.vals[1,],4),perm$summary,signif(p.vals[2,],4)))
		dimnames(res)<-list(c("model","permuted model","p-value"), tmp)
	} else {
		tmp<-c("Q2","RMSEP")	
		#need to summarize results from train objects
		p.vals<-sapply(1:length(tmp), function(i) {	
				val<-train$performance[,tmp[i]]
				per.val<-tryCatch(perm$permuted.values[,tmp[i]], error=function(e) {"not found"}) 
				tryCatch(t.test(per.val,val)$p.value, error=function(e) {1}) #force error = insiginificant 
		})
		res<-data.frame(rbind(train$summary,perm$summary[c(1,3)],signif(p.vals,4))) # don't include Xvar
		dimnames(res)<-list(c("model","permuted model","p-value"), tmp)
	}
	return(res)
}

#conduct train/test validations on PLS model
PLS.train.test<-function(pls.data,pls.y,pls.train.index,comp,...) 
	{
		pls.y<-as.matrix(pls.y)
		#order for merging test with train stats in one object
		new.order<-c(c(1:nrow(pls.data))[pls.train.index=="train"],c(1:nrow(pls.data))[pls.train.index=="test"])
		back.sort<-order(new.order)

		train.y<-train.real<-pls.y[pls.train.index=="train",]
		train.data<-pls.data[pls.train.index=="train",]
		#all arguments gave been preset elsewhere
		test.pls.results<-make.PLS.model(train.y,train.data,ncomp=comp,...)
		Q2<-unlist(R2(test.pls.results)$val[,,max(comp)+1])

		train.pred<-test.pls.results$fitted.values[,,comp]
		
		#use model to predict test values
		test.data<-as.matrix(pls.data[pls.train.index=="test",])
		test.pred<- as.data.frame(predict(object=test.pls.results, newdata=test.data, ncomp = c(1:comp), comps=c(1:comp),type ="response"))

		test.real<-pls.y[pls.train.index=="test",]
		RMSEP<-sapply(1:ncol(pls.y), function(i){
			(sum((test.pred[,i]-test.real[,i])^2)/nrow(test.pred))^.5
		})

		#results
		predicted.y<-rbind(train.pred,test.pred)
		actual.y<-rbind(train.real,test.real)
		test.index<-pls.train.index
		res<-list(predicted.y[back.sort,], actual.y[back.sort,], test.index,RMSEP,Q2,LVs=PCs)
		names(res)<-c("predicted.y","actual.y=","pls.train.index","RMSEP","Q2","LVs")
		return(res)
	}

#conduct train/test validations on O-PLS model	
OSC.PLS.train.test<-function(pls.data,pls.y,train.test.index,comp,OSC.comp,...) 
	{
		pls.y<-as.matrix(pls.y)
		results<-lapply(1:ncol(train.test.index), function(i){
			pls.train.index<-as.matrix(train.test.index[,i])
			#order for merging test with train stats in one object
			new.order<-c(c(1:nrow(pls.data))[pls.train.index=="train"],c(1:nrow(pls.data))[pls.train.index=="test"])
			back.sort<-order(new.order)

			train.y<-train.real<-pls.y[pls.train.index=="train",]
			train.data<-pls.data[pls.train.index=="train",]
			test.real<-pls.y[pls.train.index=="test",]
			#all arguments gave been preset elsewhere
			test.pls.results<-make.OSC.PLS.model(pls.y=pls.y,pls.data=pls.data,comp=comp,OSC.comp=OSC.comp, train.test.index=pls.train.index,...)
			Q2<-test.pls.results$Q2[[OSC.comp+1]][comp,]
			
			#fitted values
			train.pred<-test.pls.results$fitted.values[[OSC.comp+1]][,,comp]
			test.pred<-test.pls.results$predicted.Y[[OSC.comp+1]]
			RMSEP<-test.pls.results$predicted.RMSEP[[OSC.comp+1]]
			#results
			predicted.y<-rbind(as.matrix(train.pred),as.matrix(test.pred))
			actual.y<-rbind(as.matrix(train.real),as.matrix(test.real))
			test.index<-pls.train.index
			res<-list(predicted.y[back.sort,], actual.y[back.sort,], test.index,RMSEP,Q2,LVs=comp)
			names(res)<-c("predicted.y","actual.y","pls.train.index","RMSEP","Q2","LVs")
			return(res)
		})
		
		#need to summarize results
		aggregated<-matrix(t(sapply(1:length(results),function(i){
			c(results[[i]]$Q2, results[[i]]$RMSEP)
		})),ncol=2)
		colnames(aggregated)<-c("Q2","RMSEP")
		
		aggregated.summary<-matrix(paste(signif(apply(aggregated,2,mean),4),"±",signif(apply(aggregated,2,sd),3)),nrow=1)
		colnames(aggregated.summary)<-c("Q2","RMSEP")
		list(full.results=results, performance=aggregated, summary=aggregated.summary)
	}	

# function for splitting dataset in to test and trainning sets
test.train.split<-function(nsamples, n=1, strata=NULL, prop.train=2/3, split.type="random",data=NULL){
	#nsamples the number of samples in the data set to split among test and trainnig data sets
	#n the number ot test/training splits to return
	#strata factor within whose levels the test/trainning sets will be derived
	#prop.train the proportion of samples in the trainning set
	#split.type how sample assignment to trainning/test splits is determined,  the options are "random", "duplex"
	#data needed for duplex method
	res<-lapply(1:n,function(i){
		if(is.null(strata)){
			if(split.type=="random"){
				t.num<-ceiling(nsamples*prop.train)
				test.train.id<-rep("test",nsamples)
				test.train.id[sample(c(1:length(test.train.id)),t.num)]<-"train"
			}
			
			if(split.type=="duplex"){ # takes the floor, if the sample number is not even the proportion of trainning samples may be one less than specified
				object<-ken.sto2(data, per = "TRUE",per.n= (1-prop.train),va = "TRUE",num=1)
				object<-duplex.select(data,object,percent.in.test=(1-prop.train))	
				test.train.id<-rep("train",nrow(data))
				test.train.id[object$`Chosen validation row number`]<-"test"	
			}	
		} else {
			if(split.type=="random"){
				tmp<-split(1:nsamples,strata)
				train.id<-unlist(sapply(1:length(tmp),function(i){
						t.num<-ceiling(length(tmp[[i]])*prop.train)
						tmp[[i]][sample(c(1:length(tmp[[i]])),t.num)]
					}))
				test.train.id<-rep("test",nsamples)
				test.train.id[train.id]<-"train"
			}
			
			if(split.type=="duplex"){ # trainning/test assignments are underestimated for odd number of samples (due rounding down)
				tmp<-split(data,strata)
				test.id<-fixlc(sapply(1:length(tmp),function(i){
					object<-ken.sto2(tmp[[i]], per = "TRUE",per.n= (1-prop.train),va = "TRUE",num=1)
					object<-duplex.select(tmp[[i]],object,percent.in.test=(1-prop.train))	
					object$`Chosen validation sample names`
					}))
				test.train.id<-rep("train",nrow(data))
				test.train.id[rownames(data)%in%test.id]<-"test"	
			}	
				
		}
		test.train.id
	})
	as.data.frame(do.call("cbind",res))
}	

#function for carrying out test/trainning split based on duplex or kennard-stone method 
ken.sto2<-function(inp, per = "TRUE", per.n = 0.3, num = 7, va = "TRUE")
 {
	#based on  ken.sto in package "soil.spec"
	#changes: altered number of PCs selection
	#took out saving, plotting
	#opened slot for custom PCA analysis options
	#fixed some bugs 

    if (class(inp) != "data.frame" & class(inp) != "matrix") 
	{
        stop("Invalid argument: 'inp' has to be of class 'data.frame' or 'matrix'.")
    	}
    if (per != "TRUE" & per != "FALSE") 
	{
        stop("Invalid argument: 'per' has to be either 'TRUE' or 'FALSE'.")
    }

    if (per == "TRUE")
	 {
        if (class(per.n) != "numeric") 
		{
            stop("Invalid argument: 'per' has to be of class 'numeric'.")
        	}

        if (per.n < 0 | per.n > 1) 
		{
            stop("Invalid argument: 'per' has to be between 0 and 1.")
        	}
        n <- round(per.n * nrow(inp), 0)
    }

    if (per == "FALSE") 
	{
        if (class(as.integer(num)) != "integer") 
		{
            	stop("Invalid argument: 'num' has to be of class 'integer'.")
       	 }

        if (num <= 0) 
	 {
            stop("Invalid argument: 'num' has to be between 1 and the number of samples minus one.")
        }

        if (num >= nrow(inp)) 
		{
           	 stop("Invalid argument: 'num' has to be between 1 and the number of samples minus one.")
        	}

        n <- num
   	 }
    if (va != "TRUE" & va != "FALSE") 
	{
        stop("Invalid argument: 'va' has to be either 'TRUE' or 'FALSE'.")
   	 }
   
    #allow to use specified PCA analysis results
    pca <- prcomp(inp, scale = T)
    prco <- as.data.frame(pca$x)
    cpv <- summary(pca)[[6]][3,]
    zzz <- matrix(nrow = 1, ncol = (length(cpv)-2))
    for (i in 1:(ncol(zzz)-2)) 
	{
        e <- (cpv[i] + 0.04) < cpv[i + 3]
        zzz[i] <- e
    	}
    pc <- (which(zzz == FALSE) - 1)[1]
    if (pc == 1|is.na(pc)) 
	{
        pc <- 2
    	}
    prco <- prco[, 1:pc]
    min <- c(rep(1, ncol(prco)))
    max <- c(rep(1, ncol(prco)))
    for (i in 1:ncol(prco)) 
	{
        blub <- which(prco[, i] == min(prco[, i]))
        min[i] <- blub[1]
        bla <- which(prco[, i] == max(prco[, i]))
        max[i] <- bla[1]
   	 }
    min <- rownames(prco)[min]
    max <- rownames(prco)[max]
    start <- unique(c(min, max))
    start.n <- match(start, rownames(inp))

    if (va == "FALSE") 
    {
        euc <- as.data.frame(as.matrix(dist(prco)))
        inp.start <- rownames(prco)[-start.n]
        inp.start.b <- inp.start
        cal <- start
	  stop<-min(c(n,length(start)))
        for (k in 1:(stop)) 
		{
            test <- apply(euc[inp.start.b, cal], 1, min)
            bla <- names(which(test == max(test)))
            cal <- c(cal, bla)
            inp.start.b <- inp.start.b[-(which(match(inp.start.b, 
                bla) != "NA"))]
        	}
       cal.n <- match(cal, rownames(inp))
	
       output <- list(`Calibration and validation set` = va, 
            `Number important PC` = pc, `PC space important PC` = prco, 
            `Chosen sample names` = unique(cal), `Chosen row number` = unique(cal.n), 
            `Chosen calibration sample names` = "NULL", `Chosen calibration row number` = "NULL", 
            `Chosen validation sample names` = "NULL", `Chosen validation row number` = "NULL")
    }

    if (va == "TRUE") 
    {
	  n<-ceiling(per.n*nrow(inp))
        cal.start <- start
        cal.start.n <- start.n
        val.min <- c(rep(1, ncol(prco)))
        val.max <- c(rep(1, ncol(prco)))
        for (i in 1:ncol(prco)) 
		{
            blub <- which(prco[-cal.start.n, i] == min(prco[-cal.start.n, 
                i]))
            val.min[i] <- blub[sample(length(blub), 1)]
            bla <- which(prco[-cal.start.n, i] == max(prco[-cal.start.n, 
                i]))
            val.max[i] <- bla[sample(length(bla), 1)]
       	 }
        val.min <- rownames(prco[-cal.start.n, ])[val.min]
        val.max <- rownames(prco[-cal.start.n, ])[val.max]
        val.start <- unique(c(val.min, val.max))
        val.start.n <- match(val.start, rownames(inp))
        cal.val <- c(cal.start, val.start)
        cal.val.start <- match(c(cal.start, val.start), rownames(inp))
        euc <- as.data.frame(as.matrix(dist(prco)))
        inp.start <- rownames(prco)[-cal.val.start]
        inp.start.b <- inp.start
        val <- val.start
	  stop<-n#min(c(n,length(val.start)))
	  k<-1
        for (k in 1:(stop)) 
		{
            test <- apply(euc[inp.start.b, val], 1, min)
            bla <- names(which(test == max(test)))
            val <- c(val, bla)
            inp.start.b <- inp.start.b[-(which(match(inp.start.b, 
                bla) != "NA"))]
       	 }
        val.n <- match(val, rownames(inp))
        cal.n <- c(1:nrow(inp))[-val.n]
        cal <- rownames(inp)[cal.n]
        
		#tmp fix for problem in function
		n<-ceiling(per.n*nrow(inp))
		if(n<1){n<-1}
		tst.id<-unique(val.n)
		if(n>length(tst.id)){n=length(tst.id)}
		val.n<-sample(tst.id,n)
		cal.n<-c(cal.n,tst.id[!tst.id%in%val.n])

		cal <- rownames(inp)[cal.n]
		val<-rownames(inp)[val.n]



        output <- list(`Calibration and validation set` = va, 
            `Number important PC` = pc, `PC space important PC` = prco, 
            `Chosen sample names` = "NULL", `Chosen row number` = "NULL", 
            `Chosen calibration sample names` = unique(cal), `Chosen calibration row number` = unique(cal.n), 
            `Chosen validation sample names` = unique(val), `Chosen validation row number` = unique(val.n))
	}
        class(output) <- "ken.sto"
        return(output)
    }

#wrapper to iterate ken.sto2
duplex.select<-function(data,ken.sto2.obj,percent.in.test)
	{
	#determine how many more are needed
	start.have<-ken.sto2.obj$`Chosen validation sample names`
	need<-percent.in.test*nrow(data)-length(start.have)
	
	#don't do anything if there are enough
	if(need>0)
	{
	#extract from remainning data
	have<-start.have
	while(need>0)
		{
			tmp.data<-data[!rownames(data)%in%have,]
			more<-ken.sto2(tmp.data, per = "TRUE", per.n = percent.in.test, num = 7, va = "TRUE")
			now.have<-more$`Chosen validation sample names`
			need<-percent.in.test*nrow(data)-(length(now.have)+length(have))
			have<-c(have,now.have)
		}

	#adjust for too many selected 
	drop<-NA
	if(need<0)
		{
			drop<-now.have[sample(length(now.have),abs(need))]
		}

	new.obj<-have[!have%in%drop]
	
	#objects to return
	`Chosen validation sample names`=c(new.obj)
	`Chosen validation row number`= c(1:nrow(data))[rownames(data)%in%new.obj]
	`Chosen calibration sample names`= rownames(data)[!rownames(data)%in%`Chosen validation sample names`]
	`Chosen calibration row number` =c(1:nrow(data))[rownames(data)%in%`Chosen calibration sample names`]
	}else{
	`Chosen validation sample names`=ken.sto2.obj$`Chosen validation sample names`
	`Chosen validation row number`= ken.sto2.obj$`Chosen validation row number`
	`Chosen calibration sample names`= ken.sto2.obj$`Chosen calibration sample names`
	`Chosen calibration row number` =ken.sto2.obj$`Chosen calibration row number`
	}
	output<-list(`Chosen validation row number`= `Chosen validation row number`,
			 `Chosen validation sample names`=`Chosen validation sample names`,
			 `Chosen calibration sample names` = `Chosen calibration sample names`, 
			 `Chosen calibration row number` = `Chosen calibration row number`)
}


#various tests
test<-function(){
library(reshape2)
library(pcaMethods)
data(mtcars)
data<-mtcars[,-c(8,9)]
y<-data.frame(am=mtcars$am)
pls.y<-do.call("cbind",lapply(1:ncol(y),function(i){fixln(y[,i])}))

color<-data.frame(am=sapply(1:ncol(y),function(i){factor(fixlc(y[,i]))}))
# color<-NULL
scaled.data<-data.frame(prep(data,center=TRUE,scale="uv"))
#make OSC model
mods<-make.OSC.PLS.model(pls.y,pls.data=scaled.data,comp=2,OSC.comp=2, validation = "LOO",method="oscorespls", cv.scale=TRUE)
switch(type,
	osc.scores 			= 	plot.OSC.results(mods,plot="scores",groups=color),
	osc.RMSEP			=	plot.OSC.results(mods,plot="RMSEP",groups=color),
	osc.loadings 		= 	plot.OSC.results(mods,plot="loadings",groups=color),
	osc.delta.weights 	=	plot.OSC.results(mods,plot="delta.weights",groups=color)
)

#make model visualization
final<-results<-get.OSC.model(obj=mods,OSC.comp=2)
plot.PLS.results(obj=final,plot="scores",groups=color)
plot.PLS.results(obj=final,plot="RMSEP",groups=color)
plot.PLS.results(obj=final,plot="loadings",groups=color)

#new plotting function
plot.PLS(obj=final,plot="scores",groups=color)
plot.PLS(obj=final,plot="RMSEP",groups=color)
plot.PLS(obj=final,plot="loadings",groups=color)
plot.PLS(obj=final,plot="biplot",groups=color)



}