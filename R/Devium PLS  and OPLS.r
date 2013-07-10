#function to carry out PLS or orthogonal signal correction PLS (OSC-PLS)
OSC.correction<-function(pls.y,pls.data,comp=5,OSC.comp=4,validation = "LOO",progress=TRUE,cv.scale=F,...){ 
	
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
		OSC.results$RMSEP[[i]]<-matrix(RMSEP(tmp.model)$val,ncol=2,byrow=TRUE)
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
	return(OSC.results)	
}

#fit many OPLS models to overview optimal LV and OLV
optimize.OPLS<-function(max.LV=4,tolerance =0.01,pls.y,pls.data,validation = "LOO",method="oscorespls",cv.scale=T,...){

	#iterate and fit OSC models for each possible LV > 1
	out<-lapply(1:max.LV, function(i){
		mod<-OSC.correction(pls.y=pls.y,pls.data=pls.data,comp=i,OSC.comp=i,validation = validation,progress=TRUE,cv.scale=cv.scale,...)
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
plot.OSC.results<-function(obj,plot="RMSEP",groups="NULL"){
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

#plot results for a single model
#plot OSC results
plot.PLS.results<-function(obj,plot="RMSEP",groups=data.frame(rep("NULL",nrow(obj$data))),comp1=1,comp2=2){
	require(ggplot2)
	#plot = one of: c("RMSEP","scores","loadings")
	#groups is a factor to show group visualization in scores plot
	
	switch(plot, # only looks right for single Y models!
		RMSEP 			=  .local<-function(obj,comp1,comp2){
								#bind info and RMSEP
								comps<-1:obj$total.LVs
								plot.obj<-tryCatch(obj$RMSEP[,2] ,error= function(e){obj$RMSEP[,1]})# CV
								
								bound<-data.frame(component=comps,RMSEP=plot.obj[-1]) # not the intercept
								
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
								
								# #return suggested number of components
								# comp <-
								# ocomp <-
								# RMSEP<-do.call("cbind",obj$RMSEP) # CV RMSEP
								# even<-1:ncol(RMSEP)%%2==0
								# tmp<-RMSEP[,even]# CV RMSEP
								# best.comp<-data.frame(	OLV 		= matrix(rep(0:(comp), (comp)), nrow(tmp), ncol(tmp))[which.min(tmp)],
														# LV		= matrix(rep(0:(ocomp), each=(comp+1)), nrow(tmp), ncol(tmp))[which.min(tmp)],
														# RMSEP 	= tmp[which.min(tmp)])
														
								# #set criteria for accepting smaller model	
								# accept<-0.05 # not worse than this, accept smaller				
								# delta<-tmp[best.comp$LV,]-tmp[best.comp$LV,best.comp$OLV+1]		
								# accepted<-which(delta<=accept)[1] # smallest	
								# accepted.comp<-data.frame(OLV = accepted-1, LV = best.comp$LV, RMSEP = tmp[best.comp$LV,accepted])		
								# OPLS.LV.results<-rbind(best.comp,accepted.comp)
								# rownames(OPLS.LV.results)<-c("minimum","chosen") # save this for later
								
							},
		scores 			=	.local<-function(obj,comp1,comp2){
								comps<-obj$total.LVs
								plot.obj<-tryCatch(obj$scores[[1]][,c(comp1,comp2)],error=function(e){obj$scores[,c(comp1,comp2)]}) # not sure how to simply unclass and coerce to data.frame
								
								#format data
								out<-as.data.frame(cbind(plot.obj[,c(comp1,comp2)],join.columns(as.matrix(groups))))
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


testing<-function(){ 
#testing
library(pls)
data(gasoline)
pls.data<-gasoline[,-1] # take out Y
pls.y<-gasoline$octane 

#get bootstrapped error for feature selected model
results<-boot.model(algorithm="plsr",data=pls.data,y=pls.y,ncomp=5,return="coef",R=500,method="oscorespls")

ncomp=2;steps=2
feature.subset<-feature.cut(obj=results[[1]][,1],type="number",cuts=seq(ncomp,ncol(data),round(ncol(data)/(steps+1),0))[-1])
obj<-multi.boot.model(algorithm="plsr",data=pls.data,y=pls.y,feature.subset=feature.subset,ncomp=ncomp,return="pred.error",R=10,parallel=FALSE,plot=TRUE)

#plot selected feature in the context of the complete data set
# as bargraph or heat map
# with and without the complete feature set
feature.set = feature.subset[[c(1:nrow(obj$results))[obj$results[,2]%in%obj$RMSEP_0.632.min]]]
weights.set = results$coef [,1]

mod<-OSC.correction(pls.y,pls.data[,feature.set],comp=2,OSC.comp=2,validation = "LOO",progress=TRUE,method="oscorespls") 

#choose optimal OSC number based on CV RMSEP
index<-matrix(rep(1:length(mod$RMSEP),each=length(mod$RMSEP)),nrow=length(mod$RMSEP))[which.min(do.call("rbind",mod$RMSEP)[,2])[1]]

best.mod<-get.OSC.model(obj=mod,OSC.comp=mod$OSC.LV[index])

plot.PLS.results(obj=best.mod,plot="loadings",groups=pls.y)
}
	
