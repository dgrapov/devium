
# data overview and normalization functions
#function to calculate within and between batch precision for all variables
calc.mRSD<-function(data,batch=data.frame(1:nrow(data)),summary.range=seq(0,100,10),use="mean"){
	library(plyr)
	
	#bin summaries into range
	if(!is.factor(batch)){batch<-tryCatch(factor(batch[,],levels=unique(batch[,])),error=function(e){factor(batch,levels=unique(batch))}); message("Batch converted to factor.")}
	
	#main object
	tmp<-data.frame(batch=batch,data)
	
	#parametric summary
	b.m<-ddply(tmp,.(batch),colwise(mean))
	b.s<-ddply(tmp,.(batch),colwise(sd))
	b.rsd<-b.s/b.m*100
	b.rsd[,1]<-b.m[,1]
	
	
	# #non-parametric summary
	# b.m2<-ddply(tmp,.(batch),colwise(median))
	# b.s2<-ddply(tmp,.(batch),colwise(IQR))
	# b.rsd2<-b.s2/b.m2*100
	# med.rsd.np<-apply(b.rsd2[,-1],1,median,na.rm=T) # 
	
	#generate summary objects for analytes between all batches
	analyte.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],2,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],2,sd,na.rm=T)) 
	colnames(analyte.RSD)[1]<-use# RSD for variables over all batches
	analyte.RSD.summary<-split.bins(obj=analyte.RSD[,1],bins=seq(0,100,10)) # summary for variables over all batches
	analyte.RSD.summary$percent<-round(analyte.RSD.summary$count/sum(analyte.RSD.summary$count)*100,1)
	
	#generate summary objects for batches based on all analytes
	within.batch.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],1,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],1,sd,na.rm=T))
	rownames(within.batch.RSD)<-b.rsd[,1]
	colnames(within.batch.RSD)[1]<-use
	within.batch.RSD.summary<-split.bins(na.omit(within.batch.RSD[,1]),bins=seq(0,100,10)) # ,max(within.batch.RSD[,1]
	within.batch.RSD.summary$percent<-round(within.batch.RSD.summary$count/sum(within.batch.RSD.summary$count)*100,1)
	
	#return summary
	list(batch.means=b.m,batch.sd=b.s,all.batch.RSD=b.rsd,variable.RSD=analyte.RSD,batch.RSD=within.batch.RSD,variable.RSD.summary=analyte.RSD.summary,batch.RSD.summary=within.batch.RSD.summary)	

}

#create summary objects from object produced by calc.mRSD
summarize.performance<-function(obj=gc.perf.raw,sig.figs=2){
	#optionally bin objects into an interval range
	
	#batch summary
	batch.med.perf<-data.frame(median.RSD=signif(median(obj$batch.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$batch.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up batch summary
	tmp<-obj$batch.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	batch.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	batch.summary$cumulative.percent<-cumsum(batch.summary$percent)
	#analyte summary
	var.med.perf<-data.frame(median.RSD=signif(median(obj$variable.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$variable.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up analyte summary
	tmp<-obj$variable.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	var.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	var.summary$cumulative.percent<-cumsum(var.summary$percent)
	
	
	
	#return
	list(batch=batch.med.perf,batch.summary=batch.summary,variable=var.med.perf,variable.summary=var.summary)
}

#split vector based on probabilities and calculate counts
split.prob<-function(obj,probs=seq(0, 1, 0.25)){
	library(plyr)
	splits<-quantile(obj,probs)
	interval<-cut(obj,splits,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	ddply(tmp,.(interval),colwise(length))
}

#split object on bins
split.bins<-function(obj,bins=seq(10,100,10)){
	library(plyr)
	
	interval<-cut(obj,bins,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	tmp<-ddply(tmp,.(interval),colwise(length))
	#need a mechanism to group values over largest bin
	int<-fixlc(tmp$interval)
	int[is.na(int)]<-paste0(">",max(bins))
	tmp$interval<-int
	tmp
}

#identify samples with > quantile
over.prob<-function(val=tmp.data$leverage,prob=0.95){
	val>quantile(val,probs=prob)
}

#adjust data by batch scalar (add ability to get ratios using QCs and adjust samples)
scalar.batch.adjust<-function(obj,factor,use="median",train=NULL){
	#calculate ratio of median of each factor level to global median
	#return adjusted value and adjustment for each factor (should be numeric else levels could be broken)
	# train can be a logical specifying which subset of the data to use to calculate the ratios
	library(plyr)
	if(!class(obj)=="data.frame"){obj<-as.data.frame(obj)}
	if(is.logical(train)){
		full<-data.frame(obj)
		split.data<-split(full,factor(train))
		train.data<-split.data$"TRUE"
		train.factor<-unlist(split(as.data.frame(factor),factor(train))$"TRUE")
	} else {
		train.data<-obj
		train.factor<-factor	
	}
	
	#remove outliers
	
	
	global.med<-apply(train.data,2,use,na.rm=TRUE)
	#main object
	tmp<-data.frame(batch=train.factor,train.data)
	
	#summary for all batches and analytes
	b.m<-ddply(tmp,.(batch),colwise(use))
	med.ratio<-sweep(b.m[,-1,drop=F],2,unlist(global.med),"/")
	
	#return ratio adjusted data
	big.l<-split(obj,factor)

	res<-lapply(1:length(big.l),function(i){
		tmp<-big.l[[i]]
		rat<-unlist(med.ratio[i,])
		res<-sweep(tmp,2,rat,"/")
		res[is.na(res)]<-0 # for ratios with zero
		res
	})
	adjusted.data<-do.call("rbind",res)
	#clean of strange vars
	adjusted.data[adjusted.data=="Inf"]<-NA
	
	list(adjusted.data=adjusted.data,ratios=med.ratio)
}

#plot a single variable line plot
summary.lineplot<-function(val,groups=NULL,view.split=NULL,theme=NULL,se=FALSE,extra=NULL,span=0.75,print.plot=TRUE){
	library(ggplot2)
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	vis.data<-data.frame(value=unlist(unname(val)))
	vis.data$id<-1:length(vis.data$val)
	
	if(is.null(groups)){vis.data$groups<-1;vis.data$color<-1} else {vis.data$groups<-factor(as.matrix(groups))}
	if(is.null(view.split)){
		add.facet<-NULL
	} else {
			vis.data$facet<-vis.data$groups
		if(view.split=="y"){
			add.facet<-facet_grid(facet ~ .)
		} else {
			add.facet<-facet_grid( . ~ facet)
		}	
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=id)) + geom_point(aes(color=groups),alpha=.75,show_guide=FALSE)+
	stat_smooth(aes(group=groups,color=groups),method = "loess", size = 1,se=se,alpha=.1,span=span) + theme + add.facet + xlab(colnames(groups))+ylab(colnames(val))+ 
	guides(col = guide_legend(title = colnames(groups))) + extra
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#box plot for 2 factors with loess smoothing
summary.boxplot2<-function(val,groups=NULL,split.on=NULL,theme=NULL,se=FALSE,span=0.75,extra=NULL,print.plot=TRUE){
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	library(ggplot2)
	vis.data<-data.frame(value=unlist(val))

	if(is.null(groups)){
		vis.data$groups<-1;vis.data$color<-1
	} else {
		vis.data$groups<-factor(as.matrix(groups))
	}
	
	if(is.null(split.on)){
		vis.data$split.on<-""
		l.guide<-NULL
		# extra<-scale_fill_manual(values ="grey50")
		smooth<-NULL
	} else {
		vis.data$split.on<-factor(as.matrix(split.on))
		l.guide<-guides(fill = guide_legend(title = colnames(split.on)))
		# extra<-NULL
		smooth<-stat_smooth(aes(group=split.on,color=split.on),method = "loess", size = 1.25,se=se,alpha=.1,show_guide=FALSE,span=span)
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=groups)) + geom_boxplot(aes(fill=split.on),alpha=.75) +
	smooth + theme + xlab(colnames(groups))+ ylab(colnames(val))+ l.guide 
	p<-p+extra
	
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#create summary plot RSD% analyte mean
RSD.means.plot<-function(obj=list(gc.perf.raw,gc.raw.t1),name=c("Raw","FAME L2 norm"),size=3,alpha=.75,use.log=TRUE,se=FALSE,theme=NULL,extra=NULL){
	
	library(ggplot2)
	#check if many or single object
	if(length(names(obj))==7){obj<-list(obj)}
	
	#obj can be a list of objects produced by calc.mRSD
	#name will be used for legends
	res<-lapply(1:length(obj),function(i){
		tmp<-obj[[i]]		
		res<-data.frame(method=name[i],RSD=tmp$variable.RSD[,1],mean=apply(tmp$batch.means[-1],2,mean,na.rm=TRUE))
		res$log.mean<-log(res$mean+1)
		res
	})
	
	vis.data<-do.call("rbind",res)
	
	if(use.log){
		p<-ggplot(vis.data,aes(x=log.mean,y=RSD,group=method,color=method,fill=method))+ 
		stat_smooth(method = "loess", size = 1,show_guide=FALSE ,se = se,alpha=.75)+
		geom_point(alpha=alpha,size=size) + 
		theme + xlab("log Mean")+ ylab("RSD")+extra #+scale_color_manual(values=rainbow(3))+
		print(p)
	
	} else {
		p<-ggplot(vis.data,aes(x=mean,y=RSD,group=method,color=method,fill=method))+ 
		geom_point(alpha=.75)+ 
		stat_smooth(method = "loess", size = 1,show_guide=FALSE ,se = se,alpha=.75)+ 
		theme + xlab("Mean")+ ylab("RSD")+extra #+scale_color_manual(values=rainbow(3))+
		print(p)
	}
}	

#bar plot to summarize performance
RSD.counts.plot<-function(obj,show="variable",plot.obj="count",name="",theme=NULL,extra=NULL,ylabel="number of metabolites"){
	
	if(show=="variable"){
		#variables
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$variable.summary)
		})
	} else {
		#samples
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$batch.summary)
		})
	}	
	
	vis.data<-do.call("rbind",res)
	#hack to get the sort correct
	vis.data<-vis.data[order(fixlc(vis.data$RSD)),]
	fix<-grep(">",vis.data$RSD)
	if(length(fix)>0){
		tmp<-rbind(vis.data[-fix,],vis.data[fix,])
		vis.data<-tmp
	}	
	
	vis.data$interval<-factor(fixlc(vis.data$RSD), levels=unique(fixlc(vis.data$RSD)))

	#switch which variable in the data is plotted
	vis.data$plot.obj<-vis.data[,plot.obj]
	upper<-max(vis.data$plot.obj)
	ulim<-if(upper>=10){10} else {upper}
	dlim<-if(upper>=10){2} else {1}
	ggplot(data=vis.data,aes(x=interval,y=plot.obj,fill=method,group=method))+ geom_bar(position=position_dodge(),stat="identity")+ theme +
	scale_y_continuous(minor_breaks = seq(0 , upper, dlim), breaks = seq(0, upper, ulim)) + xlab("RSD")+ylab(ylabel)+ extra #scale_fill_brewer(palette="Set1")
	
}

#conduct LOESS normalization on a data frame or matrix
loess.normalization<-function(x,y,subset=NULL,progress=TRUE,scale.to=NULL,scale.with=NULL,span=0.75,...){

	#subset = logical specifying which subset of the data to be used for fitting
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(x), style = 3)} else {pb<-NULL}
	span<-tryCatch(unlist(matrix(span,nrow=length(x))),error=function(e){unlist(matrix(span,nrow=nrow(x)))}) # recycle
	res<-do.call("cbind",lapply(1:ncol(x),function(i){	
		tmp.x<-x[,i]
		fit<-loess(tmp.x~y,subset=subset,span=span[i],...)
		pred<-predict(fit,data.frame(tmp.x=tmp.x))
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(tmp.x-pred) # residuals for train and test
	}))
	if (progress == TRUE){close(pb)}
	if(!is.null(scale.to)){
			scale<-apply(x,2,scale.to,na.rm=TRUE)
			res<-sweep(res,2,scale,scale.with)
		
		}
	return(res)
}

#cross-validation based tuning of LOESS
tune.loess<-function(data,y,folds=7,span.vals=seq(.25,1,by=.05),progress=TRUE){
	# X can be a data frame
	# wrapper for bisoreg::loess.wrappwer
	#returns optimal span for each column in X
	library(bisoreg)
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
	res<-unlist(lapply(1:ncol(data),function(i){
		x<-tryCatch(loess.wrapper(x=data[,i], y=y, span.vals = span.vals, folds = folds)$pars$span, error=function(e){NA})
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(x)
	}))
	if (progress == TRUE){close(pb)}
	cat("\n")
	return(res)
}

#calculate RSD
calc.RSD<-function(x,...){sd(x,...)/mean(x,...)}




#test
test<-function(){
spans<-tune.loess(tmp.data[,1:2],y=tmp.retention.time,folds=5)
loess.normalization(x=tmp.data[,1:2],y=tmp.retention.time,span=spans)

}