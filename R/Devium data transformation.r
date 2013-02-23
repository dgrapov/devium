
#test for normality
 normal.test<-function(var,test.method){
			check.get.packages("nortest")
                test<-switch(test.method,
				# add "Kolmogorov-Smirno","Shapiro-Francia"
                "Shapiro-Wilk"  = shapiro.test(var)$p.value,
                "Cramer-von Mises"   = cvm.test(var)$p.value,
                "Anderson-Darling" = tryCatch(ad.test(var)$p.value,error=function(var) 0))        
                if(is.na(test)){test<-0}
                return(test)    
        }

transform.to.normal<-function(data,data.name="",test.method= "Anderson-Darling", alpha = 0.05 , force.positive=TRUE, transformation="none")
	{	
		check.get.packages(c("plyr","nortest","car"))
		#function to carry out the transformation
		trans<-function(var,transformation,test.method,alpha,force.positive)
			{
				if(force.positive){var<-var + abs(min(var))}
				p.val<-normal.test(var,test.method)
				if(p.val <= alpha) 
					{
							switch(transformation,
							"log"		= .local<-function(var){ list(data=log(var+1), transformation = transformation)},
							"none"		= .local<-function(var){list(data=var,transformation="none")},
							"BOX-COX" 	= .local<-function(var)
								{
									power<-tryCatch(as.numeric(unlist(powerTransform(na.omit(var))$lambda )),error=function(e){NA}) # for negatives family="yjPower"
									if(is.na(power)) { out<-var }else{ out<-var^power }
									list(data=out,transformation=paste(round(power,3),"power", sep=" "))
								})
							
					} else {
							.local<-function(var){list(data=var,transformation="none")}
							}
							
					obj<-.local(var)	
					return(list(data=as.data.frame(obj$data),p.value = p.val,transformation=obj$transformation))
			}
			
			
		#ignore non-numeric
		data<-data[sapply(1:ncol(data), function(i) {class(data[,i])=="numeric"})]
		
		obj<-colwise(trans) (data, transformation,test.method=test.method,alpha=alpha,force.positive)
		trans.data<-as.data.frame(as.list(obj[1,]));colnames(trans.data)<-colnames(data)
		diagnostics=as.data.frame(t(obj[-1,]));colnames(diagnostics)<-c("p-value","transformation")
		out<-list(trans.data, diagnostics=diagnostics);names(out)<-paste(data.name,c("transformed","transformed.diagnostics"),sep=".")
		return(out)
	}
	
#function to store or return transformed object
transform.to.normal.output<-function(obj,name="transformed.data", envir=devium)
		{
		#object stored: get("devium.data.transformation.results",envir)
		#diagnostics
		tmp<-obj[[2]]
		diagnostics<-as.data.frame(matrix(unlist(tmp),ncol=2));dimnames(diagnostics)<-dimnames(tmp)		# need to break list else gwidget doesn't show properly
		#data
		data<-obj[[1]]
		data.name<-names(obj)
		
		#Determine placement of output for EXCEL
		data.list<-list(data,diagnostics)
		list.names<-matrix(data.name)
		start.row<-1;spacer<-1;start.col<-1
		direction<-"horizontal"

		#assign complete object to envir = vdevium
		assign("devium.data.transformation.results",list(data=data, diagnostics=diagnostics,
		placement=list.placement.full(data.list,list.names,direction="horizontal",start.col,start.row,spacer)),envir=envir)
		
		#add results to global environment
		assign(names(obj)[1],as.data.frame(data),envir=.GlobalEnv)
		assign(names(obj)[2],data.frame(p.value=diagnostics[,1,drop=FALSE],transformation=as.factor(unlist(diagnostics[,2,drop=FALSE]))),envir=.GlobalEnv)
		}
	
scale.data<-function(data, scale="uv", center=TRUE)
	{
		switch(scale,
		"uv" 			= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"pareto" 		= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"vector"		= .local<-function(){check.get.packages("pcaMethods");pcaMethods::prep(data,scale,center)},
		"none"			= .local<-function(){return(data)},
		"range scale" 	= .local<-function(){tmp<-sapply(1:ncol(data),function(i)
												{
													obj<-data[,i]
													tmp<-range(obj)
													(obj-tmp[1])/(tmp[2]-tmp[1])
													
												})
												colnames(tmp)<-colnames(data)
												return(tmp)
											})
		.local()						
	}