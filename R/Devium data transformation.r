
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
	
#function to conduct transformations
transform.to.normal.old<-function(data,test.method= "Anderson-Darling",force.positive=TRUE, transform="optimize")#
        {
        #supply data with samples as rows and  measurements as columns
        #test methods should be "shapiro", "cramer" or "anderson"
        #see below for more information
        #http://www.stat.ucl.ac.be/ISdidactique/Rhelp/library/nortest/html/00Index.html
        #for Box-Cox see car package
        # if optimize: transformations will be tried in the following order:
        #square root, Box-Cox, shifted natural logarithm
        #if force.positive = TRUE : power transformations are limited to positive values

        check.get.packages(c("nortest","car"))
     
        out.put<-list()
        n<-ncol(data)
        i<-1
        for(i in 1:n)
        {
                print(paste("transforming variable",i,"of", n,sep=" "))
                out1<-out2<-out3<-out4<-list()          
                j<-1
                var<-data[,i]
                #filter supplied constants
                if(sd(var,na.rm=TRUE)==0)
                {
                        result<-0.06
                }else{
                        result<- normal.test(var=var,test.method=test.method)
                }
                #catch error for nomal test here

                out1[["variable"]][[j]]<-var
                out2[["p.value"]][[j]]<-result
                #avoid test 
                if(is.na(result)|result == 0) {next}
                if(result<0.05)
                {
                        out3[["transformation"]][[j]]<-"none"
                        out4[["initial"]][[j]]<-"not normal"
                }else{
                        out3[["transformation"]][[j]]<-"none"
                        out4[["initial"]][[j]]<-"normal"
                }
                
                j<-j+1
                #if(is.scores==FALSE)
                #{
                #Apply transformations
                        if(result<0.05)
                        {
                                trans<-"square root"
                                #var<-data[,i]^.5
                                # to handle negative values
                                var<-neg.power.transform(data[,i],power=0.5)
                                result<- normal.test(var=var,test.method=test.method)
                                out1[["variable"]][[j]]<-var
                                out2[["p.value"]][[j]]<-result
                                out3[["transformation"]][[j]]<-trans
                                j<-j+1
                        }
                        
                        if(result<0.05)
                        {
                                var<-(data[,i])
                                if(any(na.omit(var)<=0)==TRUE)
                                {
                                        power<-tryCatch(as.numeric(unlist(powerTransform(na.omit(var),family="yjPower")$lambda )),error=function(e)print("error"))
                                        if(power=="error")
                                        {
                                                power<-1        
                                        }
                                }else{
                                        #catch strange errors
                                        e <- simpleError("test error")
                                        power<-tryCatch(powerTransform(na.omit(var)),error=function(e)print("error"))   
                                        if(power=="error")
                                        {
                                                power<-1        
                                        }else{
                                                power<-as.numeric(unlist(powerTransform(na.omit(var))$lambda))
                                        }                               
                                }

                                if(force.positive==TRUE)
                                {
                                power<-abs(power)
                                }

                                #special transformation for negative values
                                if(any(na.omit(data[,i])<0))
                                {
                                        var<-neg.power.transform(data[,i],power=power)
                                        print("special handling of negative variable")
                                }else{
                                        var<-(data[,i])^power
                                }

                                var[var=="Inf"]<-NA
                                trans<-paste("power transformation by", signif(power,4))
                                result<- normal.test(var=var,test.method=test.method)
                                out1[["variable"]][[j]]<-var
                                out2[["p.value"]][[j]]<-result
                                out3[["transformation"]][[j]]<-trans
                                j<-j+1
                                
                        }

                        if(result<0.05)
                        {       if(all(na.omit(data[,i])>0))
                                {
                                        trans<-"shifted natural log"
                                        var<-log(data[,i]+1, 2.718282)
                                        result<- normal.test(var=var,test.method=test.method)
                                        out1[["variable"]][[j]]<-var
                                        out2[["p.value"]][[j]]<-result
                                        out3[["transformation"]][[j]]<-trans
                                        j<-j+1
                                }

                        }
                        
                        #if(result<0.05)
                        #{
                                #trans<-"inverse"
                                #var<-(1/data[,i])
                                #var[var=="Inf"]<-0
                                #result<- normal.test(var,test.method)
                                #out1[["variable"]][[j]]<-var
                                #out2[["p.value"]][[j]]<-result
                                #out3[["transformation"]][[j]]<-trans
                                #j<-j+1
                        #}
                        
                        index<-which.max(unlist(out2))[1]
                        out.put[["variable"]][[i]]<-out1[[1]][[as.numeric(index)]]
                        out.put[["transformation"]][[i]]<-out3[[1]][[as.numeric(index)]]        
                        out.put[["initial"]][[i]]<-out4[["initial"]]    

                #}else{
                        #if(result<0.05)
                        #{
                                #var<-(data[,i])+min(data[,i])
                                #power<-as.numeric(unlist(powerTransform(na.omit(var),family="yjPower")$lambda ))
                                #var<-((data[,i])+min(data[,i]))^power
                                #var[var=="Inf"]<-NA
                                #trans<-paste("power transformation by", signif(power,4))
                                #result<- normal.test(var=var,test.method=test.method)
                                #out1[["variable"]][[j]]<-var
                                #out2[["p.value"]][[j]]<-result
                                #out3[["transformation"]][[j]]<-trans
                                #j<-j+1
                        #}
                        #out.put[["variable"]][[i]]<-out1[[1]]
                        #out.put[["transformation"]][[i]]<-out3[[1]]
                        #out.put[["initial"]][[i]]<-out4[["initial"]]
                #}
                        
                                

                        if(result<0.05)
                        {
                                out.put[["normal"]][[i]]<-"not normal"
                        }else{
                                out.put[["normal"]][[i]]<-"normal"
                        }

                        
        }
                results<-list() 
                if(length(out.put)>0)
                {
                        
                        new.data<-as.data.frame(matrix(,nrow(data),ncol(data))) 
                        error.vars<-c(1:ncol(data))[is.na(out.put$initial)]
                        #error.vars<-c(error.vars,                      
                        #put back error vars
                        if(!length(error.vars)==0)
                        {
                                vars<-c(1:ncol(data))[!c(1:ncol(data))%in%error.vars]
                                ok.data<-as.data.frame(do.call("cbind",out.put[["variable"]]))
                                new.data[,vars]<-ok.data
                                new.data[,error.vars]<-data[,error.vars]
                                out.put[["normal"]][error.vars]<-"sample size must be > 7"
                        }else{
                                new.data<-as.data.frame(do.call("cbind",out.put[["variable"]]))
                        }
                        
                        if(!class(rownames(data))=="NULL")
                        {
                                dimnames(new.data)<-dimnames(data)
                        }else{
                                colnames(new.data)<-colnames(data)
                        }

                        results[["data"]]<-new.data
                        results[["initial"]]<-out.put[["initial"]]
                        results[["transformation"]]<-out.put[["transformation"]]
                        results[["final"]]<-out.put[["normal"]]
                        return(results)
                }else{
                        results[["data"]]<-"error"
                        results[["initial"]]<-"error"
                        results[["transformation"]]<-"error"
                        results[["final"]]<-"sample size must be > 7"
                }
                return(results)
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