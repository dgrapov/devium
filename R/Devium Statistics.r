
#function to convert pattern to a single char objects name
rename <- function(x, pattern, replace="_")
	{
		#strangely sapply will not work without effort here
		replace=rep(replace,length(pattern))
		for(i in seq_along(pattern))
			{
				x<-gsub(pattern[i], replace[i], x)
			}
		return(x)	
}

# wrapper to compute functions on subsets of data specefied by the factor
calc.stat<-function(data,factor,stat)
	{
		d.list<-split(data,as.factor(factor))
		#function to calculate (ddply, stopped working after update? not obvious why)
		calc<-function(d.list,stat){
					out<-sapply(1:length(d.list),function(i)
						{
							obj<-d.list[[i]]
							apply(obj,2,get(stat))
						})
					colnames(out)<-paste(stat, "-",names(d.list), sep="")	
					out
					}
		#wrapper to calculate stats from a list (fix later using all plyr fxns)
		output<-do.call("cbind",lapply(1:length(stat),function(i)
			{
				what<-stat[i]
				name.what<-paste(what,"-",sep="")
				calc(d.list,stat[i])	
			}))
			return(as.data.frame(output))
	}

# break string and get object into colummns by potision in original string position 
str.get<- function(obj, sep="±",get=1)
	{
		do.call("cbind",lapply(1:ncol(obj),function(i)
			{
				tmp<-as.character(obj[,i])
				as.numeric(t(as.data.frame(strsplit(tmp,sep)))[,get])
			}))
	}

#calculate fold change relative to column
fold.change<-function(obj,rel=1,log=FALSE)
	{
	if(log==FALSE){
		rel<-obj[,rel]
		obj/rel
	} else {
		rel<-obj[,rel]
		obj-rel
	}
	}

# function to extract data based on non-missing in index
sub.data<-function(data,index)
	{
		#input = data object with rows the dimension to be split
		#index specifying groups for comparison with ordered same a sample rows
		#returns list [1] = index
		# [2] data
		keep.id<-!is.na(index)
		list(factor= index[keep.id],data=data[keep.id,])
	}

#match two data frames based on rownames	
match.data<-function(data1,data2)
	{
		#args
		#data1 and data2 are two data frames with rownames to be matched
		#return values
		#data1 whose rownames intersect with rownames of data2
		#data2 whose rownames intersect with rownames of data1
		#both sharing the same rowname order
		tmp1<-data1[rownames(data1)%in%rownames(data2),]
		tmp2<-data2[rownames(data2)%in%rownames(data1),]
		list(data1=tmp1[order(rownames(tmp1)),],data2=tmp2[order(rownames(tmp2)),])
	}

#anova
anova.formula.list<-function(data,formula,meta.data)
	{
	  tmp.data<-cbind(meta.data,data) # bind with data for easy scoping
	  tmp<-lapply(1:ncol(data),function(i)
		{
			#tryCatch(na.omit(as.data.frame(with(meta.data,anova(lm(as.formula(paste("data[,",i,"]~",formula,sep=""))))[,5,drop=FALSE]))), error=function(e){NULL})
			tryCatch(na.omit(as.data.frame(anova(lm(as.formula(paste("data[,",i,"]~",formula,sep="")),data=tmp.data))[,5,drop=FALSE])), error=function(e){NULL})
			
		})
		if(is.null(tmp))
			{
					return(cat("Error in test","\n"))
			} else {
					tmp<-as.data.frame(tmp)
					colnames(tmp)<-colnames(data)
					as.data.frame(t(tmp))
			}
}

#get summary statistics
stats.summary <- function(data,comp.obj,formula,sigfigs=3,log=FALSE)
	{
		#summarise and make ANOVA from data based on formula 
		#check.get.packages(c("qvalue"))  using fdrtools instead to avoid random erros with initialization
		
		test.obj<-join.columns(comp.obj)
		#get summary by splitting data by each column of meta.data
		data.summary<-function(data,test.obj,log,sigfigs)
				{
					#split data
					tmp<-sub.data(data,test.obj)
					fct<-factor(as.character(unlist(tmp[1])))
					tmp.data<-data.frame(tmp[[2]])
						
					# get means ± sd, fold change
					means<-calc.stat(tmp.data,factor=fct,stat=c("mean"))
					sds<-calc.stat(tmp.data,factor=fct,stat=c("sd"))
					fc<-fold.change(means,log=log)
					colnames(fc)<-paste(colnames(fc),rep(colnames(fc)[1],ncol(fc)), sep="/")

					#format output from means and sd
					names<-paste(unlist(as.data.frame(strsplit(colnames(means),"-"))[2,])," mean ± std dev" , sep="")
					mean.sd<-matrix(paste(unlist(signif(means,sigfigs)), " ± ", unlist(signif(sds,sigfigs-1)),sep=""), ncol=ncol(means))
					colnames(mean.sd)<-names
					#bind with fold change
					cbind(mean.sd,round(fc[,-1,drop=FALSE],2))
				}
		cat("Generating data summary...","\n")
		stats.summary<-data.summary(data,test.obj,sigfigs=sigfigs,log=log)		

		#statistical tests
		cat("Conducting tests...","\n")
		p.values<-anova.formula.list(data,formula,meta.data=comp.obj)

		#multiple hypotheses tested adjustments	
		cat("Conducting FDR corrections...","\n")
		adj.p<-do.call("cbind",sapply(1:ncol(as.matrix(p.values)),function(i)
			{
				as.data.frame(p.adjust(as.matrix(p.values[,i]), method = "BH", n = nrow(p.values)))
			}))
		colnames(adj.p)<-paste(colnames(p.values),"adjusted.pvalues",sep="_")	
		#estimate q-values	
		adjusted.q<-sapply(1:ncol(as.matrix(p.values)),function(i)
			{
				#tryCatch(qvalue(as.matrix(p.values[,i]))$qvalues,error=function(e){matrix("Can not estimate",nrow=nrow(p.values),ncol=1)})
				FDR.adjust(as.matrix(p.values[,i]),type="pvalue",return.all=TRUE)$qval
			})
		colnames(adjusted.q)<-paste(colnames(p.values),"q.values",sep="_")	
		colnames(p.values)<-paste(colnames(p.values),"p.values",sep="_")	
		cbind(stats.summary,p.values,adj.p,adjusted.q)
	}

#function to carry out covariate adjustments
#-------------------------
covar.adjustment<-function(data,formula)
	{
	
	#data--> subjects as rows, measurements as columns
	#formula	<- ~ character vector
	#lm will be iteratively fit on each variable 
	#model residuals will be returned
	data<-as.data.frame(data)
	names(data)<-colnames(data)
	output<-list()
	n<-ncol(data)
	i<-1
	output<-lapply(1:n,function(i)
		{
			tryCatch(tmp<-as.formula(c(paste(paste("data$'",colnames(data)[i],"'~",sep=""),paste(formula,sep="+"),sep=""))),
			error= function(e){tmp<-as.formula(c(paste(paste("data[,i]","~",sep=""),paste(formula,sep="+"),sep="")))})
			fit<-lm(tmp,data=data)$residuals
			matrix(fit,,1)
		})
	out<-as.data.frame(do.call("cbind",output))
	dimnames(out)<-dimnames(data)
	#addback grand mean
	out<-out+mean(data)
	return(out)
	}

#helper function for getting statistics for making box plots
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE) 
	{
		require(plyr)

		# New version of length which can handle NA's: if na.rm==T, don't count them
		length2 <- function (x, na.rm=FALSE) {
			if (na.rm) sum(!is.na(x))
			else       length(x)
		}

		# This is does the summary; it's not easy to understand...
		datac <- ddply(data, groupvars, .drop=.drop,
					   .fun= function(xx, col, na.rm) {
							   c( N    = length2(xx[,col], na.rm=na.rm),
								  mean = mean   (as.numeric(as.matrix(xx[,col])), na.rm=na.rm),
								  sd   = sd     (xx[,col], na.rm=na.rm)
								  )
							  },
						measurevar,
						na.rm
				 )

		# Rename the "mean" column    
		#datac <- rename(datac, c("mean"= measurevar))

		datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

		# Confidence interval multiplier for standard error
		# Calculate t-statistic for confidence interval: 
		# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
		ciMult <- qt(conf.interval/2 + .5, datac$N-1)
		datac$ci <- datac$se * ciMult
		return(datac)
	}
	
#return object to Excel worksheet
return.to.Excel<-function(workbook.path="new",return.obj.list,return.name=names(return.obj.list),workbook.name=NULL)
	{
		check.get.packages(c("XLConnect"))
		
		#load workbok
		if (!workbook.path=="new")
			{
				#connect to worksheet
				old.dir<-getwd()
				wd<-dirname(workbook.path)
				workbook.name<-basename(workbook.path)
				setwd(wd)
				wb = loadWorkbook(workbook.name, create = FALSE)
				
			}else{
			
				if(is.null(workbook.name)){workbook.name<-paste('Workbook',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"),"xls", sep = ".")}
				wb = loadWorkbook(workbook.name, create = TRUE)
			}
			
		#place objects in workbook
		sapply(1:length(return.name),function(i)
			{
				obj.name<-rename(x=return.name[i],pattern=c(" ","\\$"))
				#create sheet
				createSheet(wb, name = obj.name)
				
				#delete name is if exists
				tryCatch(removeName(wb,obj.name),error=function(e){})
				createName(wb, name = obj.name, formula = paste(obj.name, "$A$1", sep = "!"))
				
				#bind rownames else not visible
				return<-cbind(rownames(return.obj.list[[i]]),return.obj.list[[i]]);colnames(return)[1]<-"Variables"
				writeNamedRegion(wb, data = return, name = obj.name, header = TRUE)
				
				#add excel auto filters need to avoid text and mean +/- stdev column else all all broken
				corners = getReferenceCoordinates(wb, obj.name)
				#don't include first column of variable names
				corners[1,2]<-(corners[2,2]-(corners[2,2]-2))
				corners[2,2]<-corners[2,2]-1
				setAutoFilter(wb, sheet = obj.name, reference = aref(corners[1,], corners[2,]))
			})
			
			saveWorkbook(wb)
	}

#get object from EXCEL	
get.from.Excel <- function(workbook.path=NULL,get.object.sheet=NULL,get.obj.name=NULL,a,return=TRUE,environment=.GlobalEnv)
				{
					check.get.packages(c("XLConnect"))
					old.dir<-getwd() # original working directory

					#check to see if workbook exists
					get.workbook<-function(workbook.path)
						{
							wd<-dirname(workbook.path)
							setwd(wd)
							loadWorkbook(basename(workbook.path))
						}
						
					workbook<-tryCatch(get.workbook(workbook.path), error=function(e) {NULL})	
					
					if (is.null(workbook) )
						{
							return(cat("file doesn't exist where you are looking", "\n"))
						} else {
								#check if named range exists
								obj<-tryCatch(readNamedRegion(workbook, name =  get.obj.name , header = FALSE),error= function(e) {NULL})
								
								if(is.null(obj))
									{
										#check if worksheet exists
										obj<-tryCatch(readWorksheet(workbook, sheet = get.object.sheet),error=function(e) {NULL})
										if(is.null(obj))
											{
												return(cat("the object can't be loaded", "\n"))
											} else {
													#assign sheet to environment
													tmp.obj<-get.object.sheet
											}
									} else {
											#assign named range to environment
											tmp.obj<-get.obj.name
									}

						}
					setwd(old.dir)
					#return obj
					#assign
					assign(tmp.obj,obj,envir=environment)
					if(return){get(tmp.obj,envir=environment)}
}						

#format binbase output
format.binbase.output<-function(data)
	{
		#data = name as string
		object<-get(data)
		#format data object
		sample.start<-which(colnames(object)=="file.id")+1
		variable.start<-which(object[,1]=="BinBase name")+1

		#data
		drows<-nrow(object)
		dcols<-max(c(1:ncol(object))[!sapply(1:ncol(object),function(i){sum(is.na(object[,i]))==drows})])
		tmp.data<-as.data.frame(t(as.matrix(object[c(variable.start:drows),c(sample.start:dcols)])))
		.names<-dimnames(tmp.data)
		#make sure all factors aconverted to numeric 
		tmp.data<-as.data.frame(matrix(as.numeric(as.character(unlist(tmp.data))),nrow=nrow(tmp.data), ncol=ncol(tmp.data)))
		dimnames(tmp.data)<-.names
		
		#get variable and sample meta data
		row.meta<-as.data.frame(t(as.matrix(object[c(1:(variable.start-2)),c(sample.start:dcols)])))
		col.meta<-as.data.frame(as.matrix(object[c(variable.start:drows),c(1:(sample.start-2))]))
		#convert data to all numeric prior to return
		data<-do.call("cbind",unclass(tmp.data));dimnames(tmp.data)<-.names # need to break factors

		#return results as a list
		list(data=tmp.data,row.metadata=row.meta,col.metadata=col.meta)
	}


#random junk I will eventually delete	
testing <-function(){
#check for errors Nan or Inf
#replace with NA
return.list<-lapply(1:length(return.obj.list),function(i)
	{
		obj<-return.obj.list[[i]]
		obj[is.na(obj)|obj=="Inf"|obj=="NaN"]<-"NA"
		obj
	})

	######use existing data to conduct a statistical test################################
# load data from workbook
workbook.path<-"C:\\Users\\dgrapov\\Dropbox\\Metabolomics Core\\Data Analysis\\Jasmohan Bajaj\\2013\\BAJAJ CTP comparison.xls" #home
get.object.sheet<- "DATA"
excel.obj<-get.from.Excel(workbook.path,get.object.sheet)
# transpose and excise rownames (avoid dealing with non-unique)
var.names<-excel.obj[,1]	
data<-as.data.frame(t(excel.obj[,-1]))
#load index for tests
get.object.sheet<- "Samples"
index<-get.from.Excel(workbook.path,get.object.sheet)
rownames(index)<-index[,1]

#match data and comparison index object
match.obj<-match.data(data1=data,data2=index)

#prepare for statistical test
data<-match.obj[[1]]
test.factor<-match.obj[[2]][,2,drop=FALSE];colname
formula<-"Comparison"

#conduct test
out<-stats.summary(data,comp.obj=test.factor,formula)

#prepare to return results to excel 
return.name<-"statistical.summary"
return.obj<-data.frame(variables=var.names,out)
rownames(return.obj)<-c(1:nrow(return.obj))
colnames(return.obj)<-c("variables",colnames(out))
return.obj.list<-list(return.obj)
	
#because sometimes this can corrupt files the results should be returned to a new worksheet copied and then added to the book in question
setwd( "C:\\Users\\dgrapov\\Dropbox\\Metabolomics Core\\Data Analysis\\Jasmohan Bajaj\\2013")
return.to.Excel(workbook.path="new",return.obj.list,return.name,workbook.name=NULL)

#create file containing basic information about the data and analysis to be used in latex report
analysis.summary<-list(	samples=nrow(data), variables=ncol(data),
						analysis.factor = "CTP",
						analysis.factor.levels = c("0 = control","1 = CTP 1 similiar to controls", "2 = CTP 1", "3 = CTP 2","4 = CTP 3"),
						univariate.analysis = "one-way analysis of variance (one-way ANOVA)",
						multivariate.analysis = "PLS-DA"
						)
						
#save an object called stats_table to the directory containing foild changes and p-values for the top 20 analytes
#########################################################################################


	
###############load data from BinBase out put############################################
#load binbse input from excel work sheet	
workbook.path<-"C:\\Users\\D\\Dropbox\\Metabolomics Core\\Data Analysis\\Manami Hara\\2013\\mx 110609_Manami Hara Data Analysis.xls" #home
workbook.path<-"C:\\Users\\dgrapov\\Dropbox\\Metabolomics Core\\Data Analysis\\Manami Hara\\2013\\mx 110609_Manami Hara Data Analysis.xls" # work
get.object.sheet<- "submit"
..<-get.from.Excel(workbook.path,get.object.sheet)	
#format loaded object and assign to environment
obj<-format.binbase.output('..')	

#carry out statictical tests based on the factor in the last column of  row.metadata
data<-as.data.frame(do.call("cbind",unclass(as.data.frame(na.omit(obj$data)))));dimnames(data)<-dimnames(as.data.frame(na.omit(obj$data))) # need to break factors

#formatting factor
fct=na.omit(as.data.frame(t(as.data.frame(strsplit(as.character(unlist(obj$row.metadata[,6])), ">")))))
fct<-as.data.frame(sapply(1:ncol(fct),function(i){gsub("-",".",fct[,i])}));colnames(fct)<-c("diabetic","age")
formula<-as.character(join.columns(as.data.frame(matrix(colnames(fct),nrow=1)),char="*")) #
formula<-"age*diabetic"

out<-stats.summary(data,comp.obj=fct,formula)
#add rownames
#function to return results to Excel worksheet via XLConnect
return.name<-"statistical.summary"
return.obj<-data.frame(variables=as.character(obj$col.metadata[,1]),out)
rownames(return.obj)<-c(1:nrow(return.obj))
colnames(return.obj)<-c("index",colnames(out))
return.obj.list<-list(return.obj)
	
#because sometimes this can corrupt files the results should be returned to a new worksheet copied and then added to the book in question
setwd( "C:\\Users\\D\\Dropbox\\Metabolomics Core\\Data Analysis\\Manami Hara\\2013")
return.to.Excel(workbook.path="new",return.obj.list,return.name,workbook.name=NULL)

#make a box plot for group-swise comparisons
#theme for plot
library(grid);library(ggplot2)
#theme for boxplot formatting
theme_box.plot <- theme (
    #axis.line = element_line(colour = 'black', size = 2), 
	axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
	#axis.text.x = element_text(size = 80,vjust=20),
	axis.text.x = element_blank(),
	axis.title.x = element_blank(),	
    axis.title.y = element_blank(), 
    axis.ticks.length = unit(1, "lines"), 
    axis.ticks.margin = unit(.1, "lines"), 
    legend.position = "none", 
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.margin = unit(c(1,.25,.25,.25), "lines"), 
    plot.background = element_blank(), 
    plot.margin = unit(0*c(.5,.25,.25,.25), "lines"))
	
#generate data summary to use for plots
#factor for plots
fct<-join.columns(fct)
#reorder factor to visualization preffernce young non-d then old non- d
fct[fct=="non.diabetic  | young "]<-1;fct[fct=="non.diabetic  | old "]<-2;fct[fct=="diabetic  | young "]<-3;fct[fct=="diabetic  | old "]<-4

col<-I(c("green","#008B00","red","#8B1A1A")) # color scheme young-old (light-dark) non-diab. (green-red)

#vvars are names of variables (using numeric index now incase of duplicates and to conserve name later)
id<-as.numeric(rownames(vvars))
vars<-as.character(unlist(vvars[,1]))
save.dir<-"C:\\Users\\dgrapov\\Dropbox\\Metabolomics Core\\Data Analysis\\Manami Hara\\2013\\node images"
setwd(save.dir)

#generate plots
i<-1
for(i in 1:length(vars))
	{
		#box plot	
		p4<- ggplot(data, aes(factor(fct), data[,id[i]]))+ geom_boxplot(aes(factor(fct)),fill=col,notch = FALSE,size=.6,width=1)+guides(fill=FALSE) + theme_box.plot

		#save top file
		png(file = paste(vars[i],".png",sep=""),pointsize=1,width=45,height=45) # was 90
		print(p4)
		#ggsave(plot = p4, filename= paste(save.file.prefix,tmp.var,".png",sep=""), dpi=300, pointsize=1)
		#plot
		dev.off()	
	}

#use age adjusted data to simplify
	
	
#insert graphs into network
#create node visualizations 
#insert images into nodes
#---------------------------------------------
path<-save.dir
library(RCytoscape)
network<- existing.CytoscapeWindow ('Sheet1.1',  copy.graph.from.cytoscape.to.R=TRUE) # get network from cytoscape

#plot nodes
#Object with cids and names of nodes to plot node inset graphs for 
plot.nodes<-as.character(cids[,2])
image.paths<-paste(sprintf ('file:///%s/%s', path,plot.nodes),".png",sep="")

image.node.info<-cbind(cids,image.paths)
#need to make sure the node is also
net.nodes<-getAllNodes(network) 
image.node.info<-image.node.info[as.character(image.node.info[,1])%in%net.nodes,]


#need to match node names with images

#cids 
setNodeImageDirect (network,as.character(image.node.info[,1]), as.character(image.node.info[,3])) # change node attributes file
setNodeLabelDirect(network,as.character(image.node.info[,1]),as.character(image.node.info[,2])) #fix labels
setNodeShapeDirect(network,as.character(image.node.info[,1]),"ellipse") # fix shape
redraw(network)



#set age-adjusted diffrences between diabetic factor as network node graphs
ffactor<-factor(fct.labels,levels=c("non-diabetic","diabetic"))

col<-I(c("#00FFFFFF","#FF0000FF")) # color scheme young-old (light-dark) non-diab. (green-red)

#look up for variable names
id<-as.numeric(rownames(vvars))
vars<-as.character(unlist(vvars))
save.dir<-"C:\\Users\\dgrapov\\Dropbox\\Metabolomics Core\\Data Analysis\\Manami Hara\\2013\\node images"
setwd(save.dir)

#generate plots
i<-1
for(i in 1:length(vars))
	{
		#box plot	
		p4<- ggplot(data, aes(factor(ffactor), data[,id[i]]))+ geom_boxplot(aes(factor(ffactor)),fill=col,notch = FALSE,size=.6,width=1)+guides(fill=FALSE) + theme_box.plot
		#save top file
		png(file = paste(vars[i],"ageadj",".png",sep=""),pointsize=1,width=45,height=45) # was 90
		print(p4)
		#ggsave(plot = p4, filename= paste(save.file.prefix,tmp.var,".png",sep=""), dpi=300, pointsize=1)
		#plot
		dev.off()	
	}

#add plots to network
#insert graphs into network
#need to translate node names to CIDs
library(RCytoscape)
cy <- CytoscapeConnection ()
network<- existing.CytoscapeWindow ('Sheet1',  copy.graph.from.cytoscape.to.R=TRUE)

#---------------------------------------------
path<-save.dir
#plot nodes
#Object with cids and names of nodes to plot node inset graphs for 
plot.nodes<-as.character(cids[,2])
image.paths<-paste(sprintf ('file:///%s/%s', path,plot.nodes),"ageadj",".png",sep="")

image.node.info<-cbind(cids,image.paths)
#need to make sure the node is also
net.nodes<-getAllNodes(network) 
image.node.info<-image.node.info[as.character(image.node.info[,1])%in%net.nodes,]



#need to match node names with images

#cids 
setNodeImageDirect (network,as.character(image.node.info[,1]), as.character(image.node.info[,3])) # change node attributes file
setNodeLabelDirect(network,as.character(image.node.info[,1]),as.character(image.node.info[,2])) #fix labels
setNodeShapeDirect(network,as.character(image.node.info[,1]),"ellipse") # fix shape
redraw(network)
set


	
#############one sample t-test to identify changes in AUC
data<-start.data
fct<-as.factor(fct) # pre post index
data<-cbind(fct,data)
#use ddply
require(plyr)


test<-function(x,mu){
		c(mean= mean(x),
		sd = sd(x),
		p.value = tryCatch(t.test(x,mu=mu)$p.value,error = function(e) {NA}))
	}

out.put<- ddply(.data=data, .(fct), colwise(test,mu=0),
    .progress = "text", .inform = FALSE, .drop = TRUE,
    .parallel = FALSE, .paropts = NULL)

# output
out.put<-as.data.frame(t(out.put));colnames(s)<-rep(c("mean","stdev","p-value"),length(.(fct)))


#add adjusted p and q.values
#get index for p-values in above results
id<-seq(1, nlevels(fct)*3)
cols<-id[id%%3==0]
adjusted.p<-lapply(1:length(cols),function(i)
	{
		p.values<-as.numeric(as.matrix(out.put[,cols[i],drop=FALSE]))
		p.vals<-as.data.frame(p.adjust(p.values, method = "BH", n = length(p.values)))
		q.vals<-tryCatch(qvalue(p.values,lambda=.5, robust=TRUE)$qvalues,error=function(e){matrix("Can not estimate",nrow=length(p.values),ncol=1)})	
		data.frame(adj.p.value = p.vals, q.value = q.vals)
	})



###fixing network size and color
path<-save.dir
library(RCytoscape)
network<- existing.CytoscapeWindow ('PLS_DA PRE',  copy.graph.from.cytoscape.to.R=TRUE) # get network from cytoscape

#plot nodes
#Object with cids and names of nodes to plot node inset graphs for 
plot.nodes<-as.character(cids[,2])
image.paths<-paste(sprintf ('file:///%s/%s', path,plot.nodes),".png",sep="")

image.node.info<-cbind(cids,image.paths)
#need to make sure the node is also
net.nodes<-getAllNodes(network) 
image.node.info<-image.node.info[as.character(image.node.info[,1])%in%net.nodes,]


#need to match node names with images

#cids 
setNodeImageDirect (network,as.character(image.node.info[,1]), as.character(image.node.info[,3])) # change node attributes file
setNodeLabelDirect(network,as.character(image.node.info[,1]),as.character(image.node.info[,2])) #fix labels
setNodeShapeDirect(network,as.character(image.node.info[,1]),"ellipse") # fix shape
redraw(network)



x<-log(1:10,base=10)

for(i in 1:length(x))
	{
		print(paste("x=",x[i]))	
	
	}
	

re<-t.test(x, x*rnorm(length(x)))$p.value
	
}