#list to character
fixlc<-function(obj){as.character(unlist(obj))}

#list to numeric
fixln<-function(obj){as.numeric(as.character(unlist(obj)))}

#import from clipboard
read.excel <- function(type="with.dimnames") {
						  #assume row and colnames are included but may not be unique
						  tmp<-read.table("clipboard-128",sep="\t")
						  devium.data.format(tmp, type)
						}

#write to clipboard 
write.to.clipboard<-function(obj,type="no.name"){

	#obj = to be written to clipboard
	#type determines dimensions and their names
	
	switch(type,
	with.dimnames = .local<-function(obj)
							{
								obj.names<-list(rownames = rownames(obj), colnames = colnames(obj))
								obj<-obj
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
							
		with.name = .local<-function(obj)
							{
								obj.names<-list(1:((length(as.character(unlist(obj))))-1),as.character(unlist(obj)[1]))
								obj<-as.data.frame(as.character(unlist(obj))[-1])
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
		no.name = .local<-function(obj)
		{
			obj.names<-list(1:((length(as.character(unlist(obj))))),"NA")
			obj<-as.data.frame(as.character(unlist(obj)))
			is.error<-NULL
			is.error<-tryCatch(dimnames(obj)<-obj.names,
			error=function(e)
					{ 
						NULL
					})#need to set as protected 
			if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
		})
		
	tmp<-.local(obj) 
	#bind row names and col names with obj
	tmp.obj<-cbind(c("",rownames(tmp)),rbind(colnames(tmp),tmp))
	write.table(as.matrix(tmp.obj),"clipboard-128",sep="\t",row.names=FALSE,col.names=FALSE)
}

#format data objects: identify headers and meta data, and give sepearately 
devium.data.format<-function(obj, type){
	#type can be with.dimnames
	# with.meta.data
	
	switch(type,
		with.dimnames = .local<-function(obj)
							{
								obj.names<-list(rownames = as.character(unlist(obj[-1,1])), colnames = as.character(unlist(obj[1,-1])))
								#obj<-as.data.frame(matrix(as.numeric(as.character(unlist(obj[-1,-1]))),nrow=length(obj.names$rownames), ncol=length(obj.names$colnames))) # as.numeric
								obj<-as.data.frame(matrix(as.character(unlist(obj[-1,-1])),nrow=length(obj.names$rownames), ncol=length(obj.names$colnames)))
								
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
							
		with.name = .local<-function(obj)
							{
								obj.names<-list(1:((length(as.character(unlist(obj))))-1),as.character(unlist(obj)[1]))
								obj<-as.data.frame(as.character(unlist(obj))[-1])
								is.error<-NULL
								is.error<-tryCatch(dimnames(obj)<-obj.names,
								error=function(e)
										{ 
											NULL
										})#need to set as protected 
								if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
							},
		no.name = .local<-function(obj)
		{
			obj.names<-list(1:((length(as.character(unlist(obj))))),"NA")
			obj<-as.data.frame(as.character(unlist(obj)))
			is.error<-NULL
			is.error<-tryCatch(dimnames(obj)<-obj.names,
			error=function(e)
					{ 
						NULL
					})#need to set as protected 
			if(is.null(is.error)){list(data=obj,rownames=obj.names$rownames,colnames=obj.names$colnames)} else {obj}
		},
		with.meta.data = .local<-function(obj)
							{
							#look for "" and set headers based on first non "" row and column (add other patterns here)
							object<-obj
							
							#identify boundaries of data the rest is meta data
							sample.start<-max(which(object[1,]=="")+2)
							variable.start<-max(which(object[,1]=="")+2)

							#data
							drows<-nrow(object)
							dcols<-max(c(1:ncol(object))[!sapply(1:ncol(object),function(i){sum(is.na(object[,i]))==drows})])
							tmp.data<-as.data.frame(as.matrix(object[c(variable.start:drows),c(sample.start:dcols)]))
							.names<-dimnames(tmp.data)
							
							#make sure all factors converted to numeric 
							tmp.data<-as.data.frame(matrix(as.numeric(as.character(unlist(tmp.data))),nrow=nrow(tmp.data), ncol=ncol(tmp.data)))
							dimnames(tmp.data)<-.names
							
							#get variable and sample meta data
							row.meta<-as.data.frame(t(as.matrix(object[c(1:(variable.start-2)),c(sample.start:dcols)])))
							col.meta<-as.data.frame(as.matrix(object[c(variable.start:drows),c(1:(sample.start-1))]))
							
							#attempt to set colnames
							tryCatch(colnames(row.meta)<-as.character(unlist(obj[1:(variable.start-2),(sample.start-1)])),errro=function(e){})
							tryCatch(colnames(col.meta)<-as.character(unlist(obj[(variable.start-1),1:(sample.start-1)])),errro=function(e){})
							
							
							#convert data to all numeric prior to return
							data<-do.call("cbind",unclass(tmp.data));dimnames(tmp.data)<-.names # need to break factors

							#return results as a list
							list(data=tmp.data,row.metadata=row.meta,col.metadata=col.meta)
							})					
	.local(obj)
		
}

#format binbase output (data with meta data structure)
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

#return objects to excel	
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

#get object from EXCEL	1
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
				
#collapse columns as strings
join.columns<-function(obj,char="|",quote.last=FALSE)
        {
		
			if(class(obj)=="list"){obj<-as.matrix(obj[[1]])} else {obj<-as.matrix(obj)}
			if(length(obj)==0)
				{
					return(NULL)
					}else{
						if(ncol(as.matrix(obj))>=2)
							{
									n<-ncol(obj)
									out<-data.frame()
									sobj<-obj
									i<-1
									for(i in 1:(n-1))
									{
											if(quote.last==TRUE)
											{
														sobj[,i]<-paste(as.character(obj[,i]),paste("'",as.character(obj[,i+1]),"'",sep=""),sep=char)
											} else {
														sobj[,i]<-paste(as.character(obj[,i]),as.character(obj[,i+1]),sep=char)
											}
									}
									sobj[,-n]
							}else{
									obj
							}
				}
        }

#accesory function to return position of first instance of unique object 
unique.id<-function(obj)
		{
			tmp<-as.factor(obj)
			id<-seq(along=obj)
			sapply(1:nlevels(tmp),function(i)
				{
					id[tmp==levels(tmp)[i]][1]
				})
		}

#function to check for packages and attempt to download if not found
check.get.packages<-function(pkg)
	{
		options(warn=-1)
		
		# #make sure bio conductor is one of the repositories
		# #will need a mechanism to make sure this stays upto date
		# if(!all(names(getOption("repos"))%in%"BioCsoft"))
			# {
				# r<-getOption("repos")
				# r["BioCsoft"]<-"http://www.bioconductor.org/packages/2.11/bioc" # needs to be specific version
				# options(repos = r)
				
				# #add layer to call 
				 # #source("http://bioconductor.org/biocLite.R")
				 # #biocLite("package to install")
				 
				 # if install fails
				# #r.version<-paste(sessionInfo()$R.version$major,sessionInfo()$R.version$minor, sep=".")
				# #bioc.url<-paste("http://www.bioconductor.org/packages/",r.version,"/bioc", sep="")
				# #r["BioCsoft"]<-bioc.url # needs to be specific to R version
				# #options(repos = r)
			# }	
		
		res<-character()
		
		need<-as.matrix(sapply(1:length(pkg),function(i)
			{
				
				if(require(pkg[i],character.only = TRUE)==FALSE)
					{
					 res<-c(res,pkg[i])
					}
			}))
			
			need<-as.character(unlist(need[!is.null(need)]))
			if(length(need)>0)
				{
					
					x<-sapply(need,install.packages)
					lib.fun<-function(need){
							sapply(1:length(need), function(i){
							out<-tryCatch(library(need[i], character.only= TRUE), error=function(e){need[i]})
							if(all(out==need[i])){need[i]}
							})
						}
						
					out<-as.character(unlist(lib.fun(need)))
					
					#try bioconductor
					if(length(out)>0){
					cat(paste("Package not found, trying Bioconductor..."),"\n")
					source("http://bioconductor.org/biocLite.R")
					lib.fun.bioc<-function(need){
							sapply(1:length(need), function(i){
							tryCatch( biocLite(need[i]), 
								error=function(e){need[i]})
							})
						}
						
					tmp<-lib.fun.bioc(out)
					final<-lib.fun(tmp)
					if(length(final)>0){cat(paste("could not find package: ",paste(as.character(unlist(final)),collapse=", "),sep=""),"\n")}
					}
					
				}

	}

#function to extract objects based on reference
extract.on.index<-function(database,index=database[,1,drop=FALSE],what,extract.on="row")
	{		
		# the merge function should be used instead
		if(extract.on=="col"){database<-t(database)}
		#assume top row are column names
		col.names<-database[1,]
		
		#first column of what and database are the index for extractions
		# cbind objects based on matching index values
		index.w<-as.character(what)
		ref<-as.data.frame(index)
		
		out<-lapply(1:ncol(ref),function(j)
			{
				index.d<-as.character(ref[,j])#as.character(database[,1])
				index.w<-index.w[index.w%in%index.d]
				tmp.database<-database
				out<-matrix(NA,length(index.w),ncol(tmp.database),1)
				e.index<-c(1:nrow(database))
				i<-1
				for(i in 1:length(index.w))
				{
					if(any(index.w[i]%in%index.d))
					{
						ext<-unique(e.index[index.w[i]==index.d])[1]
						out[i,]<-tmp.database[ext,]
					}
			
				}

				# placing rownames in a column to avoid duplicate error
				out<-as.matrix(cbind(index.w,out))
				
				if(extract.on=="col")
					{
						out<-cbind(col.names,as.data.frame(t(out)))
						#fix column names
						col.names<-as.character(unlist(out[1,]))
						out<-out[-1,]
						colnames(out)<-col.names
					}else{
						out<-as.data.frame(out)
						colnames(out)<-col.names
					}
			})
		return(out)
	}

#check object value or set default on condition
if.or<-function(object,if.value=NULL,default,environment=devium)
	{
		obj<-tryCatch(svalue(get(object,envir=environment)),error=function(e){NA})
		if(!any(obj%in%c(if.value,NA))){return(obj)}else{return(default)}
	}

#get unassigned variables from within data frame
gget<-function(obj)
{
	#break obj on $ 
	# [1] = data frame
	# [2] = variable name
	# return object
	tmp<-unlist(strsplit(obj,"\\$"))
	if(!length(tmp)==0) get(tmp[1])[,tmp[2]] else NULL
}
		
#function to connect to google docs
GetGoogleDoc<-function(account,password,connection="new")
	{
		#returns list 
		# [1] = connection name
		# [2] = names of documents
		#  connection = as.character connection name if already made using this function 
		# and stored in the envir= googDocs
		
		#install RGoogleDocs if not available
		if(require("RGoogleDocs")==FALSE)
			{
				install.packages("RGoogleDocs", repos = "http://www.omegahat.org/R", type="source")
				library("RGoogleDocs")
			}
			
		if(connection == "new")
			{
					#make time stampped name for connection
					con.name<-con.name.txt<-paste('connection',format(Sys.time(), "%Y.%m.%d_%I_%M_%p"), sep = ".")
				
					#set options to avoid ssl error 
					options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
					
					#assign to new envir
					assign("googDocs",new.env(),envir=.GlobalEnv)
					assign(con.name,getGoogleDocsConnection(getGoogleAuth(account, password, service ="wise")), envir = googDocs )	
			} else {
					con.name<-con.name.txt<-connection
					}		
					
		docs<-getDocs(tryCatch(get(con.name,envir=googDocs),error=function(e){stop(paste(connection, " does not exist","\n"))}))
		dnames<-names(docs)
		return(list(connection = con.name.txt , names = dnames))
	}

#function to view excel objects	
viewExcelObject<-function(obj.path)
	{
		#connect to file and view: 
		#worksheet names
		#named ranges
	
		if(require("XLConnect")==FALSE)
			{
				install.packages("XLConnect")
				library("XLConnect")
			}
		
		#load workbook
		old.dir<-getwd()
		wd<-dirname(obj.path)
		workbook<-basename(obj.path)
		setwd(wd)
		wb = loadWorkbook(workbook, create = FALSE)
		
		#get sheet names
		all.worksheets<-getSheets(wb)
		
		#get all valid named ranges
		all.named.ranges<-getDefinedNames(wb, validOnly=TRUE)
		setwd(old.dir)
		return(list(worksheets=all.worksheets,named.ranges=all.named.ranges))
1	}

#accesory functions based/from package pmg
#----------------------------------------------------
Paste<-function (..., sep = "", collapse = "") 
	{
		x = unlist(list(...))
		x = x[!is.na(x)]
		x = x[x != "NA"]
		paste(x, sep = sep, collapse = collapse)
	}

is.gWindow<-function (obj) 
	{
		is(obj, "gWindowRGtk")
	}

rpel<-function (string, envir = .GlobalEnv) 
	{
		eval(parse(text = string), envir = envir)
	}
	
#function to calculate placement of list items into an Excel worksheet
list.placement.full<-function(data.list,list.names,direction,start.col,start.row,spacer)
	{
        
		#accessory fxn
		list.object.dim.full<-function(data.list,list.names)
        {
                l.dim<-list()
                n<-length(data.list)
                i<-1
                for(i in 1:n)
                {
                tmp.list<-as.data.frame(data.list[[i]])
                height<-dim(tmp.list)[1]
                width<-dim(as.data.frame(tmp.list))[2]
                l.dim[[i]]<-as.data.frame(matrix(cbind(width,height),ncol=2))
                }
                        out<-do.call("rbind",l.dim)
                        out<-cbind(list.names,out)
                        colnames(out)<-c("objects","width","height")
                        out
        }
		
		set.1<-list.object.dim.full(data.list,list.names)
        col.i<-rbind(matrix(LETTERS,ncol=1),matrix(paste(rep(LETTERS,each=length(LETTERS)),rep(LETTERS,length(LETTERS)),sep=""),ncol=1))
        row.i<-matrix(1:1e6,ncol=1)
        place.row<-matrix()
        place.col<-matrix()
        place.range<-matrix()
        columns<-matrix()
        rows<-matrix()
        n<-dim(set.1)[1]
        i<-1
                for(i in 1:n)
					{
                        place.row[i]<-start.row+sum(unlist(set.1[1:i,3]))+spacer*(i-1)-unlist(set.1[i,3])
                        place.col[i]<-col.i[start.col+sum(unlist(set.1[1:(i),2]))+spacer*(i-1)-unlist(set.1[i,2])]
                        if(direction=="vertical"){
                        place.range[i]<-matrix(paste(col.i[start.col],place.row[i],sep=""),ncol=1)} else{
                        if(direction=="horizontal"){
                        place.range[i]<-matrix(paste(place.col[i],start.row,sep=""),ncol=1)}}}
                        ex.range<-as.data.frame(cbind(set.1,place.range))
                        ex.range
	}

#function to get gwidget svalues for assigned widgets
d.get<-function(object, main.object="devium.pca.object",envir=devium)
	{
		#check to see if main object exists else create
		sapply(1:length(object),function(i)
			{
				tmp<-svalue(get(object[i],envir=envir))
				d.assign(add.obj=object[i],value=tmp,main.object,envir=envir)
			})
	}
	
#function to get gwidget svalues for assigned widgets
#main.object and its envir as string
check.get.obj<-function(object, main.object="devium.pca.object",envir="devium")
	{
		#check to see if main object exists else create
		check.get.envir(main.object,envir)
		env<-get(envir)
		sapply(1:length(object),function(i)
			{
				tmp<-svalue(get(object[i],envir=get(envir)))
				d.assign(add.obj=object[i],value=tmp,main.object,envir=get(envir))
			})
	}	

#check to see if environment exists else create one
check.get.envir<-function(main.object,envir)
	{
				if(!exists(envir)){ assign(envir,new.env(),envir= .GlobalEnv)} 
				if(!exists(main.object,envir=get(envir))){assign(main.object,list(),envir=get(envir))}
	}
	
 #fxn to create the environment "devium" if it does not exist
 create.devium.env<-function()
	{
		if(!exists("devium"))
				{
				if(!is.environment("devium")){ assign("devium",new.env(),envir= .GlobalEnv)}
				
				#check for devium objects and set to null if they don't exist
				for (i in c("devium.helpBrowser.window", "devium.plotnotebook.window")) 
					{
					if(!exists(i))
						{
							assign(i, NULL, envir = devium)
						}
					}
				}
	}	
	  
 #function to make assignments to storage object
 d.assign<-function(add.obj,value,main.object,envir=devium) #main.object="devium.pca.object"
	{
		.local<-function()
			{
				tmp<-get(main.object,envir=envir)
				tmp[[add.obj]]<-value
				assign(get("main.object"),tmp,envir=devium)
			}
			tryCatch(.local(),error=function(e){})
	}
	
#from plyr: get as text
.<-function (..., .env = parent.frame()) 
{
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}
	
#load devium objects (outdated)
source.dir<-function(type="file",dir=getwd(),
	file.list=c("https://raw.github.com/dgrapov/devium/master/R/Devium%20GUI%20elements.r",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20Plotting%20Functions.r",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20common%20functions.R",
				"https://raw.github.com/dgrapov/devium/master/R/Devium%20network%20functions.r"))
	{
		#check to see the type of source
		switch(type,
		"file" = .local<-function(file.list)
					{
						o.dir<-getwd()
						setwd(dir)
						obj<-dir()
						sapply(1:length(obj),function(i)
							{
								tryCatch(source(obj[i]),error=function(e){print(paste("can't load:",obj[i]))})
							})
						setwd(o.dir)	
					},
		"https" = .local<-function(file.list)	
					{
						if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
						if(is.null(file.list)){return()}else{obj<-file.list}
						sapply(1:length(obj),function(i)
						{
							tryCatch( eval( expr = parse( text = getURL(obj[i],
							   ssl.verifypeer=FALSE) ),envir=.GlobalEnv),error=function(e){print(paste("can't load:",obj[i]))})
						})
					}
				)
			.local(file.list=file.list)
	}
