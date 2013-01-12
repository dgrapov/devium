
#collapse columns as strings
join.columns<-function(obj)
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
											sobj[,i]<-paste(as.character(obj[,i]),as.character(obj[,i+1]),sep="|")
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
		res<-character()
		need<-as.matrix(sapply(1:length(pkg),function(i)
			{
				if(require(pkg[i],character.only = TRUE)==FALSE)
					{
					 res<-c(res,pkg[i])
					}
			}))
			if(!any(need=="NULL"))
				{
					x<-apply(need,1,install.packages)
					tryCatch(apply(need,1,library, character.only= TRUE),error=function(e){paste("could not get one of the packages: ",paste(as.character(need),collapse=", "),sep="")})
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
