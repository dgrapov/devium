#looks useful # https://subversion.assembla.com/svn/gWidgets-df/trunk/Filter.R
#book on gwidgets: http://eom.pp.ua/books/%D0%9A%D0%BE%D0%BF%D1%8C%D1%8E%D1%82%D0%B5%D1%80%D1%8B%D0%98%D1%81%D0%B5%D1%82%D0%B8/%D0%9F%D0%BE%D0%BF%D1%83%D0%BB%D1%8F%D1%80%D0%BD%D1%8B%D0%B5%20%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D1%8B/S-PLUS%20R/Programming%20Graphical%20User%20Interfaces%20in%20R.pdf

#common accesory functions based from package = pmg
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
#-----------------------------------------------------

#Accesory functions and GUIs
done.info.GUI<-function(message)
		{
			done<-gwindow("Information",width = 200, height= 100)
			g<-glayout(cont = done)
			g[2,1]<-glabel(message,container = g)
			g[3,3]<-gbutton("ok",container = done, handler = function(h,...){dispose(done)})
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
	}

#2D scatter plot for 2 vectors or basic pairs plot for a data.frame 
devium.scatter.plot<- function(container=NULL) 
 {
	#container= gwindow("test")
	
	#making the GUI------------------------
	#create notebook container for GUI
	mainWin = ggroup(horizontal = FALSE, container = container)
	
	#gvarbrowser(container=container)
	
	#make tool bar on top
	make.tool.bar<-function(container=NULL)
	{
		
			mainWin = ggroup(horizontal = FALSE, container = container)
	
			#make tool bar on top
			buttonBar = ggroup(spacing = 0,container=mainWin)
			add(mainWin, buttonBar)
			
			#setting options second tool bar
			toolbar = list()

			toolbar$save$icon = "save"
			#toolbar$tmp1$separator = TRUE
			toolbar$save$handler = function(h, ...){
				tmp<-tryCatch(get("devium.plotnotebook.window",envir=devium),error= function(e){NULL})
				if (is.null(tmp) || !is.gWindow(tmp) ||  is.invalid(tmp)) {
					tmp<-gwindow("D E V I U M plot notebook", visible = TRUE)  
					add(tmp, ggraphicsnotebook())
					 assign("devium.plotnotebook.window", tmp, envir = devium)
				}
				else {
					focus(get("devium.plotnotebook.window",envir=devium)) <- TRUE
				}
			}

			#toolbar$tmp2$separator = TRUE
			toolbar$plotnotebook$icon = "plot"
			toolbar$plotnotebook$handler = function(h, ...) {
			refresh.plot()
			}

			toolbar$help$icon = "help"
			toolbar$help$handler = function(h, ...) 
			{
				done.info.GUI("to do--> write help file.")
			}	
			tmp = gtoolbar(toolbar)
			add(buttonBar, tmp, expand = TRUE)
		}

	make.tool.bar(container=mainWin)
	
	#notebook to hold options
	.notebook<-gnotebook(tab.pos=2,container=mainWin,pageno=1)

	
	#options for points
	#-------------------------------------------
	plot.opts<-c("Xaxis","Yaxis","  color","  size","  shape","  border","  width") # form refrence
	plot.defaults<-c(x=NA,y=NA,bg="gray",cex=1,pch=21,col="black",lwd=1)	#fxn defaults
	plot.names<-c("x","y","bg","cex","pch","col","lwd") # fxn input possibilities
	
	# to left align (has to be an easier way!)
	#check length and add white space to make all the same length
	ln<-sapply(strsplit(plot.opts,""),length)
	aligned.plot.opts<-sapply(1:length(plot.opts),function(i)
		{
			spaces<-(max(ln)-ln[i])
			#convert underscore to spaces
			paste(gsub("_"," ",plot.opts[i]),paste(rep(" ",spaces),collapse=""),sep="")
		})
	
	# point properties
	point.var<-glayout(container=.notebook,label="Points")
	
	i<-1
	for(i in 1:length(plot.opts))
		{	
			point.var[(i+1),2]<-assign(plot.opts[i],gedit("", container=point.var)) #,envir=devium
			point.var[(i+1),1]<-glabel(plot.opts[i],width=5)
		}
	
	#add color alpha
	point.var[9,2]<-assign("color.alpha",gslider(from = 0.1, to = 1,by=0.01,value=.75),envir=devium)
	point.var[9,1]<-glabel("transparency")
	
	#for mapping point plotting properties (names up top)
	point.var[1,3]<- glabel("  n  |  levels    options/offset")


	
	# make discreet or use n levels for mapping
	#bg color
	point.var[4,3]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.bg.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.bg",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp),envir=devium)
	#pallet
	point.var[4,4]<- assign("color.pallet.bg",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE,container=tmp),envir=devium)
	
	#bordercolor
	point.var[7,3]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.col.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.col",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp),envir=devium)
	point.var[7,4]<- assign("color.pallet.col",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE,container=tmp),envir=devium)
	
	i<-5
	for(i in 5:8)
		{
			point.var[i,3]<-tmp<-ggroup(horizontal=TRUE)
			assign(paste(plot.opts[i],"levels.n",sep="."),gcheckbox("n",container=tmp))
			assign(paste(plot.opts[i],"levels",sep="."),gspinbutton(from=0, to = 20, selected = 1, editable = FALSE,container=tmp))
			if(!i%in%c(7))
				{
					assign(paste(plot.opts[i],"decrease",sep="."),gbutton("-",container=tmp))
					assign(paste(plot.opts[i],"increase",sep="."),gbutton("+",container=tmp))
				}
		}
		
	#offset	
	
	#options for groups
	#-------------------------------------------
	group<-glayout(container = .notebook,label="Groups")
	group[1,1]<-glabel("  variable")
	group[1,2]<-assign("group.type.var",gedit("",container=group),envir=devium)# choosing type of visualization
	group[1,3]<-glabel("visualization")
	group[1,4]<-assign("group.type",gcombobox(c("none","ellipse","polygon","cluster","ellipse.cluster"), selected = 1, editable = FALSE,container=group),envir=devium)
	
	#setting color and grouping options
	group[2,2]<-tmp<-ggroup(horizontal=TRUE)
	group[2,1]<-glabel("  color pallet")
	group[2,2]<-assign("color.pallet.group",gcombobox(c("rainbow","heat","topo","terrain","chromatic"), selected = 1, editable = FALSE),envir=devium)
	group[3,1]<-glabel("  levels")
	group[3,2]<-tmp<-ggroup(horizontal=TRUE)
	assign("color.levels.group.n",gcheckbox("n",container=tmp),envir=devium)
	assign("color.levels.group",gspinbutton(from=1, to=100, selected = 1, editable = FALSE,container=tmp),envir=devium)
	
	# visual options
	group[4,1]<-glabel("  transparency")
	group[4,2]<-assign("group.transparency",gslider(from = 0, to = 1, by = .05,value=0.5),envir=devium)
	group[4,3]<-tmp<-ggroup(horizontal=TRUE)
	glabel("line width",container=tmp)
	assign("group.width",gspinbutton(from = 0, to = 10, by = 1,value=1,container=tmp),envir=devium)
	group[4,4]<-tmp<-ggroup(horizontal=TRUE)
	glabel("line type",container=tmp)
	assign("group.line.type",gspinbutton(from = 0, to = 6, by = 1,value=1,container=tmp),envir=devium)
	# C.I. for ellipse
    group[5,1]<-glabel("  ellipse level")
	group[5,2]<-assign("ellipse.level",gslider(from = 0, to = 1, by = .01,value=0.95),envir=devium)
	
	#global plotting options
	global<-ggroup(container = .notebook,label="Global")
	
	#legend plotting options
	legend<-ggroup(container = .notebook,label="Legend")
	svalue(.notebook)<-1
	#GUI Done next need functions for handlers
	#-------------------------------------------
	
	
	#add handlers for drag and drop objects
	#should minimaly change name dislayed and call a fxn
	#which gathers inputs from all fomr fields
	#based on 
	# plot.opts 	= names of drag and drop objects
	# plot.defaults = generics for NA or NULL
	# plot.names 	= names of objects in another fxn
	
	#function to update form with droped objects name
	drop.names<-function(name)
		{
			#name = form obj name
			adddroptarget(name,handler = function(h,...) 
				{
					svalue(name)<-id(h$dropdata)
				})

		}
	
	#function to gather all form inputs
	#based on plot opts 	
	gather.names<-function(names)
		{
			tmp<-lapply(1:length(names),function(i)
				{
					svalue(get(names[i]))
				})
			names(tmp)<-names	
			return(tmp)			
		}
	
	# match names between objects (form and fxn)
	# and get object values
	get.named<-function(obj)
		{
			
			id<-names(obj)
			tmp<-lapply(1:length(id),function(i)
				{
					tryCatch(gget(svalue(get(id[i]))),error=function(e){NULL})
				})
			#match names based on loolup table
			names(tmp)<-id	
			return(tmp)			
		}
	
	#translate between form and fxn based on look up table
	translate.names <-function(names,lookup=cbind(plot.opts,plot.names))
		{
		#lookup 	= 	is a column matrix with the first column the index
		#				for the object
		# 				and subsequent columns the translations
				id<-lookup[,1]
				lookup[id%in%names,-1,drop=FALSE]				
		}

	#add entry in plot legend object

	set.plot.legend<-function(obj,name="scatter.plot.legend",env=devium)
		{
			#object = get("scatter.plot.legend.ids", env= devium) contains names of mapped objects
		
			#check or make "scatter.plot.legend"
			if(!exists( name,env=devium)){assign(name,list(),env=devium)}
			record<-get(name,env=env)
			
			#append for legend 
			#get unique joint levels
			tmp<-join.columns(obj)
			if(class(tmp)=="NULL")
				{
					return()
				}else{
					tmp<-do.call("rbind",strsplit(unique(tmp),"\\|"))
					colnames(tmp)<-c("name",names(obj))
					record[[names(obj)]]<-as.data.frame(tmp)
					
					#store for legend
					assign(name,record,env=env)
				}
		}
		
	#convert options to fxn inputs and/or set defaults if missing/NULL
	convert.or.set<-function(named.obj,default=plot.defaults)
		{

			#functions to do conversions
			#create "scatter.plot.legend" in env devium
			#convert object to color
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
					
			#convert object to shape
			convert.to.shape<-function(object,from=c(21:25,1:20),legend="pch")
					{
						fct<-as.factor(unlist(object))
						out<-as.numeric(from[1:nlevels(fct)][fct])
						
						#bind with factor for legend
						obj<-list(data.frame(factor = fct,pch=out))
						names(obj)<-legend
						#save to legend 
						set.plot.legend(obj,name="scatter.plot.legend",env=devium)
						return(out)
					}

			#convert to size
			convert.to.size<-function(object,from=c(1:100),legend="cex")
					{
						fct<-as.factor(unlist(object))
						out<-as.numeric(from[1:nlevels(fct)][fct])
						
						#bind with factor for legend
						obj<-list(data.frame(factor = fct,pch=out))
						names(obj)<-legend
						#save to legend 
						set.plot.legend(obj,name="scatter.plot.legend",env=devium)	
						return(out)
					}
			
			id<-names(named.obj)
			obj<-lapply(1:length(id),function(i)
				{	
					
					out<-switch(id[i],
								x 		= named.obj[i],
								y 		= named.obj[i],
								bg 		= convert.to.color(named.obj[i],legend="bg", pallet=if.or("color.pallet.bg",default="rainbow"),alpha=if.or("color.alpha",default=.75)),
								cex 	= convert.to.size(named.obj[i],legend="cex"),
								pch 	= convert.to.shape(named.obj[i],legend="pch"),
								col 	= convert.to.color(named.obj[i],legend = "col",pallet=if.or("color.pallet.col",default="rainbow"),alpha=if.or("color.alpha",default=.75)),
								lwd		= convert.to.size(named.obj[i],legend="lwd"),
								xlab 	= named.obj[i],
								ylab 	= named.obj[i])
								
					#can't use NULL above due to exclusion in string		
					# need to set the type of the variable character or numeric
					if(class(out)=="NULL"|length(out)==0) assign("out",default[i])
					if(any(is.na(out))) out<-default[i]
					unlist(out)
				})	
				names(obj)<-id
				return(obj)		
		}
	
	#generate fxn input from form output
	get.inputs<-function(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults)
		{
			tmp<-gather.names(form.names)
			assign("scatter.plot.legend.ids",tmp,env=devium)
			tmp<-get.named(tmp)
			tmp.names<-translate.names(names(tmp),lookup=lookup)
			names(tmp)<-tmp.names
			convert.or.set(tmp,default)
		}
		
	#fxn to make plot 
	new.plot<-function(fxn,tmp,layer=if.or("group.type",default="none"))
		{
		#need to add or new plot control 
		if(names(dev.cur())=="null device"){x11()} #dev.new("X11")
		
		switch(fxn,
			plot = 	.plot<-function(fxn) 
				{
					#initialize plot
					do.call(fxn,
							list(
									x=tmp$x,
									y=tmp$y,
									type="n",
									frame.plot=FALSE,
									xlab=as.character(tmp$xlab), 
									ylab=as.character(tmp$ylab)
								))
					
					#add layers
					#collect layer options
					layer.par<-list()
					layer.par$lwd<-if.or("group.width",default=1)
					layer.par$lty<-if.or("group.line.type",default=1)
					layer.par$color<-if.or("group.color",default=tmp$bg)
					layer.par$ellipse.level<-if.or("ellipse.level",default=.95)
					layer.par$transparency<-if.or("group.transparency",default=.5)
					
					
					switch(layer,
						none 			= .layer<-function(){return()},
						cluster 		= .layer<-function(){edge.group(obj=cbind(tmp$x,tmp$y),color=tmp$bg,lwd=layer.par$lwd,lty=layer.par$lty)},
						ellipse 		= .layer<-function(){ellipse.group(obj=cbind(tmp$x,tmp$y),color=tmp$bg,lwd=layer.par$lwd,lty=layer.par$lty,
											border="black",ellipse.level=layer.par$ellipse.level,show.polygon=TRUE,alpha=layer.par$transparency)},
						polygon 		= .layer<-function(){polygon.group(obj=cbind(tmp$x,tmp$y),color=tmp$bg,lwd=layer.par$lwd,lty=layer.par$lty,
											border="black",show.polygon=TRUE,alpha=layer.par$transparency)},
						ellipse.cluster = .layer<-function()
												{
													ellipse.group(obj=cbind(tmp$x,tmp$y),color=tmp$bg,lwd=layer.par$lwd,lty=layer.par$lty,
														border="black",ellipse.level=layer.par$ellipse.level,show.polygon=TRUE,alpha=layer.par$transparency)
														edge.group(obj=cbind(tmp$x,tmp$y),color=tmp$bg,lwd=layer.par$lwd,lty=layer.par$lty)
												} 
							)
				
					#avoid for ideimensional objects					
					if(!is.null(tmp$x)&!is.null(tmp$y)){.layer()}					
					
					#add points
					do.call("points",
							list(
									x=tmp$x,
									y=tmp$y,
									col=tmp$col,
									bg=tmp$bg,
									cex=as.numeric(tmp$cex),
									pch=as.numeric(tmp$pch),
									lwd=as.numeric(tmp$lwd)
								))
								
					#make legend
					legend<-get("scatter.plot.legend",env=devium)
					if(length(legend)==0) 
						{
							return() 
						} else {
							obj<-do.call("rbind",format.for.legend(legend.list=legend,limit=5))
							make.plot.legend(obj)
						}				
				},
							
			pairs = .plot<-function(fxn) 
				{
				do.call(fxn,
							list(
									x=tmp$x,
									col=tmp$col,
									bg=tmp$bg,
									frame.plot=FALSE,
									cex=as.numeric(tmp$cex),
									pch=as.numeric(tmp$pch),
									lwd=as.numeric(tmp$lwd)
								)
							)
				})
				
			.plot(fxn)	
			bringToTop(which=dev.cur())
		}
	
	#make plot legend
	format.for.legend<-function(legend.list=get("scatter.plot.legend",env=devium),limit=4)
			{
				check.get.packages("gtools")
				#take a named list and place items in columns for merged legend or separate for separate and
				#format
				#obj structure by colummns 
				#	[1] name 
				#	[2] color 
				#	[3] shape
				#	[4] outline color 
				#	[5] outline width 
				#	[6] size
				
				#use to set defualt if error 
				def.val<-function()
					{
						tmp<-list()
						tmp$col = "black"
						tmp$bg = "gray"
						tmp$cex = 2
						tmp$pch = 21
						tmp$lwd = 1
						return(tmp)
					}
					
				default<-function(name="",nrow=1)
					{
						tmp<-as.data.frame(matrix(
						c(name=name,bg=rep("#FFFFFF00",length(name)),pch=rep(21,length(name)),col=rep("#FFFFFF00",length(name)),lwd=rep(1,length(name)),cex=rep(2,length(name)))
						,nrow=nrow, ncol=6,byrow=FALSE))
						colnames(tmp)<-c("name","bg","pch","col","lwd","cex")
						return(tmp)
					}
				
				#initialize				
				#obj1<-as.matrix(legend.list[[1]])[as.matrix(order(legend.list[[1]][,"name"])),]	
				#attempt to merge legend on common mapping "name"
				#common<-sapply(2:(length(legend.list)),function(i)
				#	{
				#		obj2<-as.matrix(legend.list[[i]])[order(as.matrix(legend.list[[i]][,"name"])),]
				#		match<-as.matrix(obj1[,"name"])==intersect(as.matrix(obj1[,"name"]),as.matrix(obj2[,"name"]))
				#		tmp<-cbind(obj1[match,1],obj1[match,-1],obj2[match,-1])
				#	})
					
				#convert to default format
				list.names<-names(legend.list)
				
				out<-lapply(1:length(legend.list),function(i)
					{
						
						fill<-default(name=list.names[i] )
						obj<-legend.list[[i]][as.matrix(order(legend.list[[i]][,"name"])),]
						out<-merge(fill,obj,all.x=TRUE,all.y=TRUE)
						#out<-out[,order(colnames(out))] # common order
						
						#fill NA with default values
						def<-def.val()
						tmp<-sapply(1:ncol(out),function(j)
							{
								tmp<-as.matrix(unlist(out[,j]) )
								tmp[is.na(tmp)]<-as.matrix(def[colnames(out)[j]])
								out[,j]<-tmp
							})
						colnames(tmp)<-colnames(out)
						
						#limit output 
						obj<-tmp[-1,]
						#test the number of unique levels
						test<-as.factor(unlist(obj[,1]))
						if(limit<nlevels(test))
							{
								limit.id<-quantile(1:nrow(obj),seq(0,1,length.out=limit))+1
							} else {
								limit.id<-unique.id(test) + 1
							}
							
						tmp[c(1,limit.id),c("name","bg","pch","col","lwd","cex")]				
					})
					
					#ids for mapped objects
					id<-get("scatter.plot.legend.ids", env= devium)
					n<-names(out)<-names(legend.list)
					
					mapped<-function(name)
								{
								switch(name,
									bg = paste("color",id$color),
									pch = paste("shape",id$shape), 
									col = paste("border",id$border),
									cex = paste("size",id$size),
									lwd = paste("width",id$border))
								}
					#only return non-generic legend items
					# identify return index
					to.send<-function(name)
									{
									switch(name,
										bg = if(!id$bg=="") name,
										pch = if(!id$shape=="")name, 
										col = if(!id$border=="")name,
										cex = if(!id$size=="") name,
										lwd = if(!id$width=="") name)
									}
					
					lapply(1:length(n),function(i)
						{
							if(is.null(to.send(n[i])))
								{
									return()
								}else{
									tmp<-out[[n[i]]]
									tmp[1,1]<-mapped(n[i])
									tmp
								}
						})	
								
					}
					
	make.plot.legend<-function(obj,legend.placement="topleft",new=FALSE,legend.ncol=1,legend.cex=.8)
		{
			#obj structure by colummns 
			#	[1] class [2] color [3] point character (required)
			#	[4] outline color [5] outline width (optional)
			#	[6] line or point logic [7] line type for lines (not used)
			
			if(is.null(obj))
				{
					return()
				}else{
				
				plot.legend<-function(place)
					{
						
						if(place=="custom"){ place<-locator(1)}
							legend(place,ncol=legend.ncol, legend=c(as.character(obj[,1])),
							col=as.character(obj[,4]),pt.lwd=as.numeric(obj[,5]),pt.bg=as.character(obj[,2]),pch=as.numeric(obj[,3]),cex=legend.cex,pt.cex=as.numeric(obj[,6]),bty="n")
					}

			if(new==TRUE)
			{
				x11()
				par(mar=c(.1,.1,.1,.1))
				plot(1,1,type="n",xaxt="n",yaxt="n",frame.plot=FALSE,xlab="",ylab="")
			}
			
			plot.legend(legend.placement)
			}	
		}

	#generic call to plot
	refresh.plot<-function(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults)
		{
						
			#gather inputs
			tmp<-get.inputs(form.name=form.names,lookup,default)
		
			#set x and y axis labels 
			tmp$xlab=svalue(Xaxis)#svalue(Xaxis)
			tmp$ylab=svalue(Yaxis)#svalue(Yaxis)
			
			#check to see if x/y is a data.frame (will be NULL in tmp)
			cx<-tryCatch(get(svalue(Xaxis)),error=function(e){NULL})
			cy<-tryCatch(get(svalue(Yaxis)),error=function(e){NULL})
			
			if(class(cx)=="data.frame")
				{
					tmp$x<-cx
				}
				
			if(class(cy)=="data.frame")
				{
					tmp$x<-cy
				}
						
			#select pairs for data.frames
			if(is.data.frame(tmp$x))
				{
						new.plot("pairs",tmp)	
					} else {
						new.plot("plot",tmp)	
				}
		}
	
	#fxn to set set form handlers
	make.plot.handler<-function(form.names,lookup,default)
		{
			handler.name<-paste("handler",form.names,sep=".")
			i<-1
			for(i in 1:length(form.names))
				{
					#form obj
					obj<-get(form.names[i]) #envir=devium
					#gather inputs and plot
					assign(handler.name[i],function(h,...) 
					{
							#upadate form to show drop source
							svalue(h$obj)<-id(h$dropdata)
												
							#gather inputs
							tmp<-get.inputs(form.name=form.names,lookup,default)
						
							#set x and y axis labels 
							tmp$xlab=svalue(Xaxis)#svalue(Xaxis)
							tmp$ylab=svalue(Yaxis)#svalue(Yaxis)
							
							#check to see if x/y is a data.frame (will be NULL in tmp)
							cx<-tryCatch(get(svalue(Xaxis)),error=function(e){NULL})
							cy<-tryCatch(get(svalue(Yaxis)),error=function(e){NULL})
							
							if(class(cx)=="data.frame")
								{
									tmp$x<-cx
								}
								
							if(class(cy)=="data.frame")
								{
									tmp$x<-cy
								}
										
							#select pairs for data.frames
							if(is.data.frame(tmp$x))
								{
										new.plot("pairs",tmp)	
									} else {
										new.plot("plot",tmp)	
								}
					})
			}
				
				#assign handler to form 
				i<-1
				handler.list<-data.frame(form.name=form.names,handler=handler.name)
				for(i in 1:nrow(handler.list))
				{
					adddroptarget(get(as.character(handler.list[i,1])),handler=get(as.character(handler.list[i,2])))
				}
		}
		
	make.plot.handler(form.names=plot.opts,lookup=cbind(plot.opts,plot.names),default=plot.defaults)
	assign("devium.scatter.plot.notebook", mainWin,envir = devium)
	return(mainWin)
	}

#2D scatter plot (using ggplot2) for 2 vectors or basic pairs plot for a data.frame 
devium.qplot<- function(container=NULL) 
 {
	check.get.packages(c("ggplot2","gWidgets","gWidgetsRGtk2"))
	options(device="gWidgetsRGtk2")
	
	#for debugging
	#container= gwindow("test")
	#gvarbrowser(container=container)
	
	#create notebook container for GUI
	mainWin = ggroup(horizontal = FALSE, container = container)
	
	#variables for plots
	#plotting variable labels  and drag n drop boxes widget
	labels<-gexpandgroup(text="Asthetic",horizontal=FALSE, container=mainWin,pos=0)
	
	#create empty space in form using glabel  there is a function for this
	gspacer<-function(number, container)
			{	
				i<-1
				for(i in 1:number)
				#sapply(1:number,function(i) # wont work
					{
						glabel("",container=container)
					}#)
			}	

	gspacer(1,labels)
	
	
	#options for plot asthetics
	plot.opts<-c("x","y","color","size","shape","alpha") # form refrence
	plot.names<-c("x","y","color","size","shape","alpha") # fxn input possibilities
	
	# to left align (has to be an easier way!)
	#check length and add white space to make all the same length
	ln<-sapply(strsplit(plot.opts,""),length)
	aligned.plot.opts<-sapply(1:length(plot.opts),function(i)
		{
			spaces<-(max(ln)-ln[i])
			#convert underscore to spaces
			paste(gsub("_"," ",plot.opts[i]),paste(rep(" ",spaces),collapse=""),sep="")
		})
		
	for(i in 1:length(plot.opts))
		{	
			grp<-paste("tmp",i)
			assign(grp,ggroup(horizontal=TRUE, container=labels))
			assign(plot.opts[i],gedit("", container=get(grp)))
			glabel(aligned.plot.opts[i],container=get(grp))
		}
	
	#add handlers for drag and drop objects
	#should minimaly change name dislayed and call a fxn
	#which gathers inputs from all fomr fields
	#based on 
	# plot.opts 	= names of drag and drop objects
	# plot.defaults = generics for NA or NULL
	# plot.names 	= names of objects in another fxn
	
	#function to update form with droped objects name
	drop.names<-function(name)
		{
			#name = form obj name
			adddroptarget(name,handler = function(h,...) 
				{
					svalue(name)<-id(h$dropdata)
				})

		}
	
	#function to gather all form inputs
	#based on plot opts 	
	gather.names<-function(names)
		{
			tmp<-lapply(1:length(names),function(i)
				{
					svalue(get(names[i]))
				})
			names(tmp)<-names	
			return(tmp)			
		}

	#function to convert output to character for call
	make.text<-function(arg.list)
		{
			out<-as.character(sapply(1:length(arg.list),function(i)
				{
					if(!arg.list[i]=="")
						#make shape a factor
						if(names(arg.list)[i]=="shape")
							{
								paste(names(arg.list)[i],"=","as.factor(",arg.list[i],")")
							}else{
								paste(names(arg.list)[i],"=",arg.list[i])
							}
				}))
			return(out[!out=="NULL"])
		}
	#generate fxn input from form output
	get.inputs<-function(form.names=plot.opts)
		{
			#shape needs to be converted to a factor
			tmp<-gather.names(form.names)
			make.text(tmp)
		}
		
	#fxn to make plot 
	new.qplot<-function(tmp)
		{
			#add some arguments
			plot.arg<-"ggplot2::qplot("
			end.arg<-")"
			x<-paste(plot.arg,paste(tmp,collapse=", "),end.arg)
			if(names(dev.cur())=="null device"){x11() } #dev.new("X11")
			p<-eval(parse(text = as.expression(x)), envir = .GlobalEnv)
			print(p)
			bringToTop(which=dev.cur())
		}
	
	#fxn to set set form handlers
	make.plot.handler<-function(form.names)
		{
			handler.name<-paste("handler",form.names,sep=".")
			i<-1
			for(i in 1:length(form.names))
				{
					#form obj
					obj<-get(form.names[i])
					#gather inputs and plot
					assign(handler.name[i],function(h,...) 
					{
							#upadate form to show drop source
							svalue(h$obj)<-id(h$dropdata)
												
							#gather inputs
							tmp<-get.inputs(form.name=form.names)
							new.qplot(tmp)
					})
			}
				
				#assign handler to form 
				i<-1
				handler.list<-data.frame(form.name=form.names,handler=handler.name)
				for(i in 1:nrow(handler.list))
				{
					adddroptarget(get(as.character(handler.list[i,1])),handler=get(as.character(handler.list[i,2])))
				}
		}
		
	make.plot.handler(form.names=plot.opts)
	#assign("devium.scatter.plot.notebook", mainWin,envir = devium)
	return(mainWin)
	}
	
#function to make gui to lad data from csv or google docs
devium.data.import<-function (container = NULL) 
{
		#for import of: CSVs (read.csv, assume header)
		#google spreadsheets (RGoogleDocs)
		#Excel Worksheet (XLConnect)
		
		main = ggroup(horizontal = FALSE, container = container)
		
		#to load from CSV
        csv = gexpandgroup(text="Import CSV",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = csv)
        tbl = glayout(cont = g)
        tbl[2, 1] <- "Select file         " # has to be a better way to align this below
        tbl[2, 2] <- (filebrowse = gfilebrowse(text = "browse to select", action = invisible, container = tbl, filter = "*.csv", quote = FALSE))
		
		#message for completed load
		done.info.GUI<-function(file.name)
			{
				done<-gwindow("Information",width = 200, height= 100)
				g<-glayout(cont = done)
				g[2,1]<-glabel(paste("Load Successful, data named: ",file.name),container = g)
				g[3,3]<-gbutton("ok",container = done, handler = function(h,...){dispose(done)})
			}
										
		#hitting apply button loads the CSV and 
		tbl[2, 3] <- (gapply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
					{
					#selected file
					theFile = svalue(filebrowse)
					if (!theFile == "browse to select")
						{
							#name for file
							tmp<-unlist(strsplit(basename(theFile), split = "\\."))[1]
							tmp<-chartr("-","_",tmp)
							file.name<-chartr(" ","_",tmp)
						
							#read as csv asume top row = header
							tryCatch(tmp.obj<-read.csv(theFile,header=TRUE),error=function(e){stop("error occured, check file")})
							assign(file.name,tmp.obj,envir=.GlobalEnv)#
							done.info.GUI(file.name)
						}
				}))
				
		#to load from google docs		
		gdocs = gexpandgroup(text="Import Google Spreadsheet",horizontal=FALSE, container=main, pos=2)
		gg = ggroup(horizontal = FALSE, cont = gdocs)
        tbl2 = glayout(cont = gg)
        tbl2[2, 1] <- "Google account"
        tbl2[2, 2] <- (gaccount = gedit("name@gmail.com", container= tbl2))
		tbl2[3, 1] <- "Password"
        tbl2[3, 2] <- (gpassword = gedit("*********", container= tbl2))
		
		# connect button --> connects to google account and gets 
		# list of available spreadsheets to the gdroplist above, asigned items in gdocs env
		# assigns connection to "gconnection" in gdocs env
		tbl2[3, 3] <- (gconnect = gbutton(text = "Connect",container = tbl2, 
			handler = function(h,...)
				{
					tryCatch(
						{
							tmp<-GetGoogleDoc(account=svalue(gaccount),password=svalue(gpassword),connection="new")
							#update dropdownlist
							items<-tmp[[2]]
							assign("items", items, envir=gdocs)
							assign("gconnection",get(tmp[[1]],envir=googDocs),envir = gdocs)
							gspreadsheets[] <- items
							svalue(gspreadsheets, index=TRUE) <- 1
						}, error = function(e){"Check account name and password"})
				}		
				))
		#dropdown box for available spreadsheets
		gdocs<-new.env()
		assign("items", c("not connected"), envir=gdocs)
		tbl2[4, 1] <- "Spreadsheets"
		tbl2[4, 2] <- (gspreadsheets = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl2))
				
		#apply button to load selectd google doc		
		tbl2[4, 3] <- (gapply = gbutton(text = "Apply",container = tbl2, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(gspreadsheets)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from google as csv asume top row = header
							tryCatch(
								{
									tmp.obj<-getWorksheets(theFile,get("gconnection",envir=gdocs))
									#later add ability to load from diffrent sheets in the same workbook
									# for now get first sheet
									target<-names(tmp.obj)[1]
									sheet<-sheetAsMatrix(tmp.obj[[target]],header=TRUE, as.data.frame=TRUE, trim=TRUE)
									assign(file.name,sheet,envir=.GlobalEnv)
									done.info.GUI(file.name)
								},	error=function(e){stop("error occured, check file")})
						}
				}		
				))
		
		#import from Excel workbook
        excel = gexpandgroup(text="Import from Excel",horizontal=FALSE, container=main, pos=2)
		g = ggroup(horizontal = FALSE, cont = excel)
        tbl = glayout(cont = g)
        tbl[2, 1] <- "Select Workbook" # has to be a better way to align this below
        tbl[2, 2] <- (filebrowse.excel = gfilebrowse(text = "browse to select", action = invisible, container = tbl, filter = "*.csv", quote = FALSE))
		
		#dropdown box for worksheets and named ranges
		tbl[4, 1] <- "Spreadsheets"
		tbl[4, 2] <- (espreadsheets = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl))
		tbl[5, 1] <- "Named Ranges"
		tbl[5, 2] <- (enamed.ranges = gcombobox("not connected", selected = 1, editable = FALSE, container = tbl))								
		
		#connect button loads worksheets and named ranges in workbook
		tbl[2, 3] <- (eapply = gbutton(text = "Connect",container = tbl, 
			handler = function(h,...)
					{
					#selected file
					theFile = svalue(filebrowse.excel)
					if (!theFile == "browse to select")
						{
							#get objects and place into dropdown boxes
							excel.objects<-viewExcelObject(theFile)
							
							#populate spreadsheets
							espreadsheets[] <- excel.objects[[1]]
							svalue(espreadsheets, index=TRUE) <- 1
						
							#populate named ranges
							enamed.ranges[] <- excel.objects[[2]]
							svalue(enamed.ranges, index=TRUE) <- 1
						}
				}))
				
		#load worksheet
		#apply button to load selectd google doc		
		tbl[4, 3] <- (eworksheet.apply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(espreadsheets)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from excel (may error of many objects on sheet due to best guess)
							tryCatch(
								{
									old.dir<-getwd()
									obj.path<-svalue(filebrowse.excel)
									wd<-dirname(obj.path)
									setwd(wd)
									workbook<-loadWorkbook(basename(obj.path))
									
									#get worksheet
									tmp.obj<-readWorksheet(workbook, sheet = theFile, header = TRUE,rownames=1)

									assign(file.name,tmp.obj,envir=.GlobalEnv)
									done.info.GUI(file.name)
									setwd(old.dir)
								},	error=function(e){stop("error occured, check excel object")})
						}
				}		
				))
		#load named range		
		tbl[5, 3] <- (eworksheet.apply = gbutton(text = "Apply",container = tbl, 
			handler = function(h,...)
				{
					#selected file
					theFile = svalue(enamed.ranges)
					
					if (!theFile == "not connected")
						{
							#fix name for file (get rid of spaces)
							tmp<-paste(as.character(unlist(strsplit(theFile," "))),collapse="_")
							tmp<-chartr(c("-"),"_",tmp) 
							file.name<-tmp
						
							#now load object from excel (may error of many objects on sheet due to best guess)
							tryCatch(
								{
									old.dir<-getwd()
									obj.path<-svalue(filebrowse.excel)
									wd<-dirname(obj.path)
									setwd(wd)
									workbook<-loadWorkbook(basename(obj.path))
									
									#get named range
									tmp.obj<-readNamedRegion(wb, name = theFile, header = TRUE)

									assign(file.name,tmp.obj,envir=.GlobalEnv)
									done.info.GUI(file.name)
									setwd(old.dir)
								},	error=function(e){stop("error occured, check excel named range")})
						}
				}		
				))		
		assign("devium.data.import.notebook",main,envir=devium)
		return(main)
    }

# function to load R data sets
devium.load.Rdataset<-function (width = 550, height = 400) 
{
			win =gwindow("Load data set", v = T)
			size(win) <- c(width, height)
			group = ggroup(horizontal = FALSE, container = win, expand = TRUE)
			dataSetHandler = function(h, ...) {
				dataSets = svalue(dataSetList, drop = FALSE)
				for (i in 1:nrow(dataSets)) {
					dataset = dataSets[i, 1]
					package = dataSets[i, 2]
					command = Paste("data(", dataset, ",package=\"", 
						package, "\")")
					cat("> ", command, "\n")
					svalue(status) <- Paste("attach data set ", dataset)
					do.call("data", list(dataset, package = package))
					svalue(status)
				}
			}
			
			getDataSets<-function (...) 
				{
					dataSets = data()$results
					dataSets = dataSets[, c(3, 1, 4)]
					return(dataSets)
				}

			dataSetList = gtable(getDataSets(), multiple = TRUE, filter.column = 2, 
				handler = dataSetHandler)
			add(group, dataSetList, expand = TRUE)
			buttonGroup = ggroup(container = group)
			addSpring(buttonGroup)
			gbutton("cancel", container = buttonGroup, handler = function(h, 
				...) dispose(win))
			status = gstatusbar("Double click data set to load", container = group)
			invisible(win)
		}		
	
devium.summary<-function(obj,...)
{
    objName = deparse(substitute(obj))
    if (is.character(obj) && length(obj) == 1) {
        objName = obj
        obj = svalue(obj)
    }
    group = ggroup(horizontal = FALSE, ...)
    icon = stockIconFromClass(class(obj))
    add(group, gimage(icon, dirname = "stock", size = "DIALOG"))
    table = glayout(adjust = "left")
    add(group, table)
    table[1, 1] = glabel("<b>Name:</b>", markup = TRUE)
    table[1, 2] = glabel(objName)
    table[2, 1] = glabel("<b>Kind:</b> ", markup = TRUE)
    table[2, 2] = glabel(paste(class(obj), sep = "", collapse = ", "))
    table[3, 1] = glabel("<b>Size:</b>", markup = TRUE)
	
	#sneaky str
	str1<-function (obj) 
		{
			md <- mode(obj)
			lg <- length(obj)
			objdim <- dim(obj)
			if (length(objdim) == 0) 
				dim.field <- paste("length:", lg)
			else {
				dim.field <- "dim:"
				for (i in 1:length(objdim)) dim.field <- paste(dim.field, 
					objdim[i])
				if (is.matrix(obj)) 
					md <- "matrix"
			}
			obj.class <- oldClass(obj)
			if (!is.null(obj.class)) {
				md <- obj.class[1]
				if (inherits(obj, "factor")) 
					dim.field <- paste("levels:", length(levels(obj)))
			}
			list(type = md, dim.field = dim.field)
		}

    if (!is.function(obj)) {
        theSize = str1(obj)$dim.field
        table[3, 2] = glabel(theSize)
    }
    else {
        table[3, 2] = glabel("NA")
    }
    stamp = NA #Timestamp(obj)
    if (!is.na(stamp)) {
        table[4, 1] = glabel("<b>Last modified:</b>", markup = TRUE)
        table[4, 2] = glabel(format(as.Date(stamp), "%B %d, %Y"))
    }
    table[5, 1] = glabel("<b>Preview:</b>", markup = TRUE)
    theValue = capture.output(eval(obj))
    if (length(theValue) > 10) 
        theValue = c(theValue[1:10], "... 8< snipped >8 ...")
    theHead = gtext(font.attr = c("monospace"))
    add(theHead, theValue)
    enabled(theHead) <- FALSE
    add(group, theHead, expand = TRUE)
    visible(table) <- TRUE
    return(group)
}

#data transformation gui
devium.data<-function()
	{
		#for debugging
		container= gwindow("test")
		
		#call new browser for looking at target object
		main = ggroup(horizontal = FALSE, container = container)
		
		#options for GUI and mapping to call
		gui.opts<-c("Target","Remove","Merge","Subset") # form refrence
		gui.defaults<-c(cut.obj=NA, merge.obj=NA,trans.obj=NA)	#fxn defaults
		fxn.names<-c("cut.obj","y","merge.obj","trans.obj") # fxn input possibilities
        
		#function to try to align text
		try.align<-function(list)
			{
				ln<-lapply(1:length(list),function(i){sapply(strsplit(as.character(list[[i]]),""),length)})# length
				vec<-unlist(ln)
				lapply(1:length(list),function(i)
				{
					
					#convert underscore to spaces
					sapply(1:length(list[[i]]),function(j)
						{
							spaces<-(max(vec)-ln[[i]][j])
							paste(list[[i]][j],paste(rep(" ",spaces),collapse=""),sep="")
						})
					
				})
			}
		
		#make a list to hold options
		make.list <-list()
		make.list$gpexp<-gui.opts #expand group names
		make.list$edits<-list("Data",c("Remove"),c("With","Index"),c("Only","Excluding")) #list for making editable (blank input) space
		make.list$labels<-try.align(list("Data  ","Name  ",c("With","Index"),c("Only","Excluding")))#names for edit boxes)
		make.list$buttons<-list(NA,c("Apply"),c(NA,"Apply"),c("Apply","Apply")) #list of button names (NA = avoid)
		
			
		#upon drop each should write its input to an approriate obj in  env=devium

		
		i<-1
		for(i in 1:length(make.list$gpexp))
			{	
				exp.group<-make.list$gpexp[[i]] # expand group text
				tmp.main<-gframe(container=main) # its frame
				tmp<-gexpandgroup(text=exp.group, container=tmp.main, pos=2) #make expand group
				#object to hold label; edit, button combo
				grp<-paste("tmp",i)
				assign(grp,ggroup(horizontal=FALSE, container=tmp))
				table = glayout(adjust = "right")
				cont<-add(get(grp), table)
				#populate expand group base on edits
				tmp<-tryCatch(make.list$edits[[i]],error=function(e){NA})
				j<-1
				for(j in 1: length(tmp))
					{
						
						#try to make edit, label , button 
						#label
						tmp<-tryCatch(make.list$labels[[i]][j],error=function(e){NA})
						if(!is.na(tmp))
							{
								table[j,1]<-glabel(tmp,container=cont,font.attr = list(c(family="monospace")))
							}
						#edit
						tmp<-tryCatch(make.list$edits[[i]][j],error=function(e){NA})
						if(!is.na(tmp))
							{
								table[j,2]<-assign(tmp,gedit("", container=cont))
							}
					
						#button
						tmp<-tryCatch(make.list$buttons[[i]][j],error=function(e){NA})
						if(!is.na(tmp))
							{
								table[j,3]<-gbutton(text = tmp,container = cont )
							}
					}	
			} 
				
				
	}
	
#completely taking out pmg functions 
DEVIUM.GUI<-function (width = 975, height = .75 * width) 
{	
	#main window dimensions and options
	rightWidth = width * 0.6
	mainHeight = height * 0.8
	options(guiToolkit = "RGtk2")
	cliType = "console" # use console instead of in gui cli

	#load accesory packages
	# fix this to download te package if not available
	 packages<-apply(matrix(c("gWidgets","gWidgetsRGtk2","proto")),1,library, character.only=T)
	 
	 #accesory functions from R package "pmg" 
	 # avoid loading package due to non controllable GUI popup

	 #using an new environment instead of namespace for now (probably a very bad idea?)
	 #fxn to create the environment "devium" if it does not exist
	 create.devium.env<-function()
		{
			if(!exists("devium"))
					{
					if(!is.environment("devium")){ assign("devium",new.env(),envir= .GlobalEnv)}
					
					#check for devium objects and set to null if they don't exist
					for (i in c("devium.helpBrowser.window", "devium.plotnotebook.window", 
						"devium.main.window")) 
						{
						if(!exists(i))
							{
								assign(i, NULL, envir = devium)
							}
						}
					}
		}
	 create.devium.env()	
	 
	 # will need to change everything when namespaec is implemented... 
	 devium.closeALL<-function () 
		{
			for (i in c("devium.helpBrowser.window", "devium.plotnotebook.window", 
				"devium.main.window")) 
				{
				if(exists(i))
					{
						window = get(i,envir = devium)
						try(dispose(window), silent = TRUE)
						assign(i, NULL, envir = devium)
					}
				}
		}			

	#basic checks
	if (!interactive()) 
		{
			cat("Devium requires an interactive environment\n")
			return()
		}

	
	#check to see if the devium main window exists 
	#if not create one
	create.devium.main<-function()
		{
		tmp<-tryCatch(get("devium.main.window", envir=devium),erorr=function(e){NULL})
		if (is.null(tmp) || !is.gWindow(tmp) || is.invalid(tmp)) 
			{
				#assign("devium.main.window", gwindow("D E V I U M", visible = FALSE), "devium") # no class
				tmp<-gwindow("D E V I U M", visible = FALSE) #uses the proto class
				size(tmp) <- c(width, height)
				assign("devium.main.window", tmp, envir=devium)
			} else {
				return()
			}
		}
	create.devium.main()
	
	#Top menue
	#-------------------------
	devium.menu<-list()
	devium.menu$Data$`Import Data`$handler<-function(h,...){add(get("devium.notebook",envir=devium),devium.data.import(), label = "Import Data") }
	devium.menu$Data$`Load Data Set`$handler<-function(h,...){devium.load.Rdataset() }
	
	#plots
	devium.menu$Plots$`Scatter Plot`$handler<-function(h,...)
		{
			add(get("devium.notebook",envir=devium),devium.scatter.plot(), label = "Scatter Plot") 
			#add buttons to the top (save, plot) 
		}
		
	devium.menu$Plots$`Plot Builder`$handler<-function(h,...)
		{
			add(get("devium.notebook",envir=devium),devium.qplot(), label = "Plot Builder") 
		}	
	
	#devium.menu$Plots$`Plot Builder`$handler<-function(h,...) # unstable due to threading problems
	#	{
	#		#need to close devium first else R will crash ~ 5 min post call to
	#		# plot builder from Deducer due to "threadsafe" problem
	#		dispose(get("devium.main.window",envir=devium))
	#		out<-devium.plot.builder()# this GUI will focus on itself and untill released
	#		assign("plot.builder.call",out[-1],envir=devium)
	#		DEVIUM.GUI()
	#		obj<-get("devium.cli",envir=devium)
	#		svalue(obj) <- Paste(get("plot.builder.call",envir=devium))
	#	}
	
	#creation of GUI objects to go into devium.main.window
	#top menu
    assign("devium.menuBar", gmenu(devium.menu, container = NULL),envir= devium)
	#notebook  tab "Data"
    assign("devium.notebook", gnotebook(closebuttons = TRUE, dontCloseThese = 1, tearable = FALSE),envir= devium)
    assign("devium.statusBar", gstatusbar("", container = NULL),envir= devium)
    mainGroup = ggroup(horizontal = FALSE, spacing = 0, container = get("devium.main.window", envir=devium), expand = TRUE)
   
   #add objects to group
	add(mainGroup, get("devium.menuBar",envir=devium)) # very tedious to do this, no wonder namespace is preffereble
	
	#make menu then add
	help.menu<-list()
	help.menu$About$`DEVIUM`$handler<-function (h, ...)
			{
				add(get("devium.notebook",envir=devium), devium.about(), label = "About D E V I U M")
			}
	help.menu$About$`DEVIUM`$icon<-"about"

    helpMenu = gmenu(help.menu, name = "R Help")
    add(get("devium.menuBar",envir=devium), helpMenu)
	
	#make a button bar below menue
    buttonBar = ggroup(spacing = 0)
    add(mainGroup, buttonBar)
    bottomGroup = ggroup(horizontal = TRUE)
    add(mainGroup, bottomGroup, expand = TRUE)
    devium.droparea = ggroup(horizontal = FALSE, container = bottomGroup)
	
	#varbrowser with summary option
     assign("devium.varbrowser", gvarbrowser(handler = function(h, ...) 
		{
			tmp<-get("devium.varbrowser",envir=devium)
			value<- svalue(tmp)
			if(!is.null(tryCatch(gget(value),error=function(e){})))	{tmp<-gget(value);assign(get("value"),tmp)}
			tmp2<-get("devium.notebook",envir=devium)
			add(tmp2, devium.summary(value), label = Paste("Summary of ", value))
		}),envir= devium)
		
	#assign("devium.varbrowser", gvarbrowser(handler = function(h, ...) {
    #    value = svalue(devium.varbrowser)
    #   add(devium.notebook, devium.summary(value), label = Paste("Summary of ", 
    #        svalue(h$obj)))
    #}),envir= devium)
	
	# need to include or append calls to it to instead go to the R console 
    commandGroup = gexpandgroup("Command area",expand=FALSE)
    visible(commandGroup) <- TRUE
    rightPanedGroup = gpanedgroup(get("devium.notebook",envir=devium), commandGroup, horizontal = FALSE)
    pg = gpanedgroup(get("devium.varbrowser", envir=devium),  rightPanedGroup)
	
	#need to do else error...has to better way?
	tmp<-get("devium.notebook",envir=devium)
	size(tmp)<-c(rightWidth, mainHeight * 0.67)
    assign("devium.notebook",tmp,envir=devium)
	
    add(bottomGroup, pg, expand = TRUE)
    add(mainGroup, get("devium.statusBar",envir=devium))
	add(mainGroup,get("devium.notebook",envir=devium))
	
	#setting options second tool bar
    toolbar = list()
    toolbar$quit$icon = "quit"
	toolbar$quit$handler = function(h, ...) {
		tmp<-get("devium.main.window",envir=devium)
        dispose(tmp)
        devium.closeALL()
		}

	toolbar$save$icon = "save"
	toolbar$tmp1$separator = TRUE
	toolbar$save$handler = function(h, ...) {
		gfile("Save workspace", type = "save", action = "save.image")
		}
	
   
     toolbar$plotnotebook$icon = "plot"
	 toolbar$plotnotebook$handler = function(h, ...) {
		tmp<-tryCatch(get("devium.plotnotebook.window",envir=devium),error= function(e){NULL})
        if (is.null(tmp) || !is.gWindow(tmp) ||  is.invalid(tmp)) {
			tmp<-gwindow("D E V I U M plot notebook", visible = TRUE)  
			add(tmp, ggraphicsnotebook())
			 assign("devium.plotnotebook.window", tmp, envir = devium)
        }
        else {
            focus(get("devium.plotnotebook.window",envir=devium)) <- TRUE
        }
    }
	
    toolbar$tmp2$separator = TRUE
    toolbar$help$handler = function(h, ...) 
		{
		obj<-tryCatch(get("devium.helpBrowser.window",envir=devium),error= function(e){NULL})
        if (class(obj) == "NULL") 
			{
				assign("devium.helpBrowser.window", ghelpbrowser(), envir=devium)
			} else {
				tmp<-get("devium.helpBrowser.window",envir=devium)
				focus(tmp) <- TRUE
			}
		}
	
    toolbar$help$icon = "help"
    tmp = gtoolbar(toolbar)
    assign("devium.toolBar", tmp,envir=devium)
	add(buttonBar, tmp, expand = TRUE)
	
	#making side bar of small images with drag and drop
	tmp<-devium.droparea
	addSpace(tmp, 20)
    editDrop = gimage("edit", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    plotDrop = gimage("plot", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    summaryDrop = gimage("info", dirname = "stock", container = tmp)
    addSpace(tmp, 10)
    add(tmp, gseparator())
    addSpace(tmp, 10)
    removeDrop = gimage("delete", dirname = "stock", container = tmp)
    addSpring(tmp)
	assign("devium.droparea",tmp,envir = devium)
	
	#handlers for the small image sidebar
    adddroptarget(summaryDrop, handler = function(h, ...) {
	obj<-get("devium.cli",envir=devium)
        svalue(obj) <-Paste("summary(", list(h$dropdata),")")
    })
	
    adddroptarget(plotDrop, handler = function(h, ...) {
		obj<-get("devium.cli",envir=devium)
        svalue(obj) <- Paste("plot(", list(h$dropdata), ")")
    })
	
	#rpel is eval text strin in a given environment
    adddroptarget(editDrop, handler = function(h, ...) {
        rpel(Paste("fix(", list(h$dropdata), ")"))
    })
	
    adddroptarget(removeDrop, handler = function(h, ...) {
		obj<-get("devium.cli",envir=devium)
        svalue(obj) <- Paste("rm(", list(h$dropdata), ")")
    })
	
	#to get a console in the GUI
    assign("devium.cli", gcommandline("", width = rightWidth, height = mainHeight * 0.33, useConsole = TRUE), envir= devium)
    add(commandGroup, get("devium.cli",envir=devium)) #, expand = TRUE
	svalue(rightPanedGroup)<-1 # open closed console
	#data frame viewer
    x = as.numeric(NA)
    df = data.frame(X1 = x)
    assign("devium.data.frame.viewer.nb",gdfnotebook(tab.pos = 1, dontCloseThese = 1),envir= devium)
	
	#initialize notebook object 
    add(get("devium.notebook",envir=devium), get("devium.data.frame.viewer.nb",envir=devium), label = "Data", pageno = 2, override.closebutton = TRUE)
		
	# add about info as notebook pane	
	devium.about<-function (container = NULL) 
		{
			group = ggroup(horizontal = FALSE, container = container)
			size(group) <- c(500, 500)
			theFactsMam <-list() 
			theFactsMam$Author<-"Dmitry Grapov"
			theFactsMam$Version<-"0.01"
			theFactsMam$Title<-"Play with your data!"
			theFactsMam$URL<-"www.devium.com"
			theFactsMam$Description<-"A GUI "
			theFactsMam$License<-"GPL (>= 2)"
			theFactsMam<-do.call("cbind",theFactsMam)
			glabel(Paste("<b>D E V I U M - </b>\n","<b>D</b>ynamic multivariat<b>E</b> data analysis and <b>VI</b>s<b>U</b>alization platfor<b>M</b>\n", "<i>", theFactsMam[1, "Title"], 
				"</i>\n", "Version ", theFactsMam[1, "Version"], "\n\n", 
				theFactsMam[1, "URL"], "\n", "Comments to devium.software@gmail.com\n", 
				"\n\n", theFactsMam[1, "Author"], "\n\n", theFactsMam[1, 
					"Description"], "\n"), markup = TRUE, container = group)
			addSpring(group, 10)
			return(group)
		}
	
    add(get("devium.notebook",envir=devium),devium.about(), label = "About D E V I U M")
	
	#show form
	tmp<-get("devium.main.window",envir=devium)
    visible(tmp) <- TRUE
	#close console sash
	svalue(rightPanedGroup)<-1
}
