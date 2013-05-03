
# functions
#-------------------------------------------------------------
#create a network from a graphNeL object
make.cynet<-function(graph,network.name,layout='jgraph-spring'){
		check.get.packages("RCytoscape")
		
		#check connection 
		con<-tryCatch(CytoscapeConnection (),error=function(e){NULL})
		if(!class(con)=="CytoscapeConnectionClass")
			{
				return(cat("No connection to Cytoscape \n"))
			} else {
				
				#check if cytoscape is open and a connection can be made
				cw <- tryCatch(new.CytoscapeWindow (network.name, graph=graph,overwriteWindow=TRUE),
					error=function(e){"couldn't connect to host"})
				if(class(cw)=="character")
					{
						return(cw)
					} else {	
						displayGraph (cw)
						layoutNetwork(cw, layout.name=layout)
						redraw(cw)
						cw
					}
			}
	}

#set characteristics of edges in an existing cytoscape network
set.edge.attribute<-function(network,edge.names,edge.attribute,edge.attribute.value){
	
		var<-edge.attribute.value
		
		#need to be set one at a time, one level at a time...
		switch(edge.attribute,
			
				color 	= 	sapply(1:nlevels(as.factor(var)),function(i)
							{
								id<-c(1:length(var))[var==levels(as.factor(var))[i]]
								setEdgeColorDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
							}),
						
				opacity = 	sapply(1:nlevels(as.factor(var)),function(i)
								{
									id<-c(1:length(var))[var==levels(as.factor(var))[i]]
									setEdgeOpacityDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
								}),
						
				width 	= 	sapply(1:nlevels(as.factor(var)),function(i)
								{
									id<-c(1:length(var))[var==levels(as.factor(var))[i]]
									setEdgeLineWidthDirect(network,edge.names[id],as.character(levels(as.factor(var))[i]))
								})
				)		
		redraw (network)
	}

#set characteristics of nodes in existing cytoscape network
set.node.attribute<-function(network,node.names,node.attribute,node.attribute.value){
		#check node names against what is in the network 
		var<-node.attribute.value
		graph.names<-getAllNodes(network)
		union<-seq(along=node.names)[node.names%in%graph.names]
		
		#append what will be set based on the union
		node.names<-node.names[union]
		var<-node.attribute.value[union]
		
		#need to be set one at a time, one level at a time...
		switch(node.attribute,
			
					color 		= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeColorDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
					size		= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeSizeDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),				
						
					opacity 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
								
					border 		=	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
								
					shape 		=	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeShapeDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.width 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderWidthDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.color 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderColorDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									}),
									
				border.opacity 	= 	sapply(1:nlevels(as.factor(var)),function(i)
									{
										id<-c(1:length(var))[var==levels(as.factor(var))[i]]
										setNodeBorderOpacityDirect(network,node.names[id],as.character(levels(as.factor(var))[i]))
									})					
				)		
		redraw (network)
	}

#dynamically link graph node and edge selections	
link.cyto.graph.nodes.select<-function(graphs,visual.style.names=NULL){
		#graphs should be a character string of graph names
		#with out knowing the active graph scan all for active nodes
		selected<-unique(as.character(na.omit(do.call("rbind",lapply(1:length(graphs),function(i)
			{
				assign("tmp",get(graphs[i]))
				getSelectedNodes(tmp)
			})))))
		
		if(!length(selected)==0)
			{	
			#select in all graphs use 
			#for some reason this looses the visual style of the graph in the procces
			sapply(1:(length(graphs)),function(i)
				{
					
					
					assign("tmp",get(graphs[i]))
					selectNodes(tmp,selected)
					#reset style
					if(!class(visual.style.names)=="NULL")
						{
							setVisualStyle (get(graphs[i]), visual.style.names[i]) 
						}
				})
			}
	}

#save cytoscape network styles 
save.cyto.graph.styles<-function(network,prefix=NA){
		sapply(1:length(network),function(i)
		{
			if(is.na(prefix))name<-paste("style-",network[i],sep="") else name<-paste(prefix,network[i],sep="") 
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#delete styles (not validated)	
delete.cyto.graph.styles<-function(network){
		sapply(1:length(network),function(i)
		{
			name<-paste(network[i],"-style",sep="")
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#create a node legend network
cyto.node.legend<-function(network="new",node.attribute,node.attribute.value,node.names,legend.title="node legend",unique.matched=FALSE){
		if(!class(network)=="CytoscapeWindowClass")	
			{
				#get unique properties
				if(unique.matched==FALSE)
					{
						id<-unique.id(node.attribute.value)
						node.attribute.value=node.attribute.value[id]
						node.names=node.names[id]
					}
					
				#create new legend
				tmp<- new("graphNEL", edgemode = "undirected")
				i<-1
				for(i in 1:length(node.names))
					{	
						tmp<-graph::addNode(node.names[i], tmp)
					}
				#draw nodes			
				cw <- CytoscapeWindow(legend.title, graph = tmp)
				displayGraph(cw)	
				network<-cw
			}
			
			if(class(network)=="CytoscapeWindowClass")
			{
				#get unique properties
				if(unique.matched==FALSE)
					{
						id<-unique.id(node.attribute.value)
						node.attribute.value=node.attribute.value[id]
						node.names=node.names[id]
					}
					
				#check to see if node exists else create new
				nodes<-getAllNodes (network)
				new.nodes<-node.names[!node.names%in%nodes]
				
				#create new nodes
				if(!length(new.nodes)==0)
				{
					i<-1
					for(i in 1:length(new.nodes))
						{	
							addCyNode(network, new.nodes[i])
						}
				}
			}
			
		#annotate node properties
				set.node.attribute(network=network,node.names=node.names,
						node.attribute=node.attribute,
						node.attribute.value=node.attribute.value)	
		
		#layoutNetwork(network, 'grid')
		redraw(network)
		return(network)
					
	}
	
#format cytoscape edge names to edge list
convert.to.edge.list<-function(graph){	
		return(do.call("rbind",strsplit(as.character(names(cy2.edge.names (graph@graph) )),"~")))
	}

#convert  qpgraph output to edge list output
mat.to.edge.list<-function(input,graph){
		#parse qpgraph object into pieces
		# generalize later if necessary
		p.cor.r<-input$R
		p.cor.p<-input$P
		
		#initialize objects
		idn<-rownames(p.cor.r)
		ids<-seq(along=idn)
		
		#common network edges names 
		edge.names<-convert.to.edge.list(graph)
		edge.ids<-do.call("rbind",lapply(1:nrow(edge.names),function(i)
				{
					data.frame(columns=ids[idn%in%edge.names[i,1]],
					rows=ids[idn%in%edge.names[i,2]])
				}))
				
		#Extract edge correlation and p-value
	
		edge.cor<-do.call("rbind",lapply(1:nrow(edge.ids),function(i)
			{
				tmp<-data.frame(correlation=p.cor.r[edge.ids[i,1],edge.ids[i,2]],
				p.value=p.cor.p[edge.ids[i,1],edge.ids[i,2]])
				rownames(tmp)<-cy.edge.names[i]
				tmp
			}))
			
		#sort rows to later match cy2.edge.names sort
		edge.cor<-cbind(edge.cor,edge.ids)
		edge.cor<-edge.cor[order(rownames(edge.cor),decreasing=T),]
		
		return(edge.cor)
	}

#generic convert symmetric matrix to edge list (use the upper triangle) (shoud make a class)
gen.mat.to.edge.list<-function(mat){
		
		#accessory function
		all.pairs<-function(r,type="one")
                {       
                        switch(type,
                        one = list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))], second = rep(1:r, r)[lower.tri(diag(r))]),
                        two = list(first = rep(1:r, r)[lower.tri(diag(r))], second = rep(1:r,rep(r,r))[lower.tri(diag(r))]))
                }
		
		ids<-all.pairs(ncol(mat))
		
		tmp<-as.data.frame(do.call("rbind",lapply(1:length(ids$first),function(i)
			{
				value<-mat[ids$first[i],ids$second[i]]
				name<-c(colnames(mat)[ids$first[i]],colnames(mat)[ids$second[i]])
				c(name,value)
			})))
		colnames(tmp)<-c("source","target","value")	
		return(tmp)
	}

#trim edge list based on some reference index 
edge.list.trim<-function(edge.list,index,cut,less.than=FALSE){
			if(less.than==TRUE){
					edge.list[index<=cut,,drop=FALSE]
				}else{
					edge.list[index>=cut,,drop=FALSE]
					}
		}

#----Identify differences in measures between two classes given some 
# threshhold (i.e. p-value) and filter out put based on pcor, combined network
# inputs structure as output from qpgraph
compare.2class.network<-function(network,class1.obj,class2.obj, threshold=0.05){
		#Extract edge correlation and p-value
		#class 1
		edge.cor1<-mat.to.edge.list(class1.obj,network)
			
		#class 2
		edge.cor2<-mat.to.edge.list(class2.obj,network)
			
		#encode id based on cor p <= cut.off
		p.cut.off<-threshold

		#pre sig extraction object	use to hide
		sig.id1<-edge.cor1[,2]<=p.cut.off
		sig.id2<-edge.cor2[,2]<=p.cut.off
		
		#---------------------FINAL ids
		#these can be used to mask redundant information
		#edges common to both classes
		tmp<-seq(1:nrow(edge.cor1))
		not.sig<-!sig.id1==FALSE&sig.id1==sig.id2

		#unique class 1 edges
		class1.sig<-!sig.id1==FALSE&!sig.id1==not.sig

		#unique class 2 edges
		class2.sig<-!sig.id2==FALSE&!sig.id2==not.sig
		
		#output results
		return(list(common.edges=not.sig,data.frame(edge.cor1,meets.threshold=class1.sig),data.frame(edge.cor2,meets.threshold=class2.sig)))
	}

#subset data and compare common and unique connections based on qpnetworks	
qpgraph.compare<-function(data,factor,threshold="auto",...){
		#Use factor to split data by rows  and calculate
		#qpnetworks
		#if threshhold ="auto" then it is estimated where all nodes are connected
		#otherwise could be numeric or "interactive"
		
		out.lists<-lapply(1:nlevels(factor),function(i)
			{
				#data subset
				tmp.data<-data[factor==levels(factor)[i],]
				
				cat("Calculating for:",levels(factor)[i],"\n")
				#qpnetwork
				full.net<-make.ave.qpgraph(tmp.data)#,...
				#overview nodes vs edges to choose threshhold
				if(all(is.numeric(threshold)))
					{
							tmp.threshold<-threshold
					}else{
							tmp.threshold<-choose.qpgraph.threshold(full.net,choose=threshold) [[1]]
						}
						
				#edgelist based on qpgraph
				qpnet <-as.matrix(qpGraph(full.net, threshold=tmp.threshold, return.type="edge.list"))
				out<-list(qpnet)
				names(out)<-paste(levels(factor)[i])
				out
			})
			
			#for edge lists
			unique.edge<-lapply(1:length(out.lists),function(i)
						{
							tmp<-as.matrix(as.data.frame(out.lists[[i]]))
							enames<-paste(tmp[,1],tmp[,2],sep="__")
							search.id<-c(1:length(out.lists))[!c(1:length(out.lists))==i]
							is.common<-sapply(1:length(search.id),function(j)
								{
									tmp2<-as.matrix(as.data.frame(out.lists[[search.id[j]]]))
									enames2<-paste(tmp2[,1],tmp2[,2],sep="__")
									enames%in%enames2
								})					
							obj<-list(data.frame(from=tmp[,1],to=tmp[,2],is.common=is.common))
							names(obj)<-names(out.lists[[i]])
							obj
						})
						
			#get common and unique edges (improve this long winded version later)	
			tmp<-as.data.frame(unique.edge[1])
			common<-tmp[tmp[,3],1:2]		
			unique.output<-lapply(1:length(unique.edge),function(i)
				{
					tmp<-as.data.frame(unique.edge[i])
					tmp[!tmp[,3],1:2]
				})
			
			list(common=common,unique.output)
			}
		
#generate qpgraph
make.ave.qpgraph<-function(data,tests=200,for.col=TRUE,...){
		check.get.packages("qpgraph")
		.local<-function(data,tests=200,for.col=TRUE,...)
		{
			m<-data # some bug in qpgraph
			average.net<-qpAvgNrr(data,nTests=tests,...) 
			return(average.net)
		}
		if(dim(data)[2]<=dim(data)[1]&for.col==TRUE)long.dim.are.variables<-FALSE else long.dim.are.variables<-TRUE
		.local(data,long.dim.are.variables=long.dim.are.variables,tests=tests)
	}
	
#plot graph edge/vertex number vs threshhold
choose.qpgraph.threshold<-function(qpnetwork,.threshold=c(0,.6),choose=NULL){
		
		#choose = c("interactive", "auto")
		
		#choose elbow in vertex vs. edge plot
		int<-c(seq(.threshold[1],.threshold[2],by=.01)[-1])
		
		xy<-do.call("rbind",lapply(1:length(int),function(i)
				{
					net <- qpGraph(qpnetwork, threshold=int[i], return.type="graphNEL")
					con.nodes<- unique(unlist(strsplit(names(net@edgeData@data),"\\|")))
					data.frame(threshold=int[i],nodes=length(con.nodes),edges=length(unlist(net@edgeL))/2)
				}))
		#plot
		plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
		lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
		legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
		#show threshold wher all nodes are connected
		tmp<-which.min(abs(nrow(qpnetwork)-xy[,2]))
		abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
		abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
		abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
		title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
		if(choose=="auto") 
			{
				return(list(auto.threshold=xy[tmp,1],list=xy))
			}
			
		if(choose=="interactive")
			{
				tmp<-locator(1)$y
				tmp<-which.min(abs(xy[,1]-tmp))
				plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				#show threshold wher all nodes are connected
				abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
				return(xy[tmp,1])
			}
	}

#partial correlation coefficients and p-values
#some times this can not be calculated due too few observations
#do instead via qpgraph
partial.correl<- function(data, qpnet, verbose=T, for.col=TRUE,...){
		.local<-function(data, qpnet, verbose=verbose,long.dim.are.variables,...)
			{
				qpPAC(data, g=qpnet, verbose=verbose,long.dim.are.variables=long.dim.are.variables,...) 
			}
		if(dim(data)[2]<=dim(data)[1]&for.col==TRUE)long.dim.are.variables<-FALSE else long.dim.are.variables<-TRUE
		.local(data, qpnet=qpnet,verbose=verbose, long.dim.are.variables=long.dim.are.variables,...)
	}

#make graphNEL object from edge list
edge.list.to.graphNEL<-function(edge.list){
		check.get.packages("graph")
		#one way
		edge.l<-split(as.character(edge.list[,2]),as.factor(as.character(edge.list[,1])))
		#reciprocal
		edge.l.r<-split(as.character(edge.list[,1]),as.factor(as.character(edge.list[,2])))
		#bind and break one more tiome to merge duplicated edges
		edge.l.final<-split(c(edge.l,edge.l.r),as.factor(names(c(edge.l,edge.l.r))))
		
		#format to numeric index for names
		tmp.names<-names(edge.l.final)
		out<-sapply(1:length(edge.l.final),function(i)
			{
				tmp<-edge.l.final[[i]]
				if(length(tmp)>1)
					{
						name<-unique(names(tmp))
						tmp<-list(as.character(unlist(tmp)))
						names(tmp)<-name
					}
				index<-tmp.names[tmp.names%in%as.character(unlist(tmp))]
				obj<-list(edges=index)
				names(obj)<-names(tmp)
				obj
			})
		
		obj<-new("graphNEL", nodes=names(out), edgeL=out,edgemode="undirected")
		return(obj)
	}
	
#extract values from a square symmetric matrix based on an edge list
sym.mat.to.edge.list<-function(mat,edge.list){
		# extract position of objects from a
		# square symmetric matrix based on its dimnames
		# according to an edge list
		
		#initialize objects
		idn<-rownames(mat)
		ids<-seq(along=idn)
		
		#common network edges names 
		edge.names<-edge.list
		edge.ids<-do.call("rbind",lapply(1:nrow(edge.names),function(i)
				{
					data.frame(columns=ids[idn%in%edge.names[i,1]],
					rows=ids[idn%in%edge.names[i,2]])
				}))
				
		#Extract value from mat based on index
		edge.val<-do.call("rbind",lapply(1:nrow(edge.ids),function(i)
			{
				tmp<-data.frame(value=mat[edge.ids[i,1],edge.ids[i,2]])
				rownames(tmp)<-paste(edge.names[i,1],edge.names[i,2],sep="~")
				tmp
			}))
		return(edge.val)
	}

#sort edge list/attributes to match order of cytoscape network
#account for reciprical edge naming 
match.cynet.edge.order<-function(obj,cynet){
		cy.order<-names(cy2.edge.names (cynet@graph))
		my.order<-rownames(obj)
		
		#split object, look for non matches and flip this assignment 
		tmp.cy<-do.call("rbind",strsplit(cy.order,"~"))
		tmp.my<-do.call("rbind",strsplit(my.order,"~"))
		
		#identify where there are no matches
		flip<-!my.order%in%cy.order
		
		#flip assignment in tmp.my
		tmp<-tmp.my[flip,1]
		tmp.my[flip,1]<-tmp.my[flip,2]
		tmp.my[flip,2]<-tmp
		
		#now bind and make sure are identical
		my.order<-paste(tmp.my[,1],tmp.my[,2],sep="~")
		if(!identical(sort(my.order),sort(cy.order))){cat("Edges do not match:","\n",my.order[!my.order%in%cy.order]);stop()}
		
		#get index for my.oder to match cy.order
		ord1<-seq(along=cy.order)[order(cy.order)]
		ord2<-seq(along=my.order)[order(my.order)]
		final<-ord2[ord1]
		return(final)
	}	

#filter edges based on some weight or binary index
filter.edges<-function(edge.list,filter,cut.off=NULL){
		#check to see if each side of the edge (to or from) meets requirement
		#if cut.off = NULL 
		#filter must be a two column matrix with node names and logical statement if they should kept
		#else the filter can be a square symmetric matrix with nodes as dimnames whose values will be used 
		#to select edges to keep if the value is <= cutoff
		#returns an index of edges matching criteria
		if(class(cut.off)=="NULL")
			{
				out<-sapply(1:nrow(edge.list),function(i)
					{
						any(as.character(unlist(edge.list[i,]))%in%as.character(filter[filter[,2]==TRUE,1]))
					})
			}
			
		if(class(cut.off)=="numeric")
			{
				#get weights for edges
				tmp<-sym.mat.to.edge.list(filter,edge.list)
				out<-sapply(1:nrow(edge.list),function(i)
					{
						tmp[i,]<=cut.off
					})
			}
		return(out)
	}

#create edge list and network attributes file from meta.data with CID keys
#data<- bound with CIDS
#remove duplicates from object based on and index/identifier 
unique.obj<-function(data, index){
		#accesory function to return position of first instance of unique object 
		id<-unique.id(index)
		data[id,]
	}

#look up KEGG reactant pairs 
get.KEGG.pairs<-function(url="https://gist.github.com/dgrapov/5438735/raw/c4cc531a8b2a1570b8b3aa52e723d04caf09e987/KEGG+RPAIRS2.r"){ 
		#older repo: "https://gist.github.com/dgrapov/4964564/raw/aec1a5097a3265d22109c9b34edd99a28f4012a3/KEGG+reaction+pairs"
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		tmp2[[1]]<-strsplit(tmp2[[1]],"\\  ")
		matrix(unlist(tmp2),ncol=2, byrow=TRUE)
	}

#look up CID to KEGG translation 	
get.CID.KEGG.pairs<-function(url="https://gist.github.com/dgrapov/4964546/raw/c84f8f209f961b23adbf7d7bd1f704ce7a1166ed/CID_KEGG+pairs"){
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		matrix(unlist(tmp2),ncol=2, byrow=TRUE)
	}

#making an edge list based on CIDs from KEGG reactant pairs
CID.to.KEGG.pairs<-function(cid,database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs()){
		
		matched<-lookup[c(1:nrow(lookup))[fixln(lookup[,1])%in%cid],]
		ids<-sapply(1:nrow(matched),function(i)
			{
				#
				c(which(as.character(matched[i,2])==as.character(database[,1])),which(as.character(matched[i,2])==as.character(database[,2])))				
			})
		names(ids)<-fixln(matched[,1])	# cid of all paired by cid
		
		#construct symmetric matrix then extract unique edge list
		mat<-do.call("rbind",lapply(1:length(ids),function(i)
			{
				obj<-ids[[i]]
				match<-sapply(1:length(ids), function(j)
					{
						tmp<-ids[[j]]
						sum(tmp%in%obj)
					})
			}))
		dimnames(mat)<-list(names(ids),names(ids))
		elist<-gen.mat.to.edge.list(mat)
		as.data.frame(elist[fixln(elist[,3])>0,1:2])	#cid source to cid target based on kegg pairs	
	}

#calculate correlations and p-values
devium.calculate.correlations<-function(data,type="pearson"){
		check.get.packages(c("impute","WGCNA","Hmisc"))
		#data will be coerced to a matrix
		# type includes pearson (WGCA), biweight(WGCA), spearman
		switch(type,
			pearson 	= .local<-function(data){
							obj<-corAndPvalue(as.matrix(data), use = "pairwise.complete.obs", alternative = "two.sided")
							list(cor=obj$cor,p.value=obj$p)
							},		
			biweight 	= .local<-function(data){
							obj<-bicorAndPvalue(as.matrix(data),use = "pairwise.complete.obs", alternative = "two.sided")
							list(cor=obj$bicor,p.value=obj$p)
							},
			spearman    = .local<-function(data){
							obj<-rcorr(as.matrix(data),type="spearman")
							list(cor=obj$r,p.value=obj$P)
							})
			.local(data)				
	}

#get tanimoto distances from cids
CID.to.tanimoto<-function(cids, cut.off = .7, parallel=FALSE, return="edge list"){
	#used cids = PUBCHEM CIDS to calculate tanimoto distances
	need<-c("snow","doSNOW","foreach","ChemmineR") # need to use others for mac
	for(i in 1:length(need)){check.get.packages(need[i])}
	#get fingerprint for calcs
	data(pubchemFPencoding)
	cid.objects<-unique(as.numeric(as.character(unlist(cids)))) # need
	
	#print to screen any duplictes which get removed 
	
	if(sum(duplicated(as.numeric(as.character(unlist(cids)))))>0){
		cat(paste("Duplicates of", paste(as.character(unlist(cids))[duplicated(as.numeric(as.character(unlist(cids))))]), "were removed" ),"\n")
		}
	cat("Using PubChem Power User Gateway (PUG) to get molecular fingerprint. This may take a moment.","\n")
	compounds <- getIds(cid.objects) # get sdfset
	cid(compounds) <- sdfid(compounds)
	
	# Convert base 64 encoded fingerprints to character vector, matrix or FPset object
	fpset <- fp2bit(compounds, type=2)
	
	if(parallel==TRUE)
		{
				#change this later
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				out<-foreach(j=c(1:length(rownames(fpset))),.combine="cbind") %dopar% ChemmineR::fpSim(fpset[j,], fpset)#length(codes)
				stopCluster(cl.tmp)	
		} else {
					out<-sapply(rownames(fpset), function(x) ChemmineR::fpSim(x=fpset[x,], fpset,sorted=FALSE)) 
		}
		
	#edgelist
	#colnames(out)<-unlist(dimnames(out)[1])1
	
	#optionally filter based on score based on score
	obj<-as.matrix(out)
	
	if(return=="edge list"){
		e.list<-gen.mat.to.edge.list(obj)
		final<-edge.list.trim(e.list,index=fixln(e.list[,3]),cut=cut.off,less.than=FALSE)
	}else{
		obj[obj<cut.off]<-0
		final<-obj
	}
	return(final)
}

#querry chemical translation service (CTS) to get tanimoto from inchis (very slow)
CID.to.tanimoto.CTS<-function(cid,lookup=get.CID.INCHIcode.pairs()){
		check.get.packages("XML")
		matched<-lookup[c(1:nrow(lookup))[lookup[,1]%in%cid],]
		matched<-matched[!matched[,2]=="",]
		codes<-gsub("=", "%3D", matched[,2])
		#use webservice to get tanimoto score between cids based in inchi key
		#do in parallel
		check.get.packages(c("snow","doSNOW","foreach"))

		i<-1
		out<-list()
		for(i in 1:length(codes))
			{
				
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				
				#fxn accesing web  can't be parallel?
				.local<-function(i,j,codes)
					{
						url=paste("http://vulcan.fiehnlab.ucdavis.edu:8080/tanimoto-service-1.2/rest/xml/calc.xml?from=",codes[i],"&to=",codes[j],sep="")
						
						text<-tryCatch(XML::xmlTreeParse(url),error=function(e){NULL})
						if(is.null(text))
							{
								return()
							}else{
								as.numeric(strsplit(unlist(text$doc$children$result[[3]]),">")[3])
							}
					}
					
			out[[i]]<-foreach(j=c(1:length(codes)),.combine="c") %dopar% .local(i,j,codes=codes)#length(codes)
			stopCluster(cl.tmp)	
			}
			
		names(ids)<-matched[,1]	# cid of all paired by cid
		
		#construct symmetric matrix then extract unique edge list
		mat<-do.call("rbind",lapply(1:length(ids),function(i)
			{
				obj<-ids[[i]]
				match<-sapply(1:length(ids), function(j)
					{
						tmp<-ids[[j]]
						sum(tmp%in%obj)
					})
			}))
		dimnames(mat)<-list(names(ids),names(ids))
		elist<-gen.mat.to.edge.list(mat)
		as.data.frame(elist[elist[,3]==1,1:2])	#cid source to cid target based on kegg pairs	
	}

#functions for devium network GUI to calculate edge list
devium.network.execute<-function(object,filter=as.numeric(object$devium.network.edge.list.weight.cutoff),FDR=as.logical(object$devium.network.edge.list.weight.cutoff.use.FDR)){
		check.get.packages(c("WGCNA","Hmisc"))
		#stored in get("devium.network.object",envir=devium)
		#switch for data  of CIDs
		if(object$devium.network.target.type=="Data"){
					main<-tryCatch(get(object$devium.network.target.object),error=function(e){NULL})
			} else{
					main<-tryCatch(get(object$devium.network.target.object),error=function(e){NULL})
					#check if it is inside a data frame and try to get it
					if(is.null(main)) {main<-tryCatch(as.matrix(as.numeric(as.character(unlist(gget(object$devium.network.target.object))))),error=function(e){NULL})}
			}
			
			if(is.null(main))
				{ 
						return()
				} else {
				
					#routing functions
					edge.list.type<-object$devium.network.edge.list.type
					tmp.data<-main[sapply(1:ncol(main), function(i) {class(main[,i])=="numeric"})]#cut out factors if present need for data
					switch(edge.list.type,
					"spearman correlations" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="spearman") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
													
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
																				
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},
					"pearson correlations" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="pearson") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
					
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},
					"biweight mid-correlation" = .local<-function()
												{
													
													cor.mat<-devium.calculate.correlations(tmp.data,type="biweight") 

													#make edge list from a square symmetric matrix	
													edge.list<-gen.mat.to.edge.list(cor.mat$cor)
													
													#FDR correct p-values
													if(FDR==TRUE){
															tmp<-cor.mat$p.value
															tmp[is.na(tmp)]<-1
															out<-FDR.adjust(tmp,type="pvalue",return.all=FALSE)
															results<-matrix(out,nrow=nrow(tmp),ncol=ncol(tmp),byrow=TRUE)
															dimnames(results)<-dimnames(tmp)
															cor.mat$p.value<-results
															#return to square symmetric matrix
														}
														
													#add options for filter
													weight.list<-gen.mat.to.edge.list(cor.mat$p.value)
													
													filtered.list<-edge.list[as.numeric(as.character(unlist(weight.list[,3])))<=filter,]
													
													#return edge list and value
													list(full.edge.list=edge.list,weights = data.frame(weight.list[,3],drop=FALSE), filtered.edge.list = filtered.list )
												},							
					"KEGG reaction pairs" 	= .local<-function()
												{
													#return edge list
													obj<-CID.to.KEGG.pairs(as.matrix(main),database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs())
													out<-data.frame(cbind(obj,1))
													list(full.edge.list=out,weights = data.frame(cbind(obj[,1:2],1)[,3],drop=FALSE), filtered.edge.list = obj )
												},
												
					"Tanimoto distances"	= .local<-function()
												{
													#return edge list
													obj<-CID.to.tanimoto(as.matrix(main), cut.off = filter, parallel=TRUE)
													filtered<-obj[as.numeric(as.character(unlist(obj[,3])))>=filter,]
													list(full.edge.list=obj,weights = data.frame(obj[,3],drop=FALSE), filtered.edge.list = filtered )
												}		
							)
					
				elist<-.local()	
				d.assign("devium.network.edge.list.full",elist$full.edge.list,main.object="devium.network.object")
				d.assign("devium.network.edge.list.weights",elist$weights,main.object="devium.network.object")
				d.assign("devium.network.edge.list.calculated",elist$filtered.edge.list,main.object="devium.network.object")
				
				#may want to to also assign to global	
				assign(paste(object$devium.network.target.object,"network.edge.list", sep="."),elist$filtered.edge.list, envir=globalenv())
				}
	}

#function to add to existing igraph.plot
devium.igraph.plot<-function(edge.list,graph.par.obj=NULL,plot.type="static",add=FALSE,not.dev=FALSE){
		#plot.type = c("static","interactive","3D-plot")
		check.get.packages(c("igraph","graph")) 
		#create grapNEL object from edge list
		graph.obj<-edge.list.to.graphNEL(edge.list)
		# could calculate tis directly but for now going through NEL because it is also used for Cytoscape graphs
		igraph.obj<-igraph.from.graphNEL(graph.obj, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
	
		
		#default options for igraph.plot
		defaults<-list(
		x = igraph.obj,
		mark.groups =  NULL,
		layout = "layout.fruchterman.reingold",  #have to get this later
		vertex.label = unclass(igraph.obj)[[9]][[3]]$name, #this it the graph object later 
		vertex.color ="gray",
		vertex.size = 6,
		vertex.label.dist=-.3)
		
		
		#join defaults with graph.par.obj
		graph.par<-defaults
		i<-1
		for(i in 1:length(defaults))
			{
				j<-1
				for(j in 1:length(names(graph.par.obj)))
					{
						if(!is.null(graph.par.obj)){
							if(as.character(names(defaults)[i])==as.character(names(graph.par.obj)[j]))
								{
										graph.par[[i]]<- graph.par.obj[[j]] #tryCatch(get(unlist(graph.par.obj[j])),error=function(e){graph.par.obj[j]})
								}
						}								#else { 
									# tmp<-graph.par[i]
									# graph.par[i]<-tmp
							# }
					}
			}
		
		#translate names to use call for tkplot and rgl plot
		#switch names to generate calls for tkplot 
		switch(plot.type,
		"interactive" 	= names(defaults)[c(1,2)]<-c("graph","..."),
		"3D-plot" 		= names(defaults)[c(2)]<-"...")
		
		names(graph.par)<-names(defaults)
		
		#calculate layout to be shared by all plots
		graph.par[names(graph.par)=="layout"][[1]]<-as.matrix(do.call(unlist(graph.par[names(graph.par)=="layout"]),list(graph.par[[1]])))
		
		#try to get objects ... if error stay with default ( was in the loop above, but need mechanism to not get for layout)
		
		
		#add third dimension if using 2D layout for 3D-plot
		if(plot.type=="3D-plot" & ncol(as.data.frame(graph.par[names(graph.par)=="layout"]))<3)
			{
				graph.par[names(graph.par)=="layout"][[1]]<-as.matrix(cbind(as.data.frame(graph.par[names(graph.par)=="layout"]),0))
			}
	
		#call plot
		switch(plot.type,
				static 		= do.call("plot",graph.par),
				interactive = do.call("tkplot",graph.par),
				"3D-plot" 	= do.call("rglplot",graph.par))
		
	}

#calculating qvalue and local FDR
FDR.adjust<-function(obj,type="pvalue",return.all=FALSE){
	check.get.packages("fdrtool")
	#adjust p-values for multiple hypothese tested
	#options for FDR for tests c("normal", "correlation", "pvalue", "studentt")\
	#methods for FDR c("fndr", "pct0", "locfdr")
	obj<-as.numeric(as.character(unlist(obj))) # just to be sure it is numeric
	obj<-fdrtool(obj, statistic=type,plot=FALSE, color.figure=FALSE, verbose=TRUE,cutoff.method="fndr",pct0=0.75)
	if(return.all==TRUE){return(obj)} else {return(as.numeric(as.character(unlist(obj$qval))))}
	}
	
#use chemical resolver to get inchi keys from smiles
get.inchikey.from.smiles<-function(smiles,progress=TRUE){
		# smiles are coerced to a 1 column data frame
		obj<-data.frame(matrix(unlist(smiles),ncol=1))
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)} # need RCurl for web querry
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = nrow(obj), style = 3)} # show progress bar
	
		start<-"http://cactus.nci.nih.gov/chemical/structure/"
		end<-"/stdinchikey"
		out<-sapply(1:nrow(obj),function(i)
			{
				if (progress == TRUE){setTxtProgressBar(pb, i)}
			
				close(pb)
				url<-paste(start,as.character(unlist(obj[i,])),end,sep="")
				url<-gsub("\\ ","%20",url) # fix spaces 
				tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){"error"})
					
			})
			
			if (progress == TRUE){close(pb)}
			
		#format output to only return InchI
		bad<-is.na(smiles)
		out<-as.character(unlist(out))
		out[bad]<-"InChIKey=error"
		#results<-matrix(as.character(unlist(as.data.frame(strsplit(out,"="))[2,])),ncol=1)
		results<-matrix(out,ncol=1)
		colnames(results)<-"InchI Key"
		return(results)
		}

#plot nodes vs edges for edge list and at some given threshold based on an index
plot.nodes.and.edges<-function(edge.list,index=NULL,.threshold=c(0,0.05), levels=10, plot="seperate"){
		
		#choose = c("interactive", "auto")
		#plot = c("seperate","ratio")
		#choose elbow in vertex vs. edge plot
		int<-c(seq(.threshold[1],.threshold[2],length.out=levels)[-1])
		
		xy<-do.call("rbind",lapply(1:length(int),function(i)
				{
					net <- edge.list[index<=int[i],]
					nodes<- unique(as.character(unlist(net)))
					data.frame(threshold=int[i],nodes=length(nodes),edges=nrow(net))
				}))
		#plot
		if(plot=="ratio"){ 
				plot(xy[,2]/xy[,3],xy[,1],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="nodes / edges")
			} else {
				plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				#show threshold wher all nodes are connected
				tmp<-which.min(abs(nrow(edge.list)-xy[,2]))
				abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
		}
		# if(choose=="auto") 
			# {
				# return(list(auto.threshold=xy[tmp,1],list=xy))
			# }
			
		# if(choose=="interactive")
			# {
				# tmp<-locator(1)$y
				# tmp<-which.min(abs(xy[,1]-tmp))
				# plot(xy[,c(3,1)],type="l",lwd=2,col="orange",pch=21,bg="red",cex=1,xlab="number")
				# lines(xy[,c(2,1)],type="l",lwd=2,col="blue",pch=21,bg="red",cex=1)
				# legend("bottomright",c("Edges","Vertices"),fill=c("orange","blue"),bty="n")
				# #show threshold wher all nodes are connected
				# abline(h=xy[tmp,1],col="gray",lty=2,lwd=1)
				# abline(v=xy[tmp,2],col="gray",lty=2,lwd=1)
				# abline(v=xy[tmp,3],col="gray",lty=2,lwd=1)
				# title(paste(xy[tmp,2],"nodes and",xy[tmp,3],"edges at threshold =",xy[tmp,1]))
				# return(xy[tmp,1])
			# }
	}

# see py version: https://github.com/mcs07/CIRpy/blob/master/README.md
#use chemical resolver to get inchi keys from smiles
CIRgetR<-function(id,to=c("pubchem_sid"),return.all=TRUE,progress=TRUE){
		#id needs to be  one of the folowing types of structural ids "inchi","inchiKey" or "smiles"
		#to can be:	smiles, names, iupac_name, cas, inchi, 
		#			stdinchi, inchikey, stdinchikey,
		#			ficts, ficus, uuuuu, image, file, 
		#			mw, monoisotopic_mass,
		#			chemspider_id
		#			pubchem_sid, chemnavigator_sid, formula, chemnavigator_sid


		# smiles are coerced to a 1 column data frame
		obj<-data.frame(matrix(unlist(id),ncol=1))
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)} # need RCurl for web querry
		if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = nrow(obj), style = 3)} # show progress bar
	
		start<-"http://cactus.nci.nih.gov/chemical/structure/"
		end<-paste("/",to,sep="")
		out<-sapply(1:nrow(obj),function(i)
			{
				if (progress == TRUE){setTxtProgressBar(pb, i)}
				url<-paste(start,as.character(unlist(obj[i,])),end,sep="")
				url<-gsub("\\ ","%20",url) # fix spaces 
				tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){"error"})
					
			})
			
			if (progress == TRUE){close(pb)}
			
		#format output to only return InchI
		bad<-is.na(out)
		out<-as.character(unlist(out))
		out[bad]<-"error"
		if(return.all==FALSE){ # only return first result
				obj<-strsplit(out,"\n")
				out<-sapply(1:length(obj),function(i)
					{
						obj[[i]][1]
					})
			} 
		
		results<-matrix(out,ncol=1)
		colnames(results)<-to
		return(results)
		}

	