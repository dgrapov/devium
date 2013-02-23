
# functions
#-------------------------------------------------------------
#create a network from a graphNeL object
make.cynet<-function(graph,network.name,layout='jgraph-spring')
	{
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
set.edge.attribute<-function(network,edge.names,edge.attribute,edge.attribute.value)
	{
	
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
set.node.attribute<-function(network,node.names,node.attribute,node.attribute.value)
	{
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
link.cyto.graph.nodes.select<-function(graphs,visual.style.names=NULL)
	{
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
save.cyto.graph.styles<-function(network,prefix=NA)
	{
		sapply(1:length(network),function(i)
		{
			if(is.na(prefix))name<-paste("style-",network[i],sep="") else name<-paste(prefix,network[i],sep="") 
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#delete styles (not validated)	
delete.cyto.graph.styles<-function(network)
	{
		sapply(1:length(network),function(i)
		{
			name<-paste(network[i],"-style",sep="")
			copyVisualStyle (get(network[i]),'default',name) 
			setVisualStyle (get(network[i]), name) 
			name
		})
	}

#create a node legend network
cyto.node.legend<-function(network="new",node.attribute,node.attribute.value,node.names,legend.title="node legend",unique.matched=FALSE)
	{
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
convert.to.edge.list<-function(graph)
	{	
		return(do.call("rbind",strsplit(as.character(names(cy2.edge.names (graph@graph) )),"~")))
	}

#convert  qpgraph output to edge list output
mat.to.edge.list<-function(input,graph)
	{
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
gen.mat.to.edge.list<-function(mat)
	{
		
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
edge.list.trim<-function(edge.list,index,cut,less.than=FALSE)
		{
			if(less.than==TRUE){
					edge.list[index<=cut,,drop=FALSE]
				}else{
					edge.list[index>=cut,,drop=FALSE]
					}
		}

#----Identify differences in measures between two classes given some 
# threshhold (i.e. p-value) and filter out put based on pcor, combined network
# inputs structure as output from qpgraph
compare.2class.network<-function(network,class1.obj,class2.obj, threshold=0.05)
	{
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
qpgraph.compare<-function(data,factor,threshold="auto",...)
	{
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
make.ave.qpgraph<-function(data,tests=200,for.col=TRUE,...)
	{
		library(qpgraph)
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
choose.qpgraph.threshold<-function(qpnetwork,.threshold=c(0,.6),choose=NULL)
	{
		#choose elbow in vertex vs. edge plot
		int<-c(seq(.threshold[1],.threshold[2],by=.01)[-1])
		
		xy<-do.call("rbind",lapply(1:length(int),function(i)
				{
					net <- qpGraph(qpnetwork, threshold=int[i], return.type="graphNEL")
					data.frame(threshold=int[i],nodes=length(net@nodes),edges=length(unlist(net@edgeL))/2)
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
partial.correl<- function(data, qpnet, verbose=T, for.col=TRUE,...)
	{
		.local<-function(data, qpnet, verbose=verbose,long.dim.are.variables,...)
			{
				qpPAC(data, g=qpnet, verbose=verbose,long.dim.are.variables=long.dim.are.variables,...) 
			}
		if(dim(data)[2]<=dim(data)[1]&for.col==TRUE)long.dim.are.variables<-FALSE else long.dim.are.variables<-TRUE
		.local(data, qpnet=qpnet,verbose=verbose, long.dim.are.variables=long.dim.are.variables,...)
	}

#make graphNEL object from edge list
edge.list.to.graphNEL<-function(edge.list)
	{
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
sym.mat.to.edge.list<-function(mat,edge.list)
	{
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
match.cynet.edge.order<-function(obj,cynet)
	{
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
filter.edges<-function(edge.list,filter,cut.off=NULL)
	{
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

#create edge list and network attributes file from meta.data wioth CID keys
#data<- bound with CIDS
#filter object basd on cid index to remove duplicates
unique.obj<-function(data, index)
	{
		#accesory function to return position of first instance of unique object 
		id<-unique.id(index)
		data[id,]
	}

#look up KEGG reactant pairs 
get.KEGG.pairs<-function(url="https://gist.github.com/dgrapov/4964564/raw/aec1a5097a3265d22109c9b34edd99a28f4012a3/KEGG+reaction+pairs")
	{
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		tmp2[[1]]<-strsplit(tmp2[[1]],"\\  ")
		matrix(unlist(tmp2),ncol=2, byrow=TRUE)
	}
#look up CID to KEGG translation 	
get.CID.KEGG.pairs<-function(url="https://gist.github.com/dgrapov/4964546/raw/c84f8f209f961b23adbf7d7bd1f704ce7a1166ed/CID_KEGG+pairs")
	{
		if(require(RCurl)==FALSE){install.packages("RCurl");library(RCurl)} else { library(RCurl)}
		text<-tryCatch( getURL(url,ssl.verifypeer=FALSE) ,error=function(e){NULL})
		tmp<-strsplit(text,"\\n")
		tmp2<-strsplit(as.character(unlist(tmp)), "\t")
		#fix header errors
		matrix(unlist(tmp2),ncol=2, byrow=TRUE)
	}

	#making an edge list based on CIDs from KEGG reactant pairs
CID.to.KEGG.pairs<-function(cid,database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs())
	{
		matched<-lookup[c(1:nrow(lookup))[lookup[,1]%in%cid],]
		ids<-sapply(1:nrow(matched),function(i)
			{
				#
				c(which(as.character(matched[i,2])==as.character(database[,1])),which(as.character(matched[i,2])==as.character(database[,2])))				
			})
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

#get tanimoto distances from cids
CID.to.tanimoto<-function(cids, cut.off = .7, parallel=TRUE)
{
	#used cids = PUBCHEM CIDS to calculate tanimoto distances
	check.get.packages(c("snow","doSNOW","foreach","ChemmineR")) # need packages
	
	#get fingerprint for calcs
	data(pubchemFPencoding)
	compounds <- getIds(as.numeric(cids))
	# Convert base 64 encoded fingerprints to character vector, matrix or FPset object
	fpset <- fp2bit(compounds, type=3)
	dimnames(fpset@fpma)[1]<-list(as.character(cids))
	
	if(parallel==TRUE)
		{
				#change this later
				cl.tmp = makeCluster(rep("localhost",Sys.getenv('NUMBER_OF_PROCESSORS')), type="SOCK") 
				registerDoSNOW(cl.tmp) 
				out<-foreach(j=c(1:length(cid(fpset))),.combine="cbind") %dopar% ChemmineR::fpSim(fpset[j], fpset, sorted=FALSE)#length(codes)
				stopCluster(cl.tmp)	
		} else {
		
				out<-sapply(1:length(cid(fpset)),function(i){ChemmineR::fpSim(fpset[i], fpset, sorted=FALSE)})
		}
		
	#edgelist
	colnames(out)<-unlist(dimnames(out)[1])
	elist<-gen.mat.to.edge.list(out)
	
	#optionally filter based on score based on score
	obj<-as.matrix(elist)
	pass<-!as.numeric(obj[,3])<=cut.off
	
	#return edgelist 
	as.data.frame(obj[pass,1:2])
}

#querry chemical translation service (CTS) to get tanimoto from inchis
#very slow
CID.to.tanimoto<-function(cid,lookup=get.CID.INCHIcode.pairs())
	{
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
devium.network.execute<-function(object)
	{
		#stored in get("devium.network.object",envir=devium)
		main<-tryCatch(get(object$devium.network.target.object),error=function(e){NULL})
				if(is.null(main))
					{ 
							return()
					} else {
						#temporary routing functions
						edge.list.type<-object$devium.network.edge.list.type
						switch(edge.list.type,
						"spearman correlations" = .local<-function()
													{
														#cut out factors if present
														tmp.data<-main[sapply(1:ncol(main), function(i) {class(main[,i])=="numeric"})]
														cor.mat<-cor(tmp.data, method="spearman") #use correlations to get edge list

														#make edge list from a square symmetric matrix	
														edge.list<-gen.mat.to.edge.list(cor.mat)
					
														#add options for filter
														
														#return edge list and value
														return(edge.list) # [,1:2]
														
													},
													
						"KEGG reaction pairs" 	= .local<-function()
													{
														#return edge list
														CID.to.KEGG.pairs(as.matrix(main),database=get.KEGG.pairs(),lookup=get.CID.KEGG.pairs())
													},
													
						"Tanimoto distances"	= .local<-function()
													{
														#return edge list
														CID.to.tanimoto(as.matrix(main), cut.off = .7, parallel=TRUE)
													}							
							
						)
						
					elist<-.local()	
					d.assign("devium.network.edge.list.calculated",elist,main.object="devium.network.object")
					#may want to to also assign to global	
					assign(paste(object$devium.network.target.object,"network.edge.list", sep="."),elist, envir=globalenv())
					}
	}

#functions for devium network GUI to plot edge list
devium.network.plot<-function(edge.list, type, graph.obj=NULL)
	{
		check.get.packages(c("igraph","graph")) 
		#optionaly try to get defaults
		
		
		switch(type,
		"static" = .local<-function(edge.list,graph.obj){devium.igraph.plot(edge.list[,1:2], graph.par.obj=graph.obj,add=FALSE)},
		 "interactive" = .local<-function(edge.list,type,graph.obj)
								{
									#create grapNEL object from edge list
									graph.obj<-edge.list.to.graphNEL(edge.list)
									# could coalculate tis directly but fornow going through NEL because it is also used for Cytoscape graphs
									igraph.obj<-igraph.from.graphNEL(graph.obj, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
									#with groups marked
									igraph.obj$V<-unclass(igraph.obj)[[9]][[3]]$name #add labels has to be a better way?
									mark.groups<-list()
									tkplot(igraph.obj, layout=layout.fruchterman.reingold, vertex.color="gray",vertex.size=6, vertex.label=igraph.obj$V, frame=FALSE,vertex.label.dist=-1.5)
								},
								
			"3D-plot" = .local<-function(edge.list,type,graph.obj)
								{
									#create grapNEL object from edge list
									graph.obj<-edge.list.to.graphNEL(edge.list)
									# could coalculate tis directly but fornow going through NEL because it is also used for Cytoscape graphs
									igraph.obj<-igraph.from.graphNEL(graph.obj, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
									#with groups marked
									igraph.obj$V<-unclass(igraph.obj)[[9]][[3]]$name #add labels has to be a better way?
									rglplot(igraph.obj,layout=layout.fruchterman.reingold, vertex.color="gray",vertex.size=6, vertex.label=igraph.obj$V, vertex.label.dist=-.25)
								})
				
			.local(edge.list,graph.obj)
	}

#function to add to existing igraph.plot
devium.igraph.plot<-function(edge.list, graph.par.obj=NULL,add=FALSE)
	{
		check.get.packages(c("igraph","graph")) 
		
		#create grapNEL object from edge list
		graph.obj<-edge.list.to.graphNEL(edge.list)
		# could calculate tis directly but for now going through NEL because it is also used for Cytoscape graphs
		igraph.obj<-igraph.from.graphNEL(graph.obj, name = TRUE, weight = TRUE,unlist.attrs = TRUE)
	
		
		#default options
		defaults<-list(
		x= igraph.obj,
		mark.groups =  NULL,
		layout = get("layout.fruchterman.reingold"),  #have to get this later
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
						if(as.character(names(defaults)[i])%in%as.character(names(graph.par.obj)[j]))
							{
									graph.par[[i]]<-tryCatch(get(unlist(graph.par.obj[j])),error=function(e){graph.par.obj[j]})
							} else { 
									tmp<-graph.par[i]
									graph.par[i]<-tmp
							}
						names(graph.par[i])<-names(defaults)[i]
					}
			}
		
		# # make a call to set visual properties
		# tmp<-graph.par
		# tmp.call<-sapply(1:length(tmp),function(i)
			# {
				# if(length(tmp[[i]])>1)
					# {
							# paste(names(tmp)[i],"=get(graph.par[",i,"])", sep="")
					# } else {
							# tmp.obj<-data.frame(cbind(names(tmp)[i],tmp[i]))
							# join.columns(tmp.obj,char="=",quote.last=TRUE)
					# }
			# })
					
		# #make character joined on ","
		# visual.call<-paste(c(unlist(tmp.call), paste("add=",add,sep="")),collapse=",")
			
		# #graph making properties
		# graph.call<-paste(c("x=get('igraph.obj')",unlist(visual.call)),collapse=",")
		
		#call plot
		do.call("plot",graph.par)
			
	}