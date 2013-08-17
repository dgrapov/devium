
#debugging  print all names and values in input
output$debug<- renderPrint({
	obj<-names(input)
	input.obj<-lapply(1:length(obj), function(i) { input[[obj[i]]]})
	names(input.obj)<-obj
	obj<-names(values)
	values.obj<-lapply(1:length(obj), function(i) { values[[obj[i]]]})
	names(values.obj)<-obj
	
	return(list(input = input.obj,values = values.obj))
})


getdata <- function(dataset = input$datasets) {
  values[[dataset]]
}	

loadUserData <- function(uFile) {

	# ext <- file_ext(uFile)  # for some reason at tmp file is being used
	# objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	tmp<- unlist(strsplit(as.character(unlist(input$serv_upload[1,1])),"\\."))
	ext <-tmp[length(tmp)]
	
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",as.character(unlist(input$serv_upload[1,1])))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  objname <- robjname <- load(uFile)
		values[[robjname]] <- get(robjname)
	}

	if(datasets[1] == '') {
		datasets <<- c(objname)
	} else {
		datasets <<- unique(c(objname,datasets))
	}

	if(ext == 'sav') {
		values[[objname]] <- read.sav(uFile)
	} else if(ext == 'dta') {
		values[[objname]] <- read.dta(uFile)
	} else if(ext == 'csv') {
		# values[[objname]]<-read.csv(uFile)
		if(input$csv_row_header) { if(input$csv_col_header){ row.names<-1 } else{}} else {row.names<-NULL}
		values[[objname]] <- data.frame(read.csv(uFile,header = input$csv_col_header, row.names=row.names))
		# get pesky ".variables" in column names
	}
	datasets <<- unique(c(robjname,datasets))
}

#load copy and paset field
loadcopyAndPaste <- function(pFile) {
	if(input$csv_row_header) { if(input$csv_col_header){ row.names<-1 } else{}} else {row.names<-NULL}
	robjname <- "clipboard"
	dat <- read.table(header = input$csv_col_header, row.names = row.names, text= pFile)
	
	if(is.null(ncol(dat))) {
		return()
	}

	values[[robjname]] <- dat

	if(datasets[1] == '') {
		datasets <<- c(robjname)
	} else {
		datasets <<- unique(c(robjname,datasets))
	}
}

#################################################
# reactive barrowed from radyant
#################################################

uploadfunc <- reactive({

  # if(input$upload == 0) return("")
 	# fpath <- try(file.choose(), silent = TRUE)
 	# if(is(fpath, 'try-error')) {
  # 	return("")
  # } else {
  # 	return(fpath)
  # }

 	values$fpath <- ""

   	if (!is.null(input$serv_upload)) {
 	    values$fpath <- input$serv_upload[1,'datapath'] 
 	  }

  values$fpath
})

output$upload_local_server <- renderUI({ # data upload function
	   withTags(div(class='row-fluid',
                 div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
                 div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE))))
	  fileInput('serv_upload','')
})

output$downloadData <- downloadHandler(
	filename = function() { paste(input$datasets[1],'.',input$saveAs, sep='') },
  content = function(file) {

	  ext <- input$saveAs
	  robj <- input$datasets[1]
	  assign(robj, getdata())

		if(ext == 'rda' || ext == 'rdata') {
	    save(list = robj, file = file)
		} 
		else if(ext == 'dta') {
			write.dta(get(robj), file)
		} else if(ext == 'csv') {
			write.csv(get(robj), file)
		}
  })


output$datasets <- renderUI({

	fpath <- uploadfunc()
	# loading user data
	if(fpath != "" ) loadUserData(fpath)
	
	
	
	# # copyAnd paste
	if(input$copyAndPaste != "") {
		if(input$copyAndPaste != values$clipboard) {
			loadcopyAndPaste(input$copyAndPaste)
		}
	}
	
	
	# Drop-down selection of data set
	selectInput(inputId = "datasets", label = "Select:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$view_data <-renderTable({
	data.frame(getdata())
})

#choose network inddex from the data and specify its type for carrying out translations
varnames <- function() {
	if(is.null(input$datasets)) return()
	colnames(getdata())
}

DB.names <- function() {
	if(is.null(input$datasets)) return()
	list("Chemical Name" = "name", "KEGG" = "kegg", "PubChem CID" = "pubchemCID", "BioCyc" =  "biocyc" ,"InChiKey" = "inchikey") #hmdb = "HMDB"
}

# function for edge list calculations
#translate index and calculate edges
calculate_edgelist<-reactive({#function(){
	
	
	#Use KEGG RPAIRS for biochemical  connections (could add option for reaction type, currently only reporting "main" reactions)
	if(input$bio_edges){
		index<-getdata()[,input$network_index_bio]
		index.type<-switch(input$network_index_type_bio,
					kegg 	=	"KEGG",
					pubchemCID = "PubChem CID",
					name = "Chemical Name",
					biocyc = "BioCyc",
					inchikey = "InChiKey"
				)
		
	
		trans.id<- !"KEGG"%in%index.type
		if(trans.id){
			kegg.id<-CTSgetR(id = index, from=index.type,to="KEGG", async=TRUE)
		} else {kegg.id<-index}
		res<-data.frame(NULL)
		reaction.DB<-get.KEGG.pairs(type="main") # can add other types of relationships, main = direct precursor -> direct transformations
		# index.translation.DB<-data.frame(cids,kegg.ids)
		#get reaction pairs
		kegg.edges<-get.Reaction.pairs(kegg.id,reaction.DB,index.translation.DB=NULL,parallel=FALSE,translate=FALSE)
		if(length(kegg.edges)>0){
			res<-data.frame(rbind(res,as.matrix(kegg.edges)),type = "KEGG", weight = 2)	
		}
	}
	
	#chemical similarity edges based on tanimoto coefficients from PubChem CID
	if(input$chem_edges){
		index<-getdata()[,input$network_index_chem]
		index.type<-switch(input$network_index_type_chem,
					kegg 	=	"KEGG",
					pubchemCID = "PubChem CID",
					name = "Chemical Name",
					biocyc = "BioCyc",
					inchikey = "InChiKey"
				)
	
		trans.id<- !"PubChem CID"%in%index.type
		if(trans.id){
			CID.id<-CTSgetR(id = index, from=index.type,to="PubChem CID")
		} else {CID.id<-index}
		
		# get tanimoto similarity
		tani.edges<-CID.to.tanimoto(cids=CID.id, cut.off = input$tanimoto_cutoff, parallel=FALSE)
		if(nrow(tani.edges)>0){
			res<-data.frame(rbind(res,data.frame(as.matrix(tani.edges[,1:2]),type = "Tanimoto", weight = tani.edges[,3,])))
		} 
	}
	values$edge.list<-res
	return(res)
})

#biochemical connections args
output$network_index_info_bio<-renderUI({
		
	wellPanel(
	 # withTags(div(class='row',
	 # div(class='span', checkboxInput(inputId = "bio_edges", label = "",value=FALSE)),
	 # div(class='span', h4('Biochemical')))),
	 checkboxInput(inputId = "bio_edges", label = "",value=FALSE),
	 h4('Biochemical'),
	 # tags$style(type='text/css', "#bio_edges { font-weight: bold; font-size:16px;}"),
		conditionalPanel(condition = "input.bio_edges",
			selectInput(inputId = "network_index_bio", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
			selectInput(inputId = "network_index_type_bio", label = "Index type:", choices = DB.names(), selected = DB.names()[2], multiple = FALSE)
		)	
	)
		
})
	
#chemical similarity args
output$network_index_info_chem<-renderUI({
	wellPanel(
	checkboxInput(inputId = "chem_edges", label = "",value=FALSE),
	h4('Chemical Similarity'),
		conditionalPanel(condition = "input.chem_edges",
		selectInput(inputId = "network_index_chem", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		selectInput(inputId = "network_index_type_chem", label = "Index type:", choices = DB.names(), selected = DB.names()[3], multiple = FALSE),
		numericInput(inputId = "tanimoto_cutoff" , "cutoff", value = 0.7, min = 0, max = 1, step = .005)
	))
})

#spectral similarity args	
output$network_index_info_spec<-renderUI({
	wellPanel(
	checkboxInput(inputId = "spec_edges", label = "",value=FALSE),
	h4('Spectral Similarity'),
		conditionalPanel(condition = "input.spec_edges",
		selectInput(inputId = "network_index_spec", label = "Metabolite index:", choices = varnames(), selected = varnames()[1], multiple = FALSE),
		selectInput(inputId = "network_index_type_spec", label = "Index type:", choices = DB.names(), selected = DB.names()[1], multiple = FALSE),
		numericInput(inputId = "spec_cutoff" , "cutoff", value = 0.7, min = 0, max = 1, step = .005)
	))
})

#correlation args	
output$network_index_info_cor<-renderUI({
	 
wellPanel(
	checkboxInput(inputId = "cor_edges", label = "",value=FALSE),
	h4('Correlation'),
		conditionalPanel(condition = "input.cor_edges",
		selectInput(inputId = "network_index_cor", label = "Metabolite index:", choices = varnames(), selected = varnames(), multiple = TRUE),
		selectInput(inputId = "network_index_type_cor", label = "Index type:", choices = DB.names(), selected = DB.names()[1], multiple = FALSE),
		numericInput(inputId = "cor_cutoff" , "p-value", value = 0.05, min = 0, max = 1, step = .0005)
	))
})


# Generate output for the summary tab
# output$summary <- renderUI(function() {
output$edge_list <- renderTable({
	if (input$create_edgelist == 0) 
		return(data.frame(""))
		
	#calculate edges		
	isolate({
		calculate_edgelist()
	})
	
})

# Generate output for the plots tab
output$network <- renderPlot({
		
		if(!input$metabomapr == "Network") return()
		if(is.null(valuse$edge.list)) plot(x = 1, type = 'n', main="Please calculate edge list first.", axes = FALSE, xlab = "", ylab = "")
		
		#igraph network
		#options for igraph.plot
		#see all options http://127.0.0.1:10494/library/ig      raph/html/plot.common.html
		graph.par.obj<-list(
		x = igraph.obj,# this is also calculated by the function should control
		mark.groups = node.par$group,
		mark.col = mark.col,
		layout = DB[,c("x.pos","y.pos")],#"layout.fruchterman.reingold",
		vertex.label = node.par$name, 
		vertex.color = mark.col,
		vertex.size = 2,
		vertex.label.dist=-2)

		#make plot
		devium.igraph.plot(edge.list,graph.par.obj,plot.type="static",add=FALSE) # if running in local mode plot can be interactive or 3D
	})
