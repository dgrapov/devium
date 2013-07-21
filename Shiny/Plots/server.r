
shinyServer(function(input, output, session) {

	#initialize
	if(!exists("ui.opts")){ui.opts<<-list()} # probably not nessesary
	
	# # load some fxns which maybe updated (doesn't seem to work as expected?)
	# source("global.r")
	# source("reactive.r")
	
	#data
	ui.opts$data<-reactive({
		switch(input$input_type,
					"demo" = .local<-function(){
									get(input$demo.data)
								},
					"load" = .local<-function(){
									if(is.null(input$files) ) { return() } else { 
										read.csv(input$load.data$datapath, header=T, stringsAsFactors = T)	
									}			
								}
				)
		.local()		
	})
	
	#plot type
	ui.opts$plot_type<-reactive({  #ui.opts$analysis.type<-
		input$plot_type
	})

	# size mapping
	ui.opts$size_mapping<-reactive({  #ui.opts$analysis.type<-
		input$size_mapping
	})
	# #print something
			# output$summary <- renderPrint({
			# input$size_mapping
		# })
	#set UI
	output$selected.ui <- renderUI({
	if (is.null(ui.opts$data())){return()}
	  get(paste0(ui.opts$plot_type(),".ui"))()
	})
	
	# Reactives UIs (later source from elsewhere, observe gave strange reseting behavior)
	output$x_var<-renderUI({
		if (is.null(ui.opts$data())){return()}
			if(ui.opts$plot_type()=="scatter_plot") {
				var.opts<-colnames(ui.opts$data())#colnames(get(input$data))#
				selectInput("x_var", "X-variable", var.opts)
				# updateSelectInput(session, "variable", choices = var.opts)
			}
		 })
		 
	output$y_var<-renderUI({
		if (is.null(ui.opts$data())){return()}
			var.opts<-colnames(ui.opts$data())#colnames(get(input$data))#
			selectInput("y_var", "Y-variable", var.opts)
			# updateSelectInput(session, "variable", choices = var.opts)
		 })	

	output$group_var<-renderUI({
		if (is.null(ui.opts$data())){return()}
			var.opts<-colnames(ui.opts$data())#colnames(get(input$data))#
			selectInput("group_var", "Group", var.opts)
			# updateSelectInput(session, "variable", choices = var.opts)
		 })	
		
	output$plot_type<-renderUI({
	if (is.null(ui.opts$data())){return()}
		plot_type()
	})
	
	#getting shattered code again
	output$box_plot.ui<-renderUI({
	
		if(ui.opts$plot_type()=="box_plot")
			checkboxInput("show.points", "show points", TRUE)
	})
	
	#size
	observe({
	if (is.null(ui.opts$data())){return()}
	# size_variable.ui<-renderUI ({
		#if(is.null(ui.opts$size_mapping())) return() else {
			#if(ui.opts$size_mapping()=="variable") {	
				var.opts<-colnames(ui.opts$data())
				updateSelectInput(session, "size_variable", choices = var.opts)
			#}
		#}
	})	
	
	size_absolute.ui<-renderUI ({
		if (is.null(ui.opts$data())){return()}
			if(ui.opts$size_mapping()=="absolute") {
					
				sliderInput("size", 
							"", 
							min = 1,
							max = 20,
							step = 1,					
							value = 3)	
			}
		#}
	})
	
	#size
	output$size.ui<-renderUI({
		if (is.null(ui.opts$data())){return()}
			if(ui.opts$plot_type()=="scatter_plot") {
				radioButtons("size_mapping","Size", choices = c("absolute","variable"), selected = "absolute")
			}
		})

	#alpha
	output$alpha.ui<-renderUI({
	if (is.null(ui.opts$data())){return()}
		sliderInput("alpha", 
					"Transparency", 
					min = .1,
					max = 1, 
					step = .005,
					value = .75)	
		})				
		
	#do stuff
	observe({
	if (is.null(ui.opts$data())){return()}
		#globals
		if(!exists("tmp")){tmp<-list()}
		
		tmp$data<-ui.opts$data() #get(input$demo.data)#
		tmp$plot.type<-ui.opts$plot_type()
		
		#call fxn
		set.args(tmp$plot.type,tmp = tmp, input = input, output = output)
		# do.call(paste0("args.",tmp$plot.type),args=list(tmp = tmp, input = input, output = output))
		
	})

	
  })
