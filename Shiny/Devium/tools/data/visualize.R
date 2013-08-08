radyant<-function(){
# # variable selection in the datatabs views
# output$vizvars1 <- renderUI({
	# cols <- varnames()
	# if(is.null(cols)) return()

	# selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
# })

# # variable selection
# output$vizvars2 <- renderUI({
	# cols <- varnames()
	# if(is.null(cols)) return()
	# # selectInput(inputId = "vizvars2", label = "Y-variable", choices = as.list(cols[-which(cols == input$vizvars1)]), selected = NULL, multiple = TRUE)
	# selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE)
# })

# output$viz_color <- renderUI({
	# cols <- varnames()
	# if(is.null(cols)) return()
	# selectInput('viz_color', 'Color', c('None'="", as.list(cols)))
# })

# output$viz_facet_row <- renderUI({
	# cols <- varnames()
	# if(is.null(cols)) return()
	# # isFct <- sapply(getdata(), is.factor || is.integer)
	# isFct <- sapply(getdata(), is.factor)
 	# cols <- cols[isFct]
	# selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols)))
# })

# output$viz_facet_col <- renderUI({
	# cols <- varnames()
	# if(is.null(cols)) return()
	# # isFct <- sapply(getdata(), is.factor || is.integer)
	# isFct <- sapply(getdata(), is.factor)
 	# cols <- cols[isFct]
	# selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols)))
# })

# output$visualize <- renderPlot({
	# if(is.null(input$datasets) || is.null(input$vizvars2)) return()
	# if(input$datatabs != 'Visualize') return()

		# # inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
		# dat <- getdata()

		# if(input$vizvars2 == "") {
			# p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(colour = 'black', fill = 'blue') 
			# return(print(p))
		# } else {
		  # p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
		# }

    # if (input$viz_color != '') {
    	# # p <- p + aes_string(color=input$viz_color) + scale_colour_gradient(colors=rainbow(4))
    	# p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
    # }

    # facets <- paste(input$viz_facet_row, '~', input$viz_facet_col)
    # if (facets != '. ~ .')
      # p <- p + facet_grid(facets)
    
    # if (input$viz_jitter)
      # p <- p + geom_jitter()
    # if (input$viz_smooth)
      # p <- p + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
    
    # print(p)

# }, width = 700, height = 700)

# # will be used in ui.R
# output$ui_visualize <- renderUI({
	# ui_visualize()
# })

# ui_visualize <- function() {
	# wellPanel(
		# uiOutput("vizvars1"),
		# uiOutput("vizvars2"),
		# conditionalPanel(condition = "input.vizvars2 != ''",
		  # uiOutput("viz_color"),
		  # uiOutput("viz_facet_row"),
		  # uiOutput("viz_facet_col"),
		  # checkboxInput('viz_smooth', 'Smooth', value = TRUE),
		  # checkboxInput('viz_jitter', 'Jitter', value = FALSE)
		# )
	# )
# }

}

other<-function(){
# #server side code
# ##################

# # #set UI
# output$selected.ui <- renderUI({
# if (is.null(getdata())){return()}
  # get(paste0(input$plot_type,".ui"))()
# })

	
# output$plot_type<-renderUI({
# if (is.null(getdata())){return()}
	# plot_type()
# })

# #getting shattered code again
# output$box_plot.ui<-renderUI({

	# if(ui.opts$plot_type()=="box_plot")
		# checkboxInput("show.points", "show points", TRUE)
# })

# # size
# #selector
# output$size.ui<-renderUI({
	# if (is.null(getdata())){return()}
		# if(input$plot_type=="scatter_plot") {
			# radioButtons("size_mapping","Size", choices = c("absolute","variable"), selected = "absolute")
		# }
	# })
	
# #variable mapping
# size_variable.ui<-renderUI ({
		# if(is.null(input$size_mapping)) return() else {
			# if(input$size_mapping=="variable") {	
				# var.opts<-varnames()
				# updateSelectInput(session, "size_variable", choices = var.opts)
			# }
		# }
	# })

# #absolute mapping
# size_absolute.ui<-renderUI ({
	# if (is.null(getdata())){return()}
		# if(input$size_mapping=="absolute") {
				
			# sliderInput("size", 
						# "", 
						# min = 1,
						# max = 20,
						# step = 1,					
						# value = 3)	
		# }
# })

# #alpha
# output$alpha.ui<-renderUI({
# if (is.null(getdata())){return()}
	# sliderInput("alpha", 
				# "Transparency", 
				# min = .1,
				# max = 1, 
				# step = .005,
				# value = .75)	
	# })				
	

# #UI code
# ######################3
# #porting custom ggplot2 plotting fxns
#data dependnet UIs 
# x_var<-function(){
		# uiOutput("x_var") 
		# }
# y_var<-function(){
		# uiOutput("y_var") 
		# }
# factor for grouping visualizations
# group_var<-function(){
		# uiOutput("group_var") 
		# }	

# #debugging
# .ui<-args.<-function(){NULL} #plot.
		
# #UIs
# plot_type<- function() {	
	# wellPanel(
		 # selectInput(
		# "plot_type", "Plot Type",
		# c("Histogram" 	= "histogram",
		# "Density plot" 	= "density_plot",
		# "Box plot" 		= "box_plot",
		# "Bar plot" 		= "bar_plot",
		# "Scatter plot" 	= "scatter_plot"),		
		# selected  		= "Histogram"
		# ),
		
		# #conditional UIs
		# conditionalPanel(
				# condition = "input.plot_type == 'box_plot'",		
				# #box_plot.ui()
				# checkboxInput('viz_smooth', 'Smooth', value = TRUE)
		# ),
		# conditionalPanel(
				# condition = "input.plot_type == 'density_plot'",		
				# #density_plot.ui()
				# checkboxInput('viz_smooth', 'Spoon', value = TRUE)
		# )#,
		# # conditionalPanel(
				# # condition = "input.plot_type == 'bar_plot'",		
				# # bar_plot.ui()
		# # ),
		# # conditionalPanel(
				# # condition = "input.plot_type == 'histogram'",		
				# # histogram.ui()
		# # )
		
	# )
# }
		

# #plot sepcific uis
# # #boxplot
# box_plot<-function(){ 
 
# uiOutput("box_plot.ui")
 # # wellPanel(
	# # y_var(),
	# # group_var(),
	# # checkboxInput("show_points", "show points", TRUE)
  # # )
# }

# size<-function(){ 
# # wellPanel(
# list(
	# uiOutput("size.ui"),
	# # uiOutput("size_variable.ui"),
	# # uiOutput("size_absolute.ui")
	# conditionalPanel(
		# condition = "input.size_mapping == 'absolute'",
		# sliderInput("size", 
							# "", 
							# min = 1,
							# max = 20,
							# step = 1,					
							# value = 3)	
	# ),
	# conditionalPanel(
		# condition = "input.size_mapping == 'variable'",
			# # uiOutput("size_variable.ui")
			# selectInput("size_variable", "", "Loading")
		# # )
	# )
# )
# }	


# alpha<-function(){ 
# uiOutput("alpha.ui")
# }


# # generic 1D plots density plot
# # UIs
# density_plot.ui<-bar_plot.ui<-histogram.ui<-scatter_plot.ui<-box_plot.ui<-function(){ 
 # wellPanel(
	# y_var(),
	# x_var(),
	# group_var(),
	# size(),
	# alpha(),
	# box_plot()
  # )
# }


# args.density_plot<-args.histogram<-args.bar_plot<-args.scatter_plot<-args.box_plot<-function(tmp,input,output){
# # tmp$group_var<-input$group_var
			# # tmp$y_var<-input$y_var#
			# tmp$variable<-input$y_var#ui.opts$variable()#input$variable#
			# tmp$x_variable<-input$x_var
			# tmp$group<-input$group_var#
			# tmp$plot.type<-input$plot_type#"density_plot"
			# tmp$size.mapping<-input$size_mapping
			# tmp$size<- input$size
			# tmp$size.variable<- input$size_variable
			# tmp$alpha<- input$alpha
			# tmp$show.points<-input$show.points	
			
			# #plot
			# output$plots<-renderPlot({
				# #plot.basic_plot(tmp)
				# make.plot(obj = tmp)	
			# })	
			# #print something
			# output$summary <- renderPrint({
			# str(tmp$data)
		# })
# }

# #set and run args
# set.args<-function(obj,tmp , input , output){
	# # type<-obj$analysis.type
	# do.call(paste0("args.",obj),list(tmp = tmp, input = input, output = output))
# }

# #switching plot
# make.plot<-function(obj){
	# # type<-obj$analysis.type
	# do.call(paste0("plot.",obj$plot.type),list(plot.obj = obj))
# }

}


#main plotting function
#could pass ars directly (fast and easy, Shiny only) or collect in an another object and then pass (more universal)
make.ggplot<-reactive({ #function() plot.obj,...
	
	# many vars could referenced directly from input
	plot.obj<-list()
	# need:
	plot.obj$plot.type<-input$plot_type
	#data
	plot.obj$data<-get(input$datasets)
	plot.obj$xvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$x_var]
	plot.obj$yvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$y_var]
	# variables to map
	plot.obj$group<-plot.obj$data[,colnames(plot.obj$data)%in%input$group_var]
	plot.obj$size<-if(input$size_mapping=="variable"){
			size.lab<-input$size_variable
			plot.obj$data[,colnames(plot.obj$data)%in%input$size_variable]	
		} else {
			size.lab<-""
			input$size			
		}
	# asthetics
	plot.obj$show.points<-input$show_points
	plot.obj$alpha<-input$alpha
	plot.obj$size.mapping<-input$size_mapping
	
	
	#create ggplot data object
	#need to make sure input is not NULL and has length != 0
	#main data object (has to be a better way)
	tmp.data<-data.frame( 	group	= if(length(plot.obj$group)>0){ plot.obj$group } else { data.frame(group=0) }, 
							yvar	= if(length(plot.obj$yvar)>0){ 	plot.obj$yvar } else { data.frame(yvar=0) }, 
							xvar	= if(length(plot.obj$xvar)>0){ 	plot.obj$xvar } else { data.frame(xvar=0) },
							size	= if(length(plot.obj$size)>0){ plot.obj$size } else { data.frame(size=0) },
							facet.y	= if(length(input$y_facet)>0){ 
										if(!input$y_facet ==".") {	
												plot.obj$data[,colnames(plot.obj$data)%in%input$y_facet,drop=FALSE]
											} else { 
												data.frame(y.facet=0) 
											}
										},
							facet.x	= if(length(input$x_facet)>0){ 
										if(!input$x_facet ==".") {	
												plot.obj$data[,colnames(plot.obj$data)%in%input$x_facet,drop=FALSE]
											} else { 
												data.frame(x.facet=0) 
											}
										}			
							)
							
	
	#switch ggplot add ons specific to each plot type 
	type<-as.character(plot.obj$plot.type)
	plot.type<-switch(type,
			"box_plot" 		= 	.local<-function(tmp){geom_boxplot()},
			"histogram" 	=	.local<-function(tmp){geom_histogram(alpha=plot.obj$alpha,position="identity")},
			"density_plot" 	=	.local<-function(tmp){geom_density(alpha=plot.obj$alpha)},
			"bar_plot" 		=	.local<-function(tmp){geom_bar(alpha=plot.obj$alpha,position="dodge")}, # width is not working
			"scatter_plot"  =	.local<-function(tmp){
									if(tmp$size.mapping=="absolute"){
										geom_point(alpha=plot.obj$alpha, size=plot.obj$size, legend=FALSE)
									} else {
										geom_point(alpha=plot.obj$alpha, aes(size=size))
									}
								}	
		)
	plot.type<-plot.type(plot.obj)
	
	#facet type
	facet.type<-paste(input$y_facet, '~', input$x_facet)
	if (facet.type != '. ~ .') {facet.type<-facet_grid(facet.type)} else {facet.type<-NULL}

	
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
				 
	if(type=="box_plot")	{		#control for 1D or 2D graphs 
		p<-ggplot(tmp.data, 
				aes(
					x 		= group, 
					y 		= yvar,
					fill 	= as.factor(group)
					)
				) + plot.type + facet.type
				
				if(plot.obj$show.points==TRUE)
				{ 
					p<-p+ geom_point(color='black',alpha=plot.obj$alpha, position = 'jitter')
				}
				
		} else {
		
		p<-ggplot(tmp.data, 
				aes(
					x 		= yvar,
					fill 	= as.factor(group)#,
					# group 	= as.factor(group),
					# color 	= as.factor(plot.obj$group)
					)
				) + plot.type + facet.type
		}
	
		if(type=="scatter_plot"){
			p<-ggplot(tmp.data, 
				aes(
					x 		= xvar,
					y 		= yvar,
					size 	= size,	
					# fill 	= group#,
					# group 	= as.factor(group),
					color 	= as.factor(group)
					)
				) + plot.type + facet.type
			labels<-labs(
				color 	= input$group_var,
				x 		= input$x_var,
				y 		= plot.obj$y_var,
				size 	= size.lab
			)  
		} else {
			labels<-labs(
					fill 	= input$group_var,
					x 		= "",
					y 		= input$y_var
				)  
		}
	 p<- p+ labels +
	.theme
	print(p)
})

#out put for Visualize
output$visualize <- renderPlot({
	if(is.null(input$datasets)) return()
	if(input$datatabs != 'Visualize') return()
	
	make.ggplot()
	
})

# dataview visualizer
output$ui_visualize <- renderUI({
	ui_visualize()
})

#shared UIs
#alpha slider 
output$alpha.ui<-renderUI({
if (is.null(getdata())){return()}
	sliderInput("alpha", 
				"Transparency", 
				min = .1,
				max = 1, 
				step = .005,
				value = .75)	
	})

# seems that UIs can not be shared between multiple functions 
# need to create redundant UIs or think of holistic hide show interface
# x-axis
output$x_var<-renderUI({
	if (is.null(getdata())){return()}
			var.opts<-varnames()#colnames(get(input$data))#
			selectInput("x_var", "X-variable", var.opts)
			# updateSelectInput(session, "variable", choices = var.opts)
	 })
#y-axis	 
output$y_var<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		selectInput("y_var", "Y-variable", var.opts)
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

#group
output$group_var<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		selectInput("group_var", "Group",c( none="", var.opts))
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

#map variable to size
output$size_variable<-renderUI({
				if (is.null(getdata())){return()}
				var.opts<-varnames()#colnames(get(input$data))#
				selectInput("size_variable", "Size", var.opts)
})

#size
size.ui<-renderUI({ 
	wellPanel(
		radioButtons("size_mapping","Size", choices = c("absolute","variable"), selected = "absolute"),
		conditionalPanel(
			condition = "input.size_mapping == 'absolute'",
			sliderInput("size", 
								"", 
								min = 1,
								max = 20,
								step = 1,					
								value = 3)	
		),
		conditionalPanel(
				condition = "input.size_mapping == 'variable'",
				uiOutput("size_variable")
			) 
	)
})

# y-facet
output$y_facet<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		factor.id <- sapply(getdata(), is.factor)
		var.opts<-var.opts[factor.id]
		selectInput("y_facet", "Y-facet", c(none=".",var.opts))
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

# x-facet
output$x_facet<-renderUI({
		var.opts<-varnames()#colnames(get(input$data))#
		factor.id <- sapply(getdata(), is.factor)
		var.opts<-var.opts[factor.id]
		selectInput("x_facet", "X-facet", c(none=".",var.opts))
	 })	
				
#main visualize UI				
ui_visualize <- function() {

	wellPanel(
		 selectInput(
		"plot_type", "Plot Type",
		c("Histogram" 	= "histogram",
		"Density plot" 	= "density_plot",
		"Box plot" 		= "box_plot",
		"Bar plot" 		= "bar_plot",
		"Scatter plot" 	= "scatter_plot"),		
		selected  		= "Histogram"
		 ),
		
		# inteweaving UIs
		conditionalPanel(
				condition = "input.plot_type == 'scatter_plot'",
				uiOutput("x_var")
		),
		uiOutput("y_var"),
		uiOutput("group_var"),
		uiOutput("y_facet"),
		uiOutput("x_facet"),
		conditionalPanel(
				condition = "input.plot_type == 'scatter_plot'",
				size.ui()
		),
		#boxplot
		conditionalPanel(
				condition = "input.plot_type == 'box_plot'",		
				checkboxInput("show_points", "show points", TRUE)
		),
		uiOutput("alpha.ui")
	)
}
	
# dataview visualizer
output$ui_visualize <- renderUI({
	ui_visualize()
})