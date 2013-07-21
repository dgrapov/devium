options(shiny.trace = TRUE)
#fun fact: period in name = your gonna have a bad time

#check for and/or install dependencies
need<-c("datasets","ggplot2") # some more should auto load...
for(i in 1:length(need)){
  if(require(need[i], character.only = TRUE)==FALSE){install.packages(need[i]);library(need[i], character.only = TRUE)} else { library(need[i],character.only = TRUE)}
}

# convert to named list, probably not useless
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}


# demo data sets
data.options<-function(){
	# use to see datasets
	# code from http://stackoverflow.com/questions/12575098/to-see-all-the-content-not-just-objects-in-a-package-in-r
	lsp <- function (package, all.names = FALSE, pattern) {
	  package <- deparse(substitute(package))
	  ls(pos = paste("package", package, sep = ":"), all.names = all.names, 
		 pattern = pattern)
	}
	
  set<-lsp(datasets)
  set.class<-sapply(seq(set), function(i){ class(get(set[i]))})
  set[set.class=="data.frame"] #|set.class=="matrix"
}


#data dependnet UIs
x_var<-function(){
		uiOutput("x_var") 
		}
y_var<-function(){
		uiOutput("y_var") 
		}
# factor for grouping visualizations
group_var<-function(){
		uiOutput("group_var") 
		}	

#debugging
.ui<-args.<-function(){NULL} #plot.

		
#UIs
plot_type<- function() {	
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
		
		#conditional UIs
		conditionalPanel(
				condition = "input.plot_type == 'box_plot'",		
				box_plot.ui()
		),
		conditionalPanel(
				condition = "input.plot_type == 'density_plot'",		
				density_plot.ui()
		),
		conditionalPanel(
				condition = "input.plot_type == 'bar_plot'",		
				bar_plot.ui()
		),
		conditionalPanel(
				condition = "input.plot_type == 'histogram'",		
				histogram.ui()
		)
		
	)
}
		

#plot sepcific uis
# #boxplot
box_plot<-function(){ 
 
uiOutput("box_plot.ui")
 # wellPanel(
	# y_var(),
	# group_var(),
	# checkboxInput("show.points", "show points", TRUE)
  # )
}

size<-function(){ 
# wellPanel(
list(
	uiOutput("size.ui"),
	# uiOutput("size_variable.ui"),
	# uiOutput("size_absolute.ui")
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
			# uiOutput("size_variable.ui")
			selectInput("size_variable", "", "Loading")
		# )
	)
)
}	


alpha<-function(){ 
uiOutput("alpha.ui")
}


# generic 1D plots density plot
density_plot.ui<-bar_plot.ui<-histogram.ui<-scatter_plot.ui<-box_plot.ui<-function(){ 
 wellPanel(
	y_var(),
	x_var(),
	group_var(),
	size(),
	alpha(),
	box_plot()
  )
}


args.density_plot<-args.histogram<-args.bar_plot<-args.scatter_plot<-args.box_plot<-function(tmp,input,output){
# tmp$group_var<-input$group_var
			# tmp$y_var<-input$y_var#
			tmp$variable<-input$y_var#ui.opts$variable()#input$variable#
			tmp$x_variable<-input$x_var
			tmp$group<-input$group_var#
			tmp$plot.type<-input$plot_type#"density_plot"
			tmp$size.mapping<-input$size_mapping
			tmp$size<- input$size
			tmp$size.variable<- input$size_variable
			tmp$alpha<- input$alpha
			tmp$show.points<-input$show.points	
			
			#plot
			output$return.plot<-renderPlot({
				#plot.basic_plot(tmp)
				make.plot(obj = tmp)	
			})	
			#print something
			output$summary <- renderPrint({
			str(tmp$data)
		})
}

#set and run args
set.args<-function(obj,tmp , input , output){
	# type<-obj$analysis.type
	do.call(paste0("args.",obj),list(tmp = tmp, input = input, output = output))
}

#switching plot
make.plot<-function(obj){
	# type<-obj$analysis.type
	do.call(paste0("plot.",obj$plot.type),list(plot.obj = obj))
}

plot.box_plot<-plot.bar_plot<-plot.density_plot<-plot.histogram<-plot.scatter_plot<-function(plot.obj,...){
	
	# need:
	# data
	# variable
	# group
	# plot.type
	# show.points
	
	# dist <- plot.obj$data
	# dist<-dist[,colnames(dist)%in%plot.obj$variable, drop=FALSE]
    # plot(dist)
	type<-as.character(plot.obj$plot.type)
	tmp.data<-data.frame(group=plot.obj$data[,colnames(plot.obj$data)%in%plot.obj$group], variable=plot.obj$data[,colnames(plot.obj$data)%in%plot.obj$variable], 
							x_variable =plot.obj$data[,colnames(plot.obj$data)%in%plot.obj$x_variable], 
							size =plot.obj$data[,colnames(plot.obj$data)%in%plot.obj$size.variable])
							
	
	#dynamic plotting options
	plot.type<-switch(type,
			"box_plot" 		= 	.local<-function(tmp){geom_boxplot()},
			"histogram" 	=	.local<-function(tmp){geom_histogram(alpha=plot.obj$alpha,position="identity")},
			"density_plot" 	=	.local<-function(tmp){geom_density(alpha=plot.obj$alpha)},
			"bar_plot" 		=	.local<-function(tmp){geom_bar(alpha=plot.obj$alpha,position="dodge")}, # width is not working
			"scatter_plot"  =	.local<-function(tmp){
									if(tmp$size.mapping=="absolute"){
										geom_point(alpha=plot.obj$alpha, size=plot.obj$size)
									} else {
										geom_point(alpha=plot.obj$alpha, aes(size=as.factor(size)))
									}
								}	
		)

	plot.type<-plot.type(plot.obj)
	
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
					y 		= variable,
					fill 	= as.factor(group)
					)
				) + plot.type
				
				if(plot.obj$show.points==TRUE)
				{ 
					p<-p+ geom_point(color='black',alpha=plot.obj$alpha, position = 'jitter')
				}
				
		} else {
		
		p<-ggplot(tmp.data, 
				aes(
					x 		= variable,
					fill 	= as.factor(group)#,
					# group 	= as.factor(group),
					# color 	= as.factor(plot.obj$group)
					)
				) + plot.type
		}
	
		if(type=="scatter_plot"){
			p<-ggplot(tmp.data, 
				aes(
					x 		= x_variable,
					y 		= variable,
					size 	= size,	
					# fill 	= group#,
					# group 	= as.factor(group),
					color 	= as.factor(group)
					)
				) + plot.type
			size.lab<-if(is.null(plot.obj$size.variable))	{NULL} else {plot.obj$size.variable}
			labels<-labs(
				color 	= plot.obj$group,
				x 		= plot.obj$group,
				y 		= plot.obj$variable,
				size 	= size.lab
			)  
		} else {
			labels<-labs(
					fill 	= plot.obj$group,
					x 		= "",
					y 		= plot.obj$variable
				)  
		}
	 p<- p+ labels +
	.theme
	print(p)
}

