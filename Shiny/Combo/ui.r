# UI for app
shinyUI(pageWithSidebar(
  # title
  headerPanel("Select Options"),
  
  #input
  sidebarPanel
  (
    h3("Data"),
	radioButtons("input_type","Input", 
				c("demo","load"), 
				selected = "demo"
	),
				
	conditionalPanel(
		condition = "input.input_type == 'demo'",		
		selectInput("demo.data","Data Set", 
					data.options()
		)
	),
	
	conditionalPanel(
		condition = "input.input_type == 'load'",		
		fileInput("load.data", "Choose .csv File", multiple=TRUE)
	),
	
	# # # Plot types (make dynamic to analysis type, not sure why this can't be sourced from elsewhere)
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
		#uiOutput("plot_type"),
		# conditionalPanel(
				# condition = "input.plot_type == 'box_plot'",		
				# box_plot.ui()
		# ),
		# uiOutput("plot_type")#,
		uiOutput("selected.ui") 
	  )
	),
	
  # # output				
  mainPanel(
	h3(textOutput('caption')),      
	#Dynamic plot
	plotOutput("return.plot",width = 900, height = 700),
	verbatimTextOutput("summary")
    )
))

