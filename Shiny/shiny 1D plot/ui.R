
# # UI for app
# shinyUI(pageWithSidebar(
	# # title
	# headerPanel("Select Options"),
	
	# #input
	# sidebarPanel
	# (
		# selectInput("dataset","Data:", 
						# list(iris = "iris", mtcars = "mtcars")
						# ),
		# # uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
		# conditionalPanel(
		# condition = "input.dataset == 'iris'",	
		# checkboxInput("show.points", "show points", TRUE),		
		# uiOutput("variable") 
		# ),
		
		# conditionalPanel(
		# condition = "input.dataset == 'mtcars'",		
		# # uiOutput("mtcars.ui")
		# checkboxInput("tt", "showtnts", TRUE)
		# ),
		
		# # depends on dataset	( set by output$group in  server.R)
		# selectInput("plot.type","Plot Type:", 
						# list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
						# ),
		# checkboxInput("show.points", "show points", TRUE),
		# uiOutput("iris.ui") 
	# ),	

	# # output				
	# mainPanel(
		# h3(textOutput("caption")),
		# #h3(htmlOutput("caption")),
		# uiOutput("plot") # depends on input 
	# )
# ))

# UI for app
shinyUI(pageWithSidebar(
	# title
	headerPanel("Select Options"),
	
	#input
	sidebarPanel
	(
		selectInput("dataset","Data:", 
						list(iris = "iris", mtcars = "mtcars")
						),
		conditionalPanel(
		condition = "input.dataset == 'iris'",				
		# selectInput("variable","Variable:", "Loading...")
		uiOutput("iris.ui")
		),
		conditionalPanel(
		condition = "input.dataset == 'mtcars'",				
			selectInput("group","Group:", "Loading...")
		),
		selectInput("plot.type","Plot Type:", 
						list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
						),
		checkboxInput("show.points", "show points", TRUE)
	),	
 
	# output				
	mainPanel(
		h3(textOutput("caption")),
		#h3(htmlOutput("caption")),
		uiOutput("plot") # depends on input 
	)
))