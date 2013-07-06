
# UI for app
shinyUI(pageWithSidebar(
	# title
	headerPanel("Bring da Heat."),
	
	#input
	sidebarPanel
	(
		h3("Data Input"),
		selectInput("data","Data Set", 
						data.options()),	

		radioButtons("dimention","Dimension", 
						c("rows","columns"), selected = "rows"),
		selectInput("type","Transform", 
						list("Z-scale" = "none", "Spearmans correlation" = "spearman", "Pearsons correlation" = "pearson","Biweight correlation" = "biweight")),	
		h3("Clustering Options"),				
		selectInput("distance","Distance", 
						c("none","euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski"), selected = "minkowski"),
						
		selectInput("linkage","Linkage",
						c("none","ward", "single", "complete", "average", "mcquitty", "median" , "centroid"),selected = "ward"),
		h3("Annotation Options"),				
		selectInput("groups","Group:", "NULL", multiple =TRUE),		
		#uiOutput("groups"),
		checkboxInput("names","Names",TRUE),
		checkboxInput("border","Border",TRUE)	,
		h3("Heatmap Colors"),
		selectInput("low.col","low", color.opts(), selected = "green" ),
		selectInput("mid.col","mid", color.opts(), selected = "black"),
		selectInput("high.col","high:", color.opts(),selected = "red"),
		h3("Dendrogram groups"),
		uiOutput("HCA.groups"), 
		helpText(
		"Made in the USA+",
		a(href="http://www.example.com ", target="_blank", "Example"))
	),

	# output				
	mainPanel(
		h3(textOutput('caption')),
		tabsetPanel(
		tabPanel("Heatmap",plotOutput("heatmap",height = 560*1.25, width = 560*1.25)),
		tabPanel("Dendrogram",plotOutput("dendrogram",height = 560*1.25, width = 560*1.25)),
		tabPanel("Data",verbatimTextOutput("summary"))
			# would like to add this to the dendrogram panel uiOutput("HCA.groups") 
			)
	)
))
