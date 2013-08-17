
	# UI for app
shinyUI(pageWithSidebar(
  # title
  headerPanel("MetaMapR"),
  
  # input
  sidebarPanel
  (
		wellPanel(
					h3('Data'),
					uiOutput("datasets"),
					tags$details(
					withTags(div(class='row-fluid',
						 div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
						 div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE)))
						 ),
					HTML("<label>Load data: (.rda | .csv | .sav | .dta)</label>"),
					uiOutput("upload_local_server"),
					HTML("<label>Paste data:</label>"),
					tags$textarea(id="copyAndPaste", rows=3, cols=40, ""))
		),
		wellPanel(				
					h3('Network Options'),
					# tags$details(
						uiOutput("network_index_info_bio"),
						uiOutput("network_index_info_chem"),
						uiOutput("network_index_info_spec"),
						uiOutput("network_index_info_cor"),
					# ),
					tags$details(checkboxInput(inputId = "unique_edges", label = "Unique edges?",value=FALSE))
					# selectInput(inputId = "edge_type", label = "Edge Type:", choices = 
					# # list( "Biochemical" = "kegg", "Chemical" = "tanimoto", "Spectral" = "ms") , selected = "Biochemical", multiple = TRUE),
					# conditionalPanel(condition = "input.edge_type != 'kegg'",
						# numericInput(inputId = "tanimoto_cutoff" , "cutoff", value = 0.7, min = 0, max = 1, step = .005)
					# ),
					# conditionalPanel(condition = "input.edge_type[1] == 'tanimoto'",
						# checkboxInput(inputId = "unique_edges", label = "Unique edges?",value=TRUE)
					# )#,
					# br()#, 
					# actionButton("calculate_edges", "Calculate") # only calculates when on edge list tab and needs a controller to not go fully reactive
					# submitButton("Calculate") # holds up everything...which is bad
				)
	),
				
  mainPanel(
	# h3('caption'),
	tabsetPanel( id = "metabomapr",
		tabPanel("Data", tableOutput("view_data")),
		tabPanel("Edge List", list(actionButton("create_edgelist", "Calculate"),
		br(),
		tableOutput("edge_list"))),
		tabPanel("Node Attributes",tableOutput("node.attributes")),
		tabPanel("Network", plotOutput("network",width = 850, height = 650)),#height = "100%"
		tabPanel("Debug", verbatimTextOutput("debug"))),
		conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
			id='progressIndicator',
			"Calculating...",
			div(id='progress',includeHTML("www/js/timer.js"))
		),
		tags$head(tags$style(type="text/css",
		  '#progressIndicator {',
		  '  position: fixed; top: 8px; center: 8px; width: 200px; height: 50px;',
		  '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
		  '}'
		))
	)
))