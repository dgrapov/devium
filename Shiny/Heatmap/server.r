
shinyServer(function(input, output, session) {
  
  
  #set reactive globals
  if(!exists("ui.opts")){ui.opts<-list()}
  
  #set dimension
  ui.opts$match.dim<-reactive({
    ui.opts$match.dim<-ifelse(input$dimention=="rows",TRUE,FALSE)
    ui.opts$match.dim
  })
  
  
  #data
  ui.opts$data<-reactive({
    #     tmp.data<-rdy.t(get(input$data)) # does not work always?
    tmp.data<-get(input$data)
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
    
    #remove factors
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
    tmp.data<-tmp.data[,!fct.id, drop=F]
    
    if(ui.opts$match.dim()){
      tmp.data<-data.frame(t(data.frame(tmp.data)))
    } 
    ui.opts$data<-tmp.data
    tmp.data
  })
  
  #top panel annotation
  ui.opts$group<-reactive({
    if(is.null(input$groups)) {
      ui.opts$group<-NULL
    } else {  
      tmp.data<-rdy.t(get(input$data))
      #       colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
      #       rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
      tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
      fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
      #get object for top annotation
      if(ui.opts$match.dim()){
        class.factor<-as.data.frame(tmp.data[,colnames(tmp.data)%in%input$groups,drop=F]) # mind f$ck
      } else {
        tmp.data<-data.frame(t(data.frame(tmp.data)))
        class.factor<-as.data.frame(tmp.data[,colnames(tmp.data)%in%input$groups,drop=F])[!fct.id,,drop=F]
      }
      
      ui.opts$group<-class.factor
      class.factor
    }
  }) 
  
  #group
  observe({
    tmp<-rdy.t(get(input$data))
    #     colnames(tmp)<-check.fix.names(colnames(tmp),ok.chars=c(".","_") )
    #     rownames(tmp)<-check.fix.names(rownames(tmp),ok.chars=c(".","_") )
    #     tmp<-rdy.t(tmp) # doesn't always work?
    if(ui.opts$match.dim()){
      group.opts<-colnames(tmp)
    } else {
      group.opts<-rownames(tmp)
    }
    # SelectInput("groups","Group", namel(group.opts))
    updateSelectInput(session, "groups", choices = group.opts)
  }) 
  
  
  #caption
  output$caption<-renderText({c(input$groups)})
  
  
  #debugging
  output$summary <- renderPrint({
    match.dim<- ifelse(ui.opts$match.dim(),1,2) 
    #data inputs (need to flip to match heat map function)
    tmp.data<-ui.opts$data()
    class.factor<-ui.opts$group()
    tmp<-list()
    tmp$data<- tmp.data#ui.opts$data() #ui.opts$group()
    tmp$match.dim<-match.dim
    tmp$class.factor<-class.factor  
    tmp$dimnames<-dimnames(tmp.data)
    str(tmp)
  })
  
  #make heatmap
  output$heatmap <- renderPlot({
    
    
    # set objects
    match.dim<- ifelse(ui.opts$match.dim(),1,2) # keep static ifelse(ui.opts$match.dim(),1,2)
    #data inputs (need to flip to match heat map function)
    if(ui.opts$match.dim()) {tmp.data<-data.frame(t(ui.opts$data()))} else {tmp.data<-ui.opts$data()}
    #     if(ui.opts$match.dim) {tmp.data<-data.frame(t(ui.opts$data))} else {tmp.data<-ui.opts$data}
    class.factor<-ui.opts$group()
    
    #another attempt in rownames saga
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    
    #UI inputs
    type <-input$type
    show.names <-input$names
    border.color <-ifelse(input$border==TRUE,"gray40",FALSE)
    
    
    #avoid clustering w/ partial args (could auto set patial defaults for anything = "none")
    if(input$linkage=="none"||input$distance=="none"){
      cluster.method<- distance.method <-"none"
    } else {
      cluster.method 	<- input$linkage
      distance.method <- input$distance
    }
    
    #colors
    heatmap.color<-c(input$low.col,input$mid.col,input$high.col)
    
    #plot heatmap 
    devium.heatmap(
      data 			      = tmp.data, 
      match.dim 		  = match.dim,
      type 			      = type,
      class.factor 	  = class.factor,
      class.color 	  = NULL,
      heatmap.color	  = heatmap.color,
      cluster.method 	= cluster.method, 
      distance.method = distance.method,
      font.size 		  = 12,
      border.color	  = border.color,
      show.names 		  = show.names)
  })	
  
  # #condition slider max on dim[2]				
  output$HCA.groups <- renderUI({
   
    #data
    tmp.data<-data.frame(t(ui.opts$data()))
  
    
    ymax <-nrow(tmp.data) # opposite of usual 
    nclust<-tryCatch(input$HCA.groups, error= function(e) {NULL})
    # avoiding errors in stats
    if(is.null(nclust)){nclust<-3}
    if(nclust < 2) {nclust<-3}
    if(nclust>ymax) {nclust<-ymax-1}
    
    sliderInput(inputId = "HCA.groups","Number of Clusters",min= 3, max = ymax, value = nclust, step = 1)
  })
  
  ##dendrogram		
  output$dendrogram <- renderPlot({	
    
    if(!input$linkage=="none"||!input$distance=="none"){
      
      #data
      tmp.data<-data.frame(t(ui.opts$data())) 
    
      
      #clustering
      distance<-dist(tmp.data, method = input$distance ) # euclidean
      clusters<-hclust(distance, method = input$linkage)
      

      #number of clusters
      nclust<-input$HCA.groups
  
      # function from http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R
      A2Rplot(clusters, k=nclust, boxes = FALSE,col.up = "gray50", col.down = rainbow(nclust), main="")
      
      # to do
      # get selected clusters and add to annotation options
      # HCA.clusters<<--cutree(clusters, k=nclust)
      # # add to data for heatmap vis and vice versa
      
      
    }
    
  })		
})
