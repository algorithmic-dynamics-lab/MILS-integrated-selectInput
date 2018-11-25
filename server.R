require("igraph")
source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/compressionLength.R")
source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")
source("scripts/listEdges.R")
source("scripts/matrixPlot.R")
source("scripts/unnameGraph.R")
source("scripts/simultaneousAttackOnStrings.R")


shinyServer(function(input, output, session) {
  
  ## MILS 2D tab
  # the names/indices of vertices for the graph are set at loadGraph
  g <- loadGraph("./data/starGraphAdjMatrix.csv")
  
  originalG <- g
  
  reducedG <- g
  
  lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, 
                                                                   blockSize=4, 
                                                                   offset = 1))
  
  lossEdges <- correctLossRanking(calculateLossByEdgeDeletion(g, 
                                                              blockSize = 4, 
                                                              offset = 1))
  
  deletionsCounter <- as.integer(1)

  reactiveData <- reactiveValues(g = g,
                                 reducedG = reducedG, 
                                 originalG = originalG,
                                 lossVertices = lossVertices,
                                 lossEdges = lossEdges,
                                 deletionsCounter = deletionsCounter)
  
  observeEvent(input$swapGraphsButton, {
    
    reactiveData$g <- reactiveData$reducedG
    
  })
  
  observeEvent(input$resetGraphsButton,{
    
    reactiveData$g <- reactiveData$originalG
    reactiveData$reducedG <- reactiveData$originalG
    
  })
  
  
  observeEvent(input$file1, {
    
    inFile <- input$file1
    
    if (is.null(inFile$datapath)){
      
      
    } else {
      
      reactiveData$lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, 4, 1))
      reactiveData$lossEdges    <- correctLossRanking(calculateLossByEdgeDeletion(g, 4, 1))
      reactiveData$g            <- loadGraph(inFile$datapath)
      reactiveData$reducedG     <- reactiveData$g
      reactiveData$originalG    <- reactiveData$g
      
      reactiveData$deletionsCounter <- as.integer(1)
      
    }
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$numberOfElements, {

    elems <- 0
    vertices <- c()
    edges    <- c()
    
    if(input$numberOfElements!=0){
    
      if(input$elementsToDelete == "vertices"){ 
          
         elems <- vcount(reactiveData$g)
         
         verticesToDelete <- reactiveData$lossVertices$name
         
         reactiveData$reducedG <- delete_vertices(reactiveData$g, 
                                                  verticesToDelete[1:input$numberOfElements])
      } 
      
      if(input$elementsToDelete == "edges") { 
      
          elems <- ecount(reactiveData$g)
          
          edgesToDelete <- formatEdgesForDeletion(reactiveData$lossEdges)
          
          reactiveData$reducedG <- delete_edges(reactiveData$g, 
                                                edgesToDelete[1:input$numberOfElements])
         
      }
    }
    
    # updateSliderInput(session,
    #                   "numberOfElements",
    #                   max = elems-1,
    #                   step = 1)
    
    updateSelectInput(session = session, 
                      inputId = "numberOfElements",                      
                      choices = c(1:(elems-1))
                      )
    
  }, ignoreNULL = FALSE)
  
  
  
  output$graphPlot <- renderPlot({
    
    if(input$showAdjacencyMatrix==TRUE){
      
      plotAdjMatrix(unnameGraph(reactiveData$g))

    }
    
    else{
    coords <- layout_(reactiveData$g, as_star())
    
    plot(reactiveData$g,
         layout = coords,
         edge.arrow.size = 0.5,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    }
  }) 

  output$reducedGraphPlot <- renderPlot({
    
    if(input$showAdjacencyMatrix==TRUE){
      
      plotAdjMatrix(unnameGraph(reactiveData$reducedG))
      
    }
    
    else{
    
      coords <- layout_(reactiveData$reducedG, as_star())
    
      plot(reactiveData$reducedG,
           layout = coords,
           edge.arrow.size = 0.5,
           vertex.size = 25,
           vertex.label.family = "Arial Black")
    }
  }) 
  
  ########### 
  ### MILS 1D
  
  observeEvent(input$blockSize1D, {
    updateSliderInput(session,
                      "blockOverlap",
                      max = input$blockSize - 1)
  })
  
  observeEvent(input$insertString, {
    updateSliderInput(session,
                      "nReduced",
                      max = nchar(input$insertString) - 2)
  })
  
  output$origStr <- renderText({
    
    input$evalButton
    isolate({
      paste0("Original string = ", input$insertString)
      
    })
    
  })
  
  output$mutateStr <- renderText({
    
    input$evalButton
    isolate({
      paste0("Reduced string with minimal algorithmic loss = ", 
             simultaneousAttackOnString(input$insertString, 
                                        blockSize= input$blockSize, 
                                        offset = (input$blockSize - input$blockOverlap), 
                                        input$alphabet, 
                                        input$nReduced, 
                                        FALSE)
      )
      
    })
    
  })
  
  ##### BDM 1D
  observeEvent(input$blockSize1D, {
    updateSliderInput(session,
                      "blockOverlap",
                      max = input$blockSize1D - 1)
  })
  
  observeEvent(input$insertString, {
    updateSliderInput(session,
                      "nReduced",
                      max = nchar(input$insertString) - 2)
  })
  
  output$origStr <- renderText({
    
    input$evalStringButton
    isolate({
      paste0("Original string = ", input$insertString)
      
    })
    
  })
  
  output$mutateStr <- renderText({
    
    input$evalStringButton
    isolate({
      paste0("Reduced string with minimal algorithmic information loss = ", 
             simultaneousAttackOnString(input$insertString, 
                                        blockSize= input$blockSize1D, 
                                        offset = (input$blockSize1D - input$blockOverlap), 
                                        input$alphabet, 
                                        input$nReduced, 
                                        FALSE)
      )
      
    })
    
  })
  
})
