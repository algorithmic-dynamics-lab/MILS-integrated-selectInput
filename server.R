require("igraph")
require("OpenImageR")
source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/loadGraph.R")
source("scripts/listEdges.R")
source("scripts/matrixPlot.R")
source("scripts/unnameGraph.R")
source("scripts/reduceImage.R")
source("scripts/compressionLength.R")
source("scripts/edgeAndVertexKnockout.R")
source("scripts/simultaneousAttackOnStrings.R")


shinyServer(function(input, output, session) {
  
  #######################
  ### MILS FOR GRAPHS ###
  #######################
  
  # the names/indices of vertices for the graph are set at load_graph
  g <- load_graph("./data/starGraphAdjMatrix.csv")
  g <- set_vertex_attr(g, "name", value = seq(1:length(V(g))))
  
  g_original <- g
  g_reduced <- g
  
  loss_vertices <- correct_loss_ranking(calculate_loss_by_vertex(g, 
                                                                   block_size = 4, 
                                                                   offset = 1))
  
  loss_edges <- correct_loss_ranking(calculate_loss_by_edge(g, 
                                                              block_size = 4, 
                                                              offset = 1))
  
  deletions_counter <- as.integer(1)

  reactive_data <- reactiveValues(g = g,
                                  g_reduced = g_reduced, 
                                  g_original = g_original,
                                  loss_vertices = loss_vertices,
                                  loss_edges = loss_edges,
                                  deletions_counter = deletions_counter)
  
  observeEvent(input$swap_graphs_button, {
    
    reactive_data$g <- reactive_data$g_reduced
    
  })
  
  observeEvent(input$reset_graphs_button, {
    
    reactive_data$g <- reactive_data$g_original
    #reactive_data$g_reduced <- reactive_data$g_original

  })
  
  
  observeEvent(input$file1, {
    
    in_file <- input$file1
    
    if (is.null(in_file$datapath)) {
      
      
    } else {
      
      reactive_data$loss_vertices <- correct_loss_ranking(calculate_loss_by_vertex(reactive_data$g, 4, 1))
      reactive_data$loss_edges <- correct_loss_ranking(calculate_loss_by_edge(reactive_data$g, 4, 1))
      reactive_data$g <- load_graph(in_file$datapath)
      reactive_data$g_original <- reactive_data$g
      reactive_data$g_reduced <- reactive_data$g
      
      reactive_data$deletions_counter <- as.integer(1)
      
    }
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$number_of_elements, {

    elems <- 0
    
    if (input$number_of_elements != 0) {
    
      if (input$elements_to_delete == "vertices") { 
          
         elems <- vcount(reactive_data$g)
         
         verticesToDelete <- reactive_data$loss_vertices$name
         
         reactive_data$g_reduced <- delete_vertices(reactive_data$g, 
                                                  verticesToDelete[1:input$number_of_elements])
         
      } 
      
      if (input$elements_to_delete == "edges") { 
      
          elems <- ecount(reactive_data$g)
          
          edges_to_delete <- format_edges(reactive_data$loss_edges)
          
          reactive_data$g_reduced <- delete_edges(reactive_data$g, 
                                                edges_to_delete[1:input$number_of_elements])
         
      }
    }
    
    updateSelectInput(session = session,
                      inputId = "number_of_elements",                      
                      choices = c(1:(elems-1)),
                      selected = input$number_of_elements)
    
  }, ignoreNULL = FALSE)
  
  
  
  output$graph_plot <- renderPlot({
    
    if (input$show_adjacency_matrix == TRUE) {
      
      plot_adj_matrix(unname_graph(reactive_data$g))

    }
    
    else{
    coords <- layout_(reactive_data$g, as_star())
    
    plot(reactive_data$g,
         layout = coords,
         edge.arrow.size = 0.5,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    }
  }) 

  output$reduced_graph_plot <- renderPlot({
    
    if (input$show_adjacency_matrix == TRUE) {
      
      plot_adj_matrix(unname_graph(reactive_data$g_reduced))
      
    } else {
    
      coords <- layout_(reactive_data$g_reduced, as_star())
    
      plot(reactive_data$g_reduced,
           layout = coords,
           edge.arrow.size = 0.5,
           vertex.size = 25,
           vertex.label.family = "Arial Black")
    }
  }) 
  
  
  #######################
  ### MILS FOR IMAGES ###
  #######################
  
  path <- 'data/images/1.png'
  read_image <- readImage(path)
  
  im <- path %>% readImage %>% 
        resizeImage(width = 150, height = 150, method = 'bilinear')
  
  im_reduced <- reduce_image(im, 4, 4, 50)
  
  react_image <- reactiveValues(im = im, im_reduced = im_reduced)
  
  observeEvent(input$file2, {
    in_file <- input$file2
    
    if (is.null(in_file$datapath)) {
      
    } else {
      react_image$im <- readImage(in_file$datapath)
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$number_of_reductions, {
    
    elems <- 0
    
    if (input$number_of_reductions != 0) {
    
      if (input$image_elements == 'rows') {
        elems <- nrow(react_image$im)
      }
      
      if (input$image_elements == 'columns') {
        elems <- ncol(react_image$im)
      }
    }
      
    updateSliderInput(session, "number_of_reductions",
                      max = elems-1, step = 1)
    
  })
  
  output$image_plot <- renderPlot({
    imageShow(react_image$im)
  })
  
  output$reduction_map_plot <- renderPlot({
    react_image$im_reduced <- reduce_image(im = react_image$im, 
                                           block_size = 4, 
                                           offset = 4, 
                                           num = input$number_of_reductions, 
                                           what = input$image_elements)
    
    imageShow(react_image$im_reduced$coloring)
  })
  
  output$reduced_image_plot <- renderPlot({
    imageShow(react_image$im_reduced$reduction)
  })
  
  ########################
  ### MILS FOR STRINGS ###
  ########################
  
  observeEvent(input$block_size_1D, {
    updateSliderInput(session,
                      "block_overlap",
                      max = input$block_size_1D - 1)
  })
  
  observeEvent(input$insert_string, {
    updateSliderInput(session,
                      "n_reduced",
                      max = nchar(input$insert_string) - 2)
  })
  
  output$orig_str <- renderText({
    
    input$evalButton
    isolate({
      paste0("Original string = ", input$insert_string)
      
    })
    
  })
  
  output$mutate_str <- renderText({
    
    input$evalButton
    isolate({
      paste0("Reduced string with minimal algorithmic loss = ", 
             simultaneous_attack_on_string(input$insert_string, 
                                           block_size = input$block_size_1D, 
                                        offset = (input$block_size - input$block_overlap), 
                                        input$alphabet, 
                                        input$n_reduced, 
                                        FALSE)
      )
      
    })
    
  })
  
  ##### BDM 1D
  observeEvent(input$block_size_1D, {
    updateSliderInput(session,
                      "block_overlap",
                      max = input$block_size_1D - 1)
  })
  
  observeEvent(input$insert_string, {
    updateSliderInput(session,
                      "n_reduced",
                      max = nchar(input$insert_string) - 2)
  })
  
  output$orig_str <- renderText({
    
    input$eval_string_button
    isolate({
      paste0(input$insert_string)
      
    })
    
  })
  
  output$mutate_str <- renderText({
    
    input$eval_string_button
    isolate({
      paste0(simultaneous_attack_on_string(input$insert_string, 
                                        block_size= input$block_size_1D, 
                                        offset = (input$block_size_1D - input$block_overlap), 
                                        input$alphabet, 
                                        input$n_reduced, 
                                        FALSE)
      )
      
    })
    
  })
  
})
