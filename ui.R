library(shiny)
library("shinythemes")

orange_slider <- ".irs-bar,
                  .irs-bar-edge,
                  .irs-single,
                  .irs-grid-pol {
                    background: #f63;
                    border-color: #f63;
                  }"

shinyUI(
  fluidPage(
    theme = shinytheme("united"),
    tags$style(orange_slider),
    
    sidebarLayout(
      
      column(6, 
             tabsetPanel(
               tabPanel("For networks",
                        value = 1,
                        
                        h3("MILS for Networks"),
                        
                        div(
                          wellPanel(
                            
                            fileInput(inputId = "file1",
                                      label = "Load a CSV file",
                                      accept = c('text/comma-separated-values',
                                                 'text/plain',
                                                 'text/csv',
                                                 '.csv')
                            ),
                            
                            checkboxInput(inputId = "show_adjacency_matrix", 
                                          label = "Show adjacency matrices", 
                                          value = FALSE, width = NULL),
                            
                            hr(),
                            radioButtons(inputId = "elements_to_delete", 
                                         label = "Elements to delete",
                                         choices = c("vertices", "edges"),
                                         selected = "vertices"),
                            
                            selectInput(inputId = "number_of_elements",
                                        label = "Number of elements to delete",
                                        choices = c(1:3)),
                            
                            hr(),
                            actionButton(inputId = "swap_graphs_button",
                                         label  ="Update evaluated graph",
                                         width = "45%"),
                            hr(),
                            actionButton(inputId="reset_graphs_button",
                                         label = "Reset evaluated graph",
                                         width = "45%")
                          )
                        )
                      ), #end "For networks" input tabpanel
               
               tabPanel("For images",
                        value = 2,
                        
                        h3("MILS for Images"),
                        
                        div(
                          
                          wellPanel(
                              
                            fileInput(inputId = "file2",
                                      label = "Load an image",
                                      accept = c('image/jpeg',
                                                 'image/tiff',
                                                 'image/png')), 
                            
                            radioButtons(inputId = "image_elements", 
                                         label = "Elements to delete",
                                         choices = c("rows", "columns"),
                                         selected = "rows"),
                            
                             sliderInput(inputId="number_of_reductions",
                                         label = "Number of reductions to be performed",
                                         min = 1, max = 150, value = 1, step = 1)
                            
                          )
                          
                        )
                        
               ),
               
               tabPanel("For strings",
                        value = 3,
                        
                        h3("MILS for Strings"),
                        
                        div(
                          
                          wellPanel(
                            
                            textInput(inputId = "insert_string",
                                      label = "Enter a string",
                                      value = "110001101010111101"),
                            
                            sliderInput(inputId = "n_reduced",
                                        label = "Number of reduced bits",
                                        min = 1, max = 10, value = 10, step = 1),
                            
                            sliderInput(inputId = "block_size_1D",
                                        label = "Block size",
                                        min = 2, max = 12, value = 12),
                            
                            sliderInput(inputId = "block_overlap",
                                        label = "Block overlap",
                                        min = 0, max = 11, value = 11),
                            
                            
                            radioButtons(inputId = "alphabet",
                                         label = "Alphabet size",
                                         inline = TRUE,
                                         choices = list("2" = 2,
                                                        "4" = 4,
                                                        "5" = 5,
                                                        "6" = 6,
                                                        "9" = 9),
                                         selected = 2),
                            
                            
                            radioButtons(inputId = "difference_type",
                                         label = "Element removal",
                                         inline = TRUE,
                                         choices = list("From absolute neutral" = "orig",
                                                        "From closest to median" = "seq"),
                                         selected = "orig"),
                            
                            actionButton("eval_string_button", "Evaluate", 
                                         style = "color: #fff; background-color: #f63; 
                                                  border-color: #f63"))
                          
                          
                        )
                        
                        ),
               
               id = "conditionedPanels")
      ),
      
      mainPanel(
        withMathJax(),
        # Output for graphs
        conditionalPanel(condition = "input.conditionedPanels == 1",
                         
                         br(),
                         fluidRow(
                          column(width = 5, 
                                 h4("Original Graph", align = "center"),
                                 plotOutput("graph_plot")),
                          column(width = 5, 
                                 h4("Reduced Graph", align = "center"),
                                 plotOutput("reduced_graph_plot"))
                         )
                        ),
        
        # Output for images
        conditionalPanel(condition = "input.conditionedPanels == 2",
                         
                         br(),
                         
                         fluidRow(
                           column(
                             width = 5,
                             h4("Original Image", align = "center"),
                             plotOutput("image_plot")),
                           column(
                             width = 5,
                             h4("Reduction Map", align = "center"),
                             plotOutput("reduction_map_plot"))
                         ),
                          
                         hr(),
                         
                         fluidRow(
                           column(width = 10,
                             h3("Reduced Image", align = "center"),
                             plotOutput("reduced_image_plot")
                           )
                         )
        ),
        
        # Output for strings
        conditionalPanel(condition = "input.conditionedPanels == 3",
                         
                         br(),
                         
                         fluidRow(
                           column(width = 5,
                                  br(),
                                  
                                  h4("Original String", align = "center"),
                                  
                                  br(),
                                  
                                  div(p(textOutput(outputId = "orig_str")),
                                      style = "font-size:120%",
                                      align = "center")
                                  ),
                                  
                           column(width=5, 
                                  
                                  br(),
                                  
                                  h4("Reduced String", align="center"),
                                  
                                  br(),
                                  
                                  div(p(textOutput(outputId = "mutate_str")),
                                      style = "font-size:120%",
                                      align = "center"))
                         )
                         
      )  
    )
   )
  )
)

  