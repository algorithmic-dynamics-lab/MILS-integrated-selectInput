library(shiny)
library("shinythemes")

orange_slider <- "
.irs-bar,
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
                                      label = "Choose a CSV file",
                                      accept = c('text/comma-separated-values',
                                                 'text/plain',
                                                 'text/csv',
                                                 '.csv')
                            ),
                            
                            checkboxInput(inputId = "showAdjacencyMatrix", 
                                          label = "Show adjacency matrices", 
                                          value = FALSE, width = NULL),
                            
                            hr(),
                            radioButtons(inputId="elementsToDelete", 
                                         label = "Elements to delete",
                                         choices = c("vertices", "edges"),
                                         selected = "vertices"),
                            
                            # updated on server
                            # sliderInput(inputId="numberOfElements",
                            #             label = "Number of elements to delete",
                            #             min = 1, 
                            #             max = 10, value = 1, step = 1
                            #             ),
                            
                            selectInput(inputId="numberOfElements",
                                        label = "Number of elements to delete",
                                        choices = c(1:3),
                                        selected = 1),
                            
                            hr(),
                            actionButton(inputId="swapGraphsButton",
                                         label  ="Update evaluated graph",
                                         width = "45%"),
                            hr(),
                            actionButton(inputId="resetGraphsButton",
                                         label = "Reset evaluated graph",
                                         width = "45%")
                          )
                        )
                      ), #end "For networks" input tabpanel
               
               tabPanel("For strings",
                        value=2,
                        
                        h3("MILS for Strings"),
                        
                        div(
                          
                          wellPanel(
                            
                            textInput(inputId = "insertString",
                                      label = "Enter a string",
                                      value = "110001101010111101"),
                            
                            sliderInput(inputId = "nReduced",
                                        label = "Number of reduced bits",
                                        min = 1, max = 10, value = 10, step = 1),
                            
                            sliderInput(inputId = "blockSize1D",
                                        label = "Block size",
                                        min = 2, max = 12, value = 12),
                            
                            sliderInput(inputId = "blockOverlap",
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
                            
                            
                            radioButtons(inputId = "differenceType",
                                         label = "Element removal",
                                         inline = TRUE,
                                         choices = list("From absolute neutral" = "orig",
                                                        "From closest to median" = "seq"),
                                         selected = "orig"),
                            
                            #,
                            
                            
                            #radioButtons(inputId = "reductionMethod",
                            #             label = "Reduction method",
                            #             inline = TRUE,
                            #             choices = list("Simultaneous" = "sim",
                            #                            "Sequential" = "seq"),
                            #             selected = "sim"),
                            
                            
                            actionButton("evalStringButton", "Evaluate", 
                                         style = "color: #fff; background-color: #f63; 
                                         border-color: #f63"))
                          
                          
                        )
                        
                        ),#end "For strings" input tabpanel
               id = "conditionedPanels"
             )
      ),
      
      mainPanel(
        withMathJax(),
        #Output for graphs
        conditionalPanel(condition="input.conditionedPanels==1",
                         
                         br(),
                         fluidRow(
                          column(width = 5, 
                                 h3("Original Graph", align="center"),
                                 plotOutput("graphPlot")),
                          column(width = 5, 
                                 h3("Reduced Graph", align="center"),
                                 plotOutput("reducedGraphPlot"))
                         )
                        ), # end conditionalPanel
        
        #Output for strings
        conditionalPanel(condition="input.conditionedPanels==2",
                         
                         br(),
                         
                         fluidRow(
                           column(width=5,
                                  br(),
                                  
                                  h3("Original String", align="center"),
                                  
                                  br(),
                                  
                                  div(p(textOutput(outputId = "origStr")),
                                      style = "font-size:120%",
                                      align = "center")
                                  ),
                                  
                           column(width=5, 
                                  
                                  br(),
                                  
                                  h3("Reduced String", align="center"),
                                  
                                  br(),
                                  
                                  div(p(textOutput(outputId = "mutateStr")),
                                      style = "font-size:120%",
                                      align = "center"))
                         )
                         
      )  
    )
   )
  )
)#end shinyUI

  