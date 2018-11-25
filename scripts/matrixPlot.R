require("igraph")
require("ggplot2")
require("reshape2")

plotAdjMatrix <- function(graphToPlot){
  
  gAdjMatrix <- as.matrix(as_adj(graphToPlot))
  
  logMatrix <- (gAdjMatrix == 1)
  
  matData <- melt(logMatrix)
  
  g <- ggplot(data = matData,
              aes(Var2, Var1)) + 
    geom_tile(aes(fill = value, 
                  color = value)) + 
    coord_equal() + 
    scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white")) + 
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) + 
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    guides(fill = FALSE, color = FALSE) +
    scale_y_reverse()
  
  print(g)
}
