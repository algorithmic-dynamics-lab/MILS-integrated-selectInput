require("igraph")
require("ggplot2")
require("reshape2")

plot_adj_matrix <- function(graph_to_plot) {
  
  g_adj_matrix <- as.matrix(as_adj(graph_to_plot))
  
  log_matrix <- (g_adj_matrix == 1)
  
  mat_data <- melt(log_matrix)
  
  g <- ggplot(data = mat_data,
              aes(Var2, Var1)) + 
       geom_tile(aes(fill = value, color = value)) + 
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
