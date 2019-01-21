library("igraph")

unname_graph <- function(graph_to_unname) {
  
  g <- graph_to_unname
  
  V(g)$name <- 1:vcount(g)
  
  return(g)
  
}