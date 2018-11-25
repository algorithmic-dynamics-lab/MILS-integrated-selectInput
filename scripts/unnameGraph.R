library("igraph")

unnameGraph <- function(graphToUnname){
  
  g <- graphToUnname
  
  V(g)$name <- 1:vcount(g)
  
  return(g)
  
}