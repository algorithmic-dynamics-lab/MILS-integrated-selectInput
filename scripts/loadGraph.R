require("igraph")


loadGraph <- function(dataPath)
{
  
  loadedDF <- read.csv(dataPath,
                       header=FALSE,
                       sep=',', #separate by comma
                       quote="'", # quote by '
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

  #selects numeric values, drops the rest
  loadedDF <- loadedDF[sapply(loadedDF, is.numeric)]
  
  rownames(loadedDF) <- colnames(loadedDF)
  loadedMat <- as.matrix(loadedDF)
  
  #loadedMat <- unname(as.matrix(loadedDF)) 
  
  #we use the rownames to index deletions
  g <- graph_from_adjacency_matrix(loadedMat) %>%
    set_vertex_attr("label", value = 1:nrow(loadedDF))

  
  return(g)
}




