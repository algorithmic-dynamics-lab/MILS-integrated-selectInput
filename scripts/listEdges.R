require("igraph")

#utility function for delete_edges
listEdges <- function(gra){
  
  edgeList <- as_edgelist(gra, names = TRUE)
  
  printableEdgeList <- character(nrow(edgeList))
  
  if (nrow(edgeList) > 0){ 
    for (i in 1:nrow(edgeList)) {
      
      printableEdgeList[i] <- paste0(edgeList[i,1],
                                     "|", 
                                     edgeList[i, 2])
      
    }
    
    return(printableEdgeList)
  }
  return("no links")
}

## test
#star <- make_star(3)

## should print 
#edgeList <- listEdges(star)
#print(edgeList)

## should plot 3 vertices, without edges
#plot(delete_edges(star, listEdges(star)[1:2]))


