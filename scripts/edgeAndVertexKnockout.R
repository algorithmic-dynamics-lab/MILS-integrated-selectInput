
require("igraph")
require("purrr")

correctLossRanking <- function (lossRanking){
  
  #rank losses
  bdmLossesDf <- lossRanking[!lossRanking$bdmIncrease, ]
  
  bdmLossesDf$perturbationsRank <-rank(
    as.numeric(bdmLossesDf$bdmDifferenceAfterDeletion),
    ties.method="min")
  
  maxLossRank <- max(bdmLossesDf$perturbationsRank)
  
  #rank gains
  bdmGainsDf <- lossRanking[lossRanking$bdmIncrease, ]
  
  bdmGainsDf$perturbationsRank <-rank(
    as.numeric(bdmGainsDf$bdmDifferenceAfterDeletion),ties.method="min"
  ) + maxLossRank
  
  rankedDf <- rbind(bdmLossesDf, bdmGainsDf)
  rankedDf <- rankedDf[order(rankedDf$perturbationsRank), ]
  
  return(rankedDf)
  
}

calculateLossByVertexDeletion <- function(origGraph, blockSize, offset){
  
  origMatrix  <- as.matrix(as_adjacency_matrix(origGraph))
  
  bdmOrig     <- bdm2D(origMatrix, 
                       blockSize = blockSize, 
                       offset = offset) 
  
  vertexPerturbationsDF <- as_data_frame(origGraph, 
                                         what = "vertices")
  
  computedCols <- c("bdmValue",
                    "bdmDifferenceAfterDeletion", 
                    "bdmIncrease")
  
  vertexPerturbationsDF[, computedCols] <- NA
  
  for(i in 1:nrow(vertexPerturbationsDF)){
    
    delMat          <- as.matrix(as_adjacency_matrix(
                          delete_vertices(origGraph, 
                                        V(origGraph)[i])))
    
    bdmValueDel     <- bdm2D(delMat, 
                             blockSize = blockSize, 
                             offset = offset)
   
    vertexPerturbationsDF[i, ]$bdmValue  <- bdmValueDel
    
    bdmDiff         <- bdmOrig - bdmValueDel 
    vertexPerturbationsDF[i, ]$bdmDifferenceAfterDeletion <- bdmDiff
    
    bdmIncrease     <- (bdmValueDel > bdmOrig)
    vertexPerturbationsDF[i, ]$bdmIncrease <- bdmIncrease
  }
  

  vertexPerturbationsDF$perturbationsRank <-rank(
    -as.numeric(vertexPerturbationsDF$bdmDifferenceAfterDeletion),
        ties.method="min")
  

  return (vertexPerturbationsDF)
}

formatEdgesForDeletion <- function(edgeLoss){
  formattedEdges <- paste0(edgeLoss$from,"|", edgeLoss$to)
  return (formattedEdges)
}

calculateLossByEdgeDeletion <- function(origGraph, blockSize, offset){
  
  origMatrix <- as.matrix(as_adjacency_matrix(origGraph))
  
  bdmOrig    <- bdm2D(origMatrix, blockSize = blockSize, 
                      offset = offset)
  
  edgePerturbationsDF     <- as_data_frame(origGraph, what = "edges")
  
  computedCols <- c("bdmValue",
                    "bdmDifferenceAfterDeletion", 
                    "bdmIncrease")
  
  edgePerturbationsDF[, computedCols] <- NA
  
  for(i in 1:nrow(edgePerturbationsDF)){
    
    deletedEdgeGraph  <- delete_edges(origGraph, 
                                      paste0(edgePerturbationsDF[i, ]$from,
                                            "|",edgePerturbationsDF[i, ]$to))
    
    deletedEdgeAdjMatrix  <- as.matrix(as_adjacency_matrix(deletedEdgeGraph)) 
    
    #added $bdmValue
    deletedEdgeBDM        <- bdm2D(deletedEdgeAdjMatrix, 
                                   blockSize = blockSize, 
                                   offset = offset)
    
    edgePerturbationsDF[i, ]$bdmValue <- deletedEdgeBDM
    
    edgePerturbationsDF[i, ]$bdmDifferenceAfterDeletion    <- (bdmOrig - deletedEdgeBDM) 
    
    edgePerturbationsDF[i, ]$bdmIncrease           <- (deletedEdgeBDM > bdmOrig) 
    
  }
  
  #TODO: test by commenting this out
  edgePerturbationsDF$perturbationsRank <- rank(
    #TODO: changed result from - as.numeric, check the same in vertexPerturbationsDF
    -as.numeric(edgePerturbationsDF$bdmDiff), ties.method ="min"
  )
  
  edgePerturbationsDF <- correctLossRanking(edgePerturbationsDF)
  
  return(edgePerturbationsDF)
}




########################


## test matrix
#  ro <- 5
#  co <- 5
# 
# set.seed(3)
# testMatrix <- apply(matrix(0, ro, co), c(1, 2), function(x) sample(c(0, 1), 1))
# 
# testGraph <- graph_from_adjacency_matrix(testMatrix)  %>%
#   set_vertex_attr("label", value = LETTERS[1: 5])

# i <-1
# td <- delete_vertices(testGraph, V(testGraph)[i])
# plot(td)
#######################################
# 
#edgePerturbationsDF <- calculatePerturbationByEdgeDeletion(testGraph, 4, 1)
# 
# vertexPal <- getColorRampPalette(vertexPerturbationsDF)
# edgePal <- getColorRampPalette(edgePerturbationsDF)
# 
# E(testGraph)$color <- edgePal(ecount(testGraph))[edgePerturbationsDF$perturbationsRank]
# V(testGraph)$color <- vertexPal(vcount(testGraph))[vertexPerturbationsDF$perturbationsRank]
# 
# plot(testGraph, vertex.label.family = "Arial Black", edge.arrow.size = .1, vertex.size = 25,
#      vertex.label.color="black")
# 
# vertexPerturbationsDF <- calculateLossByVertexDeletion(testGraph, 4, 1)
# print(vertexPerturbationsDF)

###############

# starGraph <- loadGraphPA("../data/starGraphAdjMatrix.csv")
# 
# lossRanking        <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)
# 
# print(lossRanking)
# 
# print(processBDMIncreases(starGraph))
# 
# edgePerturbationsDF <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)
# print(edgePerturbationsDF)





