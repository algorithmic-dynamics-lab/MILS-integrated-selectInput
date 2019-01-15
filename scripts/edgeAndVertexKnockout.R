require("igraph")
require("purrr")

correct_loss_ranking <- function(loss_ranking) {
  # For rank losses
  bdm_losses_df <- loss_ranking[!loss_ranking$bdm_increase, ]
  
  bdm_losses_df$perturbations_rank <- rank(as.numeric(bdm_losses_df$bdm_difference),
                                           ties.method = "min")
  
  max_loss_rank <- max(bdm_losses_df$perturbations_rank)
  
  # For rank gains
  bdm_gains_df <- loss_ranking[loss_ranking$bdm_increase, ]
  
  bdm_gains_df$perturbations_rank <- rank(as.numeric(bdm_gains_df$bdm_difference),
                                          ties.method = "min") + max_loss_rank
  
  ranked_df <- rbind(bdm_losses_df, bdm_gains_df)
  ranked_df <- ranked_df[order(ranked_df$perturbations_rank), ]
  
  return(ranked_df)
}

calculate_loss_by_vertex <- function(orig_graph, block_size, offset){
  
  orig_matrix <- as.matrix(as_adjacency_matrix(orig_graph))
  
  bdm_orig <- bdm2D(orig_matrix, block_size, offset) 
  
  vertex_perturbations_df <- as_data_frame(orig_graph, what = "vertices")
  
  computed_cols <- c("bdm_value", "bdm_difference", "bdm_increase")
  
  vertex_perturbations_df[, computed_cols] <- NA
  
  for(i in 1:nrow(vertex_perturbations_df)){
    
    deleted_vertex_matrix <- as.matrix(as_adjacency_matrix(
                                          delete_vertices(orig_graph, 
                                                    V(orig_graph)[i])))
    
    deleted_edge_bdm <- bdm2D(deleted_vertex_matrix, block_size, offset)
   
    vertex_perturbations_df[i, ]$bdm_value <- deleted_edge_bdm
    
    vertex_perturbations_df[i, ]$bdm_difference <- bdm_orig - deleted_edge_bdm 
    
    increase <- (deleted_edge_bdm > bdm_orig)
    vertex_perturbations_df[i, ]$bdm_increase <- increase
  }
  

  vertex_perturbations_df$perturbations_rank <-rank(
    -as.numeric(vertex_perturbations_df$bdm_difference),
        ties.method="min")
  

  return (vertex_perturbations_df)
}

format_edges <- function(edge_loss){
  formatted_edges <- paste0(edge_loss$from,"|", edge_loss$to)
  return (formatted_edges)
}

calculate_loss_by_edge <- function(orig_graph, block_size, offset){
  
  orig_matrix <- as.matrix(as_adjacency_matrix(orig_graph))
  
  bdm_orig    <- bdm2D(orig_matrix, block_size = block_size, offset = offset)
  
  edge_perturbations_df     <- as_data_frame(orig_graph, what = "edges")
  
  computed_cols <- c("bdm_value",
                    "bdm_difference", 
                    "bdm_increase")
  
  edge_perturbations_df[, computed_cols] <- NA
  
  for(i in 1:nrow(edge_perturbations_df)){
    
    deleted_edge_graph  <- delete_edges(orig_graph, 
                                      paste0(edge_perturbations_df[i, ]$from,
                                            "|",edge_perturbations_df[i, ]$to))
    
    deleted_edge_adj_matrix  <- as.matrix(as_adjacency_matrix(deleted_edge_graph)) 
    
    #added $bdm_value
    deleted_edge_bdm        <- bdm2D(deleted_edge_adj_matrix, 
                                   block_size = block_size, 
                                   offset = offset)
    
    edge_perturbations_df[i, ]$bdm_value <- deleted_edge_bdm
    
    edge_perturbations_df[i, ]$bdm_difference <- (bdm_orig - deleted_edge_bdm) 
    
    edge_perturbations_df[i, ]$bdm_increase <- (deleted_edge_bdm > bdm_orig) 
  }
  
  #TODO: test by commenting this out
  edge_perturbations_df$perturbations_rank <- rank(
    #TODO: changed result from - as.numeric, check the same in vertex_perturbations_df
    -as.numeric(edge_perturbations_df$bdm_diff), ties.method = "min"
  )
  
  edge_perturbations_df <- correct_loss_ranking(edge_perturbations_df)
  
  return(edge_perturbations_df)
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
#edge_perturbations_df <- calculatePerturbationByEdgeDeletion(testGraph, 4, 1)
# 
# vertexPal <- getColorRampPalette(vertex_perturbations_df)
# edgePal <- getColorRampPalette(edge_perturbations_df)
# 
# E(testGraph)$color <- edgePal(ecount(testGraph))[edge_perturbations_df$perturbations_rank]
# V(testGraph)$color <- vertexPal(vcount(testGraph))[vertex_perturbations_df$perturbations_rank]
# 
# plot(testGraph, vertex.label.family = "Arial Black", edge.arrow.size = .1, vertex.size = 25,
#      vertex.label.color="black")
# 
# vertex_perturbations_df <- calculate_loss_by_vertex(testGraph, 4, 1)
# print(vertex_perturbations_df)

###############

# starGraph <- loadGraphPA("../data/starGraphAdjMatrix.csv")
# 
# loss_ranking        <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)
# 
# print(loss_ranking)
# 
# print(processbdm_increases(starGraph))
# 
# edge_perturbations_df <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)
# print(edge_perturbations_df)





