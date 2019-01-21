require("igraph")

load_graph <- function(data_path) {
  
  loaded_df <- read.csv(data_path, header = FALSE, sep = ',', quote = "'",
                       stringsAsFactors = FALSE, check.names = FALSE)

  # Selects numeric values, drops the rest
  loaded_df <- loaded_df[sapply(loaded_df, is.numeric)]
  
  rownames(loaded_df) <- colnames(loaded_df)
  loaded_mat <- as.matrix(loaded_df)
  
  # loaded_mat <- unname(as.matrix(loaded_df)) 
  
  # We use the rownames to index deletions
  g <- graph_from_adjacency_matrix(loaded_mat) %>%
    set_vertex_attr("label", value = 1:nrow(loaded_df))
  
  return(g)
}




