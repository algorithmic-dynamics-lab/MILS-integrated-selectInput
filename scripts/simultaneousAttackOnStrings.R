require(acss)
require(stringr)
# source("BDM1D.R")


simultaneous_attack_on_string <- function(orig_string, block_size, offset, base, 
                                          number_attacked_bits, evaluate_from_median) {
  
  k_values <- get_k_values(alphabet_size = base)
  orig_string_bdm <- bdm1D(orig_string, block_size, offset, base, k_values)
  
  string_vector <- unlist(str_split(orig_string, pattern = ""))
  
  deletion_strings <- c()
  
  for (i in 1: length(string_vector)) {
    
    bool_index_vector <- !logical(length(string_vector))
    bool_index_vector[i] <- FALSE
    
    back <- paste(string_vector[bool_index_vector], 
                  sep = "", collapse = "")
    deletion_strings <- c(deletion_strings, back)
  }
  
  deletion_strings_bdm <- unlist(lapply(deletion_strings, bdm1D, block_size, 
                                        offset, base, k_values))
  
  if (evaluate_from_median) {
    bdm_differences<- (median(deletion_strings_bdm) - deletion_strings_bdm)
  } else {
    bdm_differences <- orig_string_bdm - deletion_strings_bdm   
  }
  
  bdm_df <- data.frame(deletion_strings= deletion_strings,
                       bdm_differences = bdm_differences, 
                       stringsAsFactors = FALSE)
  
  bdm_df$diff_rank <- rank(bdm_df$bdm_differences, ties.method = "min")
  
  sort_by_diff_rank <- order(bdm_df$diff_rank)
  
  index_rank <- as.numeric(rownames(bdm_df[sort_by_diff_rank, ]))
  
  removed_bits_index <- index_rank[c(1:number_attacked_bits)]
  
  bool_index_vector <- !logical(length(string_vector))
  bool_index_vector[removed_bits_index] <- FALSE
  
  bool_index_vector
  
  string_vector[bool_index_vector]
  
  removed_bits_string <- paste(string_vector[bool_index_vector], 
                               sep = "", collapse = "")
  
  return (removed_bits_string)
}

## no block overlap

#start.time <- Sys.time()
# simultaneous_attack_on_string("110001101010111101", 
#                            12, 12, 2, 10, evaluate_from_median = TRUE)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)
# 
##max block overlap
# start.time <- Sys.time()
# simultaneous_attack_on_string("110001101010111101", 
#                            12, 1, 2, 10, evaluate_from_median = TRUE)
# end.time <- Sys.time()
# time.taken <-end.time - start.time
# print (time.taken)

