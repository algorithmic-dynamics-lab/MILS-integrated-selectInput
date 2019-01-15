require("igraph")
require("OpenImageR")

inforank_image <- function(im, block_size, offset, what) {
  
  if(length(dim(im)) == 3) {
    im <- rgb_2gray(im)
  }
  
  k <- 0
  bdm_orig <- bdm2D(im, block_size, offset) 
  deletion_df <- data.frame(bdm_value = NA, bdm_difference = NA)
  
  if (what == 'rows') {
    k <- nrow(im)
  } else if (what == 'columns') {
    k <- ncol(im)
  }
  
  for(i in 1:k) {
    if (what == 'rows') {
      im_del <- im[-i, ]
    } else if (what == 'columns') {
      im_del <- im[, -i]
    }
    
    bdm_del <- bdm2D(im_del, block_size, offset)
    
    deletion_df[i, ]$bdm_value  <- bdm_del
    deletion_df[i, ]$bdm_difference <- bdm_orig - bdm_del 
  }
  
  deletion_df$perturbation_rank <- rank(-as.numeric(deletion_df$bdm_difference),
                                            ties.method = "min")
  
  return(deletion_df)
}

reduce_image <- function(im, block_size, offset, num, what = 'rows') {
  inforank <- inforank_image(im, block_size, offset, what)
  info_loss <- inforank[inforank$perturbation_rank > num, ]
  
  pair <- c()
  elements <- as.numeric(row.names(info_loss))
  
  if (is.na(elements[1])) elements[1] <- 2
  else if (is.na(elements[2])) elements[2] <- 2
  
  if (what == 'rows') {
    im[-(elements), ] <- 0.4
    pair <- list(coloring = im, 
                 reduction = im[elements, ])
  } else if (what == 'columns') {
    im[, -(elements)] <- 0.4
    pair <- list(coloring = im, 
                 reduction = im[, elements])
  }
  
  return(pair)
}