
require(acss)
require(stringr)
#source("BDM1D.R")



simultaneousAttackOnString <- function(origString,
                                       blockSize, 
                                       offset, base, 
                                       numberOfAttackedBits,
                                       evaluateFromMedian) {
  
  kValues <- getKValues(alphabetSize=base)
  
  origStringBDMValue <- evaluateBDM1D(origString, blockSize, offset, base, kValues)
  
  stringVector <- unlist(str_split(origString, pattern=""))
  
  deletionStrings <- c()
  
  for (i in 1: length(stringVector)){
    
    boolIndexVector <- !logical(length(stringVector))
    boolIndexVector[i] <- FALSE
    back <- paste(stringVector[boolIndexVector], sep = "",
                  collapse="")
    deletionStrings <- c(deletionStrings, back)
    
  }
  
  deletionStringsBDMValues <- unlist(
    lapply(deletionStrings, 
           evaluateBDM1D, blockSize, offset, base, kValues))
  
  if(evaluateFromMedian){
 
    bdmDifferences<- (median(deletionStringsBDMValues) - 
                                 deletionStringsBDMValues)
  }
  
  else{
    bdmDifferences <- origStringBDMValue - deletionStringsBDMValues   
  }
  
  bdmDf <- data.frame(deletionStrings= deletionStrings,
                      bdmDifferences = bdmDifferences, 
                      stringsAsFactors=FALSE)
  
  bdmDf$diffRank <- rank(
     bdmDf$bdmDifferences, ties.method ="min"
  )
  
  sortByDiffRank <- order(bdmDf$diffRank)
  
  indexRank <- as.numeric(rownames(bdmDf[sortByDiffRank,]))
  
  removeddBitsIdx <- indexRank[c(1:numberOfAttackedBits)]
  
  boolIndexVector <- !logical(length(stringVector))
  boolIndexVector[removeddBitsIdx] <- FALSE
  
  boolIndexVector
  
  stringVector[boolIndexVector]
  
  removedBitsString <- paste(stringVector[boolIndexVector], 
                             sep="", collapse="")
  
  
  return (removedBitsString)
  
}

## no block overlap

#start.time <- Sys.time()
# simultaneousAttackOnString("110001101010111101", 
#                            12, 12, 2, 10, evaluateFromMedian = TRUE)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)
# 
##max block overlap
# start.time <- Sys.time()
# simultaneousAttackOnString("110001101010111101", 
#                            12, 1, 2, 10, evaluateFromMedian = TRUE)
# end.time <- Sys.time()
# time.taken <-end.time - start.time
# print (time.taken)

