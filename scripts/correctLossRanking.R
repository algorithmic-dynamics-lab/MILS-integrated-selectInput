
source("edgeAndVertexKnockout.R")

starGraph <- loadGraphPA("../data/starGraphAdjMatrix.csv")

pe        <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)

#rank losses
bdmLossesDf <- pe[!pe$bdmIncrease, ]

print(bdmLossesDf)

bdmLossesDf$perturbationsRank <-rank(
  as.numeric(bdmLossesDf$bdmDifferenceAfterDeletion),
  ties.method="min")

maxLossRank <- max(bdmLossesDf$perturbationsRank)

#rank gains
bdmGainsDf <- pe[pe$bdmIncrease, ]

bdmGainsDf$perturbationsRank <-rank(
  as.numeric(bdmGainsDf$bdmDifferenceAfterDeletion),ties.method="min"
) + maxLossRank

rankedDf <- rbind(bdmLossesDf, bdmGainsDf)
rankedDf <- rankedDf[order(rankedDf$perturbationsRank), ]

print(rankedDf)

correctLossRanking <- function (pe){
  
  pe        <- calculatePerturbationByEdgeDeletion(starGraph, 4, 1)
  
  #rank losses
  bdmLossesDf <- pe[!pe$bdmIncrease, ]
  
  bdmLossesDf$perturbationsRank <-rank(
    as.numeric(bdmLossesDf$bdmDifferenceAfterDeletion),
    ties.method="min")
  
  maxLossRank <- max(bdmLossesDf$perturbationsRank)
  
  #rank gains
  bdmGainsDf <- pe[pe$bdmIncrease, ]
  
  bdmGainsDf$perturbationsRank <-rank(
    as.numeric(bdmGainsDf$bdmDifferenceAfterDeletion),ties.method="min"
  ) + maxLossRank
  
  rankedDf <- rbind(bdmLossesDf, bdmGainsDf)
  rankedDf <- rankedDf[order(rankedDf$perturbationsRank), ]
  
  return(rankedDf)
  
}

correctLossRanking(pe)