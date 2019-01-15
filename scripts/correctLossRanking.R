
source("edgeAndVertexKnockout.R")

starGraph <- load_graph("../data/starGraphAdjMatrix.csv")

pe <- calculate_loss_by_edge(starGraph, 4, 1)

#rank losses
bdmLossesDf <- pe[!pe$bdm_increase, ]

print(bdmLossesDf)

bdmLossesDf$perturbations_rank <-rank(
  as.numeric(bdmLossesDf$bdm_difference),
  ties.method="min")

maxLossRank <- max(bdmLossesDf$perturbations_rank)

#rank gains
bdmGainsDf <- pe[pe$bdm_increase, ]

bdmGainsDf$perturbations_rank <-rank(
  as.numeric(bdmGainsDf$bdm_difference),ties.method="min"
) + maxLossRank

rankedDf <- rbind(bdmLossesDf, bdmGainsDf)
rankedDf <- rankedDf[order(rankedDf$perturbations_rank), ]

print(rankedDf)

correctLossRanking <- function (pe){
  
  pe <- calculate_loss_by_edge(starGraph, 4, 1)
  
  #rank losses
  bdmLossesDf <- pe[!pe$bdm_increase, ]
  
  bdmLossesDf$perturbations_rank <-rank(
    as.numeric(bdmLossesDf$bdm_difference),
    ties.method="min")
  
  maxLossRank <- max(bdmLossesDf$perturbations_rank)
  
  #rank gains
  bdmGainsDf <- pe[pe$bdm_increase, ]
  
  bdmGainsDf$perturbations_rank <-rank(
    as.numeric(bdmGainsDf$bdm_difference),ties.method="min"
  ) + maxLossRank
  
  rankedDf <- rbind(bdmLossesDf, bdmGainsDf)
  rankedDf <- rankedDf[order(rankedDf$perturbations_rank), ]
  
  return(rankedDf)
  
}

correctLossRanking(pe)