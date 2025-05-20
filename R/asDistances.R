asDistances <- function(AS){
  Dist <- matrix(0, AS$nv, AS$nv)
  for(k in 1:length(AS$Crit)){
    Dist[AS$Cpaires[1,k], AS$Cpaires[2,k]] <- AS$Crit[k]
  }
  # Dist[AS$orphelines,] <- NULL
  # Dist[,AS$orphelines] <- NULL
  if(length(AS$orphelines) > 0)  Dist <- Dist[-AS$orphelines, -AS$orphelines]
  AS$Dist <- t(Dist * upper.tri(Dist))
  return(AS)
}
