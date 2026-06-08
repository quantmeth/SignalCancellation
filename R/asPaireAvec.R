asPaireAvec <- function(AS, v) {
  # Retourne les rangs de AS$Cpaires qui impliquent la variable v
  
  P <- AS$Cpaires
  rP <- which(P[1, ] == v | P[2, ] == v)
  
  return(rP)
}