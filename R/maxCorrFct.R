maxCorrFct <- function(p) {
  # rmax=maxCorrFct(p)
  # Pour des probabilités p entre .001 et .25 associées à l'agrégation de deux
  # sous-grappes, retourne les corrélations maximales rmax qu'on veut tolérer
  # entre les deux sous-grappes pour ne pas les forcer à s'agréger
  
  L <- log(c(0.001, p, 0.25))
  rmax <- 0.5 + 0.3 * (L[2:(length(L)-1)] - L[length(L)]) / (L[1] - L[length(L)])
  
  return(rmax)
}