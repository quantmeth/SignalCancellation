asCorrFct <- function(AS, brG) {
  # Estime les corrélations entre les variables des facteurs (groupes dans AS$VG[[brG]]$Gr)
  # et corrige les corrélations des variables pour devenir celles des groupes
  # Ce qu'on utilisera dans asCoplanaire.m
  # Ce n'est que plus tard qu'on ramènera à 0 celles qui ne sont pas significatives
  
  AS$VG[[brG]]$CorEstim <- NULL
  ng <- length(AS$VG[[brG]]$Gr)
  
  AS$VG[[brG]]$CorFct <- matrix(0, nrow = ng, ncol = ng)  # On remplira le triangle supérieur puis on complètera
  AS$VG[[brG]]$pCorFct <- matrix(0, nrow = ng, ncol = ng)  # On remplira le triangle supérieur
  AS$VG[[brG]]$CorEstim <- matrix(0, nrow = ng, ncol = ng)
  
  for (j in 1:(ng - 1)) {
    for (k in (j + 1):ng) {  # Pour chaque paire de facteurs (j, k)
      res <- fctCor(AS, brG, j, k)
      AS$VG[[brG]]$CorFct[j, k] <- res$r
      AS$VG[[brG]]$pCorFct[j, k] <- res$pr
      AS$VG[[brG]]$CorEstim[j, k] <- res$cc
    }
  }
  
  return(AS)
}
