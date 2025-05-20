asInitFct_Cor <- function(AS) {
  # Initialise les champs .Fct et .Cor de chaque scénario 
  # à partir des variables s'annulant par paire
  
  for (brG in seq_along(AS$VG)) {
    ng <- length(AS$VG[[brG]]$Gr)  # Pour le maximum de groupes
    
    AS$VG[[brG]]$Fct <- matrix(0, nrow = AS$nv, ncol = ng)
    
    # Estimer les saturations des facteurs pour les variables unifactorielles
    AS <- asSaturations(AS, brG)  # Prépare aussi AS$Var : une variable par groupe
    
    # Quand il n'y a pas de grappe avec p > seuils(2), il faut remplir .reste
    f <- which(apply(abs(AS$VG[[brG]]$Fct), 1, max) > 0)
    AS$VG[[brG]]$reste <- setdiff(AS$pertinent, f)
    
    if (length(AS$VG[[brG]]$Gr) > 1) {
      AS <- asCorrFct(AS, brG)
    } else {
      AS$VG[[brG]]$CorFct <- 1
      AS$VG[[brG]]$CorEstim <- 1
    }
  }
  
  return(AS)
}
