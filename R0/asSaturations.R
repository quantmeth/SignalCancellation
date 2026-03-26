asSaturations <- function(AS, brG) {
  # Initialise les saturations des facteurs pour chaque groupe de variables
  
  ng <- length(AS$VG[[brG]]$Gr)
  AS$VG[[brG]]$Var <- numeric(ng)  # Variable qui pourra représenter chaque facteur
  
  for (gr in seq_len(ng)) {
    var <- sort(AS$VG[[brG]]$Gr[[gr]])
    AS$VG[[brG]]$Gr[[gr]] <- var  # S'assurer que les variables sont en ordre croissant pour les retrouver dans AS$Cpaires
    
    n <- length(var)
    satur <- matrix(0, nrow = n - 1, ncol = n)  # Pour examiner l'homogénéité des n-1 estimations de chaque variable
    rg <- numeric(n)  # Les rangs où écrire les saturations de chaque variable
    
    for (j in seq_len(n - 1)) {
      for (k in (j + 1):n) {  # Pour chaque paire des variables du facteur
        if (var[j] < 0 || var[k] < 0) {
          break  # Ignorer une variable de rang rendu négatif (trouvée orpheline)
        }
        
        sat <- asSatPaire(AS, var[c(j, k)])
        
        if (satur[1, j] != 0 && sign(sat[1]) != sign(satur[1, j])) {  # Assurer polarités constantes
          sat <- -sat
        }
        
        rg[c(j, k)] <- rg[c(j, k)] + 1
        satur[rg[j], j] <- sat[1]
        satur[rg[k], k] <- sat[2]
      }
    }
    
    if (any(var < 0)) {
      AS$pertinent <- AS$pertinent[!AS$pertinent %in% -var[var < 0]]
    }
    
    if (nrow(satur) > 1) {
      satur <- colMeans(satur)
    }
    
    if (sum(satur) < 0) {
      satur <- -satur
    }
    
    AS$VG[[brG]]$Fct[var, gr] <- satur
    f <- var[which.max(abs(satur))]  # Désigner la variable représentative
    AS$VG[[brG]]$Var[gr] <- f
  }
  
  return(AS)
}
