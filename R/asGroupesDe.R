asGroupesDe <- function(AS, brG, var) {
  # Retourne les rangs des groupes des variables var dans AS$VG[[brG]]$Gr
  
  Gr <- AS$VG[[brG]]$Gr
  nv <- length(var)
  gd <- numeric(nv)
  
  for (k in seq_len(nv)) {
    group_found <- FALSE
    
    for (j in seq_along(Gr)) {
      if (var[k] %in% Gr[[j]]) {
        group_found <- TRUE
        break
      }
    }
    
    if (group_found) {
      gd[k] <- j
    } else {
      stop(sprintf("La variable v%d n'est dans aucun groupe de AS$VG[%d]", var[k], brG))
    }
  }
  
  return(gd)
}