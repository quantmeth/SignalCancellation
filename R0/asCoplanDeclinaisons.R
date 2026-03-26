asCoplanDeclinaisons <- function(AS, brG) {
  # AS$declin <- asCoplanDeclinaisons(AS, brG);
  # Si brG > 1, mettre à jour uniquement ce qui a changé depuis AS$VG[[1]]
  
  ns <- length(AS$SCEN)
  declin <- vector("list", ns)
  
  for (k in seq_len(ns)) {  # Pour chaque SCEN[[ ]] établir declin[[k]]
    cc <- recens(AS$GRP[AS$SCEN[[k]], , drop = FALSE])
    doubl <- cc[cc[, 2] > 1, 1]
    ngr <- length(AS$SCEN[[k]])
    garde <- vector("list", ngr)
    
    for (j in seq_len(ngr)) {
      gr <- AS$GRP[AS$SCEN[[k]][j], , drop = FALSE]
      gr <- gr[(gr != 0)]
      signes <- matSignes(length(gr))
      garde[[j]] <- seq_len(nrow(signes))
      
      for (i in seq_along(doubl)) {
        garde[[j]] <- setdiff(garde[[j]], which(signes[, gr == doubl[i]] < 0))
      }
      
      # Ne garder dans garde que les grappes de 2 variables
      for (i in rev(seq_along(garde[[j]]))) {  # Boucle modifiée le 1er mars
        G <- which(signes[i, ] == -1)
        
        if (length(G) == 1 && length(AS$VG[[brG]]$Gr[[gr[[G]]]]) > 2) {
          garde[[j]][i] <- NULL  # On n'utilisera pas cette ligne de signes comme base de scénario
        }
        
        if (length(G) > 1 && max(length(AS$VG[[brG]]$Gr[[gr[[G[1]]]]]), length(AS$VG[[brG]]$Gr[[gr[G[2]]]])) > 2) {
          garde[[j]][i] <- NULL
        }
      }
    }
    
    croise <- garde[[1]]
    croise <- as.vector(croise)
    
    for (j in seq_len(ngr)[-1]) {
      croise <- 10 * croise + garde[[j]]
      croise <- as.vector(croise)
    }
    
    declin[[k]] <- croise
  }
  
  return(declin)
}