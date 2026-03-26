asBranches <- function(AS) {
  # AS <- asBranches(AS)
  # Explore la structure d'arbre et ajoute dans AS le champ .scenario
  # contenant les divers scénarios produits
  
  branches <- matrix(nrow = 0, ncol = 4)
  
  for (g in seq_along(AS$VG)) {
    if (length(AS$VG[[g]]$Gr) > 0) {
      if (is.null(AS$VG[[g]]$FC[[1]]$Fct)) {
        AS$VG[[g]]$FC[[1]] <- NULL
      } else {
        for (c in seq_along(AS$VG[[g]]$FC)) {
          Fct <- AS$VG[[g]]$FC[[c]]$Fct
          Co <- AS$VG[[g]]$FC[[c]]$CorFct
          f <- ncol(Fct)
          
          if (any(diag(Fct %*% Co %*% t(Fct)) > .99)) {
            f <- -f
          }
          
          branches <- rbind(branches, c(AS$VG[[g]]$FC[[c]]$Fit, g, c, f))
        }
      }
    }
  }
  
  nb <- nrow(branches)
  
  # Il peut arriver que AS$VG[[1]] n'aie pas de coplanarité mais que AS$VG[[>1]]
  # l'aie avec le même nombre maximum de grappes que AS$VG[[1]]
  
  rG <- 0  # rang de 1ere grappe coplanaire (nécessairement au maximum de grappes, je crois)
  cpl <- FALSE
  
  for (b in seq_len(nb)) {
    if (!cpl && length(AS$VG[[branches[b, 4]]]$coplan) > 3) {
      if (abs(branches[b, 4]) >= abs(branches[1, 4])) {
        rG <- b
        break
      }
    }
  }
  
  if (rG > 1) {
    branches[c(1, rG), ] <- branches[c(rG, 1), ]
  }
  
  for (b in seq(nb, 2)) {
    if (abs(branches[b, 1]) < AS$seuils[1]) {
      branches <- branches[-b, , drop = FALSE]
    }
  }
  
  if (!is.null(AS$VG[[1]]$coplan) && AS$VG[[1]]$coplan[1, 1] > AS$seuils[2]) {
    nf <- abs(branches[1, 4])  # max de grappes
    f <- which(abs(branches[, 4]) == nf)
    
    if (length(f) > 1) {
      branches <- branches[-f[-1], , drop = FALSE]  # exclure les scénarios avec nf facteurs si trio coplanaire présent
    }
  }
  
  AS$scenarios <- branches
  return(AS)
}