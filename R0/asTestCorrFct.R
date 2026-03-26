asTestCorrFct <- function(AS) {
  # AS <- asTestCorrFct(AS)
  # Annule les corrélations non significatives et produit des
  # branches pour toutes les corrélations entre les deux seuils
  # Enlève d'abord les tests et corrélations des groupements exclus pour coplanarité
  
  if (length(AS$VG[[1]]$Gr) == 0) {
    return(AS)
  }
  
  pe <- AS$pertinent
  R <- AS$R[pe, pe]
  
  for (brG in seq_along(AS$VG)) {
    if (length(AS$VG[[brG]]$Gr) == 0) {
      next
    }
    
    if (!is.null(AS$doublet)) {
      pex <- setdiff(pe, AS$doublet)
    }
    
    Fct <- AS$VG[[brG]]$Fct[pe,]
    FctX <- Fct
    Db <- AS$doublet
    D <- Fct[Db, ]
    FctX[Db, ] <- 0
    FctX[FctX != 0] <- 2
    FctX[Db, ] <- D
    
    ng <- length(AS$VG[[brG]]$Gr)
    
    if (ng == 0) {
      next  # ne pas traiter un scénario avorté
    }
    
    tryCatch({
      if (ng == 1) {
        CORR <- 0
        pr <- 1
      } else {
        triU <- function(mat) {}  # Placeholder, implémentez cette fonction
        correctionFDR <- function(pr) {}  # Placeholder
        
        pr_rg <- triU(AS$VG[[brG]]$pCorFct)
        pr <- correctionFDR(pr_rg)
        
        for (k in seq_along(pr)) {
          AS$VG[[brG]]$pCorFct[pr_rg[k, 1], pr_rg[k, 2]] <- pr[k]
        }
        
        f <- which(pr > AS$seuils[2])  # annuler les corrélations avec p > seuils(2)
        
        if (length(f) > 0) {
          for (i in f) {
            AS$VG[[brG]]$CorFct[pr_rg[i, 1], pr_rg[i, 2]] <- 0
          }
        }
        
        CORR <- AS$VG[[brG]]$CorFct  # garder la matrice de toutes les corrélations avec p <= seuils(2)
      }
      
      FF <- list(NULL)  # aucune corrélation exclue si F[[1]] pas remplacé
      f <- which((pr >= AS$seuils[1]) & (pr <= AS$seuils[2]))
      
      if (length(f) == 0) {
        cc <- 1
      } else {
        cc <- 2
        if (length(f) == 1) {
          FF[[cc]] <- f
        } else {
          FF[cc:(cc + length(f) - 1)] <- list(f)
        }
      }
      
    }, error = function(e) {
      AS$VG[[brG]]$erreur <- paste0(e$message, sprintf(" dans asTestCorrFct, brG=%d", brG))
    })
  }
  
  return(AS)
}