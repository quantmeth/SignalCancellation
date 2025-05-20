asCoplanaire <- function(AS) {
  # AS <- asCoplanaire(AS)
  # si trois groupes n'occupent qu'un plan, en enlever un
  
  for (brG in seq_along(AS$VG)) {
    ng <- length(AS$VG[[brG]]$Gr)
    if (ng < 3) { next }
    
    # Initialisation des variables
    AS$VG[[brG]]$coplan <- list()
    AS$VG[[brG]]$GrCoplan <- list()
    Gr <- AS$VG[[brG]]$Gr
    ng <- length(Gr)
    
    if (ng > 2) {
      trios <- t(combn(seq_len(ng), 3, simplify = TRUE))
      
      if (!"Tuples" %in% names(AS) || length(AS$Tuples) < 3) {
        AS$Tuples[[3]]$melange <- rep(0, length(trios))  # POC POURQUOI 3? 
      }
      
      for (t in 1:nrow(trios)) {
        tri <- trios[t,]
        #np <- rep(0,3)
        np <- sapply(tri, function(k) length(Gr[[k]]))
        
        if (all(np > 2)) { next } else {
          #if(!all(np <= 2)){
          tri <- ordonneTrio(AS, brG, tri)
          np <- prod(np)
          
          nt <- 0
          pire <- 0
          
          for (i in seq_along(Gr[[tri[1]]])) {
            for (j in seq_along(Gr[[tri[2]]])) {
              for (k in seq_along(Gr[[tri[3]]])) {
                nt <- nt + 1
                melange <- c(Gr[[tri[1]]][i], Gr[[tri[2]]][j], Gr[[tri[3]]][k])
                
                if (any(melange < 0)) {
                  pire <- -1
                } else {
                  AS <- asTuples(AS, melange)
                  cr <- AS$tmp$Crit
                  
                  if (cr > pire) {
                    pire <- cr
                  }
                }
              }
            }
          }
          
          if (pire > 0) {
            pire <- pire * (AS$N - 1)
            pr <- pchisq(pire, length(AS$pertinent) - 3, lower.tail = TRUE)
            pr <- 1 - pr^nt
            
            if (pr > AS$seuils[1]) {
              
              AS$VG[[brG]]$coplan <- rbind(AS$VG[[brG]]$coplan,
                                           c(pr, tri))
              
            }
          }
        }
      }
    }

    # TODO ICI!!!
    AS <- asValideCoplan(AS, brG)
  }
  return(AS)
}