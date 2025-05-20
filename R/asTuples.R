asTuples <- function(AS, k) {
  # Vérifier si 'pertinent' existe dans AS, sinon l'initialiser
  if (!"pertinent" %in% names(AS)) {
    AS$pertinent <- 1:AS$nv
  }
  cible <- AS$pertinent
  
  if (is.numeric(k) && length(k) == 1) {
    melange <- AS$Cpaires[k, ]
  } else {
    melange <- k
    if ("Tuples" %in% names(AS)) {
      mel <- rangMelange(AS, melange)
      if (!is.null(mel) && length(mel) > 0) {
        nm <- length(melange)
        AS$tmp$Crit <- AS$Tuples[[nm]]$cr[mel]
        #AS$tmp$Corr <- t(AS$Tuples[[nm]]$cor[mel, ])
        #AS$tmp$Poids <- t(AS$Tuples[[nm]]$Po[mel, ])
        return(AS)
      }
    } else {
      AS$Tuples[[1]] <- list()
    }
  }
  
  mul <- c(0.5, 1, 2)
  nm <- length(mul)
  cr <- numeric(nm)
  cor <- matrix(0, nrow = AS$nv, ncol = nm)
  Po <- matrix(0, nrow = length(melange) - 1, ncol = nm)
  
  mel <- abs(melange)
  P <- AS$R[mel[1:(length(mel) - 1)], mel[length(mel)]]
  
  for (e in 1:nm) {
    #Po[, e] <- NM_AA(mul[e] * P, AS$GS, mel, cible)
    suppressWarnings({
      fopt <- optim(P, fn = asCrit, G = AS$GS, method = "Nelder-Mead",
                    combine = melange,
                    cible = cible,
                    control = list(maxit = 1e9,
                                   factr = 1e-9,
                                   pgtol = 1e-9))
    })
    
    Po[, e] <- fopt$par
    cor[, e] <- asCrit(Po[, e], AS$GS, mel, cible, to.opt = FALSE)$cor
    cr[e] <- sum(cor[, e]^2)
  }
  
  ma <- apply(abs(Po), 2, max)
  f <- which(ma < 10)
  if (length(f) == 0) {
    f <- which.min(ma)#which(ma == min(ma))[1]
  } else {
    f <- which.min(cr)#which(cr == min(cr[f]))[1]
  }
  
  if (length(k) == 1) {
    AS$Crit[k] <- cr[f]
    AS$Corr[, k] <- cor[, f]
    AS$Ppaires[k, ] <- Po[, f]
  } else {
    AS$tmp$Crit <- cr[f]
    AS$tmp$Corr <- cor[, f]
    AS$tmp$Poids <- Po[, f]
    
    nm <- length(melange)
    if (nm > length(AS$Tuples)) {
      #AS$Tuples[[nm]]$melange <- 0
      AS$Tuples[[nm]] <- list(melange = 0)
    }
    
    ptr <- which(AS$Tuples[[nm]]$melange == 0)[1]
    if (is.na(ptr)) {
      ptr <- length(AS$Tuples[[nm]]$melange) + 1
    }
    # POC : Pourquoi toujours nm = 3 ou plus?
    # POC : ajouter cor et Po éventuellement
    AS$Tuples[[nm]]$melange[ptr] <- encodeMelange(AS, melange)
    AS$Tuples[[nm]]$cr[ptr] <- cr[f]
    # Retirer pour l'instant
    #AS$Tuples[[nm]]$cor[ptr, ] <- cor[, f] 
    #AS$Tuples[[nm]]$Po[ptr, ] <- Po[, f]
  }
  
  return(AS)
}