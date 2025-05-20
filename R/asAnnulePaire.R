asAnnulePaire <- function(AS, k) {
  # AS$GS(v,v) est la GrammSchmidt (en triangle supérieur) de R(v,v)
  # k est un scalaire
  # Optimise les variables de la rangée k de AS$Cpaires pour en minimiser le signal
  # Remplit AS$Crit[k], AS$Ppaires[k] et AS$Corr[,k]
  # AS$Crit[k] est déjà le X2: N-1 fois la somme des corrélations au carré
  
  G <- AS$GS
  cible <- AS$pertinent
  melange <- AS$Cpaires[, k]
  lP <- 0
  pd <- probNonDoublet(AS, melange)
  
  if (pd < 0.05) {
    r <- AS$R[melange[1], melange[2]]
    K <- -sign(r)  # le poids fixe pour la 2e variable
    lP <- 0  # logarithme de P=1
    #POC: remove option and add them in optim
    suppressWarnings({
    lP <- optim(lP, fn = CritLog,
                method = "Nelder-Mead", 
                control =list(maxit = 1e9,
                              factr = 1e-6,
                              pgtol = 1e-6),
                G = G, combine = melange, K = K, cible = cible)$par
    P <- exp(lP)
    })
  } else {
    AS$doublet <- rbind(AS$doublet, melange)
    K <- 1
  }
  
  corr <- CritLog(lP, G, melange, cible, K, to.opt = FALSE)$corr
  AS$Crit[k] <- sum(corr^2) * (AS$N - 1)
  AS$Corr[, k] <- corr
  AS$Ppaires[k] <- exp(lP)
  
  return(AS)
}

CritLog <- function(px, G, combine, cible, K, to.opt = TRUE) {
  pp <- c(exp(px), K)
  SP <- G[, combine] %*% pp
  SP <- SP %*% solve(sqrt(t(SP) %*% (SP)))
  corr <- t(SP) %*% G
  corr[setdiff(1:length(corr), setdiff(cible, combine))] <- 0
  crit <- max(corr^2)
  if (is.complex(crit)) {
    crit <- 99e9
  }
  if(to.opt){
    out <- crit
  } else {
    out <- list(crit = crit, corr = corr)
  }
  return(out)
}
