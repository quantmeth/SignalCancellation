optim_tuple <- function(AS,melange){
  # melange contient les rangs de colonnes du tuple.
  TEMOINS <- as.matrix(AS$GS[,-melange]) # même si une seule colonne
  # if (length(AS$orphelines>0))    # on veut plutôt ne pas enlever les variables orphelines
  #   TEMOINS <- TEMOINS[,-AS$orphelines]
  VAR <- AS$GS[,melange]
  nm <- length(melange)
  out <- list()
  rg <- 1:nm
  cc <- 1
  # print(melange)
  poids <- matrix(NA, nrow = nm, ncol = nm)
  crit <- rep(NA, nm)
  basCrit <- 9e9
  meilPoids <- rep(NA, nm)
  for (k in 1:nm){
    par <- rep(0,nm-1)
    rg <- c(rg[-1],rg[1])
    VAR <- cbind(VAR[,-1], VAR[,1])  # mettre la variable k en dernier
    out <- optim(par, crit_tuple, gr = NULL, VAR, TEMOINS, method="Nelder-Mead")
    # pds[ <- out$par
    if (out$value<basCrit){
      basCrit <- out$value
      meilPoids[rg] <- c(out$par,-1)
    }
    crit[k] <- out$value
    cor <- abs(t(sc1(VAR %*% c(out$par,-1))) %*%  TEMOINS)
    # print(round(cor,3))
    po <- as.vector(c(out$par,-1))
    po <- po/as.numeric(sqrt(po %*% po))
    po[rg] <- po
    poids[,k] <- po
    cc <- min(cc, out$value)
  }
  mc <- which.min(crit)
  meilPoids <- -meilPoids/abs(meilPoids[1])
  # if (meilPoids[1]<0) meilPoids <- -meilPoids
  # VAR <- cbind(AS$GS[,c(melange[-1],melange[1])])
  # p <- meilPoids[-1]
  # crit1 <- crit_tuple(p,VAR,TEMOINS)
  # # out <- optim_nm(crit_tuple,start=p)
  #   out <- optim(p,crit_tuple,gr=NULL,VAR,TEMOINS,method="Nelder-Mead")
  # if (nm==4){
  #   print(melange)
  #   print(round(meilPoids,3))
  #   }
  return(list(cor=cc,prob=prodCorr(cc,AS$N)$p,meilPoids=meilPoids,poids=poids,crit=crit))
}

crit_tuple <- function(p,VAR,TEMOINS){
  max(abs(t(sc1(VAR %*% c(p,-1))) %*%  TEMOINS))
}