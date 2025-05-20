asCrit <- function(p, G, combine, cible = NULL, to.opt = TRUE){
  if(is.null(cible)){
    v <- ncol(G)
    cible <- which(!(1:v) %in% combine)
  }
  #p <- mean(p)
  p <- c(p, -1)
  SP <- G[, combine] %*% p
  SP <- SP / c(sqrt(t(SP) %*% SP))
  corr <- t(SP) %*% G
  corr[, setdiff(1:ncol(G), setdiff(cible, combine))] <- 0
  crit <- max(corr^2, na.rm = TRUE)
  if(is.complex(crit)){
    crit <- 99e9 
  }
  if(!to.opt){
    crit <- list(crit = crit, corr = corr)
  }
  crit
}
