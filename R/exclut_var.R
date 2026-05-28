exclut_var <- function(R,N,ote){
  # place les variables de rangs ote en premier, 
  # calcule les covariances résiduelles (triangle inférieur)
  # et les probabilités des corrélations correspondantes (triangle supérieur)
  nv <- ncol(R)
  no <- length(ote)
  nd <- nv-no  
  o <- c(ote,setdiff(1:nv,ote))
  # browser()
  GS <- chol(R[o,o])
  # GS1 <- GS[,1:no]
  # CR1 <- t(GS1) %*% GS1
  # diag(CR1) <- NA
  # GS1 <- SC1(GS1)
  # Rr <- t(GS1) %*% GS1
  # CR1[upper.tri(CR1)] <- prob_Rr(Rr[upper.tri(Rr)],N,no)
  GS2 <- GS[,-(1:no)]
  CR2 <- t(GS2) %*% GS2
  diag(CR2) <- NA
  GS2 <- SC1(GS2)
  Rr <- t(GS2) %*% GS2
  # diag(Rr) <- 1
  CR2[upper.tri(CR2)] <- prob_Rr(Rr[upper.tri(Rr)],N,no)
  return(CR2)
  # return(list(p1R=CR1,p2R=CR2))
}

prob_Rr <- function(R,N,no){
  dl <- N-no-1
  RR <- 1-R*R
  if (any(RR<0)) browser() # c'est une erreur, mais on veut en savoir plus
  t <- R*sqrt(dl/RR)
  return(2*pt(-abs(t),dl))
}