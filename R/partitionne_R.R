partitionne_R <- function(R,N,ote){
  # on devrait commencer par tester l'annulation des variables de ote
  ote <- matrix(ote,nrow=2)
  nv <- nrow(R)
  for (k in 1:ncol(ote)){
    sa <- sat_sur_facteur(R, N, ote[,k])$sat
    SA <- sa %*% t(sa)
    R <- R - SA
  }
  p2 <- R[-ote,-ote]
  diag(p2) <- NA
  co <- p2[upper.tri(p2)]
  p2[upper.tri(p2)] <- prob_Rr(co,N,length(ote))
  return(p2) 
  }