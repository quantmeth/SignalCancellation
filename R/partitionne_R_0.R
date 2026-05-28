partitionne_R <- function(R,N,ote){
  # on devrait commencer par tester l'annulation des variables de ote
  # on pourra ajouter des tests de ces deux parties séparément
  ote <- matrix(ote,nrow=2)
  sa <- list()
  nc <- ncol(ote)
  for (k in 1:nc)
    sa[[k]] <- sat_sur_facteur(R, N, ote[,k],seuil=.5)$sat
  sa <- matrix(unlist(sa),ncol=nc)
  rownames(sa) <- colnames(R)
  if (length(ote) > 2){
    O <- as.vector(R[ote[,1],ote[,2]])
    A <- as.vector(sa[ote[,1],1] %*% t(sa[ote[,2],2]))
    r <- t(A) %*% O / (t(A) %*% A)
    r <- .5*(r+mean(c(sa[ote[1,1],2],sa[ote[2,1],2],sa[ote[1,2],1],sa[ote[2,2],1])))
    rr <- solve(matrix(c(1,r,r,1),nrow=2))
  } else
    rr <- 1
  sb <- sa
  sa <- sb %*% chol(rr)
  browser()
  p1 <- sb %*% rr %*% t(sb)
  p1 <- p1[-ote,-ote]
  dp1 <- diag(p1)
  if (any(dp1>.99)) browser()
  p2 <- R[-ote,-ote]-p1
  d <- diag(1/sqrt(1-diag(p1)))
  p2a <- d %*% p2 %*% d
  if (any(abs(p2)>1)) browser()
  p1[upper.tri(p1)] <- prob_Rr(p1[upper.tri(p1)],N,0)
  p2[upper.tri(p2)] <- prob_Rr(p2[upper.tri(p2)],N,length(ote))
  p2a[upper.tri(p2a)] <- prob_Rr(p2a[upper.tri(p2a)],N,length(ote))
  diag(p1) <- NA
  diag(p2) <- NA
  diag(p2a) <- NA
  # browser()
  return(list(p1=p1,p2=p2,p2a=p2a,satur=sa%*%rr,r=r)) 
  }