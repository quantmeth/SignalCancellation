dim_struct <- function(nf=3,nv=10,minA=.5,maxA=.8,mult_unic=1){
  nf1 <- nf+1
  signes <- sign(runif (nv*nf1,min=-.5, max=1))
  F <- matrix(runif(nv*nf1,min=minA,max=maxA),nrow=nv)
  F <- F * signes
  d <- sqrt(1/diag(F %*% t(F))) # marche aussi pour nf=1 à cause du nf+1
  F <- F * d
  F[,nf1] <- F[,nf1] * F[,nf1]
  return(F)
}
