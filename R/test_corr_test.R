test_corr_test <- function(S,N,nrep=500){
  prb <- matrix(NA,nrow=7,ncol=nrep) # recevra le triangle inférieur des $p2
  for (k in 1:nrep){
    dat <- gen_data(S,N)$dt
    co <- cor(dat)
    pp <- partitionne_R(co,N,1:4)
    # browser()
    p2 <- t(pp$p2)
    prb[,k] <- p2[lower.tri(p2)][1:7]
  }
  # return(rowMeans(prb<.05))
  return(prb)
}