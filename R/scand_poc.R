scand_poc <- function(AS){
  # if (!isSymmetric(R)){
  #   N=nrow(R)
  #   R <- cor(R)
  # }
  #AS <- init_SCA(R,N)
  out <- list()
  encore <- TRUE
  k <- 0
  vDir <- 0
  while (encore) {
    k <- k+1
    tst <- seq_k_dim(AS,vDir)
    if (k==1) vDir <- tst$meil
    out[[length(out)+1]] <- tst
    encore <- tst$prob<.05
    vDir <- c(vDir,tst$prochain)
  }
  return(nfactors = length(out))
}