prep_init <- function(np,p=.25){
  pp <- c(-p,0,p)
  init <- as.matrix(pp)
  for (k in 1:(np-1)){
    init <- rbind(init,init,init)
    init <- cbind(init,rep(pp,each=3^k))
  }
  return(t(init))
}