ortho_norm <- function(M){
  # orthonormalise les colonnes de M
  M <- SC1(M)
  for (k in 2:ncol(M)){
    co <- lm(M[,k]~0+M[,1:(k-1)])
    M[,k] <- sc1(co$residuals)
  }
  return(M)
}
