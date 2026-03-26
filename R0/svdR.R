svdR <- function(R, nf = ncol(R)){
  #removed : arg = 'largest'
  E <- svd(R)
  Q <- diag(t(E$v)%*%E$u)[1:nf]
  b <- E$d[1:nf] * Q
  q <- colSums(E$v^3)[1:nf]
  a <- E$v[,1:nf] %*% diag(sign(q))
  list(a = a,
       b = b)
}
