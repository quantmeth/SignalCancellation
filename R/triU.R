triU <- function(R){
  V <- which(upper.tri(R))
  rc <- which(upper.tri(R), arr.ind = TRUE)
  list(V = V,
       rc = rc)
}