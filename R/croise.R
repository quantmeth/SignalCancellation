croise <- function(A, B) {
  # Retourne tous les croisements des lignes de A avec celles de B
  A <- as.matrix(A)
  B <- as.matrix(B)
  ra <- nrow(A)
  ca <- ncol(A)
  rb <- nrow(B)
  cb <- ncol(B)
  
  tuple <- matrix(nrow = ra * rb, ncol = ca + cb)
  ligne <- numeric(ca + cb)
  
  index <- 1
  for (j in seq_len(ra)) {
    ligne[1:ca] <- A[j, ]
    for (k in seq_len(rb)) {
      ligne[(ca + 1):(ca + cb)] <- B[k, ]
      tuple[index, ] <- ligne
      index <- index + 1
    }
  }
  
  return(tuple)
}