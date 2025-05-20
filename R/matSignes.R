matSignes <- function(k) {
  # signes <- matSignes(k);
  # G??n??re une matrice de signes selon la valeur de k
  if(!any(k == 3:5))   stop("matSignes(k) pr??vu seulement pour k entre 3 et 5")
  
  MAT <- list(list(),list(),
              matrix(c(1, 1, -1,
                       1, -1, 1,
                       -1, 1, 1), nrow = 3, byrow = TRUE),
              `4` = matrix(c(1, 1, -1, -1,
                             1, -1, 1, -1,
                             1, -1, -1, 1,
                             -1, 1, 1, -1,
                             -1, 1, -1, 1,
                             -1, -1, 1, 1), nrow = 6, byrow = TRUE),
              `5` = matrix(c(1, 1, -1, -1, -1,
                             1, -1, 1, -1, -1,
                             1, -1, -1, 1, -1,
                             1, -1, -1, -1, 1,
                             -1, 1, 1, -1, -1,
                             -1, 1, -1, 1, -1,
                             -1, 1, -1, -1, 1,
                             -1, -1, 1, 1, -1,
                             -1, -1, 1, -1, 1,
                             -1, -1, -1, 1, 1), nrow = 10, byrow = TRUE))
  
  return(MAT[[k]])
}