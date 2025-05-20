correctionFDR <- function(prob) {
  # prob est un vecteur de probabilit??s pour des tests ?? estimer par False Discovery Rate (FDR)
  
  pr <- sort(prob, decreasing = FALSE)
  n <- rev(seq_along(pr))  # Equivalent ?? numel(pr):-1:1 en MATLAB
  
  # Calcul de la correction FDR
  corrig <- 1 - (1 - pr)^(n)
  
  # Remettre les valeurs dans leur ordre initial
  tryCatch({
    corrig[order(prob)] <- corrig
  }, error = function(e) {
    stop("Erreur lors de la r??organisation des valeurs corrig??es.")
  })
  
  return(corrig)
}