recens <- function(coplan) {
  # cens <- recens(coplan)
  # coplan est une matrice (k,4) avec la probabilité associée à la coplanarité
  # du trio en colonne 1, ou (k,3) sans cette probabilité
  # si de longueur 4, le premier item, une probabilité, est ignoré
  # les 0 dans coplan ne sont pas recensés
  
  A <- coplan
  if (any(A[[1]] < 1)) {
    A <- A[, -1]  # Exclure la première colonne si elle contient des probabilités
  }
  
  A <- unlist(A)  # Convertir en vecteur
  g <- unique(A)
  g <- g[g != 0]  # Retirer les valeurs égales à 0
  
  cens <- data.frame(grappe = g, count = sapply(g, function(x) sum(A == x)))
  
  return(cens)
}