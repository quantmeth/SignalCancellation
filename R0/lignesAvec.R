lignesAvec <- function(mat, val) {
  # lign=lignesAvec(mat,val);
  # Retourne les rangs des lignes de mat qui contiennent un élément de val
  
  nl <- nrow(mat)
  val <- val[val != 0]  # Supprime les valeurs égales à 0
  lign <- seq_len(nl)
  
  for (k in nl:1) {
    A <- val %in% mat[k, ]  # Vérifie si un élément de val est présent dans la ligne
    if (!any(A)) {
      lign <- lign[lign != k]  # Supprime l'index si aucun élément n'est trouvé
    }
  }
  
  return(lign)
}