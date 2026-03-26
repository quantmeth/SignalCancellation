classeTriplets <- function(coplan) {
  # À l'entrée, la colonne 1 de coplan (k,4) contient une probabilité, ce qui sera ignoré
  # À la sortie, coPlus sera (k,5), soit coplan avec la colonne 5 contenant un code.
  # Un code finissant par 4 marque trois ou quatre triplets (i.e., avec mêmes quatre
  # rangs de grappes); le code est 14 (puis 24, etc. si requis) pour un deuxième quatrain.
  # Le code des 1 (+10j) pour un triplet ne partageant aucune grappe avec les autres.
  # C'est 2 (+10j) pour deux triplets partageant 1 rang de grappes (mais pas dans un quatrain)
  # Et 3 (+10j) pour deux triplets (mutuellement incompatibles) partageant 2 rangs de grappes.
  # Cela ne décrit pas comment signaler deux groupes qui partagent une grappe qui doit rester un facteur.
  # Cela est à faire dans une autre fonction.
  
  k <- nrow(coplan)
  coPlus <- cbind(coplan, rep(0, k))
  
  coPlus <- Quad(coPlus)
  coPlus <- Doubl(coPlus)
  
  coPlus[coPlus[, ncol(coPlus)] == 0, ncol(coPlus)] <- 1
  
  o <- order(coPlus[, 1], decreasing = TRUE)
  coPlus <- coPlus[o, , drop = FALSE]
  
  return(coPlus)
}