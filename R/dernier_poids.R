dernier_poids <- function(poids, directions,cible,C2cible = 1.0){
  # pour expliquer une variable autre de communauté C2cible à partir de k directions
  # on part de k-1 poids pour calculier le poids de la dernière direction
  # directions[,k] contient les coordonnées des k variables explicatives dont le signal a été porté à une longueur estimée de 1
  R <- t(directions) %*% directions
  diag(R) <- 1
  k1 <- length(poids)
  poids <- c(poids,1)
  pp <- poids %*% t(poids)
  R <- R * pp
  C <- sum(R[1:k1,1:k1])-C2cible
  B <- 2 * sum(R[1:k1,k1+1])
  QQ <- B^2-4*C
  if (QQ < 0) return(NA)
  QQ <- sqrt(QQ)
  po <- c(-B+QQ,-B-QQ)/2
  contrastePlus <- sc1(directions %*% c(poids[1:k1],po[1]))
  contrasteMoins <- sc1(directions %*% c(poids[1:k1],po[2]))
  rPlus <- t(contrastePlus) %*% cible
  rMoins <- t(contrasteMoins) %*% cible
  # if (!is.numeric(rPlus) || !is.numeric(rMoins)) browser()
  j <- 1
  UnDeux <- (rPlus < rMoins)
  if (!is.logical(UnDeux)) browser()
  if (UnDeux)
    j <- 2
  return(po[j])
}