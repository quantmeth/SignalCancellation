#' Fonction pour initier SCA
#'
#' @param R une matrice de corrélation ou un jeu de données.
#' @param N si une matrice de corrélation est entrée, indiquer le nombre de sujets.
#' @param seuils Erreur de type I. Valeur défaut = .001 et .25.
#'
#' @returns Retourne une liste avec plusieurs éléments de SCA.
#' 
#' @export
#' 
#' @import stats utils
#'
#' @examples
#' R <- Rnest::tabachnick_fidell2019
#' out <- init_SCA(R, N = 175)
init_SCA <- function(R, N = NULL, seuils = c(.001, .25)){
  #sort seuil TODO
  AS <- list(dat = R, seuils = seuils)
  if(is.null(N)) {AS$N = nrow(R)} else {AS$N = N}
  if(isSymmetric(as.matrix(R))) {AS$R <- R} else {AS$R <- cov(R)}
  AS$et <- sqrt(diag(AS$R)) # AA: Ceci est destiné à pouvoir exprimer la solution factorielle en termes des variables d'origine.
  iet <-  1 / AS$et
  AS$R <- AS$R*(iet %*% t(iet))
  #  browser()
  if(det(AS$R) < 0) stop("\nLa matrice de corr\u00e9lation n'a pas un d\u00e9terminant positif.\n")
  AS$nv <- ncol(AS$R)
  AS$pertinent <- 1:AS$nv  # avant d'exclure les variables orphelines
  AS$GS <- chol(AS$R)
  colnames(AS$GS) <- letters[1:AS$nv]
  if (AS$nv > 26) colnames(AS$GS[27:AS$nv]) <- LETTERS[1:(AS$nv-26)]
  AS <- asOrphelines(AS) 
  # POC: retirer un conditionnel ici, utiliser deux fois ####
  #  AS$pertinent <- setdiff(1:AS$nv, AS$orphelines)
  AS$minFct <- 1  # en attendant de restituer rNEST
  # AS$N <- 2000 #### ATTENTION! à retirer ######
  AS <- asPairesIndicatrices(AS)
  # AS <- asDistances(AS) # POC : POUR SCROF
  # AS <- asGrappes(AS)   # POC : POUR SCROF
  return(AS)
}