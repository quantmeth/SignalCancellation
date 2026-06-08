init_SCA <- function(R, N = NULL, seuils=NULL){
  if (is.null(seuils)) seuils <- c(.001,.25) 
  AS <- list(dat = R,seuils=seuils)
  if(is.null(N)) {AS$N = nrow(R)} else {AS$N = N}
  if(isSymmetric(as.matrix(R))) {AS$R <- R} else {AS$R <- cov(R)}
  AS$et <- sqrt(diag(AS$R)) # AA: Ceci est destiné à pouvoir exprimer la solution factorielle en termes des variables d'origine.
  iet <-  1 / AS$et
  AS$R <- AS$R*(iet %*% t(iet))
  #  browser()
  if(det(AS$R) < 0) stop("\nLa matrice de corr\u00E9lation n'a pas un d\u00E9terminant positif.\n")
  AS$nv <- ncol(AS$R)
  AS$pertinent <- 1:AS$nv  # avant d'exclure les variables orphelines
  # if (isSymmetric(AS$dat))
  AS$GS <- chol(AS$R)
  # else
  #   AS$GS <- scale(AS$dat)/sqrt(AS$N-1)
  colnames(AS$GS) <- letters[1:AS$nv]
  if (AS$nv > 26) colnames(AS$GS[27:AS$nv]) <- LETTERS[1:(AS$nv-26)]
  # AS <- asOrphelines(AS)
  # POC: retirer un conditionnel ici, utiliser deux fois ####
  #  AS$pertinent <- setdiff(1:AS$nv, AS$orphelines)
  AS$minFct <- 1  # en attendant de restituer rNEST
  AS <- as_paires_indicatrices(AS)
  # AS <- asDistances(AS)
  # AS <- asGrappes(AS)
  return(AS)
}