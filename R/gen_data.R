#' Génération de jeux de données selon une structure factorielle.
#'
#' @param FS Une structure factorielle.
#' @param N Nombre de sujets.
#' @param R une matrice de corrélation des facteurs.
#' @param with_var Si \code{with_var = TRUE}, la dernière colonne de F est la variance des unicités, autrement la variance d'unicité est 1 moins la somme des carrés sur les colonnes.
#' @param empirical logical. If \code{TRUE}, R specify the empirical not population covariance matrix.
#'
#' @returns Une liste contenant un jeu de données (\code{dt}) et leur source \code{srce}.
#' @export
#' 
#' @importFrom MASS mvrnorm
#' @importFrom expm sqrtm
#' 
gen_data <- function(FS, N, R = NULL, with_var = FALSE, empirical = FALSE){
# FS est matrice de patrons de nv rangées
# R, si présente, est la matrice de corrélation des facteurs
# si with_var est vraie, la dernière colonne de F est la variance des unicités
# autrement la variance d'unicité est 1 moins la somme des carrés sur les colonnes
  if (!is.matrix(FS)){
    nf <- 1
    nv <- length(FS)
    unic <- 1 - FS*FS
    f <- as.matrix(FS)
  } else {
    nv <- nrow(FS)
    nf <- ncol(FS)
    if (with_var){
      unic <- FS[,nf]
      FS <- FS[,-nf]
      nf <- nf-1
    } else
      unic <- 1-rowSums(FS * FS)
  }
  if (is.null(R))
    R <- diag(rep(1, nf))
  src_cov <- diag(nf+nv)
  src_cov[1:nf, 1:nf] <- R
  G <- cbind(FS %*% expm::sqrtm(R), diag(sqrt(unic)))
  srce <- MASS::mvrnorm(N, rep(0, nf + nv), src_cov, empirical = empirical)
  dat <- srce %*% t(G)
  return(list(dt = dat, srce = srce))
}
  