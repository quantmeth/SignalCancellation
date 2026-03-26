#' contribSources
#'
#' @param factCoef (nv,nf) contient un jeu de coefficients factoriels
#' @param expanded (nv,nf+nv) donne la matrice d'amplitudes sur les facteurs (patrons) et les unicités
#' @param correlSources (nf+nv,nf+nv) est données par corrSources
#'
#' @returns  retourne une liste avec les coefficients factoriels tels que donnés en entrée, les mêmes ajustés pour donner une variance attendue des scores de 1.0 (facilite la comparaison entre méthodes) la corrélation attendue des scores et la contribution des nf+nv sources aux nf scores
#' @export
#' 
#' @author André Achim
contribSources <- function(factCoef, expanded, correlSources) {
  # factCoef (nv,nf) contient un jeu de coefficients factoriels
  # expanded (nv,nf+nv) donne la matrice d'amplitudes sur les facteurs (patrons) et les unicités
  # correlSources (nf+nv,nf+nv) est données par corrSources

  contrib <-  t(factCoef) %*% expanded
  scorCorr <- contrib %*% correlSources %*% t(contrib)
  tmp <- 1. / sqrt(diag(scorCorr))
  if (length(tmp)>1) {
    coefScale <- diag(tmp)
  } else {
    coefScale <- tmp
  }
  scorCorr <- coefScale %*% scorCorr %*% coefScale
  contrib <- coefScale %*% contrib
  out <- list(
    coef = factCoef,
    zCoef = factCoef %*% coefScale,
    scoreCorr = scorCorr,
    contrib=contrib)
  return(out)
}