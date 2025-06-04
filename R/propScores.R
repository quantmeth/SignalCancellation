#' propScores 
#'
#' @description
#' TODO
#'
#' @param factCoef si donnée, est (nf,nf) (defaut I(nf,nf)).
#' @param factPattern (nv,nf) est la matrice de patrons factoriels.
#' @param factCorr si donnée, est (nf,nf) (defaut I(nf,nf)).
#' @param variances variances (optionnel) (nv) donne les variances des nv varables si autres que 1.
#'
#' @returns TODO
#' @export
#' 
#' @author André Achim
propScores <- function(factCoef, factPattern, factCorr = 1, variances = 1){
  correlSources <- corrSources(factPattern, factCorr)
  dims <- dim(factPattern)
  #if (~is.matrix(factCorr))
  #  factCorr <- diag(dims[2])
  unique <- variances-diag(factPattern %*% factCorr %*% t(factPattern))
  expanded <- cbind(factPattern, diag(sqrt(unique)))
  scoreProperties <- contribSources(factCoef, expanded, correlSources)
  return(scoreProperties)
}