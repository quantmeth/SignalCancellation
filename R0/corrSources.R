#' corrSources
#'
#' @description corrélations entre toutes les sources, tant d'unicités que des facteurs.
#'
#' @param factPattern todo
#' @param factCorr todo
#'
#' @returns le résultat est (nf+nv, nf+nv).
#' @export
#'
#' @author André Achim
corrSources <- function(factPattern, factCorr = 1){
  nv <- nrow(factPattern)
  nf <- ncol(factPattern)
  cor <- diag(1, nf+nv)
  if (is.matrix(factCorr)) {
    cor[1:nf,1:nf] <- factCorr
  }
  return(cor)
}