#' CoeffMethode
#'
#' @param factPattern todo
#' @param factCorr todo
#' @param nomMethode todo
#'
#' @returns todo
#' @export
#' 
#' @importFrom psych factor.scores
#' 
#' @author André Achim
CoeffMethode <- function(factPattern, factCorr, nomMethode){
  R <- factPattern %*% factCorr %*% t(factPattern)
  diag(R) <- 1
  test <- psych::factor.scores(R, factPattern, factCorr, method = nomMethode)
  coef <-  test$weights
}