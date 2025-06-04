#' choixScores 
#'
#' @param factPattern todo
#' @param fctCorr todo
#' @param meth todo
#' @param N todo
#'
#' @returns todo
#' @export
#' 
#' @importFrom MASS mvrnorm
#' 
#' @author André Achim
choixScores <- function(factPattern, fctCorr = 1, meth = list("Thurstone", "Bartlett", "tenBerge"), N = 100){
  dims <- dim(factPattern)
  if (length(fctCorr) == 1) {
    factCorr <- diag(dims[2])
  } else {
    factCorr <- fctCorr
  }
  variances <- 1
  correlSources <- corrSources(factPattern,factCorr)
  unique <- variances - diag(factPattern %*% factCorr %*% t(factPattern))
  expanded <- cbind(factPattern, diag(sqrt(unique)))
  randSources <- MASS::mvrnorm(N, rep(0, sum(dims)), correlSources, empirical = TRUE)
  data <- randSources %*% t(expanded)
  out <- list(
    pattern = factPattern,
    factCorr = fctCorr,
    scores = randSources[,1:dims[2]],
    dat = data)
  for (mt in meth){
    coef <- CoeffMethode(factPattern,factCorr,mt)
    propMethode <- propScores(coef,factPattern,factCorr)
    propMethode$contributions <- scoreOrig(propMethode,factCorr)
    propMethode$calcScores <- data %*% propMethode$zCoef
    propMethode$varScores <- cov(propMethode$calcScores)
    propMethode$projScoresFct <- t(propMethode$calcScores) %*% out$scores / (N-1)
    out[[mt]] <- propMethode
  }
  return(out)
}