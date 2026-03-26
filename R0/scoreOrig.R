#' scoreOrig
#'
#' @param scoreProp todo
#' @param factCorr todo
#'
#' @returns todo
#' @export
#' 
#' @author André Achim
#'
scoreOrig <- function(scoreProp, factCorr = 0){
  Orig <- matrix(scoreProp$contrib[,1:3], ncol = 3)
  nf <- nrow(scoreProp$contrib)
  rangs <- 1:nf
  for (ff in 1:nf) {
    ortho <- chol(factCorr[rangs,rangs])
    ortho <- ortho %*% scoreProp$contrib[ff,rangs]
    ortho <- ortho * ortho
    o1 <- ortho[1,1]
    Orig[ff,1] <- o1
    if (nf>1)
      Orig[ff,2] <- sum(ortho[2:nf])
    else
      Orig[ff,2] <- 0
    Orig[ff,3] <- t(scoreProp$contrib[ff,-(1:nf)]) %*% scoreProp$contrib[ff,-(1:nf)]
    rangs <- rotation(rangs)
  }
  return(Orig)
}