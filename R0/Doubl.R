Doubl <- function(coPlus) {
  # coPlus=Doubl(coPlus);
  # Chaque ligne de coPlus contient une probabilité, trois rangs de grappes et un 0 ou code
  
  d4 <- tail(which(coPlus[, ncol(coPlus)] == 4), 1)  # Équivalent de find(..., 'last')
  coP <- coPlus[coPlus[, ncol(coPlus)] == 0, 2:4]
  cens <- recens(coP)
  d <- 0
  
  while (any(cens[, 2] > 1)) {
    f <- which(coPlus[, ncol(coPlus)] == 0)
    r <- cens[which(cens[, 2] > 1)[1], 1]
    li <- lignesAvec(coP, r)
    L <- integer()
    g <- coP[li[1], ]
    
    if (length(li) > 2) {
      browser()  # Remplace 'keyboard' en MATLAB pour le debug en R
    }
    
    for (k in 2:length(li)) {  # length(li) au lieu de numel(li)
      uni <- unique(c(g, coP[li[k], ]))
      if (length(uni) == 5) {
        L <- c(f[1], f[k], 2 + d)
      }
      if (length(uni) == 4) {
        L <- c(f[1], f[k], 3 + d)
      }
    }
    
    coPlus[li + d4, ncol(coPlus)] <- L[3]
    d <- d + 10
    
    coP <- coPlus[coPlus[, ncol(coPlus)] == 0, 2:4]
    cens <- recens(coP)
  }
  
  return(coPlus)
}