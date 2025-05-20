Quad <- function(coPlus) {
  # coPlus=Quad(coPlus);
  # Chaque ligne de coPlus contient 1 probabilité, 3 rangs de grappes 1 0 ou code
  # La probabilité des quatrains est forcée à 0.5
  
  coP <- coPlus[coPlus[, ncol(coPlus)] == 0, 2:4]
  cens <- recens(coP)
  d <- 0
  
  while (any(cens[, 2] > 2)) {
    r <- which(cens[, 2] > 2)[1]
    r <- cens[r, 1]
    li <- lignesAvec(coP, r) # Il manque la ligne avec les 3 autres que r
    L <- li[1]
    g <- coP[li[1], ]
    
    for (k in 2:length(li)) {
      if (length(unique(c(g, coP[li[k], ]))) == 4) {
        L <- c(L, li[k])
      }
    }
    
    # autre = setdiff(unique(coP[L, ]), r) * c(1, 10, 100)
    sd <- setdiff(unique(coP[L, ]), r)
    mu <- 10^(0:(length(sd) - 1))
    autre <- sum(sd * mu)
    
    L <- c(L, which(rowSums(coP * c(1, 10, 100)) == autre))
    f <- which(coPlus[, ncol(coPlus)] == 0)
    coPlus[f[L], ncol(coPlus)] <- 4 + d
    coPlus[f[L], 1] <- 0.5
    d <- d + 10
    
    coP <- coPlus[coPlus[, ncol(coPlus)] == 0, 2:4]
    cens <- recens(coP)
  }
  
  return(coPlus)
}