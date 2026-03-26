fctCor <- function(AS, br, i, j) {
  # [r, pr, cc, st] <- fctCor(AS, br, i, j)
  # retourne la corrélation r entre les grappes i et j du groupe br
  # la probabilité pr et la corrélation canonique cc sont issues de canoncorr
  # des variables
  # tandis que r estime la corrélation du signal des variables
  # utilisant AS$GS et AS$N
  
  Gr <- AS$VG[[br]]$Gr
  
  # test de signification de la corrélation
  X <- prepGS(AS$GS[, Gr[[i]]], AS$N)
  Y <- prepGS(AS$GS[, Gr[[j]]], AS$N)
  
  #canoncorr_result <- canoncorr(X, Y)
  #cc <- canoncorr_result$cor[1]
  #pr <- canoncorr_result$pF[1]
  cca_result <- cancor(X, Y)
  cc <- cca_result$cor
  capture.output(st <- CCP::p.asym(cc, dim(X)[1], dim(X)[2], dim(Y)[2], tstat = "Wilks"))
  pr <- st$p.value[1]
  # calcul de la corrélation
  R <- AS$R[Gr[[i]], Gr[[j]]]
  FA <- AS$VG[[br]]$Fct
  P <- FA[Gr[[i]], i] %*% t(FA[Gr[[j]], j])
  # same as r=P(:)\R(:);
  r <- sum(P * R) / sum(P^2)
  
  return(list(r = r, pr = pr, cc = cc[1], st = st))
}

prepGS <- function(M, N) {
  MM <- sqrt(0.5) * M
  MM <- rbind(MM, -MM)
  v <- ncol(MM)
  n <- N - nrow(MM)
  MM <- rbind(MM, matrix(0, n, v))
  return(MM)
}
