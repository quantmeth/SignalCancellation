agregeParCorr <- function(AS) {
  # AS <- agregeParCorr(AS)
  # examine les agrégations avec probabilités entre les deux seuils.
  # Si deux grappes d'au moins deux variables ont une corrélation qui
  # dépasse le seuil calculé par maxCorrFct(p), les scénarios avec ces deux
  # grappes sont éliminés, sachant que leur agrégation est acceptée dans un
  # autre scénario
  
  AS$exception <- list()
  f <- which((AS$Z[, ncol(AS$Z)] > AS$seuils[1]) & (AS$Z[, ncol(AS$Z)] <= AS$seuils[2]))
  nv <- length(AS$pertinent)
  ote <- c()
  
  for (k in f) {
    if (min(AS$Z[k, 1:2]) > nv) {
      # if length(a) > 1 && length(b) > 1
      a <- AS$GrBrut[[AS$Z[k, 1] - nv]]
      b <- AS$GrBrut[[AS$Z[k, 2] - nv]]
      ga <- asGrappeDe(AS, 1, a[1])   # remplace asGroupesDe le 2 mars 2025
      gb <- asGrappeDe(AS, 1, b[1])   # idem
      R <- AS$R[a, b]
      FA <- AS$VG[[1]]$Fct
      P <- FA(a, ga) %*% t(FA(b, gb))
      r <- solve(P, R)  # Équivalent de P(:)\R(:) en MATLAB
      
      if (r > maxCorrFct(AS$Z[k, ncol(AS$Z)])) {
        AS$exception <- c(AS$exception, list(c(k, AS$Z[k, ncol(AS$Z)], r)))
        AS$Z[k, ncol(AS$Z)] <- AS$seuils[2] + 0.0001
        ote <- c(ote, 1)
        
        for (j in 2:length(AS$VG)) {
          if (asGrappeDe(AS, j, a[1]) != asGrappeDe(AS, j, b[1])) {  # encore ici aux deux places
            ote <- c(ote, j)
          }
        }
      }
    }
  }
  
  AS$VG[unique(ote)] <- NULL
  return(AS)
}