asMultiSatur <- function(AS, brG, v, np) {
  # Trouve la meilleure annulation de la variable v par np (=2 ou 3) grappes
  # de AS$VG[[brG]]$Gr, excluant les grappes déclarées facteurs doublets
  # Retourne la probabilité associée (la pire probabilité corrigée)
  # de même que les np saturations sur les np grappes
  
  nv <- AS$nv
  rP <- asPaireAvec(AS, v)
  cr <- min(AS$Crit[rP])
  p <- 1 - pchisq(cr, length(AS$pertinent) - 2)
  
  if (p > AS$seuils[1]) {
    vg <- setdiff(AS$Cpaires[AS$Crit == cr], v)
    G <- asGrappeDe(AS, brG, vg)  # vide si la meilleure annulation par paire n'est pas dans une grappe
  } else {
    G <- NULL
  }
  
  var <- AS$VG[[brG]]$Var  # Variable représentative de chaque grappe, pour évaluation primaire
  V <- outer(var, AS$doublet, FUN = "==")
  var <- var[rowSums(V) == 0]  # Exclure les grappes doublets
  
  Cc <- combn(var, np, simplify = FALSE)  # Tous les croisements de np variables représentant leurs facteurs
  
  if (!is.null(G)) {  
    Cc <- Cc[sapply(Cc, function(x) any(x == var[G]))]
  }
  
  nc <- length(Cc)
  Crit <- numeric(nc)
  Corr <- matrix(0, nrow = nv, ncol = nc)
  P <- matrix(0, nrow = nc, ncol = np)
  prob <- numeric(nc)
  dl <- length(AS$pertinent) - np
  
  for (j in seq_len(nc)) {
    melange <- c(Cc[[j]], v)
    AS <- asTuples(AS, melange)
    Crit[j] <- AS$tmp$Crit
    P[j, ] <- AS$tmp$Poids
    Corr[, j] <- AS$tmp$Corr
    prob[j] <- 1 - pchisq(Crit[j] * (AS$N - 1), dl)
  }
  
  f <- which.max(prob)
  
  if (length(f) == 0 || prob[f] < AS$seuils[1]) {  
    return(list(prob = prob[f], Satur = matrix(0, nrow = 1, ncol = np), Grp = matrix(0, nrow = 1, ncol = np)))
  }
  
  Grp <- asGroupesDe(AS, brG, Cc[[f]])  # Groupes des variables qui annulent le signal de reste(1)
  
  tryCatch({
    crois <- expand.grid(AS$VG[[brG]]$Gr[[Grp[3]]],
                         AS$VG[[brG]]$Gr[[Grp[2]]],
                         AS$VG[[brG]]$Gr[[Grp[1]]])[,3:1]
    #crois <- croise(AS$VG[[brG]]$Gr[[Grp[1]]], AS$VG[[brG]]$Gr[[Grp[2]]])
    
    # for (cc in seq(3, length(Grp))) {
    #  crois <- croise(crois, AS$VG[[brG]]$Gr[[Grp[cc]]])
    # }
    
    nt <- nrow(crois)
    CritTuple <- numeric(nt)
    saturTuple <- matrix(0, nrow = nt, ncol = np)
    
    for (f in seq_len(nt)) {
      melange <- unlist(c(crois[f, ], v))
      AS <- asTuples(AS, melange)
      CritTuple[f] <- AS$tmp$Crit
      Po <- AS$tmp$Poids
      saturTuple[f, ] <- Po * rowSums(AS$VG[[brG]]$Fct[melange[1:np], ]) # POC ou colsums? why 3x5?
    }
    
    pp <- max(CritTuple, na.rm=TRUE)[1] # POC : make sure to get only one + remove NA
    prob <- 1 - pchisq(pp * (AS$N - 1), dl) ^ length(CritTuple)
    Satur <- colMeans(saturTuple)
    
  }, error = function(e) {
    stop("Erreur lors du calcul des saturations.")
  })
  
  return(list(prob = prob, Satur = Satur, Grp = Grp))
}