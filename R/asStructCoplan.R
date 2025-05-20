asStructCoplan <- function(AS, brG) {
  # Prépare la production de toutes les structures de scènes découlant de AS$VG[[brG]]$coplan
  # SCEN[[ng]] (k,4) contient les rangs de grappes formant des triplets ou des quatrains.
  # SCEN et GR sont stockés dans AS plutôt que AS$VG() pour des raisons historiques.
  # Des triplets mutuellement incompatibles sont dans des SCEN[[ ]] distincts.
  # Les probabilités dans la colonne 1 de coplan sont déjà prises en compte.
  # GR est la liste des groupements
  # Ces structures sont à décliner selon les paires de grappes désignées comme facteurs.
  
  # POC: Pourquoi 2
  seuil <- AS$seuils[length(AS$seuils)]  # AS$seuils[1] est toujours dépassé si coplan existe
  coplan <- AS$VG[[brG]]$coplan
  coPlus <- classeTriplets(coplan)  # Ajoute code de restriction aux triplets
  coP <- coPlus  # Servira à produire le GRP à retourner
  GRP <- numeric()
  
  res <- rangXk(coPlus, 4)  # Identifier les quatrains s'il y en a
  q <- res$rg
  codes <- res$codes
  
  # Ajouter les quatrains
  for (k in q) {
    gr <- unique(coPlus[coPlus[, ncol(coPlus)] == k, 2:4])
    GRP <- rbind(GRP, c(.5, gr))
  }
  
  # Ajouter tous les triplets
  lGRP <- max(nrow(GRP),0)
  p <- which(unlist(coP[, ncol(coP)]) %% 10 < 4)
  GRP <- rbind(GRP, coP[p, ,drop = FALSE])
  GR <- GRP[, -1, drop  = FALSE]
  GR[(lGRP+1):nrow(GR), ncol(GR)] <- 0
  
  # Gérer les triplets incompatibles
  q <- rangXk(GRP, 3)$rg
  incomp <- list()
  
  for (k in q) {
    t <- which(GRP[, ncol(GRP)] == k)
    p <- GRP[t, 1]
    if (sum(p > 0.25) == 1) {
      GRP[t[1], ncol(GRP)] <- 1
      GRP <- GRP[-t[2], ]
      GR <- GR[-t[2], ]
    } else {
      incomp <- append(incomp, list(t))
    }
  }
  
  # Réduire GRP aux groupes avec p > seuil
  p <- which(GRP[, 1] < seuil)
  GRP <- GRP[setdiff(1:nrow(GRP), p), , drop = FALSE]
  
  codSur <- seq_len(nrow(GRP))
  for (k in seq_along(incomp)) {
    codSur <- codSur[codSur != incomp[[k]][2]]
  }
  
  # Fabriquer SCEN
  # D'abord ne pointer qu'au premier de deux triplets incompatibles, aussi
  # pour les sous-ensembles à ajouter
  p <- p[p != unlist(lapply(incomp, function(x) x[2]))]
  SS <- subsets(p)
  SCEN <- list(codSur)
  
  if (length(SS[[1]]) > 0) {
    for (k in seq_along(SS)) {
      SCEN <- append(SCEN, list(c(SCEN[[1]], SS[[k]])))
    }
  }
  
  if (length(SCEN[[1]]) == 0) SCEN <- SCEN[-1]
  
  # Dédoubler les scènes avec triplets incompatibles
  if (length(incomp) > 0) {
    for (k in seq_along(SCEN)) {
      A <- SCEN[[k]] %in% unlist(lapply(incomp, function(x) x[1]))
      if (any(A)) {
        SCEN <- append(SCEN, list(SCEN[[k]]))
        f <- which(rowSums(A) > 0)
        g <- which(colSums(A) > 0)
        SCEN[[length(SCEN)]][f] <- unlist(lapply(incomp, function(x) x[2]))[g]
      }
    }
  }
  
  return(list(SCEN = SCEN, GRP = GR))
}

rangXk <- function(mat, k) {
  # Retourne les rangs des lignes de mat dont le dernier élément modulo 10 est k
  codes <- unlist(unique(mat[, ncol(mat)]))
  return(list(rg = which(codes %% 10 == k),
              codes = codes))
}