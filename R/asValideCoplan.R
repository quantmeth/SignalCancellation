asValideCoplan <- function(AS, brG) {
  # AS <- asValideCoplan(AS, brG)
  # À l'appel, AS$VG n'a qu'un niveau.
  # Détecte s'il y a une incompatibilité entre les triplets coplanaires initiaux.
  # Ce serait le partage de 2 grappes par seulement une paire de triplets.
  # On garde AS$VG[[1]] avec toutes ses grappes pour en avoir les corrélations.
  # Si un des deux triplets partageant une même grappe a p > .25 et l'autre p < .25,
  # on retire du tableau AS$VG[[1]]$coplan la ligne avec p < .25.
  # Si les deux lignes ont leur probabilité du même côté de .25, on ajoute
  # deux groupes VG avec chacun des plans. Le plus haut p est toujours gardé,
  # l'autre est conservé séparément seulement si p du même côté de .25.
  # Si on a deux paires indépendantes de plans partageant chacune un facteur,
  # on veut les croisements des deux paires indépendantes, chacune gardant
  # ses deux plans ou pas selon les probabilités.
  
  coplan <- AS$VG[[brG]]$coplan
  nc <- length(coplan)
  
  if (nc < 2) {
    return(AS)
  }
  
  s <- AS$seuils[2]
  cens <- recens(coplan)
  f <- which(cens[, 2] == 2)
  
  if (length(f) == 0) {
    return(AS)
  }
  
  q <- subsets(f, 2, 2)
  
  # Ne garder que les paires ayant les deux mêmes grappes
  for (k in rev(seq_along(q))) {
    l1 <- lignesQuiOnt(coplan, cens[q[[k]][1], 1])$L
    l2 <- lignesQuiOnt(coplan, cens[q[[k]][2], 1])$L
    
    if (!all(l1 == l2)) {
      q <- q[-k]
    }
  }
  
  g <- list()
  
  for (k in seq_along(q)) {
    f <- q[[k]][1]
    lignes_prob <- lignesQuiOnt(coplan, cens[f, 1])
    L <- lignes_prob$L
    P <- lignes_prob$P
    g[[k]] <- L
    
    if (P[2] < .25 && P[1] > .25) {
      g[[k]] <- g[[k]][-2]
      AS$VG[[brG]]$coplan <- AS$VG[[brG]]$coplan[-L[2], , drop = FALSE]
    }
  }
  
  if (length(g) > 1) {
    COPL <- assemble(coplan, g)
    
    for (j in seq_along(COPL)) {
      AS$VG[[length(AS$VG) + 1]] <- AS$VG[[brG]]
      AS$VG[[length(AS$VG)]]$coplan <- COPL[[j]]
    }
  }
  
  return(AS)
}

lignesQuiOnt <- function(coplan, gr) {
  # Retourne les rangs de lignes (L) dans coplan qui contiennent la grappe gr
  # et les probabilités (P) associées à ces lignes.
  
  L <- which(apply(coplan[, -1, drop = FALSE], 1, function(x) gr %in% x))
  P <- coplan[L, 1]
  
  return(list(L = L, P = P))
}

assemble <- function(coplan, G) {
  # assemble(coplan, G)
  # COPL sera une liste combinant un élément de chaque coplan selon g 
  # contenant un ou deux rangs dans coplan.
  
  COPL <- lapply(G[[length(G)]], function(k) coplan[k, , drop = FALSE])
  
  for (j in seq_along(G) - 1) {
    deja <- COPL
    COPL <- list()
    
    for (k in seq_along(deja)) {
      for (i in seq_along(G[[j]])) {
        COPL[[length(COPL) + 1]] <- list(coplan[G[[j]], , drop = FALSE], deja[[k]])
      }
    }
  }
  
  return(COPL)
}