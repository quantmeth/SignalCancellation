asGereReste <- function(AS, brG) {
  # AS <- asGereReste(AS, brG);
  # Cherche des annulations du signal des variables de AS$VG[[brG]]$reste 
  # par les variables de 2 ou 3 grappes, avec préférence pour moins de prédicteurs 
  # si la différence est flagrante.
  
  if (!"probMultiSatur" %in% names(AS$VG[[brG]])) {
    AS$VG[[brG]]$probMultiSatur <- list()
  }
  
  reste <- AS$VG[[brG]]$reste
  nr <- length(reste)
  
  for (r in seq(nr, 1)) {  # Pour chaque variable à expliquer
    prob <- numeric(3)
    Satur <- vector("list", 3)
    Grp <- vector("list", 3)
    v <- reste[r]
    sol <- numeric(2)
    
    for (np in 2:min(3, length(AS$VG[[brG]]$Gr))) {  # Pour 2 et 3 prédicteurs
      res <- asMultiSatur(AS, brG, v, np)
      prob[np] <- res[[1]]
      Satur[[np]] <- res[[2]]
      Grp[[np]] <- res[[3]]
      
      if (prob[2] > AS$seuils[2]) {
        break  # Pas besoin d'estimer avec 3 prédicteurs
      }
    }
    
    if (prob[2] > AS$seuils[1]) {
      sol[1] <- 2
    }
    
    if (prob[3] > max(prob[2], AS$seuils[1])) {
      sol[sol == 0] <- 3  # pourrait mettre 3 sans les deux entrées de sol
    }
    
    if (sol[1] == 0) {
      AS$VG[[brG]]$probMultiSatur <- rbind(AS$VG[[brG]]$probMultiSatur, c(v, max(prob)))
      next  # on n'a pas trouvé de solution pour cette variable
    }
    
    reste <- reste[-r]
    AS$VG[[brG]]$reste <- reste
    Fct <- AS$VG[[brG]]$Fct
    Fct[v, Grp[[sol[1]]]] <- Satur[[sol[1]]]
    
    if (sol[2] != 0 && sol[2] != sol[1]) {
      AS$VG[[length(AS$VG) + 1]] <- AS$VG[[brG]]  # Copier avant de modifier Fct
      AS$VG[[length(AS$VG)]]$Fct[v, Grp[[sol[2]]]] <- Satur[[sol[2]]]
      AS$VG[[length(AS$VG)]]$Creat <- "MultiSatur"
      AS$VG[[length(AS$VG)]]$Parent <- brG
      AS$VG[[length(AS$VG)]]$probMultiSatur <- rbind(AS$VG[[length(AS$VG)]]$probMultiSatur, c(v, prob[sol[2]]))
    }
    
    AS$VG[[brG]]$Fct <- Fct
    AS$VG[[brG]]$probMultiSatur <- rbind(AS$VG[[brG]]$probMultiSatur, c(v, prob[sol[1]]))
  }
  
  return(AS)
}