asGereCoplanaire <- function(AS, brG) {
  # À l'entrée, AS$VG[[brG]] contient les champs Gr et coplan.
  # Le rôle de la gestion des grappes trouvées coplanaires par triplet est de 
  # corriger dans les divers scénarios ajoutés résultant de ces coplanarités les 
  # champs de AS$VG[*] concernés. Cela implique de remplacer dans le champ Fct 
  # les saturations des variables devenant bifactorielles sur leur grappe d'origine 
  # par des saturations sur les deux grappes retenues comme facteurs pour le plan,
  # de retirer les grappes bifactorielles du champ Gr et des matrices liées aux corrélations
  # entre facteurs (lignes et colonnes correspondant aux grappes bifactorielles).
  
  if (!"GRP" %in% names(AS)) {
    #res <- asStructCoplan(AS, brG)
    #AS$SCEN <- res$SCEN
    #AS$GRP <- res$GR
    AS <- append(AS, asStructCoplan(AS, brG))
  }
  
  AS$declin <- asCoplanDeclinaisons(AS, brG)
  AS <- SubstitutionsDeSaturations(AS)
  
  for (sc in seq_along(AS$SCEN)) {
    plans <- AS$GRP[AS$SCEN[[sc]], , drop = FALSE]
    declin <- as.matrix(AS$declin[[sc]]) # POC : as.matrix for nrow
    
    for (de in seq_len(nrow(declin))) {
      AS$VG[[length(AS$VG) + 1]] <- AS$VG[[brG]]
      AS$VG[[length(AS$VG)]]$Creat <- "Coplan"
      AS$VG[[length(AS$VG)]]$Parent <- brG
      grBifact <- c()
      varBifact <- c()
      
      for (p in seq_len(nrow(plans))) {
        plan <- plans[p, ]
        plan <- plan[plan != 0]
        subs <- AS$satur[[sc]][[de]]
        facteurs <- unlist(plan[subs[1, 2:3]]) # POC : unlist
        variables <- subs[-1, 1]
        
        grBifact <- c(grBifact, setdiff(plan, facteurs))
        varBifact <- c(varBifact, variables)
        AS$VG[[length(AS$VG)]]$Fct[variables, facteurs] <- subs[-1,2:3]#
      }
      
      grBifact <- unlist(grBifact)
      
      AS$VG[[length(AS$VG)]]$coplan <- grBifact
      AS$VG[[length(AS$VG)]]$Var <- AS$VG[[length(AS$VG)]]$Var[-grBifact]
      
      if ("GrCoplan" %in% names(AS$VG[[length(AS$VG)]])) {
        AS$VG[[length(AS$VG)]]$GrCoplan[[length(AS$VG[[length(AS$VG)]]$GrCoplan) + 1]] <- varBifact
      } else {
        AS$VG[[length(AS$VG)]]$GrCoplan <- list(varBifact)
      }
      
      AS$VG[[length(AS$VG)]]$Gr[grBifact] <- NULL
      AS$VG[[length(AS$VG)]]$Fct <- AS$VG[[length(AS$VG)]]$Fct[, -grBifact]
      
      champs <- c("CorFct", "CorEstim", "pCorFct")
      for (cx in 1:length(champs)) {
        Q <- AS$VG[[length(AS$VG)]][[champs[cx]]]
        #Q[,grBifact] <- NULL
        #Q[grBifact,] <- NULL
        Q <- Q[-grBifact,-grBifact]
        AS$VG[[length(AS$VG)]][[champs[cx]]] <- Q
      }
    }
  }
  
  return(AS)
}