SubstitutionsDeSaturations <- function(AS) {
  # AS <- SubstitutionsDeSaturations(AS);
  
  ngrp <- nrow(AS$GRP)
  satur <- vector("list", ngrp)
  
  for (k in seq_len(ngrp)) {
    grp <- AS$GRP[k, ]
    grp <- unlist(grp[grp != 0]) # POC check unlist
    signes <- matSignes(length(grp))
    signes <- signes[AS$declin[[k]], ]  # Ajout 18 fév 2025
    ns <- nrow(signes)
    
    sat <- vector("list", ns)  # Une liste car le nombre de lignes des matrices est le nb de variables +1
    
    for (s in seq_len(ns)) {
      sat[[s]] <- saturations(AS, 1, grp * signes[s, ])
    }
    
    satur[[k]] <- sat
  }
  
  AS$satur <- satur
  return(AS)
}