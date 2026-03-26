asPairesIndicatrices <- function(AS) {
  # Identifier les variables indicatrices, en vérifiant l'annulation de toutes les paires de variables non-orphelines
  # POC : length de AS$pertinent et non pas le contenu de AS$pertinent
  AS$Cpaires <- combn(length(AS$pertinent), 2) # tous les sous-ensembles de 2 parmi les variables pertinentes
  nc <- ncol(AS$Cpaires)
  AS$Crit <- rep(0, nc)      # max(abs(Corr))
  AS$Prob <- rep(0, nc)      # corrigé pour le nombre de corrélations
  AS$Ppaires <- rep(0, nc)   # les poids d'annulation
  AS$satPaires <- matrix(NA,nrow=2,ncol=nc)
  AS$Corr <- matrix(0, AS$nv-2-length(AS$orphelines), nc)
  AS$doublet <- NULL
  for (k in 1:nc) {
    AS <- asAnnulePaire(AS, k)  # optimiser les variables de la rangée k de AS$Cpaires
  }
  return(AS)
}
