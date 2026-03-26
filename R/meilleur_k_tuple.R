meilleur_k_tuple <- function(AS, k, Rreduit){
  var <- AS$pertinent
  cmb <- combn(var, k)
  ei <- 0
  for (j in 1:ncol(cmb)){
    r <- Rreduit[cmb[,j],cmb[,j]]
    eig <- eigen(r,only.values=TRUE)$values[k]
    if (eig > ei){
      ei <- eig
      meilleur <- cmb[,j]
    }
  }
  # POC : stop au lieu de error()
  if (ei <= 0) stop('Anomalie: aucun ',k,'-tuple avec toutes ses valeurs propres positives.')
  ou <- optim_tuple(AS, meilleur);
  return(list(meilleur = meilleur,
              stats = c(ou$prob,
                        ou$cor,
                        ei,
                        ou$poids)))
  # stats: 1 prob, 1 min(corr), 1 k-ieme eig val, k*k poids
  # (chaque variable en premier) (à préciser) 
}

# meilleur_k_tuple <- function(AS, k){
#   # R <- AS$Rreduit
#   # browser()
#   # if (is.null(R)){
#     # Rreduit <- AS$R   # restera accessible à optim_tuple
#     # diag(Rreduit) <- (1/diag(solve(Rreduit))) # peut donner des valeurs propres négatives, c'est OK
#   #   AS$Rreduit <- R
#   # }
#   #browser()  # on ne devrait pas utiliser cette version de la fonction maintenant définie à l'intérieur de test_k_dim
#   var <- AS$pertinent
#   cmb <- combn(var, k)
#   ei <- 0
#   for (j in 1:ncol(cmb)){
#     r <- Rreduit[cmb[,j],cmb[,j]]
#     eig <- eigen(r,only.values=TRUE)$values[k]
#     if (eig>ei){
#       ei <- eig
#       meilleur <- cmb[,j]
#     }
#   }
#   #browser()
#   ou <- optim_tuple(AS,meilleur);
#   eig <- eigen(Rreduit[melange,melange], only.values=TRUE)$values
#   return(list(meilleur = meilleur,
#               stats = c(ou$prob,ou$cor,eig))) 
# }
