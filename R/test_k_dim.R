test_k_dim <- function(AS,k){
  ASk <- AS  # est-ce requis pour ne pas modifier AS en dehors de cette fonction?
  ASk$GS[,AS$orphelines] <- AS$colOrphelines # remettre dans $GS les colonnes des orphelines
  ASk$pertinent <- 1:ASk$nv
  if (length(k)>1){
    tuple <- k
    k <- length(k)
    me <- list(stats=NA)
  } else {
    if (k<2) stop("Ne marche pas pour 1 dimension")
    # fa <- Rnest::fareg(ASk$R,k)
    fa <- Rnest::fareg(ASk$R,k)
    Rreduit <- ASk$R
    diag(Rreduit) <- fa$h2
    if (k>2){
      meilleur_k_tuple <- function(AS,k){
        var <- AS$pertinent
        cmb <- combn(var,k)
        ei <- 0
        for (j in 1:ncol(cmb)){
          r <- Rreduit[cmb[,j],cmb[,j]]
          eig <- eigen(r,only.values=TRUE)$values[k]
          if (eig>ei){
            ei <- eig
            meilleur <- cmb[,j]
          }
        }
        if (ei<=0) error('Anomalie: aucun ',k,'-tuple avec toutes ses valeurs propres positives')
        ou <- optim_tuple(AS,meilleur);
        return(list(meilleur=meilleur,stats=c(ou$prob,ou$cor,ei,ou$poids)))
        # stats: 1 prob, 1 min(corr), 1 k-ieme eig val, k*k poids
        # (chaque variable en premier) (à préciser) 
      }
      me <- meilleur_k_tuple(AS,k) # AS, pas ASk, on ne veut pas retenir une variable soupçonnée orpheline
    }
    else{
      rg <- which.min(ASk$Prob)
      me <- list(meilleur=AS$Cpaires[,rg],stats=c(AS$Prob[rg],AS$Crit[rg]))
    }
    tuple <- me$meilleur
  }
  var <- setdiff(1:ASk$nv,tuple)
  satur <- matrix(0,nrow=ASk$nv,ncol=k) # faut-il un ajustement pour les orphelines?
  dSat <- sqrt(fa$h2[tuple])
  diag(satur[tuple,]) <- dSat
  out <- NULL
  for (k in var){
    variables <- c(k,tuple)
    ou <- optim_tuple(ASk,variables)
    satur[k,] <- -ou$meilPoids[-1] * dSat
    out <- rbind(out,c(variables,ou$prob))
  }
  R <- Rreduit[tuple,tuple]
  d <- diag(1/sqrt(diag(R)))
  R <- d %*% R %*% d
  satur <- ajustePolarites(satur,R,Rreduit)
  z <- qnorm(out[,ncol(out)])
  pr <- pnorm(sum(z)/sqrt(length(z)));pr
  return(list(prob=pr,out=out,meil=me$stats,satur=satur,Rfct=R))
}

ajustePolarites <- function(satur,Rfct,Rreduit){
  v <- which(rowSums(satur==0)==0)
  s <- polarites_de_correlations(Rreduit[v,v])
  for (k in 1:(10*length(v))){
    V <- satur %*% Rfct %*% t(satur)
    sv <- polarites_de_correlations(V[v,v])
    i <- which(sv != s)
    # sR <- sign(V[v,v])
    # aa <- rowSums(sR * abs(V[v])^2)
    # browser()
    if (length(i)==0)
      break
    else
      satur[v[i[1]],] <- -satur[v[i[1]],]
  }
  return(satur)  
}

polarites_de_correlations <- function(R,e=2){
  diag(R) <- 0
  sR <- sign(R)
  s <- sign(rowSums(sR * abs(R)^e))
}
