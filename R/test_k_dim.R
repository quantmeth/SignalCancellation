test_k_dim <- function(AS, k){
  AS$GS[,AS$orphelines] <- AS$colOrphelines # remettre dans $GS les colonnes des orphelines
  if (length(k) > 1){
    tuple <- k
    k <- length(k)
    me <- list(stats = NA)
  } else {
    
    if (k < 2) stop("Ne fonctionne pas pour 1 dimension.")
    
    fa <- Rnest::fareg(AS$R, k)
    Rreduit <- AS$R
    diag(Rreduit) <- fa$h2
    
    if (k > 2){

      me <- meilleur_k_tuple(AS, k, Rreduit) # melange?
    
      } else {
      rg <- which.min(AS$Prob)
      me <- list(meilleur = AS$Cpaires[,rg],
                 stats = c(AS$Prob[rg],
                         AS$Crit[rg]))
    }
    tuple <- me$meilleur
  }
  var <- setdiff(1:AS$nv,tuple)
  satur <- matrix(0,nrow=AS$nv,ncol=k) # faut-il un ajustement pour les orphelines?
  dSat <- sqrt(fa$h2[tuple])
  diag(satur[tuple,]) <- dSat
  out <- NULL
  for (k in var){
    variables <- c(k,tuple)
    ou <- optim_tuple(AS,variables)
    satur[k,] <- -ou$meilPoids[-1] * dSat
    out <- rbind(out, c(variables,ou$prob))
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
  v <- which(rowSums(satur == 0) == 0)
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

polarites_de_correlations <- function(R, e = 2){
  diag(R) <- 0
  sR <- sign(R)
  s <- sign(rowSums(sR * abs(R)^e))
}
