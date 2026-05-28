reunion <- function(Eab,Eba){
  E <- Eab
  cv <- colnames(E$GS)[c(Eab$excl_AB,Eba$excl_AB)]
  col <- aGarder(Eab,cv)
  E$GS <- E$GS[,-col$ote]
  noms <- colnames(Eba$GS)
  col <- which(nchar(noms)==3)
  col <- aGarder(Eba,cv)
  E$GS <- cbind(E$GS,Eba$GS[,col$garde])
  return(E)
}

aGarder <- function(E1,cv){
  noms <- colnames(E1$GS)
  col <- which(nchar(noms)==3)
  cc <- col
  for (k in cv){
    ote <- str_locate(noms[col],k)
    col[ote[,1]==1] <- 0
  }
  return(list(garde=cc[col>0],ote=cc[col==0]))
}
