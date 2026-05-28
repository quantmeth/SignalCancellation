colonnesGS <- function(AS,ote){
  dernier <- ncol(AS$GS)
  noms <- colnames(AS$GS)
  var <- noms[ote]
  nc <- nchar(noms[dernier])
  groupe <- which(nchar(noms)==nc)
  OTE <- NULL  
  for (k in 1L:length(ote)){
    OTE <- c(OTE,which(grepl(substring(var[k],1,1),noms)))
  }
  OTE <- OTE[OTE >= groupe[1]]
  return(list(ote=OTE,cibl=setdiff(groupe,OTE)))
}