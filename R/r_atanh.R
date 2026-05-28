r_atanh <- function(AS){
  nc <- nchar(colnames(AS$GS))
  nco <- nc[length(nc)]
  cc <- which(nc==nco)
  noms <- colnames(AS$GS[,cc])
  dv <- unique(substr(noms,1,1))
  cv2 <- colnames(AS$GS[,AS$excl_AB])
  if (nco==2){
    cv1 <- cv2[1]
    cv2 <- cv2[2]
  } else
    cv1 <- setdiff(unique(substr(noms,2,2)),cv2)
  pb <- paires_bruit(cv1,cv2)
  ss <- combn(dv,2)
  pp <- paste0(ss[1,],ss[2,])
  rPaires <- matrix(NA,nrow=length(pb),ncol=length(pp))
  rownames(rPaires)=pb
  colnames(rPaires)=pp
  for (k in pp){
    a <- substr(k,1,1)
    b <- substr(k,2,2)
    for (j in pb) {
      if (nco==2){
        A <- paste0(a,substr(j,1,1))
        B <- paste0(b,substr(j,2,2))
      } else {
      A <- paste0(a,substr(j,1,2))
      B <- paste0(b,substr(j,3,4))
      }
      rPaires[j,k] <- atanh(t(sc1(AS$GS[,A])) %*% sc1(AS$GS[,B]))
    }
  }
  return(rPaires)
}