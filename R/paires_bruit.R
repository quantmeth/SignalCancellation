paires_bruit <- function(cov1,cov2){
  # exemple: cov1="a" et cov2="e"  OU cov1=c("a","e") et cov2=c("g","i") 
  if (length(cov1)==1) return(c(paste0(cov1,cov2),paste0(cov2,cov1)))
  a <- cov1[1]
  b <- cov1[2]
  f <- cov2[1]
  g <- cov2[2]
  pb <- c(paste0(a,f,b,g),paste0(a,f,g,b),paste0(a,g,b,f),paste0(a,g,f,b))
  pb <- c(pb,paste0(b,f,a,g),paste0(b,f,g,a),paste0(b,g,a,f),paste0(b,g,f,a))
  pb <- c(pb,paste0(f,a,b,g),paste0(f,a,g,b),paste0(f,b,a,g),paste0(f,b,g,a))
  pb <- c(pb,paste0(g,a,f,b),paste0(g,a,b,f),paste0(g,b,a,f),paste0(g,b,f,a))
  return(pb)
}