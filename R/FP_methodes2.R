#' Todo
#'
#' @param FS todo
#' @param N  todo 
#' @param ote todo
#' @param zero todo
#' @param nrep todo
#'
#' @returns En sortie, on a les nombres de positifs (vrais ou faux). Les nombres de faux positifs sont donnée en négatif. Le première ligne imprimée est pour la corrélation partielle traditionnelle et la deuxième ligne pour partitionne_R.
#' 
#' @export
#'
#' @examples 
#' FS <- matrix(c(.7,.6,-.45,-.39,.5,.4,.3,0,0,0,0.6,.52,0,.3,.4,.5),ncol=2)
#' out <- FP_methodes2(FS, -500,c(1,2),c(2,3,6,9,13), nrep=1000)
#' out$sig
#' 
#'
FP_methodes2 <- function(FS, N, ote, zero=NULL, nrep=10){
  #SQ2 <- matrix(c(.7,.6,-.45,-.39,.5,.4,.3,0,0,0,0.6,.52,0,.3,.4,.5),ncol=2)
  SQ2 <- FS
  rownames(SQ2) <- letters[1:nrow(SQ2)]
  nv <- nrow(SQ2)
  ng <- nv-length(ote)
  nc <- ng*(ng-1)/2
  out <- matrix(NA,nrow=2*nc,ncol=nrep)
  if (N<0){
    N <- -N
    set.seed(N)
  }
  for (k in 1:nrep){
    dat <- gen_data(SQ2,N)$dt
    R <- cor(dat)
    
    dt <- init_SCA(dat)
    dt <- factor_exclusion(dt,c(1,2))
    re <- rapport_exclusion(dt)
    out[1:nc,k] <- re$CR[upper.tri(re$CR)]    
    
    # Rp <- corr_partielles(R,N,ote)$Rp
    # out[1:nc,k] <- Rp[upper.tri(Rp)]
    Rp <- partitionne_R(R,N,ote)$p1 # POC : ADDED $p1
    out[(nc+1):(2*nc),k] <- Rp[upper.tri(Rp)]
  }
  sig <- t(matrix(rowSums(0+(out<.05),na.rm =TRUE),ncol=2))
  if (length(zero)>0)
    sig[,zero] <- -sig[,zero]
  return(list(sig=sig,out=out))
}