#' SCRoF : Signal Cancellation Recovery of Factors
#'
#' @param R correlation, covariance matrix or a data set.
#' @param N sample size if correlation or covariance matrix is supplied.
#' @param seuils significance threshold.
#'
#' @returns
#' A list of class "SCROF"
#' 
#' @references 
#' Achim, A. (2024). Signal cancellation factor analysis. \doi{10.31234/osf.io/h7qwg}
#' Achim, A. (2025). Signal cancellation recovery of factors and meta-factors. \emph{Prooceedingd of the 89th Annual Meeting of the Psychometric Society}. Spring, Prague
#' 
#' @author 
#' André Achim (MATLAB)
#' Pier-Olivier Caron (R)
#' 
#' @import stats Rnest utils
#' @importFrom CCP p.asym
#' 
#' @export
#'
#' @aliases SCROF scrof
#' 
#' @examples
#' res <- SCRoF(N1000_1)
SCRoF <- function(R, N = NULL, seuils = c(.001,.25)){
  # Preliminaries ####
  AS <- list(dat = R)
  if(is.null(N)) {AS$N = nrow(R)} else {AS$N = N}
  if(isSymmetric(as.matrix(R))) {AS$R <- R} else {AS$R <- cov(R)}
  AS$seuils <- sort(seuils)
  AS$et <- sqrt(diag(AS$R)) # AA: Ceci est destiné à pouvoir exprimer la solution factorielle en termes des variables d'origine.
  iet <-  1 / AS$et
  AS$R <- AS$R*(iet %*% t(iet))
  if(det(AS$R) < 0) stop("\nLa matrice de corrélation n'a pas un déterminant positif.\n")
  rNEST <- Rnest::nest(AS$R, n = AS$N)
  rPA <- Rnest::pa(rNEST)
  cat("NEST suggère", rNEST$nfactors, "facteurs.\n")
  cat("PA suggère", rPA$nfactors, "facteurs.\n")
  AS$minFct <- rNEST$nfactors[[1]]
  
  # START HERE
  AS$nv <- ncol(AS$R)
  AS$GS <- chol(AS$R)
  AS <- asOrphelines(AS) 
  # POC: retirer un conditionnel ici, utiliser deux fois ####
  AS$pertinent <- setdiff(1:AS$nv, AS$orphelines)
  
  # SCFA starts here ####
  AS <- asPairesIndicatrices(AS)
  AS <- asGrappes(AS)
  if(is.null(AS$VG)) {cat('\nAucune annulation du signal par paire.\n')}
  AS <- asInitFct_Cor(AS) # CHECK 
  AS <- agregeParCorr(AS) # TODO : POC check with another dataset
  
  if(length(AS$VG[[1]]$Gr) < 3){ # à quoi ça sert?
    AS$VG[[1]]$coplan <- numeric()
    AS$VG[[1]]$GrCoplan <- numeric()
  }
  
  AS <- asCoplanaire(AS)
  AS <- OtePseudoQuatrains(AS)
  #AS$VG[[1]]
  brG <- 0
  # Initialisation de la boucle
  while (brG < length(AS$VG)) {
    brG <- brG + 1
    
    # Vérification si coplan n'est pas vide et que la première valeur est inférieure à 1
    if (!is.null(AS$VG[[brG]]$coplan) && AS$VG[[brG]]$coplan[[1]] < 1) {
      AS <- asGereCoplanaire(AS, brG)
    }
    
    # Vérification si coplan est vide ou si les conditions de rang de grappe ou probabilité sont respectées
    if (is.null(AS$VG[[brG]]$coplan) || AS$VG[[brG]]$coplan[[1]] >= 1 || AS$VG[[brG]]$coplan[[1]] < AS$seuils[length(AS$seuils)]) {
      AS <- asGereReste(AS, brG)  # si coplan vide ou rang de grappe ou prob. < .25
      #POC:
      #       Messages d'avis :
      # 1: Dans max(CritTuple, na.rm = TRUE) :
      #   aucun argument pour max ; -Inf est renvoyé
      
    }
  }
  
  AS <- asTestCorrFct(AS)
  #AS <- asBranches(AS)
  # 
  if ("tmp" %in% names(AS)) {
    AS$tmp <- NULL  # Supprimer le champ 'tmp'
  }
  # 
  # if ("Tuples" %in% names(AS)) {
  #   for (k in 3:length(AS$Tuples)) {
  #     AS$Tuples[[k]]$melange <- decodeMelange(AS, AS$Tuples[[k]]$melange)
  #   }
  # }
  
  return(AS)
}
scrof <- SCROF <- SCRoF