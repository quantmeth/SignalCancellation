#' Signal Cancellation Analysis of Number of Dimensions
#'
#' @description \code{scan_dim} is used to identify the number of factors to retain in exploratory factor analysis.
#'
#' @param .data  data frame, a numeric matrix, covariance matrix or correlation matrix from which to determine the number of factors.
#' @param ... See details.
#' @param n the number of cases (subjects, participants, or units) if a covariance matrix is supplied in \code{.data}.
#' @param alpha a vector of type I error rates. Default is .05.
#' @param max.fact an optional maximum number of factor to extract. Default is \code{NULL}, so the maximum number possible.
#' @param missing todo
#' @param cluster todo
#' @param ordered todo
#' @param simulation FALSE
#'
#' @returns A list.
#' @export
#' 
#' @importFrom stats lm as.dist cor cov cov2cor hclust median optim optimize pchisq pf pnorm pt qnorm qt runif
#' @importFrom utils combn tail
#' @importFrom Rnest fareg
#'
#' @author André Achim & P.-O. Caron
#'
#' @aliases SCAN_DIM SCAN_Dim
#' 
#' @examples
#' R <- Rnest::tabachnick_fidell2019
#' scan_dim(R, n = 175)
scan_dim <- function(.data, ..., n = NULL, alpha = .05, max.fact = NULL, missing = NULL, cluster = NULL, ordered = NULL, simulation = FALSE){
  # TO ADD TODO
  # missing = "fiml", cluster = NULL, ordered = NULL
  # pour l'instant missing, cluster et ordered ne sont pas utilisé
  R <- list() # Une liste de sortie
  
  # compatibilité avec le tidyverse ####
  #, aka, le pipe +
  # flexibilité de input
  if(!(is.matrix(.data) || is.data.frame(.data) || is.array(.data))){
    ls <- .data
    if(!is.null(ls$n)) n <- ls$n
    if(!is.null(ls$covmat)) {.data <- ls$covmat
    } else {
      .data <- ls$.data
    }
  }
  
  # cluster ####
  # ordered ####
  
  # check #1 positive semi définie ####
  if(nrow(.data) == ncol(.data)){
    
    if(!is.null(cluster)){
      .d2 <- as.matrix(.data[which(!(colnames(.data) %in% cluster)), which(!(colnames(.data) %in% cluster))])
    } else {
      .d2 <- as.matrix(.data)
    }
    
    if(isSymmetric(.d2, check.attributes = FALSE)){
      
      if(!is.null(cluster)) warning("cluster is ignored with covariance matrix.")
      if(!is.null(cluster)) warning("ordered is ignored with covariance matrix.")
      
      if(is.null(n)){
        stop("Argument \"n\" is missing with covariance matrix.")
      } else {
        R$n <- n
      }
      if(all(diag(.data) == 1)){
        R$cor <- .d2
      } else {
        R$cor <-  cov2cor(.d2)
      }
    }
  }
  
  # missing ####
  
  # Estimation de la matrice de corrélation si un data.frame est donné
  # check n également ####
  if(is.null(n) || is.null(R$n)){
    # tiré de Rnest::nest()
    # R$cor <- cor_nest(.data = .data,
    #                   ordered = ordered, 
    #                   missing = missing, 
    #                   cluster = cluster, 
    #                   ...)$covmat
    
    R$cor <- cor(.data, use = "pairwise.complete.obs")
    R$n <- nrow(.data)
    
    if(R$n != n && !is.null(n)) warning("The value of n does not match the number of rows in .data. It is overwritten to ",R$n,".")
    
  } else {
    
    R$n <- n
    
  }
  
  # check #2 pour positive semi définie ####
  
  nv <- ncol(R$cor)
  R$values <- t(as.matrix(eigen(R$cor, symmetric = TRUE)$values))
  
  if((length(Re(R$values)) != nv) && (sum(R$values) != nv) && all(R$values > 0)){
    stop("Correlation matrix is not positive semi definite.")
  }
  
  R$alpha <- alpha
  #R$method <- method
  #R$na.action <- missing
  #R$nreps <- nreps
  #R$Eig <- list()
  R$prob <- numeric()
  # TO ADD ???
  #R$convergence <- TRUE
  
  # check .max.fact ####
  if(is.null(max.fact)) max.fact <- Rnest::Ledermann(ncol(R$cor))-1 #ncol(R$cor)-1 
  
  # scan_dim ####
  AA <- init_SCA(R = R$cor, N = R$n, seuils = R$alpha)
  
  # is alpha the same as seuils? 
  # check for k == 1?  
  out  <- list()
  
  # METTRE un chec pour k=0 etk=1
  # TDO
  if(simulation){
    
    k <- list(nfactors = c(scand1 = scand_poc(AA),
                           scand3 = scand3_poc(AA)))
    
    
  } else {
    # REAL OUTPUT
    
    for(k in 2:max.fact){
      if(k == 1){
        # TODO
        # O and 1
        
        RANG <- t(rbind(AA$Cpaires, AA$Prob))
        tuple <- 4
        CHOIX <- c(which(RANG[,1]==tuple),
                   which(RANG[,2]==tuple))
        
        z <- qnorm(RANG[CHOIX,3])
        pr <- pnorm(sum(z)/sqrt(length(z)));
        
        out[[k-1]] <- test_k_dim(AA, k) # scand or seq_k_dim
        # add convergence issue?
        if(out[[k-1]]$prob > R$alpha) break
      }
    }
  }
  
  sortie <- list(
    nfactors = k,
    details = R,
    scan_dim = out,
    AS = AA
  )
  
  return(sortie)
  
  
  # add class
  # add print
  # add summary
  # add anova ???
}


