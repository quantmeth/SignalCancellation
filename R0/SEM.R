SEM <- function(object, alpha = .05, ...){
  
  
  FS <- object$Fct
  ng <- object$ng
  
  # if(is.null(FS)) {
  #   
  #   cat("No factor structure was recovered. \n")
  #   cat("Did you use `factors.only = TRUE` in SCFA? \n \n")
  #   print(object)
  #   CS <- Loadings <- fit <- NULL
  #   
  # } else {
    
    if(is.null(row.names(FS))) row.names(FS) <- paste0("V",1:ncol(object$R))
    
    model <- ""
    for(i in 1:ng){
      model = paste0(model,colnames(FS)[i],"=~", paste(rownames(FS)[FS[,i]!=0], collapse = "+"),"\n")
    }
    
    test <- lavaan::cfa(model = model, sample.cov = object$R, sample.nobs = object$N)
    
    a <- lavaan::standardizedSolution(test)$est.std
    FS[FS!=0] <- a[1:sum(FS!=0)]
    #FS2 <- ifelse(FS^2 < qchisq(.95, 1)/object$N, "", sprintf(FS, fmt = '%#.3f'))

    
    fit <- lavaan::fitmeasures(test)[c("npar","chisq","df","pvalue","cfi","tli","logl","aic","bic","rmsea","srmr")]
    
    if(ng > 1){
      CS <- matrix(0, ng, ng)
      rownames(CS) <- colnames(CS) <- colnames(FS)
      CS[lower.tri(CS)] <- a[(sum(FS!=0) + nrow(FS) - length(object$orphelines) + ng + 1) : length(a)]
      #CS <- CS + t(CS)
      diag(CS) <- a[(sum(FS!=0) + nrow(FS) - length(object$orphelines) + 1)+(1:ng)-1]
      
     # CS2 <- ifelse(CS^2 < qchisq(.95, 1)/object$N, "", sprintf(CS, fmt = '%#.3f'))
      
     # if(any(CS<0)){
    #    CS2 <- (ifelse(CS < 0, CS2, paste0(" ",CS2)))
    #  } 

      #cat(" Correlations between factors: \n")
      #print(CS2 , quote = FALSE)
    }
   
 # }
  # output
  invisible(list(nfactors = ng,
                 CorrFact = CS,
                 Loadings = FS,
                 fit = fit))
}


