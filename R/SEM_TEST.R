SEM <- function(AS){
  
  pe <- AS$pertinent
  R <- AS$R[pe, pe]
  
  Db <- AS$doublet
  
  for(k in 1:length(AS$VG)){
    k <- 1
    FS <- AS$VG[[k]]$Fct
    rownames(FS) <- colnames(AS$dat)
    FS <- FS[pe,]
    #FS <- AS$VG[[k]]$Fct
    CF <- AS$VG[[k]]$CorFct
    
    #check nom dans AS$R?
    if(is.null(row.names(FS))) row.names(FS) <- paste0("V", 1:nrow(FS))
    colnames(CF) <- rownames(CF) <- colnames(FS) <- paste0("F", 1:ncol(FS))
    model <- ""
    for(i in 1:ncol(FS)){
      model <-  paste0(model,
                       colnames(FS)[i],"=~", paste(rownames(FS)[FS[,i]!=0], collapse = "+"),"\n")#,
      # rownames(CF)[i],"~~", paste(rownames(CF)[CF[i,]!=0], collapse = "+"),"\n")
    }
    model <- paste0(model,
                    apply(which(CF!=0, arr.ind = TRUE), 1, 
                          function(x) paste0("F",x, collapse = "~~")), collapse = "\n")
    test <- lavaan::cfa(model = model, sample.cov = AS$R[pe, pe], sample.nobs = AS$N)
    a <- lavaan::standardizedSolution(test)$est.std
    FS[FS!=0] <- a[1:sum(FS!=0)]
    FS
    # remove non-significant
    #
    
    
  }
}