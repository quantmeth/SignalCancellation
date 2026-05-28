inspection_scfa <- function(x, seuil = .05){
  
  sortie <- x$AS
  
  
  sortie2 <- as.data.frame(round(t(rbind(sortie[["Cpaires"]],sortie[["Prob"]],sortie[["satPaires"]])),3))
  colnames(sortie2) <- c("var1","var2","Prob","sat1","sat2")
  sortie2$check <- ifelse(sortie2$Prob <= seuil,"*","")
  sortie2
}
