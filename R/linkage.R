linkage <- function(d, method = "complete", members = NULL){
  rez <- hclust(as.dist(d), method, members)
  out <- rez$merge
  out[out>0] <- out[out>0] + ncol(d)
  out <- abs(out)
  cbind(out, rez$height)
}
